#!/usr/bin/env bash
# collect-metrics.sh - Collect CPU and memory metrics during build
#
# Usage: collect-metrics.sh start METRICS_DIR [INTERVAL]
#        collect-metrics.sh stop
#
# Commands:
#   start   - Start collecting metrics (runs in background)
#   stop    - Stop metrics collection
#
# Arguments:
#   METRICS_DIR - Directory for metrics output
#   INTERVAL    - Sample interval in seconds (default: 0.5)
#
# Output files:
#   $METRICS_DIR/metrics.csv  - CSV with timestamp, cpu%, mem_used_mb, mem_total_mb
#   $METRICS_DIR/collector.pid - PID file for the collector process

set -uo pipefail

CMD="${1:-}"
METRICS_DIR="${2:-_build/metrics}"
INTERVAL="${3:-0.5}"

PID_FILE="$METRICS_DIR/collector.pid"
METRICS_FILE="$METRICS_DIR/metrics.csv"

# Detect OS for platform-specific commands
OS="$(uname -s)"

# State file for CPU delta calculation
CPU_STATE_FILE=""

# Get CPU usage percentage (cross-platform)
# Calculates delta between samples for accurate instantaneous usage
get_cpu_usage() {
    case "$OS" in
        Darwin)
            # macOS: sum per-process CPU usage via ps
            # Note: kern.cp_time is FreeBSD-only and doesn't exist on macOS.
            # Use absolute path — nix devx shell may not include /bin in PATH.
            /bin/ps -A -o %cpu 2>/dev/null \
                | awk '{sum += $1} END {printf "%.1f", sum}' \
                || echo "0.0"
            ;;
        Linux)
            # Linux: calculate from /proc/stat with delta
            # /proc/stat format: cpu  <user> <nice> <system> <idle> <iowait> <irq> <softirq> [steal] [guest] [guest_nice]
            if [[ ! -f /proc/stat ]]; then
                echo "0"
                return
            fi

            local line
            read -r line < /proc/stat

            # Parse fields - use named variable for label instead of _ (special bash variable)
            # Note: /proc/stat has 10 numeric fields; we only need the first 7 for CPU calculation
            # The 'rest' variable captures any additional fields (steal, guest, guest_nice)
            local label user nice sys idle iowait irq softirq rest
            read label user nice sys idle iowait irq softirq rest <<< "$line"

            # Validate we got numeric values (guards against parse failures)
            if [[ -z "$user" || -z "$idle" ]]; then
                echo "0"
                return
            fi

            local total=$((user + nice + sys + idle + iowait + irq + softirq))

            if [[ -f "$CPU_STATE_FILE" ]]; then
                local prev_total prev_idle
                read prev_total prev_idle < "$CPU_STATE_FILE"
                local delta_total=$((total - prev_total))
                local delta_idle=$((idle - prev_idle))
                if [[ $delta_total -gt 0 ]]; then
                    echo "$total $idle" > "$CPU_STATE_FILE"
                    awk "BEGIN {printf \"%.1f\", 100 * (1 - $delta_idle / $delta_total)}"
                    return
                fi
            fi

            echo "$total $idle" > "$CPU_STATE_FILE"
            if [[ $total -gt 0 ]]; then
                awk "BEGIN {printf \"%.1f\", 100 * (1 - $idle / $total)}"
            else
                echo "0"
            fi
            ;;
        *)
            echo "0"
            ;;
    esac
}

# Get memory usage in MB (cross-platform)
get_memory_usage() {
    case "$OS" in
        Darwin)
            # macOS: use vm_stat and sysctl with absolute paths
            # Nix devx shell may not include /usr/bin or /usr/sbin in PATH.
            page_size=$(/usr/sbin/sysctl -n hw.pagesize 2>/dev/null || echo 4096)
            total_mb=$(( $(/usr/sbin/sysctl -n hw.memsize 2>/dev/null || echo 0) / 1024 / 1024 ))

            # Parse vm_stat output — use $NF (last field) for robustness across
            # macOS versions (the label column count varies, e.g. "Pages compressed"
            # vs "Pages stored in compressor").
            /usr/bin/vm_stat 2>/dev/null | awk -v ps="$page_size" -v total="$total_mb" '
                /Pages active/                { active     = $NF + 0 }
                /Pages wired/                 { wired      = $NF + 0 }
                /Pages stored in compressor/  { compressed = $NF + 0 }
                /Pages compressed/            { compressed = $NF + 0 }
                END {
                    used_mb = int((active + wired + compressed) * ps / 1024 / 1024)
                    printf "%d,%d", used_mb, total
                }
            '
            ;;
        Linux)
            # Linux: parse /proc/meminfo
            awk '
                /^MemTotal:/ { total = $2 }
                /^MemAvailable:/ { available = $2 }
                END {
                    total_mb = int(total / 1024)
                    used_mb = int((total - available) / 1024)
                    printf "%d,%d", used_mb, total_mb
                }
            ' /proc/meminfo
            ;;
        *)
            echo "0,0"
            ;;
    esac
}

# Collector loop
run_collector() {
    mkdir -p "$METRICS_DIR"

    # Initialize CPU state file for delta calculations
    CPU_STATE_FILE="$METRICS_DIR/.cpu_state"
    rm -f "$CPU_STATE_FILE"

    # Write CSV header
    echo "timestamp,cpu_percent,mem_used_mb,mem_total_mb" > "$METRICS_FILE"

    while true; do
        timestamp=$(date +%s)
        cpu=$(get_cpu_usage)
        mem=$(get_memory_usage)

        echo "$timestamp,$cpu,$mem" >> "$METRICS_FILE"
        sleep "$INTERVAL"
    done
}

case "$CMD" in
    start)
        mkdir -p "$METRICS_DIR"

        # Stop any existing collector
        if [[ -f "$PID_FILE" ]]; then
            old_pid=$(cat "$PID_FILE")
            kill "$old_pid" 2>/dev/null || true
            rm -f "$PID_FILE"
        fi

        # Start collector in background
        run_collector &
        collector_pid=$!
        echo "$collector_pid" > "$PID_FILE"
        echo "Started metrics collector (PID: $collector_pid, interval: ${INTERVAL}s)"
        echo "Output: $METRICS_FILE"
        ;;

    stop)
        if [[ -f "$PID_FILE" ]]; then
            pid=$(cat "$PID_FILE")
            if kill "$pid" 2>/dev/null; then
                echo "Stopped metrics collector (PID: $pid)"
            else
                echo "Collector process $pid not running"
            fi
            rm -f "$PID_FILE"
        else
            echo "No collector PID file found"
        fi
        ;;

    *)
        echo "Usage: $0 start METRICS_DIR [INTERVAL]"
        echo "       $0 stop"
        exit 1
        ;;
esac
