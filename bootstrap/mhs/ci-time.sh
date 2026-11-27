#!/usr/bin/env bash
# ci-time.sh — clean reusable timing helper for GitHub Actions & local runs.
#
# Usage:
#   bootstrap/mhs/ci-time.sh run <timing_file> <phase_id> <label> -- <cmd...>
#   bootstrap/mhs/ci-time.sh summary <title> <timing_file...>
#
# `run` appends one TSV row to <timing_file>:
#   <phase_id>\t<label>\t<start_iso>\t<end_iso>\t<duration_sec>\t<exit_code>
# and preserves the wrapped command's exit code. On GitHub Actions
# (GITHUB_ACTIONS=true) it also emits ::group::/::endgroup:: banners.
#
# `summary` prints a GitHub-Flavored Markdown table
# (| Phase | Duration | Status |) with `Xm Ys` durations, per-file
# subtotals, a total wall-time line, and OK / FAIL badges, suitable for
# appending directly to $GITHUB_STEP_SUMMARY.
set -euo pipefail

usage() {
  cat >&2 <<'EOF'
Usage:
  ci-time.sh run <timing_file> <phase_id> <label> -- <cmd...>
  ci-time.sh summary <title> <timing_file...>
EOF
  exit 2
}

fmt_dur() {
  local s="$1" h m r
  h=$((s / 3600)); r=$((s % 3600)); m=$((r / 60)); r=$((r % 60))
  if [ "$h" -gt 0 ]; then
    printf '%dh %dm %ds' "$h" "$m" "$r"
  else
    printf '%dm %ds' "$m" "$r"
  fi
}

cmd_run() {
  if [ "$#" -lt 4 ]; then usage; fi
  local timing_file="$1" phase_id="$2" label="$3"
  shift 3
  if [ "${1:-}" != "--" ]; then usage; fi
  shift
  if [ "$#" -eq 0 ]; then usage; fi

  mkdir -p "$(dirname "$timing_file")"
  [ -f "$timing_file" ] || : > "$timing_file"

  local start_epoch start_iso end_epoch end_iso dur rc
  start_epoch="$(date +%s)"
  start_iso="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

  if [ "${GITHUB_ACTIONS:-false}" = "true" ]; then
    echo "::group::[$phase_id] $label"
  else
    echo "=== [$phase_id] $label (start: $start_iso) ==="
  fi

  set +e
  "$@"
  rc=$?
  set -e

  end_epoch="$(date +%s)"
  end_iso="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  dur=$((end_epoch - start_epoch))

  printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$phase_id" "$label" "$start_iso" "$end_iso" "$dur" "$rc" >> "$timing_file"

  if [ "${GITHUB_ACTIONS:-false}" = "true" ]; then
    echo "::endgroup::"
  fi
  if [ "$rc" -eq 0 ]; then
    echo "=== [$phase_id] $label: OK in $(fmt_dur "$dur") ==="
  else
    echo "=== [$phase_id] $label: FAIL ($rc) after $(fmt_dur "$dur") ===" >&2
  fi
  return "$rc"
}

cmd_summary() {
  if [ "$#" -lt 2 ]; then usage; fi
  local title="$1"
  shift

  echo "## $title"
  echo ""
  echo "| Phase | Duration | Status |"
  echo "|-------|----------|--------|"

  local total=0 f line pid label _s _e dur code
  for f in "$@"; do
    if [ ! -f "$f" ]; then
      echo "| \`$f\` | 0m 0s | FAIL |"
      continue
    fi
    local sub=0
    while IFS=$'\t' read -r pid label _s _e dur code; do
      [ -n "${pid:-}" ] || continue
      case "$pid" in \#*) continue;; esac
      dur="${dur:-0}"; code="${code:-1}"
      sub=$((sub + dur)); total=$((total + dur))
      if [ "$code" -eq 0 ]; then
        echo "| $label | $(fmt_dur "$dur") | OK |"
      else
        echo "| $label | $(fmt_dur "$dur") | FAIL |"
      fi
    done < "$f"
    echo "| **Subtotal ($f)** | **$(fmt_dur "$sub")** |  |"
  done
  # silence unused warning for the loop var when no files had rows
  line="${line:-}"
  echo ""
  echo "**Total wall time:** $(fmt_dur "$total")"
}

if [ "$#" -eq 0 ]; then usage; fi
sub="$1"; shift
case "$sub" in
  run)     cmd_run "$@" ;;
  summary)  cmd_summary "$@" ;;
  -h|--help|help) usage ;;
  *) usage ;;
esac
