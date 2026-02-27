#!/usr/bin/env python3
"""
plot-metrics.py - Generate build metrics visualization

Usage: plot-metrics.py METRICS_DIR TIMING_DIR [OUTPUT_PREFIX]

Arguments:
    METRICS_DIR   - Directory containing metrics.csv
    TIMING_DIR    - Directory containing phase timing files (.start, .end)
    OUTPUT_PREFIX - Output file prefix (default: METRICS_DIR/build-metrics)
                    Generates: PREFIX-build.svg and PREFIX-test.svg

Creates dual-axis plots showing:
    - CPU usage over time (left axis, blue)
    - Memory usage over time (right axis, green)
    - Phase markers with shaded regions and labels

Two separate plots are generated:
    - Build plot: cabal, stage1, stage2, stage2-utils, bindist phases
    - Test plot: test phase only
"""

import sys
import os
import csv
import hashlib
from datetime import datetime
from pathlib import Path

# Try to import matplotlib, provide helpful error if not available
try:
    import matplotlib
    matplotlib.use('Agg')  # Non-interactive backend for headless use
    import matplotlib.pyplot as plt
    import matplotlib.dates as mdates
    from matplotlib.patches import Rectangle
except ImportError:
    print("Error: matplotlib is required. Install with:")
    print("  nix run nixpkgs#python3Packages.matplotlib -- mk/plot-metrics.py ...")
    print("  # or: pip install matplotlib")
    sys.exit(1)


def read_metrics(metrics_file):
    """Read metrics CSV file and return timestamps, cpu, and memory data."""
    timestamps = []
    cpu_percent = []
    mem_used_mb = []
    mem_total_mb = []

    with open(metrics_file, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            try:
                ts = int(row['timestamp'])
                timestamps.append(datetime.fromtimestamp(ts))
                cpu_percent.append(float(row['cpu_percent']))
                mem_used_mb.append(float(row['mem_used_mb']))
                mem_total_mb.append(float(row['mem_total_mb']))
            except (ValueError, KeyError) as e:
                continue  # Skip malformed rows

    return timestamps, cpu_percent, mem_used_mb, mem_total_mb


def read_phases(timing_dir):
    """Read phase timing files and return list of (name, start_time, end_time, status)."""
    phases = []
    timing_path = Path(timing_dir)

    # Find all .start files
    for start_file in timing_path.glob('*.start'):
        phase_name = start_file.stem
        end_file = timing_path / f"{phase_name}.end"
        status_file = timing_path / f"{phase_name}.status"

        if not end_file.exists():
            continue

        try:
            with open(start_file) as f:
                start_ts = int(f.read().strip())
            with open(end_file) as f:
                end_ts = int(f.read().strip())

            status = "OK"
            if status_file.exists():
                with open(status_file) as f:
                    status = "FAIL" if f.read().strip() == "1" else "OK"

            phases.append((
                phase_name,
                datetime.fromtimestamp(start_ts),
                datetime.fromtimestamp(end_ts),
                status
            ))
        except (ValueError, IOError):
            continue

    # Sort by start time
    phases.sort(key=lambda x: x[1])
    return phases


def format_duration(seconds):
    """Format duration in human-readable form."""
    if seconds < 60:
        return f"{seconds}s"
    elif seconds < 3600:
        mins = seconds // 60
        secs = seconds % 60
        return f"{mins}m {secs}s"
    else:
        hours = seconds // 3600
        mins = (seconds % 3600) // 60
        return f"{hours}h {mins}m"


def create_plot(timestamps, cpu, mem_used, mem_total, phases, title, output_file):
    """Create a single metrics plot for the given data and phases."""
    # Define colors
    cpu_color = '#2E86AB'      # Blue
    mem_color = '#28A745'      # Green
    phase_colors = {
        'cabal': '#FFD700',        # Gold
        'stage1': '#FF6B6B',       # Red
        'stage2': '#4ECDC4',       # Teal
        'test': '#DDA0DD',         # Plum
    }
    # stage3-* platforms get distinct colors
    stage3_palette = ['#FF8C42', '#6A0572', '#1B998B', '#E55934']

    # Create figure with dual y-axes - wider aspect ratio (20:6)
    fig, ax1 = plt.subplots(figsize=(20, 6))

    # Calculate effective concurrency from max CPU usage
    max_cpu = max(cpu) if cpu else 100
    effective_cores = int((max_cpu + 99) // 100)  # ceil(max_cpu / 100)
    cpu_limit = effective_cores * 100

    # Plot CPU usage
    ax1.set_xlabel('Time', fontsize=11)
    ax1.set_ylabel(f'CPU Usage (%, {effective_cores} cores)', color=cpu_color, fontsize=11)
    line1, = ax1.plot(timestamps, cpu, color=cpu_color, linewidth=1.2, alpha=0.8, label='CPU %')
    ax1.tick_params(axis='y', labelcolor=cpu_color)
    ax1.set_ylim(0, cpu_limit * 1.05)
    ax1.grid(True, alpha=0.3)

    # Create second y-axis for memory
    ax2 = ax1.twinx()
    ax2.set_ylabel('Memory Used (GB)', color=mem_color, fontsize=11)
    mem_gb = [m / 1024 for m in mem_used]
    line2, = ax2.plot(timestamps, mem_gb, color=mem_color, linewidth=1.2, alpha=0.8, label='Memory (GB)')
    ax2.tick_params(axis='y', labelcolor=mem_color)

    # Set memory y-axis limit based on total memory
    if mem_total:
        max_mem_gb = max(mem_total) / 1024
        ax2.set_ylim(0, max_mem_gb * 1.1)

    # Add phase markers as shaded regions
    if phases and timestamps:
        plot_start = timestamps[0]
        plot_end = timestamps[-1]

        for phase_name, start, end, status in phases:
            # Clamp to plot range
            if end < plot_start or start > plot_end:
                continue

            start = max(start, plot_start)
            end = min(end, plot_end)

            # Get color for phase (stage3-* uses rotating palette)
            if phase_name in phase_colors:
                color = phase_colors[phase_name]
            elif phase_name.startswith('stage3-'):
                # Stable hash so the same platform always gets the same color
                idx = int(hashlib.sha256(phase_name.encode()).hexdigest(), 16) % len(stage3_palette)
                color = stage3_palette[idx]
            else:
                color = '#CCCCCC'

            # Add shaded region
            ax1.axvspan(start, end, alpha=0.2, color=color)

            # Add vertical line at phase start
            ax1.axvline(x=start, color=color, linestyle='--', linewidth=1, alpha=0.7)

            # Add phase label at top
            mid_time = start + (end - start) / 2
            duration = int((end - start).total_seconds())
            duration_str = format_duration(duration)
            status_marker = '\u2713' if status == 'OK' else '\u2717'

            ax1.annotate(
                f'{phase_name}\n{duration_str} {status_marker}',
                xy=(mid_time, cpu_limit),
                fontsize=10,
                ha='center',
                va='top',
                bbox=dict(boxstyle='round,pad=0.3', facecolor=color, alpha=0.7)
            )

    # Format x-axis
    ax1.xaxis.set_major_formatter(mdates.DateFormatter('%H:%M'))
    ax1.xaxis.set_major_locator(mdates.AutoDateLocator())
    plt.xticks(rotation=45)

    # Title
    plt.title(title, fontsize=13, fontweight='bold')

    # Legend
    lines = [line1, line2]
    labels = ['CPU Usage (%)', 'Memory Used (GB)']
    ax1.legend(lines, labels, loc='upper left', framealpha=0.9)

    # Tight layout
    plt.tight_layout()

    # Save as SVG
    plt.savefig(output_file, format='svg', bbox_inches='tight')
    plt.close(fig)
    print(f"Plot saved to: {output_file}")


def filter_metrics_for_phases(timestamps, cpu, mem_used, mem_total, phases):
    """Filter metrics data to only include time range covered by phases."""
    if not phases or not timestamps:
        return timestamps, cpu, mem_used, mem_total

    # Get time range from phases
    phase_start = min(p[1] for p in phases)
    phase_end = max(p[2] for p in phases)

    # Add some margin (30 seconds before and after)
    from datetime import timedelta
    margin = timedelta(seconds=30)
    range_start = phase_start - margin
    range_end = phase_end + margin

    # Filter data
    filtered = [(t, c, m, mt) for t, c, m, mt in zip(timestamps, cpu, mem_used, mem_total)
                if range_start <= t <= range_end]

    if not filtered:
        return timestamps, cpu, mem_used, mem_total

    return zip(*filtered)


def plot_metrics(metrics_dir, timing_dir, output_prefix):
    """Generate the metrics plots (build and test separately)."""
    metrics_file = Path(metrics_dir) / 'metrics.csv'

    if not metrics_file.exists():
        print(f"Error: Metrics file not found: {metrics_file}")
        sys.exit(1)

    # Read data
    timestamps, cpu, mem_used, mem_total = read_metrics(metrics_file)
    all_phases = read_phases(timing_dir)

    if not timestamps:
        print("Error: No metrics data found")
        sys.exit(1)

    # Separate build phases from test phase.
    # Include top-level phases only (no sub-phases with dots) to avoid clutter.
    # stage3-* platforms are build phases.
    def is_build_phase(name):
        if '.' in name:
            return False  # Skip sub-phases
        return name in ('cabal', 'stage1', 'stage2') or name.startswith('stage3-')

    build_phases = [p for p in all_phases if is_build_phase(p[0])]
    test_phases = [p for p in all_phases if p[0] == 'test']

    # Generate build plot
    if build_phases:
        ts_build, cpu_build, mem_build, mem_total_build = filter_metrics_for_phases(
            timestamps, cpu, mem_used, mem_total, build_phases)
        ts_build, cpu_build, mem_build, mem_total_build = list(ts_build), list(cpu_build), list(mem_build), list(mem_total_build)

        build_duration = int((build_phases[-1][2] - build_phases[0][1]).total_seconds())
        build_title = f'GHC Build Metrics - Total: {format_duration(build_duration)}'
        build_output = f"{output_prefix}-build.svg"
        create_plot(ts_build, cpu_build, mem_build, mem_total_build,
                   build_phases, build_title, build_output)

    # Generate test plot
    if test_phases:
        ts_test, cpu_test, mem_test, mem_total_test = filter_metrics_for_phases(
            timestamps, cpu, mem_used, mem_total, test_phases)
        ts_test, cpu_test, mem_test, mem_total_test = list(ts_test), list(cpu_test), list(mem_test), list(mem_total_test)

        test_duration = int((test_phases[-1][2] - test_phases[0][1]).total_seconds())
        test_title = f'GHC Test Metrics - Duration: {format_duration(test_duration)}'
        test_output = f"{output_prefix}-test.svg"
        create_plot(ts_test, cpu_test, mem_test, mem_total_test,
                   test_phases, test_title, test_output)

    # Print phase summary
    if all_phases:
        total_duration = int((all_phases[-1][2] - all_phases[0][1]).total_seconds())
        print("\nPhase Summary:")
        print("-" * 50)
        for phase_name, start, end, status in all_phases:
            duration = int((end - start).total_seconds())
            print(f"  {phase_name:15} {format_duration(duration):>10}  [{status}]")
        print("-" * 50)
        print(f"  {'TOTAL':15} {format_duration(total_duration):>10}")


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(1)

    metrics_dir = sys.argv[1]
    timing_dir = sys.argv[2]
    output_prefix = sys.argv[3] if len(sys.argv) > 3 else os.path.join(metrics_dir, 'metrics')

    plot_metrics(metrics_dir, timing_dir, output_prefix)


if __name__ == '__main__':
    main()
