#!/usr/bin/env python3
"""Compare only T26537 with two existing native Intel macOS bindists.

Both compilers receive the same lightweight startup probe before one timed
compilation each, serially. Every invocation has a fresh process and fresh
output directory. No operating-system cache purge or cold-cache claim is made.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import selectors
import shlex
import shutil
import signal
import subprocess
import sys
import time


# Copied from the final release test's normal-way compiler command. Keep lint
# flags: removing them would change the workload whose timeout we investigate.
FLAGS = [
    "-dcore-lint", "-dstg-lint", "-dcmm-lint", "-fno-dump-with-ways",
    "-no-user-package-db", "-rtsopts", "-fno-warn-missed-specialisations",
    "-fshow-warning-groups", "-fdiagnostics-color=never",
    "-fno-diagnostics-show-caret", "-fprint-error-index-links=never",
    "-Werror=compat", "-dno-debug-output", "-O2", "-fregs-graph",
]
COMPILE_TIMEOUT = 300 * 5
RUN_TIMEOUT = 300
FIXTURE = Path("testsuite/tests/codeGen/should_run")
PROVENANCE = {
    "196": {
        "head": "170619c331da2d0760666f680b6738626c5f6bbb",
        "run": 38730232981,
        "artifact": 11986008507,
        "artifact_digest": "sha256:3c421408710a211304d0e4ccb5c0865706c630f2f6a5cd8fedb75f7743ea146d",
    },
    "197": {
        "head": "5e64db50c754cdf4be4a03690ce0d374f42110c2",
        "run": 38730229809,
        "artifact": 11990188469,
        "artifact_digest": "sha256:928bdfdd4483e2d567b27fc72089e1b1429d732fa0e027adc824fcb6cee7ffce",
    },
}


def digest(path):
    hashed = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            hashed.update(chunk)
    return hashed.hexdigest()


def run_process(command, cwd, env, timeout, stdout_path, stderr_path=None,
                live=False):
    """Tee output and enforce the original wall-clock limit on our process group."""
    start = time.monotonic()
    timed_out = False
    with stdout_path.open("wb") as output:
        error = stderr_path.open("wb") if stderr_path else None
        try:
            with subprocess.Popen(
                command, cwd=cwd, env=env, stdin=subprocess.DEVNULL,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE if error else subprocess.STDOUT,
                start_new_session=True,
            ) as process:
                try:
                    with selectors.DefaultSelector() as selector:
                        selector.register(process.stdout, selectors.EVENT_READ, output)
                        if error:
                            selector.register(process.stderr, selectors.EVENT_READ, error)
                        last_heartbeat = start
                        while selector.get_map() or process.poll() is None:
                            now = time.monotonic()
                            if not timed_out and now - start >= timeout:
                                timed_out = True
                                try:
                                    os.killpg(process.pid, signal.SIGKILL)
                                except ProcessLookupError:
                                    pass
                            if now - last_heartbeat >= 30:
                                print(f"Still running after {now - start:.1f}s", flush=True)
                                last_heartbeat = now
                            remaining = max(0, timeout - (time.monotonic() - start))
                            poll_interval = 0.25 if timed_out else min(0.25, remaining)
                            for key, _ in selector.select(timeout=poll_interval):
                                chunk = os.read(key.fd, 65536)
                                if not chunk:
                                    selector.unregister(key.fileobj)
                                    continue
                                key.data.write(chunk)
                                key.data.flush()
                                if live:
                                    sys.stdout.buffer.write(chunk)
                                    sys.stdout.buffer.flush()
                        returncode = process.wait()
                except BaseException:
                    try:
                        os.killpg(process.pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass
                    process.wait()
                    raise
        finally:
            if error:
                error.close()
    return {
        "command": command, "elapsed_seconds": time.monotonic() - start,
        "timeout_seconds": timeout, "timed_out": timed_out,
        "exit_code": 99 if timed_out else returncode,
    }


def compiler_environment(prefix):
    env = os.environ.copy()
    # Both arms use the same controlled environment; no user package DB or
    # inherited runtime tuning can make one compiler's workload differ.
    for name in ("GHCRTS", "GHC_ENVIRONMENT", "GHC_PACKAGE_PATH"):
        env.pop(name, None)
    env["PATH"] = str(prefix / "bin") + os.pathsep + env["PATH"]
    return env


def timing_records(log):
    text = log.read_text(errors="replace")
    completed = [
        {"phase": name, "allocated_bytes": int(allocated), "cpu_ms": float(ms)}
        for name, allocated, ms in re.findall(
            r"^(.+?): alloc=(\d+) time=([\d.]+)\s*$", text, re.MULTILINE,
        )
    ]
    started = re.findall(r"^\*\*\* (.+):\s*$", text, re.MULTILINE)
    # Timers may be nested; preserve records, rather than adding overlapping
    # phases or treating an unfinished phase as a measured zero.
    return {
        "completed_phases": completed,
        "last_started_phase": started[-1] if started else None,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", type=Path, required=True)
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--timings", action="store_true",
                        help="enable GHC phase timing; off by default")
    parser.add_argument("--dry-run", action="store_true",
                        help="validate fixtures and print commands without running a compiler")
    args = parser.parse_args()
    prefixes = {"196": args.baseline.resolve(), "197": args.candidate.resolve()}
    for prefix in prefixes.values():
        for relative in ("bin/ghc", str(FIXTURE / "T26537.hs"),
                         str(FIXTURE / "T26537.stdout"), str(FIXTURE / "all.T")):
            if not (prefix / relative).is_file():
                parser.error(f"missing artifact member: {prefix / relative}")
    for relative in ("T26537.hs", "T26537.stdout", "all.T"):
        if digest(prefixes["196"] / FIXTURE / relative) != digest(prefixes["197"] / FIXTURE / relative):
            parser.error(f"baseline and candidate fixtures differ: {relative}")
    declaration = (prefixes["197"] / FIXTURE / "all.T").read_text()
    if not re.search(r"test\('T26537'.*compile_timeout_multiplier\(5\).*'-O2 -fregs-graph'", declaration):
        parser.error("T26537 no longer has the recorded flags/timeout multiplier")
    if not args.dry_run and (sys.platform != "darwin" or platform.machine() != "x86_64"):
        parser.error("the real comparison must run on native Intel macOS")
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    manifest = {
        "platform": platform.platform(), "machine": platform.machine(),
        "logical_cpus": os.cpu_count(),
        "provenance": {label: dict(info) for label, info in PROVENANCE.items()},
        "timings_enabled": args.timings,
        "cache_policy": "identical --info preflight, then one fresh-process clean compilation each; no OS cold-cache claim",
        "trials": [],
    }
    for label, prefix in prefixes.items():
        manifest["provenance"][label]["compiler_sha256"] = digest(prefix / "bin/ghc")
        manifest["provenance"][label]["source_sha256"] = digest(prefix / FIXTURE / "T26537.hs")
        manifest["provenance"][label]["golden_sha256"] = digest(prefix / FIXTURE / "T26537.stdout")
        if not args.dry_run:
            # Same lightweight startup probe for both before any measurements.
            probe = run_process(
                [str(prefix / "bin/ghc"), "--info"], output, compiler_environment(prefix),
                60, output / f"{label}-compiler-info.txt",
            )
            if probe["exit_code"] != 0:
                raise RuntimeError(f"compiler {label} preflight failed: {probe}")
    failed = False
    order = [("first-clean", "196"), ("first-clean", "197")]
    for index, (pair, label) in enumerate(order, 1):
        prefix = prefixes[label]
        trial = output / f"{index:02d}-{label}-{pair}"
        trial.mkdir()
        shutil.copyfile(prefix / FIXTURE / "T26537.hs", trial / "T26537.hs")
        command = [str(prefix / "bin/ghc"), "-o", "T26537", "T26537.hs", *FLAGS]
        if args.timings:
            command.append("-ddump-timings")
        result = {"compiler": label, "pair": pair, "directory": str(trial)}
        print(f"Trial {index}: {pair}, PR {label}: {shlex.join(command)}", flush=True)
        if args.dry_run:
            result["compile"] = {"command": command, "timeout_seconds": COMPILE_TIMEOUT}
        else:
            env = compiler_environment(prefix)
            result["compile"] = run_process(
                command, trial, env, COMPILE_TIMEOUT, trial / "compile.log", live=True,
            )
            result["phase_timings"] = timing_records(trial / "compile.log")
            if result["compile"]["exit_code"] == 0:
                result["run"] = run_process(
                    [str(trial / "T26537")], trial, env, RUN_TIMEOUT,
                    trial / "run.stdout", trial / "run.stderr",
                )
                result["stdout_matches_golden"] = (
                    (trial / "run.stdout").read_bytes()
                    == (prefix / FIXTURE / "T26537.stdout").read_bytes()
                )
                result["stderr_empty"] = (trial / "run.stderr").stat().st_size == 0
                result["passed"] = (
                    result["run"]["exit_code"] == 0
                    and result["stdout_matches_golden"] and result["stderr_empty"]
                )
            else:
                result["passed"] = False
            failed |= not result["passed"]
        manifest["trials"].append(result)
        (trial / "result.json").write_text(json.dumps(result, indent=2) + "\n")
        (output / "comparison.json").write_text(json.dumps(manifest, indent=2) + "\n")
        print(json.dumps(result, indent=2), flush=True)
    return int(failed)


if __name__ == "__main__":
    sys.exit(main())
