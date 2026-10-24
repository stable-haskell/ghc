#!/usr/bin/env python3
"""Collect opt-in phase timings for unpack_sums_6 without judging performance."""

import argparse
import base64
from decimal import Decimal, InvalidOperation
import hashlib
import json
import math
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import time


SOURCE = Path(__file__).resolve().parents[2] / "testsuite/tests/unboxedsums/unpack_sums_6.hs"
EXPECTED_STDOUT = SOURCE.with_suffix(".stdout")
DEFAULT_PROBE_TIMEOUT = 60
DEFAULT_COMPILE_TIMEOUT = 180
DEFAULT_RUN_TIMEOUT = 120
DEFAULT_TOTAL_TIMEOUT = 900
REPEATS = 5
TIMING_PREFIX = "GHC_UNPACK_SUMS_TIMINGS "
TIMING_RE = re.compile(
    r"^GHC_UNPACK_SUMS_TIMINGS unlayered=(-?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)s) "
    r"layered=(-?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)s)$"
)


class CollectionError(Exception):
    pass


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def process_group_options():
    return {"start_new_session": True}


def stop_owned_process(proc):
    try:
        os.killpg(proc.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    try:
        proc.wait(timeout=3)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait()


def run_command(argv, cwd, env, timeout):
    start = time.monotonic()
    proc = subprocess.Popen(
        argv,
        cwd=cwd,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        **process_group_options(),
    )
    timed_out = False
    try:
        stdout, stderr = proc.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        timed_out = True
        stop_owned_process(proc)
        stdout, stderr = proc.communicate()
    return {
        "argv": [str(arg) for arg in argv],
        "pid": proc.pid,
        "process_group_id": proc.pid,
        "session_id": proc.pid,
        "cwd": str(cwd),
        "timeout_seconds": timeout,
        "elapsed_seconds": time.monotonic() - start,
        "returncode": proc.returncode,
        "timed_out": timed_out,
        "stdout": stdout,
        "stderr": stderr,
    }


def remaining_timeout(deadline, phase_timeout):
    remaining = deadline - time.monotonic()
    if remaining <= 0:
        raise CollectionError("total collection time limit expired")
    return min(remaining, phase_timeout)


def record_command(manifest, output_dir, name, result):
    stdout_path = output_dir / f"{name}.stdout"
    stderr_path = output_dir / f"{name}.stderr"
    stdout = result.pop("stdout")
    stderr = result.pop("stderr")
    stdout_path.write_bytes(stdout)
    stderr_path.write_bytes(stderr)
    result["stdout_file"] = str(stdout_path)
    result["stderr_file"] = str(stderr_path)
    result["stdout_base64"] = base64.b64encode(stdout).decode("ascii")
    result["stderr_base64"] = base64.b64encode(stderr).decode("ascii")
    manifest.setdefault("commands", {})[name] = result
    write_json(output_dir / "results.json", manifest)
    return result


def parse_timings(stderr_bytes):
    lines = [
        line for line in stderr_bytes.decode("utf-8", errors="replace").splitlines()
        if line.startswith(TIMING_PREFIX)
    ]
    if len(lines) != 1:
        raise CollectionError(f"expected exactly one timing line, found {len(lines)}")
    match = TIMING_RE.fullmatch(lines[0])
    if not match:
        raise CollectionError(f"invalid timing line: {lines[0]!r}")
    values = []
    for value in match.groups():
        try:
            seconds = Decimal(value[:-1])
        except InvalidOperation as error:
            raise CollectionError(f"invalid duration: {value!r}") from error
        if seconds < 0:
            raise CollectionError(f"negative duration: {value!r}")
        values.append({"nominal_diff_time": value, "seconds": str(seconds)})
    return {"unlayered": values[0], "layered": values[1]}


def check_command(result, name, manifest):
    if result["timed_out"]:
        manifest["errors"].append(f"{name} timed out")
        return False
    if result["returncode"] != 0:
        manifest["errors"].append(f"{name} exited with status {result['returncode']}")
        return False
    return True


def collect(args, output_dir, manifest):
    deadline = time.monotonic() + args.total_timeout
    source = SOURCE.resolve(strict=True)
    expected_stdout = EXPECTED_STDOUT.resolve(strict=True).read_bytes()
    compiler = args.ghc.resolve(strict=True)
    if not compiler.is_file() or not os.access(compiler, os.X_OK):
        raise CollectionError(f"compiler is not an executable file: {compiler}")

    executable = output_dir / "unpack_sums_6"
    manifest.update({
        "source": {"path": str(source), "sha256": sha256(source)},
        "expected_stdout": {"path": str(EXPECTED_STDOUT.resolve()), "sha256": hashlib.sha256(expected_stdout).hexdigest()},
        "compiler": {"path": str(compiler), "sha256": sha256(compiler)},
        "compile_flags": ["-O", "-rtsopts", "-no-user-package-db", "-fforce-recomp", "-outputdir", str(output_dir)],
        "repeat_count": REPEATS,
        "total_timeout_seconds": args.total_timeout,
        "host": {
            "platform": sys.platform,
            "uname": list(os.uname()),
            "cpu_count": os.cpu_count(),
            "load_average_start": os.getloadavg(),
            "GHCRTS": os.environ.get("GHCRTS"),
        },
        "predicate": "original strict comparison; exit 0 pass, exit 1 fail",
        "warmup": None,
        "measurements": [],
    })
    child_env = os.environ.copy()
    child_env.pop("GHC_UNPACK_SUMS_TIMINGS", None)

    try:
        info_timeout = remaining_timeout(deadline, args.probe_timeout)
    except CollectionError as error:
        manifest["errors"].append(str(error))
        return manifest
    info = run_command([str(compiler), "--info"], output_dir, child_env, info_timeout)
    info = record_command(manifest, output_dir, "ghc-info", info)
    manifest["compiler_info"] = info
    write_json(output_dir / "results.json", manifest)
    if not check_command(info, "compiler --info", manifest):
        return manifest

    compile_argv = [
        str(compiler), "-O", "-rtsopts", "-no-user-package-db", "-fforce-recomp",
        "-outputdir", str(output_dir),
        str(source), "-o", str(executable),
    ]
    try:
        compile_timeout = remaining_timeout(deadline, args.compile_timeout)
    except CollectionError as error:
        manifest["errors"].append(str(error))
        return manifest
    compile_result = run_command(compile_argv, output_dir, child_env, compile_timeout)
    compile_result = record_command(manifest, output_dir, "compile", compile_result)
    if not check_command(compile_result, "compile", manifest):
        return manifest
    if not executable.is_file():
        manifest["errors"].append("compile returned success without producing the executable")
        return manifest
    manifest["executable_sha256"] = sha256(executable)

    run_env = child_env.copy()
    run_env["GHC_UNPACK_SUMS_TIMINGS"] = "1"
    run_names = ["warmup"] + [f"repeat-{index:02d}" for index in range(1, REPEATS + 1)]
    for name in run_names:
        try:
            timeout = remaining_timeout(deadline, args.run_timeout)
        except CollectionError as error:
            manifest["errors"].append(str(error))
            break
        result = run_command([str(executable)], output_dir, run_env, timeout)
        result = record_command(manifest, output_dir, name, result)
        measurement = {
            "name": name,
            "returncode": result["returncode"],
            "timed_out": result["timed_out"],
            "usable": False,
            "stdout_matches_expected": False,
            "predicate": "unknown",
        }
        stdout = Path(result["stdout_file"]).read_bytes()
        stderr = Path(result["stderr_file"]).read_bytes()
        if result["returncode"] in (0, 1):
            measurement["predicate"] = "pass" if result["returncode"] == 0 else "fail"
        else:
            manifest["errors"].append(
                f"{name} exited with unsupported status {result['returncode']}"
            )
        measurement["stdout_matches_expected"] = stdout == expected_stdout
        if not measurement["stdout_matches_expected"]:
            manifest["errors"].append(f"{name} stdout differs from unpack_sums_6.stdout")
        try:
            measurement["timings"] = parse_timings(stderr)
        except CollectionError as error:
            manifest["errors"].append(f"{name}: {error}")
        measurement["usable"] = (
            result["returncode"] in (0, 1)
            and not result["timed_out"]
            and measurement["stdout_matches_expected"]
            and "timings" in measurement
        )
        if not measurement["usable"] and result["timed_out"]:
            manifest["errors"].append(f"{name} timed out")
        if name == "warmup":
            manifest["warmup"] = measurement
        else:
            manifest["measurements"].append(measurement)
        write_json(output_dir / "results.json", manifest)
    return manifest


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ghc", required=True, type=Path, help="GHC executable to measure")
    parser.add_argument("--output-dir", required=True, type=Path, help="new directory for raw results")
    parser.add_argument("--probe-timeout", type=float, default=DEFAULT_PROBE_TIMEOUT)
    parser.add_argument("--compile-timeout", type=float, default=DEFAULT_COMPILE_TIMEOUT)
    parser.add_argument("--run-timeout", type=float, default=DEFAULT_RUN_TIMEOUT)
    parser.add_argument("--total-timeout", type=float, default=DEFAULT_TOTAL_TIMEOUT)
    args = parser.parse_args()
    timeouts = (
        ("probe", args.probe_timeout, DEFAULT_PROBE_TIMEOUT),
        ("compile", args.compile_timeout, DEFAULT_COMPILE_TIMEOUT),
        ("run", args.run_timeout, DEFAULT_RUN_TIMEOUT),
        ("total", args.total_timeout, DEFAULT_TOTAL_TIMEOUT),
    )
    for name, value, maximum in timeouts:
        if not math.isfinite(value) or value <= 0 or value > maximum:
            parser.error(f"{name} timeout must be in (0, {maximum}] seconds")
    return args


def main():
    args = parse_args()
    if os.name != "posix":
        print("the diagnostic collector currently requires POSIX process groups", file=sys.stderr)
        return 2
    output_dir = args.output_dir.expanduser().absolute()
    if output_dir.exists():
        print(f"refusing to reuse output directory: {output_dir}", file=sys.stderr)
        return 2
    try:
        output_dir.mkdir(parents=True)
    except OSError as error:
        print(f"cannot create output directory {output_dir}: {error}", file=sys.stderr)
        return 2

    manifest = {
        "schema": "unpack-sums-timing-v1",
        "output_dir": str(output_dir),
        "measurements": [],
        "warmup": None,
        "commands": {},
        "errors": [],
    }
    try:
        manifest = collect(args, output_dir, manifest)
    except (CollectionError, OSError) as error:
        manifest["errors"].append(str(error))
    write_json(output_dir / "results.json", manifest)
    complete = (
        manifest.get("warmup") is not None
        and manifest["warmup"].get("usable")
        and len(manifest["measurements"]) == REPEATS
        and all(sample.get("usable") for sample in manifest["measurements"])
    )
    if manifest["errors"] or not complete:
        print(f"collection incomplete: {output_dir / 'results.json'}", file=sys.stderr)
        for error in manifest["errors"]:
            print(f"- {error}", file=sys.stderr)
        return 1
    print(f"collected warmup and {REPEATS} measurements in {output_dir}")
    print("predicate statuses are diagnostic data; this is not benchmark acceptance")
    return 0


if __name__ == "__main__":
    sys.exit(main())
