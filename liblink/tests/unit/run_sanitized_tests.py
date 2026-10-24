#!/usr/bin/env python3
"""Build and run the native liblink C units with ASan and UBSan enabled."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import shlex
import signal
import subprocess
import sys
import time
from pathlib import Path

import run_native_tests


ROOT = Path(__file__).resolve().parents[3]
LIBLINK = ROOT / "liblink"
SOURCE_LIST = LIBLINK / "native-sources.list"
SANITIZER_FLAGS = [
    "-fsanitize=address,undefined",
    "-fno-sanitize-recover=undefined",
    "-fno-omit-frame-pointer",
]
STRICT_FLAGS = [
    "-std=c11",
    "-Wall",
    "-Wextra",
    "-Werror=vla",
    "-pedantic",
    "-DLL_ASSERTS",
    "-fvisibility=hidden",
    "-fPIC",
    *SANITIZER_FLAGS,
]


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--build-root",
        type=Path,
        default=ROOT / "_build/stage2/build/host",
        help="stage2 host build tree (default: %(default)s)",
    )
    parser.add_argument(
        "--store-root",
        type=Path,
        default=ROOT / "_build/stage2/store/host",
        help="stage2 host package store (default: %(default)s)",
    )
    parser.add_argument(
        "--cc",
        default=os.environ.get("CC", "cc"),
        help="C compiler command (default: CC or cc)",
    )
    parser.add_argument(
        "--ar",
        default=os.environ.get("AR", "ar"),
        help="static archive tool command (default: AR or ar)",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        required=True,
        help="new or empty directory for objects, archive, and JSON report",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=900,
        help="overall wall-clock limit in seconds (default: %(default)s)",
    )
    parser.add_argument(
        "--command-timeout",
        type=int,
        default=90,
        help="per-command wall-clock limit in seconds (default: %(default)s)",
    )
    return parser.parse_args(argv)


def native_host() -> str:
    return run_native_tests.native_host()


def source_files() -> list[Path]:
    sources: list[Path] = []
    for line in SOURCE_LIST.read_text().splitlines():
        item = line.strip()
        if item and not item.startswith("#"):
            source = (LIBLINK / item).resolve()
            if not source.is_relative_to(LIBLINK.resolve()) or not source.is_file():
                raise RuntimeError(f"invalid native source manifest entry: {item}")
            sources.append(source)
    if not sources:
        raise RuntimeError(f"native source manifest is empty: {SOURCE_LIST}")
    names = [source.with_suffix(".o").name for source in sources]
    duplicates = sorted({name for name in names if names.count(name) > 1})
    if duplicates:
        raise RuntimeError(
            "native archive map attribution requires unique source basenames: "
            + ", ".join(duplicates)
        )
    return sources


def input_files(
    sources: list[Path],
    config_include: Path,
    fs_include: Path,
    fs_archive: Path | None,
) -> list[Path]:
    files = {SOURCE_LIST.resolve(), *sources}
    for directory in (LIBLINK / "include", LIBLINK / "src", config_include, fs_include):
        if not directory.is_dir():
            raise RuntimeError(f"missing required input directory: {directory}")
        files.update(path.resolve() for path in directory.rglob("*") if path.is_file())
    if fs_archive is not None:
        files.add(fs_archive.resolve())
    return sorted(files)


def hash_inputs(files: list[Path]) -> dict[str, str]:
    hashes: dict[str, str] = {}
    root = ROOT.resolve()
    for path in files:
        try:
            label = path.relative_to(root).as_posix()
        except ValueError:
            label = str(path)
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        hashes[label] = digest
    return hashes


class BoundedCommands:
    def __init__(self, deadline: float, command_timeout: int) -> None:
        self.deadline = deadline
        self.command_timeout = command_timeout
        self.commands: list[dict[str, object]] = []

    def run(self, argv: list[str], *, label: str) -> None:
        remaining = self.deadline - time.monotonic()
        if remaining <= 0:
            raise RuntimeError("overall sanitizer run timed out")
        timeout = min(float(self.command_timeout), remaining)
        print(f"[{label}] + {shlex.join(argv)}", flush=True)
        record: dict[str, object] = {
            "label": label,
            "argv": argv,
            "timeout_seconds": timeout,
            "exit_code": None,
        }
        self.commands.append(record)
        process = subprocess.Popen(
            argv,
            cwd=ROOT,
            stdin=subprocess.DEVNULL,
            start_new_session=True,
        )
        try:
            return_code = process.wait(timeout=timeout)
        except subprocess.TimeoutExpired as error:
            record["outcome"] = "timeout"
            try:
                os.killpg(process.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                pass
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.wait()
            raise RuntimeError(f"{label} timed out after {timeout:.1f} seconds") from error
        record["exit_code"] = return_code
        record["outcome"] = "passed" if return_code == 0 else "failed"
        if return_code != 0:
            raise RuntimeError(f"{label} exited with status {return_code}")


def ensure_empty_output(path: Path) -> Path:
    output = path.expanduser().resolve()
    if output.exists():
        if not output.is_dir() or any(output.iterdir()):
            raise RuntimeError(f"output directory must be new or empty: {output}")
    else:
        output.mkdir(parents=True)
    return output


def write_report(output: Path, report: dict[str, object]) -> None:
    (output / "sanitized-run.json").write_text(
        json.dumps(report, indent=2, sort_keys=True) + "\n"
    )


def execute(args: argparse.Namespace) -> tuple[dict[str, object], int]:
    deadline = time.monotonic() + max(0, args.timeout)
    host = native_host()
    report: dict[str, object] = {
        "host": host,
        "machine": platform.machine(),
        "outcome": "failed",
        "commands": [],
    }
    output: Path | None = None
    try:
        if host not in ("Linux", "Darwin"):
            raise RuntimeError(f"sanitized native tests support Linux and Darwin, got {host}")
        if args.timeout <= 0 or args.command_timeout <= 0:
            raise RuntimeError("timeouts must be positive")
        output = ensure_empty_output(args.output_dir)
        _archive, config_include, fs_include = run_native_tests.locate_artifacts(
            args.build_root.expanduser().resolve(), args.store_root.expanduser().resolve()
        )
        fs_archive = run_native_tests.locate_rts_fs_archive(fs_include, host)
        sources = source_files()
        inputs = input_files(sources, config_include, fs_include, fs_archive)
        before = hash_inputs(inputs)
        report.update(
            {
                "source_manifest": str(SOURCE_LIST),
                "source_manifest_sha256": before[str(SOURCE_LIST.relative_to(ROOT))],
                "sources": [str(path) for path in sources],
                "config_include": str(config_include),
                "fs_include": str(fs_include),
                "rts_fs_archive": str(fs_archive) if fs_archive is not None else None,
                "inputs_before_sha256": before,
                "archive": str(output / "liblink.a"),
                "compiler": shlex.split(args.cc),
                "archiver": shlex.split(args.ar),
                "sanitizer_environment": {
                    name: os.environ.get(name)
                    for name in ("ASAN_OPTIONS", "UBSAN_OPTIONS")
                },
            }
        )
        compiler = shlex.split(args.cc)
        archiver = shlex.split(args.ar)
        if not compiler or not archiver:
            raise RuntimeError("compiler and archiver commands must not be empty")

        commands = BoundedCommands(deadline, args.command_timeout)
        objects: list[Path] = []
        for source in sources:
            relative = source.relative_to(LIBLINK.resolve())
            obj = output / "obj" / relative.with_suffix(".o")
            obj.parent.mkdir(parents=True, exist_ok=True)
            argv = [
                *compiler,
                *STRICT_FLAGS,
                "-isystem",
                str(config_include),
                "-isystem",
                str(fs_include),
                "-I",
                str(LIBLINK / "include"),
                "-I",
                str(LIBLINK / "src"),
                "-c",
                str(source),
                "-o",
                str(obj),
            ]
            commands.run(argv, label=f"compile {relative.as_posix()}")
            objects.append(obj)

        native_archive = output / "liblink.a"
        commands.run(
            [*archiver, "rcs", str(native_archive), *(str(obj) for obj in objects)],
            label="archive sanitized liblink",
        )
        if not native_archive.is_file():
            raise RuntimeError(f"archiver did not create the fresh archive: {native_archive}")
        archive_hash = hashlib.sha256(native_archive.read_bytes()).hexdigest()
        report["archive_sha256"] = archive_hash
        test_cc = shlex.join([*compiler, *SANITIZER_FLAGS])
        unit_argv = [
            sys.executable,
            str(LIBLINK / "tests/unit/run_native_tests.py"),
            "--build-root",
            str(args.build_root.expanduser().resolve()),
            "--store-root",
            str(args.store_root.expanduser().resolve()),
            "--native-archive",
            str(native_archive),
            "--cc",
            test_cc,
        ]
        commands.run(unit_argv, label="run native C units with sanitizers")
        after = hash_inputs(inputs)
        report["inputs_after_sha256"] = after
        if before != after:
            raise RuntimeError("source, header, config, or rts-fs inputs changed during run")
        report["archive_sha256_after_tests"] = hashlib.sha256(
            native_archive.read_bytes()
        ).hexdigest()
        if report["archive_sha256_after_tests"] != archive_hash:
            raise RuntimeError("sanitized archive changed during native unit execution")
        report["outcome"] = "passed"
        report["commands"] = commands.commands
        return report, 0
    except Exception as error:
        report["error"] = f"{type(error).__name__}: {error}"
        if "commands" in locals():
            report["commands"] = commands.commands
        return report, 1
    finally:
        if output is not None:
            native_archive = output / "liblink.a"
            if native_archive.is_file():
                report["archive_sha256_observed"] = hashlib.sha256(
                    native_archive.read_bytes()
                ).hexdigest()
            if "inputs_after_sha256" not in report and "inputs" in locals():
                try:
                    report["inputs_after_sha256"] = hash_inputs(inputs)
                except OSError as error:
                    report["input_hash_error"] = str(error)
            report["report_file"] = str(output / "sanitized-run.json")
            write_report(output, report)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    report, exit_code = execute(args)
    print(f"sanitized native unit run: {report['outcome']}", flush=True)
    if "error" in report:
        print(report["error"], file=sys.stderr, flush=True)
    if "report_file" in report:
        print(f"report: {report['report_file']}", flush=True)
    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
