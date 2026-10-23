#!/usr/bin/env python3
"""Run focused liblink C units against the current stage2 native archive."""

from __future__ import annotations

import argparse
import os
import platform
import re
import shlex
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
TIMEOUT_SECONDS = 90


def parse_args() -> argparse.Namespace:
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
        "--foundation-only",
        action="store_true",
        help="run only error_state.c; core_error_status.c currently needs POSIX mmap/fd APIs",
    )
    return parser.parse_args()


def unique_path(paths: list[Path], description: str) -> Path:
    paths = sorted(set(paths))
    if len(paths) != 1:
        candidates = "\n".join(f"  {path}" for path in paths) or "  (none)"
        raise RuntimeError(
            f"expected exactly one {description}, found {len(paths)}:\n{candidates}"
        )
    return paths[0]


def locate_artifacts(build_root: Path, store_root: Path) -> tuple[Path, Path, Path]:
    archives = [
        path
        for path in build_root.rglob("liblink.a")
        if path.parent.name == "native-linker"
        and path.parent.parent.name == "build"
        and "liblink-0.1.0.0" in path.parts
    ]
    archive = unique_path(archives, "stage2 native liblink.a")
    config_include = archive.parent.parent / "include"
    for header in ("ll_autoconf.h", "ll_platform.h"):
        if not (config_include / header).is_file():
            raise RuntimeError(f"missing generated liblink header: {config_include / header}")

    fs_headers = [
        path
        for path in store_root.rglob("fs.h")
        if "rts-fs-1.0.0.0" in path.parts and path.parent.name == "include"
    ]
    fs_header = unique_path(fs_headers, "installed rts-fs fs.h")
    return archive, config_include, fs_header.parent


def command_text(argv: list[str]) -> str:
    return shlex.join(str(arg) for arg in argv)


def run(argv: list[str], *, label: str) -> str:
    print(f"[{label}] + {command_text(argv)}", flush=True)
    try:
        result = subprocess.run(
            [str(arg) for arg in argv],
            cwd=ROOT,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=TIMEOUT_SECONDS,
            check=False,
        )
    except subprocess.TimeoutExpired as error:
        stdout = error.stdout or ""
        stderr = error.stderr or ""
        if isinstance(stdout, bytes):
            stdout = stdout.decode(errors="replace")
        if isinstance(stderr, bytes):
            stderr = stderr.decode(errors="replace")
        if stdout:
            print(stdout, end="", flush=True)
        if stderr:
            print(stderr, end="", file=sys.stderr, flush=True)
        raise RuntimeError(f"{label} timed out after {TIMEOUT_SECONDS} seconds") from error

    if result.stdout:
        print(result.stdout, end="", flush=True)
    if result.stderr:
        print(result.stderr, end="", file=sys.stderr, flush=True)
    if result.returncode != 0:
        raise RuntimeError(f"{label} exited with status {result.returncode}")
    return result.stdout + result.stderr


def linker_map_flag(path: Path) -> str:
    host = platform.system()
    if host == "Darwin":
        return f"-Wl,-map,{path}"
    if host == "Linux":
        return f"-Wl,-Map,{path}"
    raise RuntimeError(
        "core and archive status fixtures currently require POSIX mmap/fd APIs; "
        "run --foundation-only on this host or add platform-specific fixtures"
    )


def core_link_flags(host: str) -> list[str]:
    if host == "Linux":
        return ["-pthread", "-ldl"]
    if host == "Darwin":
        return ["-pthread"]
    return []


def verify_link_map(
    map_path: Path,
    archive: Path,
    link_trace: str,
    *,
    included_source: str,
    forbidden_member: str,
) -> None:
    if not map_path.is_file():
        raise RuntimeError(f"linker did not create map file: {map_path}")
    text = map_path.read_text(errors="replace")
    if str(archive) not in text + link_trace:
        raise RuntimeError(f"link did not use expected stage2 archive: {archive}")
    duplicate_source = re.compile(
        rf"liblink\.a\([^)]*{re.escape(forbidden_member)}[^)]*\.o\)"
    )
    if any(duplicate_source.search(line) for line in text.splitlines()):
        raise RuntimeError(
            f"link map selected archive {forbidden_member}.o beside included {included_source}"
        )
    print(
        f"[{included_source}] link map uses the stage2 archive "
        f"without archive {forbidden_member}.o"
    )


def verify_macho_link_map(
    map_path: Path,
    archive: Path,
    link_trace: str,
    *,
    included_source: str,
    forbidden_members: tuple[str, ...],
    required_members: tuple[str, ...] = (),
) -> None:
    if not map_path.is_file():
        raise RuntimeError(f"linker did not create map file: {map_path}")
    text = map_path.read_text(errors="replace")
    evidence = text + link_trace
    if str(archive) not in text or str(archive) not in link_trace:
        raise RuntimeError(f"link did not use expected stage2 archive: {archive}")

    for member in forbidden_members:
        pattern = re.compile(
            rf"{re.escape(str(archive))}"
            rf"\((?:[^)]*[/_])?{re.escape(member)}(?:\.c)?\.(?:o|obj)\)",
            flags=re.IGNORECASE,
        )
        if pattern.search(text):
            raise RuntimeError(
                f"link map selected archive {member}.o beside included {included_source}"
            )

    for member in required_members:
        pattern = re.compile(
            rf"{re.escape(str(archive))}"
            rf"\((?:[^)]*[/_])?{re.escape(member)}(?:\.c)?\.(?:o|obj)\)",
            flags=re.IGNORECASE,
        )
        if not pattern.search(evidence):
            raise RuntimeError(f"link did not select {member} from {archive}")


def main() -> int:
    args = parse_args()
    system = platform.system()
    host = (
        "Windows"
        if os.name == "nt"
        or sys.platform in ("win32", "msys")
        or system.startswith(("MSYS_NT", "MINGW"))
        else system
    )
    if host == "Windows" and not args.foundation_only:
        raise RuntimeError(
            "core_error_status.c is POSIX-only: the Windows linker path uses "
            "stdio rather than mmap and needs a platform-specific fixture"
        )
    if host not in ("Linux", "Darwin", "Windows"):
        raise RuntimeError(f"unsupported host platform for liblink C units: {host}")

    compiler = shlex.split(args.cc)
    if not compiler:
        raise RuntimeError("empty C compiler command")
    archive, config_include, fs_include = locate_artifacts(
        args.build_root.resolve(), args.store_root.resolve()
    )
    print(f"[inputs] compiler={command_text(compiler)}")
    print(f"[inputs] generated_config={config_include}")
    print(f"[inputs] rts_fs_include={fs_include}")
    print(f"[inputs] native_archive={archive}")

    common_flags = [
        "-std=c11",
        "-Wall",
        "-Wextra",
        "-Werror=vla",
        "-pedantic",
        "-DLL_ASSERTS",
        "-isystem",
        str(config_include),
        "-isystem",
        str(fs_include),
        "-I",
        str(ROOT / "liblink/include"),
        "-I",
        str(ROOT / "liblink/src"),
    ]
    cc = compiler + common_flags

    with tempfile.TemporaryDirectory(prefix="liblink-native-units-") as temporary:
        output = Path(temporary)
        error_state = output / "error_state"
        run(
            cc
            + [
                str(ROOT / "liblink/tests/unit/error_state.c"),
                str(ROOT / "liblink/src/ll_error.c"),
                str(ROOT / "liblink/src/ll_host.c"),
                "-o",
                str(error_state),
            ],
            label="compile error_state",
        )
        run([str(error_state)], label="run error_state")

        if args.foundation_only:
            print("liblink native unit tests: PASS (foundation only)")
            return 0

        core_test = output / "core_error_status"
        map_file = output / "core_error_status.map"
        link_trace = run(
            cc
            + [
                str(ROOT / "liblink/tests/unit/core_error_status.c"),
                f"-L{archive.parent}",
                "-llink",
                *core_link_flags(host),
                "-Wl,-t",
                linker_map_flag(map_file),
                "-o",
                str(core_test),
            ],
            label="compile and link core_error_status",
        )
        verify_link_map(
            map_file,
            archive,
            link_trace,
            included_source="Linker.c",
            forbidden_member="Linker",
        )
        run([str(core_test)], label="run core_error_status")

        archive_test = output / "archive_error_status"
        archive_map_file = output / "archive_error_status.map"
        archive_link_trace = run(
            cc
            + [
                str(ROOT / "liblink/tests/unit/archive_error_status.c"),
                f"-L{archive.parent}",
                "-llink",
                *core_link_flags(host),
                "-Wl,-t",
                linker_map_flag(archive_map_file),
                "-o",
                str(archive_test),
            ],
            label="compile and link archive_error_status",
        )
        verify_link_map(
            archive_map_file,
            archive,
            archive_link_trace,
            included_source="LoadArchive.c",
            forbidden_member="LoadArchive",
        )
        run([str(archive_test)], label="run archive_error_status")

        if host == "Darwin":
            macho_import_test = output / "macho_import_status"
            macho_import_map = output / "macho_import_status.map"
            macho_import_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/macho_import_status.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *core_link_flags(host),
                    "-Wl,-t",
                    linker_map_flag(macho_import_map),
                    "-o",
                    str(macho_import_test),
                ],
                label="compile and link macho_import_status",
            )
            verify_macho_link_map(
                macho_import_map,
                archive,
                macho_import_trace,
                included_source="MachO.c",
                forbidden_members=("MachO",),
            )
            run([str(macho_import_test)], label="run macho_import_status")

        riscv_u_type_test = output / "riscv_u_type"
        riscv_u_type_source = output / "riscv_u_type_generated.c"
        run(
            [
                sys.executable,
                str(ROOT / "liblink/tests/unit/riscv_u_type_source.py"),
                "--output",
                str(riscv_u_type_source),
            ],
            label="extract current RISC-V U-type helpers",
        )
        riscv_u_type_map = output / "riscv_u_type.map"
        riscv_u_type_link_trace = run(
            cc
            + [
                str(riscv_u_type_source),
                f"-L{archive.parent}",
                "-llink",
                *core_link_flags(host),
                "-Wl,-t",
                linker_map_flag(riscv_u_type_map),
                "-o",
                str(riscv_u_type_test),
            ],
            label="compile and link riscv_u_type",
        )
        verify_link_map(
            riscv_u_type_map,
            archive,
            riscv_u_type_link_trace,
            included_source="extracted current RISC-V U-type helpers",
            forbidden_member="elf_reloc_riscv64",
        )
        run([str(riscv_u_type_test)], label="run riscv_u_type")

        if host == "Darwin":
            macho_fixture_dir = ROOT / "liblink/tests/fixtures/macho-input"
            macho_objects = {
                "ordinary": output / "macho_ordinary.o",
                "common_only": output / "macho_common_only.o",
                "zero_size": output / "macho_zero_size.o",
            }
            for fixture, source in (
                ("ordinary", "ordinary.c"),
                ("common_only", "common_only.c"),
                ("zero_size", "zero_size.c"),
            ):
                flags = ["-fcommon"] if fixture == "common_only" else []
                run(
                    cc
                    + flags
                    + [
                        "-c",
                        str(macho_fixture_dir / source),
                        "-o",
                        str(macho_objects[fixture]),
                    ],
                    label=f"compile Mach-O {fixture} fixture",
                )

            macho_test = output / "macho_input"
            macho_map_file = output / "macho_input.map"
            macho_link_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/macho_input.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *core_link_flags(host),
                    "-Wl,-t",
                    linker_map_flag(macho_map_file),
                    "-o",
                    str(macho_test),
                ],
                label="compile and link macho_input",
            )
            verify_macho_link_map(
                macho_map_file,
                archive,
                macho_link_trace,
                included_source="MachOVerify selection",
                forbidden_members=(),
                required_members=("MachOVerify",),
            )
            run(
                [
                    str(macho_test),
                    str(macho_objects["ordinary"]),
                    str(macho_objects["common_only"]),
                    str(macho_objects["zero_size"]),
                ],
                label="run macho_input",
            )

            segment_padding_test = output / "macho_segment_padding"
            segment_padding_map = output / "macho_segment_padding.map"
            segment_padding_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/macho_segment_padding.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *core_link_flags(host),
                    "-Wl,-t",
                    linker_map_flag(segment_padding_map),
                    "-o",
                    str(segment_padding_test),
                ],
                label="compile and link macho_segment_padding",
            )
            verify_macho_link_map(
                segment_padding_map,
                archive,
                segment_padding_trace,
                included_source="MachO.c and Linker.c",
                forbidden_members=("MachO", "Linker"),
            )
            run([str(segment_padding_test)], label="run macho_segment_padding")

            macho_prefile_test = output / "macho_prefile_error"
            macho_prefile_map = output / "macho_prefile_error.map"
            macho_prefile_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/macho_prefile_error.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *core_link_flags(host),
                    "-Wl,-t",
                    linker_map_flag(macho_prefile_map),
                    "-o",
                    str(macho_prefile_test),
                ],
                label="compile and link macho_prefile_error",
            )
            verify_macho_link_map(
                macho_prefile_map,
                archive,
                macho_prefile_trace,
                included_source="MachO.c",
                forbidden_members=("MachO",),
                required_members=("MachOVerify",),
            )
            run([str(macho_prefile_test)], label="run macho_prefile_error")

            platform_header = (config_include / "ll_platform.h").read_text()
            if re.search(r"^#define\s+aarch64_HOST_ARCH\s+1\s*$",
                         platform_header, flags=re.MULTILINE):
                macho_status_test = output / "macho_error_status"
                macho_status_map = output / "macho_error_status.map"
                macho_status_trace = run(
                    cc
                    + [
                        str(ROOT / "liblink/tests/unit/macho_error_status.c"),
                        f"-L{archive.parent}",
                        "-llink",
                        *core_link_flags(host),
                        "-Wl,-t",
                        linker_map_flag(macho_status_map),
                        "-o",
                        str(macho_status_test),
                    ],
                    label="compile and link macho_error_status",
                )
                verify_macho_link_map(
                    macho_status_map,
                    archive,
                    macho_status_trace,
                    included_source="MachO.c",
                    forbidden_members=("MachO",),
                )
                run([str(macho_status_test)], label="run macho_error_status")
            else:
                print("[macho_error_status] SKIP: generated target is not AArch64")

            macho_load_errors_test = output / "macho_load_errors"
            macho_load_errors_map = output / "macho_load_errors.map"
            macho_load_errors_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/macho_load_errors.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *core_link_flags(host),
                    "-Wl,-t",
                    linker_map_flag(macho_load_errors_map),
                    "-o",
                    str(macho_load_errors_test),
                ],
                label="compile and link macho_load_errors",
            )
            verify_macho_link_map(
                macho_load_errors_map,
                archive,
                macho_load_errors_trace,
                included_source="MachO.c, SymbolExtras.c, and Linker.c",
                forbidden_members=("MachO", "SymbolExtras", "Linker"),
            )
            run([str(macho_load_errors_test)], label="run macho_load_errors")

    print("liblink native unit tests: PASS")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError) as error:
        print(f"liblink native unit tests: ERROR: {error}", file=sys.stderr)
        raise SystemExit(1) from error
