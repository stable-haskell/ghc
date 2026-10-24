#!/usr/bin/env python3
"""Run focused liblink C units against the current stage2 native archive."""

from __future__ import annotations

import argparse
import os
import platform
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

import link_trace_checks


ROOT = Path(__file__).resolve().parents[3]
TIMEOUT_SECONDS = 90
PE_SUPPORT_LIBS = ["-ldbghelp", "-lpsapi"]


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
        "--native-archive",
        type=Path,
        help="use this freshly built native liblink archive instead of the stage2 archive",
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
    parser.add_argument(
        "--protection-only",
        action="store_true",
        help="run only the focused Darwin Mach-O memory-protection cases",
    )
    parser.add_argument(
        "--protection-gap-probes",
        action="store_true",
        help="run Darwin quarantine, recovery, and allocator retry regressions",
    )
    parser.add_argument(
        "--resolution-failure-only",
        action="store_true",
        help="run Darwin terminal backend-resolution quarantine regressions",
    )
    parser.add_argument(
        "--windows-m32-only",
        action="store_true",
        help="run only the native Windows M32 and PE COMMON ownership units",
    )
    parser.add_argument(
        "--macho-table-alignment-only",
        action="store_true",
        help="run only packed Mach-O symbol/relocation table alignment cases",
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
    if host == "Windows" or host.startswith(("MSYS_NT", "MINGW")):
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


def configured_riscv64_elf_backend(config_include: Path) -> bool:
    platform_header = config_include / "ll_platform.h"
    text = platform_header.read_text(errors="replace")
    linux = re.search(
        r"^\s*#\s*define\s+linux_HOST_OS(?:\s+1)?\s*$",
        text,
        re.MULTILINE,
    )
    riscv64 = re.search(
        r"^\s*#\s*define\s+riscv64_HOST_ARCH(?:\s+1)?\s*$",
        text,
        re.MULTILINE,
    )
    return linux is not None and riscv64 is not None


def configured_linux_x86_64_elf_backend(config_include: Path) -> bool:
    platform_header = config_include / "ll_platform.h"
    text = platform_header.read_text(errors="replace")
    linux = re.search(
        r"^\s*#\s*define\s+linux_HOST_OS(?:\s+1)?\s*$",
        text,
        re.MULTILINE,
    )
    x86_64 = re.search(
        r"^\s*#\s*define\s+x86_64_HOST_ARCH(?:\s+1)?\s*$",
        text,
        re.MULTILINE,
    )
    return linux is not None and x86_64 is not None


def configured_linux_aarch64_elf_backend(config_include: Path) -> bool:
    platform_header = config_include / "ll_platform.h"
    text = platform_header.read_text(errors="replace")
    linux = re.search(
        r"^\s*#\s*define\s+linux_HOST_OS(?:\s+1)?\s*$",
        text,
        re.MULTILINE,
    )
    aarch64 = re.search(
        r"^\s*#\s*define\s+aarch64_HOST_ARCH(?:\s+1)?\s*$",
        text,
        re.MULTILINE,
    )
    return linux is not None and aarch64 is not None


def normalize_windows_path(path: str) -> str:
    return path.replace("\\", "/").casefold()


def windows_path_forms(path: Path) -> list[str]:
    forms = [str(path), path.as_posix()]
    cygpath = shutil.which("cygpath")
    if cygpath is not None:
        try:
            result = subprocess.run(
                [cygpath, "-m", str(path)],
                stdin=subprocess.DEVNULL,
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                text=True,
                timeout=5,
                check=False,
            )
        except (OSError, subprocess.TimeoutExpired):
            result = None
        if result is not None and result.returncode == 0:
            converted = result.stdout.strip()
            if converted:
                forms.append(converted)
    return list(dict.fromkeys(normalize_windows_path(form) for form in forms))


def windows_path_matches(forms: list[str], text: str) -> bool:
    normalized = normalize_windows_path(text)
    return any(normalize_windows_path(form) in normalized for form in forms)


def archive_member_pattern(archive_form: str, member: str) -> re.Pattern[str]:
    return re.compile(
        rf"{re.escape(archive_form)}"
        rf"\([^)]*{re.escape(member)}(?:\.c)?\.(?:o|obj)\)",
        flags=re.IGNORECASE,
    )


def verify_link_map(
    map_path: Path,
    archive: Path,
    link_trace: str,
    *,
    included_source: str,
    forbidden_member: str,
    windows_paths: bool = False,
) -> None:
    if not map_path.is_file():
        raise RuntimeError(f"linker did not create map file: {map_path}")
    text = map_path.read_text(errors="replace")
    if windows_paths:
        forms = windows_path_forms(archive)
        if link_trace_checks.is_lld_mingw_verbose_trace(link_trace):
            link_trace_checks.verify_lld_mingw_archive_map(
                text,
                archive,
                link_trace,
                included_source=included_source,
                forbidden_members=(forbidden_member,),
            )
            print(
                f"[{included_source}] LLD trace selects a member represented in the map "
                f"without archive {forbidden_member}.o"
            )
            return
        if not windows_path_matches(forms, text):
            raise RuntimeError(f"link map omitted expected stage2 archive: {archive}")
        if not windows_path_matches(forms, link_trace):
            raise RuntimeError(f"link trace omitted expected stage2 archive: {archive}")
        normalized_text = normalize_windows_path(text)
        duplicate = any(
            archive_member_pattern(form, forbidden_member).search(normalized_text)
            for form in forms
        )
    else:
        if str(archive) not in text + link_trace:
            raise RuntimeError(f"link did not use expected stage2 archive: {archive}")
        duplicate_source = re.compile(
            rf"liblink\.a\([^)]*{re.escape(forbidden_member)}(?:\.c)?(?:\.o|\.obj)\)"
        )
        duplicate = any(duplicate_source.search(line) for line in text.splitlines())
    if duplicate:
        raise RuntimeError(
            f"link map selected archive {forbidden_member}.o beside included {included_source}"
        )
    print(
        f"[{included_source}] link map uses the stage2 archive "
        f"without archive {forbidden_member}.o"
    )


def verify_overlay_include_argv() -> None:
    synthetic = ["clang", "-I", "src", "-c", "fixture.c"]
    pair = synthetic.index("-I")
    synthetic[pair:pair] = ["-I", "overlay", "-I", "linker"]
    expected = ["clang", "-I", "overlay", "-I", "linker",
                "-I", "src", "-c", "fixture.c"]
    if synthetic != expected:
        raise RuntimeError("synthetic compiler argv overlay probe failed")
    for index, token in enumerate(synthetic[:-1]):
        if token == "src" and synthetic[index - 1] != "-I":
            raise RuntimeError("synthetic compiler argv has a bare include path")
    link_args = ["-llink", *PE_SUPPORT_LIBS]
    if link_args != ["-llink", "-ldbghelp", "-lpsapi"]:
        raise RuntimeError("synthetic PE support-library argv probe failed")


def run_pei386_common_ownership(cc: list[str], archive: Path,
                                output: Path) -> None:
    verify_overlay_include_argv()
    test_source = ROOT / "liblink/tests/unit/pei386_common_ownership.c"
    test_binary = output / "pei386_common_ownership.exe"
    test_map = output / "pei386_common_ownership.map"
    trace = run(
        cc
        + [
            str(test_source),
            f"-L{archive.parent}",
            "-llink",
            *PE_SUPPORT_LIBS,
            # MinGW LLD rejects GNU ld's -t; --verbose prints resolved inputs.
            "-Wl,--verbose",
            linker_map_flag(test_map),
            "-o",
            str(test_binary),
        ],
        label="compile and link pei386_common_ownership",
    )
    included = (
        "PEi386.c, Linker.c, M32Alloc.c, M32Reservations.c, and MMap.c"
    )
    for member in ("PEi386", "Linker", "M32Alloc", "M32Reservations", "MMap"):
        verify_link_map(
            test_map,
            archive,
            trace,
            included_source=included,
            forbidden_member=member,
            windows_paths=True,
        )
    run([str(test_binary)], label="run pei386_common_ownership")

    # Compile an isolated one-line old-tag mutation. The fixture ledger spots
    # the M32 address at host_free and deliberately declines to call free().
    source_text = (ROOT / "liblink/src/linker/PEi386.c").read_text()
    old_line = (
        "SECTIONKIND_RWDATA, SECTION_M32,\n"
        "                  bss, globalBssSize, 0, 0, 0);"
    )
    if source_text.count(old_line) != 1:
        raise RuntimeError("could not uniquely locate the approved PE COMMON tag")
    overlay = output / "old-tag-overlay"
    overlay_file = overlay / "linker/PEi386.c"
    overlay_file.parent.mkdir(parents=True, exist_ok=True)
    overlay_file.write_text(
        source_text.replace(
            old_line,
            old_line.replace("SECTION_M32", "SECTION_MALLOC"),
            1,
        )
    )
    mutant_cc = list(cc)
    source_include = str(ROOT / "liblink/src")
    source_include_index = mutant_cc.index(source_include)
    overlay_includes = ["-I", str(overlay), "-I", str(ROOT / "liblink/src/linker")]
    include_pair_index = source_include_index - 1
    if mutant_cc[include_pair_index] != "-I":
        raise RuntimeError("could not locate the existing source -I pair")
    mutant_cc[include_pair_index:include_pair_index] = overlay_includes
    expected_include_window = [
        "-I", str(overlay), "-I", str(ROOT / "liblink/src/linker"),
        "-I", source_include,
    ]
    if mutant_cc[include_pair_index:include_pair_index + 6] != expected_include_window:
        raise RuntimeError("old-tag overlay include argv order is malformed")
    mutant_binary = output / "pei386_common_ownership_old_tag.exe"
    mutant_map = output / "pei386_common_ownership_old_tag.map"
    mutant_trace = run(
        mutant_cc
        + [
            str(test_source),
            f"-L{archive.parent}",
            "-llink",
            *PE_SUPPORT_LIBS,
            "-Wl,--verbose",
            linker_map_flag(mutant_map),
            "-o",
            str(mutant_binary),
        ],
        label="compile old-tag ownership regression mutant",
    )
    for member in ("PEi386", "Linker", "M32Alloc", "M32Reservations", "MMap"):
        verify_link_map(
            mutant_map,
            archive,
            mutant_trace,
            included_source=included,
            forbidden_member=member,
            windows_paths=True,
        )
    print(
        "[run old-tag ownership regression mutant] + "
        + command_text([mutant_binary]),
        flush=True,
    )
    try:
        mutant_result = subprocess.run(
            [str(mutant_binary)],
            cwd=ROOT,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=TIMEOUT_SECONDS,
            check=False,
        )
    except subprocess.TimeoutExpired as error:
        raise RuntimeError("old-tag mutant timed out") from error
    mutant_output = mutant_result.stdout + mutant_result.stderr
    expected_diagnostic = "PE COMMON M32 BSS passed to host_free"
    if mutant_result.returncode == 0 or expected_diagnostic not in mutant_output:
        print(mutant_output, end="", file=sys.stderr)
        raise RuntimeError(
            "old-tag mutant did not produce the bounded host-free ledger failure"
        )
    print(
        "[old-tag mutation] PASS: host-free ledger detected the stale tag; "
        "the fixture did not free the M32 address",
        flush=True,
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


def run_macho_load_errors(cc: list[str], archive: Path, output: Path,
                          host: str, *, mode: str = "") -> None:
    macho_test = output / "macho_load_errors"
    macho_map = output / "macho_load_errors.map"
    macho_trace = run(
        cc
        + [
            str(ROOT / "liblink/tests/unit/macho_load_errors.c"),
            f"-L{archive.parent}",
            "-llink",
            *core_link_flags(host),
            "-Wl,-t",
            linker_map_flag(macho_map),
            "-o",
            str(macho_test),
        ],
        label="compile and link macho_load_errors",
    )
    verify_macho_link_map(
        macho_map,
        archive,
        macho_trace,
        included_source="MachO.c, SymbolExtras.c, Linker.c, M32Alloc.c, and MMap.c",
        forbidden_members=("MachO", "SymbolExtras", "Linker", "M32Alloc", "MMap"),
    )
    command = [str(macho_test)]
    if mode:
        command.append(mode)
    run(command, label="run macho_load_errors")


def run_macho_table_alignment(cc: list[str], archive: Path, output: Path,
                              host: str) -> None:
    if host != "Darwin":
        raise RuntimeError("--macho-table-alignment-only requires Darwin")
    fixture_dir = ROOT / "liblink/tests/fixtures/macho-input"
    fixture = output / "macho_table_alignment_data.o"
    run(
        cc + ["-fno-sanitize=all", "-fcommon", "-c",
              str(fixture_dir / "table_alignment_data.c"),
              "-o", str(fixture)],
        label="compile Mach-O table-alignment fixture",
    )
    test = output / "macho_table_alignment"
    map_file = output / "macho_table_alignment.map"
    trace = run(
        cc + [str(ROOT / "liblink/tests/unit/macho_table_alignment.c"),
              f"-L{archive.parent}", "-llink", *core_link_flags(host), "-Wl,-t",
              linker_map_flag(map_file), "-o", str(test)],
        label="compile and link macho_table_alignment",
    )
    verify_macho_link_map(
        map_file,
        archive,
        trace,
        included_source="macho_input.c test harness",
        forbidden_members=(),
        required_members=("MachOVerify",),
    )
    run([str(test), str(fixture)], label="run macho_table_alignment")


def main() -> int:
    args = parse_args()
    selected_modes = sum((args.foundation_only, args.protection_only,
                          args.protection_gap_probes,
                          args.resolution_failure_only, args.windows_m32_only,
                          args.macho_table_alignment_only))
    if selected_modes > 1:
        raise RuntimeError("native unit modes are mutually exclusive")
    system = platform.system()
    host = (
        "Windows"
        if os.name == "nt"
        or sys.platform in ("win32", "msys")
        or system.startswith(("MSYS_NT", "MINGW"))
        else system
    )
    if args.windows_m32_only and host != "Windows":
        raise RuntimeError("--windows-m32-only requires a native Windows host")
    if args.macho_table_alignment_only and host != "Darwin":
        raise RuntimeError("--macho-table-alignment-only requires a native Darwin Mach-O build")
    if host == "Windows" and not (args.foundation_only or args.windows_m32_only):
        raise RuntimeError(
            "core_error_status.c is POSIX-only: the Windows linker path uses "
            "stdio rather than mmap; use --windows-m32-only for the M32 fixture"
        )
    if (args.protection_only or args.protection_gap_probes or
            args.resolution_failure_only) and host != "Darwin":
        raise RuntimeError("protection modes require a native Darwin Mach-O build")
    if host not in ("Linux", "Darwin", "Windows"):
        raise RuntimeError(f"unsupported host platform for liblink C units: {host}")

    run(
        [sys.executable, str(ROOT / "liblink/tests/unit/test_link_trace_checks.py")],
        label="linker trace parser tests",
    )

    compiler = shlex.split(args.cc)
    if not compiler:
        raise RuntimeError("empty C compiler command")
    archive, config_include, fs_include = locate_artifacts(
        args.build_root.resolve(), args.store_root.resolve()
    )
    if args.native_archive is not None:
        archive = args.native_archive.resolve()
        if not archive.is_file():
            raise RuntimeError(f"native liblink archive does not exist: {archive}")
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

    if args.macho_table_alignment_only:
        with tempfile.TemporaryDirectory(prefix="liblink-macho-table-alignment-") as temporary:
            run_macho_table_alignment(cc, archive, Path(temporary), host)
        print("liblink Mach-O table alignment unit: PASS")
        return 0

    with tempfile.TemporaryDirectory(prefix="liblink-native-units-") as temporary:
        output = Path(temporary)
        if args.windows_m32_only:
            m32_test = output / "m32_windows_reservations.exe"
            m32_map = output / "m32_windows_reservations.map"
            m32_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/m32_windows_reservations.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    # MinGW LLD rejects GNU ld's -t; --verbose prints resolved inputs.
                    "-Wl,--verbose",
                    linker_map_flag(m32_map),
                    "-o",
                    str(m32_test),
                ],
                label="compile and link m32_windows_reservations",
            )
            for member in ("M32Alloc", "M32Reservations", "MMap"):
                verify_link_map(
                    m32_map,
                    archive,
                    m32_trace,
                    included_source=(
                        "M32Alloc.c, M32Reservations.c, and MMap.c"
                    ),
                    forbidden_member=member,
                    windows_paths=True,
                )
            run([str(m32_test)], label="run m32_windows_reservations")
            run_pei386_common_ownership(cc, archive, output)
            print("liblink native unit tests: PASS (Windows M32/PE ownership only)")
            return 0

        error_state = output / "error_state"
        if not args.protection_only and not args.protection_gap_probes and \
                not args.resolution_failure_only:
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

        if (args.protection_only or args.protection_gap_probes or
                args.resolution_failure_only):
            mode = "--protection-only" if args.protection_only \
                else "--protection-gap-probes" if args.protection_gap_probes \
                else "--resolution-failure-only"
            run_macho_load_errors(cc, archive, output, host, mode=mode)
            outcome = "protection only" if args.protection_only \
                else "quarantine regression cases" if args.protection_gap_probes \
                else "terminal resolution quarantine cases"
            print(f"liblink native unit tests: PASS ({outcome})")
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
            m32_map_test = output / "m32_map_status"
            m32_map_file = output / "m32_map_status.map"
            m32_map_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/m32_map_status.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *core_link_flags(host),
                    "-Wl,-t",
                    "-Wl,-why_load",
                    linker_map_flag(m32_map_file),
                    "-o",
                    str(m32_map_test),
                ],
                label="compile and link m32_map_status",
            )
            verify_macho_link_map(
                m32_map_file,
                archive,
                m32_map_trace,
                included_source="M32Alloc.c",
                forbidden_members=("M32Alloc",),
            )
            link_trace_checks.verify_macho_archive_selection(
                archive,
                m32_map_trace,
                included_source="M32Alloc.c",
                forbidden_members=("M32Alloc",),
            )
            run([str(m32_map_test)], label="run m32_map_status")

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

        if host == "Linux" and configured_riscv64_elf_backend(config_include):
            riscv_test = output / "riscv64_reloc_errors"
            riscv_map = output / "riscv64_reloc_errors.map"
            riscv_link_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/riscv64_reloc_errors.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *core_link_flags(host),
                    "-Wl,-t",
                    linker_map_flag(riscv_map),
                    "-o",
                    str(riscv_test),
                ],
                label="compile and link riscv64_reloc_errors",
            )
            verify_link_map(
                riscv_map,
                archive,
                riscv_link_trace,
                included_source="linker/elf_reloc_riscv64.c",
                forbidden_member="elf_reloc_riscv64",
            )
            run([str(riscv_test)], label="run riscv64_reloc_errors")
        else:
            print(
                "[riscv64_reloc_errors] skipped: generated config is not "
                "riscv64_HOST_ARCH + linux_HOST_OS"
            )

        if host == "Linux" and configured_linux_aarch64_elf_backend(config_include):
            aarch64_test = output / "aarch64_reloc_errors"
            aarch64_map = output / "aarch64_reloc_errors.map"
            aarch64_link_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/aarch64_reloc_errors.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *core_link_flags(host),
                    "-Wl,-t",
                    linker_map_flag(aarch64_map),
                    "-o",
                    str(aarch64_test),
                ],
                label="compile and link aarch64_reloc_errors",
            )
            verify_link_map(
                aarch64_map,
                archive,
                aarch64_link_trace,
                included_source="linker/elf_reloc_aarch64.c",
                forbidden_member="elf_reloc_aarch64",
            )
            run([str(aarch64_test)], label="run aarch64_reloc_errors")
        else:
            print(
                "[aarch64_reloc_errors] skipped: generated config is not "
                "aarch64_HOST_ARCH + linux_HOST_OS"
            )

        if host == "Linux" and configured_linux_x86_64_elf_backend(config_include):
            elf_test = output / "elf_relocation_errors"
            elf_map = output / "elf_relocation_errors.map"
            elf_link_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/elf_relocation_errors.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *core_link_flags(host),
                    "-Wl,-t",
                    linker_map_flag(elf_map),
                    "-o",
                    str(elf_test),
                ],
                label="compile and link elf_relocation_errors",
            )
            verify_link_map(
                elf_map,
                archive,
                elf_link_trace,
                included_source="Elf.c",
                forbidden_member="Elf",
            )
            run([str(elf_test)], label="run elf_relocation_errors")

            section_test = output / "elf_section_errors"
            section_map = output / "elf_section_errors.map"
            section_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/elf_section_errors.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *core_link_flags(host),
                    "-Wl,--wrap=ll_mmap_anon_for_linker",
                    "-Wl,--wrap=ll_mmap_for_linker",
                    "-Wl,--wrap=ll_munmap_for_linker",
                    "-Wl,--wrap=m32_alloc_for",
                    "-Wl,--wrap=open",
                    "-Wl,--wrap=close",
                    "-Wl,-t",
                    linker_map_flag(section_map),
                    "-o",
                    str(section_test),
                ],
                label="compile and link elf_section_errors",
            )
            section_map_text = section_map.read_text(errors="replace")
            if str(archive) not in section_trace:
                raise RuntimeError(
                    f"section link trace omitted expected archive: {archive}"
                )
            for member in ("Elf", "Linker", "MMap", "M32Alloc"):
                pattern = archive_member_pattern(str(archive), member)
                if not pattern.search(section_map_text):
                    raise RuntimeError(
                        f"section link map omitted {archive}({member}.o)"
                    )
            run([str(section_test)], label="run elf_section_errors")
        else:
            print(
                "[elf_relocation_errors] skipped: generated config is not "
                "x86_64_HOST_ARCH + linux_HOST_OS"
            )
            print("[elf_section_errors] skipped: requires generated Linux x86_64 ELF config")

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

            run_macho_table_alignment(cc, archive, output, host)

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

            if re.search(r"^#define\s+x86_64_HOST_ARCH\s+1\s*$",
                         platform_header, flags=re.MULTILINE):
                macho_x86_status_test = output / "macho_x86_error_status"
                macho_x86_status_map = output / "macho_x86_error_status.map"
                macho_x86_status_trace = run(
                    cc
                    + [
                        str(ROOT / "liblink/tests/unit/macho_x86_error_status.c"),
                        f"-L{archive.parent}",
                        "-llink",
                        *core_link_flags(host),
                        "-Wl,-t",
                        linker_map_flag(macho_x86_status_map),
                        "-o",
                        str(macho_x86_status_test),
                    ],
                    label="compile and link macho_x86_error_status",
                )
                verify_macho_link_map(
                    macho_x86_status_map,
                    archive,
                    macho_x86_status_trace,
                    included_source="MachO.c",
                    forbidden_members=("MachO",),
                    required_members=("MachOVerify",),
                )
                run([str(macho_x86_status_test)], label="run macho_x86_error_status")
            else:
                print("[macho_x86_error_status] SKIP: generated target is not x86_64")

            run_macho_load_errors(cc, archive, output, host)

            platform_header = (config_include / "ll_platform.h").read_text()
            if re.search(
                r"^\s*#\s*define\s+aarch64_HOST_ARCH\b",
                platform_header,
                flags=re.MULTILINE,
            ):
                macho_stub_test = output / "macho_stub_errors"
                macho_stub_map = output / "macho_stub_errors.map"
                macho_stub_trace = run(
                    cc
                    + [
                        str(ROOT / "liblink/tests/unit/macho_stub_errors.c"),
                        f"-L{archive.parent}",
                        "-llink",
                        *core_link_flags(host),
                        "-Wl,-t",
                        linker_map_flag(macho_stub_map),
                        "-o",
                        str(macho_stub_test),
                    ],
                    label="compile and link macho_stub_errors",
                )
                verify_macho_link_map(
                    macho_stub_map,
                    archive,
                    macho_stub_trace,
                    included_source="MachO.c, macho/plt.c, and macho/plt_aarch64.c",
                    forbidden_members=("MachO", "plt", "plt_aarch64"),
                )
                run([str(macho_stub_test)], label="run macho_stub_errors")

    print("liblink native unit tests: PASS")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError) as error:
        print(f"liblink native unit tests: ERROR: {error}", file=sys.stderr)
        raise SystemExit(1) from error
