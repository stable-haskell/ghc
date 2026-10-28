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
TIMEOUT_SECONDS = 180
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
        "--all-backends",
        action="store_true",
        help="check a library built with Cabal +all-backends",
    )
    parser.add_argument(
        "--initializer-errors-only",
        action="store_true",
        help="run common initializer status, cleanup, and terminal-error policy units",
    )
    parser.add_argument(
        "--format-dispatch-only",
        action="store_true",
        help="run runtime format selection and per-object lifecycle dispatch controls",
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
        "--pe-resolution-only",
        action="store_true",
        help="run native Windows x86_64 PE resolution and COMMON reuse/conflict cases",
    )
    parser.add_argument(
        "--pe-load-resources-only",
        action="store_true",
        help="run only native Windows x86_64 PE load-resource failure cases",
    )
    parser.add_argument(
        "--macho-table-alignment-only",
        action="store_true",
        help="run only packed Mach-O symbol/relocation table alignment cases",
    )
    parser.add_argument(
        "--macho-x86-operands-only",
        action="store_true",
        help="run only native x86_64 Darwin operand-width and compiler-fixture units",
    )
    parser.add_argument(
        "--elf-resources-only",
        action="store_true",
        help="run only native Linux x86_64 COMMON/GOT resource units",
    )
    parser.add_argument(
        "--mmap-address-bounds-only",
        action="store_true",
        help="run only the POSIX linker mmap address-bound unit",
    )
    parser.add_argument(
        "--path-ownership-only",
        action="store_true",
        help="run only the private pathdup owner-allocation unit",
    )
    parser.add_argument(
        "--native-load-errors-only",
        action="store_true",
        help="run only POSIX native-library cleanup, ownership, and retry controls",
    )
    return parser.parse_args()


def native_host() -> str:
    system = platform.system()
    return (
        "Windows"
        if os.name == "nt"
        or sys.platform in ("win32", "msys")
        or system.startswith(("MSYS_NT", "MINGW"))
        else system
    )


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
    if not archives:
        archives = [
            path
            for path in store_root.rglob("libHSliblink*.a")
            if "liblink-0.1.0.0" in path.parts
        ]
    if not archives:
        archives = [
            path
            for path in build_root.rglob("libHSliblink*.a")
            if "liblink-0.1.0.0" in path.parts
            and "objs-" not in path.parts
        ]
    if not archives:
        archives = [
            path
            for path in list(build_root.rglob("liblink.a")) + list(store_root.rglob("liblink.a"))
            if "liblink-0.1.0.0" in path.parts
        ]
    archive = unique_path(archives, "stage2 native liblink archive")

    if archive.name != "liblink.a":
        liblink_alias = archive.parent / "liblink.a"
        if not liblink_alias.exists():
            try:
                liblink_alias.symlink_to(archive.name)
            except OSError:
                import shutil
                shutil.copy2(archive, liblink_alias)
        archive = liblink_alias

    candidates = [
        archive.parent / "include",
        archive.parent.parent / "include",
        ROOT / "liblink/include",
    ]
    config_include = None
    for candidate in candidates:
        if (candidate / "ll_autoconf.h").is_file() and (candidate / "ll_platform.h").is_file():
            config_include = candidate
            break
    if config_include is None:
        for h in list(store_root.rglob("ll_platform.h")) + list(build_root.rglob("ll_platform.h")):
            if (h.parent / "ll_autoconf.h").is_file():
                config_include = h.parent
                break
    if config_include is None:
        raise RuntimeError(
            f"missing generated liblink headers (ll_autoconf.h, ll_platform.h) under {archive.parent}"
        )

    fs_headers = [
        path
        for path in store_root.rglob("fs.h")
        if "rts-fs-1.0.0.0" in path.parts and path.parent.name == "include"
    ]
    fs_header = unique_path(fs_headers, "installed rts-fs fs.h")
    return archive, config_include, fs_header.parent


def locate_rts_fs_archive(
    fs_include: Path, host: str, *, required: bool = True
) -> Path | None:
    if host == "Darwin" or not required:
        return None
    if host not in ("Linux", "Windows"):
        raise RuntimeError(f"unsupported host for rts-fs test linking: {host}")
    archive = fs_include.parent / "libHSrts-fs-1.0.0.0.a"
    if not archive.is_file():
        raise RuntimeError(
            "native C units require the installed rts-fs static archive on "
            f"{host}: {archive}"
        )
    return archive


def command_text(argv: list[str]) -> str:
    return shlex.join(str(arg) for arg in argv)


def run(argv: list[str], *, label: str,
        env: dict[str, str] | None = None) -> str:
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
            env=env,
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


def native_link_flags(
    host: str,
    rts_fs_archive: Path | None,
    trailing_libraries: list[str],
    *,
    require_rts_fs: bool = True,
) -> list[str]:
    if host == "Darwin" or not require_rts_fs:
        return list(trailing_libraries)
    if host not in ("Linux", "Windows"):
        raise RuntimeError(f"unsupported host for native unit links: {host}")
    if rts_fs_archive is None or not rts_fs_archive.is_file():
        raise RuntimeError(
            f"native C unit links on {host} require an installed rts-fs archive"
        )
    return [str(rts_fs_archive), *trailing_libraries]


def native_unit_link_arguments(
    source: Path,
    archive: Path,
    output: Path,
    link_flags: list[str],
    linker_flags: list[str],
) -> list[str]:
    return [
        str(source),
        f"-L{archive.parent}",
        "-llink",
        *link_flags,
        *linker_flags,
        "-o",
        str(output),
    ]


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


def configured_posix_mmap_backend(config_include: Path) -> bool:
    autoconf_header = config_include / "ll_autoconf.h"
    text = autoconf_header.read_text(errors="replace")
    return re.search(
        r"^\s*#\s*define\s+RTS_LINKER_USE_MMAP\s+1\s*$",
        text,
        re.MULTILINE,
    ) is not None


def run_elf_x86_64_sites(cc: list[str], archive: Path, output: Path,
                        link_flags: list[str], config_include: Path,
                        host: str, all_backends: bool) -> None:
    platform_header = (config_include / "ll_platform.h").read_text()
    x64_host = re.search(r"^\s*#\s*define\s+x86_64_HOST_ARCH(?:\s+1)?\s*$",
                         platform_header, re.MULTILINE) is not None
    arm64_host = re.search(r"^\s*#\s*define\s+(?:aarch64|arm64)_HOST_ARCH(?:\s+1)?\s*$",
                           platform_header, re.MULTILINE) is not None
    if not (x64_host or arm64_host) or host not in ("Linux", "Darwin"):
        print("[elf_x86_64_sites] data-only unit currently requires a POSIX x64/ARM64 host")
        return
    binary = output / "elf_x86_64_sites"
    native_elf_x64 = x64_host and (host == "Linux" or all_backends)
    # An ARM64 or native-only Darwin library has no x64 ELF apply member.
    # Compile that one actual TU for its data-only tests; shared storage and
    # host services still come from the real native archive.
    target_flags = [] if native_elf_x64 else [
        "-DLL_ALL_BACKENDS", "-DLL_TARGET_ARCH=LL_ARCH_X86_64",
        str(ROOT / "liblink/src/linker/elf_reloc_x86_64.c")]
    run(cc + target_flags + native_unit_link_arguments(
        ROOT / "liblink/tests/unit/elf_x86_64_sites.c", archive, binary,
        link_flags, []), label="compile and link elf_x86_64_sites")
    run([str(binary)], label="run elf_x86_64_sites")
    stubs = output / "elf_aarch64_stub_sites"
    run(cc + ["-DLL_ALL_BACKENDS", "-ULL_TARGET_ARCH",
              "-DLL_TARGET_ARCH=LL_ARCH_AARCH64",
              str(ROOT / "liblink/tests/unit/elf_aarch64_stub_sites.c"),
              "-o", str(stubs)], label="compile paired ELF AArch64 stub controls")
    run([str(stubs)], label="run paired ELF AArch64 stub controls")
    arm_veneer = output / "elf_arm_veneer"
    run(cc + ["-DLL_ALL_BACKENDS", "-ULL_TARGET_ARCH",
              "-DLL_TARGET_ARCH=LL_ARCH_ARM",
              str(ROOT / "liblink/tests/unit/elf_arm_veneer.c"),
              "-o", str(arm_veneer)], label="compile ARM veneer pointer-width control")
    run([str(arm_veneer)], label="run ARM veneer pointer-width control")
    arm_stub = output / "elf_arm_thumb_stub"
    run(cc + ["-DLL_ALL_BACKENDS", "-ULL_TARGET_ARCH",
              "-DLL_TARGET_ARCH=LL_ARCH_ARM",
              str(ROOT / "liblink/tests/unit/elf_arm_thumb_stub.c"),
              "-o", str(arm_stub)], label="compile ARM Thumb stub control")
    run([str(arm_stub)], label="run ARM Thumb stub control")
    i386_sites = output / "elf_i386_sites"
    run(cc + ["-DLL_ALL_BACKENDS", "-ULL_TARGET_ARCH",
              "-DLL_TARGET_ARCH=LL_ARCH_I386",
              str(ROOT / "liblink/tests/unit/elf_i386_sites.c"),
              str(ROOT / "liblink/src/ll_error.c"), "-o", str(i386_sites)],
        label="compile ELF i386 site controls")
    run([str(i386_sites)], label="run ELF i386 site controls")
    encoder = output / "elf_aarch64_encoder_sites"
    run(cc + ["-DLL_ALL_BACKENDS", "-ULL_TARGET_ARCH",
              "-DLL_TARGET_ARCH=LL_ARCH_AARCH64",
              str(ROOT / "liblink/tests/unit/elf_aarch64_encoder_sites.c"),
              str(ROOT / "liblink/src/ll_error.c"), "-o", str(encoder)],
        label="compile ELF AArch64 site encoder controls")
    run([str(encoder)], label="run ELF AArch64 site encoder controls")
    classifier = output / "elf_aarch64_classifier"
    run(cc + ["-DLL_ALL_BACKENDS", "-ULL_TARGET_ARCH",
              "-DLL_TARGET_ARCH=LL_ARCH_AARCH64",
              str(ROOT / "liblink/tests/unit/elf_aarch64_classifier.c"),
              str(ROOT / "liblink/src/ll_error.c"), "-o", str(classifier)],
        label="compile ELF AArch64 instruction classifier controls")
    run([str(classifier)], label="run ELF AArch64 instruction classifier controls")
    aarch64_sites = output / "elf_aarch64_sites"
    run(cc + ["-DLL_ALL_BACKENDS", "-ULL_TARGET_ARCH",
              "-DLL_TARGET_ARCH=LL_ARCH_AARCH64",
              str(ROOT / "liblink/tests/unit/elf_aarch64_sites.c"),
              str(ROOT / "liblink/src/ll_error.c"), "-o", str(aarch64_sites)],
        label="compile ELF AArch64 apply table controls")
    run([str(aarch64_sites)], label="run ELF AArch64 apply table controls")
    storage = output / "noexec_storage"
    run(cc + [str(ROOT / "liblink/tests/unit/noexec_storage.c"),
              str(ROOT / "liblink/src/ll_error.c"), "-o", str(storage)],
        label="compile NO_EXEC storage controls")
    run([str(storage)], label="run NO_EXEC storage controls")
    for target in ("I386", "ARM"):
        storage32 = output / ("noexec_storage_" + target.lower())
        run(cc + ["-ULL_TARGET_ARCH", "-DLL_TARGET_ARCH=LL_ARCH_" + target,
                  str(ROOT / "liblink/tests/unit/noexec_storage.c"),
                  str(ROOT / "liblink/src/ll_error.c"), "-o", str(storage32)],
            label="compile ELF32 NO_EXEC range controls")
        run([str(storage32)], label="run ELF32 NO_EXEC range controls")
    tail = output / "noexec_tail"
    run(cc + [str(ROOT / "liblink/tests/unit/noexec_tail.c"),
              str(ROOT / "liblink/src/ll_error.c"), "-o", str(tail)],
        label="compile NO_EXEC tail reservation controls")
    run([str(tail)], label="run NO_EXEC tail reservation controls")
    command = [sys.executable, "-m", "unittest", "discover", "-s",
               "liblink/tests/oracle", "-p", "test_elf_x86_64.py", "-v"]
    run([sys.executable, "-m", "unittest", "discover", "-s", "liblink/tests/oracle",
         "-p", "test_elf_aarch64.py", "-v"],
        label="replay ELF AArch64 reference and corruption controls")
    run([sys.executable, "-m", "unittest", "discover", "-s", "liblink/tests/oracle",
         "-p", "test_elf32*.py", "-v"],
        label="replay ELF32 references and corruption controls")
    run([sys.executable, "-m", "unittest", "discover", "-s", "liblink/tests/oracle",
         "-p", "test_elf_riscv64.py", "-v"],
        label="replay ELF RISC-V baseline and corruption controls")
    if not native_elf_x64:
        run(command, label="replay ELF x64 reference and corruption controls")
        return
    # Scalar bounds use the target payload width, including no payload.
    for target, width in (("X86_64", 16), ("AARCH64", 0), ("RISCV64", 8)):
        bounds = output / ("elf_extras_bounds_" + target.lower())
        map_file = output / (bounds.name + ".map")
        trace = run(cc + native_unit_link_arguments(
            ROOT / "liblink/tests/unit/elf_extras_bounds.c", archive, bounds,
            link_flags, ["-DLL_TARGET_ARCH=LL_ARCH_" + target,
                         "-DEXPECTED_EXTRA_STRIDE=" + str(width),
                         "-Wl,-t", linker_map_flag(map_file)]),
            label="compile ELF extras bounds for " + target)
        verify_link_map(map_file, archive, trace,
                        included_source="elf_extras_bounds.c", forbidden_member="ElfVerify")
        run([str(bounds)], label="run ELF extras bounds for " + target)
    capture = output / "elf_capture"
    run(cc + native_unit_link_arguments(
        ROOT / "liblink/tests/runner/elf_capture.c", archive, capture,
        link_flags, []), label="compile and link ELF native capture")
    # The test's optional live capture mode is selected through its documented
    # environment variable without mutating the driver's process environment.
    run(command, env=dict(os.environ, LIBLINK_ELF_CAPTURE=str(capture)),
        label="check live ELF x64 fields and unchanged native goldens")
    core = output / "noexec_core"
    map_file = output / "noexec_core.map"
    trace = run(cc + native_unit_link_arguments(
        ROOT / "liblink/tests/unit/noexec_core.c", archive, core, link_flags,
        ["-Wl,-t", linker_map_flag(map_file)]), label="compile NO_EXEC core controls")
    for member in ("Linker", "Elf", "elf_got", "LoadArchive", "LoadNativeObjPosix", "ll_noexec"):
        verify_link_map(map_file, archive, trace, included_source="noexec_core.c",
                        forbidden_member=member)
    fixture = ROOT / "liblink/tests/fixtures/elf-noexec/symbols.o"
    fixture_archive = output / "noexec_symbols.a"
    run(shlex.split(os.environ.get("AR", "ar")) +
        ["rcs", str(fixture_archive), str(fixture)], label="archive NO_EXEC symbols fixture")
    generated = ROOT / "liblink/tests/fixtures/generated/elf-x86_64/x86_64-linux-gnu"
    run([str(core), str(fixture), str(generated / "init_fini.o"), str(fixture_archive),
         str(generated / "libelf-longname-gnu.a"), str(generated / "R_X86_64_TLSGD.o"),
         str(generated / "R_X86_64_64.o"),
         str(ROOT / "liblink/tests/fixtures/elf-noexec/gottpoff.o")],
        label="run NO_EXEC core controls")


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
                                output: Path,
                                link_flags: list[str]) -> None:
    verify_overlay_include_argv()
    test_source = ROOT / "liblink/tests/unit/pei386_common_ownership.c"
    test_binary = output / "pei386_common_ownership.exe"
    test_map = output / "pei386_common_ownership.map"
    trace = run(
        cc
        + native_unit_link_arguments(
            test_source,
            archive,
            test_binary,
            link_flags,
            # MinGW LLD rejects GNU ld's -t; --verbose prints resolved inputs.
            ["-Wl,--verbose", linker_map_flag(test_map)],
        ),
        label="compile and link pei386_common_ownership",
    )
    included = (
        "PEi386.c, PEi386Host.c, Linker.c, M32Alloc.c, M32Reservations.c, and MMap.c"
    )
    for member in ("PEi386", "PEi386Host", "Linker", "M32Alloc", "M32Reservations", "MMap"):
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
        "SECTIONKIND_RWDATA, ll_noexec_enabled(oc->L) ? SECTION_NOEXEC : SECTION_M32,\n"
        "                  bss, globalBssSize, 0, block.allocation, 0, sizeof(struct ll_coff_section_info));"
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
        + native_unit_link_arguments(
            test_source,
            archive,
            mutant_binary,
            link_flags,
            ["-Wl,--verbose", linker_map_flag(mutant_map)],
        ),
        label="compile old-tag ownership regression mutant",
    )
    for member in ("PEi386", "PEi386Host", "Linker", "M32Alloc", "M32Reservations", "MMap"):
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


def run_pei386_resolution_status(cc: list[str], archive: Path,
                                 output: Path,
                                 link_flags: list[str]) -> None:
    included = "PEi386.c, PEi386Host.c, Linker.c, M32Alloc.c, M32Reservations.c, and MMap.c"
    for unit in ("pei386_resolution_status", "pei386_unwind_retention",
                 "pei386_resolver_classification", "pei386_resolver_quarantine",
                 "pei386_common_conflict"):
        binary = output / f"{unit}.exe"
        map_file = output / f"{unit}.map"
        trace = run(
            cc + native_unit_link_arguments(
                ROOT / f"liblink/tests/unit/{unit}.c",
                archive,
                binary,
                link_flags,
                ["-Wl,--verbose", linker_map_flag(map_file)],
            ),
            label=f"compile and link {unit}",
        )
        for member in ("PEi386", "PEi386Host", "Linker", "M32Alloc", "M32Reservations", "MMap"):
            verify_link_map(
                map_file,
                archive,
                trace,
                included_source=included,
                forbidden_member=member,
                windows_paths=True,
            )
        run([str(binary)], label=f"run {unit}")


def run_pei386_load_resources(cc: list[str], archive: Path,
                              output: Path,
                              link_flags: list[str]) -> None:
    unit = "pei386_load_resources"
    binary = output / f"{unit}.exe"
    map_file = output / f"{unit}.map"
    trace = run(
        cc + native_unit_link_arguments(
            ROOT / f"liblink/tests/unit/{unit}.c",
            archive,
            binary,
            link_flags,
            ["-Wl,--verbose", linker_map_flag(map_file)],
        ),
        label=f"compile and link {unit}",
    )
    included = "PEi386.c, PEi386Host.c, Linker.c, M32Alloc.c, M32Reservations.c, and MMap.c"
    for member in ("PEi386", "PEi386Host", "Linker", "M32Alloc", "M32Reservations", "MMap"):
        verify_link_map(
            map_file,
            archive,
            trace,
            included_source=included,
            forbidden_member=member,
            windows_paths=True,
        )
    run([str(binary)], label=f"run {unit}")


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
                          link_flags: list[str], *, mode: str = "") -> None:
    macho_test = output / "macho_load_errors"
    macho_map = output / "macho_load_errors.map"
    macho_trace = run(
        cc
        + [
            str(ROOT / "liblink/tests/unit/macho_load_errors.c"),
            f"-L{archive.parent}",
            "-llink",
            *link_flags,
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
                              host: str,
                              link_flags: list[str]) -> None:
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
              f"-L{archive.parent}", "-llink", *link_flags, "-Wl,-t",
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


def run_macho_x86_operands(cc: list[str], archive: Path, output: Path,
                           link_flags: list[str]) -> None:
    fixture = output / "macho_x86_operand_ordinary.o"
    run(cc + ["-O1", "-fno-stack-protector", "-fno-asynchronous-unwind-tables",
              "-c", str(ROOT / "liblink/tests/fixtures/macho-input/ordinary.c"),
              "-o", str(fixture)], label="compile x86 operand compiler fixture")
    for name, forbidden, required in (
        ("macho_x86_operand_widths", ("MachO",), ("ll_host",)),
        ("macho_x86_operand_fixture", (), ("MachO",)),
    ):
        binary = output / name
        map_file = output / (name + ".map")
        trace = run(
            cc + [str(ROOT / "liblink/tests/unit" / (name + ".c")),
                  f"-L{archive.parent}", "-llink", *link_flags,
                  "-Wl,-t", "-Wl,-why_load", linker_map_flag(map_file),
                  "-o", str(binary)], label="compile and link " + name)
        verify_macho_link_map(map_file, archive, trace, included_source=name,
                              forbidden_members=forbidden, required_members=required)
        link_trace_checks.verify_macho_archive_selection(
            archive, trace, included_source=name, forbidden_members=forbidden)
        command = [str(binary)]
        if name == "macho_x86_operand_fixture":
            command.append(str(fixture))
        run(command, label="run " + name)


def run_elf_resources(cc: list[str], archive: Path, output: Path,
                      link_flags: list[str]) -> None:
    fixtures = output / "elf-resource-fixtures"
    run([sys.executable, str(ROOT / "liblink/tests/unit/elf_resource_fixtures.py"),
         str(fixtures)], label="generate ELF resource fixtures")
    binary = output / "elf_resource_errors"
    map_file = output / "elf_resource_errors.map"
    trace = run(
        cc + [str(ROOT / "liblink/tests/unit/elf_resource_errors.c"),
              f"-L{archive.parent}", "-llink", *link_flags,
              "-Wl,--wrap=ll_mmap_anon_for_linker",
              "-Wl,--wrap=ll_mmap_for_linker",
              "-Wl,--wrap=ll_munmap_for_linker",
              "-Wl,--wrap=m32_alloc_for", "-Wl,--wrap=open", "-Wl,--wrap=close",
              "-Wl,-t", linker_map_flag(map_file), "-o", str(binary)],
        label="compile and link elf_resource_errors")
    verify_link_map(map_file, archive, trace, included_source="Linker.c",
                    forbidden_member="Linker")
    map_text = map_file.read_text(errors="replace")
    for member in ("Elf", "MMap", "M32Alloc"):
        if not archive_member_pattern(str(archive), member).search(map_text):
            raise RuntimeError(f"resource map omitted {archive}({member}.o)")
    run([str(binary), str(fixtures)], label="run elf_resource_errors")


def run_mmap_address_bounds(cc: list[str], archive: Path, output: Path,
                            link_flags: list[str]) -> None:
    binary = output / "mmap_address_bounds"
    map_file = output / "mmap_address_bounds.map"
    trace = run(
        cc + [str(ROOT / "liblink/tests/unit/mmap_address_bounds.c"),
              f"-L{archive.parent}", "-llink", *link_flags,
              "-Wl,-t", linker_map_flag(map_file), "-o", str(binary)],
        label="compile and link mmap_address_bounds",
    )
    verify_link_map(map_file, archive, trace, included_source="MMap.c",
                    forbidden_member="MMap")
    map_text = map_file.read_text(errors="replace")
    if not archive_member_pattern(str(archive), "ll_host").search(map_text):
        raise RuntimeError(f"address map omitted {archive}(ll_host.o)")
    run([str(binary)], label="run mmap_address_bounds")


def run_pathdup_ownership(cc: list[str], archive: Path, output: Path,
                          link_flags: list[str], host: str) -> None:
    binary = output / "pathdup_ownership"
    map_file = output / "pathdup_ownership.map"
    trace_flags = ["-Wl,--verbose"] if host == "Windows" else ["-Wl,-t"]
    trace = run(
        cc + native_unit_link_arguments(
            ROOT / "liblink/tests/unit/pathdup_ownership.c",
            archive,
            binary,
            link_flags,
            [*trace_flags, linker_map_flag(map_file)],
        ),
        label="compile and link pathdup_ownership",
    )
    verify_link_map(
        map_file,
        archive,
        trace,
        included_source="pathdup_ownership.c",
        forbidden_member="pathdup_ownership",
        windows_paths=host == "Windows",
    )
    if host == "Windows" and link_trace_checks.is_lld_mingw_verbose_trace(trace):
        link_trace_checks.verify_lld_mingw_archive_map(
            map_file.read_text(errors="replace"),
            archive,
            trace,
            included_source="pathdup_ownership.c",
            forbidden_members=("pathdup_ownership",),
            required_members=("ll_path",),
        )
        run([str(binary)], label="run pathdup_ownership")
        return
    map_text = map_file.read_text(errors="replace")
    if host == "Windows":
        map_text = normalize_windows_path(map_text)
        archive_forms = [normalize_windows_path(form) for form in windows_path_forms(archive)]
    else:
        archive_forms = [str(archive)]
    if not any(archive_member_pattern(form, "ll_path").search(map_text)
               for form in archive_forms):
        raise RuntimeError(f"link map did not select ll_path from {archive}")
    run([str(binary)], label="run pathdup_ownership")

def run_initializer_binary(binary: Path, host: str, *, label: str) -> None:
    try:
        run([str(binary)], label=label)
    except RuntimeError:
        debugger = shutil.which("gdb") if host == "Windows" else None
        if debugger is not None:
            # Diagnose a fresh process while its executable still exists.
            # The first failure remains authoritative, even if this run passes.
            try:
                run(
                    [debugger, "-nx", "-q", "-batch", "-return-child-result",
                     "-ex", "set pagination off", "-ex", "run",
                     "-ex", "thread apply all bt full", "--args", str(binary)],
                    label=f"diagnose {label}",
                )
            except (RuntimeError, OSError, UnicodeError) as error:
                print(f"[{label}] debugger result: {error}", file=sys.stderr,
                      flush=True)
        raise


def run_initializer_errors(cc: list[str], archive: Path, output: Path,
                           link_flags: list[str], host: str) -> None:
    units = (
        ("init_array_status", ("InitFini",)),
        ("init_array_policy", ("Linker", "InitFini", "ll_failure")),
    )
    for unit, forbidden in units:
        binary = output / (unit + (".exe" if host == "Windows" else ""))
        map_file = output / f"{unit}.map"
        trace_flags = ["-Wl,--verbose"] if host == "Windows" else ["-Wl,-t"]
        trace = run(
            cc + native_unit_link_arguments(
                ROOT / f"liblink/tests/unit/{unit}.c", archive, binary,
                link_flags, [*trace_flags, linker_map_flag(map_file)],
            ),
            label=f"compile and link {unit}",
        )
        for member in forbidden:
            verify_link_map(
                map_file, archive, trace,
                included_source=f"{unit}.c included implementations",
                forbidden_member=member, windows_paths=host == "Windows",
            )
        run_initializer_binary(binary, host, label=f"run {unit}")


def run_native_load_errors(cc: list[str], archive: Path, output: Path,
                           link_flags: list[str], host: str,
                           config_include: Path) -> None:
    autoconf = (config_include / "ll_autoconf.h").read_text(errors="replace")
    mapping_controls = host == "Linux" and re.search(
        r"^\s*#\s*define\s+HAVE_DLINFO\s+1\s*$", autoconf, re.MULTILINE
    ) is not None
    wrap_flags = ([f"-Wl,--wrap={name}" for name in
                   ("dlinfo", "dl_iterate_phdr", "dlclose")]
                  if mapping_controls else [])
    source = ROOT / "liblink/tests/unit/native_load_fixture.c"
    suffix = ".dylib" if host == "Darwin" else ".so"
    shared = ["-dynamiclib"] if host == "Darwin" else ["-shared", "-fPIC"]
    lazy_flags = (["-Wl,-undefined,dynamic_lookup"] if host == "Darwin" else
                  ["-fplt", "-Wl,-z,lazy", "-Wl,--allow-shlib-undefined"])
    libraries = {}
    kinds = ("ordinary", "lazy", "lazy-defaults") if host == "Linux" else (
        "ordinary", "lazy"
    )
    for kind in kinds:
        library = output / ("libnative-load-" + kind + suffix)
        flags = (["-DNATIVE_LOAD_LAZY_FIXTURE", *lazy_flags]
                 if kind == "lazy" else [])
        if kind == "lazy-defaults":
            # Preserve the original fixture command to measure ambient binding.
            flags = ["-DNATIVE_LOAD_LAZY_FIXTURE", "-Wl,--allow-shlib-undefined"]
        run(cc + [*shared, *flags, str(source), "-o", str(library)],
            label="compile native-load " + kind + " fixture")
        libraries[kind] = library
    if host == "Linux":
        print(f"[native-load binding] LD_BIND_NOW={os.environ.get('LD_BIND_NOW')!r}",
              flush=True)
        reader = shutil.which("readelf")
        if reader is not None:
            for kind in ("lazy-defaults", "lazy"):
                run([reader, "-d", "-r", str(libraries[kind])],
                    label="inspect native-load " + kind + " binding")
        else:
            print("[native-load binding] readelf unavailable; no ELF tag evidence",
                  flush=True)
    binary = output / "native_load_errors"
    map_file = output / "native_load_errors.map"
    trace = run(
        cc + native_unit_link_arguments(
            ROOT / "liblink/tests/unit/native_load_errors.c", archive,
            binary, [*wrap_flags, *link_flags], ["-Wl,-t", linker_map_flag(map_file)]),
        label="compile and link native_load_errors")
    verify_link_map(map_file, archive, trace,
                    included_source="native_load_errors.c",
                    forbidden_member="native_load_errors")
    evidence = map_file.read_text(errors="replace") + trace
    for member in ("Linker", "LoadNativeObjPosix"):
        if not archive_member_pattern(str(archive), member).search(evidence):
            raise RuntimeError(f"native-load test omitted {member} from {archive}")
    if host == "Linux":
        for case, kind in (("loader-defaults", "lazy-defaults"),
                           ("loader-controlled", "lazy")):
            run([str(binary), case, str(libraries[kind])],
                label="native-load " + case)
    missing = output / ("does-not-exist" + suffix)
    for case in ("positive", "collision", "missing", "missing-host-state",
                 "lazy", "missing-no-unload-hook"):
        path = (libraries["ordinary"] if case in ("positive", "collision") else
                libraries["lazy"] if case == "lazy" else missing)
        run([str(binary), case, str(path)], label="native-load " + case)
    if mapping_controls:
        for case in ("dlinfo", "dlinfo-quiet", "ranges", "ranges-quiet"):
            run([str(binary), case, str(libraries["ordinary"])],
                label="native-load mapping fallback " + case)


def main() -> int:
    args = parse_args()
    selected_modes = sum((args.initializer_errors_only, args.foundation_only, args.protection_only,
                          args.protection_gap_probes,
                          args.resolution_failure_only, args.windows_m32_only,
                          args.pe_resolution_only, args.pe_load_resources_only,
                          args.macho_table_alignment_only,
                          args.macho_x86_operands_only,
                          args.elf_resources_only,
                          args.mmap_address_bounds_only,
                          args.path_ownership_only, args.native_load_errors_only,
                          args.format_dispatch_only))
    if selected_modes > 1:
        raise RuntimeError("native unit modes are mutually exclusive")
    host = native_host()
    if args.windows_m32_only and host != "Windows":
        raise RuntimeError("--windows-m32-only requires a native Windows host")
    if args.pe_resolution_only and host != "Windows":
        raise RuntimeError("--pe-resolution-only requires a native Windows host")
    if args.pe_load_resources_only and host != "Windows":
        raise RuntimeError("--pe-load-resources-only requires a native Windows host")
    if args.macho_table_alignment_only and host != "Darwin":
        raise RuntimeError("--macho-table-alignment-only requires a native Darwin Mach-O build")
    if args.macho_x86_operands_only and host != "Darwin":
        raise RuntimeError("--macho-x86-operands-only requires native x86_64 Darwin")
    if args.elf_resources_only and host != "Linux":
        raise RuntimeError("--elf-resources-only requires native Linux x86_64 ELF")
    if args.mmap_address_bounds_only and host not in ("Linux", "Darwin"):
        raise RuntimeError("--mmap-address-bounds-only requires a POSIX host")
    if args.native_load_errors_only and host not in ("Linux", "Darwin"):
        raise RuntimeError("--native-load-errors-only requires a POSIX native host")
    if host == "Windows" and not (args.initializer_errors_only or
                                   args.foundation_only or args.windows_m32_only
                                   or args.pe_resolution_only
                                   or args.pe_load_resources_only
                                   or args.path_ownership_only or args.format_dispatch_only):
        raise RuntimeError(
            "core_error_status.c is POSIX-only: the Windows linker path uses "
            "stdio rather than mmap; use a focused Windows unit mode"
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
    if args.all_backends:
        compiler.append("-DLL_ALL_BACKENDS")
    archive, config_include, fs_include = locate_artifacts(
        args.build_root.resolve(), args.store_root.resolve()
    )
    rts_fs_archive = locate_rts_fs_archive(
        fs_include, host, required=not args.foundation_only
    )
    mmap_backend = configured_posix_mmap_backend(config_include)
    if args.mmap_address_bounds_only and not mmap_backend:
        raise RuntimeError(
            "--mmap-address-bounds-only requires RTS_LINKER_USE_MMAP=1"
        )
    if args.native_archive is not None:
        archive = args.native_archive.resolve()
        if not archive.is_file():
            raise RuntimeError(f"native liblink archive does not exist: {archive}")
        if archive.name != "liblink.a":
            liblink_alias = archive.parent / "liblink.a"
            if not liblink_alias.exists():
                try:
                    liblink_alias.symlink_to(archive.name)
                except OSError:
                    import shutil
                    shutil.copy2(archive, liblink_alias)
            archive = liblink_alias
    link_flags = native_link_flags(
        host,
        rts_fs_archive,
        core_link_flags(host),
        require_rts_fs=not args.foundation_only,
    )
    rts_fs_link_flags = native_link_flags(
        host, rts_fs_archive, [], require_rts_fs=not args.foundation_only
    )
    pe_link_flags = (
        native_link_flags(
            host,
            rts_fs_archive,
            PE_SUPPORT_LIBS,
            require_rts_fs=not args.foundation_only,
        )
        if host == "Windows" else []
    )
    print(f"[inputs] compiler={command_text(compiler)}")
    print(f"[inputs] generated_config={config_include}")
    print(f"[inputs] rts_fs_include={fs_include}")
    if rts_fs_archive is not None:
        print(f"[inputs] rts_fs_archive={rts_fs_archive}")
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

    if selected_modes == 0 or args.pe_resolution_only:
        with tempfile.TemporaryDirectory(prefix="liblink-coff-layout-") as temporary:
            binary = Path(temporary) / "coff_layout"
            source = ROOT / "liblink/tests/unit/coff_layout.c"
            run(cc + [str(source), "-o", str(binary)],
                label="compile SDK-independent COFF layout unit")
            if sys.byteorder == "little":
                run([str(binary)], label="check COFF packed arrays and wire layout")
            else:
                print("[coff_layout] wire-byte runtime check requires a little-endian host; layout compiled")
            if host == "Windows":
                run(cc + ["-DLL_COFF_COMPARE_SDK", str(source), "-o", str(binary)],
                    label="compile real Windows SDK layout comparison")
                run([str(binary)], label="check Windows SDK COFF layout equivalence")

    if selected_modes == 0:
        with tempfile.TemporaryDirectory(prefix="liblink-elf-layout-") as temporary:
            binary = Path(temporary) / "elf_layout"
            source = ROOT / "liblink/tests/unit/elf_layout.c"
            run(cc + [str(source), "-o", str(binary)],
                label="compile SDK-independent ELF layout unit")
            if sys.byteorder == "little":
                run([str(binary)], label="check ELF32/ELF64 wire layout")
            else:
                print("[elf_layout] wire-byte runtime check requires a little-endian host; layout compiled")
            if host in ("Linux", "FreeBSD", "OpenBSD", "NetBSD", "DragonFly", "SunOS"):
                run(cc + ["-DLL_ELF_COMPARE_SDK", str(source), "-o", str(binary)],
                    label="compile real ELF SDK layout comparison")
                if sys.byteorder == "little":
                    run([str(binary)], label="check native SDK ELF layout equivalence")

    if selected_modes == 0:
        with tempfile.TemporaryDirectory(prefix="liblink-macho-layout-") as temporary:
            binary = Path(temporary) / "macho_layout"
            source = ROOT / "liblink/tests/unit/macho_layout.c"
            run(cc + [str(source), "-o", str(binary)],
                label="compile SDK-independent Mach-O layout unit")
            if sys.byteorder == "little":
                run([str(binary)], label="check Mach-O wire layout")
            else:
                print("[macho_layout] wire-byte runtime check requires a little-endian host; layout compiled")
            if host == "Darwin":
                run(cc + ["-DLL_MACHO_COMPARE_SDK", str(source), "-o", str(binary)],
                    label="compile real Apple SDK layout comparison")
                run([str(binary)], label="check Apple SDK Mach-O layout equivalence")
        run([sys.executable, str(ROOT / "liblink/tests/unit/test_target_config.py"),
             "--cc", command_text(compiler), "--config-dir", str(config_include)],
            label="check target selection and host definitions")
        run([sys.executable, str(ROOT / "liblink/tests/unit/test_shared_layout.py"),
             "--cc", command_text(compiler), "--config-dir", str(config_include)],
            label="check private producer and native client shared layouts")
        with tempfile.TemporaryDirectory(prefix="liblink-page21-encoding-") as temporary:
            directory = Path(temporary)
            header = directory / "macho_page21_generated.h"
            binary = directory / "macho_page21_encoding"
            run([sys.executable, str(ROOT / "liblink/tests/unit/macho_page21_source.py"),
                 "--output", str(header)], label="extract current Mach-O PAGE21 writer")
            run(cc + ["-I", str(directory),
                      str(ROOT / "liblink/tests/unit/macho_page21_encoding.c"),
                      "-o", str(binary)], label="compile portable PAGE21 encoding unit")
            run([str(binary)], label="run portable PAGE21 encoding unit")

    if args.format_dispatch_only or selected_modes == 0:
        with tempfile.TemporaryDirectory(prefix="liblink-format-dispatch-") as temporary:
            binary = Path(temporary) / ("format_dispatch.exe" if host == "Windows" else "format_dispatch")
            run(cc + native_unit_link_arguments(
                ROOT / "liblink/tests/unit/format_dispatch.c", archive, binary,
                pe_link_flags if host == "Windows" else link_flags, []),
                label="compile and link format_dispatch")
            run([str(binary)], label="run format_dispatch")
            memory_binary = Path(temporary) / (
                "backend_memory.exe" if host == "Windows" else "backend_memory")
            run(cc + native_unit_link_arguments(
                ROOT / "liblink/tests/unit/backend_memory.c", archive, memory_binary,
                pe_link_flags if host == "Windows" else link_flags, []),
                label="compile and link backend_memory")
            run([str(memory_binary)], label="run backend_memory")
        if args.format_dispatch_only:
            return 0

    if selected_modes == 0:
        with tempfile.TemporaryDirectory(prefix="liblink-elf-x64-sites-") as temporary:
            run_elf_x86_64_sites(cc, archive, Path(temporary), link_flags,
                                config_include, host, args.all_backends)

    if args.native_load_errors_only:
        with tempfile.TemporaryDirectory(prefix="liblink-native-load-errors-") as temporary:
            run_native_load_errors(cc, archive, Path(temporary), link_flags, host,
                                   config_include)
        print("liblink native load-error unit: PASS")
        return 0

    if args.initializer_errors_only:
        with tempfile.TemporaryDirectory(prefix="liblink-initializer-errors-") as temporary:
            run_initializer_errors(
                cc, archive, Path(temporary),
                pe_link_flags if host == "Windows" else link_flags, host,
            )
        print("liblink initializer error units: PASS")
        return 0

    if args.path_ownership_only:
        with tempfile.TemporaryDirectory(prefix="liblink-pathdup-ownership-") as temporary:
            run_pathdup_ownership(cc, archive, Path(temporary), link_flags, host)
        print("liblink native unit tests: PASS (pathdup ownership only)")
        return 0

    if args.macho_x86_operands_only:
        platform_header = (config_include / "ll_platform.h").read_text()
        if not re.search(r"^#define\s+x86_64_HOST_ARCH\s+1\s*$",
                         platform_header, flags=re.MULTILINE):
            raise RuntimeError("operand units require generated x86_64 Darwin config")
        with tempfile.TemporaryDirectory(prefix="liblink-macho-x86-operands-") as temporary:
            run_macho_x86_operands(cc, archive, Path(temporary), link_flags)
        print("liblink Mach-O x86 operand units: PASS")
        return 0

    if args.elf_resources_only:
        if not configured_linux_x86_64_elf_backend(config_include):
            raise RuntimeError("resource units require generated Linux x86_64 config")
        with tempfile.TemporaryDirectory(prefix="liblink-elf-resources-") as temporary:
            run_elf_resources(cc, archive, Path(temporary), link_flags)
        print("liblink ELF resource units: PASS")
        return 0

    if args.mmap_address_bounds_only:
        with tempfile.TemporaryDirectory(prefix="liblink-mmap-address-bounds-") as temporary:
            run_mmap_address_bounds(cc, archive, Path(temporary), link_flags)
        print("liblink mmap address bounds unit: PASS")
        return 0

    if args.macho_table_alignment_only:
        with tempfile.TemporaryDirectory(prefix="liblink-macho-table-alignment-") as temporary:
            run_macho_table_alignment(cc, archive, Path(temporary), host, link_flags)
        print("liblink Mach-O table alignment unit: PASS")
        return 0

    with tempfile.TemporaryDirectory(prefix="liblink-native-units-") as temporary:
        output = Path(temporary)
        if args.pe_resolution_only:
            platform_header = (config_include / "ll_platform.h").read_text()
            windows_x64 = (
                re.search(r"^#define\s+mingw32_HOST_OS\s+1\s*$",
                          platform_header, flags=re.MULTILINE)
                and re.search(r"^#define\s+x86_64_HOST_ARCH\s+1\s*$",
                              platform_header, flags=re.MULTILINE)
            )
            if not windows_x64:
                raise RuntimeError(
                    "PE resolution units require generated Windows x86_64 config"
                )
            run_pei386_resolution_status(cc, archive, output, pe_link_flags)
            print("liblink PE resolution unit: PASS (Windows x86_64 only)")
            return 0

        if args.pe_load_resources_only:
            platform_header = (config_include / "ll_platform.h").read_text()
            windows_x64 = (
                re.search(r"^#define\s+mingw32_HOST_OS\s+1\s*$",
                          platform_header, flags=re.MULTILINE)
                and re.search(r"^#define\s+x86_64_HOST_ARCH\s+1\s*$",
                              platform_header, flags=re.MULTILINE)
            )
            if not windows_x64:
                raise RuntimeError(
                    "PE load-resource unit requires generated Windows x86_64 config"
                )
            run_pei386_load_resources(cc, archive, output, pe_link_flags)
            print("liblink PE load-resource unit: PASS (Windows x86_64 only)")
            return 0

        if args.windows_m32_only:
            m32_test = output / "m32_windows_reservations.exe"
            m32_map = output / "m32_windows_reservations.map"
            m32_trace = run(
                cc
                + [
                    *native_unit_link_arguments(
                        ROOT / "liblink/tests/unit/m32_windows_reservations.c",
                        archive,
                        m32_test,
                        rts_fs_link_flags,
                        [
                            # MinGW LLD rejects GNU ld's -t; --verbose prints resolved inputs.
                            "-Wl,--verbose",
                            linker_map_flag(m32_map),
                        ],
                    ),
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
            run_pei386_common_ownership(cc, archive, output, pe_link_flags)
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

        if not (args.protection_only or args.protection_gap_probes or
                args.resolution_failure_only):
            run_initializer_errors(cc, archive, output, link_flags, host)

        if (host in ("Linux", "Darwin") and
                not (args.protection_only or args.protection_gap_probes or
                     args.resolution_failure_only)):
            run_native_load_errors(cc, archive, output, link_flags, host,
                                   config_include)

        if (host in ("Linux", "Darwin") and mmap_backend and
                not (args.protection_only or args.protection_gap_probes or
                     args.resolution_failure_only)):
            run_mmap_address_bounds(cc, archive, output, link_flags)

        if (args.protection_only or args.protection_gap_probes or
                args.resolution_failure_only):
            mode = "--protection-only" if args.protection_only \
                else "--protection-gap-probes" if args.protection_gap_probes \
                else "--resolution-failure-only"
            run_macho_load_errors(cc, archive, output, link_flags, mode=mode)
            outcome = "protection only" if args.protection_only \
                else "quarantine regression cases" if args.protection_gap_probes \
                else "terminal resolution quarantine cases"
            print(f"liblink native unit tests: PASS ({outcome})")
            return 0

        core_test = output / "core_error_status"
        map_file = output / "core_error_status.map"
        link_trace = run(
            cc
            + native_unit_link_arguments(
                ROOT / "liblink/tests/unit/core_error_status.c",
                archive,
                core_test,
                link_flags,
                ["-Wl,-t", linker_map_flag(map_file)],
            ),
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
            + native_unit_link_arguments(
                ROOT / "liblink/tests/unit/archive_error_status.c",
                archive,
                archive_test,
                link_flags,
                ["-Wl,-t", linker_map_flag(archive_map_file)],
            ),
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

        member = output / "format_archive_member.o"
        run(cc + ["-c", str(ROOT / "liblink/tests/unit/format_archive_member.c"),
                  "-o", str(member)], label="compile native archive registry member")
        registry_test = output / "format_archive"
        run(cc + native_unit_link_arguments(
            ROOT / "liblink/tests/unit/format_archive.c", archive,
            registry_test, link_flags, []),
            label="compile and link format_archive")
        run([str(registry_test), str(member)], label="run format_archive")

        extras_image_test = output / "symbol_extras_image"
        run(cc + native_unit_link_arguments(
            ROOT / "liblink/tests/unit/symbol_extras_image.c", archive,
            extras_image_test, link_flags, []),
            label="compile and link symbol_extras_image")
        run([str(extras_image_test), str(member)], label="run symbol_extras_image")

        extras_storage_test = output / "symbol_extras_storage"
        run(cc + native_unit_link_arguments(
            ROOT / "liblink/tests/unit/symbol_extras_storage.c", archive,
            extras_storage_test, link_flags, []),
            label="compile and link symbol_extras_storage")
        run([str(extras_storage_test)], label="run symbol_extras_storage")

        run_pathdup_ownership(cc, archive, output, link_flags, host)

        if host == "Darwin":
            m32_map_test = output / "m32_map_status"
            m32_map_file = output / "m32_map_status.map"
            m32_map_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/m32_map_status.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *link_flags,
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
                    *link_flags,
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
                *link_flags,
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
                    *link_flags,
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

            cache_test = output / "riscv_cache_range"
            cache_map = output / "riscv_cache_range.map"
            cache_link_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/riscv_cache_range.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *link_flags,
                    "-Wl,-t",
                    linker_map_flag(cache_map),
                    "-o",
                    str(cache_test),
                ],
                label="compile and link riscv_cache_range",
            )
            verify_link_map(
                cache_map,
                archive,
                cache_link_trace,
                included_source="linker/elf_reloc_riscv64.c",
                forbidden_member="elf_reloc_riscv64",
            )
            run([str(cache_test)], label="run riscv_cache_range")

            encoder_test = output / "elf_riscv64_encoder_sites"
            encoder_map = output / "elf_riscv64_encoder_sites.map"
            encoder_trace = run(
                cc + [str(ROOT / "liblink/tests/unit/elf_riscv64_encoder_sites.c"),
                      f"-L{archive.parent}", "-llink", *link_flags, "-Wl,-t",
                      linker_map_flag(encoder_map), "-o", str(encoder_test)],
                label="compile and link elf_riscv64_encoder_sites",
            )
            verify_link_map(encoder_map, archive, encoder_trace,
                            included_source="linker/elf_reloc_riscv64.c",
                            forbidden_member="elf_reloc_riscv64")
            run([str(encoder_test)], label="run elf_riscv64_encoder_sites")

            stub_test = output / "elf_riscv64_stub_sites"
            stub_map = output / "elf_riscv64_stub_sites.map"
            stub_trace = run(
                cc + [str(ROOT / "liblink/tests/unit/elf_riscv64_stub_sites.c"),
                      f"-L{archive.parent}", "-llink", *link_flags, "-Wl,-t",
                      linker_map_flag(stub_map), "-o", str(stub_test)],
                label="compile and link elf_riscv64_stub_sites",
            )
            for member in ("elf_plt", "elf_plt_riscv64"):
                verify_link_map(stub_map, archive, stub_trace,
                                included_source="RISC-V ELF stub bodies",
                                forbidden_member=member)
            run([str(stub_test)], label="run elf_riscv64_stub_sites")

            reloc_sites = output / "elf_riscv64_sites"
            reloc_sites_map = output / "elf_riscv64_sites.map"
            reloc_sites_trace = run(
                cc + [str(ROOT / "liblink/tests/unit/elf_riscv64_sites.c"),
                      f"-L{archive.parent}", "-llink", *link_flags, "-Wl,-t",
                      linker_map_flag(reloc_sites_map), "-o", str(reloc_sites)],
                label="compile and link elf_riscv64_sites",
            )
            verify_link_map(reloc_sites_map, archive, reloc_sites_trace,
                            included_source="linker/elf_reloc_riscv64.c",
                            forbidden_member="elf_reloc_riscv64")
            for member in ("elf_plt", "elf_plt_riscv64"):
                verify_link_map(reloc_sites_map, archive, reloc_sites_trace,
                                included_source="RISC-V ELF apply dependencies",
                                forbidden_member=member)
            run([str(reloc_sites)], label="run elf_riscv64_sites")
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
                    *link_flags,
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
                    *link_flags,
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
                    *link_flags,
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
            run_elf_resources(cc, archive, output, link_flags)
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
                    *link_flags,
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

            run_macho_table_alignment(cc, archive, output, host, link_flags)

            segment_padding_test = output / "macho_segment_padding"
            segment_padding_map = output / "macho_segment_padding.map"
            segment_padding_trace = run(
                cc
                + [
                    str(ROOT / "liblink/tests/unit/macho_segment_padding.c"),
                    f"-L{archive.parent}",
                    "-llink",
                    *link_flags,
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
                    *link_flags,
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
                        *link_flags,
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
                        *link_flags,
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
                run_macho_x86_operands(cc, archive, output, link_flags)
            else:
                print("[macho_x86_error_status] SKIP: generated target is not x86_64")

            run_macho_load_errors(cc, archive, output, link_flags)

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
                        *link_flags,
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

            # Live relocation replay needs LLVM and the generated Mach-O fixtures;
            # opt in when those external test inputs are available.
            if os.environ.get("LIBLINK_MACHO_NOEXEC_ORACLE") == "1":
                if re.search(r"^#define\s+aarch64_HOST_ARCH\s+1\s*$",
                             platform_header, flags=re.MULTILINE):
                    variable = "LIBLINK_MACHO_ARM_NOEXEC"
                elif re.search(r"^#define\s+x86_64_HOST_ARCH\s+1\s*$",
                               platform_header, flags=re.MULTILINE):
                    variable = "LIBLINK_MACHO_X86_NOEXEC"
                else:
                    raise RuntimeError("Mach-O NO_EXEC oracle requires ARM64 or x86_64")
                capture = output / "macho_noexec_capture"
                run(cc + native_unit_link_arguments(
                    ROOT / "liblink/tests/runner/macho_noexec_capture.c", archive,
                    capture, link_flags, []), label="compile Mach-O NO_EXEC capture")
                env = dict(os.environ, **{variable: str(capture)})
                run([sys.executable, "-m", "unittest", "discover", "-s",
                     "liblink/tests/oracle", "-p", "test_macho_noexec.py", "-v"],
                    env=env, label="check Mach-O NO_EXEC normal and synthetic layouts")

    print("liblink native unit tests: PASS")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError) as error:
        print(f"liblink native unit tests: ERROR: {error}", file=sys.stderr)
        raise SystemExit(1) from error
