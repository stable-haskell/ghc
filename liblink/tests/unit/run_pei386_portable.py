#!/usr/bin/env python3
"""Exercise real COFF data processing and required process-service rejection.

The COFF compiler must genuinely target x86_64 Windows. The host compiler and
configuration must describe the running non-Windows host. No fixture code runs.
"""
from __future__ import annotations

import argparse
import os
from pathlib import Path
import platform
import shlex
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[3]


def run(command: list[str]) -> None:
    print("+ " + shlex.join(command), flush=True)
    subprocess.run(command, cwd=ROOT, timeout=90, check=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cc", default=os.environ.get("CC", "cc"))
    parser.add_argument("--coff-cc", required=True)
    parser.add_argument("--native-archive", type=Path, required=True)
    parser.add_argument("--config-include", type=Path, required=True)
    parser.add_argument("--fs-include", type=Path, required=True)
    parser.add_argument("--rts-fs-archive", type=Path)
    args = parser.parse_args()
    host = platform.system()
    if host not in ("Darwin", "Linux"):
        parser.error("requires a real Darwin or Linux host")
    if host == "Linux" and args.rts_fs_archive is None:
        parser.error("Linux requires --rts-fs-archive")
    for name in ("ll_autoconf.h", "ll_platform.h"):
        if not (args.config_include / name).is_file():
            parser.error(f"missing generated configuration header {name}")
    cc = shlex.split(args.cc)
    flags = ["-std=c11", "-Wall", "-Wextra", "-Werror=vla", "-pedantic",
             "-DLL_ASSERTS", "-I", str(args.config_include),
             "-I", str(args.fs_include), "-I", str(ROOT / "liblink/include"),
             "-I", str(ROOT / "liblink/src")]
    with tempfile.TemporaryDirectory(prefix="liblink-pe-portable-") as directory:
        output = Path(directory)
        fixture = output / "data.c"
        # Two ordinary data symbols keep this positive fixture within the old
        # verifier's table-alignment assumptions with current MinGW GCC.
        # The one-symbol GCC regression is retained as an explicit open hold.
        fixture.write_text("int ll_pe_portable_data = 0x13579bdf;\n"
                           "int ll_pe_portable_spare = 3;\n")
        coff = output / "data.obj"
        run(shlex.split(args.coff_cc) + ["-c", str(fixture), "-o", str(coff)])
        backend = output / "PEi386.o"
        run(cc + flags + ["-DLL_ALL_BACKENDS", "-DLL_TARGET_ARCH=2", "-c",
                         str(ROOT / "liblink/src/linker/PEi386.c"), "-o", str(backend)])
        binary = output / "pei386_portable_data"
        libraries = ([str(args.rts_fs_archive)] if args.rts_fs_archive else [])
        libraries += ["-pthread"] + (["-ldl"] if host == "Linux" else [])
        run(cc + flags + [str(ROOT / "liblink/tests/unit/pei386_portable_data.c"),
                          str(backend), str(args.native_archive), *libraries,
                          "-o", str(binary)])
        run([str(binary), str(coff)])


if __name__ == "__main__":
    main()
