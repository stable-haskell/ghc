#!/usr/bin/env python3
"""Check native defaults, explicit targets, and unchanged host definitions."""
from __future__ import annotations

import argparse
import os
from pathlib import Path
import shlex
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[3]


def run(argv: list[str], *, success: bool = True) -> str:
    print("+ " + shlex.join(argv), flush=True)
    result = subprocess.run(
        argv, stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=90
    )
    if (result.returncode == 0) != success:
        raise RuntimeError(result.stdout + result.stderr)
    return result.stdout + result.stderr


def host_definitions(output: str) -> set[str]:
    return {
        line for line in output.splitlines()
        if "_HOST_ARCH " in line or "_HOST_OS " in line
        or line.startswith(("#define __MSVCRT__ ", "#define _REENTRANT "))
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cc", default=os.environ.get("CC", "cc"))
    parser.add_argument("--config-dir", required=True, type=Path)
    args = parser.parse_args()
    cc = shlex.split(args.cc) + [
        "-std=c11", "-Wall", "-Wextra", "-Werror=vla", "-pedantic",
        "-isystem", str(args.config_dir), "-I", str(ROOT / "liblink/src"),
        "-I", str(ROOT / "liblink/include"),
    ]
    # Expected widths and capabilities are independent of the compiler host.
    targets = {
        "I386": (4, 0, 0, 0, 0),
        "X86_64": (8, 0, 1, 0, 16),
        "ARM": (4, 1, 1, 0, 16),
        "AARCH64": (8, 1, 1, 1, 0),
        "RISCV64": (8, 1, 1, 0, 8),
        "POWERPC": (4, 0, 1, 0, 16),
        "SPARC": (4, 0, 0, 0, 0),
    }
    with tempfile.TemporaryDirectory(prefix="liblink-target-config-") as temporary:
        directory = Path(temporary)
        source = ROOT / "liblink/tests/unit/target_config.c"
        empty = directory / "empty.c"
        empty.write_text("\n")
        original = host_definitions(run(
            cc + ["-E", "-dM", "-include", "ll_platform.h", str(empty)]
        ))
        assert original, "configured host definitions are missing"
        run(cc + ["-c", str(source), "-o", str(directory / "native.o")])
        for arch, (width, plt, extras, macho_plt, extra_size) in targets.items():
            flags = [
                f"-DLL_TARGET_ARCH=LL_ARCH_{arch}",
                f"-DLL_EXPECT_TARGET=LL_ARCH_{arch}",
                f"-DLL_EXPECT_WORD_SIZE={width}", f"-DLL_EXPECT_ELF_PLT={plt}",
                f"-DLL_EXPECT_ELF_EXTRAS={extras}",
                f"-DLL_EXPECT_MACHO_PLT={macho_plt}",
                f"-DLL_EXPECT_EXTRA_SIZE={extra_size}",
            ]
            run(cc + flags + ["-c", str(source), "-o", str(directory / (arch + ".o"))])
            current = host_definitions(run(
                cc + flags + ["-E", "-dM", "-include", "ll_target.h", str(empty)]
            ))
            assert current == original, f"{arch}: target selection changed host definitions"
        rejected = run(cc + ["-DLL_TARGET_ARCH=999", "-c", str(source),
                             "-o", str(directory / "invalid.o")], success=False)
        assert "unsupported LL_TARGET_ARCH" in rejected, rejected
    print("target_config: PASS (native, seven explicit targets, host preservation, invalid target)")


if __name__ == "__main__":
    main()
