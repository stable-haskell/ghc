#!/usr/bin/env python3
"""Link a private all-backend/target producer to an ordinary RTS-header client."""
from __future__ import annotations
import argparse
from pathlib import Path
import shlex
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[3]


def run(command: list[str]) -> None:
    print("+ " + shlex.join(command), flush=True)
    subprocess.run(command, check=True, timeout=90)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cc", default="cc")
    parser.add_argument("--config-dir", required=True, type=Path)
    args = parser.parse_args()
    cc = shlex.split(args.cc) + [
        "-std=c11", "-Wall", "-Wextra", "-Werror=vla", "-pedantic",
        "-isystem", str(args.config_dir), "-I", str(ROOT / "liblink/include"),
        "-I", str(ROOT / "liblink/src"),
    ]
    source = str(ROOT / "liblink/tests/unit/shared_layout.c")
    with tempfile.TemporaryDirectory(prefix="liblink-shared-layout-") as temporary:
        directory = Path(temporary)
        client = directory / "client.o"
        run(cc + ["-c", source, "-o", str(client)])
        for target in ("I386", "X86_64", "ARM", "AARCH64", "RISCV64", "POWERPC", "SPARC"):
            producer = directory / "producer.o"
            binary = directory / "shared_layout"
            run(cc + ["-DLL_LAYOUT_PRODUCER", "-DLL_ALL_BACKENDS",
                      f"-DLL_TARGET_ARCH=LL_ARCH_{target}", "-c", source,
                      "-o", str(producer)])
            run(cc + [str(client), str(producer), "-o", str(binary)])
            run([str(binary)])
    print("shared_layout: PASS (seven private targets vs native client)")


if __name__ == "__main__":
    main()
