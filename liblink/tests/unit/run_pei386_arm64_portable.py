#!/usr/bin/env python3
"""Compile an ARM64 COFF fixture and test the real PE relocation backend."""
from __future__ import annotations
import argparse
import os
from pathlib import Path
import platform
import shlex
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[3]


def run(args: list[str]) -> None:
    print('+ ' + shlex.join(args), flush=True)
    subprocess.run(args, cwd=ROOT, timeout=120, check=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cc', default=os.environ.get('CC', 'cc'))
    parser.add_argument('--coff-cc', required=True)
    parser.add_argument('--native-archive', required=True, type=Path)
    parser.add_argument('--config-include', required=True, type=Path)
    parser.add_argument('--fs-include', required=True, type=Path)
    parser.add_argument('--rts-fs-archive', type=Path)
    args = parser.parse_args()
    system = platform.system()
    if system not in ('Darwin', 'Linux'):
        parser.error('requires a real Darwin or Linux host')
    if system == 'Linux' and args.rts_fs_archive is None:
        parser.error('Linux requires --rts-fs-archive')
    cc = shlex.split(args.cc)
    flags = ['-std=c11', '-Wall', '-Wextra', '-Werror=vla', '-pedantic',
             '-DLL_ASSERTS', '-DLL_ALL_BACKENDS', '-DLL_TARGET_ARCH=4',
             '-I', str(args.config_include), '-I', str(args.fs_include),
             '-I', str(ROOT / 'liblink/include'), '-I', str(ROOT / 'liblink/src')]
    libraries = ([str(args.rts_fs_archive)] if args.rts_fs_archive else [])
    libraries += ['-pthread'] + (['-ldl'] if system == 'Linux' else [])
    with tempfile.TemporaryDirectory(prefix='liblink-pe-arm64-') as directory:
        output = Path(directory)
        fixture = output / 'fixture.obj'
        run(shlex.split(args.coff_cc) + ['-O0', '-c',
            str(ROOT / 'liblink/tests/fixtures/coff-aarch64/relocations.c'), '-o', str(fixture)])
        if fixture.read_bytes()[:2] != b'\x64\xaa':
            raise RuntimeError('fixture compiler did not produce ARM64 COFF')
        objects = []
        for source in ('linker/PEi386Host.c', 'RtsSymbolInfo.c', 'll_noexec.c'):
            obj = output / (Path(source).stem + '.o')
            run(cc + flags + ['-c', str(ROOT / 'liblink/src' / source), '-o', str(obj)])
            objects.append(str(obj))
        binary = output / 'pei386_arm64_reloc'
        run(cc + flags + [str(ROOT / 'liblink/tests/unit/pei386_arm64_reloc.c'),
                          *objects, str(args.native_archive), *libraries, '-o', str(binary)])
        run([str(binary), str(fixture)])


if __name__ == '__main__':
    main()
