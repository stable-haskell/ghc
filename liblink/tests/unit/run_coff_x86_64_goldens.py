#!/usr/bin/env python3
"""Capture normal and NO_EXEC AMD64 COFF relocations against saved goldens."""
import argparse
import os
import platform
from pathlib import Path
import shlex
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[3]


def run(command, **kwargs):
    print('+ ' + shlex.join(map(str, command)), flush=True)
    return subprocess.run(command, cwd=ROOT, check=True, timeout=120, **kwargs)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cc', default=os.environ.get('CC', 'cc'))
    parser.add_argument('--native-archive', required=True, type=Path)
    parser.add_argument('--config-include', required=True, type=Path)
    parser.add_argument('--fs-include', required=True, type=Path)
    parser.add_argument('--rts-fs-archive', type=Path)
    args = parser.parse_args()
    cc = shlex.split(args.cc)
    flags = ['-std=c11', '-DLL_ALL_BACKENDS', '-DLL_TARGET_ARCH=2',
             '-I', str(args.config_include), '-I', str(args.fs_include),
             '-I', str(ROOT / 'liblink/include'), '-I', str(ROOT / 'liblink/src')]
    libraries = ([str(args.rts_fs_archive)] if args.rts_fs_archive else []) + ['-pthread']
    if platform.system() == 'Linux':
        if args.rts_fs_archive is None:
            parser.error('Linux requires --rts-fs-archive')
        libraries.append('-ldl')
    with tempfile.TemporaryDirectory(prefix='liblink-coff-goldens-') as temp:
        temp = Path(temp)
        allocator = temp / 'allocator.o'
        run(cc + flags + ['-c', 'liblink/src/ll_noexec.c', '-o', str(allocator)])
        host = temp / 'host.o'
        run(cc + flags + ['-c', 'liblink/src/linker/PEi386Host.c', '-o', str(host)])
        for mode in ('normal', 'noexec'):
            pe = temp / (mode + '.o')
            replacement = ['-Dll_pe_host_image_base=capture_pe_image_base'] if mode == 'normal' else []
            run(cc + flags + replacement + ['-c', 'liblink/src/linker/PEi386.c', '-o', str(pe)])
            executable = temp / mode
            real_base = ['-DLL_CAPTURE_REAL_IMAGE_BASE'] if mode == 'noexec' else []
            run(cc + flags + real_base + ['liblink/tests/runner/coff_capture.c', str(pe), str(host), str(allocator),
                str(args.native_archive), *libraries, '-o', str(executable)])
            for name in ('ADDR64', 'ADDR32NB', 'REL32', 'SECREL'):
                stem = 'IMAGE_REL_AMD64_' + name
                obj = ROOT / 'liblink/tests/fixtures/generated/coff-x86_64' / (stem + '.o')
                golden = ROOT / 'liblink/tests/golden/coff-x86_64' / (stem + '.json')
                for iteration in range(2):
                    capture = temp / (mode + '-' + stem + '-' + str(iteration) + '.json')
                    with capture.open('w') as output:
                        run([str(executable), *(['--noexec'] if mode == 'noexec' else []), str(obj)], stdout=output)
                    run(['python3', 'liblink/tests/oracle/coff_x86_64_check.py', '--object', str(obj),
                         '--capture', str(capture), '--golden', str(golden)])


if __name__ == '__main__':
    main()
