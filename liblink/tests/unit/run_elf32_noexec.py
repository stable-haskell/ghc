#!/usr/bin/env python3
"""Build both ELF32 backends on a 64-bit Linux host and replay their goldens."""
import argparse
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[3]
LIBLINK = ROOT / 'liblink'


def run(command, *, env=None):
    result = subprocess.run(command, cwd=ROOT, env=env, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=90)
    if result.returncode:
        sys.stderr.write(result.stdout + result.stderr)
        raise RuntimeError(f"command failed: {shlex.join(command)}")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config-dir', type=Path, required=True,
                        help='native generated liblink include directory')
    parser.add_argument('--rts-fs-dir', type=Path, required=True,
                        help='directory containing fs.c and fs.h')
    parser.add_argument('--cc', default=os.environ.get('CC', 'cc'))
    parser.add_argument('--ar', default=os.environ.get('AR', 'ar'))
    args = parser.parse_args()
    if not sys.platform.startswith('linux') or sys.maxsize <= 2**32:
        parser.error('requires a 64-bit Linux host')
    cc, ar = shlex.split(args.cc), shlex.split(args.ar)
    sources = [LIBLINK / line.strip() for line in
               (LIBLINK / 'native-sources.list').read_text().splitlines()
               if line.strip() and not line.lstrip().startswith('#')]
    common = ['-std=gnu11', '-Wall', '-Wextra', '-Werror=vla', '-pedantic',
              '-DLL_ASSERTS', '-g', '-O0', '-fsanitize=address,undefined',
              '-fno-sanitize-recover=undefined', '-fno-omit-frame-pointer',
              '-I' + str(args.config_dir.resolve()), '-I' + str(args.rts_fs_dir.resolve()),
              '-I' + str(LIBLINK / 'include'), '-I' + str(LIBLINK / 'src')]
    env = os.environ.copy()
    env.setdefault('ASAN_OPTIONS', 'detect_leaks=1')
    with tempfile.TemporaryDirectory(prefix='liblink-elf32-noexec-') as temp:
        root = Path(temp)
        for target in ('i386', 'arm'):
            print(f'Building sanitized {target} backend', flush=True)
            out = root / target
            out.mkdir()
            flags = common + ['-DLL_TARGET_ARCH=LL_ARCH_' + target.upper()]
            objects = []
            for source in sources:
                obj = out / (source.stem + '.o')
                run(cc + flags + ['-c', str(source), '-o', str(obj)])
                objects.append(obj)
            fs = out / 'fs.o'
            run(cc + common + ['-DFS_NAMESPACE=rts', '-c',
                               str(args.rts_fs_dir.resolve() / 'fs.c'), '-o', str(fs)])
            objects.append(fs)
            archive = out / 'liblink.a'
            run(ar + ['rcs', str(archive)] + [str(obj) for obj in objects])
            for runner, variable in [('elf_capture', 'CAPTURE'), ('elf32_apply', 'APPLY')]:
                exe = out / runner
                run(cc + flags + [str(LIBLINK / 'tests/runner' / (runner + '.c')),
                                   str(archive), '-ldl', '-pthread', '-o', str(exe)])
                env['LIBLINK_ELF32_' + target.upper() + '_' + variable] = (
                    str(exe) + (' --noexec' if variable == 'CAPTURE' else ''))
            control = out / 'noexec_storage'
            run(cc + flags + [str(LIBLINK / 'tests/unit/noexec_storage.c'),
                               str(LIBLINK / 'src/ll_error.c'), '-o', str(control)])
            run([str(control)], env=env)
            if target == 'arm':
                short = out / 'elf_arm_sites'
                run(cc + flags + [str(LIBLINK / 'tests/unit/elf_arm_sites.c'),
                                   str(archive), '-ldl', '-pthread', '-o', str(short)])
                run([str(short)], env=env)
        print('Checking all normal goldens and sanitized NO_EXEC captures', flush=True)
        result = run([sys.executable, '-m', 'unittest', 'discover', '-s',
                      str(LIBLINK / 'tests/oracle'), '-p', 'test_elf32*.py', '-v'], env=env)
        sys.stdout.write(result.stdout + result.stderr)


if __name__ == '__main__':
    main()
