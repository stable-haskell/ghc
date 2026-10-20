#!/usr/bin/env python3
"""Exercise Make/config profiling metadata without compiling any Haskell."""

import argparse
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile
from types import SimpleNamespace


def check_case(root, work, make, profiled, dynamic):
    # Model ghc-config output from --info. Profiling is an ABI: RTS ways have
    # no p component, and both ABIs use ordinary interface names.
    metadata = {
        'GhcStage': '2', 'HostOS': 'linux', 'WORDSIZE': '64',
        'TARGETPLATFORM': 'x86_64-unknown-linux',
        'TargetOS_CPP': 'linux', 'TargetARCH_CPP': 'x86_64',
        'GhcDebugAssertions': 'NO', 'GhcWithNativeCodeGen': 'YES',
        'GhcWithInterpreter': 'NO', 'CrossCompiling': 'NO',
        'GhcWithRtsLinker': 'YES', 'GhcUnregisterised': 'NO',
        'GhcWithSMP': 'YES', 'GhcLibdir': str(work),
        'GhcDynamic': 'YES' if dynamic else 'NO',
        'GhcProfiled': 'YES' if profiled else 'NO',
        'GhcRTSWays': 'v thr dyn' if dynamic else 'v thr',
    }
    config_mk = work / 'ghc-config.mk'
    config_mk.write_text(''.join(f'{key}={value}\n' for key, value in metadata.items()))
    capture = work / 'capture.py'
    capture.write_text('import json, sys\nprint(json.dumps(sys.argv[1:]))\n')
    (work / 'timeout-tool').touch()
    makefile = work / 'Makefile'
    makefile.write_text(
        f'include {root.as_posix()}/mk/boilerplate.mk\n'
        f'include {root.as_posix()}/mk/test.mk\n')
    # Run the actual test recipe. Only its Python runner is replaced; no GHC
    # or timeout build is needed. Avoid unrelated ghc-pkg/gdb feature queries.
    command = make + [
        '-s', '--no-print-directory', '-f', str(makefile), 'test',
        f'TOP={root.as_posix()}', f'ghc_config_mk={config_mk}',
        f'PYTHON={sys.executable}', f'RUNTESTS={shlex.quote(str(capture))}',
        'TIMEOUT_PROGRAM=timeout-tool', 'BIGNUM_GMP=',
        'HAVE_GDB=NO', 'HAVE_READELF=NO',
        'EXTRA_RUNTEST_OPTS=--fixture-ghci-extension=$(ghciWayExt)',
    ] + [f'{tool}={sys.executable}' for tool in
         ['TEST_HC', 'GHC_PKG', 'HSC2HS', 'HP2PS_ABS', 'HPC']]
    env = os.environ.copy()
    # Outer testsuite Make variables must not leak into these four controls.
    env.pop('MAKEFLAGS', None)
    env.pop('MFLAGS', None)
    env.pop('MAKEOVERRIDES', None)
    completed = subprocess.run(command, cwd=work, env=env, check=True,
                               capture_output=True, text=True, timeout=30)
    arguments = json.loads(completed.stdout)
    parser = argparse.ArgumentParser()
    parser.add_argument('-e', action='append', default=[])
    parser.add_argument('--config-file', action='append', default=[])
    parser.add_argument('--config', action='append', default=[])
    parser.add_argument('--fixture-ghci-extension')
    args, _ = parser.parse_known_args(arguments)

    # Use the real defaults and config entrypoint, in runtests.py's order.
    from testglobals import TestConfig
    config = TestConfig()
    probes = []

    def compile_probe(command, **kwargs):
        flags = shlex.split(command)
        probes.append(flags)
        supported = (profiled or '-prof' not in flags) and (dynamic or '-dynamic' not in flags)
        return SimpleNamespace(returncode=0 if supported else 1)

    namespace = {
        'config': config, 'Path': Path, 'ghc_env': {},
        'subprocess': SimpleNamespace(run=compile_probe, DEVNULL=subprocess.DEVNULL),
    }
    for expression in args.e:
        exec(expression, namespace)
    for config_file in args.config_file:
        path = Path(config_file)
        exec(compile(path.read_text(), str(path), 'exec'), namespace)
    for setting in args.config:
        field, value = setting.split('=', 1)
        setattr(config, field, value)
    namespace['get_compiler_info']()

    case = f'profiled={profiled}, dynamic={dynamic}'
    assert config.compiler_profiled == profiled, f'GhcProfiled propagation: {case}'
    assert config.ghc_dynamic == dynamic, case
    assert args.fixture_ghci_extension == ('dyn_hi' if dynamic else 'hi'), (
        f'ordinary interface extension: {case}: {args.fixture_ghci_extension}')
    assert config.have_profiling == profiled, case
    assert config.have_profiling_dynamic == (profiled and dynamic), case
    assert ('profasm' in config.compile_ways) == profiled, case
    assert ('profthreaded' in config.run_ways) == profiled, case
    assert any('-prof' in flags for flags in probes), 'profiling must be probed'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--testsuite-root', type=Path, required=True)
    parser.add_argument('--make', required=True,
                        help='GNU Make command selected by the outer driver')
    args = parser.parse_args()
    root = args.testsuite_root.resolve()
    make = shlex.split(args.make)
    assert make, 'the selected GNU Make command is empty'
    sys.path.insert(0, str(root / 'driver'))
    with tempfile.TemporaryDirectory(prefix='profiling-metadata-') as scratch:
        for profiled in [False, True]:
            for dynamic in [False, True]:
                work = Path(scratch) / f'{profiled}-{dynamic}'
                work.mkdir()
                check_case(root, work, make, profiled, dynamic)


if __name__ == '__main__':
    main()
