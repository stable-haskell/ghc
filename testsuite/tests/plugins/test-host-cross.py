#!/usr/bin/env python3
"""Compile target TH while loading a plugin built with a native host GHC.

Run with host and cross compilers from the same compiler build. This is kept
separate from ordinary plugin tests, whose fixtures use the compiler under test
to build plugins and therefore cannot build host plugins with a cross-compiler.
"""

import argparse
import ast
from pathlib import Path
import shutil
import subprocess
import tempfile


def run(command, *, cwd=None):
    print("+ " + " ".join(map(str, command)), flush=True)
    return subprocess.run(command, cwd=cwd, check=True, text=True,
                          capture_output=True, timeout=180)


def info(compiler, options=()):
    return dict(ast.literal_eval(run([compiler, *options, "--info"]).stdout))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--host-ghc", type=Path, required=True)
    parser.add_argument("--host-ghc-pkg", type=Path, required=True)
    parser.add_argument("--cross-ghc", type=Path, required=True)
    parser.add_argument("--cross-option", action="append", default=[])
    parser.add_argument("--expected-th-platform", required=True,
                        help="target System.Info arch-os, for example wasm32-wasi")
    args = parser.parse_args()
    # Preserve the executable name: multi-target installations select their
    # target through symlinks such as wasm32-unknown-wasi-ghc -> ghc.
    host = args.host_ghc.absolute()
    host_pkg = args.host_ghc_pkg.absolute()
    cross = args.cross_ghc.absolute()
    host_info, cross_info = info(host), info(cross, args.cross_option)
    host_target = host_info["Target platform"]
    cross_target = cross_info["Target platform"]
    if host_target == cross_target:
        raise SystemExit("the cross compiler must have a different target platform")
    if host_info["Host platform"] != host_target:
        raise SystemExit("the host compiler must be native")
    libdir = run([host, "--print-libdir"]).stdout.strip()
    source = Path(__file__).resolve().parent
    shared = host_info.get("GHC Dynamic") == "YES"
    with tempfile.TemporaryDirectory(prefix="plugin-host-cross-") as scratch:
        work = Path(scratch)
        shutil.copytree(source / "simple-plugin", work / "simple-plugin",
                        ignore=shutil.ignore_patterns("pkg.*", "*.o", "*.hi", "*.dyn_o", "*.dyn_hi"))
        shutil.copy2(source / "PluginHostCross.hs", work)
        run(["make", "-s", "--no-print-directory", "-C", work / "simple-plugin",
             "package.cross", "TOP=" + str(source.parents[1]),
             "TEST_HC=" + str(host), "GHC_PKG=" + str(host_pkg),
             "CABAL_PLUGIN_BUILD=--enable-library-vanilla " +
             ("--enable-shared" if shared else "--disable-shared")], cwd=work)
        result = run([cross, *args.cross_option, "-v0", "-fforce-recomp", "-c",
                      "PluginHostCross.hs", "-fexternal-interpreter",
                      "-plugin-libdir", libdir, "-plugin-package-db",
                      "simple-plugin/pkg.cross/local.package.conf",
                      "-plugin-package", "simple-plugin", "-fplugin", "Simple.Plugin",
                      "-fplugin-opt", "Simple.Plugin:cross"], cwd=work)
        expected = ("Simple Plugin Passes Queried\nGot options: cross\n"
                    "Simple Plugin Pass Run\n")
        if result.stderr != expected:
            raise AssertionError("unexpected plugin output: " + repr(result.stderr))
        if result.stdout != "TH platform: " + args.expected_th_platform + "\n":
            raise AssertionError("TH ran on the wrong platform: " + repr(result.stdout))
        if not (work / "PluginHostCross.o").stat().st_size:
            raise AssertionError("the target object is empty")
    print("host plugin and target TH compiled successfully for " + cross_target)


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as failure:
        print(failure.stdout, end="")
        print(failure.stderr, end="")
        raise
