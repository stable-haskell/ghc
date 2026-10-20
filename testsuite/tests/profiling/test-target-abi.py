#!/usr/bin/env python3
"""Exercise a native compiler and its separate profiling target installation.

Requires a complete stage3-profile build and the matching patched Cabal. All
package databases, Cabal stores and build products created here are temporary.
"""

import argparse
import ast
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import textwrap


def run(command, *, cwd=None, env=None, check=True):
    print("+ " + " ".join(map(str, command)), flush=True)
    result = subprocess.run(command, cwd=cwd, env=env, check=False, text=True,
                            capture_output=True, timeout=600)
    if check and result.returncode:
        print(result.stdout, end="")
        print(result.stderr, end="")
        raise subprocess.CalledProcessError(result.returncode, command,
                                            result.stdout, result.stderr)
    return result


def write(path, content):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(textwrap.dedent(content).lstrip())


def check_profile_file_names(root):
    """Reject profiling file ways throughout a target's build or install tree."""
    assert root.is_dir(), "missing profiling artifact directory: " + str(root)
    suffix = re.compile(
        r"(?:\.p_(?:dyn_)?(?:hi|o|hc|s)(?:-boot)?|_p(?:_dyn)?_o_split|"
        r"_p(?:_dyn)?(?:-ghc[^/]*)?\.(?:a|so(?:\.\d+)*|dylib|dll(?:\.a)?))$")
    visited = set()
    forbidden = []

    def walk_error(error):
        raise error

    for directory, directories, files in os.walk(root, followlinks=True, onerror=walk_error):
        stat = os.stat(directory)
        identity = stat.st_dev, stat.st_ino
        if identity in visited:
            directories.clear()
            continue
        visited.add(identity)
        forbidden.extend(str(Path(directory) / name) for name in directories + files
                         if suffix.search(name))
    assert not forbidden, "profiling filename suffixes: " + repr(forbidden)


def fixture(work):
    write(work / "cabal.project", """
        packages: generator app
        active-repositories: :none
    """)
    write(work / "cabal.project.profile", """
        import: cabal.project
        constraints: abi-workflow +profile-target
    """)
    write(work / "generator/abi-generator.cabal", """
        cabal-version: 3.0
        name: abi-generator
        version: 0
        build-type: Simple
        executable abi-generator
          main-is: Main.hs
          c-sources: ordinary.c
          build-depends: base
          default-language: Haskell2010
    """)
    write(work / "generator/Main.hs", r'''
        {-# LANGUAGE ForeignFunctionInterface #-}
        import Foreign.C.Types
        import System.Environment
        foreign import ccall unsafe "rts_isProfiled" isProfiled :: IO CInt
        foreign import ccall unsafe "generator_c_profiled" cProfiled :: IO CInt
        main = do
          profiled <- isProfiled
          c <- cProfiled
          if profiled /= 0 || c /= 0 then fail "build tool used profiling ABI" else do
            args <- getArgs
            case args of
              [] -> putStrLn "module Generated where\nanswer :: Int\nanswer = 42"
              [_, input, output] -> readFile input >>= writeFile output
              _ -> fail "unexpected native preprocessor arguments"
    ''')
    write(work / "generator/ordinary.c", """
        int generator_c_profiled(void)
        {
        #ifdef PROFILING
            return 1;
        #else
            return 0;
        #endif
        }
    """)
    write(work / "app/abi-workflow.cabal", """
        cabal-version: 3.0
        name: abi-workflow
        version: 0
        build-type: Custom
        flag profile-target
          default: False
          manual: True
        custom-setup
          setup-depends: base, Cabal, process
        library
          hs-source-dirs: lib
          exposed-modules: AbiLibrary
          other-modules: Probe ForeignProbe Generated TargetLayout
          c-sources: cbits/profiling.c
          cxx-sources: cbits/profiling-cxx.cpp
          build-depends: base, template-haskell
          build-tool-depends: abi-generator:abi-generator
          ghc-options: -F -pgmF abi-generator
          ghc-prof-options: -fprof-cafs
          default-language: Haskell2010
          if flag(profile-target)
            cpp-options: -DEXPECT_PROFILE
        executable abi-workflow
          hs-source-dirs: exe
          main-is: Main.hs
          build-depends: base, abi-workflow
          ghc-options: -rtsopts
          default-language: Haskell2010
          if flag(profile-target)
            cpp-options: -DEXPECT_PROFILE
    """)
    write(work / "app/Setup.hs", r'''
        {-# LANGUAGE CPP, ForeignFunctionInterface #-}
        #ifdef PROFILING
        #error Custom Setup must use the ordinary host ABI
        #endif
        import Distribution.Simple
        import Foreign.C.Types
        import System.Process
        foreign import ccall unsafe "rts_isProfiled" isProfiled :: IO CInt
        main = do
          profiled <- isProfiled
          if profiled /= 0 then fail "Setup used profiling ABI" else
            defaultMainWithHooks simpleUserHooks
              { preBuild = \args flags -> do
                  generated <- readProcess "abi-generator" [] ""
                  writeFile "lib/Generated.hs" generated
                  preBuild simpleUserHooks args flags
              }
    ''')
    write(work / "app/lib/Generated.hs", """
        module Generated where
        answer :: Int
        answer = error "native build tool did not run"
    """)
    write(work / "app/lib/Probe.hs", r'''
        {-# LANGUAGE ForeignFunctionInterface #-}
        module Probe where
        import Foreign.C.Types
        foreign import ccall unsafe "rts_isProfiled" isProfiled :: IO CInt
    ''')
    write(work / "app/lib/ForeignProbe.hs", r'''
        {-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
        module ForeignProbe (foreignProfiled) where
        import Foreign.C.Types
        import Language.Haskell.TH.Syntax
        $(do
          addForeignSource LangC $ unlines
            [ "int th_c_profiled(void) {"
            , "#ifdef PROFILING"
            , "return 1;"
            , "#else"
            , "return 0;"
            , "#endif"
            , "}"
            ]
          pure [])
        foreign import ccall unsafe "th_c_profiled" foreignProfiled :: IO CInt
    ''')
    write(work / "app/lib/AbiLibrary.hs", r'''
        {-# LANGUAGE CPP, ForeignFunctionInterface, TemplateHaskell #-}
        #if defined(EXPECT_PROFILE) && !defined(PROFILING)
        #error target Haskell CPP must define PROFILING
        #elif !defined(EXPECT_PROFILE) && defined(PROFILING)
        #error ordinary Haskell CPP must not define PROFILING
        #endif
        module AbiLibrary (value, checkPreprocessors) where
        import Generated
        import ForeignProbe
        import Foreign.C.Types
        import Language.Haskell.TH
        import Probe
        import TargetLayout
        foreign import ccall unsafe "target_c_profiled" cProfiled :: IO CInt
        foreign import ccall unsafe "target_cxx_profiled" cxxProfiled :: IO CInt
        checkPreprocessors :: IO ()
        checkPreprocessors = do
          c <- cProfiled
          cxx <- cxxProfiled
        #ifdef EXPECT_PROFILE
          if c /= 1 || cxx /= 1 || layoutProfiling /= 1 then fail "target C/C++ or hsc2hs lost PROFILING" else pure ()
        #else
          if c /= 0 || cxx /= 0 || layoutProfiling /= 0 then fail "native C/C++ or hsc2hs received PROFILING" else pure ()
        #endif
        value :: Int
        value = $(do
          profiled <- runIO isProfiled
          foreignABI <- runIO foreignProfiled
        #ifdef EXPECT_PROFILE
          if profiled /= 1 || foreignABI /= 1 then fail "TH or its foreign object used native ABI" else [| answer |])
        #else
          if profiled /= 0 || foreignABI /= 0 then fail "native TH or its foreign object used profiling ABI" else [| answer |])
        #endif
    ''')
    write(work / "app/lib/TargetLayout.hsc", """
        #if defined(EXPECT_PROFILE) && !defined(PROFILING)
        #error profiling target hsc2hs must define PROFILING
        #elif !defined(EXPECT_PROFILE) && defined(PROFILING)
        #error ordinary host hsc2hs must not define PROFILING
        #endif
        #ifdef PROFILING
        #define ABI_PROFILING 1
        #else
        #define ABI_PROFILING 0
        #endif
        module TargetLayout where
        layoutProfiling :: Int
        layoutProfiling = #{const ABI_PROFILING}
    """)
    write(work / "app/cbits/profiling.c", """
        int target_c_profiled(void)
        {
        #ifdef PROFILING
            return 1;
        #else
            return 0;
        #endif
        }
    """)
    write(work / "app/cbits/profiling-cxx.cpp", '''
        extern "C" int target_cxx_profiled(void)
        {
        #ifdef PROFILING
            return 1;
        #else
            return 0;
        #endif
        }
    ''')
    write(work / "app/exe/Main.hs", r'''
        {-# LANGUAGE CPP, ForeignFunctionInterface #-}
        import AbiLibrary
        import Foreign.C.Types
        foreign import ccall unsafe "rts_isProfiled" isProfiled :: IO CInt
        main = do
          checkPreprocessors
          profiled <- isProfiled
        #ifdef EXPECT_PROFILE
          if profiled == 0 then fail "executable used native ABI" else print value
        #else
          if profiled /= 0 then fail "native executable used profiling ABI" else print value
        #endif
    ''')


def check_host_dynamic_th(host, host_info, work, env):
    """The bundled ordinary compiler retains its dynamic external interpreter."""
    if host_info.get("GHC Dynamic") != "YES":
        return
    source = work / "host-dynamic-th"
    write(source / "Probe.hs", r'''
        {-# LANGUAGE ForeignFunctionInterface #-}
        module Probe where
        import Foreign.C.Types
        foreign import ccall unsafe "rts_isProfiled" isProfiled :: IO CInt
        foreign import ccall unsafe "rts_isDynamic" isDynamic :: IO CInt
    ''')
    write(source / "Main.hs", r'''
        {-# LANGUAGE TemplateHaskell #-}
        import Language.Haskell.TH
        import Probe
        answer :: Int
        answer = $(do
          profiled <- runIO isProfiled
          dynamic <- runIO isDynamic
          if profiled /= 0 || dynamic /= 1 then fail "ordinary dynamic TH used the wrong RTS ABI" else [| 42 |])
        main = print answer
    ''')
    executable = source / ("host-dynamic-th.exe" if os.name == "nt" else "host-dynamic-th")
    run([host, "--make", "Main.hs", "-v0", "-fforce-recomp", "-dynamic",
         "-fexternal-interpreter", "-o", executable], cwd=source, env=env)
    assert run([executable], cwd=source, env=env).stdout.strip() == "42"


def check_bundled_ffi(profile_pkg, profile_lib, env):
    """Generated iservs must not rely on the installer's system libffi SDK."""
    for package in ("rts", "ghci"):
        dependencies = run([profile_pkg, "field", package, "depends", "--simple-output"],
                           env=env).stdout.split()
        assert any(dep.startswith("libffi-clib-3.5.2-") for dep in dependencies), package + " does not depend on bundled libffi"
        for field in ("extra-libraries", "extra-libraries-static"):
            libraries = run([profile_pkg, "field", package, field, "--simple-output"],
                            env=env).stdout.split()
            assert "ffi" not in libraries, package + " still requires system -lffi"
    archives = list(profile_lib.rglob("libHSlibffi-clib-3.5.2-*.a"))
    assert archives and all(path.stat().st_size for path in archives), "bundled libffi archive missing from target installation"


def check_native_interface(host, profile, work, env):
    """A shared ISA/OS and ordinary filename must not hide either ABI mismatch."""
    for label, producer, consumer in (("native", host, profile), ("profiling", profile, host)):
        origin = work / "interface-mismatch" / label / "origin"
        target = work / "interface-mismatch" / label / "consumer"
        target.mkdir(parents=True)
        write(origin / "WrongABI.hs", "module WrongABI where\nanswer :: Int\nanswer = 42\n")
        write(target / "UseWrongABI.hs", "module UseWrongABI where\nimport WrongABI\nvalue = answer\n")
        run([producer, "-v0", "-c", "WrongABI.hs"], cwd=origin, env=env)
        result = run([consumer, "-v0", "-fforce-recomp", "-c", "UseWrongABI.hs", "-i" + str(origin)],
                     cwd=target, env=env, check=False)
        assert result.returncode != 0, "consumer accepted the " + label + " interface ABI"
        assert "WrongABI" in result.stderr, result.stderr
        assert "mismatched interface file profile tag" in result.stderr, result.stderr


def check_caf_instrumentation(profile, work, env):
    """Cost-centre collection must follow the ABI after profiling ways disappear."""
    source = work / "caf-probe"
    write(source / "CAFs.hs", """
        module Main where
        {-# NOINLINE values #-}
        values :: [Integer]
        values = [1..100000]
        main = print (sum values)
    """)
    executable = source / ("caf-probe.exe" if os.name == "nt" else "caf-probe")
    run([profile, "-v0", "-O0", "-fprof-cafs", "-rtsopts", "CAFs.hs", "-o", executable],
        cwd=source, env=env)
    assert run([executable, "+RTS", "-P"], cwd=source, env=env).stdout.strip() == "5000050000"
    assert "CAF:values" in (source / "caf-probe.prof").read_text(), "profiling CAF cost centre missing"


def check_host_plugin(host, profile, host_pkg, host_lib, cabal, triple, work, env):
    """Run an ordinary plugin and profiling TH in the same target compilation."""
    write(work / "cabal.project.plugin", """
        packages: plugin
        active-repositories: :none
    """)
    write(work / "plugin/abi-host-plugin.cabal", """
        cabal-version: 3.0
        name: abi-host-plugin
        version: 0
        build-type: Simple
        library
          exposed-modules: AbiHostPlugin
          build-depends: base, ghc
          default-language: Haskell2010
    """)
    write(work / "plugin/AbiHostPlugin.hs", r'''
        {-# LANGUAGE CPP, ForeignFunctionInterface #-}
        #ifdef PROFILING
        #error host plugin must use the ordinary ABI
        #endif
        module AbiHostPlugin (plugin) where
        import GHC.Plugins
        import Foreign.C.Types
        foreign import ccall unsafe "rts_isProfiled" isProfiled :: IO CInt
        plugin = defaultPlugin
          { installCoreToDos = \_ todos -> do
              profiled <- liftIO isProfiled
              if profiled /= 0 then liftIO (fail "plugin ran in profiling interpreter") else pure ()
              putMsgS "host plugin used native ABI"
              pure todos
          , pluginRecompile = purePlugin
          }
    ''')
    plugin_build = work / "plugin-build"
    run([cabal, "build", "abi-host-plugin", "--offline", "--project-file=cabal.project.plugin",
         "--builddir=" + str(plugin_build), "--with-compiler=" + str(host),
         "--with-build-compiler=" + str(host), "--with-hc-pkg=" + str(host_pkg)],
        cwd=work, env=env)
    plugin_db = plugin_build / "store/host" / triple / "package.conf.d"
    assert plugin_db.is_dir(), "host plugin package database was not installed"
    source = work / "plugin-target"
    write(source / "Probe.hs", r'''
        {-# LANGUAGE ForeignFunctionInterface #-}
        module Probe where
        import Foreign.C.Types
        foreign import ccall unsafe "rts_isProfiled" isProfiled :: IO CInt
    ''')
    write(source / "Main.hs", r'''
        {-# LANGUAGE TemplateHaskell #-}
        import Language.Haskell.TH
        import Probe
        value :: Int
        value = $(do
          profiled <- runIO isProfiled
          if profiled /= 1 then fail "plugin compilation ran TH with native ABI" else [| 42 |])
        main = print value
    ''')
    executable = source / ("plugin-target.exe" if os.name == "nt" else "plugin-target")
    implicit = run([profile, "--make", "Main.hs", "-v0", "-fforce-recomp",
                    "-fplugin", "AbiHostPlugin", "-o", executable],
                   cwd=source, env=env, check=False)
    assert implicit.returncode != 0, "profiling target implicitly reused the host plugin namespace"
    assert "require -plugin-libdir" in implicit.stderr, implicit.stderr
    result = run([profile, "--make", "Main.hs", "-v0", "-fforce-recomp",
                  "-plugin-libdir", host_lib, "-plugin-package-db", plugin_db,
                  "-plugin-package", "abi-host-plugin", "-fplugin", "AbiHostPlugin",
                  "-o", executable], cwd=source, env=env)
    assert "host plugin used native ABI" in result.stderr, result.stderr
    assert run([executable], cwd=source, env=env).stdout.strip() == "42"


def install_package_environment(label, compiler, host, pkg, cabal, project, work, env,
                                store_dir=None, build_dir=None):
    """Install into the persistent store and use the emitted local environment."""
    write(work / "UseEnvironment.hs", "import AbiLibrary\nmain = print value\n")
    environments = set(work.glob(".ghc.environment.*"))
    options = ["--store-dir=" + str(store_dir)] if store_dir else []
    build_options = ["--builddir=" + str(build_dir)] if build_dir else []
    run([cabal, *options, "install", "--lib", "abi-workflow", "--package-env=.", "--offline",
         "--project-file=" + project, *build_options,
         "--with-compiler=" + str(compiler), "--with-build-compiler=" + str(host),
         "--with-hc-pkg=" + str(pkg)], cwd=work, env=env)
    installed = set(work.glob(".ghc.environment.*"))
    created = installed - environments
    assert len(created) == 1, "Cabal did not write a separate " + label + " package environment"
    environment = created.pop()
    assert environment.name.endswith("-profiling") == (label == "profile"), environment.name
    info = dict(ast.literal_eval(run([compiler, "--info"], env=env).stdout))
    identity = info["Target platform"]
    if info.get("Target ABI"):
        identity += "-" + info["Target ABI"]
    store = Path(store_dir) if store_dir else Path(env["CABAL_DIR"]) / "store"
    if not store.is_absolute():
        store = work / store
    expected_db = store / "host" / identity / "package.conf.d"
    database_paths = [Path(line.removeprefix("package-db "))
                      for line in environment.read_text().splitlines()
                      if line.startswith("package-db ")]
    resolved_dbs = {(path if path.is_absolute() else environment.parent / path).resolve()
                    for path in database_paths}
    assert expected_db.resolve() in resolved_dbs, "environment omitted its persistent package database: " + str(expected_db)
    assert list(expected_db.glob("abi-workflow-*.conf")), "installed library is missing from the environment database"
    if label == "profile":
        check_profile_file_names(expected_db.parent)
    output = work / (label + "-environment")
    output.mkdir()
    executable = output / ("app.exe" if os.name == "nt" else "app")
    run([compiler, "UseEnvironment.hs", "-v0", "-fforce-recomp", "-outputdir", output,
         "-o", executable], cwd=work, env=env)
    assert run([executable], cwd=work, env=env).stdout.strip() == "42"
    return environment


def check_package_environments(host, profile, host_pkg, profile_pkg, cabal, work, env):
    """Cabal writes distinct local environments which GHC selects by ABI."""
    assert not list(work.glob(".ghc.environment.*")), "fixture already has local package environments"
    environments = []
    for label, compiler, pkg, project in (
        ("native", host, host_pkg, "cabal.project"),
        ("profile", profile, profile_pkg, "cabal.project.profile"),
    ):
        environments.append(install_package_environment(label, compiler, host, pkg, cabal, project,
                                                        work, env, build_dir=work / "build"))
    assert len(set(environments)) == 2, "native and profiling package environments overlap"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("host-ghc", "profile-ghc", "host-ghc-pkg", "profile-ghc-pkg"):
        parser.add_argument("--" + name, type=Path)
    parser.add_argument("--cabal", type=Path, required=True)
    parser.add_argument("--bindist", type=Path,
                        help="install this profiling bindist in a temporary prefix and test it")
    args = parser.parse_args()
    if args.bindist:
        with tempfile.TemporaryDirectory(prefix="ghc-profile-install-") as scratch:
            work = Path(scratch)
            extracted, installed = work / "extracted", work / "installed"
            extracted.mkdir()
            run(["tar", "xzf", args.bindist.absolute(), "-C", extracted])
            run(["sh", "configure", "--prefix=" + str(installed)], cwd=extracted)
            run(["make", "install"], cwd=extracted)
            targets = list((installed / "lib/targets").glob("*-profiling"))
            assert len(targets) == 1, "expected exactly one profiling target"
            target = targets[0].name
            suffix = ".exe" if os.name == "nt" else ""
            result = run([sys.executable, Path(__file__).absolute(),
                          "--host-ghc", installed / ("bin/ghc" + suffix),
                          "--host-ghc-pkg", installed / ("bin/ghc-pkg" + suffix),
                          "--profile-ghc", installed / ("bin/" + target + "-ghc" + suffix),
                          "--profile-ghc-pkg", installed / ("bin/" + target + "-ghc-pkg" + suffix),
                          "--cabal", args.cabal.absolute()])
            print(result.stdout, end="")
        return
    if not all((args.host_ghc, args.profile_ghc, args.host_ghc_pkg, args.profile_ghc_pkg)):
        parser.error("provide --bindist or all four --*-ghc/--*-ghc-pkg paths")
    # absolute() preserves dispatching symlink names; resolve() would lose them.
    host, profile = args.host_ghc.absolute(), args.profile_ghc.absolute()
    host_pkg, profile_pkg = args.host_ghc_pkg.absolute(), args.profile_ghc_pkg.absolute()
    cabal = args.cabal.absolute()
    host_info = dict(ast.literal_eval(run([host, "--info"]).stdout))
    profile_info = dict(ast.literal_eval(run([profile, "--info"]).stdout))
    triple = host_info["Target platform"]
    assert host_info.get("Target ABI", "") == "", "host compiler is already a target ABI"
    assert profile_info.get("Target ABI") == "profiling", "not a profiling target compiler"
    assert host_info["Target platform"] == profile_info["Target platform"], "ISA/OS changed"
    assert host_info["Host platform"] == host_info["Target platform"], "host compiler is not native"
    host_lib = Path(run([host, "--print-libdir"]).stdout.strip())
    profile_lib = Path(run([profile, "--print-libdir"]).stdout.strip())
    assert host_lib != profile_lib, "target package universes overlap"
    check_profile_file_names(profile_lib)

    with tempfile.TemporaryDirectory(prefix="ghc-profile-target-") as scratch:
        work = Path(scratch)
        env = os.environ.copy()
        env.update(HOME=str(work / "home"), XDG_DATA_HOME=str(work / "data"),
                   CABAL_DIR=str(work / "cabal"))
        for variable in ("GHC_PACKAGE_PATH", "GHC_ENVIRONMENT", "GHC_CHARENC"):
            env.pop(variable, None)
        (work / "home").mkdir()
        check_bundled_ffi(profile_pkg, profile_lib, env)
        for pkg in (host_pkg, profile_pkg):
            run([pkg, "recache", "--user"], env=env)
        native_db = run([host_pkg, "list", "--user"], env=env).stdout.splitlines()[0]
        target_db = run([profile_pkg, "list", "--user"], env=env).stdout.splitlines()[0]
        assert native_db != target_db and "-profiling" in target_db, "user package databases overlap"
        check_host_dynamic_th(host, host_info, work, env)

        # A source module must have ordinary names and the profiling RTS ABI.
        write(work / "Direct.lhs", r'''
            > {-# LANGUAGE ForeignFunctionInterface #-}
            > import Foreign.C.Types
            > foreign import ccall unsafe "rts_isProfiled" isProfiled :: IO CInt
            > main = isProfiled >>= print
        ''')
        legacy = run([host, "-prof", "-fno-code", "Direct.lhs"], cwd=work, env=env, check=False)
        assert legacy.returncode != 0, "ordinary compiler accepted legacy -prof"
        assert "requires a separate profiling compiler" in legacy.stderr, legacy.stderr
        direct = work / ("direct.exe" if os.name == "nt" else "direct")
        # Redundant -prof on an intrinsic profiling compiler changes no names.
        run([profile, "-prof", "Direct.lhs", "-rtsopts", "-fprof-auto", "-o", direct], cwd=work, env=env)
        assert run([direct, "+RTS", "-p"], cwd=work, env=env).stdout.strip() == "1"
        assert (work / "Direct.hi").is_file() and (work / "Direct.o").is_file()
        assert (work / "direct.prof").is_file(), "RTS profiling report missing"
        threaded = work / ("threaded.exe" if os.name == "nt" else "threaded")
        run([profile, "Direct.lhs", "-threaded", "-rtsopts", "-o", threaded], cwd=work, env=env)
        assert run([threaded, "+RTS", "-N2", "-p"], cwd=work, env=env).stdout.strip() == "1"
        assert (work / "threaded.prof").is_file(), "threaded profiling report missing"
        suffix = ".exe" if os.name == "nt" else ""
        program_name = profile.stem if suffix else profile.name
        runghc = profile.with_name(program_name.removesuffix("ghc") + "runghc" + suffix)
        assert run([runghc, "Direct.lhs"], cwd=work, env=env).stdout.strip() == "1"
        check_native_interface(host, profile, work, env)
        check_caf_instrumentation(profile, work, env)

        fixture(work)
        for label, compiler, pkg, project in (
            ("native", host, host_pkg, "cabal.project"),
            ("profile", profile, profile_pkg, "cabal.project.profile"),
        ):
            options = ["--offline", "--project-file=" + project,
                       "--builddir=" + str(work / "build"),
                       "--with-compiler=" + str(compiler),
                       "--with-build-compiler=" + str(host),
                       "--with-hc-pkg=" + str(pkg)]
            command = [cabal]
            if label == "native":
                legacy = run([*command, "build", "all", *options, "--enable-profiling", "--dry-run"],
                             cwd=work, env=env, check=False)
                assert legacy.returncode != 0, "Cabal accepted legacy profiling with the ordinary compiler"
                assert "requires a separate profiling-target compiler" in legacy.stdout + legacy.stderr, legacy.stdout + legacy.stderr
            run([*command, "build", "all", *options], cwd=work, env=env)
            binary = run([*command, "list-bin", "exe:abi-workflow", *options], cwd=work, env=env).stdout.strip().splitlines()[-1]
            assert Path(binary).is_file(), "Cabal list-bin returned a missing executable"
            runtime = [binary, "+RTS", "-p"] if label == "profile" else [binary]
            assert run(runtime, cwd=work, env=env).stdout.strip() == "42"
            if label == "profile":
                component_log = work / "build/logs/host" / (triple + "-profiling") / "abi-workflow-0"
                component_log.unlink(missing_ok=True)
                instrumented = run([*command, "build", "all", *options, "-v2", "--enable-profiling",
                                    "--profiling-detail=toplevel-functions",
                                    "--library-profiling-detail=late"], cwd=work, env=env)
                command_output = instrumented.stdout + instrumented.stderr
                if component_log.exists():
                    command_output += component_log.read_text()
                compiler_commands = [line.replace("\\", "/") for line in
                                     command_output.splitlines()
                                     if "--make" in line]
                library_commands = [line for line in compiler_commands
                                    if "-this-unit-id" in line and "AbiLibrary" in line]
                executable_commands = [line for line in compiler_commands if "exe/Main.hs" in line]
                assert any("-fprof-late" in line and "-fprof-cafs" in line
                           for line in library_commands), "library profiling instrumentation missing from GHC command"
                assert any("-fprof-auto-top" in line for line in executable_commands), "executable profiling detail missing from GHC command"
                assert all("-fprof-auto-top" not in line for line in library_commands), "executable profiling detail leaked into library command"
                assert all("-fprof-late" not in line for line in executable_commands), "library profiling detail leaked into executable command"
                assert all("-fprof-cafs" not in line for line in executable_commands), "library profiling options leaked into executable command"
                assert run(runtime, cwd=work, env=env).stdout.strip() == "42"
        report = (work / "abi-workflow.prof").read_text()
        assert "COST CENTRE" in report and "Main" in report, "profiling report is incomplete"
        store_dirs = {path.name for path in (work / "build/store/host").iterdir()}
        assert {triple, triple + "-profiling"} <= store_dirs, "Cabal stores overlap"
        profile_build = work / "build/build/host" / (triple + "-profiling")
        products = list(profile_build.rglob("*"))
        assert any(path.name == "AbiLibrary.hi" for path in products)
        check_profile_file_names(profile_build)
        check_profile_file_names(work / "build/store/host" / (triple + "-profiling"))
        check_host_plugin(host, profile, host_pkg, host_lib, cabal, triple, work, env)
        check_package_environments(host, profile, host_pkg, profile_pkg, cabal, work, env)
        check_profile_file_names(profile_lib)
    print("native host tools/plugin, profiling TH/foreign objects/C/C++/hsc2hs, ABI rejection and profiling report verified")


if __name__ == "__main__":
    main()
