#!/usr/bin/env python3
"""Exercise a native compiler and its separate profiling target installation.

Requires a complete stage3-profile build and the matching patched Cabal. All
package databases, Cabal stores and build products created here are temporary.
"""

import argparse
import ast
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import textwrap

from toolchain_trace import (
    check_no_logical_profiling_target_in_toolchain,
    check_profile_cpp_trace,
    check_hsc2hs_wrapper_path_independence,
    check_profile_hsc2hs_invocations,
    command_program_name,
    command_target_selectors,
    compile_command_tokens,
    link_command_tokens,
    make_hsc2hs_recording_wrapper,
)


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


def read_info(program, env=None):
    pairs = ast.literal_eval(run([program, "--info"], env=env).stdout)
    keys = [key for key, _ in pairs]
    for field in ("Project version", "Target platform", "Target name", "Target ABI",
                  "LibDir", "Global Package DB"):
        assert keys.count(field) == 1, "missing or duplicate identity field: " + field
    return dict(pairs)


def check_target_entry_points(host, profile, host_pkg, profile_pkg, host_info, profile_info, env):
    fields = ("Project version", "Target platform", "Target name", "Target ABI",
              "LibDir", "Global Package DB")
    for compiler, pkg, expected in ((host, host_pkg, host_info), (profile, profile_pkg, profile_info)):
        package_info = read_info(pkg, env)
        assert all(package_info[field] == expected[field] for field in fields), "package tool identity mismatch"
        assert run([compiler, "--print-target-name"], env=env).stdout.strip() == expected["Target name"]
    # Package database overrides cannot spoof the package tool's own target.
    overridden = ast.literal_eval(run([host_pkg, "--info", "--global-package-db",
                                      profile_info["Global Package DB"]], env=env).stdout)
    assert dict(overridden)["Target name"] == host_info["Target name"]
    libdir = Path(profile_info["LibDir"])
    aliases = ast.literal_eval((libdir.parent / "target.aliases").read_text())
    assert aliases, "profiling target has no short executable alias"
    suffix = ".exe" if os.name == "nt" else ""
    for target in (profile_info["Target name"], *aliases):
        for version in ("", "-" + profile_info["Project version"]):
            for tool in ("ghc", "ghc-pkg"):
                executable = profile.parent / (target + "-" + tool + version + suffix)
                actual = read_info(executable, env)
                assert all(actual[field] == profile_info[field] for field in fields), str(executable)


def check_package_tool_mismatch(host, profile, host_pkg, cabal, work, env):
    def databases():
        files = set(work.rglob("package.cache")) | set(work.rglob("*.conf"))
        return {path: (path.read_bytes(), path.stat().st_mtime_ns) for path in files}

    before = databases()
    failed = run([cabal, "build", "all", "--dry-run", "--offline",
                  "--project-file=cabal.project.profile", "--builddir=" + str(work / "wrong-tool"),
                  "--with-compiler=" + str(profile), "--with-build-compiler=" + str(host),
                  "--with-hc-pkg=" + str(host_pkg)], cwd=work, env=env, check=False)
    assert failed.returncode != 0, "same-version native ghc-pkg accepted for profiling target"
    assert "ghc-pkg target metadata mismatch" in failed.stdout + failed.stderr, failed.stdout + failed.stderr
    after = databases()
    assert after == before, "mismatched package tool wrote a package database"


def check_invalid_target_settings(host, host_pkg, host_info, work, env):
    original = ast.literal_eval((Path(host_info["LibDir"]) / "settings").read_text())
    cases = (
        ("unsafe", "target name", "../unsafe", "target name"),
        ("contradictory", "target name", host_info["Target name"] + "-prof", "contradicts"),
        ("unknown-abi", "target ABI", "unknown-abi", "unsupported target abi"),
    )
    for label, key, value, diagnostic in cases:
        directory = work / "bad-settings" / label
        directory.mkdir(parents=True)
        pairs = [(k, value if k == key else v) for k, v in original]
        content = "[" + ",".join("(" + json.dumps(k) + "," + json.dumps(v) + ")"
                                  for k, v in pairs) + "]\n"
        (directory / "settings").write_text(content)
        failed = run([host, "-B" + str(directory), "--info"], cwd=work, env=env, check=False)
        assert failed.returncode != 0 and diagnostic in failed.stderr.lower(), failed.stderr
    duplicate = work / "bad-settings/duplicate"
    duplicate.mkdir()
    pairs = original + [("target name", host_info["Target name"])]
    (duplicate / "settings").write_text("[" + ",".join(
        "(" + json.dumps(k) + "," + json.dumps(v) + ")" for k, v in pairs) + "]\n")
    failed = run([host, "-B" + str(duplicate), "--info"], cwd=work, env=env, check=False)
    assert failed.returncode != 0 and "duplicate target identity" in failed.stderr.lower(), failed.stderr

    # Run the real package-tool binary against an isolated installation. Keep
    # its native shared libraries reachable without sharing any writable DB.
    installation = work / "bad-package-tool"
    libdir = installation / "lib"
    libdir.mkdir(parents=True)
    executable = installation / "bin" / host_pkg.name
    executable.parent.mkdir()
    shutil.copy2(host_pkg.resolve(), executable)
    for child in Path(host_info["LibDir"]).iterdir():
        if child.is_dir() and child.name not in ("package.conf.d", "targets"):
            (libdir / child.name).symlink_to(child, target_is_directory=True)
    database = libdir / "package.conf.d"
    database.mkdir()
    sentinel = database / "package.cache"
    sentinel.write_bytes(b"invalid identity must be rejected before recaching\n")
    for source, diagnostic in ((work / "bad-settings/contradictory", "contradicts"),
                               (duplicate, "duplicate target identity")):
        shutil.copy2(source / "settings", libdir / "settings")
        before = sentinel.read_bytes(), sentinel.stat().st_mtime_ns
        failed = run([executable, "--no-user-package-db", "--global", "recache"],
                     cwd=work, env=env, check=False)
        assert failed.returncode != 0 and diagnostic in failed.stderr.lower(), failed.stderr
        assert (sentinel.read_bytes(), sentinel.stat().st_mtime_ns) == before, "invalid settings mutated global DB"


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
          ghc-options: -F -pgmF abi-generator -v
          ghc-prof-options: -fprof-cafs
          default-language: Haskell2010
          if flag(profile-target)
            cpp-options: -DEXPECT_PROFILE
        executable abi-workflow
          hs-source-dirs: exe
          main-is: Main.hs
          build-depends: base, abi-workflow
          ghc-options: -rtsopts -v
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


def haskell_settings(rows):
    """Serialize settings as a Haskell Read list, not a Python repr."""
    pairs = ["(" + json.dumps(key, ensure_ascii=False) + ", " +
             json.dumps(value, ensure_ascii=False) + ")" for key, value in rows]
    return "[" + ", ".join(pairs) + "]\n"


def shadow_target_install(profile_lib, host_pkg, host_lib, work):
    """Build a private target libdir and ghc-pkg base dir for settings probes."""
    profile_lib = profile_lib.resolve()
    target_dir = profile_lib.parent
    name = target_dir.name
    root = work / "settings-validation-install"
    shadow_base = root / "lib"
    shadow_base.mkdir(parents=True)
    for item in Path(host_lib).resolve().iterdir():
        if item.name not in ("settings", "package.conf.d", "targets"):
            (shadow_base / item.name).symlink_to(item, target_is_directory=item.is_dir())
    (shadow_base / "package.conf.d").mkdir()
    shadow_targets = shadow_base / "targets"
    shadow_targets.mkdir()
    installed_targets = Path(host_lib).resolve() / "targets"
    if installed_targets.is_dir():
        for installed_target in installed_targets.iterdir():
            if installed_target.name == name:
                continue
            shadow_target = shadow_targets / installed_target.name
            shadow_target.mkdir()
            for item in installed_target.iterdir():
                if item.name != "lib":
                    (shadow_target / item.name).symlink_to(item, target_is_directory=item.is_dir())
                    continue
                shadow_target_lib = shadow_target / "lib"
                shadow_target_lib.mkdir()
                for lib_item in item.iterdir():
                    if lib_item.name == "package.conf.d":
                        (shadow_target_lib / lib_item.name).mkdir()
                    else:
                        (shadow_target_lib / lib_item.name).symlink_to(
                            lib_item, target_is_directory=lib_item.is_dir())
    shadow_target = shadow_targets / name
    shadow_lib = shadow_target / "lib"
    shadow_lib.mkdir(parents=True)
    for item in profile_lib.iterdir():
        if item.name not in ("settings", "package.conf.d"):
            (shadow_lib / item.name).symlink_to(item, target_is_directory=item.is_dir())
    (shadow_lib / "package.conf.d").mkdir()
    shutil.copy2(target_dir / "target.aliases", shadow_target / "target.aliases")
    pkg_name = "ghc-pkg.exe" if os.name == "nt" else "ghc-pkg"
    pkg_candidates = [Path(host_lib).parent / "bin" / pkg_name,
                      Path(host_lib) / "bin" / pkg_name,
                      Path(os.path.realpath(host_pkg))]
    def is_executable_binary(candidate):
        if not candidate.is_file():
            return False
        with candidate.open("rb") as executable:
            return executable.read(2) != b"#!"

    pkg_binary = next((candidate for candidate in pkg_candidates
                       if is_executable_binary(candidate)), None)
    assert pkg_binary is not None, "could not locate the installed ghc-pkg executable"
    shadow_pkg = root / "bin" / pkg_binary.name
    shadow_pkg.parent.mkdir(parents=True)
    shutil.copy2(pkg_binary, shadow_pkg)
    return shadow_lib, shadow_pkg, name


def check_settings_info_invariants(profile, profile_lib, host_pkg, host_lib, work, env):
    """New identity settings own computed info fields; old settings retain overrides."""
    original_settings = profile_lib / "settings"
    rows = ast.literal_eval(original_settings.read_text())
    shadow_lib, shadow_pkg, target = shadow_target_install(profile_lib, host_pkg, host_lib, work)
    shadow_topdir = shadow_lib.parent.parent.parent
    settings_file = shadow_lib / "settings"
    keys = ("Project version", "LibDir", "Global Package DB")
    for key in keys:
        # Duplicate and single injected values must both fail before ghc-pkg
        # can initialize a package database, even with user DBs disabled.
        for injected in ([(key, "injected")],
                         [(key, "first"), (key, "second")]):
            settings_file.write_text(haskell_settings(rows + injected))
            compiler_result = run([profile, "-B" + str(shadow_topdir), "--info"],
                                  env=env, check=False)
            pkg_result = run([shadow_pkg, "--target", target, "--info"],
                             env=env, check=False)
            for label, result in (("ghc", compiler_result), ("ghc-pkg", pkg_result)):
                assert result.returncode != 0, label + " accepted reserved setting " + key
                assert key in result.stderr, result.stderr
            db = work / "must-not-be-created" / key.replace(" ", "-")
            result = run([shadow_pkg, "--target", target, "--no-user-package-db",
                          "--global-package-db", db, "init", db], env=env, check=False)
            assert result.returncode != 0, "ghc-pkg accepted malformed settings with --no-user-package-db"
            assert key in result.stderr, result.stderr
            assert not db.exists(), "ghc-pkg created a database after settings validation failed"
            recache_db = work / "must-not-be-recached" / key.replace(" ", "-")
            recache_db.mkdir(parents=True, exist_ok=True)
            result = run([shadow_pkg, "--target", target, "--no-user-package-db",
                          "--global-package-db", recache_db, "recache"], env=env, check=False)
            assert result.returncode != 0, "ghc-pkg accepted malformed settings while recaching"
            assert key in result.stderr, result.stderr
            assert not any(recache_db.iterdir()), "ghc-pkg mutated a package database after settings validation failed"

    # The legacy settings format had no target name and allowed these raw
    # values to override the computed --info fields.
    legacy_rows = [(key, value) for key, value in rows if key != "target name"]
    overrides = {key: "legacy override for " + key for key in keys}
    legacy_rows.extend(overrides.items())
    settings_file.write_text(haskell_settings(legacy_rows))
    info = ast.literal_eval(run([profile, "-B" + str(shadow_topdir), "--info"], env=env).stdout)
    first_values = {}
    for key, value in info:
        first_values.setdefault(key, value)
    for key, value in overrides.items():
        assert first_values.get(key) == value, "legacy --info override changed for " + key

    # A settings file can be internally coherent while belonging to a different
    # target than the canonical directory selected by an alias or executable.
    machine = dict(rows)["target platform string"]
    wrong_rows = [(key, value) for key, value in rows
                  if key not in ("target ABI", "target name")]
    wrong_rows.extend((("target ABI", ""), ("target name", machine)))
    settings_file.write_text(haskell_settings(wrong_rows))
    aliases = ast.literal_eval((profile_lib.parent / "target.aliases").read_text())
    for index, selection in enumerate((target, aliases[0])):
        compiler_result = run([profile, "-B" + str(shadow_topdir),
                               "--target=" + selection, "--info"],
                              env=env, check=False)
        assert compiler_result.returncode != 0, \
            "GHC accepted coherent settings under a different canonical target directory"
        assert "contradicts settings target name" in compiler_result.stderr, compiler_result.stderr
        for selector_index, selector_args in enumerate((("--target=" + selection,),
                                                          ("--target", selection))):
            pkg_result = run([shadow_pkg, *selector_args, "--info"], env=env, check=False)
            assert pkg_result.returncode != 0, \
                "ghc-pkg --info accepted settings for a different canonical target"
            assert "contradicts settings target name" in pkg_result.stderr, pkg_result.stderr
            db = work / ("wrong-target-init-" + str(index) + "-" + str(selector_index))
            result = run([shadow_pkg, *selector_args, "--no-user-package-db",
                          "--global-package-db", db, "init", db], env=env, check=False)
            assert result.returncode != 0, "ghc-pkg init accepted a different canonical target"
            assert "contradicts settings target name" in result.stderr, result.stderr
            assert not db.exists(), "ghc-pkg init wrote a database for the wrong target identity"
            recache_db = work / ("wrong-target-recache-" + str(index) + "-" + str(selector_index))
            recache_db.mkdir(parents=True, exist_ok=True)
            result = run([shadow_pkg, *selector_args, "--no-user-package-db",
                          "--global-package-db", recache_db, "recache"], env=env, check=False)
            assert result.returncode != 0, "ghc-pkg recache accepted a different canonical target"
            assert "contradicts settings target name" in result.stderr, result.stderr
            assert not any(recache_db.iterdir()), "ghc-pkg recache wrote a database for the wrong target identity"


def check_ghc_pkg_target_precedence(host_pkg, profile_info, profile_lib, env):
    """ghc-pkg --info and read-only list use the same last target option."""
    aliases_file = profile_lib.parent / "target.aliases"
    aliases = ast.literal_eval(aliases_file.read_text())
    assert isinstance(aliases, list) and all(isinstance(alias, str) for alias in aliases), \
        "invalid target.aliases manifest"
    assert aliases, "profiling target has no explicit dispatch alias"
    alias = aliases[0]
    canonical = profile_info["Target name"]
    assert alias != canonical, "target.aliases unexpectedly repeats its canonical name"
    option_orders = (("--target=missing-target", "--target", alias),
                     ("--target", "missing-target", "--target=" + alias))
    for options in option_orders:
        info = dict(ast.literal_eval(run([host_pkg, *options, "--info"], env=env).stdout))
        assert info["Target name"] == canonical, "ghc-pkg --info did not select the last target alias"
        listing = run([host_pkg, *options, "list"], env=env).stdout
        assert profile_info["Global Package DB"] in listing, \
            "ghc-pkg list did not use the target selected by the last --target option"


def check_profile_codegen_commands(profile, profile_info, work, env):
    """Inspect generated LLVM IR identity; real commands come from GHC's -v log."""
    source = work / "qualified-target-commands"
    source.mkdir(parents=True, exist_ok=True)
    llvm_tools = []
    for key in ("LLVM llc command", "LLVM opt command"):
        command = profile_info.get(key, "")
        executable_name = shlex.split(command)[0] if command else ""
        if executable_name and (Path(executable_name).is_file() or shutil.which(executable_name)):
            llvm_tools.append(key)
    if len(llvm_tools) != 2:
        print("SKIP LLVM IR triple assertion: configured llc/opt tools are unavailable", flush=True)
        return
    ir_source = source / "LLVMProbe.hs"
    write(ir_source, "module LLVMProbe where\nvalue :: Int\nvalue = 42\n")
    run([profile, "-S", "-fllvm", "-keep-llvm-files", "LLVMProbe.hs", "-v"],
        cwd=source, env=env)
    ir_files = list(source.glob("*.ll"))
    assert ir_files, "LLVM mode did not retain an LLVM IR file"
    llvm_target = profile_info.get("LLVM target", "")
    assert llvm_target, "profile settings omitted LLVM target"
    ir = "\n".join(path.read_text() for path in ir_files)
    assert 'target triple = "' + llvm_target + '"' in ir, ir


def cabal_plan_ids(path):
    plan = json.loads(path.read_text())
    entries = plan["install-plan"]
    ids = sorted(entry.get("id", entry.get("unit-id")) for entry in entries)
    assert all(ids), "Cabal install plan omitted component IDs"
    return ids


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
    info = read_info(compiler, env)
    identity = info["Target name"]
    assert environment.name == ".ghc.environment." + identity + "-" + info["Project version"], environment.name
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


def check_cabal_alias_plans(cabal, host, profile, profile_pkg, profile_info,
                            profile_lib, work, env, canonical_plan):
    """Infer matching tools for each alias and preserve canonical unit IDs."""
    expected_ids = cabal_plan_ids(canonical_plan)
    aliases = ast.literal_eval((profile_lib.parent / "target.aliases").read_text())
    assert aliases, "profiling target has no dispatch aliases"
    alias = aliases[0]
    version = run([profile, "--numeric-version"], env=env).stdout.strip()
    entries = []
    for label, target, suffix in (("canonical-versioned", profile_info["Target name"], "-" + version),
                                  ("short", alias, ""),
                                  ("short-versioned", alias, "-" + version)):
        compiler = profile.with_name(target + "-ghc" + suffix + (".exe" if os.name == "nt" else ""))
        pkg = profile_pkg.with_name(target + "-ghc-pkg" + suffix + (".exe" if os.name == "nt" else ""))
        assert compiler.is_file() and pkg.is_file(), "missing " + label + " target dispatch aliases"
        entries.append((label, compiler))
    if os.name != "nt":
        # An invocation symlink may live outside the installation. Its native
        # executable still has the canonical target companions beside it.
        external = work / "external compiler entry"
        external.mkdir()
        compiler = external / (alias + "-ghc")
        compiler.symlink_to(profile)
        entries.append(("external-short", compiler))
    for label, compiler in entries:
        info = dict(ast.literal_eval(run([compiler, "--info"], env=env).stdout))
        assert info.get("Target name") == profile_info["Target name"], \
            label + " GHC alias selected a noncanonical target"
        assert info.get("Target ABI") == "profiling", label + " GHC alias lost the profiling ABI"
        build_dir = work / ("build-alias-" + label)
        options = ["--offline", "--project-file=cabal.project.profile",
                   "--builddir=" + str(build_dir), "--with-compiler=" + str(compiler),
                   "--with-build-compiler=" + str(host)]
        run([cabal, "build", "all", *options, "--enable-profiling",
             "--profiling-detail=toplevel-functions", "--library-profiling-detail=late",
             "--dry-run"], cwd=work, env=env)
        plan = build_dir / "cache/plan.json"
        assert plan.is_file(), label + " Cabal dry-run did not write its install plan"
        assert cabal_plan_ids(plan) == expected_ids, \
            label + " target alias changed Cabal unit IDs"


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
            extracted, installed = work / "extracted", work / "installed prefix with spaces"
            extracted.mkdir()
            run(["tar", "xzf", args.bindist.absolute(), "-C", extracted])
            run(["sh", "configure", "--prefix=" + str(installed)], cwd=extracted)
            run(["make", "install"], cwd=extracted)
            relocated = work / "relocated prefix with spaces"
            shutil.move(installed, relocated)
            installed = relocated
            run(["sh", "relocate.sh"], cwd=installed)
            targets = list((installed / "lib/targets").glob("*-prof"))
            assert len(targets) == 1, "expected exactly one profiling target"
            target = targets[0].name
            suffix = ".exe" if os.name == "nt" else ""
            child_command = [sys.executable, Path(__file__).absolute(),
                             "--host-ghc", installed / ("bin/ghc" + suffix),
                             "--host-ghc-pkg", installed / ("bin/ghc-pkg" + suffix),
                             "--profile-ghc", installed / ("bin/" + target + "-ghc" + suffix),
                             "--profile-ghc-pkg", installed / ("bin/" + target + "-ghc-pkg" + suffix),
                             "--cabal", args.cabal.absolute()]
            print("+ " + " ".join(map(str, child_command)), flush=True)
            subprocess.run(child_command, check=True, timeout=1800)
        return
    if not all((args.host_ghc, args.profile_ghc, args.host_ghc_pkg, args.profile_ghc_pkg)):
        parser.error("provide --bindist or all four --*-ghc/--*-ghc-pkg paths")
    # absolute() preserves dispatching symlink names; resolve() would lose them.
    host, profile = args.host_ghc.absolute(), args.profile_ghc.absolute()
    host_pkg, profile_pkg = args.host_ghc_pkg.absolute(), args.profile_ghc_pkg.absolute()
    cabal = args.cabal.absolute()
    host_info = read_info(host)
    profile_info = read_info(profile)
    triple = host_info["Target platform"]
    assert host_info.get("Target ABI", "") == "", "host compiler is already a target ABI"
    assert profile_info.get("Target ABI") == "profiling", "not a profiling target compiler"
    assert host_info["Target platform"] == profile_info["Target platform"], "ISA/OS changed"
    assert host_info["Target name"] == triple, "native logical target differs from machine target"
    assert profile_info["Target name"] == triple + "-prof", "profiling logical target lost machine ABI"
    # Native execution below tests executability; older Host platform macros
    # may omit the environment component now retained in Target platform.
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
        check_target_entry_points(host, profile, host_pkg, profile_pkg, host_info, profile_info, env)
        check_invalid_target_settings(host, host_pkg, host_info, work, env)
        check_ghc_pkg_target_precedence(host_pkg, profile_info, profile_lib, env)
        check_settings_info_invariants(profile, profile_lib, host_pkg, host_lib, work, env)
        check_bundled_ffi(profile_pkg, profile_lib, env)
        for pkg in (host_pkg, profile_pkg):
            run([pkg, "recache", "--user"], env=env)
        native_db = run([host_pkg, "list", "--user"], env=env).stdout.splitlines()[0]
        target_db = run([profile_pkg, "list", "--user"], env=env).stdout.splitlines()[0]
        assert native_db != target_db, "user package databases overlap"
        assert host_info["Target name"] + "-" + host_info["Project version"] in native_db, native_db
        assert profile_info["Target name"] + "-" + profile_info["Project version"] in target_db, target_db
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
        for flag in ("-dynamic", "-dynamic-too", "-debug"):
            unsupported = run([profile, "Direct.lhs", flag, "-o", work / "unsupported"],
                              cwd=work, env=env, check=False)
            assert unsupported.returncode != 0, "unsupported profiling variant accepted: " + flag
            assert "profil" in unsupported.stderr.lower() and "support" in unsupported.stderr.lower(), unsupported.stderr
        suffix = ".exe" if os.name == "nt" else ""
        program_name = profile.stem if suffix else profile.name
        runghc = profile.with_name(program_name.removesuffix("ghc") + "runghc" + suffix)
        assert run([runghc, "Direct.lhs"], cwd=work, env=env).stdout.strip() == "1"
        host_program_name = host.stem if suffix else host.name
        host_runghc = host.with_name(host_program_name.removesuffix("ghc") + "runghc" + suffix)
        assert run([host_runghc, "Direct.lhs"], cwd=work, env=env).stdout.strip() == "0"
        aliases = ast.literal_eval((profile_lib.parent / "target.aliases").read_text())
        compiler_version = run([profile, "--numeric-version"], env=env).stdout.strip()
        for alias_name in (aliases[0] + "-runghc",
                           aliases[0] + "-runghc-" + compiler_version):
            alias_runghc = profile.with_name(alias_name + suffix)
            assert alias_runghc.is_file(), "missing runghc dispatch alias " + str(alias_runghc)
            assert run([alias_runghc, "Direct.lhs"], cwd=work, env=env).stdout.strip() == "1"
        check_native_interface(host, profile, work, env)
        check_caf_instrumentation(profile, work, env)

        fixture(work)
        check_no_logical_profiling_target_in_toolchain(profile_info)
        check_profile_cpp_trace(profile, profile_info, work, env, run)
        check_package_tool_mismatch(host, profile, host_pkg, cabal, work, env)
        # Inference must preserve target dispatch even though both compiler
        # names resolve to the same executable. Explicit pairing is covered by
        # the package-environment workflow and the wrong-tool rejection above.
        for label, compiler, project in (
            ("native", host, "cabal.project"),
            ("profile", profile, "cabal.project.profile"),
        ):
            options = ["--offline", "--project-file=" + project,
                       "--builddir=" + str(work / "build"),
                       "--with-compiler=" + str(compiler),
                       "--with-build-compiler=" + str(host)]
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
                component_log = work / "build/logs/host" / profile_info["Target name"] / "abi-workflow-0"
                component_log.unlink(missing_ok=True)
                hsc2hs_suffix = ".exe" if os.name == "nt" else ""
                hsc2hs_helper = profile.with_name(
                    profile_info["Target name"] + "-hsc2hs" + hsc2hs_suffix)
                assert hsc2hs_helper.is_file(), \
                    "missing target-dispatched profiling hsc2hs helper: " + str(hsc2hs_helper)
                check_hsc2hs_wrapper_path_independence(hsc2hs_helper, work, env, write)
                hsc2hs_wrapper, hsc2hs_log, hsc2hs_env = make_hsc2hs_recording_wrapper(
                    hsc2hs_helper, work, env, write)
                shutil.rmtree(work / "build/build/host" / profile_info["Target name"] /
                              "abi-workflow-0", ignore_errors=True)
                instrumented = run([*command, "build", "all", *options, "-v2", "--enable-profiling",
                                    "--profiling-detail=toplevel-functions",
                                    "--library-profiling-detail=late",
                                    "--with-hsc2hs=" + str(hsc2hs_wrapper)], cwd=work, env=hsc2hs_env)
                check_profile_hsc2hs_invocations(hsc2hs_log, profile_info)
                check_cabal_alias_plans(cabal, host, profile, profile_pkg, profile_info,
                                        profile_lib, work, env,
                                        work / "build/cache/plan.json")
                command_output = instrumented.stdout + instrumented.stderr
                if component_log.exists():
                    command_output += component_log.read_text()
                compiler_commands = [line.replace("\\", "/") for line in
                                     command_output.splitlines()
                                     if "--make" in line]
                cc_command = profile_info.get("C compiler command", "")
                cxx_command = profile_info.get("C++ compiler command", "")
                cc_program = command_program_name(cc_command)
                cxx_program = command_program_name(cxx_command)
                c_commands = [line for line in command_output.splitlines()
                              if compile_command_tokens(line, cc_program, "profiling.c")]
                cxx_commands = [line for line in command_output.splitlines()
                                if compile_command_tokens(line, cxx_program, "profiling-cxx.cpp")]
                link_commands = [line for line in command_output.splitlines()
                                 if link_command_tokens(line, cc_program, "abi-workflow")]
                assert c_commands, "profile GHC -v log omitted the C compiler command"
                assert cxx_commands, "profile GHC -v log omitted the C++ compiler command"
                assert link_commands, "profile GHC -v log omitted the linker command"
                llvm_target = profile_info["LLVM target"]
                configured_tools = (
                    ("C", c_commands, ("C compiler flags",)),
                    ("C++", cxx_commands, ("C++ compiler flags",)),
                    ("link", link_commands, ("C compiler flags", "C compiler link flags")),
                )
                for tool, lines, setting_keys in configured_tools:
                    expected_machine, _ = command_target_selectors(" ".join(
                        profile_info.get(key, "") for key in setting_keys))
                    for line in lines:
                        tokens = shlex.split(line)
                        actual_machine, actual_llvm = command_target_selectors(line)
                        assert "-prof" not in tokens and all(
                            not target.endswith("-prof") for target in actual_machine | actual_llvm), \
                            "logical -prof target leaked into tool command: " + line
                        assert expected_machine == actual_machine, \
                            tool + " command target selectors differ from configured flags: " + line
                        assert all(target == llvm_target for target in actual_llvm), \
                            "LLVM command selected a target different from LLVM target: " + line
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
        assert {host_info["Target name"], profile_info["Target name"]} <= store_dirs, "Cabal stores overlap"
        profile_build = work / "build/build/host" / profile_info["Target name"]
        products = list(profile_build.rglob("*"))
        assert any(path.name == "AbiLibrary.hi" for path in products)
        check_profile_file_names(profile_build)
        check_profile_file_names(work / "build/store/host" / profile_info["Target name"])
        check_host_plugin(host, profile, host_pkg, host_lib, cabal, triple, work, env)
        check_package_environments(host, profile, host_pkg, profile_pkg, cabal, work, env)
        check_profile_codegen_commands(profile, profile_info, work, env)
        check_profile_file_names(profile_lib)
    print("native host tools/plugin, profiling TH/foreign objects/C/C++/hsc2hs, ABI rejection and profiling report verified")


if __name__ == "__main__":
    main()
