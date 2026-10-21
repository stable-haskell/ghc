#!/usr/bin/env python3
"""Test generated target entry points without building a compiler."""

import ast
import importlib.util
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tarfile
import tempfile
import unittest


spec = importlib.util.spec_from_file_location("target_aliases", Path(__file__).with_name("target-aliases.py"))
aliases = importlib.util.module_from_spec(spec)
spec.loader.exec_module(aliases)


class TargetAliases(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="target aliases ")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.bin = self.root / "bin"
        self.bin.mkdir()

    def generate(self, machine="aarch64-apple-darwin", abi="profiling", extension=""):
        target = machine + ("-prof" if abi == "profiling" else "")
        directory = self.root / "lib/targets" / target
        canonical = self.bin / (target + "-ghc" + extension)
        canonical.write_text("selected target executable\n")
        aliases.write_aliases(directory, self.bin, machine, abi, "9.14", ["ghc"], extension=extension)
        return directory

    def test_manifest_and_versioned_aliases(self):
        directory = self.generate()
        self.assertEqual(ast.literal_eval((directory / "target.aliases").read_text()), ["aarch64-darwin-prof"])
        canonical = self.bin / "aarch64-apple-darwin-prof-ghc"
        for name in ("aarch64-darwin-prof-ghc", "aarch64-darwin-prof-ghc-9.14",
                     "aarch64-apple-darwin-prof-ghc-9.14"):
            self.assertEqual((self.bin / name).resolve(), canonical.resolve())

    def test_arm_environment_is_preserved(self):
        soft = self.generate("arm-unknown-linux-gnueabi")
        hard = self.generate("arm-unknown-linux-gnueabihf")
        self.assertEqual(ast.literal_eval((soft / "target.aliases").read_text()), ["arm-linux-gnueabi-prof"])
        self.assertEqual(ast.literal_eval((hard / "target.aliases").read_text()), ["arm-linux-gnueabihf-prof"])
        self.assertNotEqual(soft, hard)

    def test_ambiguous_alias_is_rejected(self):
        self.generate()
        with self.assertRaisesRegex(ValueError, "ambiguous"):
            self.generate("aarch64-other-darwin")

    def test_alias_cannot_shadow_a_canonical_target(self):
        (self.root / "lib/targets/aarch64-darwin-prof").mkdir(parents=True)
        with self.assertRaisesRegex(ValueError, "ambiguous"):
            self.generate()

    def test_case_collision_is_rejected(self):
        (self.root / "lib/targets/AARCH64-darwin-prof").mkdir(parents=True)
        with self.assertRaisesRegex(ValueError, "ambiguous"):
            self.generate()

    def test_same_owner_case_alias_is_rejected(self):
        directory = self.generate()
        with self.assertRaisesRegex(ValueError, "ambiguous"):
            aliases.write_aliases(directory, self.bin, "aarch64-apple-darwin", "profiling",
                                  "9.14", ["ghc"], ["AARCH64-APPLE-DARWIN-PROF"])

    def test_bootstrap_and_stage2_store_identities(self):
        compiler = self.root / "bootstrap.py"
        cases = (("x86_64-unknown-linux", None),
                 ("x86_64-unknown-linux-gnu", "x86_64-unknown-linux-gnu"))
        source_root = Path(__file__).resolve().parent.parent
        for machine, target in cases:
            with self.subTest(machine=machine, target=target):
                compiler.write_text(
                    "import sys\n"
                    "option = sys.argv[1]\n"
                    "if option == '--print-target-name':\n"
                    + ("    raise SystemExit(1)\n" if target is None else "    print(" + repr(target) + ")\n")
                    + "elif option == '--print-target-platform':\n    print(" + repr(machine) + ")\n"
                    + "elif option == '--print-host-platform':\n    print('x86_64-unknown-linux')\n"
                    + "else:\n    raise SystemExit(2)\n")
                rule = ('print-target-identities:; @echo "$(BOOTSTRAP_TARGET)|$(STAGE2_PLATFORM)|'
                        '$(STAGE1_PATH)|$(STAGE2_PATH)|$(STAGE3_TOOLS_PATH)"')
                result = subprocess.run(
                    ["make", "--no-print-directory", "--eval=" + rule,
                     "GHC0=" + sys.executable + " '" + str(compiler) + "'", "print-target-identities"],
                    cwd=source_root, check=True, text=True, capture_output=True, timeout=30)
                bootstrap, stage2, stage1_path, stage2_path, tools = result.stdout.strip().split("|")
                self.assertEqual(bootstrap, target or machine)
                self.assertEqual(stage2, "x86_64-unknown-linux-gnu")
                self.assertEqual(stage1_path, "_build/stage1/store/host/" + bootstrap)
                self.assertEqual(stage2_path, "_build/stage2/store/host/" + stage2)
                self.assertEqual(tools, "_build/stage3-tools/store/host/" + bootstrap)

    def test_existing_executable_is_not_replaced(self):
        path = self.bin / "aarch64-darwin-prof-ghc"
        path.write_text("user executable\n")
        with self.assertRaisesRegex(ValueError, "refusing to replace"):
            self.generate()
        self.assertEqual(path.read_text(), "user executable\n")

    def test_unsafe_names_are_rejected(self):
        for name in ("", "../foo", "a--b", "a b", "a/b", "a\\b", "é-apple-darwin"):
            with self.subTest(name=name), self.assertRaises(ValueError):
                aliases.target_aliases(name, "profiling")

    def test_windows_version_precedes_executable_extension(self):
        directory = self.generate(extension=".exe")
        self.assertIn("bin/aarch64-darwin-prof-ghc-9.14.exe",
                      (directory / "target.executables").read_text().splitlines())

    def test_archive_carries_every_manifest_entry_point(self):
        directory = self.generate()
        archive = self.root / "bindist.tar.gz"
        subprocess.run(["tar", "czhf", archive, "--directory=" + str(self.root),
                        "--files-from=" + str(directory / "target.executables"),
                        "lib/targets/" + directory.name], check=True,
                       capture_output=True, timeout=30)
        extracted = self.root / "unpacked bindist"
        extracted.mkdir()
        subprocess.run(["tar", "xzf", archive, "-C", extracted], check=True,
                       capture_output=True, timeout=30)
        for name in (directory / "target.executables").read_text().splitlines():
            executable = extracted / name
            self.assertFalse(executable.is_symlink())
            self.assertEqual(executable.read_text(), "selected target executable\n")

    def test_haskell_toolchain_recipe_reads_target_manifest(self):
        source_root = Path(__file__).resolve().parent.parent
        fixture = tempfile.TemporaryDirectory(prefix="toolchain-archive-")
        self.addCleanup(fixture.cleanup)
        root = Path(fixture.name)
        dist = root / "dist"
        target = dist / "lib/targets/javascript-unknown-ghcjs"
        for path in (dist / "bin", dist / "lib/package.conf.d",
                     dist / "lib/x86_64-unknown-linux", target / "lib"):
            path.mkdir(parents=True, exist_ok=True)
        for path, contents in (
            (dist / "bin/ghc", "native ghc\n"),
            (dist / "bin/javascript-unknown-ghcjs-ghc", "target ghc\n"),
            (dist / "lib/ghc-usage.txt", "usage\n"),
            (dist / "lib/ghci-usage.txt", "usage\n"),
            (dist / "lib/settings", "[]\n"),
            (dist / "lib/llvm-targets", "targets\n"),
            (dist / "lib/llvm-passes", "passes\n"),
            (dist / "lib/template-hsc.h", "/* template */\n"),
            (target / "lib/settings", "[]\n"),
        ):
            path.write_text(contents)
        (target / "target.executables").write_text("bin/javascript-unknown-ghcjs-ghc\n")
        cabal = root / "cabal"
        cabal.write_text("cabal\n")
        relative_dist = os.path.relpath(dist, source_root)
        stamp = os.path.relpath(root / "stage2-stamp", source_root)
        archive = dist / "haskell-toolchain.tar.gz"
        make = shlex.split(os.environ.get("MAKE", "make")) or ["make"]
        command = [*make, "--no-print-directory", "-f", str(source_root / "Makefile"), "-B",
                   "-o", stamp, "-o", "stable-cabal", "-o", "stage3-javascript-unknown-ghcjs",
                   "GHC0=:", "HOST_PLATFORM=x86_64-unknown-linux",
                   "BOOTSTRAP_TARGET=x86_64-unknown-linux",
                   "STAGE2_PLATFORM=x86_64-unknown-linux", "DIST_DIR=" + relative_dist,
                   "STAGE2_STAMP=" + stamp,
                   "LLVM_TABLE_FILES=" + relative_dist + "/lib/llvm-targets "
                   + relative_dist + "/lib/llvm-passes",
                   "CABAL=" + str(cabal), "STAGE2_EXECUTABLES=ghc",
                   relative_dist + "/haskell-toolchain.tar.gz"]
        subprocess.run(command, cwd=source_root, check=True, text=True,
                       capture_output=True, timeout=60)
        with tarfile.open(archive, "r:gz") as bindist:
            names = bindist.getnames()
        self.assertIn("bin/javascript-unknown-ghcjs-ghc", names)
        self.assertIn("lib/targets/javascript-unknown-ghcjs", names)


if __name__ == "__main__":
    unittest.main()
