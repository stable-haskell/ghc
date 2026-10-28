#!/usr/bin/env python3
"""Check discovery of actual shared-link commands in Cabal logs."""

from pathlib import Path
from tempfile import TemporaryDirectory
import unittest

from native_shared_link import native_command


class NativeCommandTests(unittest.TestCase):
    def test_package_log_fallback(self):
        with TemporaryDirectory() as directory:
            root = Path(directory)
            main = root / "build.log"
            package = root / "package.log"
            main.write_text("completed liblink\n")
            package.write_text(
                "Running: /usr/bin/ghc -shared -dynamic "
                "-o /tmp/native-linker/liblink.so /tmp/input.o\n"
            )
            args, suffix, source = native_command([main, package])
            self.assertEqual(source, package)
            self.assertEqual(suffix, ".so")
            self.assertEqual(args[-1], "/tmp/input.o")

    def test_cabal_response_file_logged_args(self):
        with TemporaryDirectory() as directory:
            root = Path(directory)
            main = root / "build.log"
            package = root / "package.log"
            main.write_text("completed liblink\n")
            package.write_text(
                "Linking DynWay library...\n"
                "GHC response file arguments: -package-env=- -shared -dynamic "
                "-package-id rts-fs-1.0.0.0 /tmp/input.dyn_o "
                "-o /tmp/build/libHSliblink-0.1.0.0-ghc9.14.so\n"
                "Running: /usr/bin/ghc @/tmp/ghc123.rsp\n"
            )
            args, suffix, source = native_command([main, package])
            self.assertEqual(source, package)
            self.assertEqual(suffix, ".so")
            self.assertEqual(args[0], "/usr/bin/ghc")
            self.assertIn("-shared", args)
            self.assertIn("-dynamic", args)
            self.assertIn("/tmp/input.dyn_o", args)
            self.assertEqual(args[args.index("-o") + 1], "/tmp/build/libHSliblink-0.1.0.0-ghc9.14.so")

    def test_cabal_response_file_on_disk(self):
        with TemporaryDirectory() as directory:
            root = Path(directory)
            rsp = root / "ghc123.rsp"
            rsp.write_text(
                "-shared -dynamic -package-id rts-fs-1.0.0.0 "
                "/tmp/input.dyn_o -o /tmp/build/libHSliblink-0.1.0.0-ghc9.14.dylib\n"
            )
            package = root / "package.log"
            package.write_text(f"Running: /usr/bin/ghc @{rsp}\n")
            args, suffix, source = native_command([package])
            self.assertEqual(source, package)
            self.assertEqual(suffix, ".dylib")
            self.assertEqual(args[0], "/usr/bin/ghc")
            self.assertIn("-shared", args)
            self.assertIn("/tmp/input.dyn_o", args)

    def test_cabal_response_file_interleaved_warning(self):
        with TemporaryDirectory() as directory:
            root = Path(directory)
            main = root / "build.log"
            package = root / "package.log"
            main.write_text("completed liblink\n")
            package.write_text(
                "Linking DynWay library...\n"
                "GHC response file arguments: -package-env=- -shared -dynamic "
                "-package-id rts-fs-1.0.0.0 /tmp/bui/usr/bin/ar: warning: creating /tmp/libHSliblink-0.1.0.0.a\n"
                "ld/host/input.dyn_o "
                "-o /tmp/build/libHSliblink-0.1.0.0-ghc9.14.dylib\n"
                "Running: /usr/bin/ghc @/tmp/ghc123.rsp\n"
            )
            args, suffix, source = native_command([main, package])
            self.assertEqual(source, package)
            self.assertEqual(suffix, ".dylib")
            self.assertEqual(args[0], "/usr/bin/ghc")
            self.assertIn("-shared", args)
            self.assertIn("-dynamic", args)
            self.assertIn("/tmp/build/host/input.dyn_o", args)
            self.assertEqual(
                args[args.index("-o") + 1],
                "/tmp/build/libHSliblink-0.1.0.0-ghc9.14.dylib",
            )

    def test_missing_command_is_rejected(self):
        with TemporaryDirectory() as directory:
            log = Path(directory) / "build.log"
            log.write_text("completed liblink\n")
            with self.assertRaisesRegex(AssertionError, "no native shared link"):
                native_command([log])


if __name__ == "__main__":
    unittest.main()
