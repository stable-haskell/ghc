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

    def test_missing_command_is_rejected(self):
        with TemporaryDirectory() as directory:
            log = Path(directory) / "build.log"
            log.write_text("completed liblink\n")
            with self.assertRaisesRegex(AssertionError, "no native shared link"):
                native_command([log])


if __name__ == "__main__":
    unittest.main()
