#!/usr/bin/env python3
"""Compile and run ghc-boot's Host.hs generator against candidate Cabal."""
import argparse
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
BOOT = ROOT / "libraries/ghc-boot"
SETUP = BOOT / "Setup.hs"
DRIVER = BOOT / "tests" / "SetupHostPlatformRegression.hs"
ARCHOS = ROOT / "libraries/ghc-platform" / "src"


def run(args):
    subprocess.run(args, check=True)


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--ghc", default="ghc", help="GHC compiler used for the focused compile")
parser.add_argument("--package-db", action="append", default=[],
                    help="package DB to use; pass GHC stage0 dependency DB first, then candidate Cabal DB")
parser.add_argument("--cabal-package", default="Cabal-3.17.0.1")
parser.add_argument("--cabal-syntax-package", default="Cabal-syntax-3.17.0.0")
args = parser.parse_args()

package_db_flags = [item for db in args.package_db for item in ("-package-db", db)]
package_flags = [
    "-package", args.cabal_package,
    "-package", args.cabal_syntax_package,
    "-package", "ghc-boot",
]
with tempfile.TemporaryDirectory(prefix="setup-host-platform-") as tmp:
    tmp = Path(tmp)
    generator = tmp / "generate-host"
    generated_other_os = tmp / "other-os"
    generated_other_arch = tmp / "other-arch"
    generated_linux = tmp / "linux"
    run([
        args.ghc, "--make", *package_db_flags, *package_flags,
        "-i" + str(BOOT), str(SETUP), str(DRIVER),
        "-main-is", "SetupHostPlatformRegression.main",
        "-outputdir", str(tmp / "generator-objects"), "-o", str(generator),
    ])
    run([str(generator), str(generated_other_os), str(generated_other_arch),
         str(generated_linux)])

    checker = tmp / "CheckHost.hs"
    checker.write_text('''module Main where
import GHC.Platform.Host (hostPlatformArch, hostPlatformOS)
import System.Environment (getArgs)
main :: IO ()
main = do
  [expectedOS, expectedArch] <- getArgs
  if show hostPlatformOS == expectedOS && show hostPlatformArch == expectedArch
    then pure ()
    else fail ("unexpected host platform: " ++ show hostPlatformArch ++ "-" ++ show hostPlatformOS)
''')
    for name, directory, expected_os, expected_arch in (
        ("other-os", generated_other_os, "OSUnknown", "ArchX86_64"),
        ("other-arch", generated_other_arch, "OSLinux", "ArchUnknown"),
        ("linux", generated_linux, "OSLinux", "ArchX86_64"),
    ):
        output = tmp / ("check-" + name)
        run([
            args.ghc, "--make", *package_db_flags, *package_flags,
            "-i" + str(directory), "-i" + str(ARCHOS),
            "-outputdir", str(tmp / (name + "-objects")), str(checker),
            "-o", str(output),
        ])
        run([str(output), expected_os, expected_arch])

print("Setup.hs generated OtherOS/OtherArch and Cabal-classified Linux Host modules compile and run")
