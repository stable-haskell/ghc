#!/usr/bin/env python3
"""Check runghc's argv[0] target selection, version names and -f override."""

import argparse
import os
from pathlib import Path
import shutil
import subprocess
import tempfile


def run(command, *, check=True):
    return subprocess.run(command, check=check, text=True, capture_output=True,
                          timeout=90)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ghc", type=Path, required=True)
    parser.add_argument("--source", type=Path, required=True)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="runghc-dispatch-") as scratch:
        work = Path(scratch)
        suffix = ".exe" if os.name == "nt" else ""
        (work / "ghcplatform.h").write_text(
            '#define CURRENT_PACKAGE_VERSION "dispatch-test"\n' +
            ('#define mingw32_HOST_OS 1\n' if os.name == "nt" else ''))
        source = work / "RunGhc.hs"
        shutil.copy2(args.source, source)
        driver = work / ("driver" + suffix)
        run([args.ghc, "-v0", "-I" + str(work), source, "-o", driver])
        (work / "FakeGhc.hs").write_text(
            'import System.Environment\nmain = getProgName >>= putStrLn\n')
        fake = work / ("fake" + suffix)
        run([args.ghc, "-v0", work / "FakeGhc.hs", "-o", fake])
        cases = [
            ("runghc", "ghc"),
            ("runghc-9.14", "ghc"),
            ("aarch64-apple-darwin-profiling-runghc", "aarch64-apple-darwin-profiling-ghc"),
            ("aarch64-apple-darwin-profiling-runghc-9.14", "aarch64-apple-darwin-profiling-ghc"),
        ]
        if os.name != "nt":
            cases.append(("aarch64-apple-darwin-profiling-runghc-9.14.exe",
                          "aarch64-apple-darwin-profiling-ghc"))
        for invocation, compiler in cases:
            invoked = work / (invocation + suffix)
            selected = work / (compiler + suffix)
            shutil.copy2(driver, invoked)
            shutil.copy2(fake, selected)
            actual = run([invoked, "does-not-exist.hs"]).stdout.strip()
            assert actual == selected.name, (invocation, actual, selected.name)
        explicit = work / ("override" + suffix)
        shutil.copy2(fake, explicit)
        prefixed = work / (cases[2][0] + suffix)
        if os.name != "nt":
            prefixed.unlink()
            prefixed.symlink_to(driver.name)
            assert run([prefixed, "test.hs"]).stdout.strip() == cases[2][1]
        assert run([prefixed, "-f", explicit, "test.hs"]).stdout.strip() == explicit.name
        # Losing a target installation must not fall back to native GHC.
        (work / (cases[2][1] + suffix)).unlink()
        failed = run([prefixed, "test.hs"], check=False)
        assert failed.returncode != 0 and not failed.stdout, "missing target fell back to native GHC"
        # Preserve the existing unprefixed in-tree Hadrian fallback.
        (work / ("ghc" + suffix)).unlink()
        fallback = work / ("ghc-stage2" + suffix)
        shutil.copy2(fake, fallback)
        assert run([work / ("runghc" + suffix), "test.hs"]).stdout.strip() == fallback.name
    print("runghc target dispatch passed")


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as failure:
        print(failure.stdout, end="")
        print(failure.stderr, end="")
        raise
