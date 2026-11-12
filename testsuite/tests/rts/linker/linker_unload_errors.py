#!/usr/bin/env python3
"""Check all six guarded error exits using the real collector before shutdown."""
import os
import subprocess
import sys


def main():
    executable, libdir = sys.argv[1:]
    for mode in ("load1", "resolve1", "lookup1", "load2", "resolve2", "lookup2"):
        env = os.environ.copy()
        env["UNLOAD_CAPABILITY_NEGATIVE"] = mode
        result = subprocess.run([executable, libdir], env=env, capture_output=True,
                                text=True, timeout=90)
        expected = [] if mode.endswith("1") else [str(i) for i in range(1000)]
        diagnostic = ("loadObj(Test.o) failed" if mode.startswith("load") else
                      "resolveObjs failed" if mode.startswith("resolve") else
                      "lookupSymbol failed")
        lines = result.stderr.splitlines()
        if not (result.returncode == 1 and result.stdout.split() == expected and
                len(lines) == 2 and lines[0].endswith(diagnostic) and
                lines[1] == "capability released on error"):
            raise RuntimeError("%s failed: status=%s stdout=%r stderr=%r" %
                               (mode, result.returncode, result.stdout, result.stderr))


if __name__ == "__main__":
    main()
