#!/usr/bin/env python3
"""Replay the actual native shared link and check its package boundary."""

import argparse
import hashlib
import json
from pathlib import Path
import shlex
import subprocess


def native_command(log):
    for line in reversed(log.read_text(errors="replace").splitlines()):
        if not line.startswith("Running: "):
            continue
        args = shlex.split(line[len("Running: "):])
        if "-shared" not in args or "-dynamic" not in args or "-o" not in args:
            continue
        output = Path(args[args.index("-o") + 1])
        if output.parent.name == "native-linker" and output.name in (
            "liblink.dylib", "liblink.so", "link.dll"
        ):
            return args, output.suffix
    raise AssertionError("no native shared link in verbose Cabal log")


def haskell_libraries(stderr):
    marker = "*** systool:linker:"
    assert marker in stderr, "missing actual linker trace"
    lines = [line.strip() for line in stderr.rsplit(marker, 1)[1].splitlines() if line.strip()]
    if lines and lines[0] == "*** Linker:":
        lines.pop(0)
    command = lines[0] if lines else ""
    assert command and not command.startswith("***"), "missing linker command"
    return [arg for arg in shlex.split(command) if arg.startswith("-lHS")]


def check_boundary(libraries, unit):
    assert len(libraries) == 1 and libraries[0].startswith("-lHS" + unit + "-ghc"), (
        "unexpected Haskell library inputs: " + repr(libraries)
    )


def hashes(paths):
    return {str(path): hashlib.sha256(path.read_bytes()).hexdigest() for path in paths}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-log", required=True, type=Path)
    parser.add_argument("--out", required=True, type=Path)
    options = parser.parse_args()
    args, suffix = native_command(options.build_log)
    units = [args[i + 1] for i, arg in enumerate(args) if arg == "-package-id"]
    assert len(units) == 1 and units[0].startswith("rts-fs-"), units
    objects = [Path(arg) for arg in args if arg.endswith(".o")]
    assert objects and all(path.is_absolute() for path in objects), "missing native objects"
    before = hashes(objects)
    out = options.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    report = {"build_log": str(options.build_log.resolve()), "objects": before, "cases": []}
    for name in ("configured", "automatic-packages"):
        command = args.copy()
        output = out / (name + suffix)
        command[command.index("-o") + 1] = str(output)
        if name == "automatic-packages":
            command = [arg for arg in command if arg != "-no-auto-link-packages"]
        command.append("-v")
        result = subprocess.run(command, capture_output=True, text=True, timeout=45)
        (out / (name + ".stdout")).write_text(result.stdout)
        (out / (name + ".stderr")).write_text(result.stderr)
        libraries = haskell_libraries(result.stderr)
        case = {"name": name, "argv": command, "exit": result.returncode,
                "output_exists": output.is_file(), "haskell_libraries": libraries}
        report["cases"].append(case)
        (out / "report.json").write_text(json.dumps(report, indent=2) + "\n")
        if name == "configured":
            assert result.returncode == 0 and output.is_file(), "native shared link failed"
            check_boundary(libraries, units[0])
            print("PASS: native shared link uses only the declared rts-fs package", flush=True)
        else:
            assert any(not lib.startswith("-lHS" + units[0] + "-ghc") for lib in libraries), (
                "counterfactual did not introduce an unexpected Haskell library"
            )
            try:
                check_boundary(libraries, units[0])
            except AssertionError:
                pass
            else:
                raise AssertionError("boundary check accepted automatic Haskell dependencies")
            print("PASS: automatic package dependency regression rejected", flush=True)
    assert hashes(objects) == before, "link changed native object inputs"


if __name__ == "__main__":
    main()
