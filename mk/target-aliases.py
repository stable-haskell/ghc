#!/usr/bin/env python3
"""Write a target's alias manifest and the corresponding executable links.

The manifest lives beside the target's lib directory. Consumers resolve these
explicit aliases before loading settings; an executable spelling never becomes
a package identity. All names in this manifest are restricted to ASCII target
identifiers, so its JSON string list is also a Haskell Read [String] value.
"""

import argparse
import ast
import json
import os
from pathlib import Path
import re
import tempfile


def valid_name(name):
    return len(name) <= 255 and bool(re.fullmatch(r"[A-Za-z0-9](?:[A-Za-z0-9_.-]*[A-Za-z0-9])?", name)) and all(
        component and component not in (".", "..") for component in name.split("-")
    )


def target_aliases(machine, abi, extra_aliases=()):
    if abi not in ("", "profiling"):
        raise ValueError("unsupported target ABI: " + abi)
    qualifier = "-prof" if abi == "profiling" else ""
    target = machine + qualifier
    parts = machine.split("-")
    aliases = set(extra_aliases)
    if len(parts) >= 3:
        aliases.add("-".join([parts[0], *parts[2:]]) + qualifier)
    aliases.discard(target)
    for name in (target, *aliases):
        if not valid_name(name):
            raise ValueError("unsafe target name: " + repr(name))
    return target, sorted(aliases)


def check_collisions(target_dir, target, aliases):
    owners = {}

    def record(name, owner):
        if not valid_name(name):
            raise ValueError("unsafe target name in manifest: " + repr(name))
        previous = owners.get(name.casefold())
        if previous is not None:
            raise ValueError("ambiguous target alias " + repr(name) + ": " + previous + ", " + owner)
        owners[name.casefold()] = owner

    for directory in target_dir.parent.iterdir():
        if not directory.is_dir() or directory.name == target:
            continue
        record(directory.name, directory.name)
        manifest = directory / "target.aliases"
        if manifest.is_file():
            values = ast.literal_eval(manifest.read_text())
            if not isinstance(values, list) or not all(isinstance(v, str) for v in values):
                raise ValueError("invalid target alias manifest: " + str(manifest))
            for alias in values:
                record(alias, directory.name)
    for name in (target, *aliases):
        record(name, target)


def atomic_write(path, content):
    with tempfile.NamedTemporaryFile(mode="w", dir=path.parent, delete=False) as handle:
        temporary = Path(handle.name)
        try:
            handle.write(content)
            handle.close()
            temporary.replace(path)
        finally:
            try:
                temporary.unlink()
            except FileNotFoundError:
                pass


def write_aliases(target_dir, bin_dir, machine, abi, version, programs, extra_aliases=(), extension=""):
    target, aliases = target_aliases(machine, abi, extra_aliases)
    if target_dir.name != target:
        raise ValueError("target directory must use canonical target name " + target)
    if not valid_name(version) or extension not in ("", ".exe"):
        raise ValueError("invalid compiler version or executable extension")
    if not programs or any(not valid_name(program) for program in programs):
        raise ValueError("invalid target executable list")
    target_dir.mkdir(parents=True, exist_ok=True)
    check_collisions(target_dir, target, aliases)
    entries = []
    links = []
    for program in programs:
        canonical = target + "-" + program + extension
        if not (bin_dir / canonical).is_file():
            raise ValueError("missing canonical target executable: " + str(bin_dir / canonical))
        for name in (target, *aliases):
            for suffix in ("", "-" + version):
                executable = name + "-" + program + suffix + extension
                entries.append("bin/" + executable)
                if executable != canonical:
                    path = bin_dir / executable
                    if path.exists() or path.is_symlink():
                        if not path.is_symlink() or os.readlink(path) != canonical:
                            raise ValueError("refusing to replace executable: " + str(path))
                    else:
                        links.append((path, canonical))
    for path, canonical in links:
        path.symlink_to(canonical)
    atomic_write(target_dir / "target.aliases", json.dumps(aliases) + "\n")
    atomic_write(target_dir / "target.executables", "".join(entry + "\n" for entry in sorted(set(entries))))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--target-directory", type=Path, required=True)
    parser.add_argument("--bin-directory", type=Path, required=True)
    parser.add_argument("--machine-target", required=True)
    parser.add_argument("--abi", default="")
    parser.add_argument("--version", required=True)
    parser.add_argument("--program", action="append", required=True)
    parser.add_argument("--alias", action="append", default=[])
    parser.add_argument("--exe-extension", default="")
    args = parser.parse_args()
    try:
        write_aliases(args.target_directory, args.bin_directory, args.machine_target,
                      args.abi, args.version, args.program, args.alias, args.exe_extension)
    except (ValueError, SyntaxError, OSError) as error:
        parser.exit(1, "target aliases: " + str(error) + "\n")


if __name__ == "__main__":
    main()
