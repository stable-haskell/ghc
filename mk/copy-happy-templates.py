#!/usr/bin/env python3
"""Copy the selected Build-stage Happy templates into a distribution."""

import argparse
import json
from pathlib import Path
import shutil


def copy_templates(plan, build_store, destination):
    units = [unit for unit in json.loads(plan.read_text())["install-plan"]
             if unit.get("pkg-name") == "happy-lib"
             and unit.get("component-name") == "lib"
             and unit.get("stage") == "build"]
    if len(units) != 1:
        raise ValueError("expected exactly one Build-stage happy-lib main library")
    unit = units[0]
    if unit.get("type") not in ("configured", "installed"):
        raise ValueError("selected happy-lib is not installed in the Build store")
    identity = unit["id"]
    if not identity.startswith("build:"):
        raise ValueError("Build-stage happy-lib has an inconsistent unit identity")
    raw_id = identity[len("build:"):]
    if raw_id in ("", ".", "..") or any(c in raw_id for c in "/\\:"):
        raise ValueError("invalid happy-lib unit identity")
    # GHC's project installs each unit's data directly in its lib/<unit>/share.
    data = build_store / "lib" / raw_id / "share"
    for name in ("HappyTemplate.hs", "GLR_Base.hs", "GLR_Lib.hs"):
        if not (data / name).is_file():
            raise ValueError("missing installed Happy template: " + str(data / name))
    shutil.copytree(data, destination, dirs_exist_ok=True, copy_function=shutil.copy2)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--plan", type=Path, required=True)
    parser.add_argument("--build-store", type=Path, required=True)
    parser.add_argument("--destination", type=Path, required=True)
    args = parser.parse_args()
    try:
        copy_templates(args.plan, args.build_store, args.destination)
    except (OSError, ValueError, KeyError, TypeError) as error:
        parser.exit(1, "Happy templates: " + str(error) + "\n")


if __name__ == "__main__":
    main()
