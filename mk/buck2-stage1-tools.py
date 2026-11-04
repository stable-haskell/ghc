#!/usr/bin/env python3
"""Wire Buck2's stage1 alex and happy at the tools-variant binaries.

``cabal buck2`` on ``cabal.project.stage1`` sets ``-build-tool-depends``,
so ``third-party/haskell/tools.bzl`` records ``ALEX`` and ``HAPPY`` as
``missing``. ``buck2/haskell.bzl`` still runs those strings on ``.x`` and
``.y`` sources.

``cabal buck2 --variant stage1-tools`` builds the pinned tools and writes
the real paths into ``third-party/haskell-stage1-tools/tools.bzl``. The
generator prefixes every path with ``third-party/haskell/``. Rewrite that
prefix onto the variant cell, whose ``cabal-store`` and ``inplace``
symlinks point at the binaries, and copy the two assignments into the
base ``tools.bzl``.

``cabal buck2`` does not emit a local rule for a Hackage executable.
It writes ``export_file(name = 'alex-exe')`` into the variant cell, and
genprimopcode's ``build_tool_depends`` names
``third-party-haskell-stage1-tools//:alex-exe``. This script checks
that those exports exist. It does not invent a ``:alex`` alias.

    mk/buck2-stage1-tools.py
    mk/buck2-stage1-tools.py --self-test
"""

from __future__ import annotations

import os
import sys

BASE_PREFIX = "third-party/haskell/"
VARIANT_PREFIX = "third-party/haskell-stage1-tools/"
TOOLS = ("alex", "happy")


def assignment(text: str, name: str) -> str | None:
    prefix = name + " = "
    for line in text.splitlines():
        stripped = line.strip()
        if not stripped.startswith(prefix):
            continue
        raw = stripped[len(prefix) :].strip()
        if len(raw) < 2 or raw[0] != '"' or raw[-1] != '"':
            raise ValueError(f"{name} is not a double-quoted string: {stripped}")
        return raw[1:-1]
    return None


def variant_path(value: str, name: str) -> str:
    if not value.startswith(BASE_PREFIX):
        raise ValueError(f"{name} does not start with {BASE_PREFIX}: {value}")
    rest = value[len(BASE_PREFIX) :]
    if rest == "missing" or rest.startswith("missing/"):
        raise ValueError(f"{name} was not built by the stage1-tools variant")
    return VARIANT_PREFIX + rest


def patch_base(text: str, alex: str, happy: str) -> str:
    wanted = {"ALEX": alex, "HAPPY": happy}
    seen: set[str] = set()
    lines: list[str] = []
    for line in text.splitlines():
        stripped = line.strip()
        replaced = False
        for name, value in wanted.items():
            if stripped.startswith(name + " = "):
                lines.append(f'{name} = "{value}"')
                seen.add(name)
                replaced = True
                break
        if not replaced:
            lines.append(line)
    missing = [name for name in wanted if name not in seen]
    if missing:
        raise ValueError("base tools.bzl has no assignment for " + ", ".join(missing))
    suffix = "\n" if text.endswith("\n") or text == "" else ""
    return "\n".join(lines) + suffix


def has_export(text: str, name: str) -> bool:
    """True when the variant cell exports ``<name>-exe``.

    Cabal's Starlark renderer quotes with single quotes. A hand-written
    file may use double quotes, so both count.
    """
    return f"name = '{name}-exe'" in text or f'name = "{name}-exe"' in text


def patch_tree(root: str) -> None:
    variant_tools = os.path.join(root, "third-party", "haskell-stage1-tools", "tools.bzl")
    base_tools = os.path.join(root, "third-party", "haskell", "tools.bzl")
    with open(variant_tools, encoding="utf-8") as handle:
        variant_text = handle.read()
    alex = variant_path(assignment(variant_text, "ALEX") or "", "ALEX")
    happy = variant_path(assignment(variant_text, "HAPPY") or "", "HAPPY")
    with open(base_tools, encoding="utf-8") as handle:
        base_text = handle.read()
    patched = patch_base(base_text, alex, happy)
    if patched != base_text:
        with open(base_tools, "w", encoding="utf-8") as handle:
            handle.write(patched)
    variant_buck = os.path.join(root, "third-party", "haskell-stage1-tools", "BUCK")
    with open(variant_buck, encoding="utf-8") as handle:
        buck_text = handle.read()
    missing = [name for name in TOOLS if not has_export(buck_text, name)]
    if missing:
        raise ValueError(
            "third-party/haskell-stage1-tools/BUCK has no export for "
            + ", ".join(name + "-exe" for name in missing)
        )


def self_test() -> None:
    alex = variant_path(BASE_PREFIX + "cabal-store/ghc/bin/alex", "ALEX")
    happy = variant_path(BASE_PREFIX + "inplace/happy-inplace/bin/happy", "HAPPY")
    if alex != VARIANT_PREFIX + "cabal-store/ghc/bin/alex":
        sys.exit(f"self-test: alex path {alex}")
    if happy != VARIANT_PREFIX + "inplace/happy-inplace/bin/happy":
        sys.exit(f"self-test: happy path {happy}")
    try:
        variant_path(BASE_PREFIX + "missing", "ALEX")
    except ValueError:
        pass
    else:
        sys.exit("self-test: missing alex was accepted")
    base = 'GHC_VERSION = "9.8.4"\nALEX = "third-party/haskell/missing"\nHAPPY = "third-party/haskell/missing"\n'
    patched = patch_base(base, alex, happy)
    if f'ALEX = "{alex}"' not in patched or f'HAPPY = "{happy}"' not in patched:
        sys.exit("self-test: base tools.bzl was not patched")
    if patch_base(patched, alex, happy) != patched:
        sys.exit("self-test: patching twice changed the file")
    if not has_export("export_file(\n    name = 'alex-exe',\n", "alex"):
        sys.exit("self-test: single-quoted alex-exe was not recognised")
    if not has_export('export_file(\n    name = "happy-exe",\n', "happy"):
        sys.exit("self-test: double-quoted happy-exe was not recognised")
    if has_export("name = 'alex'\n", "alex"):
        sys.exit("self-test: an unsuffixed name looked like the export")


def main(argv: list[str]) -> None:
    if argv[1:] == ["--self-test"]:
        self_test()
        return
    if len(argv) != 1:
        sys.exit(f"usage: {argv[0]} | --self-test")
    try:
        patch_tree(".")
    except (OSError, ValueError) as error:
        sys.exit(f"buck2-stage1-tools: {error}")


if __name__ == "__main__":
    main(sys.argv)
