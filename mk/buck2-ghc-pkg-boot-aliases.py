#!/usr/bin/env python3
"""Alias the ghc-pkg-boot copies of the boot RTS closure.

`cabal buck2 --variant ghc-pkg-boot` writes a second third-party cell.
`haskell.bzl` still adds `@third-party-haskell//:base` and `:rts` on the
default platform. `stage1-libdir` depends on ghc-pkg in that platform, so
the `ghc-pkg-boot` select arm is not the configuration Buck2 analyzes.
The variant cell then contributes the same bootstrap `.so` again.

GHC 9.8.4's `base` depends on `ghc-bignum`, `ghc-prim`, and `rts`. A
prebuilt library merges the shared libraries of its deps, so aliasing
only `base` leaves the next soname. An `alias` forwards the main cell's
providers, and the duplicate check allows the same artifact twice.

    mk/buck2-ghc-pkg-boot-aliases.py BUCK
    mk/buck2-ghc-pkg-boot-aliases.py --self-test
"""

from __future__ import annotations

import sys

# Package deps of base-4.19.2.0 on the GHC 9.8.4 bootstrap, plus base.
# `ghc-pkg field` on that compiler: base -> ghc-bignum ghc-prim rts;
# ghc-bignum -> ghc-prim; ghc-prim -> rts; rts -> (none).
BOOT_CLOSURE = ("base", "rts", "ghc-prim", "ghc-bignum")
MAIN_CELL = "third-party-haskell"


def call_end(text: str, open_paren: int) -> int:
    """Index of the `)` that closes the call whose `(` is `open_paren`."""
    depth = 0
    quote = ""
    escape = False
    i = open_paren
    while i < len(text):
        char = text[i]
        if quote:
            if escape:
                escape = False
            elif char == "\\":
                escape = True
            elif char == quote:
                quote = ""
        elif char in ("'", '"'):
            quote = char
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return i
        i += 1
    raise ValueError("unclosed haskell_prebuilt_library call")


def rule_name(call: str) -> str | None:
    marker = "name = "
    at = call.find(marker)
    if at < 0:
        return None
    rest = call[at + len(marker) :]
    if not rest or rest[0] not in ("'", '"'):
        return None
    quote = rest[0]
    end = rest.find(quote, 1)
    if end < 0:
        return None
    return rest[1:end]


def alias_call(name: str) -> str:
    return (
        "alias(\n"
        f"    name = '{name}',\n"
        f"    actual = '{MAIN_CELL}//:{name}',\n"
        "    visibility = ['PUBLIC'],\n"
        ")"
    )


def rewrite(text: str) -> tuple[str, set[str]]:
    needle = "haskell_prebuilt_library("
    replaced: set[str] = set()
    out: list[str] = []
    cursor = 0
    while True:
        at = text.find(needle, cursor)
        if at < 0:
            out.append(text[cursor:])
            break
        open_paren = at + len(needle) - 1
        end = call_end(text, open_paren)
        call = text[at : end + 1]
        name = rule_name(call)
        out.append(text[cursor:at])
        if name in BOOT_CLOSURE:
            out.append(alias_call(name))
            replaced.add(name)
        else:
            out.append(call)
        cursor = end + 1
    return "".join(out), replaced


def rewrite_file(path: str) -> None:
    with open(path, encoding="utf-8") as handle:
        text = handle.read()
    rewritten, replaced = rewrite(text)
    missing = [name for name in BOOT_CLOSURE if name not in replaced]
    if missing:
        sys.exit(
            f"{path}: no haskell_prebuilt_library for {', '.join(missing)}"
        )
    if rewritten != text:
        with open(path, "w", encoding="utf-8") as handle:
            handle.write(rewritten)


def self_test() -> None:
    sample = """\
haskell_prebuilt_library(
    name = 'base',
    shared_libs = {
        'libHSbase-4.19.2.0-9fbd-ghc9.8.4.so': 'ghc-9.8.4/libHSbase.so',
    },
    deps = [
        ':ghc-bignum',
        ':ghc-prim',
        ':rts',
    ],
    visibility = [
        'PUBLIC',
    ],
)

haskell_prebuilt_library(
    name = 'pretty',
    deps = [
        ':base',
        ':deepseq',
        ':ghc-prim',
    ],
    visibility = [
        'PUBLIC',
    ],
)

haskell_prebuilt_library(
    name = 'ghc-prim',
    deps = [
        ':rts',
    ],
    visibility = [
        'PUBLIC',
    ],
)

haskell_prebuilt_library(
    name = 'note',
    id = 'keep (parens)',
    visibility = [
        'PUBLIC',
    ],
)

haskell_prebuilt_library(
    name = 'rts',
    visibility = [
        'PUBLIC',
    ],
)

haskell_prebuilt_library(
    name = 'ghc-bignum',
    deps = [
        ':ghc-prim',
    ],
    visibility = [
        'PUBLIC',
    ],
)
"""
    rewritten, replaced = rewrite(sample)
    if replaced != set(BOOT_CLOSURE):
        sys.exit(f"self-test: replaced {sorted(replaced)}")
    for name in BOOT_CLOSURE:
        if f"name = '{name}',\n    actual = '{MAIN_CELL}//:{name}',\n" not in rewritten:
            sys.exit(f"self-test: missing alias for {name}")
        if f"haskell_prebuilt_library(\n    name = '{name}'," in rewritten:
            sys.exit(f"self-test: {name} is still a prebuilt library")
    if "name = 'pretty'" not in rewritten or "':base'," not in rewritten:
        sys.exit("self-test: pretty lost its base dep")
    if "id = 'keep (parens)'," not in rewritten:
        sys.exit("self-test: a quoted parenthesis ended a call early")
    if rewritten.count("haskell_prebuilt_library(") != 2:
        sys.exit("self-test: unexpected number of remaining prebuilt rules")
    # A second pass finds no prebuilt rules for the closure. Generation
    # overwrites the file first, so `buck2-stage1` runs this once.
    again, replaced_again = rewrite(rewritten)
    if again != rewritten or replaced_again:
        sys.exit("self-test: second pass changed an aliased file")


def main(argv: list[str]) -> None:
    if argv[1:] == ["--self-test"]:
        self_test()
        return
    if len(argv) != 2:
        sys.exit(f"usage: {argv[0]} BUCK | --self-test")
    rewrite_file(argv[1])


if __name__ == "__main__":
    main(sys.argv)
