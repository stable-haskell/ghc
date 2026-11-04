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

``cabal buck2 --variant stage2`` does not build alex or happy either.
``readBaseTools`` in Cabal's ``CmdBuck2`` scans only the base
``third-party/haskell/BUCK`` for a ``name = '<tool>-exe'`` line whose
next line is ``src = '<path>'``. The joined path
``third-party/haskell/<path>`` is what configure runs for the version
check, and the same target is what stage2 ``build_tool_depends``
references. Copy those two calls into the base file. Rewrite ``src``
through a symlink inside the base cell
(``stage1-tools-cabal-store`` -> ``../haskell-stage1-tools/cabal-store``)
so the path stays inside the package, the same way Cabal's own
``cabal-store`` symlink does. A base file that already exports the tool
is left alone.

    mk/buck2-stage1-tools.py
    mk/buck2-stage1-tools.py --self-test
"""

from __future__ import annotations

import os
import sys

BASE_PREFIX = "third-party/haskell/"
VARIANT_PREFIX = "third-party/haskell-stage1-tools/"
TOOLS = ("alex", "happy")
MARKER_BEGIN = "# BEGIN stage1-tools executables (mk/buck2-stage1-tools.py)"
MARKER_END = "# END stage1-tools executables"


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


def export_src(text: str, tool: str) -> str:
    """The ``src`` Cabal's ``readBaseTools`` would pair with this export.

    The Haskell parser keeps a tool only when the ``src`` line is the
    immediate next line and both sides use single quotes.
    """
    lines = text.splitlines()
    name_prefix = f"name = '{tool}-exe'"
    for index, line in enumerate(lines):
        if not line.lstrip(" ").startswith(name_prefix):
            continue
        if index + 1 >= len(lines):
            raise ValueError(f"{tool}-exe has no following src line")
        src_line = lines[index + 1].lstrip(" ")
        if not src_line.startswith("src = '"):
            raise ValueError(f"{tool}-exe src is not the next single-quoted line")
        rest = src_line[len("src = '") :]
        quote = rest.find("'")
        if quote < 0:
            raise ValueError(f"{tool}-exe src is not closed")
        src = rest[:quote]
        if not src or "'" in src or src.startswith(("/", "../")) or "/" not in src:
            raise ValueError(f"{tool}-exe src is not a relative store path: {src}")
        return src
    raise ValueError(f"no single-quoted {tool}-exe export")


def extract_export(text: str, tool: str) -> str:
    """The ``export_file(...)`` call that defines ``<tool>-exe``."""
    needle = f"name = '{tool}-exe'"
    start_at = 0
    while True:
        idx = text.find(needle, start_at)
        if idx < 0:
            raise ValueError(f"no export_file for {tool}-exe")
        call_at = text.rfind("export_file(", 0, idx)
        if call_at < 0:
            raise ValueError(f"{tool}-exe is not inside export_file")
        depth = 0
        for pos in range(call_at, len(text)):
            char = text[pos]
            if char == "(":
                depth += 1
            elif char == ")":
                depth -= 1
                if depth == 0:
                    return text[call_at : pos + 1]
        start_at = idx + len(needle)


def rebase_src(src: str) -> tuple[str, str, str]:
    """Map a variant-cell ``src`` onto a symlink inside the base cell.

    Returns ``(link name, link target, src for the base export)``.
    ``cabal-store/ghc/bin/alex`` becomes
    ``stage1-tools-cabal-store/ghc/bin/alex``, with the link pointing at
    ``../haskell-stage1-tools/cabal-store``.
    """
    top, _, rest = src.partition("/")
    if not top or top in (".", "..") or not rest or "/" in top:
        raise ValueError(f"tool src has no cell-relative prefix: {src}")
    link = "stage1-tools-" + top
    return link, "../haskell-stage1-tools/" + top, link + "/" + rest


def rewrite_src(call: str, new_src: str) -> str:
    lines = call.splitlines()
    replaced = False
    out: list[str] = []
    for index, line in enumerate(lines):
        previous = lines[index - 1].lstrip(" ") if index else ""
        if previous.startswith("name = '") and line.lstrip(" ").startswith("src = '"):
            indent = line[: len(line) - len(line.lstrip(" "))]
            comma = "," if line.rstrip().endswith(",") else ""
            out.append(f"{indent}src = '{new_src}'{comma}")
            replaced = True
        else:
            out.append(line)
    if not replaced:
        raise ValueError("export_file has no src line after its name")
    return "\n".join(out)


def strip_marker(text: str) -> str:
    begin = text.find(MARKER_BEGIN)
    if begin < 0:
        return text
    end = text.find(MARKER_END, begin)
    if end < 0:
        raise ValueError("base BUCK has an unclosed stage1-tools marker")
    end = end + len(MARKER_END)
    if end < len(text) and text[end] == "\n":
        end += 1
    # Drop a blank line that this script inserted before the marker.
    while begin >= 2 and text[begin - 2 : begin] == "\n\n":
        begin -= 1
    return text[:begin] + text[end:]


def splice_exports(text: str, calls: list[str]) -> str:
    """Append ``calls``, replacing a previous copy of this block.

    A tool that the base file already exports is not added again.
    ``calls`` is empty when every tool is already there.
    """
    stripped = strip_marker(text)
    missing = [call for call in calls if not has_export(stripped, _tool_of_call(call))]
    if not missing:
        return stripped if stripped.endswith("\n") or stripped == "" else stripped + "\n"
    body = stripped if stripped.endswith("\n") or stripped == "" else stripped + "\n"
    if not body.endswith("\n\n"):
        body += "\n"
    block = MARKER_BEGIN + "\n" + "\n\n".join(missing) + "\n" + MARKER_END + "\n"
    return body + block


def _tool_of_call(call: str) -> str:
    for tool in TOOLS:
        if f"name = '{tool}-exe'" in call:
            return tool
    raise ValueError("export_file is not alex-exe or happy-exe")


def read_base_tools(text: str) -> dict[str, str]:
    """Replica of Cabal's ``readBaseTools`` line pairing."""
    lines = text.splitlines()
    found: dict[str, str] = {}
    for name_line, src_line in zip(lines, lines[1:]):
        rest = name_line.lstrip(" ")
        if not rest.startswith("name = '"):
            continue
        name = rest[len("name = '") :].split("'", 1)[0]
        if not name.endswith("-exe"):
            continue
        src_rest = src_line.lstrip(" ")
        if not src_rest.startswith("src = '"):
            continue
        src = src_rest[len("src = '") :].split("'", 1)[0]
        found[name[: -len("-exe")]] = src
    return found


def ensure_symlink(link: str, target: str) -> None:
    if os.path.islink(link):
        if os.readlink(link) == target:
            return
        os.remove(link)
    elif os.path.exists(link):
        raise ValueError(f"{link} exists and is not a symlink")
    os.symlink(target, link)


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
    base_buck_path = os.path.join(root, "third-party", "haskell", "BUCK")
    base_dir = os.path.join(root, "third-party", "haskell")
    with open(base_buck_path, encoding="utf-8") as handle:
        base_buck = handle.read()
    # Drop our previous block before deciding which tools are missing, so a
    # second run refreshes the src instead of treating our own copy as the
    # compiler plan's export.
    already = strip_marker(base_buck)
    calls: list[str] = []
    for tool in TOOLS:
        if has_export(already, tool):
            continue
        src = export_src(buck_text, tool)
        link, target, new_src = rebase_src(src)
        if os.path.basename(new_src) != tool:
            raise ValueError(f"{tool} src does not end in the program name: {src}")
        ensure_symlink(os.path.join(base_dir, link), target)
        binary = os.path.join(base_dir, new_src)
        if not os.path.isfile(binary) or not os.access(binary, os.X_OK):
            raise ValueError(f"{tool} binary is not executable: {binary}")
        calls.append(rewrite_src(extract_export(buck_text, tool), new_src))
    patched_buck = splice_exports(base_buck, calls)
    found = read_base_tools(patched_buck)
    for tool in TOOLS:
        src = found.get(tool)
        if not src:
            raise ValueError(f"base BUCK does not expose {tool} to readBaseTools")
        binary = os.path.join(base_dir, src)
        if not os.path.isfile(binary) or not os.access(binary, os.X_OK):
            raise ValueError(f"{tool} binary is not executable: {binary}")
    if patched_buck != base_buck:
        with open(base_buck_path, "w", encoding="utf-8") as handle:
            handle.write(patched_buck)


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
    _self_test_tree()


def _executable(path: str) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as handle:
        handle.write("#!/bin/sh\nprintf 'alex 3.5.2.0\\n'\n")
    os.chmod(path, 0o755)


def _export(name: str, src: str) -> str:
    return (
        "export_file(\n"
        f"    name = '{name}-exe',\n"
        f"    src = '{src}',\n"
        f"    out = '{name}',\n"
        "    mode = 'copy',\n"
        "    executable_bit_override = True,\n"
        "    visibility = ['PUBLIC'],\n"
        ")\n"
    )


def _self_test_tree() -> None:
    import tempfile

    with tempfile.TemporaryDirectory() as tmp:
        variant = os.path.join(tmp, "third-party", "haskell-stage1-tools")
        base = os.path.join(tmp, "third-party", "haskell")
        os.makedirs(variant)
        os.makedirs(base)
        alex_src = "cabal-store/ghc-9.8.4/bin/alex"
        happy_src = "cabal-store/ghc-9.8.4/bin/happy"
        _executable(os.path.join(variant, alex_src))
        _executable(os.path.join(variant, happy_src))
        with open(os.path.join(variant, "tools.bzl"), "w", encoding="utf-8") as handle:
            handle.write(
                'ALEX = "third-party/haskell/cabal-store/ghc-9.8.4/bin/alex"\n'
                'HAPPY = "third-party/haskell/cabal-store/ghc-9.8.4/bin/happy"\n'
            )
        with open(os.path.join(variant, "BUCK"), "w", encoding="utf-8") as handle:
            handle.write(_export("alex", alex_src) + "\n" + _export("happy", happy_src))
        with open(os.path.join(base, "tools.bzl"), "w", encoding="utf-8") as handle:
            handle.write(
                'ALEX = "third-party/haskell/missing"\n'
                'HAPPY = "third-party/haskell/missing"\n'
            )
        with open(os.path.join(base, "BUCK"), "w", encoding="utf-8") as handle:
            handle.write("haskell_prebuilt_library(\n    name = 'base',\n)\n")
        patch_tree(tmp)
        with open(os.path.join(base, "BUCK"), encoding="utf-8") as handle:
            buck = handle.read()
        found = read_base_tools(buck)
        if found.get("alex") != "stage1-tools-cabal-store/ghc-9.8.4/bin/alex":
            sys.exit(f"self-test: readBaseTools alex {found.get('alex')}")
        if found.get("happy") != "stage1-tools-cabal-store/ghc-9.8.4/bin/happy":
            sys.exit(f"self-test: readBaseTools happy {found.get('happy')}")
        if buck.count("name = 'alex-exe'") != 1 or buck.count("name = 'happy-exe'") != 1:
            sys.exit("self-test: export was duplicated")
        if "executable_bit_override = True" not in buck:
            sys.exit("self-test: export_file fields were dropped")
        link = os.path.join(base, "stage1-tools-cabal-store")
        if os.readlink(link) != "../haskell-stage1-tools/cabal-store":
            sys.exit(f"self-test: symlink {os.readlink(link)}")
        alex_bin = os.path.join(base, found["alex"])
        if not os.path.samefile(alex_bin, os.path.join(variant, alex_src)):
            sys.exit("self-test: alex symlink does not reach the variant binary")
        with open(os.path.join(base, "tools.bzl"), encoding="utf-8") as handle:
            tools = handle.read()
        if "haskell-stage1-tools/cabal-store" not in tools:
            sys.exit("self-test: tools.bzl was not rewritten in the tree")
        patch_tree(tmp)
        with open(os.path.join(base, "BUCK"), encoding="utf-8") as handle:
            again = handle.read()
        if again != buck:
            sys.exit("self-test: patching the tree twice changed the base BUCK")

        # A compiler plan that already exports the tools must be left alone.
        pre = os.path.join(tmp, "pre")
        pre_base = os.path.join(pre, "third-party", "haskell")
        pre_variant = os.path.join(pre, "third-party", "haskell-stage1-tools")
        os.makedirs(pre_base)
        os.makedirs(pre_variant)
        _executable(os.path.join(pre_variant, alex_src))
        _executable(os.path.join(pre_variant, happy_src))
        _executable(os.path.join(pre_base, alex_src))
        _executable(os.path.join(pre_base, happy_src))
        with open(os.path.join(pre_variant, "tools.bzl"), "w", encoding="utf-8") as handle:
            handle.write(
                'ALEX = "third-party/haskell/cabal-store/ghc-9.8.4/bin/alex"\n'
                'HAPPY = "third-party/haskell/cabal-store/ghc-9.8.4/bin/happy"\n'
            )
        with open(os.path.join(pre_variant, "BUCK"), "w", encoding="utf-8") as handle:
            handle.write(_export("alex", alex_src) + "\n" + _export("happy", happy_src))
        with open(os.path.join(pre_base, "tools.bzl"), "w", encoding="utf-8") as handle:
            handle.write(
                'ALEX = "third-party/haskell/missing"\n'
                'HAPPY = "third-party/haskell/missing"\n'
            )
        original = "haskell_prebuilt_library(\n    name = 'base',\n)\n\n" + _export(
            "alex", alex_src
        ) + "\n" + _export("happy", happy_src)
        with open(os.path.join(pre_base, "BUCK"), "w", encoding="utf-8") as handle:
            handle.write(original)
        patch_tree(pre)
        with open(os.path.join(pre_base, "BUCK"), encoding="utf-8") as handle:
            kept = handle.read()
        if kept != original:
            sys.exit("self-test: an existing base export was rewritten")
        if os.path.lexists(os.path.join(pre_base, "stage1-tools-cabal-store")):
            sys.exit("self-test: a symlink was added beside an existing export")


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
