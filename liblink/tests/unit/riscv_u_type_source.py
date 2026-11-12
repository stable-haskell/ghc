#!/usr/bin/env python3
"""Generate the portable U-type helper unit from current RISC-V source."""

from __future__ import annotations

import argparse
import hashlib
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_SOURCE = ROOT / "liblink/src/linker/elf_reloc_riscv64.c"
TEMPLATE = Path(__file__).with_name("riscv_u_type.c")
MARKER = "/* RISCV_U_TYPE_HELPER_DEFINITIONS */"
EXPECTED_SIGNATURES = {
    "checkInt": {
        "void checkInt(ll_linker *L, inst_t *loc, int32_t v, int n)",
        "bool checkInt(ll_linker *L, const void *loc, int32_t v, int n)",
    },
    "read32le": {"uint32_t read32le(const inst_t *p)"},
    "write32le": {"void write32le(inst_t *p, uint32_t v)"},
    "setUType": {
        "void setUType(ll_linker *L, inst_t *loc, int32_t val)",
        "bool setUType(ll_linker *L, inst_t *loc, int32_t val)",
    },
}


def find_body_end(source: str, opening_brace: int, name: str) -> int:
    depth = 0
    state = "code"
    i = opening_brace
    while i < len(source):
        current = source[i]
        following = source[i + 1] if i + 1 < len(source) else ""
        if state == "code":
            if current == "/" and following == "*":
                state = "block_comment"
                i += 2
                continue
            if current == "/" and following == "/":
                state = "line_comment"
                i += 2
                continue
            if current == '"':
                state = "string"
            elif current == "'":
                state = "character"
            elif current == "{":
                depth += 1
            elif current == "}":
                depth -= 1
                if depth == 0:
                    return i + 1
        elif state == "block_comment":
            if current == "*" and following == "/":
                state = "code"
                i += 2
                continue
        elif state == "line_comment":
            if current == "\n":
                state = "code"
        elif state in ("string", "character"):
            if current == "\\":
                i += 2
                continue
            if (state == "string" and current == '"') or (
                state == "character" and current == "'"
            ):
                state = "code"
        i += 1
    raise RuntimeError(f"unterminated function body for {name}")


def extract_function(source: str, name: str) -> str:
    return_types = "void|bool|uint32_t"
    pattern = re.compile(
        rf"(?m)^[ \t]*(?P<signature>(?:{return_types})[ \t]+{name}"
        rf"[ \t]*\([^;{{}}]*\))[ \t\r\n]*\{{"
    )
    matches = list(pattern.finditer(source))
    if len(matches) != 1:
        raise RuntimeError(
            f"expected one definition for {name}, found {len(matches)}"
        )
    match = matches[0]
    signature = re.sub(r"\s+", " ", match.group("signature")).strip()
    if signature not in EXPECTED_SIGNATURES[name]:
        allowed = " or ".join(sorted(EXPECTED_SIGNATURES[name]))
        raise RuntimeError(
            f"unexpected {name} signature {signature!r}; expected {allowed!r}"
        )
    body_end = find_body_end(source, match.end() - 1, name)
    return source[match.start() : body_end].strip()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source",
        type=Path,
        default=DEFAULT_SOURCE,
        help="RISC-V relocation source (defaults to this checkout)",
    )
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()

    source_path = args.source.resolve()
    source = source_path.read_text()
    template = TEMPLATE.read_text()
    if template.count(MARKER) != 1:
        raise RuntimeError(f"expected one extraction marker in {TEMPLATE}")
    functions = "\n\n".join(
        extract_function(source, name)
        for name in ("checkInt", "read32le", "write32le", "setUType")
    )
    args.output.write_text(template.replace(MARKER, functions))
    digest = hashlib.sha256(source_path.read_bytes()).hexdigest()
    print(f"[riscv-u-type] source={source_path}")
    print(f"[riscv-u-type] source_sha256={digest}")
    print(f"[riscv-u-type] generated={args.output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError) as error:
        raise SystemExit(f"riscv U-type source extraction failed: {error}") from error
