#!/usr/bin/env python3
"""Write data-only native Linux x86_64 ELF resource-unit fixtures."""

from __future__ import annotations

import argparse
import hashlib
import json
import struct
from pathlib import Path


def fixture(shared_size: int, *, unique_size: int | None = None,
            zero: bool = False, ordinary: bool = False) -> bytes:
    names = b"\0.data\0.symtab\0.strtab\0.shstrtab\0"
    strings = b"\0elf_resource_shared\0elf_resource_unique\0elf_resource_zero\0"
    symbols = [bytes(24)]

    def symbol(name: bytes, size: int, section: int = 0xFFF2) -> bytes:
        return struct.pack("<IBBHQQ", strings.index(name + b"\0"), 0x11, 0,
                           section, 0 if section == 1 else 8, size)

    # The unique symbol precedes the conflicting shared symbol, so a failed
    # larger-COMMON load must undo an earlier publication from this object.
    if unique_size is not None:
        symbols.append(symbol(b"elf_resource_unique", unique_size))
    symbols.append(symbol(b"elf_resource_zero" if zero else b"elf_resource_shared",
                          shared_size, 1 if ordinary else 0xFFF2))
    symbol_bytes = b"".join(symbols)
    section_count = 5
    metadata_end = 64 + section_count * 64
    names_offset = metadata_end
    strings_offset = names_offset + len(names)
    symbols_offset = (strings_offset + len(strings) + 7) & ~7
    data_offset = (symbols_offset + len(symbol_bytes) + 15) & ~15
    image = bytearray(data_offset + 16)
    ident = b"\x7fELF\x02\x01\x01" + bytes(9)
    struct.pack_into("<16sHHIQQQIHHHHHH", image, 0, ident, 1, 62, 1,
                     0, 0, 64, 0, 64, 0, 0, 64, section_count, 4)

    def section(index: int, name: bytes, kind: int, flags: int, offset: int,
                size: int, link: int = 0, info: int = 0,
                alignment: int = 1, entry_size: int = 0) -> None:
        struct.pack_into("<IIQQQQIIQQ", image, 64 + index * 64,
                         names.index(name + b"\0"), kind, flags, 0,
                         offset, size, link, info, alignment, entry_size)

    section(1, b".data", 1, 3, data_offset, 16, alignment=8)
    section(2, b".symtab", 2, 0, symbols_offset, len(symbol_bytes),
            link=3, info=1, alignment=8, entry_size=24)
    section(3, b".strtab", 3, 0, strings_offset, len(strings))
    section(4, b".shstrtab", 3, 0, names_offset, len(names))
    image[names_offset:names_offset + len(names)] = names
    image[strings_offset:strings_offset + len(strings)] = strings
    image[symbols_offset:symbols_offset + len(symbol_bytes)] = symbol_bytes
    image[data_offset:data_offset + 16] = bytes(range(0x31, 0x41))
    return bytes(image)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    fixtures = {
        "shared64.o": fixture(64),
        "same.o": fixture(64),
        "shared32.o": fixture(32),
        "conflict.o": fixture(65, unique_size=32),
        "zero.o": fixture(0, zero=True),
        "got-only.o": fixture(8, ordinary=True),
        "sum-overflow.o": fixture((1 << 64) - 1, unique_size=64),
        "round-overflow.o": fixture((1 << 64) - 1),
    }
    manifest = {}
    for name, data in fixtures.items():
        (args.output / name).write_bytes(data)
        manifest[name] = {"size": len(data), "sha256": hashlib.sha256(data).hexdigest()}
    (args.output / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")


if __name__ == "__main__":
    main()
