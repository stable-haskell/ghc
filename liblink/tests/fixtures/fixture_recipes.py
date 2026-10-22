"""Narrow deterministic COFF transformation for forms LLVM cannot emit.

These are fixture encoders, not object loaders. Unexpected layouts are rejected.
"""

from __future__ import annotations

import struct


class RecipeError(ValueError):
    pass


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise RecipeError(message)


def coff_rel32_variant(data: bytes, variant: int) -> bytes:
    """Change one AMD64 REL32 to REL32_N, preserving its target expression.

    LLVM 21 X86WinCOFFObjectWriter emits REL32, not REL32_1 through REL32_5.
    Increasing the inline addend by N compensates for the extra PC bias N.
    """
    _require(type(variant) is int and 1 <= variant <= 5, "REL32 variant must be 1..5")
    _require(len(data) >= 20, "truncated COFF header")
    machine, sections, _, symbols, count, optional, _ = struct.unpack_from("<HHIIIHH", data)
    _require(machine == 0x8664 and optional == 0 and sections > 0,
             "expected ordinary AMD64 COFF without optional header")
    end_headers = 20 + sections * 40
    _require(end_headers <= len(data), "truncated COFF section table")
    _require(symbols >= end_headers and count > 0 and symbols + count * 18 + 4 <= len(data),
             "invalid COFF symbol table")
    string_size = struct.unpack_from("<I", data, symbols + count * 18)[0]
    _require(string_size >= 4 and symbols + count * 18 + string_size == len(data),
             "invalid COFF string table")
    primary_symbols: set[int] = set()
    index = 0
    while index < count:
        primary_symbols.add(index)
        aux = data[symbols + index * 18 + 17]
        _require(index + aux < count, "truncated COFF auxiliary symbol records")
        index += 1 + aux

    candidates: list[tuple[int, int]] = []
    regions = [(0, end_headers), (symbols, len(data))]
    for number in range(sections):
        header = 20 + number * 40
        physical, virtual, size, raw, relocs, lines, nrelocs, nlines, flags = struct.unpack_from("<IIIIIIHHI", data, header + 8)
        _require(physical == 0 and virtual == 0 and lines == 0 and nlines == 0 and not flags & 0x01000000,
                 "unsupported COFF section address, line table or extended relocations")
        if raw:
            _require(raw >= end_headers and raw + size <= symbols, "invalid COFF section data bounds")
            regions.append((raw, raw + size))
        else:
            _require(nrelocs == 0, "relocations in section without raw data")
        if nrelocs:
            _require(relocs >= end_headers and relocs + nrelocs * 10 <= symbols,
                     "invalid COFF relocation table bounds")
            regions.append((relocs, relocs + nrelocs * 10))
        for i in range(nrelocs):
            record = relocs + i * 10
            offset, symbol, kind = struct.unpack_from("<IIH", data, record)
            _require(symbol in primary_symbols, "relocation references invalid or auxiliary symbol")
            _require(offset < size, "relocation site outside section")
            if kind == 4:
                _require(offset + 4 + variant <= size, "REL32 variant lacks inline field or trailing bytes")
                candidates.append((record, raw + offset))
    occupied = sorted((lo, hi) for lo, hi in regions if lo != hi)
    _require(all(a[1] <= b[0] for a, b in zip(occupied, occupied[1:])), "overlapping COFF regions")
    _require(len(candidates) == 1, "expected exactly one AMD64 REL32 relocation")
    record, site = candidates[0]
    addend = struct.unpack_from("<i", data, site)[0]
    _require(addend + variant <= 0x7fffffff, "REL32 adjusted addend overflows signed 32 bits")
    result = bytearray(data)
    struct.pack_into("<H", result, record + 8, 4 + variant)
    struct.pack_into("<i", result, site, addend + variant)
    return bytes(result)
