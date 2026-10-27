#!/usr/bin/env python3
"""Check PE x64 capture fields and untouched bytes against COFF records."""
import argparse
import hashlib
import json
from pathlib import Path
import struct
import sys


def verify(data, capture):
    machine, sections, _, symoff, symcount, optional, _ = struct.unpack_from('<HHIIIHH', data)
    if machine != 0x8664 or optional != 0:
        raise ValueError('not a supported AMD64 COFF object')
    found = []
    for index in range(sections):
        pos = 20 + index * 40
        name, _, vaddr, size, raw, reloff, _, nreloc, _, _ = struct.unpack_from('<8sIIIIIIHHI', data, pos)
        if nreloc:
            if nreloc != 1 or raw + size > len(data) or reloff + 10 > len(data):
                raise ValueError('unsupported section or relocation layout')
            offset, symbol, kind = struct.unpack_from('<IIH', data, reloff)
            if symbol >= symcount or symoff + 18 * (symbol + 1) > len(data):
                raise ValueError('relocation symbol out of bounds')
            symbol_name = data[symoff + 18 * symbol:symoff + 18 * symbol + 8]
            if symbol_name[:4] == b'\0\0\0\0':
                strbase = symoff + 18 * symcount
                stroff = struct.unpack_from('<I', symbol_name, 4)[0]
                strsize = struct.unpack_from('<I', data, strbase)[0]
                if stroff < 4 or stroff >= strsize or strbase + strsize > len(data):
                    raise ValueError('invalid symbol string offset')
                end = data.find(b'\0', strbase + stroff, strbase + strsize)
                if end < 0:
                    raise ValueError('unterminated symbol')
                symbol_name = data[strbase + stroff:end]
            else:
                symbol_name = symbol_name.rstrip(b'\0')
            found.append((name.rstrip(b'\0').decode(), vaddr, size, raw, offset, kind, symbol_name))
    if len(found) != 1:
        raise ValueError('expected one relocated section')
    name, vaddr, size, raw, virtual, kind, symbol_name = found[0]
    if symbol_name != b'll_fixture_target':
        raise ValueError('unexpected relocation target')
    if kind not in (1, 3, 4, 11) or capture['type'] != kind:
        raise ValueError('unexpected relocation kind')
    offset = virtual - vaddr
    width = 8 if kind == 1 else 4
    if offset < 0 or offset + width > size or offset != capture['offset']:
        raise ValueError('invalid relocation offset')
    before = bytes.fromhex(capture['before'])
    after = bytes.fromhex(capture['after'])
    if len(before) != size or len(after) != size or before != data[raw:raw + size]:
        raise ValueError('capture section differs from object')
    section = int(capture['section_address'], 0)
    target = int(capture['target'], 0)
    base = int(capture['image_base'], 0)
    if target != (0x123456789abcdef0 if kind == 1 else section + 0x100):
        raise ValueError('unexpected resolver input')
    if base != target - 0x200:
        raise ValueError('unexpected image-base input')
    addend = int.from_bytes(before[offset:offset + width], 'little')
    if kind == 4 and addend & (1 << 31):
        addend -= 1 << 32
    result = target + addend
    if kind == 3:
        result -= base
    elif kind == 4:
        result -= section + offset + 4
    elif kind == 11:
        result -= section
    if kind != 1 and not -(1 << 31) <= result < (1 << 31):
        raise ValueError('field is out of range')
    expected = bytearray(before)
    expected[offset:offset + width] = (result % (1 << (8 * width))).to_bytes(width, 'little')
    if expected != after:
        raise ValueError('relocated or untouched bytes differ')
    return {'schema': 1, 'format': 'COFF AMD64', 'object_sha256': hashlib.sha256(data).hexdigest(),
            'section': name, 'type': kind, 'offset': offset, 'before': before.hex(),
            'after': after.hex(), 'reference_capture': capture}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--object', required=True, type=Path)
    parser.add_argument('--capture', required=True, type=Path)
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--golden', type=Path)
    group.add_argument('--write-golden', type=Path)
    args = parser.parse_args()
    try:
        data = args.object.read_bytes()
        report = verify(data, json.loads(args.capture.read_text()))
        if args.golden:
            saved = json.loads(args.golden.read_text())
            if verify(data, saved['reference_capture']) != saved:
                raise ValueError('saved reference does not replay')
            for key in ('schema', 'format', 'object_sha256', 'section', 'type', 'offset', 'before', 'after'):
                if report[key] != saved[key]:
                    raise ValueError('golden differs: ' + key)
        else:
            args.write_golden.parent.mkdir(parents=True, exist_ok=True)
            with args.write_golden.open('x') as output:
                json.dump(report, output, indent=2, sort_keys=True)
                output.write('\n')
        print('COFF AMD64 oracle: PASS')
    except (OSError, ValueError, KeyError, TypeError, struct.error) as error:
        print('COFF AMD64 oracle: FAIL: ' + str(error), file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
