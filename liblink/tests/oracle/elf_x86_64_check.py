#!/usr/bin/env python3
"""Check native ELF x86_64 captures against original RELA records and formulas.

Arithmetic: AMD64 psABI 1.0, table 4.9 and section 4.4.1:
https://gitlab.com/x86-psABIs/x86-64-ABI/-/jobs/artifacts/master/raw/x86-64-ABI/abi.pdf?job=build
The native GHC extras layout is checked separately: one 8-byte target followed
by `jmp *-14(%rip)` and two zero pad bytes. No backend code is imported.
This oracle covers the nine supported ordinary fixtures, not TLS or NO_EXEC.
"""

import argparse
import hashlib
import json
from pathlib import Path
import struct
import sys


MASK64 = (1 << 64) - 1
NAMES = {0: 'R_X86_64_NONE', 1: 'R_X86_64_64', 2: 'R_X86_64_PC32',
         4: 'R_X86_64_PLT32', 9: 'R_X86_64_GOTPCREL', 10: 'R_X86_64_32',
         11: 'R_X86_64_32S', 24: 'R_X86_64_PC64', 41: 'R_X86_64_GOTPCRELX',
         42: 'R_X86_64_REX_GOTPCRELX'}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def bounded(data, offset, size):
    require(offset >= 0 and size >= 0 and offset + size <= len(data),
            f'input range outside buffer: offset={offset}, size={size}')
    return data[offset:offset + size]


def unpack(layout, data, offset):
    return struct.unpack(layout, bounded(data, offset, struct.calcsize(layout)))


def string(data, offset):
    require(0 <= offset < len(data), 'string offset outside table')
    end = data.find(b'\0', offset)
    require(end >= 0, 'unterminated table string')
    return data[offset:end].decode('utf-8')


def parse(original):
    require(bounded(original, 0, 7) == b'\x7fELF\x02\x01\x01',
            'oracle requires ELF64 little-endian version 1')
    require(unpack('<HH', original, 16) == (1, 62),
            'oracle requires an x86_64 relocatable object')
    shoff, = unpack('<Q', original, 40)
    entry_size, count, names_index = unpack('<HHH', original, 58)
    require(entry_size == 64 and 0 < count < 0xff00 and names_index < count,
            'unsupported section table layout')
    sections = []
    for i in range(count):
        values = unpack('<IIQQQQIIQQ', original, shoff + i * entry_size)
        section = dict(zip(('name_offset', 'type', 'flags', 'address', 'offset',
                            'size', 'link', 'info', 'align', 'entry_size'), values))
        section['bytes'] = (bytes(section['size']) if section['type'] == 8 else
                            bounded(original, section['offset'], section['size']))
        sections.append(section)
    for section in sections:
        section['name'] = string(sections[names_index]['bytes'], section['name_offset'])
    return sections


def signed64(value):
    value &= MASK64
    return value if value < (1 << 63) else value - (1 << 64)


def fits32(value):
    return -(1 << 31) <= signed64(value) < (1 << 31)


def verify(original, capture):
    sections = parse(original)
    require(capture['schema'] == 'liblink-elf-native-address-v1', 'wrong capture schema')
    require(capture['resolved'] is True and capture['status'] == 0 and
            capture['error'] == '' and capture['initializers_ran'] is False,
            'capture did not resolve successfully without initializers')
    loaded = {}
    expected = {}
    section_signature = []
    for sample in capture['sections']:
        index = sample['index']
        require(0 < index < len(sections) and index not in loaded,
                'duplicate or invalid captured section index')
        section = sections[index]
        before, after = bytes.fromhex(sample['before']), bytes.fromhex(sample['after'])
        require(sample['size'] == section['size'] == len(before) == len(after) and
                before == section['bytes'], 'pre-relocation section differs from original')
        loaded[index] = (int(sample['address'], 0), after)
        expected[index] = bytearray(before)
        section_signature.append(dict(index=index, name=section['name'], size=len(before)))

    extras = capture['extras']
    require(extras['stride'] == 16 and extras['jump_offset'] == 8,
            'native x64 extras layout changed')
    extra_before, extra_after = bytes.fromhex(extras['before']), bytes.fromhex(extras['after'])
    require(extras['first'] >= 0 and extras['count'] >= 0 and
            len(extra_before) == len(extra_after) == extras['count'] * 16 and
            not any(extra_before), 'extras initial state or captured extent changed')
    extra_expected = bytearray(extra_before)
    extras_address = int(extras['address'], 0)
    semantic, fields = [], []

    def symbol(symtab_index, symbol_index):
        require(0 <= symtab_index < len(sections), 'symbol table index outside sections')
        table = sections[symtab_index]
        require(table['type'] == 2 and table['entry_size'] == 24 and
                table['size'] % 24 == 0, 'unsupported symbol table')
        name, info, _, shndx, value, _ = unpack('<IBBHQQ', table['bytes'], symbol_index * 24)
        require(table['link'] < len(sections), 'symbol strings index outside sections')
        name = string(sections[table['link']]['bytes'], name)
        if shndx == 0:
            require(name in capture['host_symbols'], 'missing independent host symbol: ' + name)
            address = int(capture['host_symbols'][name], 0)
            origin = dict(kind='host-symbol', name=name)
        elif shndx == 0xfff1:
            address, origin = value, dict(kind='absolute', value=hex(value))
        else:
            require(shndx in loaded, 'defined symbol section was not captured')
            address = loaded[shndx][0] + value
            origin = dict(kind='section', index=shndx, offset=value)
        return name, info & 15, address & MASK64, origin

    def extra_slot(index, target):
        slot = index - extras['first']
        require(0 <= slot < extras['count'], 'relocation symbol has no extra slot')
        # Independent instruction encoding: FF /4, RIP-relative disp32 = -14.
        payload = struct.pack('<Q', target) + b'\xff\x25\xf2\xff\xff\xff\0\0'
        extra_expected[slot * 16:(slot + 1) * 16] = payload
        return extras_address + slot * 16

    for reloc_section in sections:
        if reloc_section['type'] != 4:
            continue
        target_index = reloc_section['info']
        require(target_index in loaded, 'relocation target was not captured')
        require(reloc_section['entry_size'] == 24 and reloc_section['size'] % 24 == 0,
                'unsupported RELA table layout')
        for record in range(reloc_section['size'] // 24):
            offset, info, addend = unpack('<QQq', reloc_section['bytes'], record * 24)
            kind, index = info & 0xffffffff, info >> 32
            require(kind in NAMES, f'unsupported oracle relocation: {kind}')
            if kind == 0:
                # The ordinary manifest has no NONE. Do not read symbol 0 for it.
                semantic.append(dict(section=target_index, offset=offset, type=NAMES[kind]))
                continue
            name, raw_type, S, origin = symbol(reloc_section['link'], index)
            P = loaded[target_index][0] + offset
            slot = None
            value = (S + addend) & MASK64
            if kind in (2, 4, 24):
                value = (value - P) & MASK64
                if kind == 4 and not fits32(value):
                    L = extra_slot(index, S) + 8
                    value = (L + addend - P) & MASK64
                    slot = index
                if kind in (2, 4):
                    require(fits32(value), 'PC-relative displacement does not fit signed 32 bits')
            elif kind in (9, 41, 42):
                G = extra_slot(index, S)
                value = (G + addend - P) & MASK64
                slot = index
                require(fits32(value), 'GOT displacement does not fit signed 32 bits')
            elif kind == 10:
                require(value <= 0xffffffff, 'R_X86_64_32 does not zero-extend')
            elif kind == 11:
                require(fits32(value), 'R_X86_64_32S does not sign-extend')
            width = 8 if kind in (1, 24) else 4
            before = bounded(sections[target_index]['bytes'], offset, width)
            payload = (value & ((1 << (width * 8)) - 1)).to_bytes(width, 'little')
            expected[target_index][offset:offset + width] = payload
            semantic.append(dict(section=target_index, offset=offset, type=NAMES[kind],
                                 symbol=name, symbol_index=index, raw_symbol_type=raw_type,
                                 S_origin=origin, A=addend, width=width, extra_symbol_index=slot))
            fields.append(dict(section=target_index, offset=offset, type=NAMES[kind],
                               P=hex(P), S=hex(S), A=addend, before=before.hex(),
                               expected=payload.hex(),
                               after=bounded(loaded[target_index][1], offset, width).hex()))
    require(fields, 'no supported relocation fields checked')
    for index, data in expected.items():
        require(data == loaded[index][1],
                f'relocated or untouched bytes differ in section {index}')
    require(extra_expected == extra_after, 'GOT/jump-island or untouched extras bytes differ')
    return dict(schema=1, format='ELF x86_64', object_sha256=hashlib.sha256(original).hexdigest(),
                semantic=dict(sections=section_signature, relocations=semantic,
                              extras={key: extras[key] for key in ('first', 'count', 'stride')}),
                fields=fields, reference_capture=capture)


def compare_golden(report, golden, original):
    # Recheck the saved reference numerically, so stale or corrupt reference
    # fields cannot silently become expected values for a fresh observation.
    replay = verify(original, golden['reference_capture'])
    for key in ('schema', 'format', 'object_sha256', 'semantic', 'fields'):
        require(replay[key] == golden[key], 'saved reference differs: ' + key)
    for key in ('schema', 'format', 'object_sha256', 'semantic'):
        require(report[key] == golden[key], 'golden differs: ' + key)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--object', type=Path, required=True)
    parser.add_argument('--capture', type=Path, required=True)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument('--golden', type=Path)
    mode.add_argument('--write-golden', type=Path, help='create a new reference; refuse replacement')
    args = parser.parse_args()
    try:
        original = args.object.read_bytes()
        report = verify(original, json.loads(args.capture.read_text()))
        if args.golden:
            compare_golden(report, json.loads(args.golden.read_text()), original)
        if args.write_golden:
            args.write_golden.parent.mkdir(parents=True, exist_ok=True)
            with args.write_golden.open('x') as output:
                json.dump(report, output, indent=2, sort_keys=True)
                output.write('\n')
        print(f"ELF x86_64 oracle: PASS ({len(report['fields'])} fields, all section/extras bytes)")
        return 0
    except (ValueError, KeyError, TypeError, OSError, struct.error) as error:
        print('ELF x86_64 oracle: FAIL: ' + str(error), file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
