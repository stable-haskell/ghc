#!/usr/bin/env python3
"""Check ordinary ELF AArch64 fields, GOT values and branch veneers.

Formulas: Arm AAELF64 sections 5.7.5–5.7.7, 2025Q4:
https://github.com/ARM-software/abi-aa/blob/main/aaelf64/aaelf64.rst
Original file records and explicit resolver inputs determine symbol values;
sampled addresses supply layout only. No linker implementation is imported.
"""

import argparse
import hashlib
import json
from pathlib import Path
import struct
import sys

from elf64_metadata import bounded, parse, require, string, unpack


MASK64 = (1 << 64) - 1
NAMES = {257: 'ABS64', 261: 'PREL32', 275: 'ADR_PREL_PG_HI21',
         277: 'ADD_ABS_LO12_NC', 278: 'LDST8_ABS_LO12_NC', 282: 'JUMP26',
         283: 'CALL26', 284: 'LDST16_ABS_LO12_NC', 285: 'LDST32_ABS_LO12_NC',
         286: 'LDST64_ABS_LO12_NC', 299: 'LDST128_ABS_LO12_NC',
         311: 'ADR_GOT_PAGE', 312: 'LD64_GOT_LO12_NC'}
SCALES = {278: 0, 284: 1, 285: 2, 286: 3, 299: 4, 312: 3}


def signed64(value):
    value &= MASK64
    return value if value < 1 << 63 else value - (1 << 64)


def insert_fields(instruction, value, fields):
    for destination, width, source in fields:
        mask = (1 << width) - 1
        instruction = (instruction & ~(mask << destination) |
                       ((value >> source) & mask) << destination)
    return instruction


def veneer(target):
    # MOVZ x16 at halfword 3, MOVK at 2/1/0, then BR x16. Validate this
    # independent encoding against the assembler in the oracle tests.
    words = [0xd2e00010, 0xf2c00010, 0xf2a00010, 0xf2800010]
    words = [insert_fields(word, target, [(5, 16, halfword * 16)])
             for word, halfword in zip(words, (3, 2, 1, 0))]
    return struct.pack('<5I', *words, 0xd61f0200)


def verify(original, capture):
    sections = parse(original, 183)
    require(capture['schema'] == 'liblink-elf-native-address-v1', 'wrong capture schema')
    require(capture['resolved'] is True and capture['status'] == 0 and
            capture['error'] == '' and capture['initializers_ran'] is False,
            'capture did not resolve successfully without initializers')
    noexec = capture.get('noexec', False)
    loaded, expected, stub_expected, stub_entries = {}, {}, {}, {}
    section_signature = []
    for sample in capture['sections']:
        index = sample['index']
        require(0 < index < len(sections) and index not in loaded,
                'duplicate or invalid captured section index')
        section = sections[index]
        before, after = bytes.fromhex(sample['before']), bytes.fromhex(sample['after'])
        require(sample['size'] == section['size'] == len(before) == len(after) and
                before == section['bytes'], 'pre-relocation section differs from original')
        work = int(sample['address'], 0)
        address = int(sample['exec_address'], 0)
        if noexec:
            require(address != work and address % 4096 == 0,
                    'NO_EXEC section did not separate work and execution layout')
        else:
            require(address == work, 'normal-mode section work and execution addresses differ')
        loaded[index] = (address, after, sample['stubs'])
        expected[index] = bytearray(before)
        stubs = sample['stubs']
        stub_before, stub_after = bytes.fromhex(stubs['before']), bytes.fromhex(stubs['after'])
        require(stubs['size'] == len(stub_before) == len(stub_after),
                'stub storage extent differs')
        if stubs['size']:
            tail_offset = ((section['size'] + 3) & ~3) if noexec else section['size']
            require(int(stubs['address'], 0) == work + tail_offset,
                    'stub storage is not at the expected section offset')
        stub_expected[index] = bytearray(stub_before)
        stub_entries[index] = []
        section_signature.append(dict(index=index, name=section['name'], size=len(before)))

    require(set(loaded) == {i for i, section in enumerate(sections)
                            if i != 0 and section['type'] != 0 and section['size'] != 0},
            'capture omitted or added a nonempty original section')

    tables = [i for i, section in enumerate(sections) if section['type'] == 2]
    require(len(tables) == 1, 'baseline oracle requires exactly one symbol table')

    def symbol(table_index, index):
        require(table_index in tables, 'invalid symbol table selection')
        table = sections[table_index]
        require(table['entry_size'] == 24 and table['size'] % 24 == 0 and
                index < table['size'] // 24, 'unsupported symbol table record')
        name, info, _, shndx, value, _ = unpack('<IBBHQQ', table['bytes'], index * 24)
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

    got = capture['got']
    got_address = int(got['exec_address'], 0)
    got_work = int(got['address'], 0)
    if noexec:
        require(got_address != got_work and got_address % 4096 == 0,
                'NO_EXEC GOT did not separate work and execution layout')
    else:
        require(got_address == got_work and got_address % 8 == 0,
                'normal GOT layout differs')
    got_slots, got_values = {}, []
    table_index = tables[0]
    table = sections[table_index]
    for index in range(table['size'] // 24):
        name, info, _, shndx, _, _ = unpack('<IBBHQQ', table['bytes'], index * 24)
        kind, binding = info & 15, info >> 4
        local_definition = (binding == 0 and name != 0 and
                            0 < shndx < 0xff00 and kind in (0, 1, 2))
        if binding in (1, 2) or local_definition or kind == 3:
            got_slots[table_index, index] = len(got_values)
            got_values.append(symbol(table_index, index)[2])
    got_bytes = b''.join(struct.pack('<Q', value) for value in got_values)
    require(got['size'] == len(got_bytes) and bytes.fromhex(got['after']) == got_bytes,
            'global GOT values differ from original symbols/resolver inputs')
    extras = capture['extras']
    require(extras['stride'] == 0 and extras['first'] == 0 and
            extras['count'] == table['size'] // 24 and
            extras['before'] == extras['after'] == '', 'AArch64 extras metadata changed')

    fields, semantic = [], []
    branch_counts = {index: 0 for index in loaded}
    for reloc_section in sections:
        if reloc_section['type'] != 4:
            continue
        target = reloc_section['info']
        require(target in loaded and reloc_section['entry_size'] == 24 and
                reloc_section['size'] % 24 == 0, 'unsupported RELA target/table')
        for record in range(reloc_section['size'] // 24):
            offset, info, addend = unpack('<QQq', reloc_section['bytes'], record * 24)
            kind, index = info & 0xffffffff, info >> 32
            require(kind in NAMES, f'unsupported oracle relocation: {kind}')
            name, raw_type, S, origin = symbol(reloc_section['link'], index)
            P = loaded[target][0] + offset
            width = 8 if kind == 257 else 4
            before = bounded(sections[target]['bytes'], offset, width)
            instruction = int.from_bytes(before, 'little')
            value = (S + addend) & MASK64
            slot = stub_ordinal = G = None
            if kind == 257:
                encoded = value
            elif kind == 261:
                value = signed64(value - P)
                require(-(1 << 31) <= value < 1 << 31, 'PREL32 result out of range')
                encoded = value & 0xffffffff
            elif kind in (282, 283):
                branch_counts[target] += 1
                expected_opcode = 0x14000000 if kind == 282 else 0x94000000
                require(instruction >> 26 == expected_opcode >> 26, 'wrong branch instruction')
                value = signed64(value - P)
                if not -(1 << 27) <= value < 1 << 27:
                    require(addend == 0, 'baseline veneer oracle requires a zero branch addend')
                    entries = stub_entries[target]
                    stub_ordinal = next((i for i, entry in enumerate(entries)
                                         if entry['target'] == hex(S)), len(entries))
                    stub_work = int(loaded[target][2]['address'], 0) + stub_ordinal * 20
                    section_work = next(int(sample['address'], 0) for sample in capture['sections']
                                        if sample['index'] == target)
                    address = loaded[target][0] + stub_work - section_work
                    if stub_ordinal == len(entries):
                        entries.append(dict(address=hex(stub_work), target=hex(S), flags=0))
                        payload = veneer(S)
                        bounded(stub_expected[target], stub_ordinal * 20, len(payload))
                        stub_expected[target][stub_ordinal * 20:(stub_ordinal + 1) * 20] = payload
                    value = signed64(address - P)
                require(value % 4 == 0 and -(1 << 27) <= value < 1 << 27,
                        'branch destination out of range or unaligned')
                encoded = insert_fields(instruction, value >> 2, [(0, 26, 0)])
            else:
                if kind in (311, 312):
                    require(addend == 0, 'GOT relocation addend is not zero')
                    require((reloc_section['link'], index) in got_slots, 'symbol has no GOT slot')
                    slot = got_slots[reloc_section['link'], index]
                    G = got_address + slot * 8
                    value = G
                if kind in (275, 311):
                    require(instruction & 0x9f000000 == 0x90000000, 'wrong ADRP instruction')
                    value = signed64((value & ~0xfff) - (P & ~0xfff))
                    require(-(1 << 32) <= value < 1 << 32, 'ADRP result out of range')
                    encoded = insert_fields(instruction, value >> 12,
                                            [(29, 2, 0), (5, 19, 2)])
                elif kind == 277:
                    require(instruction & 0x7fc00000 == 0x11000000, 'wrong unshifted ADD instruction')
                    value &= 0xfff
                    encoded = insert_fields(instruction, value, [(10, 12, 0)])
                else:
                    scale = SCALES[kind]
                    require(instruction & 0x3b000000 == 0x39000000, 'wrong load/store instruction')
                    instruction_scale = instruction >> 30 & 3
                    if instruction_scale == 0 and instruction & 0x04800000 == 0x04800000:
                        instruction_scale = 4
                    require(scale == instruction_scale and value % (1 << scale) == 0,
                            'load/store scale or target alignment differs')
                    value &= 0xfff
                    encoded = insert_fields(instruction, value >> scale, [(10, 12, 0)])
            payload = encoded.to_bytes(width, 'little')
            expected[target][offset:offset + width] = payload
            semantic.append(dict(section=target, offset=offset, type='R_AARCH64_' + NAMES[kind],
                                 symbol=name, symbol_index=index, symbol_table=reloc_section['link'],
                                 raw_symbol_type=raw_type, S_origin=origin, A=addend,
                                 width=width, got_slot=slot, stub_ordinal=stub_ordinal))
            fields.append(dict(section=target, offset=offset, type='R_AARCH64_' + NAMES[kind],
                               P=hex(P), S=hex(S), A=addend, GOT_S=None if G is None else hex(G),
                               value=value, before=before.hex(), expected=payload.hex(),
                               after=bounded(loaded[target][1], offset, width).hex()))
    require(fields, 'no supported relocation fields checked')
    for index, data in expected.items():
        require(data == loaded[index][1], f'relocated or untouched bytes differ in section {index}')
        stubs = loaded[index][2]
        require(stubs['size'] == branch_counts[index] * 20, 'stub capacity changed')
        require(stub_expected[index] == bytes.fromhex(stubs['after']),
                'veneer or untouched stub bytes differ')
        require(stub_entries[index] == stubs['entries'], 'stub identity/order/target differs')
    report = dict(schema=1, format='ELF AArch64', object_sha256=hashlib.sha256(original).hexdigest(),
                  semantic=dict(sections=section_signature, relocations=semantic,
                                extras={key: extras[key] for key in ('first', 'count', 'stride')}),
                  fields=fields, reference_capture=capture)
    if noexec:
        report['noexec_image'] = dict(
            sections=[dict(index=i, exec_address=hex(address),
                           sha256=hashlib.sha256(after).hexdigest(),
                           stubs_sha256=hashlib.sha256(bytes.fromhex(stubs['after'])).hexdigest())
                      for i, (address, after, stubs) in loaded.items()],
            got=dict(exec_address=hex(got_address), sha256=hashlib.sha256(got_bytes).hexdigest()))
    return report


def compare_golden(report, golden, original):
    replay = verify(original, golden['reference_capture'])
    for key in ('schema', 'format', 'object_sha256', 'semantic', 'fields'):
        require(replay[key] == golden[key], 'saved reference differs: ' + key)
    require(replay.get('noexec_image') == golden.get('noexec_image'),
            'saved NO_EXEC image differs')
    for key in ('schema', 'format', 'object_sha256', 'semantic'):
        require(report[key] == golden[key], 'golden differs: ' + key)
    require(report.get('noexec_image') == golden.get('noexec_image'),
            'golden differs: NO_EXEC image')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--object', type=Path, required=True)
    parser.add_argument('--capture', type=Path, required=True)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument('--golden', type=Path)
    mode.add_argument('--write-golden', type=Path)
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
        print(f"ELF AArch64 oracle: PASS ({len(report['fields'])} fields, all section/GOT/stub bytes)")
        return 0
    except (ValueError, KeyError, TypeError, OSError, struct.error) as error:
        print('ELF AArch64 oracle: FAIL: ' + str(error), file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
