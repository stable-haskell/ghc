#!/usr/bin/env python3
"""Validate RISC-V ELF captures against original records and ABI encodings.

The current backend narrows relocation operands to 32 bits; this baseline
records that existing behavior while independently checking encoded fields.
"""

import hashlib
import struct

from elf64_metadata import bounded, parse, require, string, unpack


def signed32(value):
    value &= 0xffffffff
    return value if value < 0x80000000 else value - 0x100000000


def insert(word, value, fields):
    for dest, width, source in fields:
        mask = (1 << width) - 1
        word = (word & ~(mask << dest)) | (((value >> source) & mask) << dest)
    return word


def verify(original, capture):
    sections = parse(original, 243)
    require(capture['schema'] == 'liblink-elf-native-address-v1', 'wrong capture schema')
    require(capture['resolved'] and capture['status'] == 0 and not capture['error']
            and not capture['initializers_ran'], 'capture did not resolve cleanly')
    require(not capture.get('noexec', False), 'baseline oracle requires normal mode')
    loaded, expected, stubs = {}, {}, {}
    for sample in capture['sections']:
        index = sample['index']
        require(0 < index < len(sections) and index not in loaded, 'bad section index')
        before, after = bytes.fromhex(sample['before']), bytes.fromhex(sample['after'])
        require(sample['size'] == sections[index]['size'] == len(before) == len(after) and
                before == sections[index]['bytes'],
                'captured source section differs')
        require(int(sample['address'], 0) == int(sample['exec_address'], 0),
                'normal section work and execution addresses differ')
        loaded[index] = sample
        expected[index] = bytearray(before)
        stubs[index] = bytearray.fromhex(sample['stubs']['before'])
        require(sample['stubs']['size'] == len(stubs[index]) ==
                len(bytes.fromhex(sample['stubs']['after'])), 'stub size differs')
    require(set(loaded) == {i for i, section in enumerate(sections)
                           if i and section['type'] and section['size']},
            'capture omitted or added a nonempty section')

    def symbol(table, index):
        tab = sections[table]
        require(tab['type'] == 2 and tab['entry_size'] == 24, 'bad symbol table')
        name, info, _, shndx, value, _ = unpack('<IBBHQQ', tab['bytes'], index * 24)
        name = string(sections[tab['link']]['bytes'], name)
        if shndx == 0:
            require(name in capture['host_symbols'], 'unknown host symbol ' + name)
            value = int(capture['host_symbols'][name], 0)
        elif shndx != 0xfff1:
            require(shndx in loaded, 'symbol section missing')
            value += int(loaded[shndx]['exec_address'], 0)
        return name, info, value

    tables = [i for i, s in enumerate(sections) if s['type'] == 2]
    require(len(tables) == 1, 'baseline requires one symbol table')
    table = tables[0]
    slots, values = {}, []
    for index in range(sections[table]['size'] // 24):
        name, info, _, shndx, _, _ = unpack('<IBBHQQ', sections[table]['bytes'], index * 24)
        binding, kind = info >> 4, info & 15
        local = binding == 0 and name != 0 and 0 < shndx < 0xff00 and kind in (0, 1, 2)
        if binding in (1, 2) or local or kind == 3:
            slots[table, index] = len(values)
            values.append(symbol(table, index)[2])
    got = capture['got']
    require(int(got['address'], 0) == int(got['exec_address'], 0),
            'normal GOT work and execution addresses differ')
    require(got['size'] == 8 * len(values) and
            bytes.fromhex(got['after']) == b''.join(struct.pack('<Q', v) for v in values),
            'GOT values differ')
    extras = capture['extras']
    require(extras['stride'] == 8 and extras['first'] == 0 and
            extras['count'] == sections[table]['size'] // 24 and
            extras['before'] == extras['after'] and
            len(bytes.fromhex(extras['after'])) == extras['count'] * 8,
            'baseline extras differ')
    got_base = int(got['exec_address'], 0)
    fields = []
    expected_entries = {index: [] for index in loaded}
    stub_capacity = {index: 0 for index in loaded}
    for relsec in sections:
        if relsec['type'] != 4:
            continue
        target, symtab = relsec['info'], relsec['link']
        require(target in loaded and relsec['entry_size'] == 24 and
                relsec['size'] % 24 == 0, 'bad RELA table')
        records = [unpack('<QQq', relsec['bytes'], n)
                   for n in range(0, relsec['size'], 24)]
        site_base = int(loaded[target]['exec_address'], 0)
        for pos, (offset, info, addend) in enumerate(records):
            kind, index = info & 0xffffffff, info >> 32
            if kind in (43, 51):
                require(offset <= len(expected[target]), 'no-op relocation outside section')
                fields.append(dict(section=target, offset=offset, type=kind,
                                   symbol=None, before='', expected=''))
                continue
            name, _, S = symbol(symtab, index)
            P = site_base + offset
            G = got_base + 8 * slots[symtab, index] if (symtab, index) in slots else None
            if kind in (24, 25):
                paired = None
                for previous in range(pos, -1, -1):
                    hi_offset, hi_info, hi_addend = records[previous]
                    hi_type, hi_index = hi_info & 0xffffffff, hi_info >> 32
                    if site_base + hi_offset != S or hi_type not in (20, 23):
                        continue
                    _, _, hi_S = symbol(symtab, hi_index)
                    if hi_type == 20:
                        require((symtab, hi_index) in slots, 'baseline HI needs GOT slot')
                        hi_S = got_base + 8 * slots[symtab, hi_index]
                    paired = signed32(hi_S + hi_addend - (site_base + hi_offset))
                    break
                require(paired is not None, 'missing paired HI')
                value = paired
            elif kind in (16, 17, 23, 44, 45):
                value = signed32(S + addend - P)
            elif kind == 20:
                require(G is not None, 'baseline GOT slot missing')
                value = signed32(G + addend - P)
            elif kind == 19:
                stub_capacity[target] += 1
                entries = expected_entries[target]
                matches = [entry for entry in entries if entry['target'] == hex(S)]
                require(G is not None, 'baseline call needs GOT slot')
                if matches:
                    stub = matches[0]
                else:
                    work = int(loaded[target]['address'], 0) + sections[target]['size'] + 12 * len(entries)
                    stub = dict(address=hex(work), target=hex(S), flags=0, got_address=hex(G))
                    entries.append(stub)
                work = int(loaded[target]['address'], 0)
                stub_exec = site_base + int(stub['address'], 0) - work
                rel = signed32(int(stub['got_address'], 0) - stub_exec)
                hi, lo = (rel + 0x800) >> 12, rel & 0xfff
                veneer = struct.pack('<III', (hi << 12 | 0xf97) & 0xffffffff,
                                     (lo << 20 | 0xfb_f83), 0xf8067)
                location = int(stub['address'], 0) - int(loaded[target]['stubs']['address'], 0)
                bounded(stubs[target], location, len(veneer))
                stubs[target][location:location + len(veneer)] = veneer
                value = signed32(stub_exec + addend - P)
            else:
                value = signed32(S + addend)
            bits = {16: 13, 17: 21, 44: 9, 45: 12}.get(kind)
            if bits is not None:
                require(-(1 << (bits - 1)) <= value < 1 << (bits - 1),
                        'branch relocation out of range')
            widths = {2: 8, 33: 1, 34: 2, 35: 4, 36: 8, 37: 1, 38: 2,
                      39: 4, 40: 8, 52: 1, 44: 2, 45: 2, 19: 8}
            width = widths.get(kind, 4)
            before = bounded(expected[target], offset, width)
            word = int.from_bytes(before, 'little')
            if kind in (2, 33, 34, 35, 36, 37, 38, 39, 40, 52):
                if kind == 2: result = value
                elif kind == 52: result = (word & 0xc0) | ((word - value) & 0x3f)
                elif kind in (33, 34, 35, 36): result = word + value
                else: result = word - value
                result &= (1 << (8 * width)) - 1
            elif kind in (20, 23, 26):
                result = insert(word, (value + 0x800) >> 12, [(12, 20, 0)])
            elif kind in (24, 27):
                result = insert(word, value, [(20, 12, 0)])
            elif kind == 25:
                result = insert(word, value, [(25, 7, 5), (7, 5, 0)])
            elif kind == 16:
                result = insert(word, value, [(31, 1, 12), (25, 6, 5), (8, 4, 1), (7, 1, 11)])
            elif kind == 17:
                result = insert(word, value, [(31, 1, 20), (21, 10, 1), (20, 1, 11), (12, 8, 12)])
            elif kind == 44:
                result = insert(word, value, [(12, 1, 8), (10, 2, 3), (5, 2, 6), (3, 2, 1), (2, 1, 5)])
            elif kind == 45:
                result = insert(word, value, [(12, 1, 11), (11, 1, 4), (9, 2, 8), (8, 1, 10), (7, 1, 6), (6, 1, 7), (3, 3, 1), (2, 1, 5)])
            elif kind == 19:
                upper, lower = word & 0xffffffff, word >> 32
                upper = insert(upper, (value + 0x800) >> 12, [(12, 20, 0)])
                lower = insert(lower, value, [(20, 12, 0)])
                result = upper | lower << 32
            else:
                raise ValueError(f'unsupported baseline relocation {kind}')
            payload = result.to_bytes(width, 'little')
            expected[target][offset:offset + width] = payload
            fields.append(dict(section=target, offset=offset, type=kind, symbol=name,
                               before=before.hex(), expected=payload.hex()))
    require(fields, 'no checked relocation fields')
    for index, sample in loaded.items():
        require(expected[index] == bytes.fromhex(sample['after']), f'section {index} differs')
        require(sample['stubs']['size'] == stub_capacity[index] * 12,
                f'stub capacity {index} differs')
        if stub_capacity[index]:
            require(int(sample['stubs']['address'], 0) ==
                    int(sample['address'], 0) + sample['size'], 'stub placement differs')
        require(sample['stubs']['entries'] == expected_entries[index],
                f'stub list {index} differs')
        require(stubs[index] == bytes.fromhex(sample['stubs']['after']), f'stub {index} differs')
    return dict(object_sha256=hashlib.sha256(original).hexdigest(), fields=fields)
