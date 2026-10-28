"""Check ELF32 fixture relocations from original records and ABI formulas.

ARM: https://github.com/ARM-software/abi-aa/blob/main/aaelf32/aaelf32.rst
i386: System V ABI Intel386 supplement, relocation table.
This oracle accepts one ordinary REL record and one symbol table per object.
"""

import argparse
import hashlib
import json
from pathlib import Path
import struct

from elf32_metadata import parse
from elf64_metadata import bounded, require, string, unpack


MASK = 0xffffffff
NAMES = {3: {1: '32', 2: 'PC32', 3: 'GOT32', 10: 'GOTPC', 43: 'GOT32X'},
         40: {2: 'ABS32', 3: 'REL32', 10: 'THM_CALL', 28: 'CALL',
              29: 'JUMP24', 43: 'MOVW_ABS_NC', 44: 'MOVT_ABS'}}


def signed(value, bits):
    value &= (1 << bits) - 1
    return value - (1 << bits) if value & (1 << (bits - 1)) else value


def thumb_veneer(target):
    def instruction(opcode, immediate):
        return (opcode | ((immediate & 0xf000) << 4) |
                ((immediate & 0x0800) << 15) | ((immediate & 0x0700) << 4) |
                (immediate & 0xff))
    movw = instruction(0xf2400c00, target & 0xffff)
    movt = instruction(0xf2c00c00, target >> 16)
    return struct.pack('<6H', movw >> 16, movw & 0xffff,
                       movt >> 16, movt & 0xffff, 0xbf00, 0x4760)


def arm_veneer(target):
    def instruction(opcode, immediate):
        return opcode | ((immediate & 0xf000) << 4) | (immediate & 0xfff)
    return struct.pack('<III', instruction(0xe300c000, target & 0xffff),
                       instruction(0xe340c000, target >> 16), 0xe12fff1c)


def verify(original, capture, machine):
    sections = parse(original, machine)
    require(capture['schema'] == 'liblink-elf-native-address-v1' and
            capture['resolved'] is True and capture['status'] == 0 and
            capture['error'] == '' and not capture['initializers_ran'], 'capture failed')
    noexec = capture['noexec']
    loaded, expected = {}, {}
    for sample in capture['sections']:
        i = sample['index']
        require(0 < i < len(sections) and i not in loaded, 'invalid section index')
        before, after = bytes.fromhex(sample['before']), bytes.fromhex(sample['after'])
        require(before == sections[i]['bytes'] and len(after) == len(before) == sample['size'],
                'section input differs')
        work, address = int(sample['address'], 0), int(sample['exec_address'], 0)
        require((work != address and address % 4096 == 0) if noexec else work == address,
                'section work/exec layout differs')
        loaded[i] = sample
        expected[i] = bytearray(before)
    require(set(loaded) == {i for i, s in enumerate(sections)
                            if i and s['type'] and s['size']}, 'missing section')
    resolver = capture['host_symbols']
    anchor = int(capture['sections'][0]['exec_address'], 0)
    function = int(resolver['external_function'], 0)
    require(int(resolver['external_data'], 0) == 0x12345000 and
            function in (anchor + 0x1000, 0x70000000) and
            int(resolver['external_function_alias'], 0) == function and
            int(resolver['external_second_function'], 0) == function + 0x1000,
            'resolver inputs differ from fixture policy')
    symtabs = [i for i, s in enumerate(sections) if s['type'] == 2]
    require(len(symtabs) == 1, 'requires one symbol table')
    table = sections[symtabs[0]]
    require(table['entry_size'] == 16 and table['size'] % 16 == 0, 'invalid symbol table')
    got = capture['got']
    got_exec = int(got['exec_address'], 0)

    def symbol(index):
        require(index < table['size'] // 16, 'invalid symbol index')
        name, value, _, info, _, shndx = unpack('<IIIBBH', table['bytes'], index * 16)
        name = string(sections[table['link']]['bytes'], name)
        if shndx == 0:
            if name == '_GLOBAL_OFFSET_TABLE_':
                return name, info, got_exec
            require(name in capture['host_symbols'], 'unknown resolver input')
            return name, info, int(capture['host_symbols'][name], 0)
        require(shndx in loaded, 'uncaptured defined symbol')
        return name, info, (int(loaded[shndx]['exec_address'], 0) + value) & MASK

    slots, values = {}, []
    for i in range(1, table['size'] // 16):
        name, value, _, info, _, shndx = unpack('<IIIBBH', table['bytes'], i * 16)
        binding, kind = info >> 4, info & 15
        local = binding == 0 and name and 0 < shndx < 0xff00 and kind in (0, 1, 2)
        if binding in (1, 2) or local or kind == 3:
            slots[i] = len(values)
            values.append(symbol(i)[2])
    got_bytes = b''.join(struct.pack('<I', v) for v in values)
    require(got['size'] == len(got_bytes) and bytes.fromhex(got['after']) == got_bytes,
            'GOT payload differs')
    require((got_exec != int(got['address'], 0)) if noexec else
            (got_exec == int(got['address'], 0)), 'GOT work/exec layout differs')
    extras = capture['extras']
    require(extras['before'] == extras['after'] and
            len(bytes.fromhex(extras['after'])) == extras['stride'] * extras['count'],
            'symbol extras changed')
    rels = [s for s in sections if s['type'] == 9]
    require(len(rels) == 1 and rels[0]['entry_size'] == 8 and rels[0]['size'] == 8,
            'requires one REL record')
    rel = rels[0]
    require(rel['link'] == symtabs[0] and rel['info'] in loaded, 'invalid REL table')
    offset, info = unpack('<II', rel['bytes'], 0)
    kind, index = info & 255, info >> 8
    require(kind in NAMES[machine], 'unsupported relocation')
    name, sym_info, S = symbol(index)
    sample = loaded[rel['info']]
    P = (int(sample['exec_address'], 0) + offset) & MASK
    raw = bounded(sections[rel['info']]['bytes'], offset, 4)
    word = int.from_bytes(raw, 'little')
    A, slot, veneer = signed(word, 32), None, None
    if machine == 3:
        if kind == 1: result = S + A
        elif kind == 2: result = S + A - P
        elif kind in (3, 43):
            require(index in slots, 'missing GOT slot')
            slot = slots[index]
            result = slot * 4 + A
        else: result = got_exec + A - P
        output = struct.pack('<I', result & MASK)
    elif kind in (2, 3):
        result = S + A - (P if kind == 3 else 0)
        output = struct.pack('<I', result & MASK)
    elif kind in (43, 44):
        addend = ((word >> 16) & 15) << 12 | (word & 0xfff)
        result = (S + addend) & MASK
        immediate = (result >> 16 if kind == 44 else result) & 0xffff
        output = struct.pack('<I', (word & ~0xf0fff) |
                             ((immediate & 0xf000) << 4) | (immediate & 0xfff))
        A = addend
    else:
        stubs = sample['stubs']
        stub_addr = int(stubs['address'], 0)
        stub_exec = (int(sample['exec_address'], 0) + stub_addr - int(sample['address'], 0)) & MASK
        if kind in (28, 29):
            A = signed((word & 0xffffff) << 2, 26)
            delta = signed(S + A - P, 32)
            if not -(1 << 25) <= delta < (1 << 25):
                veneer = dict(address=hex(stub_addr), target=hex((S + A + 8) & MASK), flags=0)
                delta = signed(stub_exec - 8 - P, 32)
            output = struct.pack('<I', word & 0xff000000 | ((delta >> 2) & 0xffffff))
        else:
            upper, lower = struct.unpack('<HH', raw)
            sign = upper >> 10 & 1
            i1 = 1 ^ (lower >> 13 & 1) ^ sign
            i2 = 1 ^ (lower >> 11 & 1) ^ sign
            A = signed((sign << 24) | (i1 << 23) | (i2 << 22) |
                       ((upper & 1023) << 12) | ((lower & 2047) << 1), 25)
            delta = signed(S + A - P, 32)
            if not -(1 << 24) <= delta < (1 << 24):
                veneer = dict(address=hex(stub_addr), target=hex((S + A + 4) & MASK), flags=1)
                delta = signed(stub_exec - 4 - P, 32)
                thumb = 1
            else:
                delta &= ~3
                thumb = 0
            sign = (delta >> 24) & 1
            j1 = 1 ^ ((delta >> 23) & 1) ^ sign
            j2 = 1 ^ ((delta >> 22) & 1) ^ sign
            output = struct.pack('<HH', (upper & 0xf800) | (sign << 10) | ((delta >> 12) & 1023),
                                 (lower & 0xd000) | (j1 << 13) | (thumb << 12) |
                                 (j2 << 11) | ((delta >> 1) & 2047))
    expected[rel['info']][offset:offset + 4] = output
    for i, data in expected.items():
        require(data == bytes.fromhex(loaded[i]['after']), f'section {i} differs')
        if machine == 40:
            stubs = loaded[i]['stubs']
            original_stubs = bytearray.fromhex(stubs['before'])
            entries = []
            if i == rel['info'] and veneer:
                payload = thumb_veneer(int(veneer['target'], 0)) if veneer['flags'] else arm_veneer(int(veneer['target'], 0))
                require(len(original_stubs) >= len(payload), 'stub storage too small')
                original_stubs[:len(payload)] = payload
                entries = [veneer]
            require(bytes(original_stubs) == bytes.fromhex(stubs['after']) and
                    entries == stubs['entries'], 'stub bytes or identity differ')
    semantic = dict(section=rel['info'], offset=offset, symbol=name, type=NAMES[machine][kind],
                    addend=A, got_slot=slot, veneer=bool(veneer))
    return dict(schema=1, machine=machine, object_sha256=hashlib.sha256(original).hexdigest(),
                semantic=semantic, field=dict(P=hex(P), S=hex(S), before=raw.hex(), after=output.hex()),
                reference_capture=capture)


def compare_golden(original, report, golden):
    replay = verify(original, golden['reference_capture'], golden['machine'])
    for key in ('schema', 'machine', 'object_sha256', 'semantic', 'field'):
        require(replay[key] == golden[key], 'saved reference differs: ' + key)
    for key in ('schema', 'machine', 'object_sha256', 'semantic'):
        require(report[key] == golden[key], 'golden differs: ' + key)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--machine', type=int, choices=(3, 40), required=True)
    p.add_argument('--object', type=Path, required=True)
    p.add_argument('--capture', type=Path, required=True)
    p.add_argument('--write-golden', type=Path, required=True)
    a = p.parse_args()
    report = verify(a.object.read_bytes(), json.loads(a.capture.read_text()), a.machine)
    a.write_golden.parent.mkdir(parents=True, exist_ok=True)
    with a.write_golden.open('x') as out:
        json.dump(report, out, indent=2, sort_keys=True)
        out.write('\n')


if __name__ == '__main__':
    main()
