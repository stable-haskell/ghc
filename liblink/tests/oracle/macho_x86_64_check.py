#!/usr/bin/env python3
"""Check ordinary x86_64 Mach-O captures against original records and layout."""
import hashlib
import json
from pathlib import Path
import struct

from macho_metadata import bounded, read_object, require

NAMES = {0: 'X86_64_RELOC_UNSIGNED', 1: 'X86_64_RELOC_SIGNED',
         2: 'X86_64_RELOC_BRANCH', 3: 'X86_64_RELOC_GOT_LOAD',
         4: 'X86_64_RELOC_GOT', 6: 'X86_64_RELOC_SIGNED_1',
         7: 'X86_64_RELOC_SIGNED_2', 8: 'X86_64_RELOC_SIGNED_4'}
ADJUST = {6: 1, 7: 2, 8: 4}


def verify(path, capture, readobj):
    original = Path(path).read_bytes()
    meta, _ = read_object(path, readobj, '64-bit x86-64')
    require(capture['object_bytes'] == original.hex() and capture['input_size'] == len(original),
            'capture object differs')
    require(capture['format'] == 'Mach-O' and capture['host'] == 'x86_64-apple-darwin',
            'capture target differs')
    require(len(capture['sections']) == len(meta['sections']) == capture['section_count'],
            'section count differs')
    extras = capture['extras']
    require(extras['stride'] == 16 and extras['count'] == len(meta['symbols']),
            'extra layout differs')
    extra_address = int(extras['address'], 0)
    expected_extras = bytearray(extras['count'] * 16)
    signature = []
    for section, sample in zip(meta['sections'], capture['sections']):
        if section['size'] == 0:
            require(sample['index'] == section['index'] and sample['size'] == 0 and
                    not section['relocations'] and not sample['relocations'],
                    'empty section differs')
            continue
        require(section['index'] == sample['index'] and section['size'] == sample['size'] and
                section['name'] == sample['name'] and section['segment'] == sample['segment'],
                'section metadata differs')
        before = section['bytes']
        require(sample['pre_resolve_bytes'] == before.hex(), 'section input differs')
        expected = bytearray(before)
        address = int(sample['mapped_start'], 0)
        require(len(sample['relocations']) == len(section['relocations']), 'relocation count differs')
        for record, observed in zip(section['relocations'], sample['relocations']):
            kind, index, offset = record['type_number'], record['symbol_index'], record['offset']
            require(kind in NAMES and record['external'] and index < len(meta['symbols']),
                    'unsupported relocation')
            symbol = meta['symbols'][index]
            require(symbol['name'] in capture['host_symbols'] and symbol['type'] == 0,
                    'unsupported symbol')
            target = int(capture['host_symbols'][symbol['name']], 0)
            width = 1 << record['length']
            addend = int.from_bytes(bounded(before, offset, width), 'little')
            place = address + offset
            require(observed['type'] == NAMES[kind] and observed['P'] == hex(place) and
                    observed['raw']['record_bytes'] == record['record_bytes'] and
                    observed['before'] == before[offset:offset+width].hex() and
                    observed['symbol'] == symbol['name'] and observed['S']['input'] == hex(target),
                    'relocation capture differs from object or host inputs')
            destination = target
            slot = None
            if kind in (3, 4) or (kind == 2 and not -(1 << 31) <= target - (place + width) < (1 << 31)):
                slot = index
                payload = struct.pack('<Q', target) + bytes.fromhex('ff25f2ffffff0000')
                expected_extras[index*16:(index+1)*16] = payload
                destination = extra_address + index*16 + (8 if kind == 2 else 0)
            if kind == 0:
                require(not record['pcrel'], 'absolute field is PC-relative')
                value = addend + destination
            else:
                require(record['pcrel'] and width == 4, 'invalid PC-relative field')
                value = addend + destination - (place + width + ADJUST.get(kind, 0))
            encoded = (value % (1 << (8*width))).to_bytes(width, 'little')
            require(observed['after'] == encoded.hex(), 'relocation field differs')
            expected[offset:offset+width] = encoded
            signature.append(dict(section=section['index'], offset=offset, type=NAMES[kind],
                                  symbol=symbol['name'], addend=hex(addend), extra=slot))
        require(sample['pre_init_relocated_bytes'] == expected.hex(), 'relocated section differs')
    require(extras['bytes'] == expected_extras.hex(), 'extra payload differs')
    return {'object_sha256': hashlib.sha256(original).hexdigest(), 'semantic': signature}


def compare(path, capture, golden, readobj):
    reference = json.loads(Path(golden).read_text())
    require(verify(path, reference['capture'], readobj) == reference['checked'],
            'saved reference differs')
    require(verify(path, capture, readobj) == reference['checked'], 'capture signature differs')
