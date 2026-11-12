"""Independent arm64 Mach-O field checks using original metadata and layout."""

import hashlib
import struct
from macho_metadata import require, bounded


def address(value):
    require(isinstance(value, str) and value.startswith('0x'), 'missing layout address')
    return int(value, 16)


def signed(value, bits):
    return (value & ((1 << bits) - 1)) - ((value & (1 << (bits - 1))) << 1)


def page(value):
    return value & ~0xfff


def branch_word(word, target, place):
    delta = target - place
    if delta % 4 or not -(1 << 27) <= delta < (1 << 27):
        return None
    return (word & ~0x03ffffff) | ((delta // 4) & 0x03ffffff)


def pageoff_scale(word):
    if word & 0x7f000000 == 0x11000000:  # ADD immediate; PAGEOFF uses the unshifted form.
        require(word & (1 << 22) == 0, 'shifted ADD PAGEOFF instruction')
        return 0
    require(word & 0x3b000000 == 0x39000000, 'unsupported PAGEOFF load/store instruction')
    scale = word >> 30
    if word & (1 << 26) and word & (1 << 23):
        require(scale == 0, 'invalid 128-bit load/store size')
        return 4
    return scale


def stub_target(data):
    """Decode the MOVZ/MOVK sequence ending in BR x16, without trusting metadata S."""
    require(len(data) == 20, 'unexpected native stub size')
    words = struct.unpack('<5I', data)
    value = 0
    for i, word in enumerate(words[:4]):
        require(word & 0xff80001f == (0xd2800010 if i == 0 else 0xf2800010),
                'invalid MOVZ/MOVK stub opcode or register')
        shift = ((word >> 21) & 3) * 16
        value = (value & ~(0xffff << shift)) | (((word >> 5) & 0xffff) << shift)
    require(words[4] == 0xd61f0200, 'stub does not branch through x16')
    return value


def symbol_value(symbol, metadata, capture):
    name = symbol['name']
    if symbol['type'] == 0xe:
        ordinal = symbol['section'] - 1
        require(0 <= ordinal < len(metadata['sections']), 'invalid symbol section')
        source = metadata['sections'][ordinal]
        value = address(capture['sections'][ordinal]['mapped_start']) + symbol['value'] - source['address']
        return value, dict(kind='section', index=ordinal, offset=symbol['value'] - source['address'])
    if symbol['type'] == 2:
        return symbol['value'], dict(kind='absolute', value=symbol['value'])
    require(symbol['type'] == 0 and symbol['value'] == 0, 'symbol needs unavailable COMMON/alias layout')
    if name == '___dso_handle':
        return address(capture['image_base']), dict(kind='object-image-base')
    require(name in capture['host_symbols'], 'missing independent host symbol: ' + name)
    return address(capture['host_symbols'][name]), dict(kind='host-symbol', name=name)


def got_value(capture, slot, expected):
    got = capture['got']
    require(got is not None and got['capture_phase'] == 'after-resolve-before-init', 'missing pre-init GOT')
    offset = slot - address(got['address'])
    data = bytes.fromhex(got['bytes'])
    require(len(data) == got['size'] and offset % 8 == 0, 'invalid GOT layout')
    require(int.from_bytes(bounded(data, offset, 8), 'little') == expected, 'GOT target differs from independent S')
    return dict(kind='got', offset=offset)


def verify(metadata, capture, original):
    require(capture['format'] == 'Mach-O' and capture['host'] == 'arm64-apple-darwin', 'wrong capture target')
    require(bytes.fromhex(capture['object_bytes']) == original and capture['input_size'] == len(original),
            'capture object differs from original file')
    sections = metadata['sections']
    require(capture['section_count'] == len(sections) == len(capture['sections']), 'capture section count differs')
    fields, semantic, used_stubs = [], [], set()
    for section, live in zip(sections, capture['sections']):
        index = section['index']
        require(live['index'] == index and live['size'] == section['size'], 'capture section layout differs')
        if section['size']:
            require(live['name'] == section['name'] and live['segment'] == section['segment']
                    and address(live['vmaddr']) == section['address'], 'capture section identity differs')
        require(bytes.fromhex(live['pre_resolve_bytes']) == section['bytes'], 'capture before bytes differ from input')
        require(live['relocation_count'] == len(section['relocations']) == len(live['relocations']),
                'capture relocation count differs')
        expected_section = bytearray(section['bytes'])
        pending = None
        for i, (reloc, observed) in enumerate(zip(section['relocations'], live['relocations'])):
            kind, number, offset = reloc['type'], reloc['type_number'], reloc['offset']
            place = address(live['mapped_start']) + offset
            require(observed['type'] == kind and observed['type_number'] == number
                    and observed['section_index'] == index and observed['relocation_index'] == i,
                    'capture relocation identity differs')
            raw = observed['raw']
            require(raw == dict(r_address=offset, r_symbolnum=reloc['symbol_index'], r_pcrel=reloc['pcrel'],
                    r_length=reloc['length'], r_extern=int(reloc['external']), r_type=number,
                    record_bytes=reloc['record_bytes']), 'capture raw relocation differs from original')
            require(address(observed['P']) == place, 'capture P differs from layout')
            width = 0 if number == 10 else 1 << reloc['length']
            before = bounded(section['bytes'], offset, width)
            require(observed['before'] == before.hex() and observed['A']['implicit_field_width'] == width
                    and observed['A']['implicit_field_bytes'] == (before.hex() if width else None),
                    'capture implicit addend bytes differ')
            if number == 10:
                require(pending is None and i + 1 < len(section['relocations']), 'unpaired ADDEND')
                following = section['relocations'][i + 1]
                require(following['type_number'] in (2, 3, 4) and following['offset'] == offset,
                        'invalid ADDEND pairing')
                pending = signed(reloc['symbol_index'], 24)
                require(observed['A']['explicit_addend'] == pending
                        and observed['A']['paired_relocation_index'] == i + 1
                        and observed['S'] is None and observed['after'] == '', 'capture ADDEND differs')
                semantic.append(dict(section=index, offset=offset, type=kind, A=pending))
                fields.append(dict(section=index, offset=offset, type=kind, P=hex(place), A=pending,
                                   before='', expected='', after=''))
                continue
            require(observed['A']['explicit_addend'] == pending, 'capture explicit addend differs')
            if pending is not None:
                require(observed['A']['from_record_index'] == i - 1, 'capture addend pairing differs')
            explicit = pending or 0
            pending = None
            require(reloc['external'], 'local relocation not yet covered by native oracle')
            require(0 <= reloc['symbol_index'] < len(metadata['symbols']), 'invalid symbol index')
            symbol = metadata['symbols'][reloc['symbol_index']]
            target, symbol_ref = symbol_value(symbol, metadata, capture)
            require(observed['symbol'] == symbol['name'] and observed['S'] is not None, 'capture symbol differs')
            require(address(observed['S']['observed']) == target, 'observed S differs from independent symbol')
            if symbol_ref['kind'] == 'host-symbol':
                require(observed['S']['input_source'] == 'host-symbol-map'
                        and address(observed['S']['input']) == target, 'capture host symbol provenance differs')
            elif symbol_ref['kind'] == 'object-image-base':
                require(observed['S']['input_source'] == 'object-image-base'
                        and address(observed['S']['input']) == target, 'capture image provenance differs')
            else:
                require(observed['S']['input'] is None and observed['S']['input_source'] == 'not-host-supplied',
                        'capture object symbol provenance differs')
            slot = address(observed['got_slot']) if observed['got_slot'] is not None else None
            slot_ref = got_value(capture, slot, target) if slot is not None else None
            word = int.from_bytes(before, 'little')
            destination = symbol_ref
            if number == 0:
                require(width in (4, 8) and not reloc['pcrel'] and not explicit, 'invalid UNSIGNED relocation')
                addend = signed(word, width * 8)
                value = target + addend
                require(0 <= value < 1 << (width * 8), 'UNSIGNED field overflow')
            else:
                require(width == 4, 'instruction relocation is not 32-bit')
                if number == 2:
                    require(word & 0x7c000000 == 0x14000000 and reloc['pcrel'], 'invalid BRANCH26 instruction')
                    addend = signed(word & 0x03ffffff, 26) * 4 + explicit
                    choices = {}
                    direct = branch_word(word, target + addend, place)
                    if direct is not None:
                        choices[direct] = symbol_ref
                    for reservation in capture['stubs']:
                        require(reservation['count'] == len(reservation['entries']), 'stub count differs')
                        require(reservation['capture_phase'] == 'after-resolve-before-init', 'wrong stub capture phase')
                        data = bytes.fromhex(reservation['reserved_bytes'])
                        require(len(data) == reservation['reserved_size'], 'stub reservation size differs')
                        for entry in reservation['entries']:
                            start = address(entry['address'])
                            offset_in_reservation = start - address(reservation['reserved_start'])
                            decoded = stub_target(bounded(data, offset_in_reservation, 20))
                            require(address(entry['target']) == decoded, 'stub target metadata differs from code')
                            if decoded == target + addend:
                                encoded = branch_word(word, start, place)
                                require(encoded is not None, 'branch cannot reach stub')
                                choices[encoded] = dict(kind='stub', section=reservation['section_index'],
                                    offset=start - address(capture['sections'][reservation['section_index']]['mapped_start']))
                    actual = int.from_bytes(bytes.fromhex(observed['after']), 'little')
                    require(actual in choices, 'BRANCH26 does not reach independent target or valid stub')
                    value, destination = actual, choices[actual]
                    if destination['kind'] == 'stub':
                        used_stubs.add((destination['section'], destination['offset']))
                elif number in (3, 5):
                    require(word & 0x9f000000 == 0x90000000 and reloc['pcrel'], 'invalid PAGE21 instruction')
                    implicit = signed(((word >> 29) & 3) | (((word >> 5) & 0x7ffff) << 2), 21) << 12
                    addend = implicit + explicit
                    if number == 5:
                        require(slot is not None and addend == 0, 'invalid GOT PAGE21 addend/layout')
                        endpoint, destination = slot, slot_ref
                    else:
                        endpoint = target + addend
                    delta = (page(endpoint) - page(place)) // 4096
                    require(-(1 << 20) <= delta < (1 << 20), 'PAGE21 displacement overflow')
                    value = (word & ~0x60ffffe0) | ((delta & 3) << 29) | (((delta >> 2) & 0x7ffff) << 5)
                elif number in (4, 6):
                    require(not reloc['pcrel'], 'PC-relative PAGEOFF12')
                    scale = pageoff_scale(word)
                    addend = ((word >> 10) & 0xfff) * (1 << scale) + explicit
                    if number == 6:
                        require(slot is not None and addend == 0, 'invalid GOT PAGEOFF addend/layout')
                        endpoint, destination = slot, slot_ref
                    else:
                        endpoint = target + addend
                    low = endpoint & 0xfff
                    require(low % (1 << scale) == 0, 'unaligned PAGEOFF target')
                    value = (word & ~0x003ffc00) | ((low >> scale) << 10)
                else:
                    raise ValueError('unimplemented oracle relocation: ' + kind)
            expected = value.to_bytes(width, 'little')
            require(observed['after'] == expected.hex(), 'relocated field differs from ABI: ' + kind)
            expected_section[offset:offset + width] = expected
            fields.append(dict(section=index, offset=offset, type=kind, symbol=symbol['name'], S=hex(target),
                               P=hex(place), A=addend, before=before.hex(), expected=expected.hex(), after=observed['after']))
            semantic.append(dict(section=index, offset=offset, type=kind, symbol=symbol['name'],
                                 S=symbol_ref, A=addend, before=before.hex(), destination=destination))
        require(pending is None, 'trailing ADDEND')
        require(bytes.fromhex(live['pre_init_relocated_bytes']) == expected_section,
                'section bytes differ outside checked relocation fields')
    for reservation in capture['stubs']:
        for entry in reservation['entries']:
            index = reservation['section_index']
            offset = address(entry['address']) - address(capture['sections'][index]['mapped_start'])
            require((index, offset) in used_stubs, 'unverified emitted stub')
    layout = dict(sections=[dict(index=s['index'], address=s['mapped_start'], size=s['size'],
                                relocated_bytes=s['pre_init_relocated_bytes'])
                            for s in capture['sections']], host_symbols=capture['host_symbols'],
                  image_base=capture['image_base'], got=capture['got'], stubs=capture['stubs'])
    return dict(schema=1, format='Mach-O arm64', object_sha256=hashlib.sha256(original).hexdigest(),
                reference_layout=layout, fields=fields, semantic=semantic,
                got_slots={r['symbol']: r['got_slot'] for s in capture['sections'] for r in s['relocations']
                           if r['symbol'] is not None and r['got_slot'] is not None})
