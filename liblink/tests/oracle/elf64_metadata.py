"""Bounded original ELF64 little-endian metadata for relocation oracles."""

import struct


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


def parse(original, machine):
    require(bounded(original, 0, 7) == b'\x7fELF\x02\x01\x01',
            'oracle requires ELF64 little-endian version 1')
    require(unpack('<HH', original, 16) == (1, machine),
            f'oracle requires a machine-{machine} relocatable object')
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


