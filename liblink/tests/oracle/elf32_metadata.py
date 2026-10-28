"""Bounded original ELF32 little-endian metadata for relocation oracles."""

from elf64_metadata import bounded, require, string, unpack


def parse(original, machine):
    require(bounded(original, 0, 7) == b'\x7fELF\x01\x01\x01',
            'oracle requires ELF32 little-endian version 1')
    require(unpack('<HH', original, 16) == (1, machine),
            f'oracle requires a machine-{machine} relocatable object')
    shoff, = unpack('<I', original, 32)
    entry_size, count, names_index = unpack('<HHH', original, 46)
    require(entry_size == 40 and 0 < count < 0xff00 and names_index < count,
            'unsupported section table layout')
    sections = []
    for i in range(count):
        values = unpack('<IIIIIIIIII', original, shoff + i * entry_size)
        section = dict(zip(('name_offset', 'type', 'flags', 'address', 'offset',
                            'size', 'link', 'info', 'align', 'entry_size'), values))
        section['bytes'] = (bytes(section['size']) if section['type'] == 8 else
                            bounded(original, section['offset'], section['size']))
        sections.append(section)
    for section in sections:
        section['name'] = string(sections[names_index]['bytes'], section['name_offset'])
    return sections
