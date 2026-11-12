"""Read Mach-O metadata with LLVM and bind it to the original object bytes."""

from pathlib import Path
import re
import struct
import subprocess


class OracleError(ValueError):
    pass


def require(condition, message):
    if not condition:
        raise OracleError(message)


def integer(value):
    return int(value, 0)


def group(text, heading):
    match = re.search(r"^" + re.escape(heading) + r" \[\n(.*?)^\]$", text, re.M | re.S)
    require(match is not None, "missing LLVM " + heading)
    return match[1]


def blocks(text, label, indent=2):
    prefix = " " * indent
    return re.findall(r"^" + prefix + re.escape(label) + r" \{\n(.*?)^" + prefix + r"\}$",
                      text, re.M | re.S)


def fields(block):
    return dict(re.findall(r"^\s+([A-Za-z0-9]+): (.*)$", block, re.M))


def named_number(value):
    match = re.fullmatch(r"(.*?) \((0x[0-9A-Fa-f]+|[0-9]+)\)", value)
    require(match is not None, "invalid LLVM named number: " + value)
    return match[1], integer(match[2])


def fixed_name(value):
    return value.rsplit(" (", 1)[0]


def parse_reader(text, arch='arm64'):
    require(f"Format: Mach-O {arch}\n" in text, "oracle requires Mach-O " + arch)
    sections = []
    for block in blocks(group(text, "Sections"), "Section"):
        f = fields(block)
        sections.append(dict(index=integer(f['Index']), name=fixed_name(f['Name']),
                             segment=fixed_name(f['Segment']), address=integer(f['Address']),
                             size=integer(f['Size']), offset=integer(f['Offset']),
                             relocation_offset=integer(f['RelocationOffset']),
                             relocation_count=integer(f['RelocationCount']),
                             type=named_number(f['Type'])[1], relocations=[]))
    require([s['index'] for s in sections] == list(range(len(sections))),
            "nonsequential LLVM section indices")
    symbols = []
    for block in blocks(group(text, "Symbols"), "Symbol"):
        f = fields(block)
        name, string_index = named_number(f['Name'])
        symbols.append(dict(name=name, string_index=string_index,
                            type=named_number(f['Type'])[1],
                            section=named_number(f['Section'])[1],
                            value=integer(f['Value']), external='    Extern\n' in block))
    groups = re.findall(r"^  Section (.*?) \{\n(.*?)^  \}$", group(text, "Relocations"), re.M | re.S)
    expected = [s for s in sections if s['relocation_count']]
    require(len(groups) == len(expected), "LLVM relocation section count differs")
    for (name, body), section in zip(groups, expected):
        require(name == section['name'], "LLVM relocation section order differs")
        for block in blocks(body, "Relocation", 4):
            f = fields(block)
            kind, number = named_number(f['Type'])
            external = 'Symbol' in f
            target_name, target_index = named_number(f['Symbol' if external else 'Section'])
            section['relocations'].append(dict(offset=integer(f['Offset']),
                pcrel=integer(f['PCRel']), length=integer(f['Length']), type=kind,
                type_number=number, external=external, symbol_index=target_index,
                target_name=target_name))
        require(len(section['relocations']) == section['relocation_count'],
                "LLVM relocation count differs")
    return dict(sections=sections, symbols=symbols)


def bounded(data, offset, size):
    require(offset >= 0 and size >= 0 and offset + size <= len(data), "object extent out of bounds")
    return data[offset:offset + size]


def bind_original(metadata, data, arch='arm64'):
    """Cross-check LLVM's interpretation; retain payloads from the original file."""
    require(len(data) >= 32, "short Mach-O header")
    magic, cpu, subtype, kind, count, command_size, flags, reserved = struct.unpack_from('<8I', data)
    cpus = {'arm64': 0x100000c, '64-bit x86-64': 0x1000007}
    require(arch in cpus and magic == 0xfeedfacf and cpu == cpus[arch] and kind == 1,
            "wrong Mach-O object header")
    offset, raw_sections, symtab = 32, [], None
    for unused in range(count):
        command, size = struct.unpack('<2I', bounded(data, offset, 8))
        require(size >= 8, "short load command")
        bounded(data, offset, size)
        if command == 0x19:
            nsections = struct.unpack('<I', bounded(data, offset + 64, 4))[0]
            require(size == 72 + nsections * 80, "invalid section command size")
            for i in range(nsections):
                raw_sections.append(struct.unpack('<16s16sQQ8I', bounded(data, offset + 72 + i * 80, 80)))
        elif command == 2:
            symtab = struct.unpack('<4I', bounded(data, offset + 8, 16))
        offset += size
    require(offset == 32 + command_size, "load command extent differs")
    require(len(raw_sections) == len(metadata['sections']), "section count differs from LLVM")
    for section, raw in zip(metadata['sections'], raw_sections):
        name, segment, address, size, payload, alignment, relocations, nreloc, flags, r1, r2, r3 = raw
        values = dict(name=name.split(b'\0')[0].decode(), segment=segment.split(b'\0')[0].decode(),
                      address=address, size=size, offset=payload, relocation_offset=relocations,
                      relocation_count=nreloc, type=flags & 0xff)
        require(all(section[k] == v for k, v in values.items()), "section metadata differs from LLVM")
        section['bytes'] = bytes(size) if section['type'] in (1, 0xc, 0x12) else bounded(data, payload, size)
        for i, relocation in enumerate(section['relocations']):
            record = bounded(data, relocations + 8 * i, 8)
            site, bits = struct.unpack('<iI', record)
            values = dict(offset=site, symbol_index=bits & 0xffffff, pcrel=(bits >> 24) & 1,
                          length=(bits >> 25) & 3, external=bool((bits >> 27) & 1), type_number=bits >> 28)
            require(all(relocation[k] == v for k, v in values.items()), "relocation metadata differs from LLVM")
            relocation['record_bytes'] = record.hex()
    require(symtab is not None, "no original symbol table")
    symoff, nsyms, stroff, strsize = symtab
    strings = bounded(data, stroff, strsize)
    require(nsyms == len(metadata['symbols']), "symbol count differs from LLVM")
    for i, symbol in enumerate(metadata['symbols']):
        name, kind, section, description, value = struct.unpack('<IBBHQ', bounded(data, symoff + 16 * i, 16))
        require(name < len(strings) and b'\0' in strings[name:], "invalid symbol string")
        require(symbol == dict(name=strings[name:].split(b'\0')[0].decode(), string_index=name,
                type=kind & 0xe, section=section, value=value, external=bool(kind & 1)),
                "symbol metadata differs from LLVM")
    return metadata


def read_object(path, readobj, arch='arm64'):
    version = subprocess.run([str(readobj), '--version'], capture_output=True, text=True, timeout=30)
    match = re.search(r'LLVM version ([0-9]+)', version.stdout)
    require(version.returncode == 0 and match and int(match[1]) >= 17, "LLVM >= 17 is required")
    argv = [str(readobj), '--file-headers', '--sections', '--symbols', '--relocations', '--expand-relocs', str(path)]
    result = subprocess.run(argv, capture_output=True, text=True, timeout=30)
    require(result.returncode == 0 and not result.stderr, "LLVM object inspection failed: " + result.stderr)
    return bind_original(parse_reader(result.stdout, arch), Path(path).read_bytes(), arch), dict(
        argv=argv, exit=result.returncode, stdout=result.stdout, stderr=result.stderr, version=version.stdout)
