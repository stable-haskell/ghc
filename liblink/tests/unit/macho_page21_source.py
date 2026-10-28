#!/usr/bin/env python3
"""Extract the current PAGE21 field writer for a portable integer unit."""
from __future__ import annotations

import argparse
import hashlib
from pathlib import Path
import textwrap

ROOT = Path(__file__).resolve().parents[3]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path,
                        default=ROOT / 'liblink/src/linker/MachO.c')
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    source = args.source.read_text()
    start = source.index('static bool\nencodeAddend(')
    start = source.index('        case LL_MACHO_ARM64_RELOC_PAGE21:', start)
    end = source.index('        case LL_MACHO_ARM64_RELOC_PAGEOFF12:', start)
    case = source[start:end]
    if case.count('case LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGE21:') != 1:
        raise RuntimeError('PAGE21 and GOT_LOAD_PAGE21 no longer share this writer')
    marker = '            const uint64_t addend_bits = (uint64_t)addend;'
    if case.count(marker) != 1 or case.count('*p = (*p & 0x9F00001F)') != 1:
        raise RuntimeError('PAGE21 writer changed; review the extraction seam')
    first = case.index(marker)
    last = case.index('            return true;', first)
    body = textwrap.dedent(case[first:last]).rstrip()
    output = ('/* Extracted PAGE21 writer; source SHA256 '
              + hashlib.sha256(source.encode()).hexdigest() + '. */\n'
              '#include <stdint.h>\n\n'
              'static uint32_t macho_page21_encode(uint32_t instruction, int64_t addend)\n'
              '{\n    uint32_t *p = &instruction;\n'
              + textwrap.indent(body, '    ') + '\n    return instruction;\n}\n')
    args.output.write_text(output)


if __name__ == '__main__':
    main()
