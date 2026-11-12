"""Check real normal-mode Thumb call destinations, including a far veneer."""

import argparse
import json
import shlex
import struct
import subprocess


def displacement(instruction):
    upper, lower = struct.unpack('<HH', bytes.fromhex(instruction))
    sign = (upper >> 10) & 1
    j1, j2 = (lower >> 13) & 1, (lower >> 11) & 1
    i1, i2 = (~(j1 ^ sign)) & 1, (~(j2 ^ sign)) & 1
    value = ((sign << 24) | (i1 << 23) | (i2 << 22) |
             ((upper & 0x3ff) << 12) | ((lower & 0x7ff) << 1))
    return value - (1 << 25) if sign else value


def check(command, fixture, far):
    args = shlex.split(command) + (['--far-function'] if far else []) + [fixture]
    capture = json.loads(subprocess.run(args, check=True, capture_output=True,
                                        text=True, timeout=30).stdout)
    section = next(s for s in capture['sections'] if s['before'] == 'fff7feff')
    target = int(capture['host_symbols']['external_function'], 0)
    place = int(section['address'], 0)
    entries = section['stubs']['entries']
    destination = (place + 4 + displacement(section['after'])) & 0xffffffff
    if far:
        assert len(entries) == 1 and entries[0]['flags'] == 1
        assert int(entries[0]['target'], 0) == target
        assert destination == int(entries[0]['address'], 0)
    else:
        assert not entries
        assert destination == target


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--capture', required=True, help='capture command with optional runner')
    parser.add_argument('--fixture', required=True)
    args = parser.parse_args()
    check(args.capture, args.fixture, False)
    check(args.capture, args.fixture, True)


if __name__ == '__main__':
    main()
