"""Replay checked ELF32 normal-mode captures and reject corrupt observations."""
import copy
import json
import os
from pathlib import Path
import shlex
import struct
import subprocess
import unittest

from elf32_check import compare_golden, verify
from elf32_metadata import parse

ROOT = Path(__file__).resolve().parents[1]
CASES = {
    3: ('elf-i386', 'i386-linux-gnu', ['R_386_32', 'R_386_PC32', 'R_386_GOT32',
                                       'R_386_GOT32X', 'R_386_GOTPC']),
    40: ('elf-arm', 'armv7-linux-gnueabihf', ['R_ARM_ABS32', 'R_ARM_REL32',
         'R_ARM_CALL', 'R_ARM_CALL-far', 'R_ARM_JUMP24', 'R_ARM_JUMP24-far',
         'R_ARM_THM_CALL', 'R_ARM_THM_CALL-far', 'R_ARM_MOVW_ABS_NC', 'R_ARM_MOVT_ABS'])
}


def original(machine, name):
    directory, triple, _ = CASES[machine]
    return (ROOT / 'fixtures/generated' / directory / triple /
            (name.removesuffix('-far') + '.o')).read_bytes()


def golden(machine, name):
    return json.loads((ROOT / 'golden' / CASES[machine][0] / (name + '.json')).read_text())


class Elf32Oracle(unittest.TestCase):
    def test_optional_apply_replay(self):
        """Replay every normal golden at its original P/S/GOT in separate storage."""
        if not (os.environ.get('LIBLINK_ELF32_I386_APPLY') or
                os.environ.get('LIBLINK_ELF32_ARM_APPLY')):
            self.skipTest('set LIBLINK_ELF32_I386_APPLY or LIBLINK_ELF32_ARM_APPLY')
        for machine, (_, _, cases) in CASES.items():
            command = os.environ.get('LIBLINK_ELF32_ARM_APPLY' if machine == 40
                                     else 'LIBLINK_ELF32_I386_APPLY')
            if not command:
                continue
            for name in cases:
                with self.subTest(name=name):
                    obj, saved = original(machine, name), golden(machine, name)
                    sections = parse(obj, machine)
                    rel = next(s for s in sections if s['type'] == 9)
                    offset, info = struct.unpack('<II', rel['bytes'])
                    symtab = sections[rel['link']]['bytes']
                    sym_info = symtab[(info >> 8) * 16 + 12] & 15
                    capture, field = saved['reference_capture'], saved['field']
                    sample = next(s for s in capture['sections'] if s['index'] == rel['info'])
                    stubs = sample.get('stubs', {'address': '0', 'before': '', 'after': ''})
                    stub_offset = (int(stubs['address'], 0) - int(sample['address'], 0)
                                   if stubs['before'] else 0)
                    got = int(capture['got']['exec_address'], 0)
                    slot = saved['semantic']['got_slot']
                    args = [str(info & 255), field['P'], field['S'], hex(got),
                            hex(got + slot * 4 if slot is not None else 0), str(sym_info),
                            str(offset), sample['before'], str(stub_offset), stubs['before']]
                    result = subprocess.run(shlex.split(command) + args, check=True,
                                            capture_output=True, text=True, timeout=30)
                    self.assertEqual(result.stdout.splitlines(), [sample['after'], stubs['after']])

    def test_replay(self):
        for machine, (directory, _, cases) in CASES.items():
            self.assertEqual({p.stem for p in (ROOT / 'golden' / directory).glob('*.json')}, set(cases))
            for name in cases:
                with self.subTest(name=name):
                    obj, saved = original(machine, name), golden(machine, name)
                    report = verify(obj, saved['reference_capture'], machine)
                    compare_golden(obj, report, saved)

    def test_corruptions(self):
        for machine, name in ((3, 'R_386_GOT32'), (40, 'R_ARM_THM_CALL-far')):
            obj, saved = original(machine, name), golden(machine, name)
            for change in ('field', 'original', 'got', 'extra', 'resolver', 'saved', 'stub'):
                if change == 'stub' and machine == 3:
                    continue
                with self.subTest(name=name, change=change):
                    modified = copy.deepcopy(saved)
                    capture = modified['reference_capture']
                    target = next(s for s in capture['sections']
                                  if s['index'] == saved['semantic']['section'])
                    def flip(item, field, offset):
                        data = bytearray.fromhex(item[field]); data[offset] ^= 1
                        item[field] = data.hex()
                    if change == 'field': flip(target, 'after', saved['semantic']['offset'])
                    elif change == 'original': flip(target, 'before', 0)
                    elif change == 'got': flip(capture['got'], 'after', 0)
                    elif change == 'extra': capture['extras']['after'] = '01'
                    elif change == 'resolver': capture['host_symbols']['external_data'] = '0x12345001'
                    elif change == 'saved': modified['field']['P'] = '0x0'
                    else: flip(target['stubs'], 'after', 0)
                    with self.assertRaises(ValueError):
                        report = verify(obj, capture, machine)
                        compare_golden(obj, report, modified)

    def test_optional_live(self):
        if not (os.environ.get('LIBLINK_ELF32_ARM_CAPTURE') or
                os.environ.get('LIBLINK_ELF32_I386_CAPTURE')):
            self.skipTest('set LIBLINK_ELF32_ARM_CAPTURE or LIBLINK_ELF32_I386_CAPTURE')
        for machine, (directory, triple, cases) in CASES.items():
            command = os.environ.get('LIBLINK_ELF32_ARM_CAPTURE' if machine == 40
                                     else 'LIBLINK_ELF32_I386_CAPTURE')
            if not command:
                continue
            for name in cases:
                path = ROOT / 'fixtures/generated' / directory / triple / (name.removesuffix('-far') + '.o')
                args = shlex.split(command) + (['--far-function'] if name.endswith('-far') else []) + [str(path)]
                with self.subTest(name=name):
                    capture = json.loads(subprocess.run(args, check=True, capture_output=True,
                                                        text=True, timeout=30).stdout)
                    obj = path.read_bytes()
                    compare_golden(obj, verify(obj, capture, machine), golden(machine, name))


if __name__ == '__main__':
    unittest.main()
