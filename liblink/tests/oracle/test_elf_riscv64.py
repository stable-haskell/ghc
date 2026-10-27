"""Replay baseline RISC-V ELF records and reject corrupted captures."""

import copy
import json
import os
from pathlib import Path
import subprocess
import struct
import unittest

import elf_riscv64_check

ROOT = Path(__file__).resolve().parents[3]
GOLDEN = ROOT / 'liblink/tests/golden/elf-riscv64'
OBJECTS = ROOT / 'liblink/tests/fixtures/generated/elf-riscv64'


class RiscvBaselineTests(unittest.TestCase):
    def test_reference_and_corruption(self):
        records = sorted(GOLDEN.glob('R_RISCV_*.json'))
        self.assertEqual(len(records), 23)
        for path in records:
            with self.subTest(path=path.name):
                record = json.loads(path.read_text())
                original = (OBJECTS / record['object']).read_bytes()
                result = elf_riscv64_check.verify(original, record['capture'])
                self.assertEqual(result['object_sha256'], record['object_sha256'])
                self.assertEqual(result['fields'], record['fields'])
                corrupt = copy.deepcopy(record['capture'])
                index = result['fields'][0]['section']
                section = next(s for s in corrupt['sections'] if s['index'] == index)
                data = bytearray.fromhex(section['after'])
                data[0] ^= 1
                section['after'] = data.hex()
                with self.assertRaises(ValueError):
                    elf_riscv64_check.verify(original, corrupt)
                corrupt = copy.deepcopy(record['capture'])
                corrupt['sections'][0]['size'] += 1
                with self.assertRaises(ValueError):
                    elf_riscv64_check.verify(original, corrupt)
                corrupt = copy.deepcopy(record['capture'])
                corrupt['sections'][0]['stubs']['entries'].append(
                    dict(address='0x1', target='0x2', flags=0, got_address='0x3'))
                with self.assertRaises(ValueError):
                    elf_riscv64_check.verify(original, corrupt)
                if record['id'] == 'R_RISCV_BRANCH':
                    corrupt = copy.deepcopy(record['capture'])
                    old = int(corrupt['host_symbols']['external_function'], 0)
                    new = old + 0x10000
                    corrupt['host_symbols']['external_function'] = hex(new)
                    got = bytes.fromhex(corrupt['got']['after'])
                    corrupt['got']['after'] = got.replace(struct.pack('<Q', old),
                                                         struct.pack('<Q', new)).hex()
                    with self.assertRaisesRegex(ValueError, 'branch relocation out of range'):
                        elf_riscv64_check.verify(original, corrupt)
                corrupt = copy.deepcopy(record['capture'])
                got = bytearray.fromhex(corrupt['got']['after'])
                if got:
                    got[0] ^= 1
                    corrupt['got']['after'] = got.hex()
                    with self.assertRaises(ValueError):
                        elf_riscv64_check.verify(original, corrupt)
                corrupt = copy.deepcopy(record['capture'])
                section = next((s for s in corrupt['sections'] if s['stubs']['size']), None)
                if section:
                    stub = bytearray.fromhex(section['stubs']['after'])
                    stub[0] ^= 1
                    section['stubs']['after'] = stub.hex()
                    with self.assertRaises(ValueError):
                        elf_riscv64_check.verify(original, corrupt)

    def test_live_captures_when_requested(self):
        runner = os.environ.get('LIBLINK_ELF_RISCV64_CAPTURE')
        if not runner:
            self.skipTest('no RISC-V capture runner supplied')
        for path in sorted(GOLDEN.glob('R_RISCV_*.json')):
            with self.subTest(path=path.name):
                record = json.loads(path.read_text())
                obj = OBJECTS / record['object']
                run = subprocess.run([runner, str(obj)], capture_output=True,
                                     text=True, timeout=30, check=True)
                result = elf_riscv64_check.verify(obj.read_bytes(), json.loads(run.stdout))
                self.assertEqual(result['object_sha256'], record['object_sha256'])
                self.assertEqual([(f['section'], f['offset'], f['type'], f['symbol'])
                                  for f in result['fields']],
                                 [(f['section'], f['offset'], f['type'], f['symbol'])
                                  for f in record['fields']])


if __name__ == '__main__':
    unittest.main()
