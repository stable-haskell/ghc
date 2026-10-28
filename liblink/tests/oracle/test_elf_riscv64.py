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
NOEXEC_REFERENCE = ROOT / 'liblink/tests/golden/elf-riscv64-noexec.json'


def normalize_noexec(capture, verified):
    """Remove heap addresses while retaining execution layout and bytes."""
    sections = []
    for section in capture['sections']:
        stubs = section['stubs']
        work = int(section['address'], 0)
        sections.append(dict(index=section['index'], exec_address=section['exec_address'],
                             size=section['size'], after=section['after'],
                             stub_offset=(int(stubs['address'], 0) - work
                                          if stubs['size'] else None),
                             stub_bytes=stubs['after'],
                             entries=[dict(offset=int(e['address'], 0) - work,
                                           target=e['target'], flags=e['flags'],
                                           got_address=e['got_address'])
                                      for e in stubs['entries']]))
    return dict(object_sha256=verified['object_sha256'], fields=verified['fields'],
                host_symbols=capture['host_symbols'], sections=sections,
                got_exec=capture['got']['exec_address'], got=capture['got']['after'],
                extras_exec=capture['extras']['exec_address'],
                extras=capture['extras']['after'])


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

    def test_live_noexec_when_requested(self):
        runner = os.environ.get('LIBLINK_ELF_RISCV64_CAPTURE')
        if not runner:
            self.skipTest('no RISC-V capture runner supplied')
        reference = json.loads(NOEXEC_REFERENCE.read_text())
        self.assertEqual(len(reference), 23)
        for path in sorted(GOLDEN.glob('R_RISCV_*.json')):
            with self.subTest(path=path.name):
                record = json.loads(path.read_text())
                obj = OBJECTS / record['object']
                for _ in range(2):
                    run = subprocess.run([runner, '--noexec', str(obj)], capture_output=True,
                                         text=True, timeout=30, check=True)
                    capture = json.loads(run.stdout)
                    verified = elf_riscv64_check.verify(obj.read_bytes(), capture, noexec=True)
                    self.assertEqual(normalize_noexec(capture, verified), reference[path.stem])
                    corrupt = copy.deepcopy(capture)
                    section = next(s for s in corrupt['sections']
                                   if s['index'] == verified['fields'][0]['section'])
                    data = bytearray.fromhex(section['after'])
                    data[0] ^= 1
                    section['after'] = data.hex()
                    with self.assertRaises(ValueError):
                        elf_riscv64_check.verify(obj.read_bytes(), corrupt, noexec=True)
                    if path.stem == 'R_RISCV_GOT_HI20':
                        corrupt = copy.deepcopy(capture)
                        corrupt['got']['exec_address'] = hex(
                            int(corrupt['got']['exec_address'], 0) + 0x1000)
                        with self.assertRaises(ValueError):
                            elf_riscv64_check.verify(obj.read_bytes(), corrupt, noexec=True)
                    if path.stem == 'R_RISCV_CALL_PLT':
                        corrupt = copy.deepcopy(capture)
                        stub = next(s['stubs'] for s in corrupt['sections']
                                    if s['stubs']['size'])
                        data = bytearray.fromhex(stub['after'])
                        data[0] ^= 1
                        stub['after'] = data.hex()
                        with self.assertRaises(ValueError):
                            elf_riscv64_check.verify(obj.read_bytes(), corrupt, noexec=True)


if __name__ == '__main__':
    unittest.main()
