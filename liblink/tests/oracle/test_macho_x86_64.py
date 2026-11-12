"""Saved and optional live x86_64 Mach-O relocation checks."""
import copy
import json
import os
from pathlib import Path
import subprocess
import unittest

from macho_metadata import OracleError
from macho_x86_64_check import compare, verify

ROOT = Path(__file__).resolve().parents[1]
OBJECTS = ROOT / 'fixtures/generated/macho-x86_64'
GOLDENS = ROOT / 'golden/macho-x86_64'


class X86MachOTests(unittest.TestCase):
    def test_references_and_mutations(self):
        reader = os.environ.get('LLVM_READOBJ', 'llvm-readobj')
        fixtures = sorted(OBJECTS.glob('X86_64_RELOC_*.o'))
        self.assertEqual(len(fixtures), 8)
        for obj in fixtures:
            with self.subTest(obj=obj.name):
                golden = GOLDENS / (obj.stem + '.json')
                saved = json.loads(golden.read_text())
                compare(obj, saved['capture'], golden, reader)
                bad = copy.deepcopy(saved['capture'])
                section = next(s for s in bad['sections'] if s['relocations'])
                data = bytearray.fromhex(section['pre_init_relocated_bytes'])
                data[0] ^= 1
                section['pre_init_relocated_bytes'] = data.hex()
                with self.assertRaises(OracleError):
                    verify(obj, bad, reader)
                runner = os.environ.get('LIBLINK_MACHO_X86_CAPTURE')
                if runner:
                    result = subprocess.run(['arch', '-x86_64', runner, str(obj)],
                                            capture_output=True, text=True, timeout=60)
                    self.assertEqual((result.returncode, result.stderr), (0, ''))
                    compare(obj, json.loads(result.stdout), golden, reader)


if __name__ == '__main__':
    unittest.main()
