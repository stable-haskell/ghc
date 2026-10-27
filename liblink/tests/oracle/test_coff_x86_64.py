#!/usr/bin/env python3
"""Replay COFF references and reject changed fields and untouched bytes."""
import copy
import json
from pathlib import Path
import unittest

import coff_x86_64_check as oracle

ROOT = Path(__file__).resolve().parents[3]
NAMES = ('IMAGE_REL_AMD64_ADDR64', 'IMAGE_REL_AMD64_ADDR32NB',
         'IMAGE_REL_AMD64_REL32', 'IMAGE_REL_AMD64_SECREL')


class CoffReferenceTests(unittest.TestCase):
    def test_references_and_corruption(self):
        for name in NAMES:
            with self.subTest(name=name):
                obj = (ROOT / 'liblink/tests/fixtures/generated/coff-x86_64' / (name + '.o')).read_bytes()
                golden = json.loads((ROOT / 'liblink/tests/golden/coff-x86_64' / (name + '.json')).read_text())
                capture = golden['reference_capture']
                self.assertEqual(oracle.verify(obj, capture), golden)
                broken = copy.deepcopy(capture)
                data = bytearray.fromhex(broken['after'])
                data[broken['offset']] ^= 1
                broken['after'] = data.hex()
                with self.assertRaises(ValueError):
                    oracle.verify(obj, broken)
                broken = copy.deepcopy(capture)
                broken['target'] = hex(int(broken['target'], 0) + 1)
                with self.assertRaises(ValueError):
                    oracle.verify(obj, broken)


if __name__ == '__main__':
    unittest.main()
