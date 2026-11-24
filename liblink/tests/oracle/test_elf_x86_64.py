"""Replay native ELF references and reject corrupt observations, never objects."""

import copy
import json
import os
from pathlib import Path
import subprocess
import unittest

from elf_x86_64_check import compare_golden, verify


LIBLINK = Path(__file__).resolve().parents[2]
FIXTURES = LIBLINK / 'tests/fixtures/generated/elf-x86_64/x86_64-linux-gnu'
GOLDENS = LIBLINK / 'tests/golden/elf-x86_64'
NOEXEC_GOLDENS = LIBLINK / 'tests/golden/elf-x86_64-noexec'
KINDS = ('64', 'PC32', 'PLT32', 'GOTPCREL', 'GOTPCRELX', 'REX_GOTPCRELX',
         '32', '32S', 'PC64')


def reference(kind='GOTPCREL'):
    stem = 'R_X86_64_' + kind
    original = (FIXTURES / (stem + '.o')).read_bytes()
    golden = json.loads((GOLDENS / (stem + '.json')).read_text())
    return original, golden


def flip(sample, key, offset):
    data = bytearray.fromhex(sample[key])
    data[offset] ^= 1
    sample[key] = data.hex()


class ElfOracle(unittest.TestCase):
    def test_rejects_corrupt_noexec_observations(self):
        original, _ = reference()
        golden = json.loads((NOEXEC_GOLDENS / 'R_X86_64_GOTPCREL.json').read_text())
        mutations = {
            'global GOT payload': lambda c: flip(c['got'], 'after', 0),
            'section aliases work': lambda c: c['sections'][0].update(
                exec_address=c['sections'][0]['address']),
            'extras alias work': lambda c: c['extras'].update(
                exec_address=c['extras']['address']),
            'global GOT aliases work': lambda c: c['got'].update(
                exec_address=c['got']['address']),
            'global GOT alignment': lambda c: c['got'].update(
                exec_address=hex(int(c['got']['exec_address'], 0) + 1)),
        }
        for name, mutate in mutations.items():
            with self.subTest(mutation=name):
                capture = copy.deepcopy(golden['reference_capture'])
                mutate(capture)
                with self.assertRaises(ValueError):
                    verify(original, capture)

    def test_rejects_corrupt_noexec_image_reference(self):
        original, _ = reference()
        golden = json.loads((NOEXEC_GOLDENS / 'R_X86_64_GOTPCREL.json').read_text())
        report = verify(original, golden['reference_capture'])
        golden['noexec_image']['got']['sha256'] = '0' * 64
        with self.assertRaisesRegex(ValueError, 'saved NO_EXEC image differs'):
            compare_golden(report, golden, original)

    def test_noexec_references(self):
        for kind in KINDS:
            with self.subTest(kind=kind):
                original, _ = reference(kind)
                golden = json.loads((NOEXEC_GOLDENS / ('R_X86_64_' + kind + '.json')).read_text())
                report = verify(original, golden['reference_capture'])
                compare_golden(report, golden, original)

    def test_all_original_native_references(self):
        for kind in KINDS:
            with self.subTest(kind=kind):
                original, golden = reference(kind)
                report = verify(original, golden['reference_capture'])
                compare_golden(report, golden, original)

    def test_rejects_corrupt_observations(self):
        original, golden = reference()
        field = golden['fields'][0]
        extra = golden['reference_capture']['extras']
        symbol_index = golden['semantic']['relocations'][0]['symbol_index']
        slot_offset = (symbol_index - extra['first']) * extra['stride']

        def target(capture):
            return next(s for s in capture['sections'] if s['index'] == field['section'])

        mutations = {
            'relocated field': lambda c: flip(target(c), 'after', field['offset']),
            'original bytes': lambda c: flip(target(c), 'before', field['offset']),
            'untouched section': lambda c: flip(c['sections'][0], 'after', 0),
            'GOT target': lambda c: flip(c['extras'], 'after', slot_offset),
            'island instruction': lambda c: flip(c['extras'], 'after', slot_offset + 8),
            'untouched extra': lambda c: flip(c['extras'], 'after', 0),
            'slot stride': lambda c: c['extras'].update(stride=32),
            'resolver input': lambda c: c['host_symbols'].update(external_data='0x12345001'),
            'initializer event': lambda c: c.update(initializers_ran=True),
        }
        for name, mutate in mutations.items():
            with self.subTest(mutation=name):
                capture = copy.deepcopy(golden['reference_capture'])
                mutate(capture)
                with self.assertRaises(ValueError):
                    verify(original, capture)

    def test_rejects_corrupt_saved_numeric_reference(self):
        original, golden = reference()
        report = verify(original, golden['reference_capture'])
        golden['fields'][0]['S'] = '0'
        with self.assertRaisesRegex(ValueError, 'saved reference differs: fields'):
            compare_golden(report, golden, original)

    def test_rejects_changed_candidate_semantics(self):
        original, golden = reference()
        report = verify(original, golden['reference_capture'])
        report['semantic']['relocations'][0]['extra_symbol_index'] += 1
        with self.assertRaisesRegex(ValueError, 'golden differs: semantic'):
            compare_golden(report, golden, original)

    @unittest.skipUnless(os.environ.get('LIBLINK_ELF_CAPTURE'), 'set LIBLINK_ELF_CAPTURE for live captures')
    def test_live_native_captures(self):
        for kind in KINDS:
            with self.subTest(kind=kind):
                original, golden = reference(kind)
                path = FIXTURES / ('R_X86_64_' + kind + '.o')
                run = subprocess.run([os.environ['LIBLINK_ELF_CAPTURE'], str(path)],
                                     capture_output=True, text=True, timeout=90)
                self.assertEqual(run.returncode, 0, run.stderr + run.stdout)
                compare_golden(verify(original, json.loads(run.stdout)), golden, original)
        # Force the PLT32 fallback without dereferencing or calling the resolver
        # input. Its destination changes, so verify its full numeric result
        # rather than comparing it to the ordinary direct-branch signature.
        original, _ = reference('PLT32')
        run = subprocess.run([os.environ['LIBLINK_ELF_CAPTURE'], '--far-function',
                              str(FIXTURES / 'R_X86_64_PLT32.o')],
                             capture_output=True, text=True, timeout=90)
        self.assertEqual(run.returncode, 0, run.stderr + run.stdout)
        report = verify(original, json.loads(run.stdout))
        self.assertIsNotNone(report['semantic']['relocations'][0]['extra_symbol_index'])

    @unittest.skipUnless(os.environ.get('LIBLINK_ELF_CAPTURE'), 'set LIBLINK_ELF_CAPTURE for live captures')
    def test_live_noexec_captures(self):
        for kind in KINDS:
            with self.subTest(kind=kind):
                original, _ = reference(kind)
                golden = json.loads((NOEXEC_GOLDENS / ('R_X86_64_' + kind + '.json')).read_text())
                for _ in range(2):
                    run = subprocess.run([os.environ['LIBLINK_ELF_CAPTURE'], '--noexec',
                                          str(FIXTURES / ('R_X86_64_' + kind + '.o'))],
                                         capture_output=True, text=True, timeout=90)
                    self.assertEqual(run.returncode, 0, run.stderr + run.stdout)
                    capture = json.loads(run.stdout)
                    self.assertIs(capture['noexec'], True)
                    compare_golden(verify(original, capture), golden, original)
        original, _ = reference('PLT32')
        run = subprocess.run([os.environ['LIBLINK_ELF_CAPTURE'], '--noexec', '--far-function',
                              str(FIXTURES / 'R_X86_64_PLT32.o')],
                             capture_output=True, text=True, timeout=90)
        self.assertEqual(run.returncode, 0, run.stderr + run.stdout)
        report = verify(original, json.loads(run.stdout))
        self.assertIsNotNone(report['semantic']['relocations'][0]['extra_symbol_index'])


if __name__ == '__main__':
    unittest.main()
