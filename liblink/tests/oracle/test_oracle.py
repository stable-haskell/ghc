"""Native integration and adversarial capture checks for the independent oracle."""

import copy
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import unittest

from macho_metadata import OracleError, read_object, parse_reader, bind_original
from macho_arm64 import verify, signed, branch_word
from reloc_check import compare_golden
from golden_reference import replay_reference


ROOT = Path(__file__).resolve().parents[1]


def first_relocation(capture, kind=None):
    return next((s, r) for s in capture['sections'] for r in s['relocations']
                if kind is None or r['type'] == kind)


def replace_field(section, relocation, value):
    relocation['after'] = value.hex()
    data = bytearray.fromhex(section['pre_init_relocated_bytes'])
    offset = relocation['raw']['r_address']
    data[offset:offset + len(value)] = value
    section['pre_init_relocated_bytes'] = data.hex()


class OracleTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.runner = os.environ.get('LIBLINK_TEST_RUNNER')
        cls.readobj = shutil.which(os.environ.get('LLVM_READOBJ', 'llvm-readobj'))
        cls.mc = shutil.which(os.environ.get('LLVM_MC', 'llvm-mc'))
        if not cls.runner or not cls.readobj or not cls.mc:
            raise RuntimeError('set LIBLINK_TEST_RUNNER, LLVM_READOBJ and LLVM_MC; real tools are required')
        cls.golden_dir = Path(os.environ.get('LIBLINK_GOLDEN_DIR', ROOT / 'golden/arm64'))
        cls.cases = {}
        cls.unsupported = None
        catalog = json.loads((ROOT / 'fixtures/macho-arm64.json').read_text())
        for item in catalog['sources']:
            if item['expected_outcome'] != 'supported':
                cls.unsupported = item
                continue
            path = ROOT / 'fixtures/generated/macho-arm64' / item['object']
            metadata, reader = read_object(path, cls.readobj)
            result = subprocess.run([cls.runner, str(path)], capture_output=True, text=True, timeout=30)
            if result.returncode or result.stderr:
                raise RuntimeError('native capture failed: ' + item['id'] + ': ' + result.stderr)
            cls.cases[item['id']] = metadata, json.loads(result.stdout), path.read_bytes(), reader

    def check_case(self, name, capture=None):
        metadata, actual, original, unused = self.cases[name]
        return verify(metadata, actual if capture is None else capture, original)

    def reject(self, name, mutate):
        capture = copy.deepcopy(self.cases[name][1])
        mutate(capture)
        with self.assertRaises(OracleError):
            self.check_case(name, capture)

    def test_all_native_fields_and_golden_signatures(self):
        self.assertEqual(len(self.cases), 14)
        records = 0
        for name in self.cases:
            with self.subTest(name=name):
                report = self.check_case(name)
                records += len(report['fields'])
                golden = self.golden_dir / (name + '.json')
                saved = json.loads(golden.read_text())
                metadata, capture, original, unused = self.cases[name]
                replay_reference(metadata, saved, original)
                compare_golden(report, saved)
        self.assertEqual(records, 23)

    def test_expected_unsupported_relocation(self):
        item = self.unsupported
        self.assertIsNotNone(item)
        path = ROOT / 'fixtures/generated/macho-arm64' / item['object']
        saved = json.loads((self.golden_dir / (item['id'] + '.unsupported.json')).read_text())
        self.assertEqual(saved['object_sha256'], hashlib.sha256(path.read_bytes()).hexdigest())
        metadata, unused = read_object(path, self.readobj)
        self.assertEqual([r['type'] for s in metadata['sections'] for r in s['relocations']],
                         saved['expected_relocations'])
        result = subprocess.run([self.runner, str(path)], capture_output=True, text=True, timeout=30)
        self.assertEqual((result.returncode, result.stdout, result.stderr),
                         (saved['exit'], '', saved['diagnostic'].replace('{object}', str(path))))

    def test_wrong_field_even_with_consistent_section_snapshot(self):
        def mutate(capture):
            section, relocation = first_relocation(capture)
            bad = bytearray.fromhex(relocation['after'])
            bad[0] ^= 1
            replace_field(section, relocation, bad)
        self.reject('ARM64_RELOC_UNSIGNED', mutate)

    def test_self_consistent_observed_symbol_is_not_ground_truth(self):
        def mutate(capture):
            section, relocation = first_relocation(capture)
            wrong = int(relocation['S']['observed'], 16) + 8
            relocation['S']['observed'] = hex(wrong)
            replace_field(section, relocation, wrong.to_bytes(8, 'little'))
        self.reject('ARM64_RELOC_UNSIGNED', mutate)

    def test_wrong_explicit_addend(self):
        self.reject('ARM64_RELOC_ADDEND', lambda c: first_relocation(c)[1]['A'].__setitem__('explicit_addend', 9))

    def test_wrong_place_and_raw_record(self):
        self.reject('ARM64_RELOC_PAGE21', lambda c: first_relocation(c)[1].__setitem__('P', '0x0'))
        self.reject('ARM64_RELOC_PAGE21', lambda c: first_relocation(c)[1]['raw'].__setitem__('record_bytes', '00' * 8))

    def test_got_contents_are_checked(self):
        def mutate(capture):
            unused, relocation = first_relocation(capture)
            offset = int(relocation['got_slot'], 16) - int(capture['got']['address'], 16)
            data = bytearray.fromhex(capture['got']['bytes'])
            data[offset] ^= 8
            capture['got']['bytes'] = data.hex()
        self.reject('ARM64_RELOC_GOT_LOAD_PAGE21', mutate)

    def test_stub_code_and_claimed_target_are_checked(self):
        def mutate(capture):
            stub = next(s for s in capture['stubs'] if s['entries'])
            data = bytearray.fromhex(stub['reserved_bytes'])
            wrong = int(stub['entries'][0]['target'], 16) ^ 8
            # Patch MOVK's low halfword and keep the claimed target consistent with it.
            instruction = int.from_bytes(data[12:16], 'little')
            instruction = (instruction & ~0x001fffe0) | ((wrong & 0xffff) << 5)
            data[12:16] = instruction.to_bytes(4, 'little')
            stub['reserved_bytes'] = data.hex()
            stub['entries'][0]['target'] = hex(wrong)
        self.reject('ARM64_RELOC_BRANCH26', mutate)

    def test_changes_outside_relocations_are_checked(self):
        def mutate(capture):
            section = next(s for s in capture['sections'] if s['size'])
            data = bytearray.fromhex(section['pre_init_relocated_bytes'])
            data[-1] ^= 1
            section['pre_init_relocated_bytes'] = data.hex()
        self.reject('ARM64_RELOC_BRANCH26', mutate)

    def test_llvm_metadata_is_bound_to_original_bytes(self):
        metadata, capture, original, reader = self.cases['ARM64_RELOC_UNSIGNED']
        parsed = parse_reader(reader['stdout'].replace('Type: ARM64_RELOC_UNSIGNED (0)',
                                                     'Type: ARM64_RELOC_UNSIGNED (1)'))
        with self.assertRaises(OracleError):
            bind_original(parsed, original)

    def test_addend_page_carry_against_independent_assembler(self):
        source = 'adrp x0, #-4096\nadd x0, x0, #0\n'
        result = subprocess.run([self.mc, '-triple=arm64-apple-macos11', '--show-encoding'],
                                input=source, capture_output=True, text=True, timeout=30)
        self.assertEqual((result.returncode, result.stderr), (0, ''))
        encodings = [bytes(int(x.strip(), 16) for x in row.split(','))
                     for row in re.findall(r'encoding: \[([^\]]+)\]', result.stdout)]
        self.assertEqual(len(encodings), 2)
        capture = copy.deepcopy(self.cases['ARM64_RELOC_ADDEND'][1])
        target, place, slot = 0x20000ff8, 0x20002000, 0x20003008
        capture['host_symbols']['_ll_fixture_target'] = hex(target)
        capture['got'].update(address=hex(slot - 8), bytes=(bytes(8) + target.to_bytes(8, 'little')).hex(), size=16)
        section = capture['sections'][0]
        section['mapped_start'] = hex(place)
        for relocation in section['relocations']:
            relocation['P'] = hex(place + relocation['raw']['r_address'])
            if relocation['type'] != 'ARM64_RELOC_ADDEND':
                relocation['S'].update(observed=hex(target), input=hex(target))
                relocation['got_slot'] = hex(slot)
                value = encodings[0 if relocation['type'] == 'ARM64_RELOC_PAGE21' else 1]
                replace_field(section, relocation, value)
        report = self.check_case('ARM64_RELOC_ADDEND', capture)
        self.assertEqual([f['A'] for f in report['fields']], [8] * 4)
        self.assertEqual(report['fields'][-1]['expected'], encodings[0].hex())

    def test_saved_numeric_reference_is_recomputed(self):
        metadata, capture, original, unused = self.cases['ARM64_RELOC_UNSIGNED']
        report = self.check_case('ARM64_RELOC_UNSIGNED')
        replay_reference(metadata, report, original)
        report['fields'][0]['A'] += 1
        with self.assertRaises(OracleError):
            replay_reference(metadata, report, original)

    def test_semantic_golden_change_is_rejected(self):
        report = self.check_case('ARM64_RELOC_UNSIGNED')
        bad = copy.deepcopy(report)
        bad['semantic'][0]['A'] += 1
        with self.assertRaises(OracleError):
            compare_golden(report, bad)

    def test_signed_addend_and_branch_range_boundaries(self):
        self.assertEqual([signed(v, 24) for v in (0, 0x7fffff, 0x800000, 0xffffff)],
                         [0, 8388607, -8388608, -1])
        self.assertIsNotNone(branch_word(0x14000000, 0, 1 << 27))
        self.assertIsNone(branch_word(0x14000000, 1 << 27, 0))
        self.assertIsNone(branch_word(0x14000000, 2, 0))


if __name__ == '__main__':
    unittest.main()
