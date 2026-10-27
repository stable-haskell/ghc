"""Check original AArch64 ELF baselines and discriminate corrupt captures."""

import copy
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest

from elf_aarch64_check import compare_golden, veneer, verify
from elf64_metadata import parse


LIBLINK = Path(__file__).resolve().parents[2]
FIXTURES = LIBLINK / 'tests/fixtures'
GENERATED = FIXTURES / 'generated/elf-aarch64/aarch64-linux-gnu'
GOLDENS = LIBLINK / 'tests/golden/elf-aarch64'
MANIFEST = json.loads((FIXTURES / 'elf-aarch64.json').read_text())
CASES = [(item['id'], [], GENERATED / item['object'].split('/')[-1])
         for item in MANIFEST['sources'] if item['expected_outcome'] == 'supported']
CASES += [(kind + '-far', ['--far-function'], GENERATED / (kind + '.o'))
          for kind in ('R_AARCH64_CALL26', 'R_AARCH64_JUMP26')]
CASES += [(kind + '-negative', ['--negative-data'], GENERATED / (kind + '.o'))
          for kind in ('R_AARCH64_PREL32', 'R_AARCH64_ADR_PREL_PG_HI21')]
CASES += [(kind, [], FIXTURES / 'elf-aarch64-sites' / (kind + '.o'))
          for kind in ('branch_reuse', 'got_alias')]
CASES += [('branch_reuse-far', ['--far-function'],
           FIXTURES / 'elf-aarch64-sites/branch_reuse.o')]


def reference(name='branch_reuse-far'):
    path = next(path for case, _, path in CASES if case == name)
    return path.read_bytes(), json.loads((GOLDENS / (name + '.json')).read_text())


def flip(sample, key, offset):
    data = bytearray.fromhex(sample[key])
    data[offset] ^= 1
    sample[key] = data.hex()


class Aarch64ElfOracle(unittest.TestCase):
    def test_all_original_references(self):
        self.assertEqual({path.stem for path in GOLDENS.glob('*.json')},
                         {name for name, _, _ in CASES})
        fields = 0
        for name, _, _ in CASES:
            with self.subTest(case=name):
                original, golden = reference(name)
                report = verify(original, golden['reference_capture'])
                compare_golden(report, golden, original)
                fields += len(report['fields'])
        self.assertEqual((len(CASES), fields), (20, 27))

    def test_rejects_corrupt_observations(self):
        original, golden = reference()
        section_index = golden['fields'][0]['section']

        def target(capture):
            return next(s for s in capture['sections'] if s['index'] == section_index)

        def wrong_stub_target(capture):
            target(capture)['stubs']['entries'][0]['target'] = '0x1234'

        mutations = {
            'relocated instruction': lambda c: flip(target(c), 'after', 0),
            'original instruction': lambda c: flip(target(c), 'before', 0),
            'GOT payload': lambda c: flip(c['got'], 'after', 0),
            'veneer instruction': lambda c: flip(target(c)['stubs'], 'after', 0),
            'veneer target': wrong_stub_target,
            'veneer order': lambda c: target(c)['stubs']['entries'].reverse(),
            'untouched stub byte': lambda c: flip(target(c)['stubs'], 'after', 40),
            'stub flag': lambda c: target(c)['stubs']['entries'][0].update(flags=1),
            'normal work/exec mismatch': lambda c: target(c).update(exec_address='0x1234'),
            'omitted metadata section': lambda c: c['sections'].pop(0),
            'resolver input': lambda c: c['host_symbols'].update(external_function='0x700000000004'),
            'extras width': lambda c: c['extras'].update(stride=16),
            'initializer event': lambda c: c.update(initializers_ran=True),
        }
        for name, mutate in mutations.items():
            with self.subTest(mutation=name):
                capture = copy.deepcopy(golden['reference_capture'])
                mutate(capture)
                with self.assertRaises(ValueError):
                    verify(original, capture)

    def test_equal_values_keep_distinct_got_slots(self):
        original, golden = reference('got_alias')
        fields = golden['fields']
        self.assertEqual(fields[0]['S'], fields[1]['S'])
        self.assertNotEqual(fields[0]['GOT_S'], fields[1]['GOT_S'])
        capture = copy.deepcopy(golden['reference_capture'])
        section = next(s for s in capture['sections'] if s['index'] == fields[0]['section'])
        data = bytearray.fromhex(section['after'])
        offsets = [field['offset'] for field in fields]
        instructions = [int.from_bytes(data[offset:offset + 4], 'little') for offset in offsets]
        for offset, instruction, other in zip(offsets, instructions, reversed(instructions)):
            changed = instruction & ~0x003ffc00 | other & 0x003ffc00
            data[offset:offset + 4] = changed.to_bytes(4, 'little')
        section['after'] = data.hex()
        with self.assertRaises(ValueError):
            verify(original, capture)

    def test_rejects_corrupt_saved_numeric_reference(self):
        original, golden = reference()
        report = verify(original, golden['reference_capture'])
        golden['fields'][0]['P'] = '0x0'
        with self.assertRaisesRegex(ValueError, 'saved reference differs: fields'):
            compare_golden(report, golden, original)

    def test_rejects_changed_slot_semantics(self):
        original, golden = reference('got_alias')
        report = verify(original, golden['reference_capture'])
        report['semantic']['relocations'][0]['got_slot'] += 1
        with self.assertRaisesRegex(ValueError, 'golden differs: semantic'):
            compare_golden(report, golden, original)

    def test_veneer_encoding_matches_assembler(self):
        compiler = os.environ.get('LLVM_MC') or shutil.which('llvm-mc')
        if compiler is None:
            self.skipTest('set LLVM_MC for independent assembler encoding checks')
        for target in (0, 0x700012345678, 0x123456789abcdef0, (1 << 64) - 1):
            with self.subTest(target=hex(target)):
                source = '\n'.join(
                    f'{operation} x16, #{target >> shift & 0xffff}, lsl #{shift}'
                    for operation, shift in (('movz', 48), ('movk', 32), ('movk', 16), ('movk', 0)))
                run = subprocess.run([compiler, '--triple=aarch64', '--show-encoding'],
                                     input=source + '\nbr x16\n', capture_output=True,
                                     text=True, timeout=30)
                self.assertEqual(run.returncode, 0, run.stderr)
                encodings = re.findall(r'encoding:\s*\[([^]]+)\]', run.stdout)
                self.assertEqual(len(encodings), 5, run.stdout)
                actual = bytes(int(value.strip(), 0) for encoding in encodings
                               for value in encoding.split(','))
                self.assertEqual(actual, veneer(target))

    @unittest.skipUnless(os.environ.get('LIBLINK_ELF_AARCH64_CAPTURE'),
                         'set LIBLINK_ELF_AARCH64_CAPTURE for live captures')
    def test_live_failure_releases_stub_metadata(self):
        original = (FIXTURES / 'elf-aarch64-sites/branch_reuse.o').read_bytes()
        tables = [section for section in parse(original, 183) if section['type'] == 4]
        self.assertEqual(len(tables), 1)
        table = tables[0]
        self.assertEqual((table['entry_size'], table['size']), (24, 4 * 24))
        changed = bytearray(original)
        position = table['offset'] + 3 * 24 + 8
        info = int.from_bytes(changed[position:position + 8], 'little')
        changed[position:position + 8] = ((info & ~0xffffffff) | 0x7fffffff).to_bytes(8, 'little')
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / 'late-unsupported.o'
            path.write_bytes(changed)
            run = subprocess.run([os.environ['LIBLINK_ELF_AARCH64_CAPTURE'],
                                  '--far-function', str(path)], capture_output=True,
                                 text=True, timeout=90)
        self.assertEqual(run.returncode, 1, run.stderr + run.stdout)
        capture = json.loads(run.stdout)
        self.assertFalse(capture['resolved'])
        self.assertFalse(capture['initializers_ran'])
        self.assertEqual(capture['status'], 3)  # LL_E_UNSUPPORTED
        self.assertEqual(capture['error'],
                         'AArch64 relocation type 2147483647: unsupported relocation type (addend 0)')
        section = next(section for section in capture['sections'] if section['index'] == table['info'])
        self.assertEqual(section['after'], '04000094030000940200009400000094')
        self.assertEqual(len(section['stubs']['entries']), 1)
        target = int(capture['host_symbols']['external_function'], 0)
        self.assertEqual(int(section['stubs']['entries'][0]['target'], 0), target)
        self.assertEqual(bytes.fromhex(section['stubs']['after'])[:20], veneer(target))
        self.assertNotIn('stub metadata records retained', run.stderr)
        self.assertNotIn('cleanup changed the primary error', run.stderr)

    @unittest.skipUnless(os.environ.get('LIBLINK_ELF_AARCH64_CAPTURE'),
                         'set LIBLINK_ELF_AARCH64_CAPTURE for live captures')
    def test_live_captures(self):
        for name, flags, path in CASES:
            with self.subTest(case=name):
                original, golden = reference(name)
                run = subprocess.run([os.environ['LIBLINK_ELF_AARCH64_CAPTURE'], *flags, str(path)],
                                     capture_output=True, text=True, timeout=90)
                self.assertEqual(run.returncode, 0, run.stderr + run.stdout)
                compare_golden(verify(original, json.loads(run.stdout)), golden, original)


if __name__ == '__main__':
    unittest.main()
