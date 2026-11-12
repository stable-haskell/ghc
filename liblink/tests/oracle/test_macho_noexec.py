"""Check NO_EXEC Mach-O bytes from original relocations and normal references."""
import json
import os
from pathlib import Path
import subprocess
import unittest

from macho_metadata import read_object, require
from macho_arm64 import pageoff_scale, signed, stub_target
from golden_reference import replay_reference
from macho_x86_64_check import compare

ROOT = Path(__file__).resolve().parents[1]
ARM = ['UNSIGNED', 'BRANCH26', 'PAGE21', 'PAGEOFF12', 'GOT_LOAD_PAGE21',
       'GOT_LOAD_PAGEOFF12', 'ADDEND']
X86 = ['UNSIGNED', 'BRANCH', 'SIGNED', 'SIGNED_1', 'SIGNED_2', 'SIGNED_4', 'GOT', 'GOT_LOAD']


def check(obj, capture, arch, reader):
    metadata, _ = read_object(obj, reader, arch)
    target = int(capture['target'], 0)
    sections = capture['sections']
    require(len(sections) == len(metadata['sections']), 'section count differs')
    for sample, section in zip(sections, metadata['sections']):
        require(sample['size'] == section['size'] and sample['index'] == section['index'],
                'section metadata differs')
        if section['size']:
            require(int(sample['work'], 0) != int(sample['exec'], 0), 'section work/exec alias')
    got = capture['got']
    if got['bytes']:
        require(int(got['work'], 0) != int(got['exec'], 0), 'GOT work/exec alias')
    extras = capture['extras']
    if extras['bytes']:
        require(int(extras['work'], 0) != int(extras['exec'], 0), 'extras work/exec alias')
    expected_extras = bytearray(len(bytes.fromhex(extras['bytes'])))
    got_expected = bytearray(len(bytes.fromhex(got['bytes'])))
    used_stubs = set()
    for i, symbol in enumerate(metadata['symbols']):
        value = capture['symbols'][i]
        slot = int(value['got_exec'], 0)
        if not slot:
            continue
        expected = (int(sections[symbol['section']-1]['exec'], 0) + symbol['value'] -
                    metadata['sections'][symbol['section']-1]['address']) if symbol['type'] == 14 else target
        offset = slot - int(got['exec'], 0)
        require(0 <= offset <= len(got_expected)-8 and offset % 8 == 0, 'bad GOT slot')
        got_expected[offset:offset+8] = expected.to_bytes(8, 'little')
    require(bytes.fromhex(got['bytes']) == got_expected, 'GOT bytes differ')
    identities = []
    for section, sample in zip(metadata['sections'], sections):
        expected = bytearray(section['bytes'])
        pending = 0
        for reloc in section['relocations']:
            kind, off, index = reloc['type_number'], reloc['offset'], reloc['symbol_index']
            if arch == 'arm64' and kind == 10:
                pending = signed(index, 24)
                identities.append((section['index'], off, reloc['type']))
                continue
            symbol = metadata['symbols'][index]
            require(symbol['name'] == '_ll_fixture_target', 'unexpected fixture symbol')
            width = 1 << reloc['length']; original = int.from_bytes(section['bytes'][off:off+width], 'little')
            place = int(sample['exec'], 0) + off
            if arch == 'arm64':
                endpoint = target
                if kind in (5, 6): endpoint = int(capture['symbols'][index]['got_exec'], 0)
                if kind == 0:
                    value = target + signed(original, width*8)
                elif kind == 2:
                    delta = endpoint - place + signed(original & 0x3ffffff, 26)*4 + pending
                    if not -(1 << 27) <= delta < (1 << 27):
                        require(len(sample.get('stubs', [])) == 1, 'missing branch stub')
                        stub = sample['stubs'][0]
                        stub_offset = int(stub['work'], 0) - int(sample['stub_work'], 0)
                        payload = bytes.fromhex(sample['stub_bytes'])[stub_offset:stub_offset+20]
                        require(stub_target(payload) == target == int(stub['target'], 0),
                                'stub target differs')
                        used_stubs.add((section['index'], stub_offset))
                        endpoint = int(sample['stub_exec'], 0) + stub_offset
                        delta = endpoint - place + signed(original & 0x3ffffff, 26)*4 + pending
                    require(delta % 4 == 0 and -(1 << 27) <= delta < (1 << 27), 'branch out of range')
                    value = (original & ~0x3ffffff) | ((delta >> 2) & 0x3ffffff)
                elif kind in (3, 5):
                    implicit = signed(((original >> 29) & 3) | (((original >> 5) & 0x7ffff) << 2), 21) << 12
                    delta = (((endpoint + implicit + pending) & ~0xfff) - (place & ~0xfff)) >> 12
                    value = (original & ~0x60ffffe0) | ((delta & 3) << 29) | (((delta >> 2) & 0x7ffff) << 5)
                elif kind in (4, 6):
                    scale = pageoff_scale(original)
                    addend = ((original >> 10) & 0xfff) * (1 << scale) + pending
                    value = (original & ~0x3ffc00) | ((((endpoint + addend) & 0xfff) >> scale) << 10)
                else: raise ValueError('unsupported arm relocation')
                pending = 0
            else:
                endpoint = target
                if kind in (3, 4) or (kind == 2 and not -(1 << 31) <= target - (place + width) < (1 << 31)):
                    endpoint = int(extras['exec'], 0) + index * extras['stride']
                    expected_extras[index*16:index*16+16] = target.to_bytes(8, 'little') + bytes.fromhex('ff25f2ffffff0000')
                    if kind == 2: endpoint += 8
                value = original + endpoint if kind == 0 else original + endpoint - (place + width + {6:1, 7:2, 8:4}.get(kind, 0))
            expected[off:off+width] = (value % (1 << (8*width))).to_bytes(width, 'little')
            identities.append((section['index'], off, reloc['type']))
        require(sample['bytes'] == expected.hex(), 'relocated section bytes differ')
        if 'stub_bytes' in sample:
            require(int(sample['stub_exec'], 0) - int(sample['exec'], 0) ==
                    int(sample['stub_work'], 0) - int(sample['work'], 0), 'stub layout differs')
            stub_bytes = bytearray.fromhex(sample['stub_bytes'])
            for stub in sample.get('stubs', []):
                offset = int(stub['work'], 0) - int(sample['stub_work'], 0)
                require((section['index'], offset) in used_stubs, 'unused stub')
                stub_bytes[offset:offset+20] = bytes(20)
            require(not any(stub_bytes), 'unexpected stub bytes')
    require(bytes.fromhex(extras['bytes']) == expected_extras, 'extras bytes differ')
    golden_dir = 'arm64' if arch == 'arm64' else 'macho-x86_64'
    golden = ROOT / 'golden' / golden_dir / (obj.stem + '.json')
    reference = json.loads(golden.read_text())
    if arch == 'arm64':
        replay_reference(metadata, reference, obj.read_bytes())
        golden_fields = reference['fields']
        golden_ids = [(f['section'], f['offset'], f['type']) for f in golden_fields]
    else:
        compare(obj, reference['capture'], golden, reader)
        golden_ids = [(f['section'], f['offset'], f['type']) for f in reference['checked']['semantic']]
    require(identities == golden_ids, 'normal reference relocation identities differ')


class MachONoexecTests(unittest.TestCase):
    def check_live(self, selected):
        reader = os.environ.get('LLVM_READOBJ', 'llvm-readobj')
        for arch, names, variable, prefix in [('arm64', ARM, 'LIBLINK_MACHO_ARM_NOEXEC', 'ARM64'),
                                               ('64-bit x86-64', X86, 'LIBLINK_MACHO_X86_NOEXEC', 'X86_64')]:
            if arch != selected: continue
            runner = os.environ.get(variable)
            if not runner: self.skipTest(variable + ' is required')
            for name in names:
                obj = ROOT / 'fixtures/generated' / ('macho-arm64' if arch == 'arm64' else 'macho-x86_64') / (prefix + '_RELOC_' + name + '.o')
                outputs = []
                for _ in range(2):
                    command = ([runner, str(obj)] if arch == 'arm64' else ['arch', '-x86_64', runner, str(obj)])
                    result = subprocess.run(command, capture_output=True, text=True, timeout=60)
                    self.assertEqual((result.returncode, result.stderr), (0, ''), obj.name)
                    capture = json.loads(result.stdout); check(obj, capture, arch, reader)
                    outputs.append(([ (s['exec'], s['bytes'], s.get('stub_bytes')) for s in capture['sections']],
                                    capture['got']['exec'], capture['got']['bytes'], capture['extras']['exec'], capture['extras']['bytes']))
                self.assertEqual(outputs[0], outputs[1], obj.name)

    def check_normal_layout(self, arch):
        names, variable, prefix = ((ARM, 'LIBLINK_MACHO_ARM_NOEXEC', 'ARM64') if arch == 'arm64'
                                   else (X86, 'LIBLINK_MACHO_X86_NOEXEC', 'X86_64'))
        runner = os.environ.get(variable)
        if not runner: self.skipTest(variable + ' is required')
        for name in names:
            stem = prefix + '_RELOC_' + name
            obj = ROOT / 'fixtures/generated' / ('macho-arm64' if arch == 'arm64' else 'macho-x86_64') / (stem + '.o')
            golden = json.loads((ROOT / 'golden' / ('arm64' if arch == 'arm64' else 'macho-x86_64') / (stem + '.json')).read_text())
            if arch == 'arm64':
                reference = golden['reference_layout']
                section = next(s['address'] for s in reference['sections'] if s['size'])
                got = (reference['got']['address'] or '0') if reference['got'] else '0'
                extras = '0'
                target = reference['host_symbols']['_ll_fixture_target']
                expected = [s['relocated_bytes'] for s in reference['sections']]
            else:
                reference = golden['capture']
                section = next(s['mapped_start'] for s in reference['sections'] if s['size'])
                got = '0'
                extras = reference['extras']['address']
                target = reference['host_symbols']['_ll_fixture_target']
                expected = [s['pre_init_relocated_bytes'] for s in reference['sections']]
            command = ([runner] if arch == 'arm64' else ['arch', '-x86_64', runner]) + [
                '--layout', str(obj), section, got, extras, target]
            result = subprocess.run(command, capture_output=True, text=True, timeout=60)
            self.assertEqual((result.returncode, result.stderr), (0, ''), obj.name)
            actual = json.loads(result.stdout)
            self.assertEqual([s['bytes'] for s in actual['sections']], expected, obj.name)
            if arch == 'arm64':
                if reference['got']:
                    self.assertEqual(actual['got']['bytes'], reference['got']['bytes'], obj.name)
                for stub in reference['stubs']:
                    self.assertEqual(actual['sections'][stub['section_index']].get('stub_bytes', ''), stub['reserved_bytes'], obj.name)
            else:
                self.assertEqual(actual['extras']['bytes'], reference['extras']['bytes'], obj.name)

    def check_far_branch(self, selected):
        reader = os.environ.get('LLVM_READOBJ', 'llvm-readobj')
        for arch, variable, prefix in [('arm64', 'LIBLINK_MACHO_ARM_NOEXEC', 'ARM64'),
                                       ('64-bit x86-64', 'LIBLINK_MACHO_X86_NOEXEC', 'X86_64')]:
            if arch != selected: continue
            runner = os.environ.get(variable)
            if not runner: self.skipTest(variable + ' is required')
            name = 'BRANCH26' if arch == 'arm64' else 'BRANCH'
            obj = ROOT / 'fixtures/generated' / ('macho-arm64' if arch == 'arm64' else 'macho-x86_64') / (prefix + '_RELOC_' + name + '.o')
            command = ([runner, '--far', str(obj)] if arch == 'arm64' else
                       ['arch', '-x86_64', runner, '--far', str(obj)])
            result = subprocess.run(command, capture_output=True, text=True, timeout=60)
            self.assertEqual((result.returncode, result.stderr), (0, ''), obj.name)
            check(obj, json.loads(result.stdout), arch, reader)

    def test_arm64(self):
        self.check_live('arm64')
        self.check_normal_layout('arm64')
        self.check_far_branch('arm64')

    def test_x86_64(self):
        self.check_live('64-bit x86-64')
        self.check_normal_layout('64-bit x86-64')
        self.check_far_branch('64-bit x86-64')

if __name__ == '__main__': unittest.main()
