"""Check ELF32 metadata against fixed fixture properties and malformed inputs."""

from pathlib import Path
import unittest

from elf32_metadata import parse
from elf64_metadata import unpack


FIXTURES = Path(__file__).resolve().parents[1] / 'fixtures/generated'


class Elf32Metadata(unittest.TestCase):
    def test_relocation_fixture_metadata(self):
        cases = [
            ('elf-i386/i386-linux-gnu/R_386_32.o', 3, 1),
            ('elf-i386/i386-linux-gnu/R_386_PC32.o', 3, 2),
            ('elf-arm/armv7-linux-gnueabihf/R_ARM_ABS32.o', 40, 2),
            ('elf-arm/armv7-linux-gnueabihf/R_ARM_CALL.o', 40, 28),
        ]
        for path, machine, relocation_type in cases:
            with self.subTest(path=path):
                sections = parse((FIXTURES / path).read_bytes(), machine)
                relocations = [section for section in sections if section['type'] == 9]
                self.assertEqual(len(relocations), 1)
                rel = relocations[0]
                self.assertEqual((rel['size'], rel['entry_size']), (8, 8))
                offset, info = unpack('<II', rel['bytes'], 0)
                self.assertEqual(offset, 0)
                self.assertEqual(info & 0xff, relocation_type)
                self.assertIn(sections[rel['info']]['name'], ('.data', '.text'))

    def test_rejects_wrong_machine_and_truncation(self):
        original = (FIXTURES / 'elf-arm/armv7-linux-gnueabihf/R_ARM_ABS32.o').read_bytes()
        with self.assertRaises(ValueError):
            parse(original, 3)
        with self.assertRaises(ValueError):
            parse(original[:-10], 40)


if __name__ == '__main__':
    unittest.main()
