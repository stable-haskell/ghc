"""Functional and malformed-input checks for the two opt-in fixture recipes."""
import json
import os
from pathlib import Path
import shutil
import struct
import subprocess
import tempfile
import unittest

import fixture_recipes as recipes
import generate


def coff(addend=-3, count=1):
    raw = 60
    reloc = raw + 16
    symbols = reloc + 10 * count
    header = struct.pack('<HHIIIHH', 0x8664, 1, 0, symbols, 1, 0, 0)
    section = b'.text\0\0\0' + struct.pack('<IIIIIIHHI', 0, 0, 16, raw, reloc, 0, count, 0, 0x60000020)
    body = struct.pack('<i', addend) + bytes(range(12))
    records = b''.join(struct.pack('<IIH', i * 4, 0, 4) for i in range(count))
    symbol = b'target\0\0' + struct.pack('<IhHBB', 0, 0, 0, 2, 0)
    return header + section + body + records + symbol + struct.pack('<I', 4)


class RecipeTests(unittest.TestCase):
    def test_coff_five_variants_preserve_expression_and_other_bytes(self):
        for n in range(1, 6):
            original = coff(-n)
            result = recipes.coff_rel32_variant(original, n)
            self.assertEqual(struct.unpack_from('<H', result, 84)[0], 4 + n)
            a = struct.unpack_from('<i', original, 60)[0]
            b = struct.unpack_from('<i', result, 60)[0]
            for s, p in [(0x123400, 0x100000), (0x100000, 0x123400)]:
                self.assertEqual(s + a - (p + 4), s + b - (p + 4 + n))
            self.assertEqual(result[:60], original[:60])
            self.assertEqual(result[64:84], original[64:84])
            self.assertEqual(result[86:], original[86:])

    def test_coff_rejects_bad_shapes(self):
        cases = [coff()[:19], coff()[:-1], coff(count=2), coff(0x7fffffff)]
        for offset, fmt, value in [(0, '<H', 0x14c), (76, '<I', 14), (80, '<I', 1), (32, '<I', 1), (40, '<I', 61)]:
            data = bytearray(coff()); struct.pack_into(fmt, data, offset, value); cases.append(bytes(data))
        for data in cases:
            with self.assertRaises(recipes.RecipeError):
                recipes.coff_rel32_variant(data, 5)
        for n in [0, 6, True, '1']:
            with self.assertRaises(recipes.RecipeError):
                recipes.coff_rel32_variant(coff(), n)

    def test_real_llvm_generator_and_readers(self):
        mc = os.environ.get('LL_FIXTURE_LLVM_MC') or shutil.which('llvm-mc')
        ar = os.environ.get('LL_FIXTURE_LLVM_AR') or shutil.which('llvm-ar')
        clang = os.environ.get('LL_FIXTURE_CLANG') or shutil.which('clang')
        self.assertTrue(mc and ar and clang, 'LLVM tools required')
        nm = str(Path(mc).parent / 'llvm-nm')
        readobj = str(Path(mc).parent / 'llvm-readobj')
        with tempfile.TemporaryDirectory(prefix='fixture-recipes-test-') as temp:
            root = Path(temp)
            def run(args, cwd=root, env_overrides=None):
                result = subprocess.run(args, cwd=cwd, capture_output=True, timeout=30,
                                        env={**os.environ, **(env_overrides or {})})
                self.assertEqual(result.returncode, 0, result.stderr.decode(errors='replace'))
                return result.stdout
            scripts = root / 'scripts'
            scripts.mkdir()
            copied_generator = scripts / 'generate.py'
            copied_helper = scripts / 'fixture_recipes.py'
            shutil.copyfile(generate.__file__, copied_generator)
            shutil.copyfile(recipes.__file__, copied_helper)
            sources = []
            for n in range(1, 6):
                (root / f'v{n}.s').write_text(f'.text\n.globl entry{n}\nentry{n}:\n.long target - . - {4+n}\n.zero {n}\n')
                sources.append({'id': f'v{n}', 'path': f'v{n}.s', 'language': 'asm', 'triple': 'x86_64-w64-windows-gnu', 'object': f'v{n}.o', 'coff_rel32_variant': n})
            catalog = {'schema': 1, 'sources': sources, 'archives': [{'path': 'wide.a', 'format': 'gnu', 'members': [f'v{n}' for n in range(1, 6)], 'symbol_table_bits': 64}, {'path': 'plain.a', 'format': 'gnu', 'members': [f'v{n}' for n in range(1, 6)]}]}
            path = root / 'catalog.json'; path.write_text(json.dumps(catalog))
            output = root / 'output'
            args = ['python3', str(copied_generator), 'regen', '--catalog', str(path), '--output-dir', str(output), '--clang', clang, '--llvm-mc', mc, '--llvm-ar', ar]
            run(args, env_overrides={'SYM64_THRESHOLD': '0'})
            args[2] = 'check'; run(args, env_overrides={'SYM64_THRESHOLD': '0'})
            for n in range(1, 6):
                info = run([readobj, '--relocations', str(output / f'v{n}.o')])
                self.assertIn(f'IMAGE_REL_AMD64_REL32_{n}'.encode(), info)
            self.assertEqual(run([ar, 't', 'wide.a'], output), run([ar, 't', 'plain.a'], output))
            # POSIX nm format omits archive filenames when each member is named explicitly.
            wide = run([nm, '--format=posix', 'wide.a'], output).replace(b'wide.a', b'ARCHIVE')
            plain = run([nm, '--format=posix', 'plain.a'], output).replace(b'plain.a', b'ARCHIVE')
            self.assertEqual(wide, plain)
            wide_bytes = (output / 'wide.a').read_bytes()
            plain_bytes = (output / 'plain.a').read_bytes()
            self.assertEqual(wide_bytes[8:24], b'/SYM64/         ')
            self.assertEqual(plain_bytes[8:24], b'/               ')
            count = struct.unpack_from('>Q', wide_bytes, 68)[0]
            self.assertEqual(count, 5)
            self.assertEqual(struct.unpack_from('>I', plain_bytes, 68)[0], count)
            wide_size = int(wide_bytes[56:66])
            plain_size = int(plain_bytes[56:66])
            shift = wide_size + wide_size % 2 - plain_size - plain_size % 2
            for i in range(count):
                old_offset = struct.unpack_from('>I', plain_bytes, 72 + 4 * i)[0]
                new_offset = struct.unpack_from('>Q', wide_bytes, 76 + 8 * i)[0]
                self.assertEqual(new_offset, old_offset + shift)
                self.assertTrue(wide_bytes[new_offset:new_offset + 16].startswith(f'v{i+1}.o/'.encode()))
            self.assertEqual(wide_bytes[68 + wide_size + wide_size % 2:],
                             plain_bytes[68 + plain_size + plain_size % 2:])
            self.assertEqual(wide_bytes[76 + 8 * count:68 + wide_size],
                             plain_bytes[72 + 4 * count:68 + plain_size])

            # Exercise the real check entry point: changing only helper source
            # must invalidate provenance without touching expected fixtures.
            expected_before = {
                path.relative_to(output): (path.read_bytes(), path.stat().st_mtime_ns)
                for path in output.rglob('*') if path.is_file()
            }
            copied_helper.write_text(copied_helper.read_text() + '\n# Fingerprint regression: no behavior change.\n')
            failed = subprocess.run(args, cwd=root, capture_output=True, timeout=30)
            self.assertNotEqual(failed.returncode, 0)
            self.assertIn(b'recipe_sha256', failed.stderr)
            expected_after = {
                path.relative_to(output): (path.read_bytes(), path.stat().st_mtime_ns)
                for path in output.rglob('*') if path.is_file()
            }
            self.assertEqual(expected_after, expected_before)



if __name__ == '__main__':
    unittest.main()
