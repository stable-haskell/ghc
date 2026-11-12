#!/usr/bin/env python3
"""Exercise catalog-driven generation with installed LLVM tools."""

from __future__ import annotations

import json
import importlib.util
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest import mock


HERE = Path(__file__).resolve().parent
GENERATOR = HERE / "generate.py"
SPEC = importlib.util.spec_from_file_location("fixture_generator", GENERATOR)
assert SPEC is not None and SPEC.loader is not None
fixture_generator = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(fixture_generator)
TRIPLES = {
    "x86_64-linux-gnu": "ret",
    "i386-linux-gnu": "ret",
    "aarch64-linux-gnu": "ret",
    "armv7-linux-gnueabihf": "bx lr",
    "riscv64-linux-gnu": "ret",
    "powerpc64le-linux-gnu": "blr",
    "x86_64-apple-macos11": "retq",
    "arm64-apple-macos11": "ret",
    "x86_64-w64-windows-gnu": "retq",
}


def tool(name: str, env_name: str) -> str:
    value = os.environ.get(env_name) or shutil.which(name)
    if not value:
        raise AssertionError(f"required real fixture tool is missing: set {env_name} or install {name}")
    return value


CLANG = tool("clang", "LL_FIXTURE_CLANG")
LLVM_AR = tool("llvm-ar", "LL_FIXTURE_LLVM_AR")
LLVM_MC = tool("llvm-mc", "LL_FIXTURE_LLVM_MC")


def invoke(operation: str, catalog: Path, output: Path, clang: str = CLANG) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            "python3", str(GENERATOR), operation, "--catalog", str(catalog), "--output-dir", str(output),
            "--clang", clang, "--llvm-ar", LLVM_AR, "--llvm-mc", LLVM_MC,
        ],
        cwd=HERE,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        timeout=180,
        check=False,
    )


def make_project(root: Path) -> tuple[Path, list[Path]]:
    source_root = root / "project"
    source_dir = source_root / "src"
    source_dir.mkdir(parents=True)
    sources = []
    archives = []
    for triple, instruction in TRIPLES.items():
        asm_id = f"{triple}-asm"
        c_id = f"{triple}-c"
        asm_path = f"src/{triple}.s"
        c_path = f"src/{triple}.c"
        (source_root / asm_path).write_text(f".text\n.globl ll_fixture_asm\nll_fixture_asm:\n  {instruction}\n")
        (source_root / c_path).write_text("int ll_fixture_c(void) { return 42; }\n")
        sources.extend((
            {"id": asm_id, "path": asm_path, "language": "asm", "triple": triple,
             "object": f"targets/{triple}/minimal-asm.o"},
            {"id": c_id, "path": c_path, "language": "c", "triple": triple,
             "object": f"targets/{triple}/minimal-c.o"},
        ))
        for archive_format in ("gnu", "bsd", "thin"):
            archives.append({
                "path": f"targets/{triple}/libfixture-{archive_format}.a",
                "format": archive_format,
                "members": [asm_id, c_id],
            })
    catalog = {"schema": 1, "sources": sources, "archives": archives}
    catalog_path = source_root / "catalog.json"
    catalog_path.write_text(json.dumps(catalog, indent=2, sort_keys=True) + "\n")
    return catalog_path, [source_root / item["path"] for item in sources]


def inventory(root: Path) -> dict[str, bytes]:
    return {
        path.relative_to(root).as_posix(): path.read_bytes()
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


class FixtureGeneratorTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.temp = tempfile.TemporaryDirectory(prefix="liblink-fixture-tests-")
        cls.root = Path(cls.temp.name)
        cls.catalog, cls.sources = make_project(cls.root / "one")
        cls.first = cls.root / "one" / "generated"
        result = invoke("regen", cls.catalog, cls.first)
        if result.returncode:
            raise AssertionError(f"regen failed ({result.returncode})\n{result.stdout}\n{result.stderr}")

    @classmethod
    def tearDownClass(cls) -> None:
        cls.temp.cleanup()

    def test_regen_check_and_independent_root_reproducibility(self) -> None:
        checked = invoke("check", self.catalog, self.first)
        self.assertEqual(checked.returncode, 0, checked.stderr)

        second_catalog, _ = make_project(self.root / "different-root")
        second = self.root / "different-root" / "generated"
        regenerated = invoke("regen", second_catalog, second)
        self.assertEqual(regenerated.returncode, 0, regenerated.stderr)
        self.assertEqual(inventory(self.first), inventory(second))

        manifest = json.loads((self.first / "MANIFEST.json").read_text())
        self.assertEqual(set(manifest["targets"]), set(TRIPLES))
        self.assertEqual(manifest["archive_formats"], ["gnu", "bsd", "thin"])
        self.assertNotIn(str(self.root).encode(), (self.first / "MANIFEST.json").read_bytes())
        for triple in TRIPLES:
            target = self.first / "targets" / triple
            self.assertTrue((target / "libfixture-gnu.a").read_bytes().startswith(b"!<arch>\n"))
            self.assertTrue((target / "libfixture-bsd.a").read_bytes().startswith(b"!<arch>\n"))
            thin = (target / "libfixture-thin.a").read_bytes()
            self.assertTrue(thin.startswith(b"!<thin>\n"))
            self.assertNotIn(str(self.root).encode(), thin)
            for archive in ("gnu", "bsd", "thin"):
                listing = subprocess.run(
                    [LLVM_AR, "t", f"libfixture-{archive}.a"], cwd=target,
                    text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=30, check=False,
                )
                self.assertEqual(listing.returncode, 0, listing.stderr)
                self.assertEqual(listing.stdout.splitlines(), ["minimal-asm.o", "minimal-c.o"])

    def test_check_rejects_input_and_output_changes(self) -> None:
        clone = self.root / "corrupt-artifact"
        shutil.copytree(self.first, clone)
        (clone / "targets/x86_64-linux-gnu/minimal-c.o").write_bytes(b"bad")
        self.assertNotEqual(invoke("check", self.catalog, clone).returncode, 0)

        source = self.sources[1]
        source.write_text("int changed(void) { return 7; }\n")
        expected_before = inventory(self.first)
        changed_source = invoke("check", self.catalog, self.first)
        self.assertNotEqual(changed_source.returncode, 0)
        self.assertEqual(inventory(self.first), expected_before)
        source_bytes = b"int ll_fixture_c(void) { return 42; }\n"
        source.write_bytes(source_bytes)

        source.unlink()
        missing_input = invoke("check", self.catalog, self.first)
        self.assertNotEqual(missing_input.returncode, 0)
        source.write_bytes(source_bytes)

        missing = self.root / "missing-input"
        shutil.copytree(self.first, missing)
        (missing / "targets/x86_64-linux-gnu/minimal-asm.o").unlink()
        self.assertNotEqual(invoke("check", self.catalog, missing).returncode, 0)

        unknown = self.root / "unknown-file"
        shutil.copytree(self.first, unknown)
        (unknown / "extra.bin").write_bytes(b"unexpected")
        self.assertNotEqual(invoke("check", self.catalog, unknown).returncode, 0)
        self.assertNotEqual(invoke("check", self.catalog, self.root / "not-generated").returncode, 0)

    def test_major_change_warns_but_still_checks_bytes(self) -> None:
        clone = self.root / "tool-major-change"
        shutil.copytree(self.first, clone)
        manifest_path = clone / "MANIFEST.json"
        manifest = json.loads(manifest_path.read_text())
        current_major = int(manifest["tools"]["llvm-mc"]["major"])
        manifest["tools"]["llvm-mc"]["major"] = str(17 if current_major != 17 else 18)
        manifest_path.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n")
        result = invoke("check", self.catalog, clone)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("major version differs", result.stderr)

        artifact = clone / "targets/x86_64-linux-gnu/minimal-asm.o"
        artifact.write_bytes(b"changed")
        self.assertNotEqual(invoke("check", self.catalog, clone).returncode, 0)

    def test_failed_tool_does_not_create_partial_output(self) -> None:
        fake_clang = self.root / "failing-clang"
        fake_clang.write_text(
            "#!/bin/sh\n"
            "if [ \"$1\" = --version ]; then echo 'clang version 21.0.0'; exit 0; fi\n"
            "echo deliberate compile failure >&2\n"
            "exit 23\n"
        )
        fake_clang.chmod(0o755)
        output = self.root / "failed-output"
        result = invoke("regen", self.catalog, output, str(fake_clang))
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse(output.exists())

        before = inventory(self.first)
        existing_failure = invoke("regen", self.catalog, self.first, str(fake_clang))
        self.assertNotEqual(existing_failure.returncode, 0)
        self.assertEqual(inventory(self.first), before)

    def test_catalog_cannot_overwrite_manifest(self) -> None:
        catalog = json.loads(self.catalog.read_text())
        catalog["sources"][0]["object"] = "MANIFEST.json"
        bad_catalog = self.root / "bad-catalog.json"
        bad_catalog.write_text(json.dumps(catalog))
        output = self.root / "reserved-output"
        result = invoke("regen", bad_catalog, output)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("reserved", result.stderr)
        self.assertFalse(output.exists())

    def test_regen_replaces_nested_artifact_path_with_file(self) -> None:
        project = self.root / "path-transition-project"
        source_dir = project / "src"
        source_dir.mkdir(parents=True)
        (source_dir / "unit.c").write_text("int transition(void) { return 42; }\n")
        source = {"id": "unit", "path": "src/unit.c", "language": "c",
                  "triple": "x86_64-linux-gnu"}
        old_catalog = project / "old.json"
        old_catalog.write_text(json.dumps({
            "schema": 1,
            "sources": [{**source, "object": "x/old/sub.o"}],
            "archives": [{"path": "x/old/old.a", "format": "gnu", "members": ["unit"]}],
        }))
        output = self.root / "path-transition-output"
        first = invoke("regen", old_catalog, output)
        self.assertEqual(first.returncode, 0, first.stderr)
        self.assertTrue((output / "x/old/sub.o").is_file())

        new_catalog = project / "new.json"
        new_catalog.write_text(json.dumps({
            "schema": 1,
            "sources": [{**source, "object": "x/old"}],
            "archives": [{"path": "x/new.a", "format": "gnu", "members": ["unit"]}],
        }))
        promoted = invoke("regen", new_catalog, output)
        self.assertEqual(promoted.returncode, 0, promoted.stderr)
        self.assertTrue((output / "x/old").is_file())
        self.assertFalse((output / "x/old/sub.o").exists())
        self.assertEqual(invoke("check", new_catalog, output).returncode, 0)

    def test_failed_tree_promotion_restores_previous_output(self) -> None:
        root = self.root / "promotion-failure"
        root.mkdir()
        output = root / "output"
        output.mkdir()
        (output / "old.txt").write_text("previous fixture bytes\n")
        staged = root / "staged"
        staged.mkdir()
        (staged / "new.txt").write_text("new fixture bytes\n")
        real_replace = os.replace
        injected = False

        def fail_staged_promotion(source: object, destination: object) -> None:
            nonlocal injected
            if Path(source) == staged and Path(destination) == output and not injected:
                injected = True
                raise OSError("injected promotion failure")
            real_replace(source, destination)

        with mock.patch.object(fixture_generator.os, "replace", side_effect=fail_staged_promotion):
            with self.assertRaises(fixture_generator.FixtureError):
                fixture_generator._promote_tree(staged, output)
        self.assertTrue(injected)
        self.assertEqual((output / "old.txt").read_text(), "previous fixture bytes\n")
        self.assertFalse((output / "new.txt").exists())
        self.assertEqual((staged / "new.txt").read_text(), "new fixture bytes\n")


if __name__ == "__main__":
    unittest.main()
