#!/usr/bin/env python3
"""Generate deterministic object and archive fixtures from a JSON catalog."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile

import fixture_recipes


SCHEMA = 1
TIMEOUT_SECONDS = 30
TARGETS = frozenset({
    "x86_64-linux-gnu",
    "i386-linux-gnu",
    "aarch64-linux-gnu",
    "armv7-linux-gnueabihf",
    "riscv64-linux-gnu",
    "powerpc64le-linux-gnu",
    "x86_64-apple-macos11",
    "arm64-apple-macos11",
    "x86_64-w64-windows-gnu",
})
ARCHIVES = {"gnu": ("--format=gnu",), "bsd": ("--format=bsd",), "thin": ("--format=gnu", "--thin")}
TOOL_NAMES = ("clang", "llvm-ar", "llvm-mc")
ID_PATTERN = re.compile(r"[A-Za-z0-9][A-Za-z0-9_.-]*\Z")


class FixtureError(Exception):
    pass


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _tool_path(value: str | None, name: str, required: bool = False) -> Path:
    found = value or shutil.which(name)
    if not found and required:
        raise FixtureError(f"required tool not found: {name}; pass --{name}")
    if not found:
        raise FixtureError(f"tool not found: {name}")
    path = Path(found).resolve()
    if not path.is_file():
        raise FixtureError(f"tool is not a file: {path}")
    return path


def _run(argv: list[str], cwd: Path, env_overrides: dict[str, str] | None = None) -> subprocess.CompletedProcess[bytes]:
    try:
        result = subprocess.run(
            argv, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            timeout=TIMEOUT_SECONDS, check=False,
            env={**os.environ, **(env_overrides or {})},
        )
    except (OSError, subprocess.TimeoutExpired) as exc:
        raise FixtureError(f"command failed within {TIMEOUT_SECONDS}s: {argv[0]}: {exc}") from exc
    if result.returncode:
        stderr = (result.stderr or b"").decode("utf-8", "replace").strip()
        raise FixtureError(f"command exited {result.returncode}: {argv!r}\n{stderr}")
    return result


def _tool_version(path: Path, cwd: Path) -> dict[str, str]:
    result = _run([str(path), "--version"], cwd)
    text = ((result.stdout or b"") + (result.stderr or b"")).decode("utf-8", "replace")
    lines = text.splitlines()
    line = next((s.strip() for s in lines if re.search(r"(?:LLVM|clang) version\s+\d+", s, re.I)), "")
    match = re.search(r"(?:LLVM|clang) version\s+(\d+)(?:\.|\b)", line, re.I)
    if not match:
        raise FixtureError(f"cannot read major version from {path.name}: {text[:240]!r}")
    major = int(match.group(1))
    if major < 17:
        raise FixtureError(f"{path.name} major {major} is unsupported; LLVM 17 or newer is required")
    return {"version": line, "major": str(major)}


def _safe_relative(value: object, label: str) -> str:
    if not isinstance(value, str) or not value:
        raise FixtureError(f"{label} must be a non-empty relative path")
    path = Path(value)
    if "\x00" in value or "\\" in value or ":" in value or path.is_absolute() or ".." in path.parts or path.as_posix() != value or value in (".", ".."):
        raise FixtureError(f"unsafe {label}: {value!r}")
    return value


def _safe_output(value: object, label: str) -> str:
    name = _safe_relative(value, label)
    if name == "MANIFEST.json" or name.startswith("MANIFEST.json/"):
        raise FixtureError(f"{label} is reserved for the generated manifest: {name!r}")
    return name


def _load_catalog(path: Path) -> tuple[dict[str, object], Path, bytes]:
    path = path.resolve()
    try:
        raw = path.read_bytes()
        catalog = json.loads(raw)
    except (OSError, json.JSONDecodeError) as exc:
        raise FixtureError(f"cannot read catalog {path}: {exc}") from exc
    if not isinstance(catalog, dict) or catalog.get("schema") != SCHEMA:
        raise FixtureError("catalog must be a JSON object with schema 1")
    sources = catalog.get("sources")
    archives = catalog.get("archives")
    if not isinstance(sources, list) or not sources or not isinstance(archives, list) or not archives:
        raise FixtureError("catalog requires non-empty sources and archives arrays")

    ids: set[str] = set()
    outputs: set[str] = set()
    source_root = path.parent
    for source in sources:
        if not isinstance(source, dict):
            raise FixtureError("each source entry must be an object")
        source_id = source.get("id")
        if not isinstance(source_id, str) or not ID_PATTERN.fullmatch(source_id) or source_id in ids:
            raise FixtureError(f"invalid or duplicate source id: {source_id!r}")
        ids.add(source_id)
        source_path = _safe_relative(source.get("path"), f"source path for {source_id}")
        language = source.get("language")
        if language not in ("c", "asm"):
            raise FixtureError(f"unsupported source language for {source_id}: {language!r}")
        if Path(source_path).suffix != (".c" if language == "c" else ".s"):
            raise FixtureError(f"source extension does not match language for {source_id}")
        triple = source.get("triple")
        if triple not in TARGETS:
            raise FixtureError(f"unsupported target triple for {source_id}: {triple!r}")
        if "coff_rel32_variant" in source:
            variant = source["coff_rel32_variant"]
            if type(variant) is not int or not 1 <= variant <= 5 or triple != "x86_64-w64-windows-gnu" or language != "asm":
                raise FixtureError("coff_rel32_variant requires AMD64 COFF assembly and integer 1..5")
        output = _safe_output(source.get("object"), f"object output for {source_id}")
        if output in outputs:
            raise FixtureError(f"duplicate object output for {source_id}: {output!r}")
        outputs.add(output)
        resolved = source_root / source_path
        component_path = source_root
        linked_component = False
        for part in Path(source_path).parts:
            component_path = component_path / part
            linked_component = linked_component or component_path.is_symlink()
        if linked_component or not resolved.is_file():
            raise FixtureError(f"source input is missing or is a symlink: {source_path}")
        try:
            resolved.resolve().relative_to(source_root)
        except ValueError as exc:
            raise FixtureError(f"source input escapes catalog directory: {source_path}") from exc

    archive_paths: set[str] = set()
    for archive in archives:
        if not isinstance(archive, dict):
            raise FixtureError("each archive entry must be an object")
        output = _safe_output(archive.get("path"), "archive path")
        if not output.endswith(".a") or output in outputs or output in archive_paths:
            raise FixtureError(f"invalid or duplicate archive path: {output!r}")
        archive_paths.add(output)
        kind = archive.get("format")
        if kind not in ARCHIVES:
            raise FixtureError(f"unsupported archive format: {kind!r}")
        if "symbol_table_bits" in archive:
            bits = archive["symbol_table_bits"]
            if type(bits) is not int or bits != 64 or kind != "gnu":
                raise FixtureError("symbol_table_bits must be 64 for a GNU archive")
        members = archive.get("members")
        if not isinstance(members, list) or not members or any(not isinstance(item, str) for item in members):
            raise FixtureError(f"archive {output} requires a non-empty member id list")
        if len(set(members)) != len(members) or any(item not in ids for item in members):
            raise FixtureError(f"archive {output} has duplicate or unknown member ids")
        member_sources = [next(item for item in sources if item["id"] == member) for member in members]
        triples = {item["triple"] for item in member_sources}
        if len(triples) != 1:
            raise FixtureError(f"archive {output} mixes target triples")
        archive_parent = Path(output).parent
        object_parents = {Path(item["object"]).parent for item in member_sources}
        if object_parents != {archive_parent}:
            raise FixtureError(f"archive {output} members must be in the same directory")
    return catalog, source_root, raw


def _tree_files(root: Path) -> dict[str, bytes]:
    files: dict[str, bytes] = {}
    for path in sorted(root.rglob("*")):
        if path.is_symlink():
            raise FixtureError(f"generated fixture tree contains a symlink: {path.relative_to(root)}")
        if path.is_file():
            files[path.relative_to(root).as_posix()] = path.read_bytes()
    return files


def _dirs_for(files: set[str]) -> set[str]:
    dirs: set[str] = set()
    for name in files:
        parent = Path(name).parent
        while str(parent) != ".":
            dirs.add(parent.as_posix())
            parent = parent.parent
    return dirs


def _manifest_inventory(manifest: dict[str, object]) -> set[str]:
    sources = manifest.get("sources")
    artifacts = manifest.get("artifacts")
    if not isinstance(sources, dict) or not isinstance(artifacts, dict):
        raise FixtureError("manifest must contain source and artifact maps")
    names = set(artifacts)
    for name in names:
        if _safe_output(name, "manifest path") != name:
            raise FixtureError(f"invalid manifest artifact path: {name!r}")
    return names | {"MANIFEST.json"}


def _verify_managed_tree(root: Path) -> dict[str, object]:
    manifest_path = root / "MANIFEST.json"
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise FixtureError(f"invalid or missing fixture manifest: {manifest_path}: {exc}") from exc
    if not isinstance(manifest, dict) or manifest.get("schema") != SCHEMA:
        raise FixtureError(f"unsupported fixture manifest schema in {manifest_path}")
    expected = _manifest_inventory(manifest)
    actual_files = _tree_files(root)
    if set(actual_files) != expected:
        raise FixtureError(
            f"managed tree inventory changed; missing={sorted(expected - set(actual_files))}, "
            f"unknown={sorted(set(actual_files) - expected)}"
        )
    actual_dirs = {p.relative_to(root).as_posix() for p in root.rglob("*") if p.is_dir()}
    if actual_dirs != _dirs_for(expected):
        raise FixtureError("managed tree directory inventory changed")
    records = manifest["artifacts"]
    assert isinstance(records, dict)
    for name, record in records.items():
        if not isinstance(record, dict):
            raise FixtureError(f"invalid manifest record: artifacts/{name}")
        data = actual_files[name]
        if record.get("sha256") != sha256(data) or record.get("size") != len(data):
            raise FixtureError(f"fixture bytes differ from manifest: {name}")
    return manifest


def _generate_tree(
    root: Path,
    catalog: dict[str, object],
    source_root: Path,
    catalog_bytes: bytes,
    tools: dict[str, Path],
    versions: dict[str, dict[str, str]],
) -> dict[str, object]:
    sources = catalog["sources"]
    archives = catalog["archives"]
    assert isinstance(sources, list) and isinstance(archives, list)
    source_records: dict[str, dict[str, object]] = {}
    source_by_id = {item["id"]: item for item in sources}

    for item in sources:
        source_path = item["path"]
        data = (source_root / source_path).read_bytes()
        source_records[source_path] = {"sha256": sha256(data), "size": len(data)}
        output = root / item["object"]
        output.parent.mkdir(parents=True, exist_ok=True)
        if item["language"] == "asm":
            argv = [str(tools["llvm-mc"]), f"-triple={item['triple']}", "-filetype=obj", source_path, "-o", str(output)]
        else:
            argv = [
                str(tools["clang"]), f"--target={item['triple']}", "-std=c11", "-Wall", "-Wextra", "-Werror=vla", "-pedantic",
                "-c", "-O1", "-fno-ident", f"-ffile-prefix-map={source_root.resolve()}=.",
                "-fno-asynchronous-unwind-tables", "-ffreestanding", source_path, "-o", str(output),
            ]
        _run(argv, source_root)
        if "coff_rel32_variant" in item:
            try:
                output.write_bytes(fixture_recipes.coff_rel32_variant(output.read_bytes(), item["coff_rel32_variant"]))
            except fixture_recipes.RecipeError as exc:
                raise FixtureError(f"COFF recipe for {source_path}: {exc}") from exc

    for archive in archives:
        output = root / archive["path"]
        output.parent.mkdir(parents=True, exist_ok=True)
        member_names = [Path(source_by_id[name]["object"]).name for name in archive["members"]]
        args = [str(tools["llvm-ar"]), *ARCHIVES[archive["format"]], "rcsD", output.name, *member_names]
        wide_index = archive.get("symbol_table_bits") == 64
        # LLVM's own archive tests use this knob to avoid multi-gigabyte inputs.
        # Pin the default too, so ambient SYM64_THRESHOLD cannot change recipes.
        _run(args, output.parent, {"SYM64_THRESHOLD": "0" if wide_index else str(1 << 32)})
        if wide_index and output.read_bytes()[:24] != b"!<arch>\n/SYM64/         ":
            raise FixtureError(f"LLVM did not emit a GNU64 symbol index for {archive['path']}")
        listed = _run([str(tools["llvm-ar"]), "t", output.name], output.parent)
        members = (listed.stdout or b"").decode("utf-8", "replace").splitlines()
        if members != member_names:
            raise FixtureError(f"unexpected members in {archive['path']}: {members!r}")

    files = _tree_files(root)
    artifacts = {
        name: {"sha256": sha256(data), "size": len(data)}
        for name, data in sorted(files.items())
    }
    targets = sorted({item["triple"] for item in sources})
    formats = list(dict.fromkeys(item["format"] for item in archives))
    return {
        "schema": SCHEMA,
        "recipe_sha256": sha256(Path(fixture_recipes.__file__).read_bytes()),
        "generator_sha256": sha256(Path(__file__).read_bytes()),
        "catalog_sha256": sha256(catalog_bytes),
        "targets": targets,
        "archive_formats": formats,
        "tools": {name: versions[name] for name in TOOL_NAMES},
        "sources": source_records,
        "artifacts": artifacts,
    }


def _toolchain(args: argparse.Namespace, cwd: Path) -> tuple[dict[str, Path], dict[str, dict[str, str]]]:
    tools = {
        "clang": _tool_path(args.clang, "clang", required=True),
        "llvm-ar": _tool_path(args.llvm_ar, "llvm-ar"),
        "llvm-mc": _tool_path(args.llvm_mc, "llvm-mc"),
    }
    versions = {name: _tool_version(tools[name], cwd) for name in TOOL_NAMES}
    return tools, versions


def _warn_tool_major_changes(expected: dict[str, object], current: dict[str, dict[str, str]]) -> None:
    old_tools = expected.get("tools")
    if not isinstance(old_tools, dict):
        raise FixtureError("manifest is missing tool versions")
    for name in TOOL_NAMES:
        old = old_tools.get(name)
        if not isinstance(old, dict) or not isinstance(old.get("major"), str):
            raise FixtureError(f"manifest has invalid tool version for {name}")
        if old["major"] != current[name]["major"]:
            print(
                f"warning: {name} major version differs: fixture={old['major']} current={current[name]['major']}; byte checks still apply",
                file=sys.stderr,
            )


def _compare_manifests(expected: dict[str, object], actual: dict[str, object]) -> list[str]:
    return [
        key for key in ("schema", "recipe_sha256", "generator_sha256", "catalog_sha256", "targets", "archive_formats", "sources", "artifacts")
        if expected.get(key) != actual.get(key)
    ]


def _promote_tree(staged: Path, output: Path) -> None:
    if not output.exists():
        try:
            os.replace(staged, output)
        except OSError as exc:
            raise FixtureError(f"cannot install generated fixture tree at {output}: {exc}") from exc
        return

    backup_root = Path(tempfile.mkdtemp(prefix=f".{output.name}-previous-", dir=output.parent))
    backup = backup_root / "tree"
    try:
        os.replace(output, backup)
    except OSError as exc:
        backup_root.rmdir()
        raise FixtureError(f"cannot preserve previous fixtures at {output}: {exc}") from exc
    try:
        os.replace(staged, output)
    except OSError as promotion_error:
        try:
            os.replace(backup, output)
        except OSError as restore_error:
            preserved = backup_root
            raise FixtureError(
                f"fixture promotion failed ({promotion_error}); previous tree preserved at {preserved} "
                f"but could not be restored ({restore_error})"
            ) from restore_error
        shutil.rmtree(backup_root)
        raise FixtureError(f"fixture promotion failed; previous tree restored: {promotion_error}") from promotion_error
    shutil.rmtree(backup_root)


def regen(output: Path, catalog_path: Path, tools: dict[str, Path], versions: dict[str, dict[str, str]]) -> None:
    output = output.resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    catalog, source_root, catalog_bytes = _load_catalog(catalog_path)
    if output.exists():
        if not output.is_dir():
            raise FixtureError(f"output path is not a directory: {output}")
        if any(output.iterdir()):
            _verify_managed_tree(output)

    with tempfile.TemporaryDirectory(prefix="liblink-fixtures-", dir=output.parent) as temporary:
        staged = Path(temporary) / "tree"
        staged.mkdir()
        manifest = _generate_tree(staged, catalog, source_root, catalog_bytes, tools, versions)
        (staged / "MANIFEST.json").write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        _promote_tree(staged, output)


def check(output: Path, catalog_path: Path, tools: dict[str, Path], versions: dict[str, dict[str, str]]) -> None:
    output = output.resolve()
    expected = _verify_managed_tree(output)
    _warn_tool_major_changes(expected, versions)
    catalog, source_root, catalog_bytes = _load_catalog(catalog_path)
    with tempfile.TemporaryDirectory(prefix="liblink-fixtures-check-") as temporary:
        generated_root = Path(temporary) / "tree"
        generated_root.mkdir()
        actual = _generate_tree(generated_root, catalog, source_root, catalog_bytes, tools, versions)
        changed = _compare_manifests(expected, actual)
        if changed:
            raise FixtureError(f"fixture check differs in manifest fields: {', '.join(changed)}")
        for name, data in _tree_files(generated_root).items():
            if (output / name).read_bytes() != data:
                raise FixtureError(f"fixture bytes differ: {name}")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="operation", required=True)
    for operation in ("regen", "check"):
        command = subparsers.add_parser(operation)
        command.add_argument("--catalog", type=Path, required=True)
        command.add_argument("--output-dir", type=Path, required=True)
        command.add_argument("--clang", required=True, help="path to an unwrapped LLVM clang executable")
        command.add_argument("--llvm-mc")
        command.add_argument("--llvm-ar")
    args = parser.parse_args(argv)
    try:
        output = args.output_dir
        output.parent.mkdir(parents=True, exist_ok=True)
        catalog, source_root, _ = _load_catalog(args.catalog)
        del catalog
        tools, versions = _toolchain(args, source_root)
        if args.operation == "regen":
            regen(output, args.catalog, tools, versions)
        else:
            check(output, args.catalog, tools, versions)
    except FixtureError as exc:
        print(f"fixture generator: {exc}", file=sys.stderr)
        return 1
    print(f"fixture {args.operation}: {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
