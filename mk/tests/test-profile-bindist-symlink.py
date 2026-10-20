#!/usr/bin/env python3
"""Run the real profiling bindist recipe with a minimal target fixture."""

import argparse
import shlex
import subprocess
import tarfile
import tempfile
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
MACHINE = "aarch64-apple-darwin"
PROFILE_TARGET = MACHINE + "-prof"
SHORT_ALIAS = "aarch64-darwin-prof"
TARGET_UNLIT = f"lib/targets/{PROFILE_TARGET}/bin/unlit"


def make_executable(path, content):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("#!/bin/sh\n" + content)
    path.chmod(0o755)


def make_fixture(root, create_stage2_stamp=True):
    dist = root / "_build/dist"
    make_executable(dist / "bin/unlit", "printf 'native unlit\\n'\n")
    make_executable(dist / "bin/ghc", "printf 'native ghc\\n'\n")
    target_bin = dist / "bin"
    for name in (PROFILE_TARGET, SHORT_ALIAS):
        (target_bin / (name + "-ghc")).symlink_to("ghc")

    (dist / "lib/ghc-usage.txt").parent.mkdir(parents=True, exist_ok=True)
    (dist / "lib/ghc-usage.txt").write_text("GHC usage\n")
    (dist / "lib/ghci-usage.txt").write_text("GHCi usage\n")
    (dist / "lib/settings").write_text("[]\n")
    (dist / "lib/template-hsc.h").write_text("/* template */\n")
    (dist / "lib/package.conf.d").mkdir()
    (dist / f"lib/{MACHINE}").mkdir()

    # Make is invoked from this private source fixture, which owns the
    # authoritative source copies used by its lightweight table targets.
    if create_stage2_stamp:
        (root / "_build/.stamp-stage2").touch()
    original_profile_tables = root / "original profile tables"
    original_profile_tables.mkdir()
    for name in ("llvm-targets", "llvm-passes"):
        source = ROOT / name
        data = source.read_bytes()
        assert data, f"authoritative LLVM table is unexpectedly empty: {source}"
        (root / name).write_bytes(data)
        (original_profile_tables / name).write_bytes(data)

    target = dist / "lib/targets" / PROFILE_TARGET
    target_bin = target / "bin"
    target_bin.mkdir(parents=True)
    (target_bin / "unlit").symlink_to("../../../../bin/unlit")
    target_lib = target / "lib"
    target_lib.mkdir()
    for name in ("llvm-targets", "llvm-passes"):
        (target_lib / name).write_bytes((root / "original profile tables" / name).read_bytes())
    (target / "target.aliases").write_text(repr([SHORT_ALIAS]) + "\n")
    (target / "target.executables").write_text(
        f"bin/{PROFILE_TARGET}-ghc\nbin/{SHORT_ALIAS}-ghc\n")

    # The recipe copies these bindist support files from its working directory.
    (root / "mk").mkdir()
    for name in ("multi-target-relocate.sh", "multi-target-configure.sh",
                 "multi-target-bindist-Makefile"):
        source = ROOT / "mk" / name
        (root / "mk" / name).write_bytes(source.read_bytes())
    (root / "config.sub").write_bytes((ROOT / "config.sub").read_bytes())


def build_with_real_make_recipe(root, make_command, dist_build=False):
    archive = root / "_build/dist/ghc-profile.tar.gz"
    command = [*make_command]
    if dist_build:
        command.append("DIST_BUILD=1")
    else:
        command.extend(["-o", "_build/.stamp-stage2"])
    command.extend([
        "-o", "stage3-profile",
        "-f", str(ROOT / "Makefile"),
        "GHC0=:",
        "HOST_PLATFORM=" + MACHINE,
        "STAGE2_PLATFORM=" + MACHINE,
        "BOOTSTRAP_TARGET=" + MACHINE,
        "DYNAMIC=0",
        "STAGE2_EXECUTABLES=unlit ghc",
        "_build/dist/ghc-profile.tar.gz",
    ])
    subprocess.run(command, cwd=root, check=True, text=True, timeout=60)
    assert archive.is_file(), "real Makefile recipe did not create the profiling archive"
    return archive


def check_archive_table_bytes(archive, root):
    with tarfile.open(archive, "r:gz") as tar:
        for name in ("llvm-targets", "llvm-passes"):
            expected_native = (root / name).read_bytes()
            expected_profile = (root / "original profile tables" / name).read_bytes()
            native = tar.extractfile(f"lib/{name}")
            assert native is not None, f"missing native LLVM table lib/{name}"
            assert native.read() == expected_native, f"native LLVM table bytes changed: {name}"
            profile_name = f"lib/targets/{PROFILE_TARGET}/lib/{name}"
            profile = tar.extractfile(profile_name)
            assert profile is not None, f"missing profile LLVM table {profile_name}"
            assert profile.read() == expected_profile, \
                f"profile LLVM table bytes changed: {profile_name}"


def check_archive_and_extraction(archive, root):
    with tarfile.open(archive, "r:gz") as tar:
        member = tar.getmember(TARGET_UNLIT)
        assert member.issym(), f"archive member is not a symlink: {member.type!r}"
        assert member.linkname == "../../../../bin/unlit", member.linkname
        for name in (PROFILE_TARGET + "-ghc", SHORT_ALIAS + "-ghc"):
            alias = tar.getmember("bin/" + name)
            assert alias.issym(), f"entry point is not a symlink: {name}"
    check_archive_table_bytes(archive, root)

    extracted = root / "install prefix with spaces"
    extracted.mkdir()
    subprocess.run(["tar", "xzf", str(archive), "-C", str(extracted)], check=True)
    target_unlit = extracted / TARGET_UNLIT
    assert target_unlit.is_symlink(), "extracted target unlit is not a symlink"
    assert target_unlit.resolve(strict=True).samefile(extracted / "bin/unlit")
    assert subprocess.run([str(target_unlit)], check=True, capture_output=True,
                          text=True, timeout=10).stdout.strip() == "native unlit"
    for name in (PROFILE_TARGET + "-ghc", SHORT_ALIAS + "-ghc"):
        assert (extracted / "bin" / name).resolve(strict=True).samefile(extracted / "bin/ghc")


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--make", default="make",
                        help="Make command and arguments (default: make)")
    args = parser.parse_args(argv)
    make_command = shlex.split(args.make)
    if not make_command:
        parser.error("--make must name a command")
    with tempfile.TemporaryDirectory(prefix="profile-bindist-recipe-") as scratch:
        root = Path(scratch)
        make_fixture(root)
        archive = build_with_real_make_recipe(root, make_command)
        check_archive_and_extraction(archive, root)
        # Make's timestamp resolution varies by filesystem; ensure source is
        # edited after the copied distribution output without skewing the
        # fixture clock into the future.
        time.sleep(1.1)
        source = root / "llvm-targets"
        source.write_bytes(source.read_bytes() + b"\n")
        build_with_real_make_recipe(root, make_command)
        check_archive_table_bytes(archive, root)

    with tempfile.TemporaryDirectory(prefix="profile-bindist-dist-recipe-") as scratch:
        root = Path(scratch)
        make_fixture(root, create_stage2_stamp=False)
        archive = build_with_real_make_recipe(root, make_command, dist_build=True)
        check_archive_and_extraction(archive, root)
    print("real profiling bindist recipe preserves target symlinks")


if __name__ == "__main__":
    main()
