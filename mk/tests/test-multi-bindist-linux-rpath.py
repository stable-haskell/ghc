#!/usr/bin/env python3
"""Exercise Linux multi-bindist native library-dir discovery and relocation."""

import os
import platform
import shutil
import subprocess
import tarfile
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
NORMALIZER = ROOT / "mk/normalize-multi-bindist-linux.sh"
HOST_DIR = "aarch64-unknown-linux-gnu"
GHC_SO = "libHSghc-9.14-ghc9.14.so"
CABAL_SO = "libHSCabal-3.17.0.1-ghc9.14.so"
DECOY_GHC_SO = "libHSghc-boot-9.14-ghc9.14.so"
DECOY_CABAL_SO = "libHSCabal-syntax-3.17.0.0-ghc9.14.so"
PROBE_SOURCE = "int ghc_runtime_probe(void) { return 0; }\n"
MAIN_SOURCE = (
    "int ghc_runtime_probe(void);\n"
    "int main(void) { return ghc_runtime_probe(); }\n"
)


def archive_tree(root, archive):
    with tarfile.open(archive, "w:gz") as tar:
        for path in sorted(root.rglob("*")):
            tar.add(path, arcname=path.relative_to(root))


def add_marker(root, host_dir):
    marker = root / "lib" / host_dir / GHC_SO
    marker.parent.mkdir(parents=True, exist_ok=True)
    marker.write_text("marker\n")


def run_normalizer(archive):
    return subprocess.run(
        [str(NORMALIZER), str(archive), "/lib/ld-linux-aarch64.so.1"],
        capture_output=True,
        text=True,
        timeout=30,
    )


def test_invalid_native_library_dirs():
    with tempfile.TemporaryDirectory(prefix="multi-bindist-invalid-") as scratch:
        root = Path(scratch)
        missing = root / "missing.tar.gz"
        empty_tree = root / "empty"
        empty_tree.mkdir()
        archive_tree(empty_tree, missing)
        result = run_normalizer(missing)
        assert result.returncode != 0, "normalizer accepted a bindist without native GHC libraries"
        assert "exactly one native libHSghc directory" in result.stderr, result.stderr

        decoys_root = root / "decoys"
        decoy_dir = decoys_root / "lib" / HOST_DIR
        decoy_dir.mkdir(parents=True)
        (decoy_dir / DECOY_GHC_SO).write_text("not the ghc package\n")
        (decoy_dir / DECOY_CABAL_SO).write_text("not the Cabal package\n")
        decoys = root / "decoys.tar.gz"
        archive_tree(decoys_root, decoys)
        result = run_normalizer(decoys)
        assert result.returncode != 0, "normalizer accepted ghc-boot and Cabal-syntax as native markers"
        assert "exactly one native libHSghc directory" in result.stderr, result.stderr

        misleading_root = root / "misleading"
        add_marker(misleading_root, HOST_DIR)
        cabal_decoy = misleading_root / "lib" / HOST_DIR / DECOY_CABAL_SO
        cabal_decoy.write_text("not the Cabal package\n")
        misleading = root / "misleading.tar.gz"
        archive_tree(misleading_root, misleading)
        result = run_normalizer(misleading)
        assert result.returncode != 0, "normalizer accepted Cabal-syntax as the Cabal library marker"
        assert "lacks libHSCabal shared library" in result.stderr, result.stderr

        for label, cabal_marker_kind in (("directory", "directory"), ("dangling", "symlink")):
            invalid_root = root / ("invalid-cabal-" + label)
            add_marker(invalid_root, HOST_DIR)
            cabal_marker = invalid_root / "lib" / HOST_DIR / CABAL_SO
            if cabal_marker_kind == "directory":
                cabal_marker.mkdir()
            else:
                cabal_marker.symlink_to("missing-cabal-library.so")
            invalid_archive = root / ("invalid-cabal-" + label + ".tar.gz")
            archive_tree(invalid_root, invalid_archive)
            result = run_normalizer(invalid_archive)
            assert result.returncode != 0, (
                "normalizer accepted a Cabal package name that is not a regular library file"
            )
            assert "lacks libHSCabal shared library" in result.stderr, result.stderr

        ambiguous_root = root / "ambiguous"
        add_marker(ambiguous_root, "aarch64-unknown-linux")
        add_marker(ambiguous_root, HOST_DIR)
        ambiguous = root / "ambiguous.tar.gz"
        archive_tree(ambiguous_root, ambiguous)
        result = run_normalizer(ambiguous)
        assert result.returncode != 0, "normalizer accepted ambiguous native library directories"
        assert "exactly one native libHSghc directory" in result.stderr, result.stderr


def test_portable_full_triple_selection():
    with tempfile.TemporaryDirectory(prefix="multi-bindist-path-") as scratch:
        root = Path(scratch)
        bindist = root / "bindist"
        libdir = bindist / "lib" / HOST_DIR
        bindir = bindist / "bin"
        libdir.mkdir(parents=True)
        bindir.mkdir()
        (libdir / GHC_SO).write_text("native library marker\n")
        (libdir / CABAL_SO).write_text("Cabal library marker\n")
        executable = bindir / "ghc"
        executable.write_text("#!/bin/sh\nexit 0\n")
        executable.chmod(0o755)
        archive = root / "bindist.tar.gz"
        archive_tree(bindist, archive)

        fake_bin = root / "fake-bin"
        fake_bin.mkdir()
        fake_file = fake_bin / "file"
        fake_file.write_text(
            "#!/bin/sh\n"
            "printf '%s: ELF 64-bit dynamically linked, interpreter /nix/store/test\\n' \"$2\"\n"
        )
        fake_file.chmod(0o755)
        patchelf_log = root / "patchelf.log"
        fake_patchelf = fake_bin / "patchelf"
        fake_patchelf.write_text(
            "#!/bin/sh\n"
            "printf '%s\\n' \"$*\" >> \"$PATCHELF_LOG\"\n"
        )
        fake_patchelf.chmod(0o755)
        env = dict(os.environ)
        env["PATH"] = str(fake_bin) + os.pathsep + env["PATH"]
        env["PATCHELF_LOG"] = str(patchelf_log)
        subprocess.run(
            [str(NORMALIZER), str(archive), "/lib/ld-linux-aarch64.so.1"],
            env=env,
            check=True,
            timeout=30,
        )
        commands = patchelf_log.read_text()
        assert "$ORIGIN/../lib/" + HOST_DIR in commands, commands
        assert "--set-rpath $ORIGIN " in commands, commands


def test_linux_elf_relocation():
    if platform.system() != "Linux":
        print("skipping ELF execution probe outside Linux")
        return

    compiler = shutil.which("cc") or shutil.which("gcc")
    patchelf = shutil.which("patchelf")
    assert compiler is not None, "Linux relocation regression requires a C compiler"
    assert patchelf is not None, "Linux relocation regression requires patchelf"
    compiler_flags = ["-std=c11", "-Werror=vla", "-pedantic", "-Wall", "-Wextra", "-Werror"]

    with tempfile.TemporaryDirectory(prefix="multi-bindist-elf-") as scratch:
        root = Path(scratch)
        bindist = root / "bindist"
        libdir = bindist / "lib" / HOST_DIR
        bindir = bindist / "bin"
        target_payload = bindist / "lib/targets/wasm32-unknown-wasi/lib"
        libdir.mkdir(parents=True)
        bindir.mkdir()
        target_payload.mkdir(parents=True)

        library_source = root / "probe.c"
        library_source.write_text(PROBE_SOURCE)
        main_source = root / "main.c"
        main_source.write_text(MAIN_SOURCE)
        ghc_library = libdir / GHC_SO
        subprocess.run(
            [compiler, *compiler_flags, "-shared", "-fPIC",
             "-Wl,-soname," + GHC_SO, "-o", str(ghc_library), str(library_source)],
            check=True,
            timeout=30,
        )
        shutil.copy2(ghc_library, libdir / CABAL_SO)
        (target_payload / "payload.txt").write_text("target payload remains present\n")

        executable = bindir / "ghc"
        subprocess.run(
            [compiler, *compiler_flags, str(main_source),
             "-L" + str(libdir), "-Wl,-l:" + GHC_SO,
             "-Wl,-rpath,$ORIGIN/../lib/aarch64-unknown-linux", "-o", str(executable)],
            check=True,
            timeout=30,
        )
        interpreter = subprocess.run(
            [patchelf, "--print-interpreter", str(executable)],
            check=True,
            capture_output=True,
            text=True,
            timeout=30,
        ).stdout.strip()
        assert interpreter.startswith("/"), interpreter
        subprocess.run(
            [patchelf, "--set-interpreter", "/nix/store/missing-ld-linux", str(executable)],
            check=True,
            timeout=30,
        )
        subprocess.run(
            [patchelf, "--force-rpath", "--set-rpath",
             "$ORIGIN/../lib/aarch64-unknown-linux", str(executable)],
            check=True,
            timeout=30,
        )
        try:
            result = subprocess.run([str(executable)], capture_output=True, timeout=10)
        except OSError:
            # The deliberately invalid interpreter can make execve itself fail.
            pass
        else:
            assert result.returncode != 0, "bad Nix interpreter unexpectedly executed"

        archive = root / "ghc-multi-target.tar.gz"
        archive_tree(bindist, archive)
        subprocess.run(
            [str(NORMALIZER), str(archive), interpreter],
            check=True,
            timeout=60,
        )

        installed = root / "installed"
        installed.mkdir()
        with tarfile.open(archive, "r:gz") as tar:
            tar.extractall(installed)
        installed_exe = installed / "bin/ghc"
        rpath = subprocess.run(
            [patchelf, "--print-rpath", str(installed_exe)],
            check=True,
            capture_output=True,
            text=True,
            timeout=30,
        ).stdout.strip()
        assert rpath == "$ORIGIN/../lib/" + HOST_DIR, rpath
        lib_rpath = subprocess.run(
            [patchelf, "--print-rpath", str(installed / "lib" / HOST_DIR / GHC_SO)],
            check=True,
            capture_output=True,
            text=True,
            timeout=30,
        ).stdout.strip()
        assert lib_rpath == "$ORIGIN", lib_rpath
        subprocess.run([str(installed_exe)], check=True, timeout=10)
        assert (installed / "lib/targets/wasm32-unknown-wasi/lib/payload.txt").read_text() == \
            "target payload remains present\n"


def main():
    test_invalid_native_library_dirs()
    test_portable_full_triple_selection()
    test_linux_elf_relocation()
    print("multi-target Linux bindist native RPATH regression passed")


if __name__ == "__main__":
    main()
