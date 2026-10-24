#!/usr/bin/env python3
"""Unit tests for the native linker trace and map parsers."""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path, PurePosixPath
from unittest import mock

import link_trace_checks as checks
import run_native_tests as driver


MAP_HEADER = "Address  Size     Align Out     In      Symbol\n"


def map_row(member: str) -> str:
    return f"00003000 00000004 4         {member}:(.data)\n"


class MinGWLLDMapTests(unittest.TestCase):
    archive = Path("C:/Build Artifacts/liblink.a")

    def trace(self, loaded: tuple[str, ...] = ("symbols-strong.o",)) -> str:
        lines = [
            "lld: Reading C:/Build Artifacts/liblink.a",
            "lld: Reading liblink.a(symbols-strong.o)",
        ]
        lines.extend(
            f"lld: Loaded liblink.a({member}) for fixture_symbol"
            for member in loaded
        )
        return "\n".join(lines) + "\n"

    def verify(self, map_text: str, trace: str | None = None,
               required_members: tuple[str, ...] = ()) -> None:
        checks.verify_lld_mingw_archive_map(
            map_text,
            self.archive,
            self.trace() if trace is None else trace,
            included_source="M32Alloc.c",
            forbidden_members=("M32Alloc",),
            required_members=required_members,
        )

    def test_accepts_archive_read_loaded_selection_and_map_record(self) -> None:
        self.verify(MAP_HEADER + map_row("symbols-strong.o"))

    def test_normalizes_windows_case_and_separators_with_spaces(self) -> None:
        archive = Path(r"c:\build artifacts\LIBLINK.A")
        trace = (
            "lld: Reading C:/BUILD ARTIFACTS/liblink.a\n"
            "lld: Loaded LIBLINK.A(SYMBOLS-STRONG.O) for fixture_symbol\n"
        )
        checks.verify_lld_mingw_archive_map(
            MAP_HEADER + map_row(r"obj\SYMBOLS-STRONG.O"),
            archive,
            trace,
            included_source="M32Alloc.c",
            forbidden_members=("M32Alloc",),
        )

    def test_rejects_wrong_archive(self) -> None:
        trace = (
            "lld: Reading D:/Other/liblink.a\n"
            "lld: Loaded liblink.a(symbols-strong.o) for fixture_symbol\n"
        )
        with self.assertRaisesRegex(RuntimeError, "did not read expected archive"):
            self.verify(MAP_HEADER + map_row("symbols-strong.o"), trace)

    def test_rejects_same_basename_archive_ambiguity(self) -> None:
        trace = self.trace() + "lld: Reading D:/Other/liblink.a\n"
        with self.assertRaisesRegex(RuntimeError, "ambiguous archive basename"):
            self.verify(MAP_HEADER + map_row("symbols-strong.o"), trace)

    def test_rejects_missing_archive_read(self) -> None:
        trace = "lld: Loaded liblink.a(symbols-strong.o) for fixture_symbol\n"
        with self.assertRaisesRegex(RuntimeError, "did not read expected archive"):
            self.verify(MAP_HEADER + map_row("symbols-strong.o"), trace)

    def test_rejects_command_line_echo_without_read_or_selection(self) -> None:
        trace = (
            "lld-link /libpath:C:/Build Artifacts C:/Build Artifacts/liblink.a\n"
        )
        with self.assertRaisesRegex(RuntimeError, "did not read expected archive"):
            self.verify(MAP_HEADER + map_row("symbols-strong.o"), trace)

    def test_rejects_archive_listing_without_selected_member(self) -> None:
        trace = (
            "lld: Reading C:/Build Artifacts/liblink.a\n"
            "lld: Reading liblink.a(symbols-strong.o)\n"
        )
        with self.assertRaisesRegex(RuntimeError, "did not show selected member"):
            self.verify(MAP_HEADER + map_row("symbols-strong.o"), trace)

    def test_rejects_forbidden_member_in_selected_trace(self) -> None:
        trace = self.trace(("symbols-strong.o", "M32Alloc.o"))
        with self.assertRaisesRegex(RuntimeError, "trace selected archive member"):
            self.verify(MAP_HEADER + map_row("symbols-strong.o"), trace)

    def test_rejects_flattened_forbidden_member_names(self) -> None:
        for member in (
            "linker_M32Alloc.o",
            r"objects\linker_M32Alloc.c.o",
            "linker-M32Alloc.o",
            "prefixM32Alloc.obj",
        ):
            with self.subTest(member=member):
                trace = self.trace(("symbols-strong.o", member))
                with self.assertRaisesRegex(RuntimeError, "trace selected archive member"):
                    self.verify(MAP_HEADER + map_row("symbols-strong.o"), trace)

    def test_does_not_match_forbidden_name_prefix(self) -> None:
        self.verify(MAP_HEADER + map_row("symbols-strong.o") +
                    map_row("M32AllocHelper.o"))

    def test_rejects_forbidden_member_in_map_inputs(self) -> None:
        with self.assertRaisesRegex(RuntimeError, "map selected archive member"):
            self.verify(MAP_HEADER + map_row("symbols-strong.o") +
                        map_row(r"objects\M32Alloc.c.o"))

    def test_rejects_flattened_forbidden_member_in_map(self) -> None:
        with self.assertRaisesRegex(RuntimeError, "map selected archive member"):
            self.verify(MAP_HEADER + map_row("symbols-strong.o") +
                        map_row("linker_M32Alloc.obj"))

    def test_rejects_explicit_loaded_archive_at_wrong_path(self) -> None:
        trace = (
            "lld: Reading C:/Build Artifacts/liblink.a\n"
            "lld: Loaded D:/Other/liblink.a(ll_error.o) for fixture_symbol\n"
        )
        with self.assertRaisesRegex(RuntimeError, "different path"):
            self.verify(MAP_HEADER + map_row("ll_error.o"), trace)

    def test_rejects_forbidden_loaded_member_from_other_archive(self) -> None:
        trace = self.trace() + (
            "lld: Loaded D:/Other/extra.a(linker_M32Alloc.o) for another_symbol\n"
        )
        with self.assertRaisesRegex(RuntimeError, "trace selected archive member"):
            self.verify(MAP_HEADER + map_row("symbols-strong.o"), trace)

    def test_rejects_selected_member_without_map_input_record(self) -> None:
        with self.assertRaisesRegex(RuntimeError, "no input record matching"):
            self.verify(MAP_HEADER + map_row("unrelated.o"))

    def test_member_map_binding_keeps_object_suffix(self) -> None:
        with self.assertRaisesRegex(RuntimeError, "no input record matching"):
            self.verify(MAP_HEADER + map_row("ll_error.obj"), self.trace(("ll_error.o",)))

    def test_accepts_required_member_with_basename_only_map_record(self) -> None:
        self.verify(
            MAP_HEADER + map_row("ll_path.o"),
            self.trace(("ll_path.o",)),
            required_members=("ll_path",),
        )

    def test_rejects_wrong_required_archive_member(self) -> None:
        with self.assertRaisesRegex(RuntimeError, "did not select required archive member"):
            self.verify(
                MAP_HEADER + map_row("symbols-strong.o"),
                self.trace(("symbols-strong.o",)),
                required_members=("ll_path",),
            )

    def test_rejects_required_member_from_wrong_archive(self) -> None:
        trace = (
            "lld: Reading C:/Build Artifacts/liblink.a\n"
            "lld: Loaded D:/Other/liblink.a(ll_path.o) for fixture_symbol\n"
        )
        with self.assertRaisesRegex(RuntimeError, "different path"):
            self.verify(
                MAP_HEADER + map_row("ll_path.o"), trace,
                required_members=("ll_path",),
            )

    def test_rejects_required_member_missing_from_map(self) -> None:
        with self.assertRaisesRegex(
            RuntimeError, "link map omitted required archive member ll_path"
        ):
            self.verify(
                MAP_HEADER + map_row("ll_host.o"),
                self.trace(("ll_host.o", "ll_path.o")),
                required_members=("ll_path",),
            )


class DarwinWhyLoadTests(unittest.TestCase):
    archive = PurePosixPath("/Build With Spaces/liblink.a")

    def verify(self, trace: str) -> None:
        checks.verify_macho_archive_selection(
            self.archive,
            trace,
            included_source="M32Alloc.c",
            forbidden_members=("M32Alloc",),
        )

    def test_accepts_local_caused_load_with_archive_index(self) -> None:
        self.verify(
            "symbol _ll_error_set caused load of "
            "/Build With Spaces/liblink.a[12](ll_error.o)\n"
        )

    def test_accepts_ci_forced_load_without_archive_index(self) -> None:
        self.verify(
            "_ll_error_set forced load of "
            "/Build With Spaces/liblink.a(ll_error.o)\n"
        )

    def test_accepts_both_linker_dialects_together(self) -> None:
        self.verify(
            "symbol _one caused load of /Build With Spaces/liblink.a[4](ll_error.o)\n"
            "_two forced load of /Build With Spaces/liblink.a(MMap.o)\n"
        )

    def test_rejects_wrong_archive_selection(self) -> None:
        with self.assertRaisesRegex(RuntimeError, "did not show archive member"):
            self.verify("_ll_error_set forced load of /Other/liblink.a(ll_error.o)\n")

    def test_darwin_archive_path_case_is_significant(self) -> None:
        with self.assertRaisesRegex(RuntimeError, "did not show archive member"):
            self.verify(
                "_ll_error_set forced load of "
                "/build with spaces/liblink.a(ll_error.o)\n"
            )

    def test_rejects_m32alloc_from_indexed_caused_load(self) -> None:
        with self.assertRaisesRegex(RuntimeError, "selected archive member"):
            self.verify(
                "symbol _m32_alloc caused load of "
                "/Build With Spaces/liblink.a[17](linker/M32Alloc.c.o)\n"
            )

    def test_rejects_m32alloc_from_unindexed_forced_load(self) -> None:
        with self.assertRaisesRegex(RuntimeError, "selected archive member"):
            self.verify(
                "_m32_alloc forced load of /Build With Spaces/liblink.a(M32Alloc.o)\n"
            )

    def test_does_not_treat_t_archive_listing_as_selection(self) -> None:
        with self.assertRaisesRegex(RuntimeError, "did not show archive member"):
            self.verify("/Build With Spaces/liblink.a(MMap.o)\n")


class NativeDriverMapDispatchTests(unittest.TestCase):
    def test_elf_duplicate_guard_matches_exact_object_stem(self) -> None:
        archive = Path("/build/liblink.a")
        with tempfile.TemporaryDirectory() as temporary:
            map_path = Path(temporary) / "probe.map"
            for member in (
                "ElfVerify.o",
                "obj/ElfVerify.o",
                "prefixElfVerify.o",
                "ElfHelper.o",
            ):
                with self.subTest(allowed_member=member):
                    map_path.write_text(f"{archive}({member})\n")
                    driver.verify_link_map(
                        map_path,
                        archive,
                        str(archive),
                        included_source="Elf.c",
                        forbidden_member="Elf",
                    )

            for member in (
                "Elf.o",
                "prefixElf.o",
                "path/Elf.o",
                "prefix_Elf.c.o",
                "Elf.obj",
            ):
                with self.subTest(forbidden_member=member):
                    map_path.write_text(f"{archive}({member})\n")
                    with self.assertRaisesRegex(RuntimeError, "selected archive Elf.o"):
                        driver.verify_link_map(
                            map_path,
                            archive,
                            str(archive),
                            included_source="Elf.c",
                            forbidden_member="Elf",
                        )

    def test_lld_trace_accepts_basename_only_map_input(self) -> None:
        archive = Path("C:/Build Artifacts/liblink.a")
        trace = (
            "lld: Reading C:/Build Artifacts/liblink.a\n"
            "lld: Loaded liblink.a(symbols-strong.o) for fixture_symbol\n"
        )
        map_text = MAP_HEADER + map_row("symbols-strong.o")
        with tempfile.TemporaryDirectory() as temporary:
            map_path = Path(temporary) / "probe.map"
            map_path.write_text(map_text)
            driver.verify_link_map(
                map_path,
                archive,
                trace,
                included_source="M32Alloc.c",
                forbidden_member="M32Alloc",
                windows_paths=True,
            )

    def test_legacy_full_path_map_still_rejects_duplicate_member(self) -> None:
        archive = Path("C:/Build Artifacts/liblink.a")
        trace = "C:/Build Artifacts/liblink.a(other.o)\n"
        with tempfile.TemporaryDirectory() as temporary:
            map_path = Path(temporary) / "probe.map"
            map_path.write_text(
                MAP_HEADER
                + "00003000 00000004 4         "
                + "C:/Build Artifacts/liblink.a(M32Alloc.o):(.text)\n"
            )
            with self.assertRaisesRegex(RuntimeError, "selected archive M32Alloc.o"):
                driver.verify_link_map(
                    map_path,
                    archive,
                    trace,
                    included_source="M32Alloc.c",
                    forbidden_member="M32Alloc",
                    windows_paths=True,
                )


class NativeRtsFsLinkArgumentsTests(unittest.TestCase):
    def artifacts(self, root: Path) -> tuple[Path, Path, Path, Path]:
        archive_dir = root / "build" / "native-linker"
        package_dir = root / "store" / "rts-fs-1.0.0.0"
        archive_dir.mkdir(parents=True)
        package_dir.mkdir(parents=True)
        liblink = archive_dir / "liblink.a"
        rts_fs = package_dir / "libHSrts-fs-1.0.0.0.a"
        source = root / "core_error_status.c"
        output = root / "core_error_status"
        liblink.touch()
        rts_fs.touch()
        source.touch()
        return liblink, rts_fs, source, output

    def test_darwin_command_keeps_existing_link_order_without_rts_fs(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            liblink, _rts_fs, source, output = self.artifacts(Path(temporary))
            flags = driver.native_link_flags(
                "Darwin", None, driver.core_link_flags("Darwin")
            )
            command = driver.native_unit_link_arguments(
                source, liblink, output, flags, ["-Wl,-map,core.map"]
            )
            self.assertEqual(
                command,
                [
                    str(source),
                    f"-L{liblink.parent}",
                    "-llink",
                    "-pthread",
                    "-Wl,-map,core.map",
                    "-o",
                    str(output),
                ],
            )

    def test_linux_main_core_link_command_contains_provider_in_order(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            archive = root / "build" / "native-linker" / "liblink.a"
            archive.parent.mkdir(parents=True)
            archive.touch()
            config = root / "config"
            config.mkdir()
            (config / "ll_autoconf.h").write_text("/* mmap disabled */\n")
            package = root / "rts-fs-1.0.0.0"
            include = package / "include"
            include.mkdir(parents=True)
            provider = package / "libHSrts-fs-1.0.0.0.a"
            provider.touch()

            class StopAfterCoreLink(Exception):
                pass

            captured: list[str] = []

            def capture_run(argv: list[str], *, label: str) -> str:
                if label == "compile and link core_error_status":
                    captured.extend(argv)
                    raise StopAfterCoreLink
                return ""

            with (
                mock.patch.object(driver, "native_host", return_value="Linux"),
                mock.patch.object(
                    driver, "locate_artifacts", return_value=(archive, config, include)
                ),
                mock.patch.object(driver, "run", side_effect=capture_run),
                mock.patch.object(driver, "run_initializer_errors"),
                mock.patch.object(driver, "run_native_load_errors"),
                mock.patch("sys.argv", ["run_native_tests.py", "--cc", "cc"]),
                self.assertRaises(StopAfterCoreLink),
            ):
                driver.main()

            self.assertIn("-llink", captured)
            self.assertLess(captured.index("-llink"), captured.index(str(provider)))
            self.assertLess(captured.index(str(provider)), captured.index("-pthread"))
            self.assertLess(captured.index("-pthread"), captured.index("-ldl"))
            self.assertEqual(captured[0], "cc")
            self.assertEqual(captured[-2], "-o")

    def test_windows_common_helper_command_contains_provider_before_support_libs(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            archive = root / "liblink.a"
            archive.touch()
            provider = root / "libHSrts-fs-1.0.0.0.a"
            provider.touch()

            class StopAfterLink(Exception):
                pass

            captured: list[str] = []

            def capture_run(argv: list[str], *, label: str) -> str:
                self.assertEqual(label, "compile and link pei386_common_ownership")
                captured.extend(argv)
                raise StopAfterLink

            with mock.patch.object(driver, "run", side_effect=capture_run):
                with self.assertRaises(StopAfterLink):
                    driver.run_pei386_common_ownership(
                        ["cc"], archive, root, [str(provider), *driver.PE_SUPPORT_LIBS]
                    )

            self.assertIn("-llink", captured)
            self.assertLess(captured.index("-llink"), captured.index(str(provider)))
            self.assertLess(captured.index(str(provider)), captured.index("-ldbghelp"))
            self.assertLess(captured.index("-ldbghelp"), captured.index("-lpsapi"))

    def test_windows_foundation_main_runs_without_missing_rts_fs_archive(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            archive = root / "build" / "native-linker" / "liblink.a"
            archive.parent.mkdir(parents=True)
            archive.touch()
            config = root / "config"
            config.mkdir()
            (config / "ll_autoconf.h").write_text("/* mmap disabled */\n")
            include = root / "rts-fs-1.0.0.0" / "include"
            include.mkdir(parents=True)
            calls: list[tuple[str, list[str]]] = []

            def capture_run(argv: list[str], *, label: str) -> str:
                calls.append((label, argv))
                return ""

            with (
                mock.patch.object(driver, "native_host", return_value="Windows"),
                mock.patch.object(
                    driver, "locate_artifacts", return_value=(archive, config, include)
                ),
                mock.patch.object(driver, "run", side_effect=capture_run),
                mock.patch("sys.argv", ["run_native_tests.py", "--foundation-only"]),
            ):
                self.assertEqual(driver.main(), 0)

            c_calls = [(label, argv) for label, argv in calls if label.startswith(("compile", "run error_state"))]
            self.assertEqual([label for label, _argv in c_calls], ["compile error_state", "run error_state"])

    def test_missing_required_archive_fails_before_c_compile_in_main(self) -> None:
        for host, mode in (("Linux", []), ("Windows", ["--windows-m32-only"])):
            with self.subTest(host=host), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                archive = root / "build" / "native-linker" / "liblink.a"
                archive.parent.mkdir(parents=True)
                archive.touch()
                config = root / "config"
                config.mkdir()
                (config / "ll_autoconf.h").write_text("/* mmap disabled */\n")
                include = root / "rts-fs-1.0.0.0" / "include"
                include.mkdir(parents=True)
                labels: list[str] = []

                def capture_run(_argv: list[str], *, label: str) -> str:
                    labels.append(label)
                    return ""

                with (
                    mock.patch.object(driver, "native_host", return_value=host),
                    mock.patch.object(
                        driver, "locate_artifacts", return_value=(archive, config, include)
                    ),
                    mock.patch.object(driver, "run", side_effect=capture_run),
                    mock.patch("sys.argv", ["run_native_tests.py", *mode]),
                    self.assertRaisesRegex(RuntimeError, "require the installed rts-fs static archive"),
                ):
                    driver.main()
                self.assertEqual(labels, ["linker trace parser tests"])


if __name__ == "__main__":
    unittest.main(verbosity=2)
