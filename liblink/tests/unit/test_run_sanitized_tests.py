#!/usr/bin/env python3
"""Focused orchestration checks for the sanitizer native-test runner."""

from __future__ import annotations

import json
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import run_sanitized_tests as driver


class SanitizedRunnerTests(unittest.TestCase):
    def fixture(self, root: Path) -> tuple[Path, Path, Path]:
        config = root / "config"
        config.mkdir()
        (config / "ll_autoconf.h").write_text("/* generated */\n")
        (config / "ll_platform.h").write_text("/* generated */\n")
        fs_include = root / "rts-fs" / "include"
        fs_include.mkdir(parents=True)
        (fs_include / "fs.h").write_text("/* installed */\n")
        (fs_include.parent / "libHSrts-fs-1.0.0.0.a").touch()
        return root / "old-liblink.a", config, fs_include

    def test_rejects_duplicate_archive_object_basenames(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            liblink = Path(temporary) / "liblink"
            source_dir = liblink / "src"
            source_dir.mkdir(parents=True)
            (source_dir / "same.c").touch()
            (source_dir / "same.S").touch()
            manifest = Path(temporary) / "native-sources.list"
            manifest.write_text("src/same.c\nsrc/same.S\n")
            with (
                mock.patch.object(driver, "LIBLINK", liblink),
                mock.patch.object(driver, "SOURCE_LIST", manifest),
            ):
                with self.assertRaisesRegex(RuntimeError, "same.o"):
                    driver.source_files()

    def args(self, output: Path) -> object:
        return driver.parse_args(
            [
                "--build-root", str(output.parent / "build"),
                "--store-root", str(output.parent / "store"),
                "--cc", "fakecc --target=native",
                "--ar", "fakear",
                "--output-dir", str(output),
                "--timeout", "120",
                "--command-timeout", "10",
            ]
        )

    def test_builds_manifest_and_runs_units_after_archive(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            output = root / "fresh-output"
            _old_archive, config, fs_include = self.fixture(root)
            calls: list[list[str]] = []

            def fake_popen(argv: list[str], **kwargs: object) -> mock.Mock:
                self.assertIs(kwargs.get("start_new_session"), True)
                calls.append(argv)
                process = mock.Mock()
                process.pid = 123
                process.wait.return_value = 0
                if "-c" in argv:
                    object_path = Path(argv[argv.index("-o") + 1])
                    object_path.touch()
                elif len(argv) > 1 and argv[1] == "rcs":
                    Path(argv[2]).touch()
                return process

            with (
                mock.patch.object(driver, "native_host", return_value="Darwin"),
                mock.patch.object(
                    driver.run_native_tests,
                    "locate_artifacts",
                    return_value=(_old_archive, config, fs_include),
                ),
                mock.patch.object(driver.subprocess, "Popen", side_effect=fake_popen),
                mock.patch("builtins.print"),
            ):
                report, exit_code = driver.execute(self.args(output))

            self.assertEqual(exit_code, 0)
            manifest_sources = driver.source_files()
            compile_commands = [argv for argv in calls if "-c" in argv]
            self.assertEqual(len(compile_commands), len(manifest_sources))
            self.assertEqual(
                [argv[argv.index("-c") + 1] for argv in compile_commands],
                [str(source) for source in manifest_sources],
            )
            for argv in compile_commands:
                for flag in driver.STRICT_FLAGS:
                    self.assertIn(flag, argv)
                self.assertLess(argv.index("-isystem"), argv.index("-c"))

            archive_index = next(i for i, argv in enumerate(calls) if len(argv) > 1 and argv[1] == "rcs")
            unit_index = next(
                i for i, argv in enumerate(calls)
                if any("run_native_tests.py" in token for token in argv)
            )
            self.assertLess(archive_index, unit_index)
            unit_command = calls[unit_index]
            self.assertEqual(
                unit_command[unit_command.index("--native-archive") + 1],
                str(output.resolve() / "liblink.a"),
            )
            test_cc = unit_command[unit_command.index("--cc") + 1]
            self.assertIn("-fsanitize=address,undefined", test_cc)
            self.assertEqual(report["outcome"], "passed")
            self.assertEqual(report["inputs_before_sha256"], report["inputs_after_sha256"])
            saved = json.loads((output / "sanitized-run.json").read_text())
            self.assertEqual(saved["archive_sha256"], report["archive_sha256"])

    def test_rejects_unsupported_host_before_spawning_commands(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "output"
            with (
                mock.patch.object(driver, "native_host", return_value="Windows"),
                mock.patch.object(driver.subprocess, "Popen") as popen,
            ):
                report, exit_code = driver.execute(self.args(output))
            self.assertEqual(exit_code, 1)
            self.assertIn("support Linux and Darwin", report["error"])
            popen.assert_not_called()
            self.assertFalse(output.exists())

    def test_refuses_occupied_output_without_changing_existing_file(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "output"
            output.mkdir()
            sentinel = output / "keep.txt"
            sentinel.write_text("user data\n")
            with (
                mock.patch.object(driver, "native_host", return_value="Darwin"),
                mock.patch.object(driver.subprocess, "Popen") as popen,
            ):
                report, exit_code = driver.execute(self.args(output))
            self.assertEqual(exit_code, 1)
            self.assertIn("new or empty", report["error"])
            self.assertEqual(sentinel.read_text(), "user data\n")
            self.assertNotIn("report_file", report)
            popen.assert_not_called()

    def test_unit_failure_and_input_mutation_are_reported(self) -> None:
        for failure in ("unit-exit", "input-change", "archive-change"):
            with self.subTest(failure=failure), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                output = root / "output"
                _old_archive, config, fs_include = self.fixture(root)
                calls: list[list[str]] = []

                def fake_popen(argv: list[str], **_kwargs: object) -> mock.Mock:
                    calls.append(argv)
                    process = mock.Mock()
                    process.pid = 789
                    process.wait.return_value = 0
                    if "-c" in argv:
                        Path(argv[argv.index("-o") + 1]).touch()
                    elif len(argv) > 1 and argv[1] == "rcs":
                        Path(argv[2]).touch()
                    elif any("run_native_tests.py" in token for token in argv):
                        if failure == "input-change":
                            (config / "ll_autoconf.h").write_text("/* changed */\n")
                        elif failure == "archive-change":
                            archive_index = argv.index("--native-archive")
                            Path(argv[archive_index + 1]).write_bytes(b"changed archive")
                        else:
                            process.wait.return_value = 23
                    return process

                with (
                    mock.patch.object(driver, "native_host", return_value="Linux"),
                    mock.patch.object(
                        driver.run_native_tests,
                        "locate_artifacts",
                        return_value=(_old_archive, config, fs_include),
                    ),
                    mock.patch.object(driver.subprocess, "Popen", side_effect=fake_popen),
                    mock.patch("builtins.print"),
                ):
                    report, exit_code = driver.execute(self.args(output))

                self.assertEqual(exit_code, 1)
                self.assertTrue(any("run_native_tests.py" in token for token in calls[-1]))
                self.assertIn("archive_sha256", report)
                self.assertIn("archive_sha256_observed", report)
                if failure == "unit-exit":
                    self.assertIn("exited with status 23", report["error"])
                    self.assertEqual(report["commands"][-1]["outcome"], "failed")
                elif failure == "input-change":
                    self.assertIn("inputs changed during run", report["error"])
                    self.assertNotEqual(
                        report["inputs_before_sha256"], report["inputs_after_sha256"]
                    )
                else:
                    self.assertIn("archive changed during native unit execution", report["error"])
                    self.assertNotEqual(
                        report["archive_sha256"], report["archive_sha256_observed"]
                    )

    def test_compile_failure_or_timeout_stops_before_archive_and_unit_runner(self) -> None:
        for failure in ("exit", "timeout"):
            with self.subTest(failure=failure), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                output = root / "output"
                _old_archive, config, fs_include = self.fixture(root)
                calls: list[list[str]] = []

                def failing_popen(argv: list[str], **_kwargs: object) -> mock.Mock:
                    calls.append(argv)
                    process = mock.Mock()
                    process.pid = 456
                    if failure == "timeout":
                        process.wait.side_effect = [
                            subprocess.TimeoutExpired(argv, 10), -9, -9
                        ]
                    else:
                        process.wait.return_value = 7
                    return process

                with (
                    mock.patch.object(driver, "native_host", return_value="Linux"),
                    mock.patch.object(
                        driver.run_native_tests,
                        "locate_artifacts",
                        return_value=(_old_archive, config, fs_include),
                    ),
                    mock.patch.object(driver.subprocess, "Popen", side_effect=failing_popen),
                    mock.patch.object(driver.os, "killpg", create=True) as killpg,
                    mock.patch.object(driver.signal, "SIGKILL", 9, create=True),
                    mock.patch("builtins.print"),
                ):
                    report, exit_code = driver.execute(self.args(output))

                self.assertEqual(exit_code, 1)
                self.assertEqual(len(calls), 1)
                self.assertNotIn("rcs", calls[0])
                self.assertNotIn("run_native_tests.py", calls[0][0])
                self.assertEqual(report["commands"][0]["outcome"], "timeout" if failure == "timeout" else "failed")
                if failure == "timeout":
                    self.assertEqual(killpg.call_args_list, [
                        mock.call(456, driver.signal.SIGTERM), mock.call(456, 9)
                    ])
                    self.assertTrue(report["error"].endswith("timed out after 10.0 seconds"))


if __name__ == "__main__":
    unittest.main(verbosity=2)
