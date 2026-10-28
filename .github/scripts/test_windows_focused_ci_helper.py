#!/usr/bin/env python3
"""Exercise the Windows focused testsuite wrapper with fake MSYS tools."""

import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile


REPO = Path(__file__).resolve().parents[2]
HELPER = REPO / ".github/scripts/run-windows-focused-tests.sh"
LABELS = [
    "t1-rts-linker",
    "t1-ghci-linking",
    "t2-ghci-normal",
    "t2-ghci-ext-interp",
    "t2-th-normal",
    "t2-th-ext-interp",
    "t2-plugins-normal",
    "t2-plugins-ext-interp",
    "t3-unload",
]


def main():
    with tempfile.TemporaryDirectory(prefix="windows-focused-helper-") as temp:
        base = Path(temp)
        bin_dir = base / "bin"
        workspace = base / "workspace"
        launch_dir = base / "launch"
        bin_dir.mkdir()
        workspace.mkdir()
        launch_dir.mkdir()

        fake_make = bin_dir / "make"
        fake_make.write_text(
            f"#!{sys.executable}\n"
            + "import json, os, pathlib, sys\n"
            + "record = {\n"
            + "  'args': sys.argv[1:], 'cwd': os.getcwd(),\n"
            + "  'junit': os.environ['JUNIT_FILE'],\n"
            + "  'summary': os.environ['SUMMARY_FILE'],\n"
            + "  'metrics': os.environ['METRICS_FILE'],\n"
            + "  'compiler': os.environ['TEST_HC'],\n"
            + "  'compiler_opts': os.environ.get('TEST_HC_OPTS', ''),\n"
            + "}\n"
            + "with open(os.environ['FAKE_MAKE_RECORD'], 'a') as output:\n"
            + "  output.write(json.dumps(record) + '\\n')\n"
            + "for key, contents in [('junit', '<testsuite/>'), ('summary', 'summary'), ('metrics', 'metrics')]:\n"
            + "  pathlib.Path(record[key]).write_text(contents)\n"
            + "print('fake make wrote ' + record['junit'])\n"
            + "fail_label = os.environ.get('FAKE_FAIL_LABEL')\n"
            + "sys.exit(23 if fail_label and fail_label in record['junit'] else 0)\n"
        )
        fake_make.chmod(0o755)

        native_workspace = r"D:\a\ghc"
        fake_cygpath = bin_dir / "cygpath"
        fake_cygpath.write_text(
            f"#!{sys.executable}\n"
            + "import os, sys\n"
            + f"expected = {native_workspace!r}\n"
            + "assert sys.argv[1:] == ['-u', expected], sys.argv[1:]\n"
            + "print(os.environ['FAKE_CYGPATH_OUTPUT'])\n"
        )
        fake_cygpath.chmod(0o755)

        cases = [
            ("all-pass", None, 0),
            ("early-failure", "t1-ghci-linking", 1),
            ("last-failure", "t3-unload", 1),
        ]
        roots = {
            "t1-rts-linker": "rts/linker",
            "t1-ghci-linking": "ghci/linking",
            "t2-ghci-normal": "ghci",
            "t2-ghci-ext-interp": "ghci",
            "t2-th-normal": "th",
            "t2-th-ext-interp": "th",
            "t2-plugins-normal": "plugins",
            "t2-plugins-ext-interp": "plugins",
            "t3-unload": "rts/linker",
        }

        for case_name, fail_label, expected_status in cases:
            case_workspace = workspace / case_name
            case_workspace.mkdir()
            record_file = base / f"{case_name}-make-record.jsonl"
            env = os.environ.copy()
            env.update(
                PATH=f"{bin_dir}{os.pathsep}{env['PATH']}",
                GITHUB_WORKSPACE=native_workspace,
                FAKE_CYGPATH_OUTPUT=str(case_workspace),
                FAKE_MAKE_RECORD=str(record_file),
            )
            if fail_label:
                env["FAKE_FAIL_LABEL"] = fail_label
            result = subprocess.run(
                ["bash", str(HELPER)],
                cwd=launch_dir,
                env=env,
                text=True,
                capture_output=True,
                check=False,
                timeout=90,
            )

            records = [json.loads(line) for line in record_file.read_text().splitlines()]
            assert len(records) == 9, f"{case_name}: expected nine runs, found {len(records)}"
            assert result.returncode == expected_status, (
                f"{case_name}: expected status {expected_status}, got {result.returncode}"
            )
            actual_labels = [Path(record["junit"]).parent.name for record in records]
            assert actual_labels == LABELS, (case_name, actual_labels)
            normalized_workspace = case_workspace.resolve()
            for record in records:
                assert "VERBOSE=3" in record["args"], record["args"]
                assert Path(record["cwd"]) == normalized_workspace, record["cwd"]
                assert Path(record["compiler"]) == normalized_workspace / "_build/dist/bin/ghc"
                assert not record["compiler_opts"], record["compiler_opts"]
                for key in ("junit", "summary", "metrics"):
                    assert Path(record[key]).is_file(), record[key]
                    assert Path(record[key]).is_relative_to(normalized_workspace)
                root = roots[Path(record["junit"]).parent.name]
                assert record["args"][0:2] == ["-C", "testsuite/tests/" + root], record["args"]
                run_log = Path(record["junit"]).parent / "run.log"
                assert run_log.is_file(), run_log

            assert "WAY=normal" in records[2]["args"]
            assert "WAY=ext-interp" in records[3]["args"]
            assert "TESTS=linker_unload linker_unload_native linker_unload_multiple_objs T20494 T25191" in records[8]["args"]
            assert "SPEED=0" in records[8]["args"]
            expected_failure = fail_label is not None
            assert ("failed with status 23" in result.stderr) == expected_failure

        print("PASS: cygpath normalization, all nine outputs, success, early and final failure aggregation")


if __name__ == "__main__":
    main()
