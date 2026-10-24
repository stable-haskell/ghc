#!/usr/bin/env python3
"""Exercise the unpack-sums collector with a fake compiler and executable."""

import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile


REPO = Path(__file__).resolve().parents[2]
COLLECTOR = REPO / ".github/scripts/measure-unpack-sums.py"
SOURCE = REPO / "testsuite/tests/unboxedsums/unpack_sums_6.hs"
EXPECTED_STDOUT = SOURCE.with_suffix(".stdout").read_bytes()
STATUSES = [0, 1, 0, 1, 1, 0]


def fake_compiler(path):
    program = "\n".join(
        [
            f"#!{sys.executable}",
            "import base64, os, pathlib, sys",
            "counter = pathlib.Path(os.environ['FAKE_COUNTER'])",
            "index = int(counter.read_text()) if counter.exists() else 0",
            "counter.write_text(str(index + 1))",
            "statuses = [int(value) for value in os.environ['FAKE_STATUS_SEQUENCE'].split(',')]",
            "sys.stdout.buffer.write(base64.b64decode(os.environ['FAKE_EXPECTED_STDOUT']))",
            "with open(os.environ['FAKE_PROGRAM_TIMING_ENV'], 'a') as out:",
            "    out.write(str(os.environ.get('GHC_UNPACK_SUMS_TIMINGS')) + '\\n')",
            "if os.environ.get('FAKE_INVALID_TIMING') == '1':",
            "    sys.stderr.write('GHC_UNPACK_SUMS_TIMINGS broken\\n')",
            "else:",
            "    sys.stderr.write('GHC_UNPACK_SUMS_TIMINGS unlayered=0.125s layered=0.25s\\n')",
            "raise SystemExit(statuses[index])",
        ]
    ) + "\n"
    script = f'''#!{sys.executable}
import json, os, pathlib, sys, time
record = {{"args": sys.argv[1:], "timing_env": os.environ.get("GHC_UNPACK_SUMS_TIMINGS")}}
with open(os.environ["FAKE_GHC_RECORD"], "a") as out:
    out.write(json.dumps(record) + "\\n")
if "--info" in sys.argv:
    print("[]")
    if os.environ.get("FAKE_DELETE_AFTER_INFO") == "1":
        pathlib.Path(__file__).unlink()
    raise SystemExit(0)
if os.environ.get("FAKE_SPAWN_DESCENDANT") == "1":
    child_code = (
        "import json, os, pathlib, time; "
        "pathlib.Path(os.environ['FAKE_CHILD_INFO']).write_text(json.dumps(["
        "os.getpid(), os.getpgrp(), os.getsid(0)])); "
        "time.sleep(30)"
    )
    child = __import__("subprocess").Popen([sys.executable, "-c", child_code])
    pathlib.Path(os.environ["FAKE_CHILD_PID"]).write_text(str(child.pid))
    pathlib.Path(os.environ["FAKE_GROUP_INFO"]).write_text(
        json.dumps({{"parent_pid": os.getpid(), "parent_pgid": os.getpgrp(),
                    "parent_sid": os.getsid(0), "child_pid": child.pid}})
    )
    for _ in range(200):
        if pathlib.Path(os.environ["FAKE_CHILD_INFO"]).exists():
            break
        time.sleep(0.01)
    else:
        raise SystemExit("descendant did not start")
    raise SystemExit(0)
if os.environ.get("FAKE_HANG_COMPILE") == "1":
    time.sleep(30)
if os.environ.get("FAKE_COMPILE_FAIL") == "1":
    print("injected compile failure", file=sys.stderr)
    raise SystemExit(7)
output = pathlib.Path(sys.argv[sys.argv.index("-o") + 1])
output.write_text({program!r})
output.chmod(0o755)
'''
    path.write_text(script)
    path.chmod(0o755)


def run_case(base, name, *, extra_env=None, extra_args=None, precreate=False, expect_status):
    compiler_dir = base / "bin"
    compiler_dir.mkdir(exist_ok=True)
    compiler = compiler_dir / "fake-ghc"
    if not compiler.exists():
        fake_compiler(compiler)

    output_dir = base / name
    if precreate:
        output_dir.mkdir()
        (output_dir / "sentinel").write_text("keep")

    env = os.environ.copy()
    env.update(
        FAKE_GHC_RECORD=str(base / f"{name}-compiler.jsonl"),
        FAKE_COUNTER=str(base / f"{name}-counter"),
        FAKE_PROGRAM_TIMING_ENV=str(base / f"{name}-timing-env"),
        FAKE_STATUS_SEQUENCE=",".join(map(str, STATUSES)),
        FAKE_EXPECTED_STDOUT=__import__("base64").b64encode(EXPECTED_STDOUT).decode("ascii"),
        GHC_UNPACK_SUMS_TIMINGS="inherited-value",
    )
    if extra_env:
        env.update(extra_env)
    command = [sys.executable, str(COLLECTOR), "--ghc", str(compiler), "--output-dir", str(output_dir)]
    command.extend(extra_args or [])
    result = subprocess.run(command, env=env, text=True, capture_output=True, timeout=30, check=False)
    assert result.returncode == expect_status, (
        f"{name}: expected {expect_status}, got {result.returncode}\n{result.stdout}\n{result.stderr}"
    )
    return result, compiler, output_dir, env


def main():
    with tempfile.TemporaryDirectory(prefix="unpack-sums-collector-") as temp:
        base = Path(temp)

        result, compiler, output_dir, env = run_case(base, "valid-mixed-statuses", expect_status=0)
        manifest = json.loads((output_dir / "results.json").read_text())
        assert manifest["repeat_count"] == 5
        assert manifest["warmup"]["name"] == "warmup"
        assert len(manifest["measurements"]) == 5
        assert [sample["name"] for sample in manifest["measurements"]] == [
            f"repeat-{index:02d}" for index in range(1, 6)
        ]
        assert [manifest["warmup"]["returncode"]] + [sample["returncode"] for sample in manifest["measurements"]] == STATUSES
        assert manifest["warmup"]["usable"] and all(sample["usable"] for sample in manifest["measurements"])
        assert "--info" in manifest["commands"]["ghc-info"]["argv"]
        assert manifest["commands"]["compile"]["argv"][1:4] == [
            "-O", "-rtsopts", "-no-user-package-db"
        ]
        compile_argv = manifest["commands"]["compile"]["argv"]
        assert compile_argv[4] == "-fforce-recomp"
        assert compile_argv[5] == "-outputdir"
        assert Path(compile_argv[6]) == output_dir
        assert Path(compile_argv[7]) == SOURCE.resolve()
        assert manifest["source"]["sha256"]
        assert manifest["compiler"]["sha256"]
        compiler_calls = [json.loads(line) for line in Path(env["FAKE_GHC_RECORD"]).read_text().splitlines()]
        assert len(compiler_calls) == 2 and all(call["timing_env"] is None for call in compiler_calls)
        timing_env_values = Path(env["FAKE_PROGRAM_TIMING_ENV"]).read_text().splitlines()
        assert timing_env_values == ["1"] * 6
        assert manifest["commands"]["warmup"]["stdout_base64"]

        _, _, compile_fail_dir, _ = run_case(
            base, "compile-failure", extra_env={"FAKE_COMPILE_FAIL": "1"}, expect_status=1
        )
        compile_fail = json.loads((compile_fail_dir / "results.json").read_text())
        assert compile_fail["commands"]["compile"]["returncode"] == 7
        assert not compile_fail["measurements"]

        _, _, timeout_dir, _ = run_case(
            base,
            "compile-timeout",
            extra_env={"FAKE_HANG_COMPILE": "1"},
            extra_args=["--compile-timeout", "0.1", "--total-timeout", "5"],
            expect_status=1,
        )
        timeout_manifest = json.loads((timeout_dir / "results.json").read_text())
        assert timeout_manifest["commands"]["compile"]["timed_out"]

        _, _, descendant_dir, descendant_env = run_case(
            base,
            "descendant-timeout",
            extra_env={
                "FAKE_SPAWN_DESCENDANT": "1",
                "FAKE_CHILD_PID": str(base / "descendant.pid"),
                "FAKE_CHILD_INFO": str(base / "descendant-child.json"),
                "FAKE_GROUP_INFO": str(base / "descendant-group.json"),
            },
            extra_args=["--compile-timeout", "3", "--total-timeout", "8"],
            expect_status=1,
        )
        descendant_manifest = json.loads((descendant_dir / "results.json").read_text())
        assert descendant_manifest["commands"]["compile"]["timed_out"]
        group_info = json.loads(Path(descendant_env["FAKE_GROUP_INFO"]).read_text())
        compile_command = descendant_manifest["commands"]["compile"]
        assert group_info["parent_pid"] == compile_command["pid"]
        assert group_info["parent_pgid"] == compile_command["process_group_id"]
        assert group_info["parent_sid"] == compile_command["session_id"]
        assert group_info["child_pid"] == int(Path(descendant_env["FAKE_CHILD_PID"]).read_text())
        child_pid, child_pgid, child_sid = json.loads(
            Path(descendant_env["FAKE_CHILD_INFO"]).read_text()
        )
        assert child_pid == group_info["child_pid"]
        assert child_pgid == compile_command["process_group_id"]
        assert child_sid == compile_command["session_id"]
        assert descendant_manifest["commands"]["compile"]["elapsed_seconds"] < 4

        _, _, partial_dir, _ = run_case(
            base,
            "partial-manifest",
            extra_env={"FAKE_DELETE_AFTER_INFO": "1"},
            expect_status=1,
        )
        partial = json.loads((partial_dir / "results.json").read_text())
        assert "ghc-info" in partial["commands"]
        assert "compiler_info" in partial
        assert partial["errors"]

        _, _, invalid_dir, _ = run_case(
            base,
            "invalid-measurement",
            extra_env={"FAKE_INVALID_TIMING": "1"},
            expect_status=1,
        )
        invalid_manifest = json.loads((invalid_dir / "results.json").read_text())
        assert invalid_manifest["warmup"] is not None
        assert len(invalid_manifest["measurements"]) == 5
        assert any("invalid timing line" in error for error in invalid_manifest["errors"])

        _, _, occupied_dir, _ = run_case(
            base, "occupied-output", precreate=True, expect_status=2
        )
        assert (occupied_dir / "sentinel").read_text() == "keep"
        assert not (base / "occupied-output-compiler.jsonl").exists()

        nan_dir = base / "non-finite-timeout"
        result = subprocess.run(
            [sys.executable, str(COLLECTOR), "--ghc", str(compiler),
             "--output-dir", str(nan_dir), "--run-timeout", "nan"],
            text=True, capture_output=True, timeout=10, check=False,
        )
        assert result.returncode == 2 and not nan_dir.exists()

    print("PASS: mixed statuses, output isolation, timeout ownership, partial manifest, invalid timing, occupied output, non-finite timeout")


if __name__ == "__main__":
    main()
