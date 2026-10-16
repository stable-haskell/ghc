"""Exercise GHC_CONVERT_OS through autoconf, without configuring GHC."""

import pathlib
import subprocess
import sys
import tempfile


macro = pathlib.Path(sys.argv[1]).resolve()
with tempfile.TemporaryDirectory() as directory:
    work = pathlib.Path(directory)
    (work / "configure.ac").write_text(
        "AC_INIT([android-target-os-test], [1])\n"
        f"m4_include([{macro}])\n"
        'GHC_CONVERT_OS([$test_os], [aarch64], [converted])\n'
        'AS_ECHO([result:$converted])\n'
        "AC_OUTPUT\n"
    )
    subprocess.run(["autoconf"], cwd=work, check=True)
    cases = {
        "android": "linux-android",
        "linux-android": "linux-android",
        "linux-android24": "linux-android",
        "linux-gnu": "linux",
        "linux": "linux",
        "darwin25": "darwin",
    }
    for os_name, expected in cases.items():
        result = subprocess.run(
            ["sh", "./configure", f"test_os={os_name}"],
            cwd=work, check=True, text=True, capture_output=True,
        )
        assert f"result:{expected}" in result.stdout.splitlines(), result.stdout
    result = subprocess.run(
        ["sh", "./configure", "test_os=not-a-supported-os"],
        cwd=work, text=True, capture_output=True,
    )
    assert result.returncode != 0, result.stdout
    assert "Unknown OS not-a-supported-os" in result.stdout, result.stdout
