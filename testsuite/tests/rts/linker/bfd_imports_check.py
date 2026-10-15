"""Reject short import archives, which do not exercise the BFD lookup path."""

import argparse
import os
import pathlib
import shutil
import subprocess


def gnu_dlltool():
    # CLANG64 installs an LLVM dlltool alias before MSYS's GNU dlltool on
    # PATH. Inspect every matching executable, including cross-prefixed ones.
    rejected = []
    for name in ("x86_64-w64-mingw32-dlltool", "dlltool"):
        for directory in os.get_exec_path():
            tool = shutil.which(name, path=directory)
            if tool is None:
                continue
            try:
                version = subprocess.run(
                    [tool, "--version"], capture_output=True, text=True,
                )
            except OSError as error:
                rejected.append(f"{tool}: {error}")
                continue
            # GNU prints argv[0] after its vendor prefix, so the banner may
            # contain a qualified path or a cross-prefixed executable name.
            if version.returncode == 0 and version.stdout.startswith("GNU "):
                return tool
            output = (version.stdout or version.stderr).splitlines()
            banner = output[0] if output else "no version output"
            rejected.append(f"{tool}: exit {version.returncode}, {banner!r}")
    details = "\n".join(rejected) or "no dlltool candidates found on PATH"
    raise RuntimeError(
        "GNU dlltool is required to test BFD import-library members; checked:\n"
        + details
    )


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--definition", help="generate the archive from this .def file")
parser.add_argument("archive")
args = parser.parse_args()
if args.definition:
    subprocess.run(
        [gnu_dlltool(), "-m", "i386:x86-64", "-d", args.definition,
         "-l", args.archive], check=True,
    )

archive = pathlib.Path(args.archive).read_bytes()
assert archive.startswith(b"!<arch>\n"), "not an archive"
for marker in (b".idata$6", b".idata$7", b"_iname"):
    assert marker in archive, "test requires GNU BFD import-library members"
