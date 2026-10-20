#!/usr/bin/env bash
set -euo pipefail

make_command='make'
if (($#)); then
  if [[ "$#" != 2 || "$1" != --make ]]; then
    echo "usage: $0 [--make 'GNU_MAKE [ARG ...]']" >&2
    exit 2
  fi
  make_command=$2
fi

repo=$(cd "$(dirname "$0")/../.." && pwd)
scratch=$(mktemp -d "${TMPDIR:-/tmp}/macho-dispatch-test.XXXXXX")
trap 'rm -rf "$scratch"' EXIT

fixture="$scratch/recipe fixture"
mock_bin="$scratch/mock-bin"
mkdir -p "$fixture/mk" "$fixture/dist" "$fixture/build" "$mock_bin"
cp "$repo/mk/clean-darwin-macho.sh" "$fixture/mk/clean-darwin-macho.sh"
python3 - "$fixture/mk/clean-darwin-macho.sh" <<'PY'
from pathlib import Path
import sys

path = Path(sys.argv[1])
lines = path.read_text().splitlines()
lines[0] = "#!/path/that/does/not/exist/env bash"
path.write_text("\n".join(lines) + "\n")
PY

cat >"$mock_bin/uname" <<'EOF'
#!/bin/sh
printf 'Linux\n'
EOF
chmod +x "$mock_bin/uname"
export PATH="$mock_bin:$PATH"

if "$fixture/mk/clean-darwin-macho.sh" "$fixture/dist" "$fixture/build" \
    >"$scratch/direct.log" 2>&1; then
  echo "expected direct execution with unusable shebang to fail" >&2
  exit 1
fi

python3 - "$repo/Makefile" "$fixture/Makefile" <<'PY'
from pathlib import Path
import sys

source, destination = map(Path, sys.argv[1:])
recipes = [line for line in source.read_text().splitlines()
           if line.startswith("\t@") and "clean-darwin-macho.sh" in line]
if len(recipes) != 1:
    raise SystemExit("expected one actual Makefile cleanup recipe")
recipe = recipes[0]
destination.write_text(
    "SHELL := bash\n"
    ".SHELLFLAGS := -eu -o pipefail -c\n"
    "DIST_DIR := dist\n"
    "BUILD_DIR := build\n"
    ".PHONY: dispatch\n"
    "dispatch:\n" + recipe + "\n")
PY

python3 - "$make_command" "$fixture" "$scratch/make.log" <<'PY'
from pathlib import Path
import shlex
import sys
import subprocess

arguments = shlex.split(sys.argv[1])
if not arguments:
    raise SystemExit("GNU Make command cannot be empty")
with Path(sys.argv[3]).open("w") as output:
    subprocess.run(arguments + ["-C", sys.argv[2], "-f", "Makefile", "dispatch"],
                   check=True, stdout=output, timeout=30)
PY
grep -F 'Not Darwin — skipping Mach-O cleanup.' "$scratch/make.log" >/dev/null

echo "Makefile cleanup dispatch uses configured shell despite unusable helper shebang"
