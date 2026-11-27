#!/usr/bin/env bash
# MicroHs does not implement GHC.Generics (Rep is an empty data type).
# `deriving (...) via Generically T` therefore cannot find
# Semigroup (Rep T ()). The patched compiler can derive Semigroup and
# Monoid for a single-constructor record directly, which is what these
# Cabal flags are. Drop the `via Generically ...` clause in place.
set -euo pipefail
root="${1:?cabal checkout}"
python3 - "$root" <<'PY'
import pathlib, sys
root = pathlib.Path(sys.argv[1])
n = 0
for path in root.rglob("*.hs"):
    text = path.read_text()
    if "via Generically" not in text:
        continue
    out = []
    for line in text.splitlines(keepends=True):
        idx = line.find(" via Generically")
        if idx >= 0:
            nl = "\n" if line.endswith("\n") else ""
            line = line[:idx] + nl
            n += 1
        out.append(line)
    path.write_text("".join(out))
if n == 0:
    raise SystemExit(f"no 'via Generically' clauses under {root}")
print(f"dropped {n} 'via Generically' clauses")
PY
