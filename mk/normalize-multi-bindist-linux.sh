#!/usr/bin/env bash
# Rewrite ELF paths in a multi-target Linux bindist for a native install.
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 BINDIST.tar.gz ELF_INTERPRETER" >&2
  exit 2
fi

TGZ=$1
INTERP=$2
STAGE=$(mktemp -d)
trap 'rm -rf "$STAGE"' EXIT
tar -C "$STAGE" -xzf "$TGZ"

# The package names in a stage2 libdir identify the native Haskell library
# directory. Keep its full config.sub triple (including ABI suffixes such as
# -gnu) instead of rebuilding a shorter name from the runner architecture.
HOST_LIB_DIRS=()
while IFS= read -r -d '' ghc_so; do
  candidate=${ghc_so%/*}
  rel_candidate=${candidate#"$STAGE/lib/"}
  # Native libraries live directly under lib/<triple>. Cross libraries are
  # nested under lib/targets/<target>/lib/<target> and must not be selected.
  if [[ "$candidate" == "$STAGE/lib/"* && "$rel_candidate" != */* ]]; then
    duplicate=false
    for existing in "${HOST_LIB_DIRS[@]}"; do
      [[ "$existing" == "$candidate" ]] && duplicate=true
    done
    [[ "$duplicate" == true ]] || HOST_LIB_DIRS+=("$candidate")
  fi
done < <(find "$STAGE/lib" -type f -name 'libHSghc-[0-9]*.so' -print0)
if [[ ${#HOST_LIB_DIRS[@]} -ne 1 ]]; then
  echo "expected exactly one native libHSghc directory, found ${#HOST_LIB_DIRS[@]}" >&2
  printf '  %s\n' "${HOST_LIB_DIRS[@]}" >&2
  exit 1
fi
HOST_LIB_DIR=${HOST_LIB_DIRS[0]}
HOST_DIR=${HOST_LIB_DIR#"$STAGE/lib/"}
if [[ "$HOST_DIR" == */* || "$HOST_DIR" == "$HOST_LIB_DIR" ]]; then
  echo "native GHC libraries are not directly under lib/: $HOST_LIB_DIR" >&2
  exit 1
fi
shopt -s nullglob
CABAL_LIBS=("$HOST_LIB_DIR"/libHSCabal-[0-9]*.so)
has_cabal_lib=false
for cabal_lib in "${CABAL_LIBS[@]}"; do
  # A directory or dangling symlink with a package-looking name is not a
  # library file. The patch loop below also operates on regular files only.
  if [[ -f "$cabal_lib" && ! -L "$cabal_lib" ]]; then
    has_cabal_lib=true
    break
  fi
done
if [[ "$has_cabal_lib" != true ]]; then
  echo "native libdir lacks libHSCabal shared library: $HOST_LIB_DIR" >&2
  exit 1
fi

echo "═══ Patchelfing binaries to interpreter $INTERP ═══"
patched=0
while IFS= read -r -d '' bin; do
  if file -L "$bin" 2>/dev/null \
       | grep -q "ELF.*dynamically linked.*interpreter /nix/store"; then
    echo "  patching: ${bin#"$STAGE"/}"
    patchelf --set-interpreter "$INTERP" "$bin"
    patchelf --remove-rpath "$bin"
    patchelf --force-rpath --set-rpath "\$ORIGIN/../lib/$HOST_DIR" "$bin"
    patched=$((patched + 1))
  fi
done < <(find "$STAGE" -type f -executable -print0)
echo "═══ Patched $patched binaries ═══"
if [[ $patched -eq 0 ]]; then
  echo "no Nix-linked ELF binaries found in bindist" >&2
  exit 1
fi

echo "═══ Setting rpath on lib/$HOST_DIR/*.so ═══"
so_patched=0
while IFS= read -r -d '' so; do
  patchelf --force-rpath --set-rpath '$ORIGIN' "$so"
  so_patched=$((so_patched + 1))
done < <(find "$HOST_LIB_DIR" -maxdepth 1 -type f -name '*.so' -print0)
if [[ $so_patched -eq 0 ]]; then
  echo "native libdir contains no shared libraries: $HOST_LIB_DIR" >&2
  exit 1
fi
echo "═══ Set rpath on $so_patched shared libs ═══"

tar -C "$STAGE" -czf "$TGZ" .
