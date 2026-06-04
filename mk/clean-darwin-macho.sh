#!/usr/bin/env bash
# clean-darwin-macho.sh — strip build-host leaks from every Mach-O
# artefact under $DIST_DIR at CONSTRUCTION TIME (during stage2.dist,
# before tarball assembly), so the bindist ships clean without
# needing a post-build install_name_tool pass in CI.
#
# Two classes of leak to remove:
#
#   (1) Absolute LC_RPATH entries like
#       /Volumes/WorkSpace/_work/ghc/ghc/_build/stage2/store/host/...
#       that the bundled Cabal's depLibraryPaths bakes in via the
#       link line. macOS 14 dyld silently falls through to the
#       portable @executable_path/../lib/<host> rpath SET_RPATH adds,
#       but macOS 15 dyld treats the unresolvable absolute path as
#       fatal and SIGABRTs the binary on launch.
#
#   (2) nix-store LC_LOAD_DYLIB install names for libiconv, libffi,
#       libc++, libz, libresolv, libncurses. The build runner's
#       devx Nix store has these visible at link time, but their
#       install names point at /nix/store paths that don't exist on
#       end-user hosts. Rewrite each to its /usr/lib equivalent
#       (Apple stub-cache library, ABI-compatible).
#
# Mutating a Mach-O invalidates its linker signature; re-sign ad-hoc
# afterwards so dyld accepts the binary on Apple Silicon.
#
# No-op on non-Darwin (the outer uname guard).
#
# Pattern adapted from input-output-hk/devx static.nix:fixup-nix-deps,
# SHA 5f05c1e1af6.
#
# Usage: clean-darwin-macho.sh <DIST_DIR>

set -euo pipefail

[ "$(uname -s)" = "Darwin" ] || { echo "Not Darwin — skipping Mach-O cleanup."; exit 0; }

DIST_DIR="${1:?DIST_DIR required}"
[ -d "$DIST_DIR" ] || { echo "::error::$DIST_DIR not a directory"; exit 1; }

echo "[stage2] Stripping build-host leaks from Mach-O artefacts in $DIST_DIR"

rpath_stripped=0
nix_rewritten=0
files_touched=0

while IFS= read -r -d '' f; do
  file -L "$f" 2>/dev/null | grep -q 'Mach-O' || continue
  changed=0

  # (1) Strip /Volumes/-prefixed LC_RPATH entries
  while IFS= read -r rp; do
    [ -z "$rp" ] && continue
    install_name_tool -delete_rpath "$rp" "$f" 2>/dev/null && {
      rpath_stripped=$((rpath_stripped + 1))
      changed=1
    } || true
  done < <(otool -l "$f" 2>/dev/null \
    | awk '/cmd LC_RPATH/{flag=1; next} flag && /path \/Volumes\//{print $2; flag=0; next} flag && /path /{flag=0}')

  # (2) Rewrite nix-store LC_LOAD_DYLIB → /usr/lib equivalents
  while IFS= read -r nixdep; do
    [ -z "$nixdep" ] && continue
    case "$nixdep" in
      *libiconv.dylib)        new=/usr/lib/libiconv.dylib       ;;
      *libiconv.2.dylib)      new=/usr/lib/libiconv.2.dylib     ;;
      *libffi.*.dylib)        new=/usr/lib/libffi.dylib         ;;
      *libc++.*.dylib)        new=/usr/lib/libc++.dylib         ;;
      *libz.dylib)            new=/usr/lib/libz.dylib           ;;
      *libresolv.*.dylib)     new=/usr/lib/libresolv.dylib      ;;
      *libncursesw.*.dylib)   new=/usr/lib/libncurses.5.4.dylib ;;
      *libncurses.*.dylib)    new=/usr/lib/libncurses.5.4.dylib ;;
      *) continue ;;
    esac
    install_name_tool -change "$nixdep" "$new" "$f" 2>/dev/null && {
      nix_rewritten=$((nix_rewritten + 1))
      changed=1
    } || true
  done < <(otool -L "$f" 2>/dev/null | awk '/nix\/store/{print $1}')

  if [ "$changed" -eq 1 ]; then
    codesign --force --sign - "$f" >/dev/null 2>&1 || true
    files_touched=$((files_touched + 1))
  fi
done < <(find "$DIST_DIR" -type f \( -perm -u+x -o -name '*.dylib' \) -print0)

echo "[stage2] Stripped $rpath_stripped LC_RPATH entries, rewrote $nix_rewritten LC_LOAD_DYLIB entries in $files_touched files"
