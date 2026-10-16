#!/usr/bin/env bash
set -euo pipefail

# Official read-only mirror: https://github.com/haskell-wasm/ghc-wasm-meta
# Fetch the whole repository: bootstrap.sh itself fetches another GitLab
# archive, which can return an HTML bot challenge with a successful HTTP status.
# Keep the revision and archive digest together when updating the toolchain.
revision=8079eb96bbc63823d99ad8acaff0765532625474
archive_sha256=4a1d62b2bea9f7c811cfad3d6e0e3ff6d000dfc53b2ea43dfcb3ae7c51345220

workdir=$(mktemp -d "${TMPDIR:-/tmp}/ghc-wasm-meta.XXXXXX")
trap 'rm -rf "$workdir"' EXIT

curl --proto '=https' --tlsv1.2 --fail --location --retry 5 \
  --connect-timeout 20 --max-time 180 --retry-max-time 180 \
  "https://codeload.github.com/haskell-wasm/ghc-wasm-meta/tar.gz/$revision" \
  --output "$workdir/source.tar.gz"
printf '%s  %s\n' "$archive_sha256" "$workdir/source.tar.gz" | shasum -a 256 -c -

mkdir "$workdir/source"
tar -xzf "$workdir/source.tar.gz" --strip-components=1 -C "$workdir/source"
test -f "$workdir/source/setup.sh"
test -f "$workdir/source/autogen.json"

# Preserve upstream's complete setup and the caller's FLAVOUR/PREFIX policy.
# In particular, do not skip GHC or replace the pinned dependency metadata.
bash "$workdir/source/setup.sh"
