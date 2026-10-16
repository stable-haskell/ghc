# miso-wasm-template

Minimal browser-facing miso counter, compiled to WebAssembly with the
stable-haskell wasm cross-GHC distributed via ghcup.

> **DRAFT — not yet validated against a real wasm cross-compiler build.**
> Lives in `lode/phase6-miso-template-draft/` until Phase 6 of the wasm-cross-ghcup
> initiative ships. See `REVIEW.md` for the gap list.

## Prerequisites

```sh
# 1. Wire up the stable-haskell ghcup channel.
ghcup config add-release-channel \
  https://raw.githubusercontent.com/stable-haskell/ghc-wasm-meta/master/ghcup-stable-wasm-0.0.1.yaml

# 2. Cross-GHC + cabal that support `with-build-compiler`.
ghcup install ghc    wasm32-wasi-9.14.0.stable
ghcup install cabal  stable-3.17.0.1

# 3. Native GHC for the build-compiler half of the cross build.
ghcup install ghc 9.10.1 && ghcup set ghc 9.10.1

# 4. Non-GHC wasm tooling (wasi-sdk, node, wasmtime, binaryen).
curl -sSL https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh \
  | SKIP_GHC=1 sh
source ~/.ghc-wasm/env
```

## Build & Serve

```sh
make build   # cabal build + post-link.mjs + stage public/
make serve   # http-server on :8080
```

Open <http://localhost:8080>.

## Native dev loop

Modify `cabal.project` to remove the `with-compiler` lines (or add a
`cabal.project.native` and pass `--project-file=cabal.project.native`):

```sh
cabal run myapp --with-compiler=ghc --with-build-compiler=ghc
```
