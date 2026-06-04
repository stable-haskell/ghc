# Phase 6 v0.0.1 — Trivial wasm reactor (proven working)

> **Status**: empirically verified end-to-end 2026-05-26.
> **Purpose**: smallest possible artifact demonstrating dual-compiler cabal +
> wasm cross GHC + JSFFI runtime work together. NO miso (see "Why no miso").

## What this proves

- **stable-haskell/cabal** at SHA `44817477` honors `with-build-compiler` and
  `with-compiler` in `cabal.project` — one cabal invocation drives two GHCs.
- Native GHC 9.8.4 (devx `ghc98-minimal-ghc`) compiles `Setup.hs` and any
  build-stage tools; wasm32-unknown-wasi-ghc compiles the host-stage code.
- `post-link.mjs` (from `lib/targets/wasm32-unknown-wasi/lib/`) emits a working
  JSFFI ESM glue module.
- The runtime invocation sequence is: `wasi.initialize()` → call
  `__ghc_wasm_jsffi_init()` → call `hs_start()`. **The middle step is
  critical** — skipping it yields `"newBoundTask: RTS is not initialised;
  call hs_init() first"`.

## Why no miso

Miso (and most TH-using packages) need `shared: True` for the wasm target
build, which transitively requires `base.dyn_hi` and friends. The stage3 wasm
target libraries shipped in this repo's current bindist are built with
`shared: False` (per `cabal.project.stage3:184-186`, where the
`if os(wasi) / package * / shared: True` block is commented out due to the
build-package pollution bug). Fix: replace with `if arch(wasm32) shared: True`
(no `package *`) — stable-haskell/cabal's Stage/Toolchain split makes this safe.
Rebuild stage3 + re-package bindist is required; not done in this session.

Once that lands, the `lode/phase6-miso-template-draft/` files can be promoted.

## Files

- `app/Main.hs` — reactor exporting `hs_start`, prints a greeting
- `myapp.cabal` — cabal file with wasm reactor flags (`-no-hs-main`,
  `-optl-mexec-model=reactor`, `--export=hs_start`)
- `cabal.project` — dual-compiler form (template; replace absolute paths with
  bare names when shipped on ghcup)
- `run.mjs` — Node.js launcher that proves end-to-end execution via `node:wasi`
- `public/index.html` + `public/index.js` — browser launcher template using
  `@bjorn3/browser_wasi_shim` (same invocation pattern as `run.mjs`)

## Verified locally

```sh
$ cd lode/phase6-trivial-reactor-poc
$ nix develop /Users/angerman/Projects/stable-haskell/ghc -c \
    /Users/angerman/Projects/stable-haskell/ghc/_build/cabal/bin/cabal build myapp
# … Build profile: -w ghc-9.14 -W ghc-9.8.4 -O1
# … Completed myapp-0.1.0.0 (exe:myapp)
$ WASM=dist-newstyle/store/host/wasm32-unknown-wasi/bin/myapp.wasm
$ node $(ghc --print-libdir)/post-link.mjs -i $WASM -o jsffi.mjs
$ nix-shell -p nodejs_22 --run "node run.mjs"
Hello from the WASM reactor!
```
