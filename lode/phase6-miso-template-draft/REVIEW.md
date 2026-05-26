# Phase 6 miso template — draft review (2026-05-26)

These template files were produced by a research agent during Phase 1 build
wait time. **Not yet validated** against a real wasm cross-compiler build —
they live here as drafts until Phase 6 actually runs.

When ready to ship Phase 6: move these to a new repo
`stable-haskell/miso-wasm-template` (NOT in this repo).

## Critical review

### Bugs found in agent output (must fix before shipping)

1. **`app/Main.hs` — invalid Haskell import alias** (BOTH `#ifdef WASM`
   branch and native branch):
   ```haskell
   import qualified Language.Javascript.JSaddle.Wasm as JSaddle.Wasm   -- INVALID
   import qualified Language.Javascript.JSaddle.Warp as JSaddle.Warp   -- INVALID
   ```
   Haskell module aliases must be single conids — no dots. Fix to e.g.
   `as JSW` / `as Warp`, or import the function directly:
   ```haskell
   import Language.Javascript.JSaddle.Wasm (run)
   ...
   main = run (startApp app)
   ```

2. **`Makefile` `cabal-build` target redundancy**: the CLI flags
   `--with-compiler` / `--with-build-compiler` duplicate what's already in
   `cabal.project`. The project file is enough; drop them from the Makefile
   target (or vice versa — but having both is just confusing).

3. **`README.md` ghcup channel URL is a guess**:
   ```
   https://raw.githubusercontent.com/stable-haskell/ghcup-metadata/main/ghcup-0.0.10.yaml
   ```
   Per our Phase 4 design (lode/wasm-cross-ghcup-plan.md §8.3), the correct
   URL is:
   ```
   https://raw.githubusercontent.com/stable-haskell/ghc-wasm-meta/master/ghcup-stable-wasm-0.0.1.yaml
   ```
   Update once Phase 4 publishes.

4. **`browser_wasi_shim` version mismatch**: agent pinned `@0.3.0`; our RTS
   bumped to `0.4.2` (commit `e0837350f14`, per memory). The user-side
   launcher needs to match what the wasm module expects from WASI imports.
   Verify against the version of `browser_wasi_shim` bundled in
   `lib/targets/wasm32-wasi/lib/` (if shipped) or the version GHC's
   `post-link.mjs` documents.

### Conceptual insight worth promoting

The agent's `cabal.project` uses:
```
if arch(wasm32)
  shared: True
```
*without* a `package *` qualifier. This is the **R7 resolution path (i)** —
it relies on stable-haskell/cabal's Stage/Toolchain split:
- BUILD packages are evaluated against the BUILD architecture (native),
  so `arch(wasm32)` is FALSE → `shared: True` does NOT apply → no `-dynamic-too`
  on `happy-lib`/etc.
- HOST packages are evaluated against the HOST architecture (wasm32),
  so `arch(wasm32)` is TRUE → `shared: True` applies → dyld.mjs can load
  them for TH.

This **only works with stable-haskell/cabal** (or a future cabal that
implements the same split). Stock cabal has only one arch context per build,
so the same form would still pollute BUILD packages.

**Action:** when ready, replicate this pattern in
`cabal.project.stage3` (currently lines 184-186 have the `package *` form
commented out as the bug-prone alternative). With stable-haskell/cabal as
the bootstrap (which is what we use), we may be able to re-enable
`shared: True` for stage3 itself. Test in Phase 5.

### Items to verify against real stage3 output

Per the agent's verification checklist (good list, reproducing the key ones):

1. `post-link.mjs --input/--output` accepted (vs `-i/-o`)
2. `hs_start` actually appears in `wasm-objdump -x myapp.wasm` exports
3. `wasi.initialize(instance)` not `wasi.start(instance)` (reactor vs WASI command)
4. Knot-tying `Object.assign(instance_exports, instance.exports)` works
5. `JSaddle.Wasm.run` (or correct name) exists in `jsaddle-wasm` package
6. `aeson` `-ordered-keymap` flag still exists / still helpful in current aeson
7. `with-build-compiler` is honored from `cabal.project` (not just CLI)

### File inventory

- `app/Main.hs` — Counter app, dual-mode (wasm + native dev). HAS BUG (#1).
- `myapp.cabal` — cabal file with wasm reactor flags. OK.
- `cabal.project` — dual-compiler form. OK; uses correct R7-resolving pattern.
- `Makefile` — self-documenting; targets help/build/link/serve/clean. Minor cleanup (#2).
- `public/index.html` — minimal. OK.
- `public/index.js` — browser launcher. Pin version (#4).
- `README.md` — install + build guide. URL needs updating (#3).
