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

### Empirical finding (2026-05-26): miso requires stage3 rebuilt with shared:True

Direct attempt to build `miso 1.11.0` against the dual-compiler stable-haskell stack
hit `character-ps-0.1` → `Failed to load dynamic interface file for Data.Word:
base-4.22.0.0/Data/Word.dyn_hi: does not exist`. Root cause: our stage3 wasm
target libraries (base, ghc-internal, etc.) were built with `shared: False` per
`cabal.project.stage3` (lines 184-186 — the old `if os(wasi) package * shared:
True` block is commented out due to the build-package pollution bug). miso (and
most TH-using packages) require `shared: True`, which transitively requires
base.dyn_hi.

**R7 resolution path (i)** — replace the commented-out form with
`if arch(wasm32) shared: True` (no `package *`). With stable-haskell/cabal's
Stage/Toolchain split, this applies ONLY to host-arch packages, not BUILD-arch
packages → no `happy-lib`/`alex` pollution. To validate, rebuild stage3 and
re-package the bindist. Order of hours; not done in this session.

**Phase 6 deliverable for v0.0.1** (this session): a trivial reactor template
(no miso) proving the full toolchain works end-to-end. Miso integration is
queued as Phase 6.5, contingent on the stage3 shared rebuild.

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

Per the agent's verification checklist (with empirical results from 2026-05-26):

1. ✅ `post-link.mjs --input/--output` accepted — confirmed locally; produces a 4915-byte ESM JSFFI glue file.
2. ✅ `hs_start` is exported — `wasm-objdump -x myapp.wasm` shows it alongside `_initialize`, `__ghc_wasm_jsffi_init`, `memory`, and the `rts_*` runtime helpers.
3. ✅ `wasi.initialize(instance)` for reactors — confirmed. But ALSO must call `instance.exports.__ghc_wasm_jsffi_init()` BEFORE `hs_start()`. Without that step, `hs_start()` throws `"newBoundTask: RTS is not initialised; call hs_init() first"`. The drafted `index.js` has been updated to include this step.
4. ✅ Knot-tying via `Object.assign(__exports, instance.exports)` after `WebAssembly.instantiate` and before `wasi.initialize` works.
5. ⏳ `JSaddle.Wasm.run` correctness — to verify when adding miso; trivial reactor (no miso) is the current empirical test point.
6. ⏳ `aeson` `-ordered-keymap` flag — to verify with miso layered in.
7. ✅ `with-build-compiler` is honored from `cabal.project` — confirmed with stable-haskell/cabal at SHA `44817477`. Build profile reports `-w ghc-9.14 -W ghc-9.8.4`. Cabal source: `cabal-install/src/Distribution/Client/ProjectConfig/FieldGrammar.hs:123`.
8. ✅ Reactor exports include `__ghc_wasm_jsffi_init` — confirmed via `wasm-objdump`; this is the RTS-init function that the launcher MUST call after `wasi.initialize()`.

### File inventory

- `app/Main.hs` — Counter app, dual-mode (wasm + native dev). HAS BUG (#1).
- `myapp.cabal` — cabal file with wasm reactor flags. OK.
- `cabal.project` — dual-compiler form. OK; uses correct R7-resolving pattern.
- `Makefile` — self-documenting; targets help/build/link/serve/clean. Minor cleanup (#2).
- `public/index.html` — minimal. OK.
- `public/index.js` — browser launcher. Pin version (#4).
- `README.md` — install + build guide. URL needs updating (#3).
