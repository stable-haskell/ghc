# MicroHs Stage 1 -> Stage 2 GHC Bootstrap TODO & Roadmap

This tracks the work to bootstrap a full, self-contained GHC Stage 2 compiler
starting solely from a C compiler and [augustss/MicroHs](https://github.com/augustss/MicroHs)
(zero preinstalled `ghc`, `happy`, or `alex`).

## Completed

- [x] **Drop `BOOT_GHC` completely** in `bootstrap/mhs/build-stage1-mhs.sh`:
  all eight Stage 1 tools (`unlit`, `happy`, `alex`, `genapply`,
  `deriveConstants`, `genprimopcode`, `ghc-pkg`, `hsc2hs`) build natively from
  source with `cc` and `mhs` under a sanitized `PATH` free of `ghc`, `happy`,
  and `alex`.
- [x] **Fix `__MHS__` `Fingerprint.hs` performance**: replaced `[Word8]`
  allocation + FNV-1a across `GHC.Utils.Fingerprint`, `GHC.Data.StringBuffer`,
  `GHC.Driver.Downsweep`, and `GHC.Iface.Tidy.StaticPtrTable` with MicroHs's C
  runtime `md5Array` (`src/runtime/md5.c`), achieving `558x` (`1 MB`) to
  `1,008x` (`8 MB`) speedups and bit-identical RFC 1321 MD5 digests.
- [x] **Restore portable Haskell 2010 `happy` & `alex` backends and delete the
  `gen-parsers.sh` post-processor**:
  - Pushed forks to `github:stable-haskell`:
    - `https://github.com/stable-haskell/MicroHs` (`feat/ghc-stage1`)
    - `https://github.com/stable-haskell/hsc2hs` (`feat/mhs-bootstrap`)
    - `https://github.com/stable-haskell/happy` (`feat/portable-haskell2010`)
    - `https://github.com/stable-haskell/alex` (`feat/portable-bootstrap`)
  - Verified exact fixed-point self-bootstrap for both `happy` and `alex` under
    MicroHs and eliminated `mhsify_happy`, `python3`, `/nix/store/*` pins, and
    `grep -v ghcconfig.h` from `bootstrap/mhs/gen-parsers.sh`.
  - Removed the monolithic `bootstrap/mhs/microhs-stage1.patch`.

## Demo GitHub Actions Workflow (`mhs`-compiled Stage 2)

Workflows, scripts, and semantic patch series landed in
`.github/workflows/mhs-stage2-demo.yml`,
`.github/workflows/mhs-stage2-demo-app.yml`,
`bootstrap/mhs/patches/microhs/`, `bootstrap/mhs/ci-time.sh`,
`cabal.project.stage2.mhs`, and `bootstrap/mhs/demo-app/`.
Target runner matrix: `ubuntu-24.04` (`x86_64-linux`) and `macos-latest`
(`aarch64-darwin`), asserting zero preinstalled `ghc`, `happy`, or `alex`.

- [x] **Phase 1 — Checkout upstream `MicroHs` source**
  (`augustss/MicroHs` at `9e1cd08`).
- [x] **Phase 2 — Super-charge `mhs` via semantic git-extracted `.patch` files**
  (split `stable-haskell/MicroHs` `feat/ghc-stage1-series` at `9ae73f34` into 8
  atomic issue-linked commits and exported
  `bootstrap/mhs/patches/microhs/0001..0008-*.patch` + `MANIFEST.tsv` via
  `git format-patch`, verified 0-byte diff vs `29504a98`):
  - [augustss/MicroHs#367](https://github.com/augustss/MicroHs/issues/367) —
    `TypeFamilies` & `DataFamilies` (open, closed, associated, injective,
    coincident instance deduplication, higher-kinded associated family arity,
    cyclic `b ~ F b` superclass orientation)
  - [augustss/MicroHs#446](https://github.com/augustss/MicroHs/issues/446) &
    [augustss/MicroHs#226](https://github.com/augustss/MicroHs/issues/226) —
    `DataKinds` promoted constructors (`'GhcPs`/`'GhcRn`/`'GhcTc`),
    `StandaloneKindSignatures`, and kind synonyms
  - [augustss/MicroHs#449](https://github.com/augustss/MicroHs/issues/449),
    [augustss/MicroHs#501](https://github.com/augustss/MicroHs/issues/501), &
    [augustss/MicroHs#502](https://github.com/augustss/MicroHs/issues/502) —
    GADT constraint propagation, existential skolemization, and local constraint
    solving
  - [augustss/MicroHs#365](https://github.com/augustss/MicroHs/issues/365) —
    `-XNoImplicitPrelude` and `LANGUAGE NoImplicitPrelude`
  - [augustss/MicroHs#278](https://github.com/augustss/MicroHs/issues/278) —
    `.hs-boot` mutual-recursion knot-tying & record-selector link aliases
  - [augustss/MicroHs#294](https://github.com/augustss/MicroHs/issues/294) &
    [augustss/MicroHs#18](https://github.com/augustss/MicroHs/issues/18) —
    pattern synonym `{}` handling, multi-comma `TupleSections`, and
    `largeRecordFieldLimit = 64` for Cabal `Lens` updates (adjacent to
    [PR #567](https://github.com/augustss/MicroHs/pull/567) &
    [PR #559](https://github.com/augustss/MicroHs/pull/559))
  - Runtime & FFI slices: 512 MB stack + `WANT_OVERFLOW=0` two's-complement
    wrapping, polymorphic `foreign import` `EForall` desugaring (`T_IO_CCALL`),
    `DeepSeq`/`Serialize` helpers
- [x] **Phase 3 — Build all 8 Stage 1 tools natively with `mhs`**
  (`unlit`, `happy`, `alex`, `genapply`, `deriveConstants`, `genprimopcode`,
  `ghc-pkg`, `hsc2hs`) — verified locally under zero-`ghc`/`happy`/`alex` `PATH`.
- [x] **Phase 4 — Checkout `cabal-install` (dual-compiler base `73d3ffda3e`) and
  super-charge it** by applying the MicroHs patch series from
  `stable-haskell/cabal` (`73d3ffda3e..b04f60210`).
- [x] **Phase 5 — Build `cabal-install` (`exe:cabal`) with `mhs`**
  (wired to run concurrently with Phase 6 on a parallel GitHub Actions job).
- [x] **Phase 6 — Concurrently build Stage 1 GHC with `mhs`**
  (staged checkpoints `00..08` + link + native codegen smoke test; verified
  locally and wired in `stage1-mhs` job until `cabal-install` drives Stage 1).
- [x] **Phase 7 — Wire Stage 2 GHC build with `cabal-mhs` + `mhs`-built Stage 1
  GHC** using `cabal.project.stage2.mhs` (disabling `+build-tool-depends` so
  MicroHs-built tools satisfy all build tools without `GHC0`).
- [x] **Phase 8 — Wire Stage 2 GHC bindist artifact packaging & per-phase
  timing summaries** (`ghc-stage2-mhs-<plat>.tar.gz` + `ci-time.sh summary` ->
  `$GITHUB_STEP_SUMMARY`).
- [x] **Dependent downstream workflow (`.github/workflows/mhs-stage2-demo-app.yml`)**:
  downloads the packaged Stage 2 compiler artifact onto a fresh runner with no
  preinstalled GHC and compiles + executes `bootstrap/mhs/demo-app/` (both
  direct `ghc -O2` and offline `cabal-mhs`).

## Next / Follow-Up Milestones

- [ ] **Live end-to-end Stage 2 verification**: run Phase 7 (`rts` ->
  `ghc-prim` -> `ghc-bignum` -> `ghc-internal` -> `base` -> `compiler` ->
  `ghc`) using the `mhs`-built Stage 1 `ghc` + `mhs`-built `cabal` and fix any
  Stage 1 runtime/codegen bugs uncovered during full Stage 2 compilation.
- [ ] **Drive Stage 1 directly via `cabal-install`** (eliminating the bespoke
  staged shell loop in `build-stage1-mhs.sh` once `cabal-install` supports
  MicroHs as a compiler target for multi-package projects).
- [ ] **Upstream clean PRs** from the atomic commit series:
  - `augustss/MicroHs` (`#367`, `#446`/`#226`, `#449`/`#501`/`#502`, `#365`,
    `#278`, `#294`, `#18`)
  - `haskell/happy` (portable Haskell 2010 backend when `-g` is omitted)
  - `haskell/alex` (omit `ghcconfig.h` outside `-g`, portable bootstrap, avoid
    lambda-lifted array recomputation)
  - `haskell/hsc2hs` (`__MHS__` portability guards)
  - `haskell/cabal` (MicroHs bootstrap + dual-compiler integration)
