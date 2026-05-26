# Stable-Haskell WASM Cross-Compiler via ghcup — Plan

> **Status:** Active. Branch: `feat/wasm-cross-ghcup` off `stable-ghc-9.14`.
> **Owner:** angerman.
> **Last updated:** 2026-05-26.

This is the living source-of-truth for the initiative. Update the **Status Log** at
the bottom as phases progress. Keep this doc tight — point at code/PRs rather than
duplicating their content.

---

## 1. Purpose

End-state: a Haskell developer can run

```bash
ghcup config add-release-channel <stable-haskell channel URL>
ghcup install ghc wasm32-wasi-9.14.0.stable -- $CONFIGURE_ARGS
ghcup install cabal stable-3.17.0.1
git clone https://github.com/stable-haskell/miso-wasm-template && cd $_
make && make serve   # opens a working miso app in the browser
```

…using **our** stack: stable-haskell GHC (245+ patches over upstream 9.14),
stable-haskell/cabal with dual-compiler ("compile-less") support, and a thin
ghcup channel that reuses upstream `ghc-wasm-meta` for the host-side
wasi-sdk/node/wasmtime pieces.

## 2. Why us, not upstream `ghc-wasm-meta`

| Capability | Upstream `ghc-wasm-meta` | This initiative |
|---|---|---|
| GHC bindist | upstream (master / RCs) | **stable-haskell/ghc** (245-patch fork, stable-9.14 line) |
| Cabal | stock 3.14.x with `wasm32-wasi-cabal` wrapper + injected config | **stable-haskell/cabal** 3.17.0.1 with real dual-compiler (`with-compiler` / `with-build-compiler`) + compile-less store + file-monitor recompilation avoidance |
| wasi-sdk / node / wasmtime / binaryen | bundled via `bootstrap.sh` | **reuse upstream's `bootstrap.sh`** with `SKIP_GHC=1` (no fork) |
| ghcup channel | `haskell-wasm/ghc-wasm-meta` channel | new `stable-haskell/ghc-wasm-meta` channel, distinct version suffix |
| Custom-Setup packages with cross | not handled (single cabal, single compiler) | works (Setup.hs compiled by build-compiler, target code by host-compiler) |

## 3. Current State Assessment (verified 2026-05-26)

### What's already on `stable-ghc-9.14`

- 245+ WASM patches: RTS WASI guards, JSFFI init ctors, NCG `.functype`/`.size`,
  on-demand `GlobalRegs`, libffi-clib exclusion for WASI, dyld handling, browser_wasi_shim
  bump, etc.
- `cabal.project.stage3` with `if os(wasi) / package * / shared: True` (the
  build-package pollution bug is **fixed**: the override is correctly nested under `os(wasi)`).
- Makefile `STAGE3_PLATFORMS := … wasm32-unknown-wasi` with full toolchain wiring
  (CC/CXX/AR/RANLIB + GHC_TOOLCHAIN_ARGS for `--merge-objs wasm-ld`,
  `--disable-tables-next-to-code`, `--disable-libffi-adjustors`).
- Stage3 target → `_build/dist/ghc-wasm32-unknown-wasi.tar.gz` plus
  JS runtime shims (`dyld.mjs`, `ghc-interp.js`, `post-link.mjs`, `prelude.mjs`).
- `feat/wasm-fixes` adds **zero** genuinely new content (its commits are already
  in `stable-ghc-9.14` by patch-id). Ignore that branch.

### What's NOT on `stable-ghc-9.14` (Phase 0 absorbs these)

- **`build/wasm-nix-environment`** (6 commits): `flake.nix` pulling `wasi-sdk`
  from upstream `ghc-wasm-meta` flake, env vars `WASM_EXTRA_{LIB,INCLUDE}_DIRS`,
  USAGE docs.
- **`feat/nix-ci-split`** (6 commits): `Makefile` cross-build refactor (dist-based
  configuration), `.github/workflows/nix-ci.yml` split into build/test/cross jobs,
  RTS fix for undefined symbols referenced only by `R_*_NONE` relocations, plus
  build-infra fixes (stamp-based deps, PHONY ordering, race condition).

### What doesn't exist anywhere yet

- ghcup-installable bindist shape (current tarball has no `./configure && make install`).
- Published `stable-haskell/ghc-wasm-meta` ghcup channel.
- stable-haskell/cabal binary releases for ghcup.
- Miso template proving the dual-compiler workflow.
- Browser launcher template (neither miso-sampler nor upstream `ghc-wasm-meta`
  commits one — biggest documentation gap in the ecosystem).

## 4. Decisions made

| # | Decision | Rationale |
|---|---|---|
| D1 | Working branch: `feat/wasm-cross-ghcup` off `stable-ghc-9.14`. | Don't pollute `stable-ghc-9.14` until Phase 2 is CI-green. |
| D2 | Rename triple `wasm32-unknown-wasi` → `wasm32-wasi`. | Match ecosystem convention (`wasm32-wasi-ghc-…`, ghcup-cross YAML, miso-sampler). Mechanical Makefile change; `ghc-toolchain` accepts both. |
| D3 | Phase-3 bindist shape: **relocatable tarball + `relocate.sh`**, ghcup `viPostInstall` invokes it. | Ships fast; can migrate to full configure-bindist (`./configure --prefix && make install`) later if user feedback demands. |
| D4 | Reuse upstream `ghc-wasm-meta` `bootstrap.sh` with `SKIP_GHC=1` for wasi-sdk/node/wasmtime/binaryen. | These pieces aren't our value-add. Tracking upstream wasi-sdk updates is a tax we don't want. |
| D5 | Do not ship a native stable-haskell GHC bindist for build-compiler use. | Any user-installed GHC (ghcup 9.10.x / 9.12.x) suffices. Revisit if "full stable stack" becomes a goal. |
| D6 | Channel hosting: GitHub raw — `raw.githubusercontent.com/stable-haskell/ghc-wasm-meta/master/ghcup-stable-wasm.yaml`. Bindists as release assets in `stable-haskell/ghc-wasm-bindists`. | Simple, no GH Pages needed, standard pattern. |
| D7 | Version suffix on ghcup entries: `9.14.0.stable.YYYYMMDD`. | Avoid collision with upstream `wasm32-wasi-9.14`; allow multiple stable releases. |

## 5. Open decisions / risks

- **R1: stable-haskell/cabal 3.17 vs the wasm-known cabal 3.16 regression.**
  Upstream `ghc-wasm-meta` README pins cabal 3.14.x. Our 3.17 tracks unreleased
  master + Andrea's patches. Must be verified by running the miso e2e (Phase 6)
  before claiming it works.
- **R2: aarch64-darwin CI 41GB APFS budget.** Stage3 adds ~3-5GB; may need
  `make clean-stage{0,1,2}` between stage2 and stage3 to free space. Determine
  empirically in Phase 2.
- **R3: ghc-toolchain triple acceptance.** D2 assumes `wasm32-wasi` works; verify
  in Phase 1 by running `ghc-toolchain --triple wasm32-wasi …` and inspecting
  the generated `settings` file.
- **R4: `build-type: Custom` package compatibility.** Andrea's dual-compiler
  patches are explicitly tested only against GHC's own stage1/stage2. Real-world
  miso transitive deps with `Custom`/`Configure` build-types may surface bugs.
  Phase 5 gate covers one Custom-build-type package as a smoke test; expect
  follow-up bug reports.
- **R5: Path-baking in `settings`.** wasi-sdk path is hardcoded into the GHC
  `settings` file after install. If a user moves/upgrades `~/.ghc-wasm/wasi-sdk`,
  installed GHC breaks. `relocate.sh` (D3) must rewrite these paths; `viPostInstall`
  must invoke it.
- **R6: Custom channel + upstream `cross` channel collision.** If a user has
  both channels enabled, the last-added wins for any colliding version key.
  D7 mitigates this.
- **R7: TH/GHCi on wasm needs `shared: True` for target libs, but stock
  cabal `package *` clauses pollute BUILD packages too.** Discovered 2026-05-26
  inspecting `cabal.project.stage3` lines 175-186 — the `if os(wasi) / package *
  / shared: True` block is **commented out** because it caused `-dynamic-too`
  on `happy-lib`/etc. compiled by the native build compiler, which fails when
  the dist artifact lacks `.dyn_hi` files. Consequences:
  (a) Phase 1 TH smoke-test gate currently CANNOT pass — the stage3 base
  libraries are built `shared: False`, so dyld.mjs at runtime has nothing to load.
  (b) Phase 6 miso e2e similarly blocked (miso uses TH heavily).
  Resolutions in priority order: **(i)** spell out per-package `shared: True`
  for the wasm target libraries individually (avoiding `package *`); **(ii)** wait
  for stable-haskell/cabal to gain Host-only `package *` clauses (extension of
  the Stage/Toolchain work — not in `stable-haskell/master` HEAD as of
  2026-05-26); **(iii)** ship without TH for v0.0.1, document, fix in v0.0.2.
  Decision pending Phase 1 attempt; (i) is the safest path forward.

## 6. Phases & gates

Each phase has **one** clear pass/fail gate. Don't advance until the prior gate is met.

### Phase 0a — Branch + planning docs (IN PROGRESS)
- Create `feat/wasm-cross-ghcup` off `stable-ghc-9.14`.
- Write `lode/wasm-cross-ghcup-plan.md` (this file).
- Update project `CLAUDE.md` with a "Current Initiative" section linking here.
- Commit.
- **Gate:** branch checked out; one commit on top of `stable-ghc-9.14`; planning
  docs reviewable by anyone fresh.

### Phase 0 — Consolidate WASM feature branches
- Cherry-pick 6 commits from `build/wasm-nix-environment` (flake.nix + wasi-sdk).
- Cherry-pick 6 commits from `feat/nix-ci-split` (Makefile cross refactor + CI split + RTS R_*_NONE fix + build-infra fixes).
- Resolve conflicts. Skip `feat/wasm-fixes` (already merged by patch-id).
- Note: flake exposes single `devShells.default` (no `wasm-cross` attr). The shell creates wrappers in `.nix-wasm-bin/` for `wasm32-wasi-{clang,clang++,ar,ranlib}`, `wasm-ld`, `llc`/`opt`/`llvm-as` (LLVM 21). `wasm-opt` (binaryen) is NOT provided — not strictly needed for compiler bring-up.
- **Gate:** `nix develop --command bash -c 'which wasm32-wasi-clang wasm-ld llc && llc --version | head -2'` succeeds and prints LLVM 21.

### Phase 1 — Local wasm cross build green
- Apply D2 (rename triple `wasm32-unknown-wasi` → `wasm32-wasi`); regenerate Makefile var refs.
- In devShell: `make CABAL=$PWD/_build/stage0/bin/cabal stage3-wasm32-wasi`.
- Smoke-test `_build/dist/bin/wasm32-wasi-ghc`:
  - Compile a one-file `hello :: IO ()`; run produced `.wasm` via `wasmtime` AND node + `browser_wasi_shim`.
- Attempt R7 resolution (i): add per-package `shared: True` for the wasm target libs (base, ghc-internal, ghc-bignum, integer-gmp, etc. — *not* `package *`). Rebuild stage3 — verify happy-lib/alex still build cleanly.
- If (i) works: compile a tiny TH-using package, run via Node iserv.
- **Gate:** non-TH hello-world runs in both `wasmtime` AND node+browser_wasi_shim. **TH gate moved to "stretch"**: if R7(i) succeeds → TH-using package runs; else log R7 as known limitation for v0.0.1 and defer to Phase 5/6.

### Phase 2 — CI wasm cross build green (both host platforms)
- Add `wasm-cross-{x86_64-linux, aarch64-darwin}` jobs to `.github/workflows/nix-ci.yml`.
- Cache wasi-sdk via nix-store. Mind aarch64-darwin 41GB APFS (R2).
- Each job: nix-develop wasm-cross → stage0/1/2 → stage3-wasm32-wasi → Phase-1 smoke test.
- **Gate:** PR's wasm-cross jobs green on both runners; total runtime <90 min per host.

### Phase 3 — Ghcup-compatible wasm bindist
**Scope shrunk (2026-05-26)**: investigation of `Makefile:409-450` shows
`DIST_COPY_LIB_CONF_CROSS` already rewrites absolute paths in `*.conf` files
to `${pkgroot}/../lib/wasm32-wasi/…` and `ghc-pkg recache` is run for the
cross-target package db at build time (line 1018). The settings file uses
**bare** tool names (`wasm32-wasi-clang`/`wasm-ld`/etc.) so no path baking
there either. Therefore bindists are *already* relocatable; `relocate.sh`
shrinks to a one-liner that re-runs `ghc-pkg recache` for the new install
location (the cache is binary and not portable).

- Repackage `_build/dist/ghc-wasm32-wasi.tar.gz` (Makefile:1029) into
  `wasm32-wasi-ghc-9.14.0-<host>.tar.xz`:
  - Keep existing layout: `bin/wasm32-wasi-ghc{,-pkg,-iserv,…}`, `lib/targets/wasm32-wasi/…`.
  - Append: `relocate.sh` (10 lines) at top-level.
  - Verify JS shims (`dyld.mjs`, `post-link.mjs`, `prelude.mjs`, `ghc-interp.js`) are present in `lib/targets/wasm32-wasi/lib/`.
  - Switch compression to `.tar.xz` (smaller than `.tar.gz`; ghcup accepts both).
- Smoke-test: untar to a random `$prefix`, run `$prefix/relocate.sh`, then `$prefix/bin/wasm32-wasi-ghc --info` + compile hello-world.
- **Gate:** tarball untars + relocates + compiles hello-world on a fresh machine without errors.

### Phase 4 — Ghcup channel published
- Create `stable-haskell/ghc-wasm-meta` repo (or repurpose).
- Add `bootstrap.sh` delegating to upstream `ghc-wasm-meta` `bootstrap.sh` with `SKIP_GHC=1`; writes `~/.ghc-wasm/env` exporting `$CONFIGURE_ARGS`.
- Author `ghcup-stable-wasm-0.0.1.yaml`: one `GHC: wasm32-wasi-9.14.0.stable.YYYYMMDD` entry per `(arch, OS)` (start with `A_64/Linux_UnknownLinux` and `A_ARM64/Darwin`).
- Bindists hosted as release assets at `stable-haskell/ghc-wasm-bindists`.
- `viPostInstall` invokes `relocate.sh`.
- **Gate:** on a clean machine: `ghcup config add-release-channel <url> && ghcup install ghc wasm32-wasi-9.14.0.stable -- $CONFIGURE_ARGS` succeeds; `wasm32-wasi-ghc --version` works.

### Phase 5 — Ship stable-haskell/cabal binary
- CI job builds static cabal-install 3.17.0.1 from `stable-haskell/cabal#stable-haskell/master` for x86_64-linux and aarch64-darwin.
- Add Cabal entry to ghcup YAML (or sibling channel).
- Verify dual-compiler `cabal.project` form: one `cabal build` uses native ghc for Setup.hs + wasm32-wasi-ghc for target.
- **Gate:** a package with `build-type: Custom` builds successfully via dual-compiler (exercises Andrea's patches, not just `Simple`).

### Phase 6 — Miso end-to-end demo
- Create `stable-haskell/miso-wasm-template` repo:
  - `app/Main.hs` minimal miso app.
  - `myapp.cabal` with `if arch(wasm32) / ghc-options: -no-hs-main -optl-mexec-model=reactor "-optl-Wl,--export=hs_start"`.
  - `cabal.project` with dual-compiler + `if arch(wasm32) shared: True`.
  - `Makefile`: `cabal build` → `post-link.mjs` → `cp` to `public/` → `npx http-server public`.
  - `public/index.html` + `public/index.js` browser launcher using `browser_wasi_shim` — **commit it**; close the ecosystem-wide documentation gap.
- Validate on a fresh machine using only our ghcup channel.
- **Gate:** developer goes `git clone` → `make && make serve` → working browser demo in <5 min (post-install).

### Phase 7 — Documentation & migration
- README on `stable-haskell/ghc-wasm-meta` repo: what we offer vs upstream
  `ghc-wasm-meta`, exact install/use commands, dual-compiler `cabal.project`
  template, known limitations (no `cabal install miso` Hackage path, no
  subprocess-using libs in browser, etc.).
- Migration notes for upstream `ghc-wasm-meta` users.
- Link from main `stable-haskell/ghc` README.
- **Gate:** dev unfamiliar with this work reaches working browser demo from
  README alone, no questions asked.

## 7. References

- Project root `CLAUDE.md` — multi-stage build system overview.
- `cabal.project.stage3`, `Makefile` (STAGE3_* vars) — current cross wiring.
- `stable-haskell/cabal#stable-haskell/master` (commit `44817477ff6d`) — dual-compiler Cabal.
- `haskell-wasm/ghc-wasm-meta` — upstream wasi-sdk/node/wasmtime installer (to reuse).
- `haskell-wasm/ghc-wasm-bindists` — upstream's bindist hosting pattern.
- `haskell/ghcup-metadata`, `ghcup-cross-0.1.0.yaml` — channel schema reference.
- `tweag/ghc-wasm-miso-examples` — working miso wasm example (config to mirror).
- `haskell-miso/miso-sampler` — official miso starter (Nix flow).
- GHC user's guide §15 — WASM backend (reactor module shape, post-link).
- Tweag blog 2024-11-21 — TH/ghci over wasm via Node iserv.

## 8. Phase 4 design proposal (draft sketches)

These are reference templates for when the `stable-haskell/ghc-wasm-meta` repo
is created. Refine against the real bindist once Phase 3 produces one.

### 8.1 `ghcup-stable-wasm-0.0.1.yaml` skeleton

```yaml
ghcupDownloads:
  GHC:
    wasm32-wasi-9.14.0.stable.YYYYMMDD:
      viTags: [base-4.21.0.0]
      viPreInstall: |
        Stable Haskell wasm32-wasi cross-compiler.

        Prerequisites — install the host-side wasm toolchain via upstream
        ghc-wasm-meta (we don't ship wasi-sdk / node / wasmtime / binaryen
        ourselves):

          curl -sL https://raw.githubusercontent.com/stable-haskell/ghc-wasm-meta/master/bootstrap.sh | sh
          source ~/.ghc-wasm/env

        Then re-run this install.
      viPostInstall: |
        # ghcup runs viPostInstall from the install prefix.
        # ${pkgroot}-relative *.conf files are portable, but the binary
        # ghc-pkg cache is not — regenerate it for the new location.
        ./bin/wasm32-wasi-ghc-pkg recache \
          --package-db ./lib/targets/wasm32-wasi/lib/package.conf.d
        echo "Stable Haskell wasm32-wasi-ghc ready at: $(pwd)/bin/wasm32-wasi-ghc"
      viArch:
        A_64:
          Linux_UnknownLinux:
            unknown_versioning:
              dlHash: <SHA256-from-Phase-3>
              dlSubdir: wasm32-wasi-ghc-9.14.0
              dlUri: https://github.com/stable-haskell/ghc-wasm-bindists/releases/download/v9.14.0.stable.YYYYMMDD/wasm32-wasi-ghc-9.14.0-x86_64-linux.tar.xz
              dlOutput: wasm32-wasi-ghc-9.14.0-x86_64-linux.tar.xz
        A_ARM64:
          Darwin:
            unknown_versioning:
              dlHash: <SHA256-from-Phase-3>
              dlSubdir: wasm32-wasi-ghc-9.14.0
              dlUri: https://github.com/stable-haskell/ghc-wasm-bindists/releases/download/v9.14.0.stable.YYYYMMDD/wasm32-wasi-ghc-9.14.0-aarch64-darwin.tar.xz
              dlOutput: wasm32-wasi-ghc-9.14.0-aarch64-darwin.tar.xz
  Cabal:
    stable-3.17.0.1:
      viTags: [Latest]
      viPreInstall: |
        Stable Haskell cabal-install with real dual-compiler support
        (with-build-compiler / with-compiler) for cross-compilation.
      viArch:
        A_64:
          Linux_UnknownLinux: { ... }
        A_ARM64:
          Darwin: { ... }
```

**Open questions to resolve before publishing:**
- Does ghcup invoke `viPostInstall` with `PWD = install prefix`, or some other dir? Confirm via ghcup source or empirically.
- Does ghcup support `Cabal:` entries in custom channels, or only `GHC:`? Test by registering a channel with just `Cabal:` and trying `ghcup list cabal`.
- Should the version suffix be `.stableYYYYMMDD` or `-stable-YYYYMMDD`? Match ghcup's preferred patterns.

### 8.2 `stable-haskell/ghc-wasm-meta/bootstrap.sh` sketch

```sh
#!/bin/sh
# Stable Haskell wasm bootstrap — installs everything needed to build wasm
# Haskell apps with stable-haskell/ghc + stable-haskell/cabal.
set -e

echo "==> Step 1/3: installing host-side wasm toolchain (wasi-sdk, node, wasmtime, binaryen)"
echo "    delegating to upstream haskell-wasm/ghc-wasm-meta with SKIP_GHC=1"
curl -sL https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh \
  | SKIP_GHC=1 SKIP_CABAL=1 sh

if [ -f "$HOME/.ghc-wasm/env" ]; then
  echo "==> Step 2/3: source ~/.ghc-wasm/env to get wasi-sdk / node / wasmtime on PATH"
  echo "    add this to your shell rc file:"
  echo "      source $HOME/.ghc-wasm/env"
fi

echo "==> Step 3/3: register stable-haskell ghcup channel"
ghcup config add-release-channel \
  https://raw.githubusercontent.com/stable-haskell/ghc-wasm-meta/master/ghcup-stable-wasm-0.0.1.yaml

echo
echo "==> Done. Next steps:"
echo "    source ~/.ghc-wasm/env"
echo "    ghcup install ghc wasm32-wasi-9.14.0.stable.YYYYMMDD"
echo "    ghcup install cabal stable-3.17.0.1"
echo "    git clone https://github.com/stable-haskell/miso-wasm-template"
echo "    cd miso-wasm-template && make && make serve"
```

### 8.3 Repo layout for `stable-haskell/ghc-wasm-meta`

```
stable-haskell/ghc-wasm-meta/
├── README.md                            (Phase 7 doc)
├── bootstrap.sh                          (8.2 above)
├── ghcup-stable-wasm-0.0.1.yaml          (8.1 above)
└── .github/workflows/
    └── validate.yml                      (lint YAML, hash-check release assets)
```

Bindists themselves live in a SECOND repo:

```
stable-haskell/ghc-wasm-bindists/
└── (releases tagged v9.14.0.stable.YYYYMMDD,
     each with .tar.xz assets per host platform)
```

This separation mirrors upstream (`haskell-wasm/ghc-wasm-meta` +
`haskell-wasm/ghc-wasm-bindists`) and keeps the small text-config repo
auditable while binaries live in releases.

## 9. Status log

- **2026-05-26** — Initiative kicked off. Five research agents fanned out; findings
  aggregated above. Branch `feat/wasm-cross-ghcup` created. This plan committed.
- **2026-05-26** — Phase 0a complete. Phase 0 cherry-picks landed cleanly:
  6 commits from `build/wasm-nix-environment` (flake.nix uses ghc-wasm-meta for
  wasi-sdk; wrappers in `.nix-wasm-bin/`) + 6 commits from `feat/nix-ci-split`
  (Makefile cross-refactor + CI nix-ci.yml split + RTS R_*_NONE fix + 3 build
  infra fixes). Single cabal.project.stage3 auto-merge, no manual conflicts.
  Branch now 13 commits ahead of `stable-ghc-9.14`. Verifying Phase 0 gate next.
- **2026-05-26** — Phase 0 gate PASSES. `nix develop` reports: bootstrap GHC
  9.8.4, cabal-install 3.16.0.0, LLVM 21.1.8-wasi-sdk, clang 21.1.8-wasi-sdk.
  All wasm wrappers on PATH (`.nix-wasm-bin/wasm32-wasi-{clang,clang++,ar,
  ranlib}`, `wasm-ld`, `llc`, `opt`, `llvm-as`). `WASI_SDK_DIR` +
  `WASM_EXTRA_{LIB,INCLUDE}_DIRS` exported. Incidental finding: devshell ships
  cabal 3.16 — this is R1 materializing (upstream ghc-wasm-meta README warns
  3.16 has a wasm regression). Doesn't affect Phase 0/1 (we're building GHC,
  not user apps with TH), but flag for Phase 5 cabal-binary work and Phase 6
  miso e2e. Cherry-picked artifact: cabal-install was Nix-built from source
  (~15 min first run); subsequent shells will be cached.
- **2026-05-26** — Phase 1 starting: apply D2 rename
  `wasm32-unknown-wasi` → `wasm32-wasi` across Makefile, flake.nix, nix-ci.yml,
  USAGE.md, build-wasm-{make,on-linux0}.sh (8 files, ~30 occurrences).
  Mechanical sed; no word-boundary risk (no `wasm32-unknown-wasi-foo` strings
  that should be preserved).
