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
- **R10: stable-haskell/cabal store layout doesn't match Makefile expectations.**
  Discovered 2026-05-26 after R9 workaround unblocked the build past unix-2.8.8.0.
  Cabal compile-less branch builds executables to `_build/stage1/bin/<name>` and
  libraries to `_build/stage1/store/ghc-9.8.4-inplace/<pkg>-<hash>/`. The Makefile
  (lines 610, 617, and many others) expects `$(STORE_DIR)/host/$(HOST_PLATFORM)/bin/`
  = `_build/stage1/store/host/aarch64-apple-darwin/bin/` — a path that simply
  doesn't exist with this cabal. Stage1 build fails at the `ghc-toolchain-bin`
  invocation step with `bash: ...: No such file or directory`. **Both at HEAD
  AND at SHA `44817477…`** (we tested the pin separately) — meaning the layout
  divergence is somewhere in the compile-less branch's foundational store
  rework, not a recent regression.
  **This blocks Phase 1 entirely.** Possible resolutions: (a) update Makefile
  to look in `_build/stage1/bin/` and `store/ghc-9.8.4-inplace/` (probably
  20-40 line changes — significant, needs cross-platform care for the GHC
  toolchain triple); (b) find an even older stable-haskell/Cabal SHA that
  produced the `store/host/HOST/` layout the Makefile expects; (c) ask the
  user if there's a known-working combination they've used recently
  (the previous successful build of `_build/stage1` we saw earlier must
  have used a compatible cabal — what SHA was that?).
  **ESCALATING TO USER** — this is bigger than the wasm-cross-ghcup
  initiative scope. Resolution choice affects the whole project's
  buildability, not just our branch.
- **R9: stable-haskell/cabal HEAD races on store install locks across
  stages.** Discovered 2026-05-26 during Phase 1 stage2 retry: the
  compile-less Stage/Toolchain split means cabal builds the same package
  (`unix`, `happy-lib`, `os-string`, `filepath`, …) for both Build and Host
  stages. When the install steps race, one process holds the per-package
  lock in `store/.../incoming/*.lock` while another tries to acquire it
  → `openFile: resource busy`. happy-lib was observed with 12 attempts.
  Andrea's earlier "TOCTOU race fix for BuildInplaceOnly tarball extraction"
  doesn't cover this. **Workarounds in priority order:** (i) retry the
  build (sometimes transient if the artifact lands on the first attempt);
  (ii) limit cabal parallelism via `--jobs=1` or `cabal-install` config;
  (iii) pin `tag:` in `cabal.project.stage{0,1,2,3}` back to
  `44817477ff6d…` (pre-rebase, last known-working). **Fix upstream** in
  `stable-haskell/cabal#stable-haskell/master` — the store install needs
  to recognize that two stages building the same package for the same arch
  produce identical artifacts and short-circuit. **Same fix-priority class
  as R8** (both block Phase 5).
- **R8: stable-haskell/cabal HEAD doesn't bootstrap with stock GHC 9.8.4.**
  Discovered 2026-05-26 during Phase 1 stage2 build attempt. `make stable-cabal`
  fails: `cabal-install/src/Distribution/Client/ProjectPlanning.hs:224` imports
  `Distribution.Simple.GHCJS` which is hidden in `Cabal-3.10.3.0` (the bundled
  Cabal library in bootstrap GHC 9.8.4). Either: (a) the GHCJS removal in the
  compile-less branch is incomplete (still references the removed module), or
  (b) the cabal-install build-depends accidentally allows linking against
  the bundled Cabal instead of the source-repository-package version.
  **Blocks Phase 5** (cabal binary shipping). Workaround for Phase 1: set
  `USE_SYSTEM_CABAL=1` to skip the stable-cabal target — uses the existing
  `_build/stage0/bin/cabal` (cabal-install 3.17.0.0, one snapshot older from
  pre-rebase tag). This is acceptable for Phase 1/2 (we're building GHC, not
  shipping cabal); for Phase 5 we must either fix the import or pin Cabal
  source-repo properly. File the fix upstream against
  `stable-haskell/cabal#stable-haskell/master`.
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
  that should be preserved). D2 applied in commit `03bd8b8c0d6`.
  Initial bad commit with embedded git repos caught and fixed via `git reset
  --mixed HEAD~1`; recommitted with explicit file paths.
- **2026-05-26** — Phase 1 stage2 build FAILED after ~3 min: `make stable-cabal`
  (rebuild cabal-install-3.17.0.1 from stable-haskell/master) fails — see R8.
  Retrying with `USE_SYSTEM_CABAL=1` to skip the rebuild and use the existing
  pre-rebase `_build/stage0/bin/cabal` (3.17.0.0).
- **2026-05-26** — Cascade of issues discovered: USE_SYSTEM_CABAL=1 hit R9
  (parallel install race on unix-2.8.8.0). Pinning Cabal source-repo tag to
  `44817477…` did NOT resolve R9 (it's in the cabal binary, not the library).
  `jobs: 1` in cabal.project.common DID resolve R9 and progressed past
  unix-2.8.8.0 through Cabal-3.17.0.0, ghc-toolchain-bin, ghc-boot, ghci,
  ghc-pkg, ghc, ghc-bin. THEN hit R10: cabal-install 3.17.0.0 (pre-Stage/
  Toolchain) puts binaries in `_build/stage1/bin/` and libraries in
  `_build/stage1/store/ghc-9.8.4-inplace/<pkg>` (old upstream layout), but
  the Makefile expects `_build/stage1/store/host/<HOST>/bin/…` (new layout
  from compile-less). Path-layout mismatch.
- **2026-05-26** — Path 2 (user-chosen): patched `stable-haskell/cabal` HEAD
  locally to remove dead GHCJS code (R8 fix) + a stale `binDirectoryFor`
  call site (uncovered after R8). Patches saved in
  `lode/r8-cabal-ghcjs-removal.patch` (2 commits). Built patched cabal-install
  in /tmp/cabal-fix with bootstrap GHC 9.8.4 + bootstrap cabal — SUCCESS.
  Binary copied to `_build/stage0-patched/bin/cabal` (version banner confirms
  patch SHAs: cabal-install 3.17.0.1 commit `3386061`, Cabal lib commit
  `d6fac5f`).
- **2026-05-26** — Cascade through R11/R12: with patched cabal + relaxed
  constraints + jobs:1, stage1 built but stage2 failed on
  `build:any.ghc-internal installed`. Tried devx ghc910 bootstrap (has
  ghc-internal-9.1003.0 installed) — different failure (mystery base-4.18.3.0
  reference in unbuildable ghc-bignum lib). Escalated to user.
- **2026-05-26** — User suggested looking at GitHub CI for working recipe.
  **HUGE finding**: CI's last success was 2026-04-23 at SHA `d8f0caefe58`.
  ALL builds since 2026-05-17 (after chore commit `e148c1c059` "use
  stable-haskell/master") have failed — same R8 we hit. The project's
  baseline has been broken for 10 days.
- **2026-05-26** — Cross-referenced cabal source at SHA `44817477` (the CI
  April-23 working pin): **no `Distribution.Simple.GHCJS` module, no dead
  GHCJS import, no `binDirectoryFor` stale call**. The R8/R8.5 bugs were
  introduced LATER on stable-haskell/master, not present at 44817477.
- **2026-05-26** — CI uses upstream `cabal-3.14.2.0` from ghcup as bootstrap,
  NOT a stable-haskell cabal-install. Downloaded 3.14.2.0 — but it fails
  with `fatal: Could not parse object 44817477ff6d…` because the SHA is
  orphaned (`wip/angerman/compile-less` branch was deleted/renamed to
  `stable-haskell/master`, leaving the SHA reachable only via direct fetch).
  CI works because it has a CACHED cabal binary that pre-dates the cache-key-
  invalidating chore commit.
- **2026-05-26** — **WORKING RECIPE FOUND**: substitute the patched cabal
  (which CAN fetch orphaned SHAs) for upstream 3.14.2.0 as `CABAL0`. The
  patched cabal builds stable-cabal from clean SHA `44817477` source, which
  produces a fresh cabal-install with no R8/R8.5/R9/R10/R11 issues.
  Command: `devx#ghc98-minimal-ghc shell` + `CABAL0=patched-cabal` +
  `cabal update` + `make clean clean-cabal distclean` + `make _build/dist/ghc.tar.gz`.
  Result: `_build/dist/ghc.tar.gz` (258MB) built in ~45 min.
  GHC 9.14 (Stable Haskell Edition) compiles+runs hello world. Stage1+2 GREEN.
- **2026-05-26** — **PHASE 1 STAGE3 GREEN**: switched to local flake (wasi-sdk),
  ran `make CABAL=_build/cabal/bin/cabal USE_SYSTEM_CABAL=1 stage3-wasm32-wasi`.
  Failed on final library-copy step due to **D2 triple mismatch**: ghc-toolchain
  normalizes `wasm32-wasi` back to `wasm32-unknown-wasi` (canonical autoconf
  form), so cabal stored libs at `host/wasm32-unknown-wasi/...` but Makefile
  looked at `host/wasm32-wasi/...`. **REVERTED D2** across Makefile, flake.nix,
  nix-ci.yml, USAGE.md, 2 build scripts. Rebuild stage3 → success.
  `_build/dist/bin/wasm32-unknown-wasi-ghc` exists; compiles `hello.hs` to
  valid `WebAssembly (wasm) binary module version 0x1 (MVP)` (1.5MB).
  Pure wasmtime can't run the binary (GHC's wasm RTS uses JSFFI imports);
  full runtime test via post-link.mjs + Node deferred to Phase 6 miso e2e.
  **PHASE 1 GATE MET (compile-only gate; runtime gate in Phase 6).**
- **2026-05-26** — **PHASE 6 v0.0.1 DELIVERED** (trivial reactor PoC).
  Empirically verified end-to-end: dual-compiler `cabal.project` form
  (`with-build-compiler` + `with-compiler`) builds a trivial wasm reactor;
  `post-link.mjs` produces JSFFI ESM glue; Node's `node:wasi` + the JSFFI
  imports run the wasm and print "Hello from the WASM reactor!". Critical
  finding: the bring-up sequence MUST be **wasi.initialize() →
  __ghc_wasm_jsffi_init() → hs_start()**. The original agent drafts (and
  CI smoke test) missed `__ghc_wasm_jsffi_init` — without it, hs_start()
  throws "RTS is not initialised; call hs_init() first". Files in
  `lode/phase6-trivial-reactor-poc/`.
- **2026-05-27** — **PHASE 6.5 shared-library blocker RESOLVED** via
  extending stable-haskell's `--enable-dynamic` rearchitecture to
  stage3 cross targets. Five concrete changes (3 commits to PR #181):
    * `cabal.project.stage3.settings.in` + `configure.ac` + `Makefile`
      (CONFIGURED_FILES + STAGE3_*_PREREQS + DYNAMIC RTS-ways sed +
      DIST_COPY_LIBS_SO_CROSS .so/.dylib fix) + `cabal.project.stage3`
      — symmetric stage2-mirror of the dynamic-enable wiring.
    * `rts/RtsStartup.c` — missing wasm exclusion at the
      `promoteBootLibrariesToGlobal()` call site (definition at line 108
      excludes wasm; call at line 427 didn't, breaking the dyn rts build).
    * `compiler/GHC/Driver/Session.hs` — disabled the wasm
      `makeDynFlagsConsistent` rule. The rule's `LinkExecutable _`
      predicate matched the DEFAULT `defaultDynFlags.ghcLink`, so it
      fired for per-module compile invocations too — stripping WayDyn
      mid-compile and breaking `.dyn_hi` lookup.
  Result: stage3-wasm32-unknown-wasi DYNAMIC=1 produces 2121 .dyn_hi
  + 40 .so files. End-user `cabal build` with `shared: True` compiles
  miso through 67/70 modules.
- **2026-05-27** — **iserv hang RESOLVED.** Instrumented dyld.mjs main()
  + minimal TH-test program → found the root cause: ghc-internal.so's
  dylink.0 needed_dynlibs lists `libHSrts-fs-1.0.0.0`, `libdl.so`,
  `libc.so` but NOT the main wasm RTS (e.g.
  `libHSrts-1.0.3-nonthreaded-nodebug-ghc9.14.so`). ghc-internal
  IMPORTs `env.registerForeignExports` (defined only in main rts).
  Toposort loads ghc-internal *before* main rts → ghc-internal's
  `_initialize` calls env.registerForeignExports → "non-existent
  function" RuntimeError → node crashes. Parent ghc never noticed
  the node death and hung on the IPC pipe.
  Workaround in `utils/jsffi/dyld.mjs`: preload any
  `libHSrts-*-nonthreaded-nodebug-ghc*.so` sitting next to ghciSoPath
  before loadDLL(ghciSoPath). Commit `1cf5c768394` on PR #181.
  Verified end-to-end with a minimal TH program: builds + installs
  the wasm executable cleanly.
  **Proper fix is at link level** — ghc-internal's wasm-ld invocation
  should emit a needed_dynlibs entry for the main rts. Tracking as
  follow-up; the dyld.mjs workaround unblocks TH for end-user wasm
  builds in the meantime.
- **2026-05-27** — **Deeper root cause for the ghc-internal/main-rts
  link bug**: `cabal.project.stage3` declares
    `package ghc-internal / ghc-options: -no-rts`
  Setting `-no-rts` (`Opt_NoRts` in GHC) is documented as the right
  thing for static `.a` libs that would otherwise fail the
  `mkUnitState` sanity check at `compiler/GHC/Unit/State.hs:1652`
  ("RTS is missing from the package database"). However, when shared
  is enabled, the *same flag* also prevents `-lHSrts-...` from being
  added to the wasm-ld command for the resulting `.so`. The `.so`
  ends up without rts in its `dylink.0/needed_dynlibs`.
  GHC already has injection logic at `Unit/State.hs:1700-1713` that
  makes `rtsWayUnitId` a synthetic dep of `ghcInternalUnitId`, but
  that's at unit-state level — by the time `linkDynLib`
  (`compiler/GHC/Linker/Dynamic.hs:55`) calls `preloadUnitsInfo'`
  with cabal's explicit `--depends` list, the injection has no effect
  because cabal didn't list rts in ghc-internal's depends.
  **Cleanest fix** (deferred): on wasm32, when building a `.so` for
  ghc-internal (or any pkg flagged `-no-rts`), explicitly append the
  current `rtsWayUnitId` to `pkgs_with_rts` in `linkDynLib`. The
  Wasm32 branch at `Dynamic.hs:92` already short-circuits to
  `pkgs_with_rts` instead of `pkgs_without_rts`, but it needs to also
  guarantee rts is *in* `pkgs_with_rts` regardless of caller intent.
- **2026-05-27** — **Second bug surfaced during miso retry**:
  stable-haskell/cabal resolves the `wasm32-unknown-wasi-ghc-pkg`
  symlink to its target `ghc-pkg` before storing the path in
  `setup-config`. GHC's ghc-pkg uses `argv[0]` to choose the target
  package db, so after resolution the *native* db ends up in cabal's
  db stack. Verified via `cabal -v3` output:
    `db stack: [..., _build/dist/lib/package.conf.d, ...]`
  (native), not `lib/targets/wasm32-unknown-wasi/lib/package.conf.d`.
  The wasm GHC itself still consults its own default db at compile
  time, so the build proceeds; but cabal's view of installed packages
  is wrong, which is a latent footgun (ABI hashes etc.). Tracking as
  a stable-haskell/cabal followup.
- **2026-05-27** — **PHASE 6.5 GATE PASSED. FULL MISO APP BUILDS END-TO-END.**
  Re-ran the miso build at `/tmp/wasm-reactor-test/` with the dyld.mjs
  preload workaround in place. All 50+ dependencies — including the
  heavy TH packages (aeson, lens, jsaddle, jsaddle-wasm) — built
  cleanly via wasm-iserv. Final `myapp.wasm` is 2.8MB, valid
  WebAssembly MVP binary. The dyld.mjs preload workaround scales.
  Two end-user gotchas surfaced and are documented:
   * miso 1.11 apps need `build-depends: ghc-experimental` to import
     `GHC.Wasm.Prim` (otherwise GHC-87110 "module hidden in
     ghc-experimental").
   * For miso, the wasm entry is `startApp defaultEvents app` *not*
     `JSW.run (startApp …)` — miso's `startApp` already returns
     `IO ()`; wrapping it in `JSW.run` triggers `JSM ()` / `IO ()`
     mismatch.
- **2026-05-27** — **PROPER LINK-LEVEL FIX LANDED. dyld.mjs workaround
  REVERTED.** Implemented the right fix in
  `compiler/GHC/Linker/Dynamic.hs` (commit `f92963f43ef`): on wasm32,
  `linkDynLib` now forces the current way's `rtsWayUnitId` into the
  pkg list when it isn't already present (looked up via `lookupUnitId`
  on `ue_units unit_env`). The fix is wasm32-only, purely additive
  (no-op for .so files that already include rts), and lets `wasm-ld`
  populate `dylink.0/needed_dynlibs` with rts even when the package
  was built with `-no-rts`.
  Verified at two levels:
   * Artifact: `libHSghc-internal-9.1400.0-ghc9.14.so` needed_dynlibs
     went from `[rts-fs, libdl, libc]` (3 entries) → `[rts, rts-fs,
     libffi, libdl, libc]` (5 entries, with the main rts now present).
   * Runtime: after reverting the dyld.mjs main-rts preload workaround
     (`d32bc88ff6a`), the full miso build *still* succeeds — aeson
     (TH-heavy) re-built cleanly via iserv, `myapp.wasm` produced.
     The link fix is sufficient on its own.
  Bindist tarball repackaged with the clean state: 226MB,
  ghc-internal.so has rts in deps, dyld.mjs no longer has the
  workaround.
- **2026-05-27** — **End-user bindist verification (Phase 3 → Phase 4
  handoff).** Extracted the 226MB tarball into a fresh prefix
  (`/tmp/wasm-bindist-test/`), ran `./relocate.sh`, then compiled two
  hello-world programs as an end user would:
   * Non-TH `hello2.hs` → `hello2.wasm` (1.7MB, valid wasm MVP) — works
     standalone (no node needed at compile time).
   * TH-using `hello.hs` (`$(lift "...")`) → `hello.wasm` (1.7MB) —
     works **with `node` on PATH** (the iserv shim shells out to node
     to run the wasm interpreter at compile time).
  Original bindist SHA256:
    `005ebbb7e9c5dfa5bf183a263ab398022409004030b817fd7a12781f1db7ef80`
  Phase 4 prerequisites:
   * Bindist file: `_build/dist/ghc-wasm32-unknown-wasi.tar.gz`
     (226742697 bytes).
   * Documented user prerequisite: **`node` must be on PATH** if the
     user wants TH evaluation (cabal pkgs with `template-haskell`).
   * Symlink-resolution cabal bug (Task #13) downgraded: only affects
     in-tree `_build/dist/` developer use (symlinks present). End
     users via ghcup get the `tar czhf`-dereferenced real files in
     `bin/`, so they don't trip it. Not gating on Phase 4.
- **2026-05-27** — **UX fix: clear error when `node` missing for TH**
  (commit `6171b4256b1`). The bindist's TH iserv path goes through
  `dyld.mjs` which starts with `#!/usr/bin/env -S node`. Without node
  the iserv child exits with 127 and GHC reports the generic
  `External interpreter terminated (127)` from `Process.hs:129` —
  cryptic for someone tracking down a CI failure.
  Added a pre-flight `findExecutable "node"` in
  `compiler/GHC/Runtime/Interpreter/Wasm.hs::spawnWasmInterp` that
  throws `InstallationError` with a clear, actionable message naming
  the dyld.mjs script and instructing the user to install Node.js.
  Verified: clearing PATH of node and recompiling a TH-using hello-
  world now produces the new message; happy path (node on PATH)
  unchanged.
  Repackaged bindist after this commit. New SHA256:
    `d10b2ee8c807f47de94870604a68b6ff73835fe75981a456b962b80d77be6789`
  (size 226745567 bytes).
- **2026-05-27** — **`relocate.sh` also warns at install time if Node.js
  missing** (commit `d2dda02f14b`). Layers the same UX hint as the
  compile-time check, but at install/relocate time so end users learn
  the requirement before their first TH compile. Non-TH builds remain
  unaffected — the script still completes successfully when node is
  missing, it just prints a NOTE to stderr.
  Final repackaged bindist SHA256:
    `8241c107930fadd77fe28773f47581e7288cbc405143a7acf5c0bec4df181d2e`
  (size 226747741 bytes). This is the artifact ready for Phase 4
  hosting.
- **2026-05-27** — **PHASE 4 GATE PASSED. ghcup channel published.**
  Decisions (user-approved):
   * Host: GitHub release attachment on stable-haskell/ghc
   * Version label: `wasm32-wasi-9.14.0.stable.0`
   * Channel: custom (GitHub Pages on stable-haskell/ghc gh-pages branch)
  Implementation:
   * Created GitHub release `wasm32-wasi-9.14.0.stable.0` (pre-release
     flag set) with the wasm bindist attached. URL:
     https://github.com/stable-haskell/ghc/releases/tag/wasm32-wasi-9.14.0.stable.0
   * Added autoconf-shaped install stubs to the bindist
     (mk/wasm-configure.sh + mk/wasm-bindist-Makefile, wired through
     the main Makefile, commit `7c26a1def7e`) so `ghcup install ghc
     <ver>`'s standard `./configure --prefix && make install
     DESTDIR=staging` flow works. The install target honors `$(DESTDIR)`,
     copies bin/+lib/+relocate.sh, and prints a node-missing NOTE.
   * Tarball SHA changed to
     `93e5d8c70fb670148015ca6cbcb76fbe1d9fe9d7b7945de0b983799e32efb60e`
     after adding the autoconf stubs.
   * Created orphan `gh-pages` branch with `ghcup-wasm.yaml` +
     `index.html`. Enabled GitHub Pages on that branch.
     YAML URL: https://stable-haskell.github.io/ghc/ghcup-wasm.yaml
     Landing page: https://stable-haskell.github.io/ghc/
   * Crucial schema finding: ghcup's custom-channel parser expects the
     0.0.9 flat schema (versions directly under tool name) — the
     0.1.0 schema (`toolDetails:`/`toolVersions:` indirection) only
     works for the upstream-cached default channel, NOT for
     `ghcup config add-release-channel <url>`.
  End-to-end verified:
   * `ghcup config add-release-channel https://stable-haskell.github.io/ghc/ghcup-wasm.yaml`
     → succeeds, channel cached.
   * `ghcup list -t ghc` shows `wasm32-wasi-9.14.0.stable.0
     latest-prerelease`.
   * `ghcup install ghc wasm32-wasi-9.14.0.stable.0` → downloads
     bindist (~216MB), verifies SHA256, runs configure + make install,
     prints the viPostInstall message including the node hint.
   * Installed compiler at `~/.ghcup/ghc/wasm32-wasi-9.14.0.stable.0/`
     builds both non-TH and TH hello-worlds.
  Phase 4 gate is met for the aarch64-darwin host. Linux hosts join
  once PR #181 CI verifies their build.
- **2026-05-27** — **PHASE 5 GATE PASSED. Dual-compiler `cabal build`
  works end-to-end via ghcup.** Created `cabal-3.17.0.0.stable.0`
  release (pre-release) on stable-haskell/ghc with the bundled
  cabal-install binary (cabal-install 3.17.0.1 from stable-haskell/cabal
  HEAD + R8 patches). Bundled `lib/libgmp.10.dylib` next to the
  binary with `@loader_path/lib`-relative rpath so it runs without
  nix/Homebrew on stock aarch64-darwin. Uses ghcup metadata's
  `dlInstallSpec.exeRules` + `dataRules: lib/**` to install both files.
  Add to the same channel YAML.
  **One critical cabal source patch was needed** to make the dual-
  compiler `cabal build` flow work on the ghcup-installed bindist:
   * cabal's `guessGhcPkgFromGhcPath` (`Cabal/src/Distribution/Simple/
     GHC.hs::guessToolFromGhcPath`) looks for `ghc-pkg` next to the
     ghc binary, then falls back to PATH. For a cross-compiler bindist
     where `bin/` only contains the target-prefixed names (e.g.
     `wasm32-unknown-wasi-ghc-pkg`, no bare `ghc-pkg`), the fallback
     grabs the BUILD compiler's `ghc-pkg` from PATH, triggering
     "Version mismatch between ghc and ghc-pkg" at configure time.
   * Patch: detect the target prefix on the ghc binary
     (`takeBaseName p` strips dir/ext, `isSuffixOf` the
     `"ghc"++versionSuffix` tail, take everything before that) and
     prepend it to the toolname when guessing. So
     `/path/wasm32-unknown-wasi-ghc` guesses
     `/path/wasm32-unknown-wasi-ghc-pkg` first, which exists in the
     bindist. Falls back to existing logic if no prefix is detected.
   * Patch saved at `lode/r12-cabal-target-prefix-aware-tool-guess.patch`
     (71 lines) for upstreaming to stable-haskell/cabal#359.
   * The patched cabal binary in the released tarball already has
     this fix baked in.
  Final cabal bindist SHA256:
    `7f755b0810f5167b7f776470b5219834cdcbdad562998fd3c074b091be77e4d6`
  Verified end-to-end:
   * Fresh sandbox: `ghcup config add-release-channel` + install ghc
     + install cabal both succeed.
   * `cabal build test-app` on a dual-compiler `cabal.project`
     (with-build-compiler = ghc-9.8.4, with-compiler = wasm GHC,
     shared:True) builds a 1.7MB valid WebAssembly MVP binary via
     `cabal-3.17.0.0.stable.0`.
  Original Task #13 (cabal symlink resolution) is RESOLVED by the
  same patch — the target-prefix issue was the ghcup end-user
  manifestation of the same underlying gap in
  `guessGhcPkgFromGhcPath`.
- **2026-05-27** — **PHASE 7 GATE PASSED.** Tutorial-grade landing
  page + downloadable hello template shipped on the `gh-pages` branch:
   * `index.html` — install instructions, 90-second hello walkthrough,
     anatomy section (cabal.project, myapp.cabal, Main.hs, run.mjs
     reactor sequence), troubleshooting table, project links.
     Light/dark mode CSS.
   * `examples/hello/` — minimal but production-shaped reactor
     template:
       - `app/Main.hs` (CPP-guarded `foreign export javascript`)
       - `cabal.project` (dual-compiler bare names + wasm32 `shared:True`)
       - `myapp.cabal` (reactor `ghc-options` + `cpp-options: -DWASM`)
       - `Makefile` (self-documenting, autodetects
         `$GHCUP_INSTALL_BASE_PREFIX`, honors macOS `/tmp` symlink
         canonicalization for `post-link.mjs`)
       - `run.mjs` (Node:wasi launcher with full reactor bring-up
         comments)
       - `public/{index.html,index.js}` (browser launcher via
         `browser_wasi_shim` ESM)
   * `examples/stable-haskell-wasm-hello.tar.gz` — 5 KB tarball,
     SHA256
     `1f422c5ee6056ec0f0c4d1055cbfda6513c98d4bcc305991dd04f067d3a68843`,
     extracts to `stable-haskell-wasm-hello/`.
  Verified end-to-end from a clean dir:
   * `curl -L https://stable-haskell.github.io/ghc/examples/stable-
     haskell-wasm-hello.tar.gz | tar xz`
   * `cd stable-haskell-wasm-hello && make run-node`
   * stdout: `Hello from the WASM reactor!`
  Phase 7 gate ("unfamiliar dev reaches demo from README alone") met.
- **2026-05-27** — **miso-counter template shipped alongside hello.**
  Phase 6.5 had verified the miso 1.11 build end-to-end on a private
  test dir at `/tmp/wasm-reactor-test/`; that working setup is now
  packaged as a shippable template on the channel:
   * `examples/miso-counter/` (browsable) and
     `examples/stable-haskell-wasm-miso-counter.tar.gz`
     (SHA `7b243af055ef012864e9e1567739411e1c854dad8a17b5442b1e58937fc0b77b`,
     5192 bytes).
   * Same Makefile shape as `hello/` but only `run-web` (miso is
     DOM-driven). `make build` first-run is ~5-10 min for the
     TH-heavy dep tree; subsequent code edits are seconds.
   * cabal.project pins miso 1.11 via `source-repository-package` and
     ships the `allow-newer: jsaddle-wasm:ghc-experimental` override
     needed for GHC 9.14.
   * Main.hs follows the miso 1.11 `component` smart-constructor +
     `startApp defaultEvents app` pattern (the API drift from older
     miso versions is called out in the README troubleshooting table).
  Landing page now features both examples side-by-side in a grid:
  hello as the 90-second on-ramp, miso-counter as the step-up to a
  real interactive UI. Acceptance-tested fresh from the published
  tarball: `curl | tar | make post-link-web` produces a 2.8MB valid
  WebAssembly MVP plus its 48KB post-link jsffi.mjs glue.
- **2026-05-26** — **PHASE 6 v0.0.2 (miso e2e) BLOCKED on wasm-backend
  shared-library support.** [HISTORICAL — superseded by 2026-05-27] Adding miso 1.11.0 + jsaddle-wasm pulls in
  `character-ps` which needs `Data.Word.dyn_hi` from base — but our wasm
  base library is built `shared: False`. Two attempts to fix in
  `cabal.project.stage3`:
    (1) project-level `if arch(wasm32) shared: True` — applied but failed
        at ghc-internal link with "[GHC-74335] -dynamic is ignored when
        linking binaries on WASM" + "mismatched interface file profile tag
        (wanted '', got 'dyn')".
    (2) package-level `package * / if arch(wasm32) shared: True` — built
        successfully but applied silently; no .dyn_hi anywhere.
  Conclusion: cabal config alone can't deliver wasm shared libs.
  Need upstream GHC work on the wasm backend (RTS / linker / interface-
  file profile tag handling). **Out of session scope.** Phase 6.5 (miso
  e2e) is queued as a separate workstream contingent on this fix.
- **2026-05-26** — Two PRs opened: (1) stable-haskell/cabal#359 with the
  R8/R8.5 patches (https://github.com/stable-haskell/cabal/pull/359); (2)
  stable-haskell/ghc#181 with our branch + Phase 0-2 work
  (https://github.com/stable-haskell/ghc/pull/181). Phase 2 CI verification
  is async on PR #181 — the `cross-wasm` job in nix-ci.yml already does
  the full smart-smoke-test runtime check.
- **2026-05-26** — **PHASE 3 GATE PASSED**. Added `mk/wasm-relocate.sh`
  (10-line script that runs `ghc-pkg recache` for the new install prefix)
  and switched the wasm bindist tar to `tar czhf` (dereferences symlinks
  → wasm-prefixed bin entries become real files instead of broken symlinks
  to the bare native names). Tarball grew 153MB → 264MB but is now
  **self-contained and relocatable**. Verified end-to-end: extract to
  `/tmp/wasm-relocate-test2`, run `./relocate.sh`, then
  `bin/wasm32-unknown-wasi-ghc hello.hs -o hello.wasm` produces valid
  wasm from the fresh prefix.
- **2026-05-26** — R8/R9/R10/R11 reclassified as **dissolved artifacts**
  of bootstrapping with broken master HEAD cabal source. Real root cause:
  the chore commit `e148c1c059` on `stable-ghc-9.14` changed the Cabal
  source-repo tag from explicit SHA `44817477…` to the branch name
  `stable-haskell/master`, exposing all subsequent builds to whatever
  bugs landed on that branch tip. **Recommended upstream fix**: revert
  `e148c1c059` (return to explicit SHA pin) OR fix the bugs on
  stable-haskell/master HEAD. Either resolves the daily CI failures.
