# Multi-target GHC bindist — design

**Goal.** One ghcup channel entry, one downloaded tarball, one extracted directory; on extraction the user has working `bin/ghc` (native), `bin/wasm32-unknown-wasi-ghc`, and `bin/javascript-unknown-ghcjs-ghc` — all the same physical binary, dispatched via `argv[0]` to the appropriate per-target settings + library set.

**Working branch.** `feat/multi-target-bindist` off `feat/wasm-cross-ghcup` HEAD (`4bd31cb4316`). The .dyn_hi shipping work from Path C is the foundation: shared libraries + host dylibs + `$ORIGIN`-relative rpath are already in place from stable.12.

**Rollback.** gh-pages tag `demo-freeze-2026-06-05` points at the gh-pages commit advertising stable.12 as LatestPrerelease. One-command revert:

```
git push origin demo-freeze-2026-06-05:gh-pages -f
```

---

## 1. Why this works (the GHC architecture)

GHC's stage2 native binary is itself the cross compiler. It inspects `argv[0]` at startup, strips the executable's basename, looks for a leading triple prefix (`<triple>-`), and if found uses `lib/targets/<triple>/lib/settings` instead of `lib/settings`. Same binary, different target.

The stage3 build, parametric in `STAGE3_PLATFORMS`, already creates these per-platform invocation entrypoints — at line ~979 of the top-level Makefile each stage3 run does:

```make
$(foreach exe,$(STAGE3_EXECUTABLES),$(LN_SF) $$(exe) $(DIST_DIR)/bin/$(1)-$$(exe);)
```

i.e. for `$(1) = wasm32-unknown-wasi`, the rule creates `bin/wasm32-unknown-wasi-ghc → bin/ghc`, `bin/wasm32-unknown-wasi-ghc-pkg → bin/ghc-pkg`, etc. Same for JS.

The per-platform support files (`lib/targets/<triple>/`) get populated by the same `stage3-<triple>` rule.

The `STAGE2_EXECUTABLES` and `STAGE3_EXECUTABLES` lists are identical 9-element sets: `ghc`, `ghc-iserv`, `ghc-pkg`, `hp2ps`, `hpc`, `hsc2hs`, `runghc`, `unlit`, `haddock`. JS doesn't use `ghc-iserv` (the JS backend has its own evaluator); we filter it out for that target.

---

## 2. Bindist tar layout (target state)

```
bin/
  ghc, ghc-iserv, ghc-pkg, …                      # native (STAGE2_EXECUTABLES = 9)
  wasm32-unknown-wasi-ghc, …                      # wasm cross (9 entries)
  javascript-unknown-ghcjs-ghc, …                 # JS cross (8 entries — no ghc-iserv)
lib/
  ghc-usage.txt, ghci-usage.txt
  template-hsc.h
  settings                                        # NATIVE settings file (argv[0] dispatches to per-target settings if prefix found)
  package.conf.d/                                 # NATIVE package db (base, ghc-internal, etc.)
  <HOST_PLATFORM>/                                # NATIVE libs (libHS*.so, .a, .hi, .dyn_hi)
  targets/
    wasm32-unknown-wasi/lib/
      settings                                    # wasm-target settings
      package.conf.d/                             # wasm-target package db
      wasm32-unknown-wasi/                        # wasm-target libs (.so + .a + .hi + .dyn_hi)
      dyld.mjs, post-link.mjs, prelude.mjs, ghc-interp.js   # wasm runtime shims
    javascript-unknown-ghcjs/lib/
      settings                                    # JS-target settings
      package.conf.d/                             # JS-target package db
      javascript-unknown-ghcjs/                   # JS-target libs
      dyld.mjs, post-link.mjs, prelude.mjs, ghc-interp.js   # JS runtime shims (same family)
relocate.sh                                       # generalised — recaches ALL three package dbs
configure, Makefile                               # autoconf-shaped stubs (legacy install path)
```

Sizes (estimated): native lib/<HOST_PLATFORM>/ ~200 MB + wasm target ~230 MB + JS target ~250 MB + binaries ~30 MB. **Total ~700 MB** per platform tarball.

---

## 3. Makefile rule (Phase 2)

Modeled on the existing `$(DIST_DIR)/haskell-toolchain.tar.gz` (line 1149, which already does native + JS). We add wasm + use `tar -czhf` (dereference) so each cross-prefixed binary is a standalone copy (predictable for ghcup's symlink pattern matcher).

```make
$(DIST_DIR)/ghc-multi-target.tar.gz: $(STAGE2_STAMP) \
    | stage3-wasm32-unknown-wasi stage3-javascript-unknown-ghcjs
	@echo "::group::Creating ghc-multi-target.tar.gz..."
	@cp -f mk/multi-target-relocate.sh $(DIST_DIR)/relocate.sh
	@chmod +x $(DIST_DIR)/relocate.sh
	@cp -f mk/multi-target-configure.sh $(DIST_DIR)/configure
	@chmod +x $(DIST_DIR)/configure
	@cp -f mk/multi-target-bindist-Makefile $(DIST_DIR)/Makefile
	tar czhf $@ \
		--directory=$(DIST_DIR) \
		$(foreach exe,$(STAGE2_EXECUTABLES),bin/$(exe)$(EXE_EXT)) \
		$(foreach exe,$(STAGE3_EXECUTABLES),bin/wasm32-unknown-wasi-$(exe)$(EXE_EXT)) \
		$(foreach exe,$(filter-out ghc-iserv,$(STAGE3_EXECUTABLES)),bin/javascript-unknown-ghcjs-$(exe)$(EXE_EXT)) \
		lib/ghc-usage.txt lib/ghci-usage.txt lib/package.conf.d lib/settings lib/template-hsc.h \
		lib/$(HOST_PLATFORM) \
		lib/targets/wasm32-unknown-wasi \
		lib/targets/javascript-unknown-ghcjs \
		relocate.sh configure Makefile
	@echo "::endgroup::"
```

**`-h` rationale.** Stage3 creates `bin/<triple>-<exe>` as symlinks pointing at `bin/<exe>`. Wasm bindist uses `-czhf` (deref) because the wasm tarball doesn't include `bin/ghc` — we needed the cross-prefixed binaries to be standalone copies. The multi-target tarball INCLUDES `bin/ghc`, so we could in principle preserve the symlinks (smaller tarball). However ghcup's `targetPattern: "bin/**"` uses `getDirectoryFilesIgnore` which lists symlinks-as-files — so we'd need to be confident the symlinks are preserved end-to-end and pass through ghcup's unpack. Keeping `-h` (deref) is more predictable; cost is ~30 MB extra (8 ghc-iserv + 8 ghc-pkg + … copies vs symlinks of ~3 MB each). Acceptable.

---

## 4. relocate.sh (Phase 2 — supporting script)

Generalises the wasm-only version to recache all three package databases:

```sh
#!/bin/sh
set -e
PREFIX="$(cd "$(dirname "$0")" && pwd)"

# Native ghc-pkg (HOST_PLATFORM target via empty triple prefix)
"$PREFIX/bin/ghc-pkg" recache --package-db "$PREFIX/lib/package.conf.d"

# Per-cross-target ghc-pkg
for plat in wasm32-unknown-wasi javascript-unknown-ghcjs; do
  pkg_db="$PREFIX/lib/targets/$plat/lib/package.conf.d"
  if [ -d "$pkg_db" ]; then
    "$PREFIX/bin/$plat-ghc-pkg" recache --package-db "$pkg_db"
  fi
done

# wasm + JS both need node ≥ 22 on PATH. emscripten needed for JS link step.
if ! command -v node >/dev/null 2>&1; then
  echo "NOTE: node not on PATH — wasm/JS TH evaluation will fail." >&2
fi
if ! command -v emcc >/dev/null 2>&1; then
  echo "NOTE: emcc (emscripten) not on PATH — JS linking will fail." >&2
fi
```

---

## 5. CI Cross: MULTI job (Phase 5)

Copy/adapt `Cross: WASM` job in `nix-ci.yml`:
- needs: `[build]` (downloads `${plat}-dynamic1-dist` stage2 artifact — same as current wasm-cross)
- installs wasi-sdk AND emscripten (the JS path needs emcc on PATH at compile/link time)
- runs `make DYNAMIC=1 DIST_BUILD=1 _build/dist/ghc-multi-target.tar.gz` (DYNAMIC=1 because we need .dyn_hi for both wasm and JS targets, same logic as Cross: WASM)
- rename tarball with host-triple suffix
- patchelf step for Linux: same as Cross: WASM — set `$ORIGIN/../lib/$HOST_DIR` rpath on the binary, `$ORIGIN` on the .so files
- upload as workflow artifact
- on tag-push (matching `multi-*` tags): also upload to GitHub Release via `softprops/action-gh-release`

Matrix: same 3-platform set as Cross: WASM. aarch64-darwin is the slow self-hosted runner.

Expected build time: ~30 min for the multi-target tar after stage2 is ready (wasm + JS in parallel via cabal's build plan parallelism — possibly).

---

## 6. Channel YAML (Phase 7)

Schema bump from 0.0.9 → 0.1.0 (Installer DSL). New entry, drafted as a parallel entry to the existing stable.12 one (additive, not destructive):

```yaml
ghcupDownloads:
  GHC:
    multi-9.14.0.stable.0:
      viTags:
        - LatestPrerelease     # only set this AT promotion gate (Phase 11), not before
      viChangeLog: https://github.com/stable-haskell/ghc/releases/tag/multi-9.14.0.stable.0
      viPreInstall: |
        Requires Node.js ≥ 22 (wasm runtime), wasi-sdk (wasm-target C tools),
        and emscripten (JS-target C tools) on PATH. See
        https://stable-haskell.github.io/ghc/install/ for setup.
      viPostInstall: |
        Multi-target GHC installed. The same compiler invokes via argv[0]:
          ghc          — native compilation (this host)
          wasm32-unknown-wasi-ghc — wasm cross
          javascript-unknown-ghcjs-ghc — JS cross
        All three share the same package db conventions but each maintains
        its own per-target lib tree under lib/targets/<triple>/.
      viArch:
        A_64:
          Linux_UnknownLinux:
            unknown_versioning:
              dlUri: https://github.com/stable-haskell/ghc/releases/download/multi-9.14.0.stable.0/ghc-multi-target-x86_64-linux.tar.gz
              dlHash: ...
              dlInstallSpec:
                bindistFiles:
                  exeRules:
                    - installSource: configure
                    - installSource: Makefile
                    - installSource: relocate.sh
                  exeSymLinked:
                    - targetPattern: "bin/**"
                      targetPatternIgnore: []
                      linkName: "${TARGETFN}-${PKGVER}"
                      setName: "${TARGETFN}"
                  dataRules:
                    - installPattern: ["lib/**"]
                  preserveMtimes: false
        # ... same shape for A_ARM64 / Darwin + Linux_UnknownLinux
    wasm32-wasi-9.14.0.stable.12:
      viTags: []                # demoted ONLY at Phase 11
      # ... existing entry preserved verbatim
```

**Schema 0.1.0 vs 0.0.9.** ghcup-0.0.9 clients can't parse the new DSL. The channel must declare schema version somewhere; if not, old clients silently ignore the new fields (defaulting to legacy install). **Need to verify**: does ghcup-0.2.5.0 parse 0.0.9-schema YAML too? If yes, all good. If not, we'd need separate channel URLs per schema (a 0.0.9 channel and a 0.1.0 channel).

This is the **highest risk** part — Phase 7 GATE explicitly tests schema parsing.

---

## 7. Sequencing + gates (already in #38..#50)

The phases are queued. Each ends with a hard gate. NO-GO at any gate means we stop, document, and the Friday demo runs on stable.12 — no functional loss.

---

## 8. Open questions

- **JS bindist needs emscripten in `viPreInstall`** — but emscripten is heavy and version-pinned. Should we make JS optional (separate channel entry) and ship native+wasm in the "multi" bindist? This would simplify the install story.
- **Schema migration**: if we go 0.1.0 schema, every stable-haskell channel user needs ghcup ≥ 0.2.5. The bootstrap.haskell.org installer ships latest, so new installs are fine. Existing users with older ghcup would silently lose access. Acceptable for a pre-release channel.
- **Naming**: `multi-9.14.0.stable.0` makes the namespace clean. Alternative: just `9.14.0.stable.0` (no triple prefix, treated as a native entry by ghcup, with the cross targets as bonus symlinks). The latter slots into the canonical ghcup ghc track. **Decision: go with `multi-` prefix initially**; namespace it cleanly so we don't conflict with anyone else's `9.14.0.stable.X`.
