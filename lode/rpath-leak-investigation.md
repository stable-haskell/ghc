# `/Volumes/WorkSpace` LC_RPATH leak — root-cause investigation

**Date:** 2026-06-02
**Symptom:** every host arm64 Mach-O in `_build/dist/ghc-multi-target-aarch64-darwin.tar.gz` ships with an unresolvable absolute LC_RPATH:

    /Volumes/WorkSpace/_work/ghc/ghc/_build/stage2/store/host/aarch64-apple-darwin/lib

coexisting with the portable `@executable_path/../lib/aarch64-apple-darwin`. macOS 14 dyld silently falls through; macos-15 (Sequoia) dyld treats it as fatal and aborts the binary on launch.

Background workaround already in place: CI commit **010b365582c** post-processes the darwin bindist with `install_name_tool -delete_rpath` + ad-hoc re-sign. This document describes the upstream defect; the workaround stays in place until a real fix lands.

---

## Where the absolute path enters the link line

The leak originates in the bundled Cabal (not in GHC, not in our Makefile), specifically in two collaborating functions:

### 1. `depLibraryPaths` — returns absolute store paths

`libraries/Cabal/Cabal/src/Distribution/Simple/LocalBuildInfo.hs:256-351`

```haskell
depLibraryPaths
  :: Bool                       -- ^ Building for inplace?
  -> Bool                       -- ^ Generate prefix-relative library paths
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> IO [FilePath]
depLibraryPaths inplace relative lbi clbi = do
    ...
    let allDepLibDirs = concatMap getDynDir external_ipkgs
    allDepLibDirsC <- traverse canonicalizePathNoFail allDepLibDirs'
    let p = prefix installDirs
        prefixRelative l = isJust (stripPrefix p l)
        libPaths
          | relative && prefixRelative relDir =
              map (\l -> if prefixRelative l
                           then shortRelativePath relDir l
                           else l)
                  allDepLibDirsC
          | otherwise = allDepLibDirsC      -- absolute paths returned as-is
```

Two gate conditions are needed to shorten paths to relative:
* `relative == True` — driven by the per-package `relocatable :: Bool` field of `LocalBuildInfo`, which itself is set by `--enable-relocatable` (or `relocatable: True` in a project file). We do **not** pass this flag from `cabal.project.stage2.settings`.
* The dep's libdir `l` must already be under the package's prefix `p`. In a cabal-store layout each package gets its own subdir of the store, so a sibling dep's `lib/` directory is **never** under `p`. `prefixRelative l` is False for every cross-package dep, even with `--enable-relocatable`.

Both gates fail, so `depLibraryPaths` returns absolute store paths.

### 2. `relPath` — only rewrites relative paths

`libraries/Cabal/Cabal/src/Distribution/Simple/GHC/Build/Link.hs:638-648`

```haskell
if supportRPaths hostOS
  then do
    libraryPaths <- liftIO $ depLibraryPaths False (relocatable lbi) lbi clbi
    let hostPref = case hostOS of
          OSX -> "@loader_path"
          _   -> "$ORIGIN"
        relPath p = if isRelative p then hostPref </> p else p
        rpaths = toNubListR (map relPath libraryPaths)
              <> toNubListR (map getSymbolicPath $ extraLibDirs bi)
    return rpaths
  else return mempty
```

Absolute paths from step 1 pass through `relPath` unchanged. GHC then emits them as `-Wl,-rpath,...` on the link command and the linker writes them into LC_RPATH (darwin) or DT_RUNPATH (Linux ELF).

### 3. Build sequencing — Makefile rewrites `.conf` files too late

`Makefile` lines 738–763 (stage2 build):

1. `$(STAGE2_CABAL_BUILD)` runs cabal which links every executable, baking the absolute store paths from step 2 directly into the binary.
2. **After** all executables are linked, the Makefile rewrites the per-package `.conf` files with `${pkgroot}/../lib/...` placeholders so ghc-pkg can relocate them post-install.

By the time the `.conf` files become relocatable, the linked binaries already contain absolute LC_RPATH.

---

## Why Linux looks fine

It isn't — Linux ELFs have the identical defect. They get masked because the Makefile/CI runs `patchelf --force-rpath --set-rpath '$ORIGIN'` over every shipped binary and `.so`:

* `Makefile:777-779` — host shared libs
* `.github/workflows/nix-ci.yml` Cross: WASM / Cross: MULTI patchelf step — every executable + lib

`patchelf --set-rpath` writes a fresh DT_RUNPATH wholesale, so it doesn't matter what was baked in.

The darwin equivalent (install_name_tool can only `-add_rpath` / `-delete_rpath` / `-rpath` one at a time, no wholesale replace) was never wired up until this week.

---

## Fix options, ordered by invasiveness

| | Approach | Where | Invasiveness | Notes |
|---|---|---|---|---|
| A | Strip post-build with `install_name_tool -delete_rpath` + re-sign | `.github/workflows/nix-ci.yml` Cross: MULTI darwin step | Low (1 step) | **In place** as of commit 010b365582c. Mirrors what patchelf does on Linux. Bandaid but reliable. |
| B | Patch `relPath` in `Link.hs` to also handle absolute paths via `makeRelative bindir p` | `libraries/Cabal/.../Link.hs:644` | Medium | Needs `bindir` (or `dynlibdir`) of the executable's component at the call site — already computed via `absoluteComponentInstallDirs` in `depLibraryPaths`, would need plumbing. Source-side correctness fix. |
| C | Patch `depLibraryPaths` to detect cabal-store-sibling layout and emit relative paths between siblings | `libraries/Cabal/.../LocalBuildInfo.hs:336-346` | Medium-high | More general but every cabal user inherits the change. Likely needs upstream discussion. |
| D | Build stage2 with `relocatable: True` AND restructure store so siblings share a common prefix | `cabal.project.stage2.settings` | High | The prefix restructure is the hard part — cabal's store-by-unitid hash layout is what makes deps land in distinct prefixes. |

**Recommendation:** keep **A** (current state). Open an upstream Cabal issue describing B/C with a minimal reproducer (`cabal init`, `cabal install -j --enable-executable-dynamic --store-dir=/tmp/store some-pkg-with-deps`, `otool -l ~/.cabal/store/.../bin/exe | grep LC_RPATH`). Drop both the darwin install_name_tool step and the Linux patchelf rpath rewrite once Cabal's `relPath` is fixed upstream — they become no-ops, not opinionated.

## Files referenced

* `libraries/Cabal/Cabal/src/Distribution/Simple/GHC/Build/Link.hs:638-648`
* `libraries/Cabal/Cabal/src/Distribution/Simple/LocalBuildInfo.hs:256-351`
* `libraries/Cabal/Cabal/src/Distribution/Simple/Setup/Config.hs:212-213,370,827-830` (relocatable flag)
* `Makefile:738-779` (stage2 build + post-link patchelf for host .so)
* `.github/workflows/nix-ci.yml:1392-1450` (current darwin install_name_tool workaround)
* `.github/workflows/nix-ci.yml:1339-1389` (current Linux patchelf step in Cross: MULTI)
* `cabal.project.stage2.settings:5-7` (`package * { shared: True; executable-dynamic: True }` — what enables the rpath codepath)

## Reproducer

On any darwin host with this branch built:

```sh
$ otool -l _build/dist/stage2/bin/ghc | awk '/cmd LC_RPATH/{f=1;next} f && /path /{print; f=0}'
         path /Volumes/WorkSpace/_work/ghc/ghc/_build/stage2/store/host/aarch64-apple-darwin/lib (offset 12)
         path @executable_path/../lib/aarch64-apple-darwin (offset 12)
```

The first entry is the leak.
