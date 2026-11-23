# Unit artifact spike: a unit store located by convention

Status: research spike, October 2026. Companion to
`unit-artifact-field-analysis.md`, which is the evidence for the
classification used here. A store is only read when `-unit-store ⟨dir⟩`
is passed, but the ids-only unit state built on top of it
(`unit-artifact-ids-only.md`) applies without the flag, to every command
line with `-hide-all-packages` and all units named by id. That includes
every Cabal build.

## Goal

Replace the *configuration* in a package database entry with
*convention* wherever a convention already exists in practice, so that
GHC can locate an installed unit the way a C or Rust compiler locates a
library: the build tool names the inputs, and everything else is found
on a search path by a fixed naming scheme.

The observation that motivates this: nearly every package database entry
Cabal writes has the same shape. In a `cabal-install` store
(`~/.local/state/cabal/store/ghc-⟨ver⟩/`) the unit
`acid-state-0.16.1.4-ec2ce8…` is described by a 60-line `.conf` whose
path fields all point into `⟨store⟩/acid-state-0.16.1.4-ec2ce8…/lib/`,
whose `hs-libraries` field is `HS` followed by the unit id, and whose
only non-derivable content is the module list, the dependency list and
the name/version label. The store directory itself already *is* a unit
store; `package.db/` next to it is a redundant index of it. The exceptions
are entries that name system directories, covered under "Counterexamples
and limits".

## Layout

A unit store is a directory. Each immediate subdirectory that contains a
`unit.meta` file is a unit, named by its directory:

```
⟨root⟩/
  ⟨uid⟩/
    unit.meta            the manifest (see below)
    lib/                 interfaces and libraries
      A/B.hi  A/B.dyn_hi  A/B.p_hi
      libHS⟨uid⟩.a
      libHS⟨uid⟩-ghc⟨ver⟩.so
      libHS⟨uid⟩_p.a
      include/           C headers, when the unit installs any
    share/               data files and documentation (not read by GHC)
```

This is the layout `cabal-install` produces today, which also puts a
`cabal-hash.txt` in each unit directory; the spike adds only the
`unit.meta` file. Subdirectories without a `unit.meta` are ignored, so a
real cabal store with its `package.db/` and `incoming/` entries is a valid
unit store.

Mapping to the fields the compiler reads (names as in
`GenericUnitInfo`):

| Field | Value under the convention |
|---|---|
| `unitId` | the directory name |
| `unitImportDirs` | `⟨root⟩/⟨uid⟩/lib` |
| `unitLibraryDirs`, `unitLibraryDirsStatic`, `unitLibraryDynDirs` | `⟨root⟩/⟨uid⟩/lib` |
| `unitLibraries` | `HS⟨uid⟩` (the compiler adds way tags and the `-ghc⟨ver⟩` suffix itself, see `unitHsLibs`) |
| `unitIncludeDirs` | `⟨root⟩/⟨uid⟩/lib/include` when that directory exists |
| `unitIsExposed` | true: visibility is decided by the environment layer |
| `unitIsTrusted` | false: trust is decided by `-trust` and `-trust-id` |
| `unitAbiHash` | a fingerprint of `unit.meta`: a cache key in `GHC.Unit.External.Index`, and what other units' `abi-depends` are checked against (see the limits below) |
| `unitAbiDepends`, `unitHaddockInterfaces`, `unitHaddockHTMLs`, `unitCcOptions`, `unitComponentName`, `unitDataDir` | empty |

Everything else comes from the manifest.

## The manifest

`unit.meta` is a sequence of `key: value` lines. A line beginning with
whitespace continues the previous value. Values are whitespace-separated
tokens; a token may be double-quoted. Unknown keys are ignored so the
format can grow.

```
name: foo
version: 1.2.0
exposed-modules: Foo Foo.Internal.Reexported from bar-0.3-9f2a:Bar.Impl
hidden-modules: Foo.Internal
depends: base-4.21.0.0-5b43 bar-0.3-9f2a
```

Keys, all optional:

| Key | Meaning | Default |
|---|---|---|
| `name`, `version` | the human label; feeds `-package name`, `MIN_VERSION_*` macros, wired-in detection and messages | the unit id, version `0` |
| `exposed-modules` | modules importable from outside; `M from ⟨unit⟩:⟨M'⟩` declares a reexport | none |
| `hidden-modules` | modules present in `lib/` but not importable from outside | none |
| `depends` | unit ids this unit was built against; defines the link closure | none |
| `indefinite` | `True` for a Backpack unit with unfilled holes | `False` |
| `instance-of` | the component id an instantiated unit was built from | the unit id up to the first `+` (`ghc-pkg` instead falls back to the whole unit id) |
| `instantiated-with` | `⟨Hole⟩=⟨unit⟩:⟨Module⟩` pairs, or `⟨Hole⟩=<⟨Hole⟩>` for an unfilled hole | none |
| `extra-libraries`, `extra-libraries-static`, `extra-ghci-libraries`, `frameworks`, `framework-dirs`, `ld-options`, `includes` | C-side link and header configuration, same meaning as in the package database | empty |

Unit and module syntax in `exposed-modules` and `instantiated-with` is
the one `-package-id` already accepts (`GHC.Unit.Parser`), so nested
instantiations work without a second grammar.

The first five keys are the irreducible pre-interface metadata from the
field analysis. The C-side keys are genuine configuration that no
convention can supply; across a cabal store nearly every unit leaves
them empty, and the `rts` unit is the main user.

## Resolution

Given `-unit-store ⟨root⟩`:

1. **Read the units.** With `-hide-all-packages` and every unit named by
   id, probe `⟨root⟩/⟨uid⟩/unit.meta` for the units the compilation needs
   ("Demand-driven reading" below). Otherwise list `⟨root⟩` and read every
   subdirectory with a `unit.meta`. Either way each manifest becomes a
   `UnitInfo` by the table above, and the result is a `UnitDatabase` like
   the one `readUnitDatabase` returns for a package database. From there
   on, store units are treated like any other.
2. **Find a module.** Unchanged: the module-to-unit map is built from
   `exposed-modules` and `hidden-modules`, and `GHC.Unit.Finder`
   searches `unitImportDirs` for `A/B.hi`.
3. **Find a library.** Unchanged: `GHC.Linker.Unit` looks for
   `lib/libHS⟨uid⟩⟨tag⟩.a` or `lib/libHS⟨uid⟩⟨tag⟩-ghc⟨ver⟩.so`.
4. **Close the dependency graph.** Unchanged: `closeUnitDeps` over
   `depends`.

A store and a package database can appear together in the same stack.
Later entries shadow earlier ones by unit id, as today.

## The environment layer

The store answers "what is installed". It deliberately does not answer
"what is visible by default" or "what is trusted", because the same
installed unit is used differently by different projects. GHC already
has a layer for that: `-package`, `-package-id`, `-hide-all-packages`,
`-trust` and environment files. The spike adds one directive to
environment files:

```
-- the path is relative to the environment file
unit-store ../store
package-id foo-1.2.0-ab12
package-id bar-0.3-9f2a
```

Only a whole line can be a comment: anything after `unit-store` is taken as
the path. This is the Rust shape exactly: a searched directory (`-L dependency=`)
plus explicit direct dependencies (`--extern`). Environment files already
imply `-hide-all-packages`, so nothing in a large store is visible
unless named.

Without an environment file, `-unit-store` alone exposes every unit in
the store, like a package database in which every entry is `exposed:
True`. GHC's existing preference rule (`compareByPreference`: latest
version, then latest database wins) then picks one unit per package
name. This is the right behaviour for a small hand-made store and the
wrong one for a thousand-unit cabal store, where `-hide-all-packages`
or an environment file is the intended mode.

## How this compares with other toolchains

| | Where metadata lives | Name to artifact | Native link flags |
|---|---|---|---|
| GHC today | central `package.conf.d` | `exposed-modules` in the db | db fields |
| Rust | `lib.rmeta` inside the rlib | `--extern name=path` for direct deps, `lib⟨name⟩-⟨hash⟩.rlib` on `-L` dirs for the rest | crate metadata |
| Go | export data inside the `.a` | explicit `-importcfg` map built by the go command | cgo directives in the object |
| Swift | `Name.swiftmodule` beside the library | file name equals module name on `-I` dirs | autolink entries in the object |
| GHC unit store | `unit.meta` beside the library | `-package-id` plus `⟨root⟩/⟨uid⟩/` | manifest |

Swift's name-equals-filename rule is not available to Haskell: a module
name does not determine its unit, and a reexported module has no file
in the reexporting unit. Rust's model is the one being copied.

## Counterexamples and limits

- **The `rts` unit.** It is the one unit whose `extra-libraries`,
  `ld-options`, `includes` and `include-dirs` carry substantial content,
  and `GHC.Unit.State` reads `DerivedConstants.h` from its include
  directory at startup. Its manifest is therefore the longest; it is
  still only a dozen lines. On `stable-ghc-9.14` the RTS way sub-libraries
  (`rts:⟨way⟩`) are wired in by matching `unitComponentName`, which store
  units leave empty, so they have to come from a package database.
- **Relocation.** All paths are derived from `⟨root⟩`, so moving the
  store moves everything. This is what `${pkgroot}` achieves in the
  database today, with the difference that no rewriting happens.
- **Paths outside the unit's tree.** The manifest has no key for them, and
  Cabal writes them: 5 of the 415 entries in a store for GHC 9.14.1, such
  as `zlib` with `/usr/lib64` in `library-dirs` and `/usr/include` in
  `include-dirs`. Those two are on the default search paths, but the same
  library installed elsewhere would not be found through a store. Hand
  registrations can go further (`import-dirs` elsewhere, `hs-libraries` not
  of the form `HS⟨uid⟩`). The package database stays available for all of
  these.
- **The `C`-prefixed library convention.** `unitHsLibs` understands
  `hs-libraries: Cfoo` as a non-Haskell library linked as `-lCfoo`
  statically and `-lfoo` dynamically. The store assumes `HS⟨uid⟩`; a
  unit that needs the other form belongs in a database.
- **Consumers of the database that are not GHC.** `ghc-pkg`, Cabal,
  Stack, Haddock, HLS and distribution packagers all read or write
  `.conf` files. The store is additive: the prototype adds a
  constructor, it removes nothing, and both can coexist in one stack
  for as long as needed.
- **`abi` and `abi-depends`.** Dropped. The per-module `mi_mod_hash`
  and `mi_usages` are the real check; the database fields only detect
  a dependency re-registered under the same id in a mutable database,
  which an immutable store keyed by a content-derived id cannot have.
  Mixing the two has one catch: a package database entry whose
  `abi-depends` records a store unit's real ABI hash fails validation,
  because that unit's `unitAbiHash` is a fingerprint of its manifest. No
  real entry we checked has `abi-depends` at all (see the field analysis).

## What is left in the manifest, and whether it could go too

The remaining pre-interface facts are the module lists, the dependency
list and the label. Each has a conceivable convention:

- *exposed vs hidden* could be a directory split or a bit in the
  interface file written at compile time;
- *reexports* could be stub interface files pointing at the origin;
- *depends* could be the union of `dep_direct_pkgs` over the unit's
  interfaces, at the cost of opening every interface in the closure
  (the interface header read stops before `mi_deps`), or a change of
  policy to link only what is imported;
- *the label* could be parsed from the unit id, which Cabal always
  writes as `⟨name⟩-⟨version⟩-⟨hash⟩`.

None of these is needed to remove the package database, and each is a
behaviour change in its own right. They are out of scope for the spike
and listed so the question is not reopened by accident.

## Prototype

Flag `-unit-store ⟨dir⟩` and environment-file directive `unit-store`.
Implementation:

- `GHC.Driver.DynFlags`: `PkgDbRef` gains `UnitStorePath OsPath`.
- `GHC.Driver.Session`: the flag and the directive, whose relative path is
  taken against the environment file's directory as for `package-db`.
- `GHC.Unit.State`: `-unit-store` paths are offset against `-working-dir`
  as `-package-db` paths are.
- `GHC.Unit.External.Database`: `resolveUnitDatabase` returns the kind
  of location along with the path.
- `GHC.Unit.External.Index`: `readOrGetUnitDatabase` dispatches on that
  kind, and `readUnitDatabasesOnDemand` does the probing.
- `GHC.Unit.External.Store` (new): enumerate or probe, parse `unit.meta`,
  build `UnitInfo`.

Tests in `testsuite/tests/driver/unit-store/` build the same artifacts
once, then compile and run a client against them twice, through a
`ghc-pkg` database and through a store, and require identical output.
Scenarios: a single unit, a chain with transitive linking, a hidden
module, a reexport, Safe Haskell trust, dynamic linking, an environment
file, and a Backpack indefinite unit instantiated by hand. Two more,
`demand` and `closure`, check what gets read and which units exist.

Results are recorded in the section below as they are obtained.

## Results

Obtained 2026-10-23 with a `--flavour=quick` build of master `0c17201cd3`
plus the prototype (GHC 10.1.20261023, x86_64 Linux, 16 cores). The
benchmarks have not been rerun on `stable-ghc-9.14`. The tests have, on a
static `make stage2` build: all pass except `unitstore_dynamic`, which is
skipped because that build has no dynamic libraries.

### Scenarios

Each scenario in `testsuite/tests/driver/unit-store/unitstore.sh` builds
its units once into `store/⟨uid⟩/lib`, writes a `pkgdb/⟨uid⟩.conf` with
`${pkgroot}`-relative paths and a `store/⟨uid⟩/unit.meta` from the same
arguments, then compiles, links and runs the client through
`-package-db pkgdb` and through `-unit-store store`, and compares the
outputs byte for byte.

| Scenario | What it exercises | Result |
|---|---|---|
| basic | one unit, module lookup, static link | identical |
| multi | `p` depends on `q`; `q` linked through the closure, never named | identical |
| hidden | importing a hidden module | identical error text, same error code |
| reexport | `r` exposes `Q from q-0.1:Q`; client sees `Q` through `r` only | identical |
| safe | Safe Haskell: `-fpackage-trust` without and with `-trust-id s-0.1` | identical failure, then identical success |
| dynamic | `.dyn_hi` and `libHS⟨uid⟩-ghc⟨ver⟩.so` in `lib/`, client `-dynamic` | identical |
| envfile | `unit-store ../store` directive, relative to the environment file | identical |
| backpack | indefinite `ind` with signature `H`, hand-instantiated as `ind+q` with `H=q-0.1:Q`, no Cabal | identical |

All eight, plus the `demand` scenario described below, pass under the
testsuite driver (`hadrian/build test --only="unitstore_…"`, 9 expected
passes, 0 unexpected failures). The tenth test, `unitstore_closure`, came
later with the ids-only unit state and is not in that count; it contrasts
the ids-only path with the by-name one.

Two things learnt from writing the scenarios rather than from the
prototype: a `Safe` module is trusted whatever its unit's trust, so a
trust test needs `Trustworthy`; and an indefinite unit must not appear in
the `depends` of its instantiations, because `depends` is the link
closure and an indefinite unit has no library. Cabal already omits it.

### Startup cost

1000 synthetic units with four modules each, metadata only, no
interfaces or libraries, compiling a one-line module with
`-hide-all-packages -package base` so that nothing is loaded or linked.
`hyperfine`, 10 runs after 2 warm-ups:

| Configuration | Mean |
|---|---|
| no extra database | 232 ms ± 27 |
| `-package-db` with `package.cache` (1000 units) | 304 ms ± 40 |
| `-unit-store` (1000 `unit.meta` files) | 326 ms ± 56 |

Reading 1000 small text files and parsing them costs about 20 ms more
than reading one binary cache, within the run-to-run noise. The store
reader is a straightforward `readFile'` per unit with no cache; a
per-store cache in the style of `package.cache` would close the gap, and
a parallel read would likely reverse it.

Pointing `-unit-store` at a real `cabal-install` store with 2423
entries and no `unit.meta` files reads it as an empty store, as
designed, in 24 ms over the plain run (332 ms vs 309 ms, 5 runs).

### Demand-driven reading

The demand-driven read stops enumerating stores when the compiler can know
which units it needs. A package database is one
`package.cache` file and has to be read whole; a store is addressable,
and `⟨root⟩/⟨uid⟩/unit.meta` is one `open`. That is the same property
Rust's crate locator relies on.

Under `-hide-all-packages`, the only units that can become visible are
those named by `-package-id` and `-plugin-package-id` flags and whatever
they reference. So `readUnitDatabases` (`GHC.Unit.External.Index`):

1. reads the package databases in full, as before, which on this branch is
   where the wired-in units come from (on the master version of this
   series, stage 3 of the ids-only work probes the compiler's own units by
   id like any other, so a stack of stores alone is enough);
2. takes the unit ids from `idsOnlyRoots` as roots (for an instantiated id,
   the indefinite unit plus the units of its instantiation);
3. probes every store on the stack for each root and follows
   `depends`, the origins of reexported modules and `instantiated-with`
   until nothing new is reached, through `probeUnitStore` in
   `GHC.Unit.External.Store` and `unitInfoReferencedUnits` in
   `GHC.Unit.External.Query`;
4. returns one `UnitDatabase` per store holding only the units reached,
   marked partial.

A flag that names a unit by package name cannot be probed for, so its
presence turns the optimisation off and the stores are enumerated as
before. Cabal always uses ids, and so do environment files unless they
contain `hide-package`, which takes a name. `UnitDatabase` gained a `unitDatabasePartial` flag so that a store read
on demand is extended rather than reused by a later `initUnits` (GHCi's
`:set -package-id`) and is never mistaken for a complete listing by an
eager read. The `UnitState` is still built from a list of databases, just
shorter ones.

The `demand` scenario puts a unit with a malformed manifest in the store.
With `-hide-all-packages -package-id p-0.1` the client compiles, links and
runs: the bad manifest is never opened. Without `-hide-all-packages` the
same compilation fails on it. Same 1000-unit store as above, `hyperfine`,
10 runs:

| Configuration | Mean |
|---|---|
| no extra database, `-package-id ⟨base⟩` | 221 ms ± 18 |
| `-package-db` with `package.cache` | 294 ms ± 34 |
| `-unit-store`, `-package base` (by name, enumerated) | 409 ms ± 35 |
| `-unit-store`, `-package-id ⟨base⟩` (probed) | 235 ms ± 24 |

Against the real 2423-entry cabal store with `-package-id ⟨base⟩`, the
probed read is indistinguishable from not passing the flag at all
(252 ms vs 264 ms, 5 runs). The one probe that happens is for base's id,
which is not in the store.

The by-name fallback is coarse: `-package base` is common in hand-written
command lines and defeats probing even though base is in the global
database; a name index per store, or a rule that names are resolved against
the eagerly read databases first, would remove that. `-trust ⟨name⟩` counts
as a by-name flag too and enumerates the stores; `-trust-id ⟨uid⟩` does not.

### What the store reader did not have to touch

The store reader on its own left `mkUnitState`, visibility, wiring,
validation, the finder, the module graph and all three linkers unchanged.
Its first version was 5 modified files (a constructor, a flag, an
environment-file directive, a `UnitDbLocation` type, a two-way dispatch)
and one new module of about 280 lines, two thirds of which parse the
manifest. The ids-only unit state, added later, does change `mkUnitState`
and visibility; see `unit-artifact-ids-only.md`.
