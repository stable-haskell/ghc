# Unit artifact spike, stage 3 sketch: an ids-only driver

Status: design sketch, October 2026; stages 1 and 2 of the path at the
end are implemented (see the two sections after it). Third document of
the unit store spike, after `unit-artifact-field-analysis.md` and
`unit-artifact-model.md`. Code references are to master `0c17201cd3`
plus the commits of this spike.

## Premise

`-package-id ⟨uid⟩` and `-unit-store ⟨dir⟩` are to GHC what a named
artifact and a search directory are to any other compiler: the build tool
says *which* units, the search path says *where*. Package *names* have no
place in that picture. Yet the driver resolves names to installed units
in several places, and a good part of `GHC.Unit.State` exists only to do
so. This sketch asks what the driver looks like when names are labels
and never lookup keys, because that is where the simpler compiler is
hiding.

## Where names are lookup keys today

| Place | Code | What it resolves |
|---|---|---|
| `-package`, `-plugin-package` | `Session.hs` `parsePackageArg`; `State.hs` `applyPackageFlag` with `PackageArg`; `Validate.hs` `findPackages`, `matchingStr` | a name, or `name-version`, to the preferred matching unit |
| `-hide-package` | same, `HidePackage` carries a `String` only | all units with that name |
| `-ignore-package` | `Validate.hs` `ignoreUnits` | all units with that name |
| `-trust`, `-distrust` | `State.hs` `applyTrustFlag` via `selectPackages (PackageArg str)` | all units with that name |
| Default visibility | `State.hs` `mkUnitState`: "latest version of each package name", `compareByPreference`, `sortByPreference`, the precedence map | which of several same-named units is visible when no flag says |
| Same-name hiding | `applyPackageFlag`, `vm_cleared`: exposing `p-0.2` hides `p-0.1` unless `-hide-all-packages` | name |
| Wired-in units | `Wired.hs` `findWiredInUnits`: `unitPackageName pc == PackageName (unitIdFS pid)` for `ghc-internal` and `rts`; then `WireMap`/`UnwireMap` rewrite ids in every `UnitInfo` and module | name to the unit the compiler will call by its fixed id |
| The `ghci` unit | `Runtime/Interpreter/{C,JS,Wasm}.hs` call `lookupPackageName` for `"ghci"` | name |
| PackageImports | `renameRawPkgQual` → `lookupPackageName` over `packageNameMap` | `import "containers" M` to a unit |
| Backpack by name | `Backpack.hs:685` `lookupPackageName`; `pkgname_map` in `mkUnitState` | `-package p` shorthand for instantiation |

Two things in the current tree already point the other way. `base` is
not wired in by name any more: its id comes from the settings file
(`"base unit-id"`, overridable with `-base-unit-id`), and `wiredInUnitIds`
is down to `ghc-internal` and `rts`. And `Note [GHC's Unit Id]` records
that the `ghc` library's unit id now matches its unit key, so it needs no
rewriting at all. The ids-only mode finishes what those two started.

## The model

The compiler takes:

| Flag | Meaning | Analogue |
|---|---|---|
| `-unit-store ⟨dir⟩` (and `-package-db ⟨dir⟩` during transition) | where to look | `-L` |
| `-package-id ⟨uid⟩[(renaming)]` | a unit this compilation may import from; the renaming and thinning syntax is unchanged | `--extern` |
| `-plugin-package-id ⟨uid⟩` | same, for plugins | |
| `-trust-id ⟨uid⟩`, `-distrust-id ⟨uid⟩` | Safe Haskell policy for a named unit | |
| `-this-unit-id ⟨uid⟩`, `-this-component-id`, `-instantiated-with` | the home unit, unchanged | |
| `-no-auto-link-packages` | do not add the implicit units below | `-nostdlib` |

And it knows, from the settings file rather than from any database, the
ids of the units it has to find on its own: `base`, `ghc-internal`, `rts`
and `ghci`. They are linked implicitly unless `-no-auto-link-packages`,
exactly as `cc` links libc.

Nothing is visible unless named. Two units with the same package name
may both be visible; a module provided by both is ambiguous at the
import site, as it already is under `-hide-all-packages`, and renaming
or thinning disambiguates, as it already does.

Names and versions stay in the manifest as *labels* of visible units:
`MIN_VERSION_⟨pkg⟩` macros, `:show packages`, error messages, and
`import "pkg" M`, which resolves against the visible set only and errors
if two visible units carry the name.

## What the unit state computation becomes

Today `mkUnitState` runs the four-phase plan written at the top of the
function: merge all databases, prune, compute default visibility and
apply flags, wire, build the module map. Ids-only is shorter:

1. **Roots.** The unit ids from `-package-id`, `-plugin-package-id`,
   the settings-file ids, and the home unit's instantiation.
2. **Closure.** Probe the stack for each root; follow `depends`,
   reexport origins and `instantiated-with`. This is the stage 2
   prototype's `readUnitDatabasesOnDemand`, now the only path. Later
   locations on the stack shadow earlier ones by id, as today.
3. **Validate the closure.** Missing dependency or cycle: error naming
   the root that pulled it in. `validateDatabase` on a set of a few
   hundred units instead of thousands.
4. **Visibility.** Exactly the roots with their renamings. No default
   set, no preference, no same-name hiding.
5. **Module map.** `mkModuleNameProvidersMap` over the visible units,
   unchanged.
6. **Preload.** Roots plus the implicit units, closed for linking by
   `closeUnitDeps`, unchanged.

There is no wiring step: the id GHC uses for `ghc-internal` *is* the id
it was installed under, because the settings file says so.

### What disappears

Rough line counts, from the current files; estimates, not measurements.

| Removed | Where | Lines |
|---|---|---|
| By-name finders and the precedence/preference ordering | `Validate.hs` `findPackages` name arm, `selectPackages`, `matchingStr`, `ignoreUnits`; `Database.hs` `compareByPreference`, `sortByPreference`, `UnitPrecedenceMap` | ~120 |
| Default visibility and same-name hiding | `State.hs` `mkUnitState` steps 2a–b, `applyPackageFlag`'s `vm_cleared` and `no_hide_others` | ~100 |
| Wiring | `Wired.hs` entirely; `Index.hs` `setupWiredInUnits`, `updateWiredInUnitIndex`, `updateWiredInUnits`, `unwireUnit`, `upd_wired_in*`; `updateVisibilityMap`; `extendForWireMap` | ~250 |
| Name flags | `Session.hs` parsers for `-package`, `-plugin-package`, `-hide-package`, `-ignore-package`, `-trust`, `-distrust`; `PackageArg` constructor; `IgnorePackageFlag` | ~80 |
| `exposed` and `trusted` as installed state | `GenericUnitInfo` fields, `TrustOverlay` by name, `distrustAllUnits` | ~60 |
| Name map | `packageNameMap`, `lookupPackageName`, `pkgname_map` | ~30 |

Against about 2,400 lines in `State.hs` plus `External/` (excluding
`Store.hs`), roughly a quarter goes, and what remains is the part that
does real work: reading manifests, closing dependencies, validating,
building the module map, computing the link set.

### What stays and why

- **Thinning and renaming** on `-package-id`. That is how two visible
  units with overlapping module names coexist. Already id based.
- **Backpack.** `-package-id p[A=q:M]` is already id based; the
  requirement collection in `applyPackageFlag` stays.
- **The module map and the finder.** Unchanged.
- **Shadowing by id across the stack.** A later `-unit-store` or
  `-package-db` wins for the same id. This is positional, not name based.
- **Labels.** Name and version in the manifest and in `UnitInfo`.

## Decisions the sketch forces

**Fixed ids for the compiler's own units.** The settings-file route
already used for `base` extends to `ghc-internal`, `rts` and `ghci`:
hadrian and Cabal know the ids they installed and write them down. The
wired-in machinery then has no job. `Note [Wired-in units]` explains the
original motive, not baking versions into GHC; the settings file
achieves that too, since the compiler reads the id at startup. The
tension is with reinstallable `base`: several bases in one store is
fine, several bases in one *compilation* is not, and the settings file
picks one. That is the same restriction wiring imposes today.

**PackageImports.** Keep the syntax, resolve the name against the
visible units' labels, error on ambiguity. The alternative, allowing a
unit id in the string, is a language change and not needed.

**GHCi.** `:set -package foo` is the one place a human types a name
interactively. The honest answer is that it belongs to the tool that
owns the environment, which is what `cabal repl` and environment files
already provide. During transition, GHCi can keep a *front-end* name
resolver (below) without the compiler core knowing about names.

**Trust.** `-trust`/`-distrust` move to ids. Trust is per compilation
policy and stays a flag; it just stops being a name.

**Enumeration.** `--show-packages`, Haddock and HLS list installed
units. That becomes an explicit operation on the store (`ghc
--list-units ⟨dir⟩` or a library function), separate from compilation,
which never enumerates.

## A staged path

Each stage is independently useful and leaves the tree working.

1. **Front-end name resolution.** Resolve `-package ⟨name⟩`, `-hide-package`,
   `-trust ⟨name⟩` to ids in the driver *before* `mkUnitState`, by a
   function that looks at the eagerly read databases only. The core then
   sees ids exclusively. This proves the core can be ids-only while every
   flag keeps working, and it moves the by-name code into one place with
   a clear boundary. (Implemented, see below.)
2. **Ids-only core.** `mkUnitState` with the six steps above, taken when
   the front end produced no name flags (always true for Cabal and
   environment files). The current path remains for the rest. The stage 2
   prototype's `demandRoots` is the switch and becomes unconditional
   inside this path. (Implemented, see below.)
3. **Settings-file ids for `ghc-internal`, `rts`, `ghci`.** Hadrian
   writes them next to `base unit-id`; the three `lookupPackageName
   "ghci"` calls read the setting; `wiredInUnitIds` becomes empty and
   `Wired.hs` goes.
4. **Default to ids-only.** `-hide-all-packages` becomes the only
   behaviour; name flags warn, then go, with `ghci`'s `:set -package`
   routed through the stage 1 resolver as the last holdout.
5. **Delete** the by-name code listed above.

Stage 1 is a few hundred lines and a day of work on top of the current
prototype. Stage 3 touches hadrian and Cabal's settings output and is the
one that needs agreement outside this tree.

### Stage 1, implemented

Done on 2026-10-23 (commit `refactor(driver): resolve package-name flags
to unit ids at one front-end boundary`). The shape it took:

- `HidePackage`, `IgnorePackage`, `TrustPackage` and `DistrustPackage`
  carry a `PackageArg` (name or unit id) like `ExposePackage` already did,
  and the parsers in `GHC.Driver.Session` build the name form.
- `GHC.Unit.External.Names` is the boundary. `mkUnitState` calls
  `resolveIgnoreFlags` on the merged map before `validateDatabase`, then
  `resolveTrustFlags` and `resolvePackageFlags` on the validated units and
  the unusable set, which are exactly the inputs the by-name matching used
  to see. From there on only ids flow. `Note [Names are resolved in the
  front end]` documents the rules, which reproduce the old behaviour:
  `-package` picks the most preferable match, hide/trust/distrust expand
  to one flag per match, `-ignore-package` of an unknown name stays
  silent, and errors reuse `PackageFlagErr`/`TrustFlagErr` with the
  original flag so the messages are byte-identical.
- The arms of `findPackages`, `selectPackages` and `ignoreUnits` that
  matched a name now panic through `nameReachedCore`, so a name cannot
  leak into the core unnoticed.
- The flag text as typed replaces `PackageArg` in `uv_explicit`,
  `explicitUnits` and `DriverUnusedPackages`, so `-Wunused-packages`
  still says `exposed by flag -package foo`, and the core's visibility
  record no longer mentions `PackageArg` at all.

What it did not do: the by-name code still exists, in one module, and
`demandRoots` still falls back to enumerating stores when a name flag is
present, because resolution happens after the databases are read. Stage 2
is where the resolved ids feed the demand-driven read directly.

### Stage 2, implemented

Done on 2026-10-23 (commit `feat(driver): build the unit state from the
closure of the unit ids on the command line`). `mkUnitState` is now a
switch between two functions, documented in `Note [The ids-only unit
state]` in `GHC.Unit.State`:

- `idsOnlyRoots` (the former `demandRoots`) decides. It returns the roots
  when `-hide-all-packages` is set and every flag carries a unit id:
  `-package-id`, `-plugin-package-id` (an instantiated id contributes the
  units it is made of), the implicitly linked units (`base` from the
  settings file, the wired-in ids), and for a Backpack home unit the
  indefinite unit of `-this-component-id` and the units in
  `-instantiated-with`, which `UnitConfig` now carries (Cabal-driven
  Backpack builds load the indefinite unit's interfaces to check the
  instantiation, and name it nowhere else). Any `-package`,
  `-hide-package`, `-ignore-package`, `-trust` or `-distrust` by name
  returns `Nothing`. The same roots drive the demand-driven store read, so
  the stage 2 prototype's switch and the core's switch are one.
- `mkUnitStateIdsOnly` runs the six steps. Stores are probed, package
  databases read in full, the stack merged with the usual shadowing by id,
  and then `closeOverReferences` (in `GHC.Unit.External.Query`, next to
  `unitInfoReferencedUnits`, which moved there from `Store.hs`) keeps only
  the units reachable from the roots over dependencies, reexport origins
  and instantiations. `validateDatabase` runs on that closure. Visibility
  is a fold of `applyPackageFlag` over an empty map with same-name hiding
  off. No default visibility, no preference ordering, no name resolution.
- `mkUnitStateByName` is the old function, with the names resolved at the
  stage 1 boundary. Both end in `finishUnitState`: wiring, plugin
  visibility, the module map and the preload set, unchanged.
- `-trust-id ⟨uid⟩` and `-distrust-id ⟨uid⟩` exist, so Safe Haskell policy
  can be stated without a name. They are the only new flags.

Two decisions taken on the way:

- **Units outside the closure do not exist.** They are not validated, not
  in `unitInfoMap`, not in `--show-packages`, and a module they provide is
  "not found", not "a member of the hidden package". This is what the
  model says (compilation never enumerates) and it is the behaviour the
  store route already had; the package database route now matches it. Two
  testsuite expectations change. `package/ImportReexport` compiles with
  `-hide-all-packages` and no flags and listed `ghc`, `ghc-boot` and
  `ghc-platform` as hidden providers of `GHC.Platform.ArchOS`; none is
  reachable from `base`, so the module is now simply not found.
  `package/T22884_interactive` does `:set -hide-all-packages` in GHCi and
  then `import Data.Text`, and used to be told that `text` is hidden and
  that `:set -package text` would expose it; now it is told the module is
  in no known package. The second one is the GHCi convenience the sketch
  discusses under "Decisions": it depends on enumerating what is
  installed, which this path does not do, and `:set -package text` still
  works (it switches the session to the by-name path). Keeping interactive
  sessions on the by-name path would be a one-line change in
  `idsOnlyRoots` if that convenience is wanted during the transition.
  `T27013e`/`T27013f`, which expect the hint for `base`, are unaffected
  because `base` is a root.
- **The compiler's own units are a by-name hold-out.** `ghc-internal`,
  `rts` and `ghci` are not reachable from the roots in general (`ghci` is
  reachable from nothing a user names; the other two only through `base`),
  and until stage 3 writes their ids in the settings file they can only be
  found by name. `compilerUnitRoots` in `GHC.Unit.External.Names` adds
  every unit carrying one of those names to the roots, so wiring and the
  external interpreter see the same candidates as before. `ghci` is added
  only when the external interpreter may run (`-fexternal-interpreter`, or
  a JavaScript or Wasm target): it is the one consumer, and its
  dependencies are two thirds of the boot libraries, which would otherwise
  be in every closure. With the stage 1 build, `-package-id base` alone
  gives a closure of 5 units out of the 45 installed. This is the single
  by-name lookup left on the ids-only path, lives in the boundary module,
  and is what stage 3 deletes. Through a pure `-unit-store` stack it finds
  nothing, as before this stage, which is exactly the stage 3 motivation.

What stage 2 does not do: `-hide-package` and `-ignore-package` have no
by-id spelling (under `-hide-all-packages` the first is pointless and the
second a corner case), `:set -package foo` in GHCi still switches the
session to the by-name path on the next `initUnits`, and the by-name path
itself is untouched.

## The simpler compiler

Put together with the first two stages of the spike, the driver that
emerges is this. Inputs are source files. `-unit-store` directories are
where compiled units live, laid out by convention. `-package-id` names
the units this compilation may import from; their dependencies are found
by following manifests, one file per unit, never by listing a directory.
Interfaces are found at `⟨root⟩/⟨uid⟩/lib/A/B.hi`, libraries at
`⟨root⟩/⟨uid⟩/lib/libHS⟨uid⟩`. The compiler's own units have ids it reads
from its settings. There is no registration, no database, no preferred
version, no wiring, and no notion of "installed and exposed". That is
the shape of rustc with `--extern` and `-L`, and it is already present in
the current driver under `-hide-all-packages` with `-package-id` flags,
which is how every Cabal-driven compilation has run for years. The work
is to make that path the only one.
