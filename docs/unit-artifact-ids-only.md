# Unit artifact spike, stage 3 sketch: an ids-only driver

Status: design sketch, October 2026, not implemented. Third document of
the unit store spike, after `unit-artifact-field-analysis.md` and
`unit-artifact-model.md`. Code references are to master `0c17201cd3`
plus the two prototype commits of this spike.

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
   a clear boundary.
2. **Ids-only core.** `mkUnitState` with the six steps above, taken when
   the front end produced no name flags (always true for Cabal and
   environment files). The current path remains for the rest. The stage 2
   prototype's `demandRoots` is the switch and becomes unconditional
   inside this path.
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
