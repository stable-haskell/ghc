# Unit artifact spike: field analysis of the installed unit record

Status: research spike, October 2026. Companion to
`unit-artifact-model.md`. Nothing here is a proposal to change the
package database format yet; it is the evidence base for deciding what a
"unit store" (an installed unit located by convention rather than
described by configuration) has to carry.

## Question

`ghc-pkg` writes an `InstalledPackageInfo` record per unit. The compiler
reads it back as `GenericUnitInfo` (`libraries/ghc-boot/GHC/Unit/Database.hs`)
and specialises it to `UnitInfo` (`compiler/GHC/Unit/Info.hs`). The record
has 33 fields. For each field this document asks:

1. **Who consumes it** inside `compiler/` and `ghc/`? (Grep over `*.hs`
   for the record selector, excluding its definition sites. Counted on
   master at `0c17201cd3`, 2026-10-23.)
2. **Is it needed before any interface file is opened?** The compiler
   must find, for a given `import M`, which unit provides `M`, and then
   locate `M.hi`. Anything needed to answer that cannot live inside an
   interface file.
3. **Is it redundant** with information that already exists elsewhere
   (interface files, a directory listing, a fixed naming scheme)?
4. **Classification**: *convention* (derivable from a fixed layout),
   *policy* (describes how the user wants to use the unit, not the
   unit itself), *intrinsic* (a fact about the unit the compiler needs
   and cannot derive), or *provenance* (informational, no compiler
   consumer).

`GHC/Driver/Backpack.hs` consumes every field because it builds a
`UnitInfo` by hand for the in-memory database of a `.bkp` session. It is
listed once here and omitted from the per-field consumer lists below.

## Summary table

| Field | Consumers (excluding Backpack.hs) | Pre-`.hi`? | Class |
|---|---|---|---|
| `unitId` | everywhere | yes | intrinsic (the directory name) |
| `unitInstanceOf` | Unit/State, External/Index | yes | intrinsic (Backpack); derivable from the id string by ghc-pkg's own rule |
| `unitInstantiations` | External/Substitution, External/Providers | yes | intrinsic (Backpack) |
| `unitPackageId` | Unit/State, Types/Name/Ppr | no | label |
| `unitPackageName` | Unit/State, External/Wired, Driver/Make, Iface/Errors/Ppr, GHCi/UI/Exception | yes | label |
| `unitPackageVersion` | Driver/Make, SysTools/Cpp, External/Database | yes | label |
| `unitComponentName` | none | no | provenance |
| `unitAbiHash` | External/Validate, External/Index, Driver/Downsweep | no | redundant with `mi_mod_hash` |
| `unitAbiDepends` | External/Validate, Driver/Downsweep | no | redundant with `mi_usages`; always empty in practice |
| `unitDepends` | Linker/Unit, Linker/Loader, Driver/Pipeline, Driver/Downsweep, External/Validate, External/Index | yes (link closure) | intrinsic |
| `unitExposedModules` | External/Providers, External/Substitution, External/Index, Driver/Make, Tc/Utils/Backpack | yes | intrinsic |
| `unitHiddenModules` | External/Providers | yes | intrinsic, or convention via directory split |
| `unitIsIndefinite` | External/Wired, External/Providers | yes | intrinsic (Backpack) |
| `unitIsExposed` | Unit/State | yes | policy |
| `unitIsTrusted` | Unit/State | no | policy |
| `unitImportDirs` | Unit/Finder | yes | convention |
| `unitLibraryDirs` | External/Database (default for the two below) | no | convention |
| `unitLibraryDirsStatic` | Linker/Unit, StgToJS/Linker/Utils | no | convention |
| `unitLibraryDynDirs` | Runtime/Interpreter/Wasm | no | convention |
| `unitLibraryBytecodeDirs` | Linker/Loader | no | convention |
| `unitLibraries` | Linker/Loader, StgToJS/Linker/Utils, Runtime/Interpreter/Wasm | no | convention (`HS<uid>`) |
| `unitExtDepLibsSys` | Linker/Unit, Linker/Loader | no | intrinsic (C side) |
| `unitExtDepLibsStaticSys` | Linker/Unit | no | intrinsic (C side) |
| `unitExtDepLibsGhc` | Linker/Loader | no | intrinsic (C side), rarely used |
| `unitExtDepFrameworks` | Linker/Loader | no | intrinsic (C side, Darwin) |
| `unitExtDepFrameworkDirs` | Linker/Loader | no | intrinsic (C side, Darwin) |
| `unitLinkerOptions` | Linker/Unit, Linker/Loader | no | intrinsic (C side) |
| `unitCcOptions` | none | no | provenance |
| `unitIncludeDirs` | Unit/State, Linker/Executable | yes (rts only) | convention (`include/`) |
| `unitIncludes` | Driver/CodeOutput | no (rts only) | intrinsic (rts only) |
| `unitHaddockInterfaces` | none | no | provenance |
| `unitHaddockHTMLs` | none | no | provenance |

Totals: 11 convention or provenance fields with no compiler consumer
that a fixed layout would not satisfy, 2 policy fields, 3 label fields,
2 redundant hash fields, and 15 intrinsic fields of which 7 are C-side
link configuration that only the `rts` unit populates in any depth.

## Field by field

### Identity

**`unitId`.** The primary key. Every subsystem uses it; it is embedded in
every exported symbol name, so two units with the same id cannot coexist
in one program. In a store layout it is the directory name and nothing
needs to be written down.

**`unitInstanceOf`, `unitInstantiations`, `unitIsIndefinite`.** Backpack.
For an indefinite unit the compiler needs to know the module holes it
has (`unitInstantiations` with `DbModuleVar` entries) and that it cannot
be linked. For an instantiated unit it needs the component id it was
instantiated from and the instantiation, because the `Unit` of every
module inside it is a `VirtUnit` built from those two. The IPI text
format has no `instance-of` field: `ghc-pkg` derives it from the
`installedComponentId` (`utils/ghc-pkg/Main.hs:1450`), which for a
definite unit is the unit id itself. The same information is also in
every interface file of the unit (`mi_module` is a `Module` whose unit
carries `instUnitInstanceOf` and `instUnitInsts`), but the compiler
needs it to build the module-to-unit map before it opens any interface,
so it stays pre-`.hi`.

### Labels

**`unitPackageName`, `unitPackageVersion`, `unitPackageId`.** Human
names. Consumers:

- `GHC/Unit/State.hs:481` and `:927`: `-package name` resolution and the
  name-to-instance map used by `-package name` with Backpack.
- `GHC/Unit/External/Wired.hs:88`: wired-in detection compares the
  *package name* with `base`, `ghc-prim` and friends.
- `GHC/SysTools/Cpp.hs:249`: the `MIN_VERSION_<pkg>` and
  `VERSION_<pkg>` CPP macros.
- `GHC/Driver/Make.hs:480`: `:show packages` style listing.
- `GHC/Unit/External/Database.hs`: `compareByPreference` prefers the
  highest version when several exposed units share a name.
- `Iface/Errors/Ppr.hs`, `GHCi/UI/Exception.hs`, `Types/Name/Ppr.hs`:
  error messages and pretty printing.

Cabal always writes unit ids of the form `<name>-<version>-<hash>` or
`<name>-<version>-inplace`, so the label is recoverable from the id by
convention, but the compiler treats `UnitId` as opaque and nothing in
`compiler/` parses it. Keep this as a one-line label in the per-unit
manifest rather than teaching the compiler to parse ids.

**`unitComponentName`.** No consumers outside Backpack.hs. Provenance.

### Hashes

**`unitAbiHash`.** Three consumers:

- `External/Validate.hs:381`: checks `unitAbiDepends` of dependents.
- `External/Index.hs:294,313,390`: the `GlobalUnitKey` that lets several
  home-unit environments share one `UnitInfo` is `(unitId, unitAbiHash)`.
  This is a cache key, not a correctness check; any stable per-unit
  fingerprint would do, including a hash of the manifest file.
- `Driver/Downsweep.hs:1024`: recorded in the unit node of the module
  graph for recompilation checking of external units.

The real ABI check in GHC is per module: `mi_mod_hash` for the exporting
module and `mi_usages` in the importer. `unitAbiHash` is the aggregate
Cabal computes from the per-module hashes at install time. It can be
recomputed from the interfaces, or replaced by a manifest hash for the
cache-key use.

**`unitAbiDepends`.** `Validate.hs:377` uses it to mark a unit broken
when a dependency's ABI differs from the one it was built against,
which only matters when a *mutable* database has a dependency
re-registered under the same id. `Downsweep.hs:1100` notes that in
practice the field never contains anything other than the empty list.
In an immutable store keyed by a content-derived id the situation it
guards against cannot arise.

### Dependencies and modules

**`unitDepends`.** Needed before any interface is read, for two jobs:

- the link closure (`Driver/Pipeline.hs:592`, `Linker/Unit.hs`,
  `Linker/Loader.hs:1250`): every transitive dependency of a visible
  unit is linked, whether or not any of its modules was imported;
- validation (`Validate.hs`): a unit whose dependency is missing is
  unusable, and the error message names it.

The union over all modules of a unit of `dep_direct_pkgs` and
`dep_plugin_pkgs` in `mi_deps` gives the set of units actually imported,
which is a subset of the Cabal `build-depends`. Deriving the link
closure from interfaces would mean opening every interface of every unit
in the closure, or changing the policy to "link only what is imported".
Both are measurable; neither is free. Keep it in the manifest.

**`unitExposedModules`.** The module-to-unit map (`External/Providers.hs`)
is built from this before any interface is opened. Each entry is either
a module of this unit or a reexport with its origin module in another
unit. A reexport has no interface file in the reexporting unit, so the
origin cannot be recovered from the file system. Irreducible.

**`unitHiddenModules`.** Hidden modules have interface files too, since
exposed modules import them. Providers.hs uses the list so that
`import Hidden` from outside the unit gives a "hidden module" error
rather than "not found". In a store this could be a directory split
(`interfaces/exposed/` vs `interfaces/hidden/`) or a manifest list.
The manifest list is simpler and costs one line.

### Policy

**`unitIsExposed`.** `State.hs:864` seeds the initial visibility map
with every exposed unit when there is no `-hide-all-packages`.
Whether a unit is visible by default is a property of the environment
the user is compiling in, not of the compiled artifact: the same
installed `containers-0.8` is exposed in one project and hidden in
another. GHC already has the right home for this: `-package`,
`-hide-all-packages` and environment files.

**`unitIsTrusted`.** Safe Haskell trust (`State.hs:511`), overridable
with `-trust`/`-distrust`/`-distrust-all-packages` through the
`TrustOverlay`. Same argument: it is the user's trust decision, stored
per installation rather than per environment only for historical
reasons.

### Paths

**`unitImportDirs`.** `Unit/Finder.hs` searches these for `M.hi`. In
Cabal output there is exactly one, `<libdir>/<uid>`. Convention:
`<root>/<uid>/interfaces/`.

**`unitLibraryDirs`, `unitLibraryDirsStatic`, `unitLibraryDynDirs`,
`unitLibraryBytecodeDirs`.** Where `libHS<uid>.a`,
`libHS<uid>-ghc<ver>.so` and bytecode archives live.
`External/Database.hs` already defaults the last three to the first when
they are empty, which is what every Cabal-written entry relies on.
Convention: `<root>/<uid>/lib/`.

**`unitLibraries`.** The `hs-libraries` field. Cabal always writes
`HS<uid>`; the compiler appends the way suffix and the GHC version
itself (`unitHsLibs` in `Linker/Unit.hs`). Convention.

**`unitIncludeDirs`.** Two consumers, both specific to the `rts` unit:
`State.hs:570` reads `DerivedConstants.h` from the rts include dir to
load platform constants, and `Linker/Executable.hs:358` passes the rts
include dir when compiling the generated `main` stub. Convention:
`<root>/<uid>/include/` when it exists.

**`unitHaddockInterfaces`, `unitHaddockHTMLs`.** No consumers in the
compiler. Haddock reads them through the GHC API's `UnitInfo`, but it
could equally follow a convention (`<root>/<uid>/doc/`). Provenance.

### C side

**`unitExtDepLibsSys` (`extra-libraries`), `unitExtDepLibsStaticSys`,
`unitExtDepLibsGhc`, `unitExtDepFrameworks`, `unitExtDepFrameworkDirs`,
`unitLinkerOptions` (`ld-options`).** Consumed by `Linker/Unit.hs`
(`collectLinkOpts`) and `Linker/Loader.hs` when linking a program or
loading a unit into GHCi. These are genuine configuration: nothing on
disk says that `zlib-0.7` needs `-lz`. Rust records the equivalent in
crate metadata, Swift and Go in the object files. They stay in the
manifest. Across a Cabal store the overwhelming majority of units have
all six empty; the `rts` unit has by far the richest set
(`rts/rts.cabal` writes a dozen conditional `extra-libraries` entries
such as `c`, `rt`, `ffi`, `numa`, `zstd` and `bfd iberty`, plus
`extra-libraries-static` and sanitizer `ld-options`).

**`unitIncludes` (`includes`).** One consumer, `Driver/CodeOutput.hs:303`,
which `#include`s the rts unit's listed headers (`Stg.h`) into generated
C stubs. Only the rts unit sets it. Manifest, rts only.

**`unitCcOptions`.** No consumers outside Backpack.hs. Provenance.

## What the interface files already carry

For the record, because it bounds how small the manifest can get. Every
`.hi` file (`GHC/Unit/Module/ModIface.hs`, `GHC/Unit/Module/Deps.hs`)
contains:

| Database field | Interface equivalent |
|---|---|
| `id` | `mi_module` |
| `instance-of`, `instantiated-with` | the `InstantiatedUnit` inside `mi_module`; `mi_sig_of` for signatures |
| `depends` (per module) | `dep_direct_pkgs`, `dep_plugin_pkgs` |
| `trusted` (what the module trusts) | `mi_trust`, `mi_trust_pkg`, `dep_trusted_pkgs` |
| `abi` | `mi_mod_hash`, finer |
| `abi-depends` | `mi_usages`, finer |

Not in any interface: whether the unit exposes the module, reexports,
the name/version label, and all C-side fields. Those are exactly the
contents of the minimal manifest in `unit-artifact-model.md`.

## Changes since the September 2026 pass

The unit code was restructured between `ac7b737e8da` (September) and
`0c17201cd3` (October): database reading moved from `GHC/Unit/State.hs`
into `GHC/Unit/External/{Database,Index,Providers,Validate,Visibility,
Wired,Substitution}.hs`, and a process-wide `UnitIndexCache` now shares
`UnitInfo` records across home units keyed by `(unitId, unitAbiHash)`.
Two consumers appeared that did not exist in September:
`Driver/Downsweep.hs` reads `unitAbiHash` and `unitDepends` to build
unit nodes in the module graph, and `External/Index.hs` uses
`unitAbiHash` as a cache key. Neither changes a classification; the
cache-key use is the reason `unitAbiHash` is listed as redundant rather
than provenance.
