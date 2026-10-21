# Implementation plan for ABI qualified targets

## Result and starting point

Implement the [target specification](profiling-qualified-target-spec.md) so a
compiler selected as `aarch64-darwin-prof-ghc` behaves as a separate profiling
target. Preserve complete machine ABI information, ordinary artifact names,
and Cabal's existing Build/Host graph. Requirements R1 through R12 and tests T1
through T13 in the specification define acceptance.

Source review D1 separately verifies the general identity/profiling boundary
and the separation of independent fixes; it is not a runtime test.

Start from the reviewed source in GHC #199 at
`c2faa54b77b2a0bf228e7e11bc9d80334be308a2` and Cabal #501 at
`99b826a276e845bbe3e623e5c5bddc22731605fa`. Recheck their exact heads and base
branches before implementation. Keep implementation in isolated worktrees;
the main GHC checkout has unrelated untracked files. No code or PR head is
changed by this documentation work.

## Change sequence and PR structure

Use four semantic units. Units 1 and 2 establish a general identity protocol;
units 3 and 4 apply profiling and remove legacy ways. Each unit includes its
own relevant regression tests and can be reviewed independently, but the
profiling product is accepted only after the integrated installed workflow.

| Unit | Repository and purpose | Dependencies | Result |
| --- | --- | --- | --- |
| 1 | GHC lossless machine target and logical target metadata | Current stable compiler/toolchain baseline | Full machine environment survives; validated `Target name` and matching namespace protocol exist. |
| 2 | Cabal lossless target identity | Unit 1 protocol; fixtures can run first | Full names enter hashes, paths, discovery, and plan output; legacy metadata has an explicit fallback. |
| 3 | GHC profiling ABI target | Unit 1 and matching Cabal from unit 2 | `M-prof` dispatch, settings, code generation, interpreter/plugin rules, install packaging, and ABI guards work. |
| 4 | Cabal profiling adapter and legacy-way deletion across both repos | Units 2 and 3 | Thin profiling policy, correct option scope, and no automatic profiling filename ways remain. |

Separate prerequisite PRs carry the persistent-store lifetime fix, HPC coverage
identity fix, and generic `InLibrary` stage/metadata pairing fix. Preserve their
existing regressions when extracting them. The profiling PRs may depend on
those fixes where the installed workflow exercises them, but their descriptions
must explain that dependency instead of presenting them as profiling design.

The current large branch already combines these concerns. First classify its
diff against each actual PR base, then construct clean semantic commits from
the final reviewed code. Preserve fixes found during earlier review; do not
copy an earlier incomplete profiling implementation just to reduce the diff.
Use a new clean branch/PR when replacing history would be necessary. Normal
fast-forward updates can retain the current PR. Do not force-push, close,
publish, or merge as part of this planning task.

These units describe review boundaries, not permission to ship an intermediate
incompatible pair. Prepare and qualify both repositories together. Make the
matching Cabal revision available first, retaining its tested legacy-compiler
fallback, then enable/distribute GHC's new metadata and namespace protocol
with that exact Cabal pin. Do not release a new-protocol native GHC whose
default build/client still uses legacy namespace calculation. The profiling
target is delivered only after units 3 and 4 pass together.

## Phase 1 Establish the identity boundary

### GHC machine target retention

Inspect `utils/ghc-toolchain/exe/Main.hs` around target normalization and
`parseTriple`, and `utils/ghc-toolchain/src/GHC/Toolchain/{ParseTriple,Target}.hs`.
Retain the normalized full machine triple before `parseTriple` discards its
environment. Add an explicit retained field to the target record and update
its handwritten `Show`, `Read` consumers, constructors, generated fixtures,
configure comparison tooling, and build declarations together.

Make `targetPlatformTriple` return that retained machine identity. Keep
`tgtLlvmTarget` and C ABI flags independent. Audit consumers that previously
expected a three-component reconstructed value; use `ArchOS` for actual
platform checks and the full string for identity. Update settings production
in `ghc-toolchain`, `hadrian/src/Rules/Generate.hs`, and
`hadrian/bindist/Makefile`. Test ordinary toolchain serialization before adding
profiling.

### GHC logical target and namespaces

Add validated `target name` settings and a shared target-name/namespace helper
in the appropriate ghc-boot layer. It must be usable by the compiler and
`ghc-pkg` without importing compiler internals. Retain the distinction between
old settings lacking the key and new settings advertising the contract.

Update `compiler/GHC/Settings{,/IO}.hs`, platform/profile metadata as needed,
`compiler/GHC/Driver/Session.hs`, and command-line query dispatch for
`--print-target-name`. Keep the `Target platform` value machine-only.
Validate contradictory identity fields and audit duplicate `--info` fields.

Add the package tool's independent read-only `ghc-pkg --info` query. Report its
own selected target settings, compiler version, libdir, and global DB without
opening/creating a user DB. Preserve configured dispatch arguments; do not
implement the query by echoing metadata supplied by Cabal. New compiler and
package-tool protocol support lands together.

Replace new-protocol namespace calculation at every producer and consumer:

| Location | Responsibility |
| --- | --- |
| `libraries/ghc-boot/GHC/UniqueSubdir.hs` | Shared new namespace rule and preserved legacy rule. |
| `compiler/GHC/Driver/DynFlags.hs` | Versioned file and application directories. |
| `compiler/GHC/Unit/State.hs` | Default user package DB lookup. |
| `compiler/GHC.hs` | User/local package environment discovery. |
| `utils/ghc-pkg/Main.hs` | Selected target settings and user package DB calculation. |

Add T1, T3, and the GHC side of T5. Include machine triples with a fourth
environment field, unknown/unsafe logical names, old settings, and native
metadata. Exit when producer serialization and consumer queries agree and no
qualified name reaches C toolchain inputs.

Cover R6 at the configure boundary as part of the same identity flow. A Host
package defaults configure probes to `--host=<Host M>` and
`--build=<Build M>`; a Build package defaults both to its Build machine
triple. Preserve each complete configured triple, including environment
components. Explicit `--host` and `--build` values override their respective
inferred defaults. Exercise default and custom Setup hook paths, plus the
serializable external-Setup route that invokes configure. Reject `M-prof`
there with migration guidance to use machine-only `M` and choose the Haskell
ABI separately.

Before merging or serializing configure defaults, reconcile only the exact
legacy bare Linux versus otherwise identical Linux GNU alias pair. Autoconf
compares raw aliases before native probes, including in packages without
`config.sub`. For two inferred aliases use the full GNU spelling; for one
explicit option preserve its literal and align only the inferred counterpart.
Use the last explicit value when options repeat, retain all explicit arguments
in order, and leave both-explicit pairs untouched. Share this policy between
client injection and Setup-only defaults. Keep machine metadata and target
identity unchanged. Add a process-like no-`config.sub` `AC_RUN_IFELSE`
regression for native probing, both mixed-explicit directions, and explicit
cross-mode preservation, plus GNU/musl and ARM soft/hard-float negatives.

## Phase 2 Centralize Cabal identity

### Library API and metadata ingestion

Add the small validated target-name abstraction and shared accessors in
`Cabal/src/Distribution/Simple/Compiler.hs` or a nearby focused module.
`Cabal/src/Distribution/Simple/GHC.hs` validates GHC metadata once at compiler
configuration. `GHC/Internal.hs` continues parsing platform facts from the
machine target; it also implements the new versus legacy namespace selection
using the shared API.

Keep `Cabal-syntax`'s `Platform`, `CompilerId`, and compiler-build `AbiTag`
representations intact. Remove profiling-only tag concatenation only when the
new hash input is in place. Carry validated target properties into configured
program entries and pair those entries with their correct Build/Host compiler.
Audit `InLibrary` without reverting its corrected selected-toolchain handling.

In `Simple.GHC.compilerProgramDb`, validate `ghc-pkg --info` against the
selected compiler when `Target name` is present. Check target name, machine
target, semantic ABI, version, and resolved libdir/global DB paths. Cover
explicit `--with-hc-pkg` as well as inferred tools and both stages. Missing
query support on this path is an error; the old compiler path retains its
existing version-only validation.

### Identity propagation

| File or area | Required change |
| --- | --- |
| `cabal-install/.../PackageHash.hs` | Add an explicit labelled full target-name input, retain compiler ABI input, update serialization and tests. |
| `cabal-install/.../ProjectPlanning.hs` | Supply that value from the package's selected stage toolchain. |
| `cabal-install/.../DistDirLayout.hs` | Replace `betterPlatform` concatenation with the canonical target API; audit store, dist, DB, and lock consumers. |
| `Cabal/.../GHC.hs` and `GHC/Internal.hs` | Align user DB, pkgRoot, and package environment rules with the selected compiler's protocol. |
| `cabal-install/.../CmdInstall.hs` | Use the same namespace in install `--lib` environment selection and writing. |
| `cabal-install/.../ProjectPlanOutput.hs` | Add target-name identity while preserving existing platform fields. |
| Plan/toolchain cache schemas and fixtures | Invalidate stale serialized values; preserve scoped option origins. |

Here `cabal-install/.../` abbreviates
`cabal-install/src/Distribution/Client/`, and `Cabal/.../` abbreviates
`Cabal/src/Distribution/Simple/`.

Add T2 through T5 using synthetic compiler metadata. Independently hold
`Platform`, compiler version, and package source fixed while varying machine
environment, profiling qualifier, and compiler-build ABI. Check both equality
for canonical aliases and inequality for incompatible targets. Test actual
hash rendering, path consumers, JSON, and cache round trips; helper-only tests
are insufficient. Exit when no identity consumer constructs its own suffix.

## Phase 3 Wire profiling targets through GHC

Rename Makefile `PROFILING_TARGET` from `$(HOST_PLATFORM)-profiling` to
`$(HOST_PLATFORM)-prof`. Retain machine `TRIPLE`, semantic ABI `profiling`,
native tools, and current static capability configuration. Add canonical
target metadata to settings rather than passing a qualified name to
`ghc-toolchain --triple`.

Audit executable/target dispatch, `compiler/GHC/SysTools/BaseDir.hs`,
`utils/runghc/Main.hs`, `utils/ghc-pkg/Main.hs`, bindist manifests, shell
wrappers, and Makefile install/relocation rules. Generate short aliases only
when unambiguous. Test versioned aliases and a moved installation with spaces
in its path. Target helpers must use the profiling package tree; unavailable
helpers must fail clearly.

Retain intrinsic profiling code generation in `GHC.Platform.Profile`,
`Driver.Session`, Cmm/code generation, RTS interfaces, and foreign compilation
paths. Keep interface ABI rejection independent of ordinary filename ways.
Preserve explicit native plugin host-toolchain configuration and target
external interpreter selection in `Driver.Env` and related interpreter
configuration. Verify that target wired-in package IDs come from the target
database rather than the running compiler's native IDs.

Add or revise T6 through T8 and T11. Exit when direct native and profiling
compilation, foreign sources, target TH, native plugins, and both directions
of interface rejection work from the same candidate installation.

## Phase 4 Thin Cabal profiling policy and delete old ways

Keep profiling-specific behaviour in capability/instrumentation helpers,
`Simple.Configure`, `Simple.GHC` option construction, the selected-toolchain
adapter, and configuration-scope handling. Reuse existing solver stages and
toolchain selection. Audit same-platform predicates to distinguish package
identity from local executability; document each changed predicate's purpose.

Preserve install-local option provenance and
`normaliseUnscopedPackageConfig`'s intended behaviour. Cover all six existing
profiling enablement/detail fields as one scoped policy. Explicit package and
Build-stage overrides remain explicit; invalid native profiling requests
produce errors. Keep profiling detail and package `ghc-prof-options` effective
on profiling targets.

Remove automatic profiling ways across GHC, RTS/Hadrian, Cabal GHC/GHCJS
backends, paths/names, documentation, and test harnesses. Public compatibility
configuration fields may remain only as a thin adapter or migration diagnostic;
they must not generate retired artifacts. Update misleading comments and
remove resolved agent review markers. Do not remove unrelated dynamic ways.

Search for `WayProf`, `ProfWay`, profiling suffix constructors, `.p_hi`,
`.p_o`, `_p.a`, and combined dynamic/profiling names. Classify residual matches
as regression fixtures, diagnostics, or unrelated identifiers. A blanket grep
with zero matches is not the acceptance test.

Add T9 and T10 with real client and Setup paths, including `build`, `install`,
and install `--lib`. Check installed files and archive members rather than
only dry-run flags. Exit when native migration errors, profiling ordinary
filenames, instrumentation choices, and stage scoping all agree.

Implement the spec's capability distinction explicitly: unavailable shared or
debug output fails when requested by a user flag/project setting; automatic
defaults choose supported static nondebug output. Remove the current warning
and fallback for explicit profiling-shared requests. Preserve provenance long
enough to distinguish explicit requests from defaults, and test both outcomes
through GHC, Cabal client, and Setup configuration.

## Phase 5 Qualify one integrated candidate

Pin the final Cabal revision in GHC build/project declarations and record both
full commit IDs. Reuse earlier results only when their source, command,
dependencies, and environment remain unchanged. The new metadata and namespace
protocol requires a fresh installed workflow even though the earlier branch
passed profiling execution.

Run checks in increasing cost order:

1. Target serialization/metadata unit tests and Cabal hash/path/cache tests.
2. Focused compiler/interface and Cabal CLI/Setup regressions on the selected
   native/profiling pair.
3. Native and existing Wasm cross controls, including platform conditions and
   scoped Build dependencies.
4. Fresh profiling build and bindist install; then direct/profile threaded
   execution, TH with C/C++, hsc2hs, native plugin, and profile report checks.
5. Relocate the installation and run a separate consumer project, install
   `--lib` environment workflow, database isolation negatives, and testsuite
   archive smoke tests.

Use existing project commands, including `nix develop -c make stable-cabal`,
`stage3-profile`, `test-profile`, and `test-profile-install`, after checking
their current dependency wiring. Choose the focused Cabal test executable and
fixture selectors from the checked-out test driver; record exact commands,
not guessed invocations. Bound heavyweight compiler runs by a wall-clock
timeout and stream logs to files. Reassess a build that makes no progress for
10 to 15 minutes; do not kill unrelated user processes.

The supported Darwin and Linux runtime claims each require their own actual
workflow result. ARM identity tests run without ARM hardware; ARM runtime and
Windows runtime claims require their own runners before being advertised.
Capability skips for unavailable shared/debug variants stay explicit.

## Test placement and review focus

| Tests | Preferred existing test area | Review question |
| --- | --- | --- |
| T1, T3 | ghc-toolchain tests plus driver/settings fixtures | Is the machine environment retained before lossy parsing, and can invalid metadata reach tools? |
| T2, T5 | Driver/package environment tests and installed profiling workflow | Do all programs resolve aliases and namespaces identically? |
| T4 | Cabal/cabal-install unit tests for compiler properties, package hashes, layouts, and plan output | Can incompatible targets collide despite equal `Platform`? |
| T6, T10 | Existing Cabal profiling target, Custom Setup, build-tool, and install-local integration fixtures | Are option origin and stage identity preserved through elaboration and cache reload? |
| T7, T8, T9, T11 | Existing GHC profiling target regression fixtures and Cabal compiler fixtures | Are actual ABI semantics and ordinary filenames correct? Does captured configure argv use full stage-specific machine triples and preserve explicit option precedence? |
| T12 | `make test-profile-install` and testsuite archive smoke harness | Does a clean installed/relocated consumer work without build-tree leakage? |
| T13 | Existing native/Wasm cross regression projects | Did general target identity alter unrelated platform behaviour? |
| D1 | Integrated diff and PR boundaries | Is profiling policy thin, with general identity and independent fixes separately reviewable? |

Review metadata/hash/namespace semantics before the broad build, and review
the final integrated diff after focused fixes. Keep authors responsible for
their fixes. Request independent review at the consequential ABI and identity
boundaries; commit count is not a review trigger. Specification review does
not substitute for source review or runtime validation.

## Documentation and delivery

Update GHC README profiling examples, `docs/users_guide/profiling.rst`, build
target help, bindist instructions, Cabal profiling/cross-toolchain guidance,
and migration diagnostics. Explain short aliases, canonical `Target name`,
machine-only `Target platform`, fresh native namespaces, legacy metadata
fallback, and the matching Cabal requirement. State that `-prof` on the
profiling compiler is redundant and that selecting another compiler changes
the ABI. Do not describe ARM identity fixtures as an ARM backend implementation.

Before proposing PR publication, prepare clean commits and descriptions for
the actual bases, with final behaviour, dependencies, exact focused commands,
and platform limits. Preserve user changes and keep agent attribution out of
commit trailers. Review current permission before pushing replacement
branches; never infer permission for a force push or merge from this plan.

Track progress in the existing profiling work status record. Delivery is the
integrated candidate plus T1 through T13 results, documented migration, and
the source review D1. Independent store/HPC fixes keep their own acceptance
criteria. Outstanding unrelated CI failures remain identified separately.

## Design review

Astra reviewed both drafts against the pinned GHC and Cabal source on
2026-10-20. The first review found two P2 specification gaps: compiler/package
tool identity validation was not enforceable, and unsupported shared/debug
fallback was undefined. The revised specification adds an independent
`ghc-pkg --info` handshake and explicit-request errors versus supported
automatic defaults. Phases 1, 2, and 4 and tests T5/T9 cover those changes.

Astra's focused follow-up review closed both findings and found no remaining
consequential design defect or unresolved central contract in this scope.
This is design review; implementation and runtime qualification remain the
work described above.

Astra's focused R6/T11 review on 2026-10-21 found no design blockers for the
stage-specific machine triples, explicit configure-option precedence,
`-prof` rejection at the C boundary, or Setup/configuration serialization
paths. Implementation and captured-configure-argv regressions remain separate
verification work.

A subsequent Linux build exposed Autoconf's raw-alias comparison before native
probes. Astra approved the bounded configure-only Linux/GNU reconciliation
above and its no-`config.sub` runtime-probe regression, with explicit argument
provenance and all target identity fields preserved.
