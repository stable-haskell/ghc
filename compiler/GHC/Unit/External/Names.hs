-- | Resolving package names in command-line flags to unit ids.
--
-- See Note [Names are resolved in the front end].
module GHC.Unit.External.Names
  ( resolvePackageFlags
  , resolveTrustFlags
  , resolveIgnoreFlags
  ) where

import GHC.Prelude

import GHC.Data.Maybe
import GHC.Driver.DynFlags
import GHC.Types.Unique.Map
import GHC.Unit.External.Database (UnitPrecedenceMap, sortByPreference)
import GHC.Unit.External.Validate
import GHC.Unit.Info
import GHC.Unit.Types

{-
Note [Names are resolved in the front end]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Several flags select installed units by *package name*: -package foo,
-hide-package foo, -ignore-package foo, -trust foo, -distrust foo (and
-plugin-package foo). The unit core (visibility, trust, validation, the
module map, linking) does not want to know about names: a name is a label
on a unit, and which unit a name denotes is a question for whoever assembled
the set of installed units, not for the compiler. Internally everything is
keyed by 'UnitId', and -package-id, which Cabal and environment files use
exclusively, already speaks that language.

So names are resolved to unit ids at one boundary, in 'mkUnitState', right
after the databases have been read and merged and before any flag is
applied. This module is that boundary. After it runs, every flag carries a
'UnitIdArg', and the arms of the core that would match a 'PackageArg' are
unreachable (they panic, see 'GHC.Unit.External.Validate.nameReachedCore').

The resolution reproduces exactly what the core used to do with names, so
that behaviour and error messages are unchanged:

  * -package foo / -plugin-package foo: of the usable units whose name or
    name-version is foo, pick the most preferable one (latest version, then
    latest database on the stack; 'sortByPreference'); exactly one flag
    results. If none is usable, fail with 'PackageFlagErr' naming the
    original flag and the unusable candidates, as 'applyPackageFlag' did.

  * -hide-package foo: every usable unit matching foo is hidden; one
    'HidePackage' per unit. Same failure as above when nothing matches.

  * -trust foo / -distrust foo: every usable unit matching foo; one flag
    per unit; 'TrustFlagErr' when nothing matches.

  * -ignore-package foo: every unit of the *merged, unvalidated* map
    matching foo, because ignoring happens inside 'validateDatabase'. A
    name that matches nothing is silently dropped: -ignore-package P is
    commonly used as a precaution in case P exists.

A flag that already carries a unit id passes through untouched.

The flag text as typed (the first field of 'ExposePackage', e.g.
"-package foo") is kept and is what messages such as -Wunused-packages
print, so users still see the flag they wrote.

This is stage 1 of docs/unit-artifact-ids-only.md: the core becomes
ids-only while every flag keeps working. Later stages move the resolution
out of the compiler altogether.
-}

-- | Resolve the names in @-package@, @-plugin-package@ and @-hide-package@
-- flags against the usable units. Flags are in command-line order.
resolvePackageFlags
  :: UnitPrecedenceMap
  -> UnusableUnits      -- ^ for error messages
  -> [UnitInfo]         -- ^ the usable units
  -> [PackageFlag]
  -> MaybeErr UnitErr [PackageFlag]
resolvePackageFlags prec_map unusable pkgs = fmap concat . mapM resolve
  where
    resolve flag = case flag of
      ExposePackage doc (PackageArg str) rn ->
        case sortByPreference prec_map (byName str) of
          []    -> Failed (PackageFlagErr flag (unusableByName str))
          p : _ -> Succeeded [ExposePackage doc (byId p) rn]
      HidePackage (PackageArg str) ->
        case byName str of
          []  -> Failed (PackageFlagErr flag (unusableByName str))
          ps  -> Succeeded [HidePackage (byId p) | p <- ps]
      _ -> Succeeded [flag]

    byName str = filter (matchingStr str) pkgs
    unusableByName str = filter (matchingStr str . fst) (nonDetEltsUniqMap unusable)

-- | Resolve the names in @-trust@ and @-distrust@ flags against the usable
-- units. Flags are in command-line order.
resolveTrustFlags
  :: UnitPrecedenceMap
  -> UnusableUnits
  -> [UnitInfo]
  -> [TrustFlag]
  -> MaybeErr UnitErr [TrustFlag]
resolveTrustFlags prec_map unusable pkgs = fmap concat . mapM resolve
  where
    resolve flag = case flag of
      TrustPackage    (PackageArg str) -> each str flag TrustPackage
      DistrustPackage (PackageArg str) -> each str flag DistrustPackage
      _ -> Succeeded [flag]

    each str flag mk = case filter (matchingStr str) pkgs of
      [] -> Failed (TrustFlagErr flag (filter (matchingStr str . fst) (nonDetEltsUniqMap unusable)))
      ps -> Succeeded [mk (byId p) | p <- sortByPreference prec_map ps]

-- | Resolve the names in @-ignore-package@ flags against the merged,
-- unvalidated unit map. Names that match nothing are dropped.
resolveIgnoreFlags :: UnitInfoMap -> [IgnorePackageFlag] -> [IgnorePackageFlag]
resolveIgnoreFlags pkg_map = concatMap resolve
  where
    resolve flag = case flag of
      IgnorePackage (PackageArg str) ->
        [ IgnorePackage (byId p) | p <- nonDetEltsUniqMap pkg_map, matchingStr str p ]
      _ -> [flag]

byId :: UnitInfo -> PackageArg
byId p = UnitIdArg (RealUnit (Definite (unitId p)))
