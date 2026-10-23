module GHC.Unit.External.Query (
  -- * Query the 'UnitInfoMap'
  lookupUnit',
  lookupUnitId',
  -- * References between units
  unitReferencedUnitIds,
  unitInfoReferencedUnits,
  closeOverReferences,
) where

import GHC.Prelude

import GHC.Types.Unique.Map
import GHC.Types.Unique.Set
import GHC.Unit.External.Substitution
import GHC.Unit.Info
import GHC.Unit.Module

-- | A more specialized interface, which doesn't require a 'UnitState' (so it
-- can be used while we're initializing 'DynFlags')
--
-- Parameters:
--    * a boolean specifying whether or not to look for on-the-fly renamed interfaces
--    * a 'UnitInfoMap'
lookupUnit' :: Bool -> UnitInfoMap -> Unit -> Maybe UnitInfo
lookupUnit' allowOnTheFlyInst pkg_map u = case u of
   HoleUnit   -> error "Hole unit"
   RealUnit i -> lookupUniqMap pkg_map (unDefinite i)
   VirtUnit i
      | allowOnTheFlyInst
      -> -- lookup UnitInfo of the indefinite unit to be instantiated and
         -- instantiate it on-the-fly
         fmap (renameUnitInfo pkg_map (instUnitInsts i))
           (lookupUniqMap pkg_map (instUnitInstanceOf i))

      | otherwise
      -> -- lookup UnitInfo by virtual UnitId. This is used to find indefinite
         -- units. Even if they are real, installed units, they can't use the
         -- `RealUnit` constructor (it is reserved for definite units) so we use
         -- the `VirtUnit` constructor.
         lookupUniqMap pkg_map (virtualUnitId i)


-- | Find the unit we know about with the given unit id, if any
lookupUnitId' :: UnitInfoMap -> UnitId -> Maybe UnitInfo
lookupUnitId' db uid = lookupUniqMap db uid

-- -----------------------------------------------------------------------------
-- References between units

-- | The unit ids a 'Unit' is made of: itself for a definite unit, and for an
-- instantiated unit the indefinite unit plus the units of its instantiation,
-- recursively. These are the units that must be available for the 'Unit'
-- to be usable.
unitReferencedUnitIds :: Unit -> [UnitId]
unitReferencedUnitIds u = case u of
  HoleUnit                 -> []
  RealUnit (Definite uid)  -> [uid]
  VirtUnit i               -> instUnitInstanceOf i
                            : concatMap (unitReferencedUnitIds . moduleUnit . snd) (instUnitInsts i)

-- | Every unit a 'UnitInfo' refers to and that therefore has to be present
-- for the unit to be usable: its dependencies, the origins of its reexported
-- modules, and the units in its instantiation.
unitInfoReferencedUnits :: UnitInfo -> [UnitId]
unitInfoReferencedUnits ui =
  unitDepends ui
  ++ concat [ unitReferencedUnitIds (moduleUnit m) | (_, Just m) <- unitExposedModules ui ]
  ++ concatMap (unitReferencedUnitIds . moduleUnit . snd) (unitInstantiations ui)

-- | Restrict a 'UnitInfoMap' to the units reachable from the given roots
-- through 'unitInfoReferencedUnits'. Roots and references that are not in
-- the map are left out silently: whether a missing unit is an error is for
-- the caller to decide ('validateDatabase' reports the units it breaks).
closeOverReferences :: [UnitId] -> UnitInfoMap -> UnitInfoMap
closeOverReferences roots pkg_map = go emptyUniqSet emptyUniqMap roots
  where
    go _    acc []           = acc
    go seen acc (uid : todo)
      | uid `elementOfUniqSet` seen = go seen acc todo
      | otherwise = case lookupUniqMap pkg_map uid of
          Nothing -> go seen' acc todo
          Just ui -> go seen' (addToUniqMap acc uid ui) (unitInfoReferencedUnits ui ++ todo)
      where seen' = addOneToUniqSet seen uid
