-- | The 'UnitIndex' is a 'UnitEnv' wide data structure that shares
-- external unit information across the 'UnitState' of all home units
-- (e.g., 'HomeUnitEnv') in a particular 'UnitEnv'.
--
-- It caches already read unit databases and all processed 'UnitInfo's.
--
-- This module is meant to be imported as @Index@.
--
-- A short overview of how the different types here related to 'UnitState', 'UnitEnv'
-- and the 'HomeUnitEnv'.
--
-- ┌─────────┐
-- │ UnitEnv │
-- └────┬────┘
--      ├───────────────────────┐
--      │                       │
-- ┌────▼──────┐          ┌─────▼─────┐
-- │HomeUnitEnv│          │ UnitIndex ├────────────────┐
-- └────┬──────┘          └───────────┘                │
--      │                                              │
--      │      Reads cached unit DBs                   │
-- ┌────▼──────┐          ┌─────────────────────┐      │
-- │ UnitState ├──────────>ExternalUnitDatabases◄──────┤
-- └────┬──┬───┘          └─────────────────────┘      │
--      │  └───────────────────────┐                   │
--      │   Writes new UnitInfos   │                   │
--      │   during initialisation  │                   │
-- ┌────▼────────┐        ┌────────v──────────┐        │
-- │ UnitInfoMap │        │ GlobalUnitInfoMap ◄────────┘
-- └────┬────────┘        └────────^──────────┘
--      │                          │
--      └──────────────────────────┘
--          UnitInfoMap references
--          GlobalUnitInfoMap values
--          (All UnitInfos are shared)
--
-- Open arrow @A ───> B@: A uses B.
-- Closed arrow @A ◄─── B@: A is a field of B.
--
-- Also, see Note [Sharing 'UnitInfo's across the 'UnitEnv'] for more technical discussion
-- about sharing 'UnitInfo's.
module GHC.Unit.External.Index (
  -- * The 'UnitIndexCache'.
  -- A mutable wrapper around 'UnitIndex'
  UnitIndexCache(..),
  initUnitIndexCache,
  readUnitIndex,
  modifyUnitIndexCache,
  clearUnitIndexCache,
  cacheExternalUnitDatabase,
  readExternalUnitDatabases,
  readExternalUnitDatabase,
  -- * 'UnitIndex'
  UnitIndex,
  emptyUnitIndex,
  globalUnits,
  externalUnitDatabases,
  addUnitInfoMap,
  -- * 'GlobalUnitInfoMap'
  GlobalUnitInfoMap,
  lookupGlobalUnitInfoMap,
  emptyGlobalUnitInfoMap,
  mkGlobalUnitInfoMap,
  -- * 'GlobalUnitKey'
  GlobalUnitKey,
  UnitAbiHash,
  mkGlobalUnitKey,
  globalUnitKeyFromUnitInfo,
  -- * Sharing 'UnitInfo's
  shareUnitInfos,
  -- * Reading external unit databases into the 'UnitIndexCache'
  readOrGetUnitDatabase,
  readUnitDatabases,
) where

import GHC.Prelude

import GHC.Data.OsPath
import GHC.Data.ShortText qualified as ST
import GHC.Data.Maybe (expectJust)
import GHC.Types.Unique.Map
import GHC.Types.Unique.Set
import GHC.Unit.Database
import GHC.Unit.External.Database
import GHC.Unit.External.Query (unitInfoReferencedUnits)
import GHC.Unit.External.Store (readUnitStore, probeUnitStore)
import GHC.Unit.Info
import GHC.Unit.Types
import GHC.Utils.Logger

import Control.Monad (liftM, forM, foldM)
import Data.Either
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, maybeToList)

-- ----------------------------------------------------------------------------
-- UnitIndex
-- ----------------------------------------------------------------------------

-- | Mutable version of 'UnitIndex'.
--
-- The 'UnitIndexCache' ensures that all calls to 'initUnits' will
-- share the 'UnitInfo' if it is possible.
--
-- To share the 'UnitInfo', the 'UnitInfo' needs to be fully-resolved, i.e., the
-- variables in its paths (@${pkgroot}@) replaced by their values.
--
-- Memory Invariant: The 'UnitIndexCache' is the root object for retaining fully-resolved
-- 'UnitInfo'. 'UnitState' is expected to reference only 'UnitInfo's from the 'UnitIndexCache'.
-- There is exactly one fully-resolved 'UnitInfo' alive for each external unit per unit database.
--
-- A second instance may or may not be stored in the 'externalUnitDatabases', which represent the
-- in-memory cache of the on-disk unit databases.
newtype UnitIndexCache = UnitIndexCache
  { uic_index :: IORef UnitIndex
  }

initUnitIndexCache :: IO UnitIndexCache
initUnitIndexCache =
  UnitIndexCache <$> IORef.newIORef emptyUnitIndex

readUnitIndex :: UnitIndexCache -> IO UnitIndex
readUnitIndex uic =
  IORef.readIORef (uic_index uic)

modifyUnitIndexCache :: UnitIndexCache -> (UnitIndex -> UnitIndex) -> IO ()
modifyUnitIndexCache uic f =
  IORef.modifyIORef' (uic_index uic) f

clearUnitIndexCache :: UnitIndexCache -> IO ()
clearUnitIndexCache uic =
  modifyUnitIndexCache uic (const emptyUnitIndex)

cacheExternalUnitDatabase :: UnitIndexCache -> UnitDatabase UnitId -> IO ()
cacheExternalUnitDatabase uic db =
  modifyUnitIndexCache uic
    (\ ui ->
      ui
        { ui_externalUnitDatabases = insertExternalUnitDatabases db (ui_externalUnitDatabases ui)
        }
    )

readExternalUnitDatabases :: UnitIndexCache -> IO (ExternalUnitDatabases UnitId)
readExternalUnitDatabases uic =
  externalUnitDatabases <$> readUnitIndex uic

readExternalUnitDatabase :: UnitIndexCache -> OsPath -> IO (Maybe (UnitDatabase UnitId))
readExternalUnitDatabase uic path = do
  dbs <- readExternalUnitDatabases uic
  pure $ lookupExternalUnitDatabases path dbs

-- | Global index for external units that can be shared across multiple 'HomeUnitEnv's.
--
-- Allows sharing of the 'UnitInfo's that are stored in the 'UnitState'
-- of each 'HomeUnitEnv'.
--
-- See Note [Sharing 'UnitInfo's across the 'UnitEnv'] for details about memory usage.
data UnitIndex = UnitIndex
  { ui_unitInfoMap :: !GlobalUnitInfoMap
    -- ^ A global map for all fully-resolved 'UnitInfo's.
    --
    -- See Note [Sharing 'UnitInfo's across the 'UnitEnv'] for more details
    -- what we use this for and what a fully-resolved 'UnitInfo' is.
  , ui_externalUnitDatabases :: !(ExternalUnitDatabases UnitId)
    -- ^ Cache the already processed unit databases in-memory.
    --
    -- These 'GenericUnitInfo's have their paths resolved, e.g., no @${pkgroot}@ is
    -- present any more.
  }

-- | Access the already processed unit databases.
externalUnitDatabases :: UnitIndex -> ExternalUnitDatabases UnitId
externalUnitDatabases = ui_externalUnitDatabases

-- | Access the global map of fully-resolved 'UnitInfo's.
--
-- A 'UnitInfo' is fully-resolved when it is based on the 'ExternalUnitDatabases'
-- results, resolving variables such as @${pkgroot}@ in paths.
--
-- See Note [Sharing 'UnitInfo's across the 'UnitEnv'] for why this is helpful.
globalUnits :: UnitIndex -> GlobalUnitInfoMap
globalUnits = ui_unitInfoMap

emptyUnitIndex :: UnitIndex
emptyUnitIndex = UnitIndex
  { ui_unitInfoMap = emptyGlobalUnitInfoMap
  , ui_externalUnitDatabases = emptyExternalUnitDatabases
  }

addUnitInfoMap :: UnitInfoMap -> UnitIndex -> UnitIndex
addUnitInfoMap unit_info_map unit_index =
  unit_index
    { ui_unitInfoMap =
        -- Order should not matter, either it is exactly the same 'UnitInfo',
        -- or a new one.
        GlobalUnitInfoMap $ plusUniqMap_C Map.union newEntriesMap oldMap
    }
  where
    GlobalUnitInfoMap newEntriesMap = mkGlobalUnitInfoMap $ nonDetUniqMapToList unit_info_map
    GlobalUnitInfoMap oldMap = ui_unitInfoMap unit_index

-- ----------------------------------------------------------------------------
-- GlobalUnitInfoMap
-- ----------------------------------------------------------------------------

type UnitAbiHash = ST.ShortText

-- | Like a 'UnitInfoMap' but stores all 'UnitInfo's.
--
-- It is keyed by the 'UnitId' and the 'UnitInfo's 'UnitAbiHash'.
-- In modern cabal, there should never be a conflict of 'UnitId's, as cabal
-- hashes the Abi, dependency hashes, source hashes and more.
--
-- However, a user can choose a conflicting 'UnitId', causing a conflict after all.
-- We use the 'UnitAbiHash' for disambiguation. If both 'UnitId' and 'UnitAbiHash' are
-- identical in separate unit databases, we can assume they are the same unit, according
-- to the documentation of GHC.
newtype GlobalUnitInfoMap = GlobalUnitInfoMap (UniqMap UnitId (Map UnitAbiHash UnitInfo))

-- | Lookup the 'UnitInfo' in the 'GlobalUnitInfoMap'.
lookupGlobalUnitInfoMap :: GlobalUnitKey -> GlobalUnitInfoMap -> Maybe UnitInfo
lookupGlobalUnitInfoMap (GlobalUnitKey uid abiHash) (GlobalUnitInfoMap globalMap) =
  case lookupUniqMap globalMap uid of
    Nothing -> Nothing
    Just sameUnitId -> Map.lookup abiHash sameUnitId

mkGlobalUnitInfoMap :: [(UnitId, UnitInfo)] -> GlobalUnitInfoMap
mkGlobalUnitInfoMap unitInfos =
  GlobalUnitInfoMap $ listToUniqMap_C Map.union (map mkEntry unitInfos)
 where
  mkEntry (uid, v) = (uid, Map.singleton (unitAbiHash v) v)

emptyGlobalUnitInfoMap :: GlobalUnitInfoMap
emptyGlobalUnitInfoMap = GlobalUnitInfoMap emptyUniqMap

-- ----------------------------------------------------------------------------
-- GlobalUnitKey
-- ----------------------------------------------------------------------------

-- | A 'GlobalUnitKey' is a key that can globally identify a 'UnitInfo', not just
-- in the 'UnitInfoMap'.
--
-- The 'UnitId' and 'UnitAbiHash' uniquely identify a 'UnitInfo'.
data GlobalUnitKey =
  GlobalUnitKey
    !UnitId -- ^ Unit Id of the 'UnitInfo'
    !UnitAbiHash -- ^ ABI hash of the 'UnitInfo'

globalUnitKeyFromUnitInfo :: UnitInfo -> GlobalUnitKey
globalUnitKeyFromUnitInfo ui = mkGlobalUnitKey (unitId ui) (unitAbiHash ui)

mkGlobalUnitKey :: UnitId -> UnitAbiHash -> GlobalUnitKey
mkGlobalUnitKey = GlobalUnitKey

-- -----------------------------------------------------------------------------
-- Sharing 'UnitInfo's

-- | Share the given 'UnitInfo's through the 'UnitIndexCache': a unit that is
-- already in the cache (same unit id and ABI hash) is replaced by the cached
-- copy, a new one is forced and cached. The result is in the same order as
-- the input. See Note [Sharing 'UnitInfo's across the 'UnitEnv'] in
-- GHC.Unit.State.
shareUnitInfos :: [UnitInfo] -> UnitIndexCache -> IO [UnitInfo]
shareUnitInfos pkgs unit_index = do
  ui <- readUnitIndex unit_index
  let
    known = ui_unitInfoMap ui
    share pkg = case lookupGlobalUnitInfoMap (globalUnitKeyFromUnitInfo pkg) known of
      Just cached -> Right cached
      Nothing     -> Left pkg
    all_pkgs = map share pkgs
    (new_pkgs', _cached) = partitionEithers all_pkgs
  -- Make sure we force the 'UnitInfo' here.
  -- Otherwise, we will retain a reference to the old 'UnitInfo'
  new_pkgs <- traverse evaluateUnitInfoLists new_pkgs'
  modifyUnitIndexCache unit_index (addUnitInfoMap $ mkUnitInfoMap new_pkgs)
  pure (map (either id id) all_pkgs)

-- -----------------------------------------------------------------------------
-- Reading the unit database(s) into the 'UnitIndexCache'

readUnitDatabases :: Logger -> UnitIndexCache -> UnitDbConfig -> IO [UnitDatabase UnitId]
readUnitDatabases logger db_cache cfg = do
  conf_refs <- getUnitDbRefs cfg
  locs      <- liftM catMaybes $ mapM (resolveUnitDatabase cfg) conf_refs
  case unitDbConfigDemandRoots cfg of
    Nothing    -> mapM (readOrGetUnitDatabase logger db_cache cfg) locs
    Just roots -> readUnitDatabasesOnDemand logger db_cache cfg roots locs

-- | Get the cached 'UnitDatabase' or read the 'UnitDatabase' at the given
-- location, which is either a package database or a unit store.
--
-- A cached database that was only partially read on demand is not good
-- enough here and is read again in full.
readOrGetUnitDatabase :: Logger -> UnitIndexCache -> UnitDbConfig -> UnitDbLocation -> IO (UnitDatabase UnitId)
readOrGetUnitDatabase logger db_cache cfg (UnitDbLocation kind path) =
  readExternalUnitDatabase db_cache path >>= \ case
    Just db | not (unitDatabasePartial db) ->
      pure db
    _ -> do
      new_db <- case kind of
        PackageDbKind -> readUnitDatabase logger cfg path
        UnitStoreKind -> readUnitStore logger path
      cacheExternalUnitDatabase db_cache new_db
      pure new_db

-- | Read the package databases in full and the unit stores on demand.
--
-- Starting from the given root unit ids, every store on the stack is probed
-- for each unit, and the units found (in stores or in the databases) are
-- followed through 'unitInfoReferencedUnits' until nothing new is reached.
-- No store is ever enumerated. The result has one 'UnitDatabase' per
-- location in stack order; those of stores are marked partial and hold only
-- the units reached.
--
-- Partial store databases already in the cache are extended rather than
-- rebuilt, so that a later 'initUnits' with more flags (GHCi's @:set@)
-- only probes for what is new. A store whose database was already read in
-- full is used as is, with no probing.
readUnitDatabasesOnDemand
  :: Logger -> UnitIndexCache -> UnitDbConfig -> [UnitId] -> [UnitDbLocation]
  -> IO [UnitDatabase UnitId]
readUnitDatabasesOnDemand logger db_cache cfg roots locs = do
  -- 1. Package databases, eagerly, as always.
  dbs <- forM locs $ \ loc@(UnitDbLocation kind path) -> case kind of
    PackageDbKind -> Left <$> readOrGetUnitDatabase logger db_cache cfg loc
    UnitStoreKind -> Right . (\ cached -> (path, cached)) <$> readExternalUnitDatabase db_cache path

  let db_units :: UniqMap UnitId UnitInfo
      db_units = listToUniqMap [ (unitId ui, ui) | Left db <- dbs, ui <- unitDatabaseUnits db ]

      -- Per store: what we already know, and whether that is everything.
      initial_store (path, cached) = case cached of
        Just db -> (path, listToUniqMap [ (unitId ui, ui) | ui <- unitDatabaseUnits db ], not (unitDatabasePartial db))
        Nothing -> (path, emptyUniqMap, False)
      stores0 = [ initial_store st | Right st <- dbs ]

  -- 2. Close the roots over everything they reference, probing the stores.
  let go seen stores [] = pure (seen, stores)
      go seen stores (uid : todo)
        | uid `elementOfUniqSet` seen = go seen stores todo
        | otherwise = do
            (stores', found) <- foldM (probe uid) ([], []) stores
            let found_db = maybeToList (lookupUniqMap db_units uid)
                refs     = concatMap unitInfoReferencedUnits (found ++ found_db)
            go (addOneToUniqSet seen uid) (reverse stores') (refs ++ todo)

      probe uid (acc, found) st@(path, known, complete)
        | Just ui <- lookupUniqMap known uid = pure (st : acc, ui : found)
        | complete = pure (st : acc, found)
        | otherwise = probeUnitStore logger path uid >>= \ case
            Nothing -> pure (st : acc, found)
            Just ui -> pure ((path, addToUniqMap known uid ui, False) : acc, ui : found)

  (_, stores) <- go emptyUniqSet stores0 roots

  -- 3. Cache the (extended) partial store databases and assemble the stack.
  store_dbs <- forM stores $ \ (path, known, complete) -> do
    let db = UnitDatabase path (nonDetEltsUniqMap known) (not complete)
    cacheExternalUnitDatabase db_cache db
    pure (path, db)
  pure [ either id (\ (path, _) -> expectJust (lookup path store_dbs)) e | e <- dbs ]
