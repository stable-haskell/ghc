{-# LANGUAGE LambdaCase #-}

-- | Reading units from a /unit store/.
--
-- A unit store is a directory in which every installed unit is a
-- subdirectory named by its 'UnitId', laid out by convention:
--
-- > <root>/<uid>/unit.meta         the manifest
-- > <root>/<uid>/lib/              interface files and libHS<uid>* libraries
-- > <root>/<uid>/lib/include/      C headers (optional)
--
-- This is the layout @cabal-install@'s store already uses; the only addition
-- is @unit.meta@. Everything that a package database entry says about
-- /where/ things are is derived from the layout, and everything it says about
-- /policy/ (exposed, trusted) is left to the command line and environment
-- files. The manifest carries only what cannot be derived: the module lists,
-- the dependencies, the Backpack instantiation, the human-readable label,
-- and the C-side link configuration.
--
-- See @docs/unit-artifact-model.md@ for the design and the rationale.
module GHC.Unit.External.Store
  ( readUnitStore
  , probeUnitStore
  , unitMetaFileName
    -- * The manifest
  , UnitMeta (..)
  , parseUnitMeta
  , storeUnitInfo
  ) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Data.OsPath (OsPath)
import GHC.Data.OsPath qualified as OsPath
import GHC.Data.ShortText qualified as ST
import GHC.Fingerprint (fingerprintString)
import GHC.Unit.External.Database (UnitDatabase (..))
import GHC.Unit.Info
import GHC.Unit.Parser
import GHC.Unit.Types
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Data.Maybe
import Data.Version (makeVersion, parseVersion, showVersion)
import System.IO (readFile')
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

-- | The manifest file inside each unit directory.
unitMetaFileName :: OsPath
unitMetaFileName = OsPath.unsafeEncodeUtf "unit.meta"

-- | Read every unit of the store rooted at the given directory.
--
-- Subdirectories without a @unit.meta@ are ignored, so a store may share its
-- root with other things (a @package.db@, @incoming/@, ...).
readUnitStore :: Logger -> OsPath -> IO (UnitDatabase UnitId)
readUnitStore logger root = do
  isdir <- OsPath.doesDirectoryExist root
  unless isdir $
    throwGhcExceptionIO $ InstallationError $
      "can't find a unit store at " ++ show root
  entries <- OsPath.getDirectoryContents root
  units <- fmap catMaybes $ forM (sort entries) $ \entry ->
    readStoreUnit root (UnitId (mkFastString (OsPath.unsafeDecodeUtf entry)))
  debugTraceMsg logger 2 $
    text "Read unit store" <+> ppr root <> colon <+> int (length units) <+> text "units"
  pure (UnitDatabase root units False)

-- | Look up one unit in the store rooted at the given directory, by its id,
-- without enumerating the store: one 'doesFileExist' on
-- @<root>/<uid>/unit.meta@ and one read if it is there.
--
-- This is what makes a unit store different from a package database: the
-- compiler can ask for the units it needs instead of reading everything
-- that is installed. See 'GHC.Unit.External.Database.unitDbConfigDemandRoots'.
probeUnitStore :: Logger -> OsPath -> UnitId -> IO (Maybe UnitInfo)
probeUnitStore logger root uid = do
  r <- readStoreUnit root uid
  debugTraceMsg logger 3 $
    text "Probed unit store" <+> ppr root <+> text "for" <+> ppr uid <> colon
      <+> text (maybe "absent" (const "found") r)
  pure r

-- | Read @<root>/<uid>/unit.meta@ if it exists.
readStoreUnit :: OsPath -> UnitId -> IO (Maybe UnitInfo)
readStoreUnit root uid = do
  let dir       = root OsPath.</> OsPath.unsafeEncodeUtf (unpackFS (unitIdFS uid))
      meta_path = dir OsPath.</> unitMetaFileName
  has_meta <- OsPath.doesFileExist meta_path
  if not has_meta
    then pure Nothing
    else do
      txt <- readFile' (OsPath.unsafeDecodeUtf meta_path)
      meta <- case parseUnitMeta txt of
        Left err -> throwGhcExceptionIO $ InstallationError $
                      "malformed " ++ show meta_path ++ ": " ++ err
        Right m  -> pure m
      has_include <- OsPath.doesDirectoryExist (dir OsPath.</> libDirName OsPath.</> includeDirName)
      pure (Just (storeUnitInfo root uid meta txt has_include))


libDirName, includeDirName :: OsPath
libDirName     = OsPath.unsafeEncodeUtf "lib"
includeDirName = OsPath.unsafeEncodeUtf "include"

-- ---------------------------------------------------------------------------
-- The manifest

-- | The contents of a @unit.meta@ file. Every field is optional in the file.
data UnitMeta = UnitMeta
  { um_name             :: Maybe String
  , um_version          :: Maybe Version
  , um_exposedModules   :: [(ModuleName, Maybe Module)]
    -- ^ @M@ or @M from unit:M'@ (a reexport)
  , um_hiddenModules    :: [ModuleName]
  , um_depends          :: [UnitId]
  , um_indefinite       :: Bool
  , um_instanceOf       :: Maybe UnitId
  , um_instantiatedWith :: [(ModuleName, Module)]
    -- C side
  , um_extraLibraries       :: [String]
  , um_extraLibrariesStatic :: [String]
  , um_extraGhciLibraries   :: [String]
  , um_frameworks           :: [String]
  , um_frameworkDirs        :: [String]
  , um_ldOptions            :: [String]
  , um_includes             :: [String]
  }

emptyUnitMeta :: UnitMeta
emptyUnitMeta = UnitMeta
  { um_name = Nothing, um_version = Nothing
  , um_exposedModules = [], um_hiddenModules = []
  , um_depends = [], um_indefinite = False
  , um_instanceOf = Nothing, um_instantiatedWith = []
  , um_extraLibraries = [], um_extraLibrariesStatic = [], um_extraGhciLibraries = []
  , um_frameworks = [], um_frameworkDirs = [], um_ldOptions = [], um_includes = []
  }

-- | Parse the text of a @unit.meta@ file.
--
-- The format is a sequence of @key: value@ lines. A line that starts with
-- whitespace continues the previous value. Blank lines and lines starting
-- with @--@ are ignored. Values are whitespace-separated tokens; a token may
-- be double-quoted to contain whitespace. Unknown keys are ignored so that
-- the format can grow.
parseUnitMeta :: String -> Either String UnitMeta
parseUnitMeta txt = do
  fields <- parseFields (lines txt)
  foldM applyField emptyUnitMeta fields
  where
    applyField m (key, toks) = case key of
      "name"        -> one key toks $ \v -> pure m { um_name = Just v }
      "version"     -> one key toks $ \v -> do ver <- runP "version" parseVersion v
                                               pure m { um_version = Just ver }
      "exposed-modules" -> do mods <- parseExposed toks
                              pure m { um_exposedModules = mods }
      "hidden-modules"  -> do mods <- mapM (runP "module name" parseModuleName) toks
                              pure m { um_hiddenModules = mods }
      "depends"     -> do uids <- mapM (runP "unit id" parseUnitId) toks
                          pure m { um_depends = uids }
      "indefinite"  -> one key toks $ \case
                         "True"  -> pure m { um_indefinite = True }
                         "False" -> pure m { um_indefinite = False }
                         v       -> Left ("indefinite: expected True or False, got " ++ show v)
      "instance-of" -> one key toks $ \v -> do uid <- runP "unit id" parseUnitId v
                                               pure m { um_instanceOf = Just uid }
      "instantiated-with" -> do insts <- mapM (runP "instantiation" parseInst) toks
                                pure m { um_instantiatedWith = insts }
      "extra-libraries"        -> pure m { um_extraLibraries = toks }
      "extra-libraries-static" -> pure m { um_extraLibrariesStatic = toks }
      "extra-ghci-libraries"   -> pure m { um_extraGhciLibraries = toks }
      "frameworks"             -> pure m { um_frameworks = toks }
      "framework-dirs"         -> pure m { um_frameworkDirs = toks }
      "ld-options"             -> pure m { um_ldOptions = toks }
      "includes"               -> pure m { um_includes = toks }
      _ -> pure m  -- unknown keys are ignored

    one key toks k = case toks of
      [v] -> k v
      _   -> Left (key ++ ": expected exactly one value")

    -- @M@ or @M from unit:M'@
    parseExposed [] = pure []
    parseExposed (m : "from" : origin : rest) = do
      mn  <- runP "module name" parseModuleName m
      org <- runP "module" parseHoleyModule origin
      ((mn, Just org) :) <$> parseExposed rest
    parseExposed (m : rest) = do
      mn <- runP "module name" parseModuleName m
      ((mn, Nothing) :) <$> parseExposed rest

    -- @Hole=unit:Mod@ or @Hole=<Hole>@
    parseInst = do
      k <- parseModuleName
      _ <- P.char '='
      v <- parseHoleyModule
      pure (k, v)

-- | Run a 'ReadP' parser that must consume the whole token.
runP :: String -> ReadP a -> String -> Either String a
runP what p s = case [ x | (x, "") <- P.readP_to_S (p <* P.eof) s ] of
  (x : _) -> Right x
  []      -> Left ("cannot parse " ++ what ++ ": " ++ show s)

-- | Group lines into @(key, tokens)@ pairs, handling continuation lines.
parseFields :: [String] -> Either String [(String, [String])]
parseFields = go []
  where
    go acc [] = Right (reverse (map finish acc))
    go acc (l : ls)
      | all isSpace l                  = go acc ls
      | take 2 (dropWhile isSpace l) == "--" = go acc ls
      | c : _ <- l, isSpace c = case acc of
          (k, v) : acc' -> go ((k, v ++ " " ++ l) : acc') ls
          []            -> Left ("continuation line with no key: " ++ show l)
      | otherwise = case break (== ':') l of
          (k, ':' : v) -> go ((trim k, v) : acc) ls
          _            -> Left ("expected `key: value`, got " ++ show l)
    finish (k, v) = (k, tokenise v)
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Split on whitespace, keeping double-quoted strings together.
tokenise :: String -> [String]
tokenise s = case dropWhile isSpace s of
  []         -> []
  '"' : rest -> let (tok, rest') = break (== '"') rest
                in tok : tokenise (drop 1 rest')
  s'         -> let (tok, rest) = break isSpace s'
                in tok : tokenise rest

-- ---------------------------------------------------------------------------
-- From manifest to UnitInfo

-- | Build the 'UnitInfo' of a unit in the store.
--
-- Everything path-like follows the layout; the manifest supplies the rest.
-- The @abi@ hash, which the compiler only uses as a cache key, is a
-- fingerprint of the manifest text.
storeUnitInfo
  :: OsPath    -- ^ store root
  -> UnitId    -- ^ the unit's directory name
  -> UnitMeta
  -> String    -- ^ manifest text, fingerprinted for 'unitAbiHash'
  -> Bool      -- ^ does @lib/include@ exist?
  -> UnitInfo
storeUnitInfo root uid meta meta_txt has_include = GenericUnitInfo
  { unitId             = uid
  , unitInstanceOf     = fromMaybe default_instance_of (um_instanceOf meta)
  , unitInstantiations = um_instantiatedWith meta
  , unitPackageId      = PackageId (mkFastString label)
  , unitPackageName    = PackageName (mkFastString name)
  , unitPackageVersion = version
  , unitComponentName  = Nothing
  , unitAbiHash        = ST.pack (show (fingerprintString meta_txt))
  , unitDepends        = um_depends meta
  , unitAbiDepends     = []
  , unitImportDirs     = [lib_dir]
  , unitLibraries      = [ST.pack ("HS" ++ uid_str)]
  , unitExtDepLibsSys       = map ST.pack (um_extraLibraries meta)
  , unitExtDepLibsStaticSys = map ST.pack (um_extraLibrariesStatic meta)
  , unitExtDepLibsGhc       = map ST.pack (um_extraGhciLibraries meta)
  , unitLibraryDirs         = [lib_dir]
  , unitLibraryDirsStatic   = [lib_dir]
  , unitLibraryDynDirs      = [lib_dir]
  , unitLibraryBytecodeDirs = [lib_dir]
  , unitExtDepFrameworks    = map ST.pack (um_frameworks meta)
  , unitExtDepFrameworkDirs = map ST.pack (um_frameworkDirs meta)
  , unitLinkerOptions       = map ST.pack (um_ldOptions meta)
  , unitCcOptions           = []
  , unitIncludes            = map ST.pack (um_includes meta)
  , unitIncludeDirs         = [include_dir | has_include]
  , unitHaddockInterfaces   = []
  , unitHaddockHTMLs        = []
  , unitExposedModules      = um_exposedModules meta
  , unitHiddenModules       = um_hiddenModules meta
  , unitIsIndefinite        = um_indefinite meta
  , unitIsExposed           = True
  , unitIsTrusted           = False
  }
  where
    uid_str     = unpackFS (unitIdFS uid)
    unit_dir    = root OsPath.</> OsPath.unsafeEncodeUtf uid_str
    lib_dir     = ST.pack (OsPath.unsafeDecodeUtf (unit_dir OsPath.</> libDirName))
    include_dir = ST.pack (OsPath.unsafeDecodeUtf (unit_dir OsPath.</> libDirName OsPath.</> includeDirName))

    name    = fromMaybe uid_str (um_name meta)
    version = fromMaybe (makeVersion [0]) (um_version meta)
    label   = case um_name meta of
      Nothing -> uid_str
      Just n  -> n ++ "-" ++ showVersion version

    -- ghc-pkg's rule: an instantiated unit @p+hash@ is an instance of @p@.
    default_instance_of = UnitId (mkFastString (takeWhile (/= '+') uid_str))
