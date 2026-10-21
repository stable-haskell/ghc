module GHC.TargetDispatch (resolveTargetAlias, targetNameFromProgram, lastTargetName, stripWindowsExeExtension) where

import Prelude

import Control.Exception (IOException, evaluate, try)
import Data.Char (toLower)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)
import GHC.TargetName (parseTargetName)

-- | Extract the target name from @T-ghc-V@, @T-ghc@, @T-ghc-pkg-V@ or
-- @T-ghc-pkg@ basenames without treating dots in T or V as extensions.
targetNameFromProgram :: String -> String -> String -> Maybe String
targetNameFromProgram tool version program =
  stripSuffix ("-" ++ tool ++ "-" ++ version) program `orElse`
  stripSuffix ("-" ++ tool) program
  where
    stripSuffix suffix value
      | suffix `isSuffixOf` value = Just (take (length value - length suffix) value)
      | otherwise = Nothing
    orElse (Just value) _ = Just value
    orElse Nothing other = other

-- | Strip a Windows executable suffix without changing the spelling of the
-- basename. Callers should use this only for MinGW executables.
stripWindowsExeExtension :: String -> String
stripWindowsExeExtension program
  | ".exe" `isSuffixOf` map toLower program = take (length program - 4) program
  | otherwise = program

-- | Select the last explicit target, matching GetOpt's ordered flag stream.
lastTargetName :: [String] -> Maybe String
lastTargetName [] = Nothing
lastTargetName names = Just (last names)

-- | Resolve an explicit target name or alias against the install manifest.
-- Every canonical directory and alias is validated before any selection.
resolveTargetAlias :: FilePath -> String -> IO (Either String String)
resolveTargetAlias targetsDir requested = do
  dirsResult <- try (listDirectory targetsDir) :: IO (Either IOException [FilePath])
  case dirsResult of
    Left _ -> pure (Left ("Target directory does not exist: " ++ targetsDir))
    Right dirs -> do
      entries <- traverse (readEntry targetsDir) =<< filterMDir dirs
      pure $ do
        pairs <- sequence entries
        let allNames = concatMap (\(canonical, aliases) -> (canonical, canonical) : map (\alias -> (alias, canonical)) aliases) pairs
            grouped = Map.fromListWith (++) [(map toLower name, [(name, canonical)]) | (name, canonical) <- allNames]
            collisions = [ names | names <- Map.elems grouped, length names > 1 ]
        case collisions of
          (names:_) -> Left ("Conflicting target aliases in installation: " ++ show names)
          [] -> case lookup requested allNames of
            Just canonical -> Right canonical
            Nothing -> Left ("Unknown target " ++ show requested ++ " in " ++ targetsDir)
  where
    filterMDir = foldr (\d rest -> do
      isDir <- doesDirectoryExist (targetsDir </> d)
      if isDir then (d:) <$> rest else rest) (pure [])
    readEntry root canonical = do
      aliasesResult <- try $ do
        contents <- readFile (root </> canonical </> "target.aliases")
        _ <- evaluate (length contents)
        pure contents
        :: IO (Either IOException String)
      pure $ do
        _ <- parseTargetName canonical
        aliases <- case aliasesResult of
          Left err | isDoesNotExistError err -> Right []
                   | otherwise -> Left ("Cannot read target.aliases for " ++ canonical ++ ": " ++ show err)
          Right contents -> maybe (Left ("Malformed target.aliases for " ++ canonical)) Right
            (readMaybe contents :: Maybe [String])
        mapM_ (\alias -> case parseTargetName alias of
          Left err -> Left ("Invalid target alias for " ++ canonical ++ ": " ++ err)
          Right _ -> Right ()) aliases
        Right (canonical, aliases)
