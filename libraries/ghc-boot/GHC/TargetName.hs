module GHC.TargetName
  ( TargetName
  , targetNameString
  , parseTargetName
  , targetNameForABI
  , validateTargetNameNamespace
  , targetNameNamespace
  ) where

import Prelude

import Data.Char (isAscii, isAlphaNum)
import Data.List (foldl')

newtype TargetName = TargetName String
  deriving (Eq, Ord)

targetNameString :: TargetName -> String
targetNameString (TargetName name) = name

parseTargetName :: String -> Either String TargetName
parseTargetName [] = Left "target name must not be empty"
parseTargetName name@(first:_)
  | length name > 255 = Left "target name must not exceed 255 ASCII characters"
  | not (asciiAlphaNum first && asciiAlphaNum (foldl' (\_ c -> c) first name)) =
      Left "target name must begin and end with an ASCII letter or digit"
  | any (not . allowed) name =
      Left "target name may contain only ASCII letters, digits, '_', '.', and '-'"
  | any (`elem` ["", ".", ".."] ) (components name) =
      Left "target name contains an empty or unsafe component"
  | otherwise = Right (TargetName name)
  where
    allowed c = asciiAlphaNum c || c `elem` "_.-"
    asciiAlphaNum c = isAscii c && isAlphaNum c
    components [] = []
    components xs = let (part, rest) = break (== '-') xs
                    in part : case rest of
                         [] -> []
                         _:ys -> components ys

targetNameForABI :: String -> String -> Either String TargetName
targetNameForABI machine abi = do
  _ <- parseTargetName machine
  name <- parseTargetName $ case abi of
    "" -> machine
    "profiling" -> machine ++ "-prof"
    _ -> machine
  if abi `elem` ["", "profiling"]
    then Right name
    else Left ("unsupported target ABI " ++ show abi)

targetNameNamespace :: TargetName -> String -> FilePath
targetNameNamespace (TargetName name) version = name ++ "-" ++ version

-- | Ensure the versioned directory component fits common filesystem limits.
validateTargetNameNamespace :: TargetName -> String -> Either String TargetName
validateTargetNameNamespace name version
  | length (targetNameNamespace name version) > 255 =
      Left "versioned target namespace must not exceed 255 ASCII characters"
  | otherwise = Right name
