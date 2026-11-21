module Demo.Report
  ( -- * Prime sieve
    primesUpTo
  , primeChecksum
    -- * Word frequency
  , wordFreq
  , topWords
    -- * Report
  , Report (..)
  , buildReport
  , renderReport
    -- * Self-checks
  , checkReport
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.List (foldl', sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Text as T

-- | Sieve of Eratosthenes: all primes @<= n@.
primesUpTo :: Int -> [Int]
primesUpTo n = sieve [2 .. n]
  where
    sieve [] = []
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- | Strict checksum over the primes: sum of (p * index).
primeChecksum :: [Int] -> Int
primeChecksum = foldl' (\acc (i, p) -> acc + i * p) 0 . zip [1 ..]

-- | Case-insensitive word frequency over strict 'T.Text'.
wordFreq :: T.Text -> Map T.Text Int
wordFreq =
  foldl' (\m w -> Map.insertWith (+) w 1 m) Map.empty
    . T.words
    . T.toCaseFold

-- | The @n@ most frequent words (descending count, then ascending word).
topWords :: Int -> Map T.Text Int -> [(T.Text, Int)]
topWords n = take n . sortBy (comparing (\(w, c) -> (negate c, w))) . Map.toList

-- | Everything the demo computes, kept in one algebraic data type so the
-- executable can pattern-match, render, and self-check it.
data Report = Report
  { reportPrimeLimit :: Int
  , reportPrimeCount :: Int
  , reportChecksum :: Int
  , reportTopWords :: [(T.Text, Int)]
  , reportBytes :: Int
  }
  deriving (Eq, Show)

sampleText :: T.Text
sampleText =
  T.unwords
    [ "the",
      "stage",
      "two",
      "ghc",
      "runs",
      "the",
      "demo",
      "the",
      "stage",
      "two",
      "ghc",
      "shines"
    ]

-- | Build the report from a prime limit. Exercises polymorphism (the
-- sieve works over 'Int', the fold over strict 'Map'), strict folds,
-- 'Data.ByteString' packing, and 'Data.Text' processing.
buildReport :: Int -> Report
buildReport limit =
  let ps = primesUpTo limit
      freq = wordFreq sampleText
      top = topWords 3 freq
      raw = BS.pack (show (primeChecksum ps) ++ concatMap (T.unpack . fst) top)
   in Report
        { reportPrimeLimit = limit,
          reportPrimeCount = length ps,
          reportChecksum = primeChecksum ps,
          reportTopWords = top,
          reportBytes = BS.length raw
        }

-- | Render a human-readable multi-line summary.
renderReport :: Report -> String
renderReport r =
  unlines
    [ "primes up to " ++ show (reportPrimeLimit r) ++ ": " ++ show (reportPrimeCount r),
      "checksum: " ++ show (reportChecksum r),
      "top words: " ++ show (reportTopWords r),
      "byte size: " ++ show (reportBytes r)
    ]

-- | Self-checks: the executable exits 0 only when every check passes.
-- Expected values are hand-verified: primes <= 30 are
-- [2,3,5,7,11,13,17,19,23,29] (10 primes) with checksum
-- 1*2+2*3+3*5+4*7+5*11+6*13+7*17+8*19+9*23+10*29 = 952,
-- and "the" x3 dominates the sample text ("ghc"/"stage"/"two" x2 each).
checkReport :: Report -> [String]
checkReport r =
  [ check "prime count" 10 (reportPrimeCount r),
    check "checksum" 952 (reportChecksum r),
    check "top word" (T.pack "the", 3) (head (reportTopWords r)),
    check "byte size positive" True (reportBytes r > 0)
  ]
  where
    check :: (Eq a, Show a) => String -> a -> a -> String
    check name expected actual
      | expected == actual = "PASS: " ++ name
      | otherwise =
        "FAIL: " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"
