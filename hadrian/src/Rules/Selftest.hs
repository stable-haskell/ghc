{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rules.Selftest (selftestRules) where

import Hadrian.Haskell.Cabal
import Test.QuickCheck

import Base
import Context
import Flavour
import Hadrian.BuildPath
import Oracles.ModuleFiles
import Packages
import Settings
import Settings.Default (defaultFlavour)
import Target
import Utilities

import qualified System.FilePath.Posix as Posix ((</>))
import qualified Text.Parsec as Parsec

instance Arbitrary Way where
    arbitrary = wayFromUnits <$> arbitrary

instance Arbitrary WayUnit where
    arbitrary = arbitraryBoundedEnum

test :: Testable a => a -> Action ()
test = liftIO . quickCheck

selftestRules :: Rules ()
selftestRules = do
    "selftest" ~> do
        testBuilder
        testChunksOfSize
        testDependencies
        testLookupAll
        testModuleName
        testPackages
        testPaths
        testWay
        testRetiredProfiling
    -- This subset needs no configured compiler or built GHC packages.
    "selftest-profiling-ways" ~> testRetiredProfiling

testBuilder :: Action ()
testBuilder = do
    putBuild "==== trackArgument"
    let make = target undefined (Make undefined) undefined undefined
    test $ forAll (elements ["-j", "MAKEFLAGS=-j", "THREADS="])
         $ \prefix (NonNegative n) ->
            not (trackArgument make prefix) &&
            not (trackArgument make ("-j" ++ show (n :: Int)))

testChunksOfSize :: Action ()
testChunksOfSize = do
    putBuild "==== chunksOfSize"
    test $ chunksOfSize 3 [  "a", "b", "c" ,  "defg" ,  "hi" ,  "jk"  ]
                       == [ ["a", "b", "c"], ["defg"], ["hi"], ["jk"] ]
    test $ \n xs ->
        let res = chunksOfSize n xs
        in concat res == xs && all (\r -> length r == 1 || length (concat r) <= n) res

testDependencies :: Action ()
testDependencies = do
    putBuild "==== pkgDependencies"
    let pkgs = ghcPackages \\ [libffi] -- @libffi@ does not have a Cabal file.
    depLists <- mapM pkgDependencies pkgs
    test $ and [ deps == sort deps | deps <- depLists ]
    putBuild "==== Dependencies of the 'ghc-bin' binary"
    ghcDeps <- pkgDependencies ghc
    test $ pkgName compiler `elem` ghcDeps
    stage0Deps <- contextDependencies (vanillaContext stage0InTree ghc)
    stage1Deps <- contextDependencies (vanillaContext Stage1 ghc)
    stage2Deps <- contextDependencies (vanillaContext Stage2 ghc)
    test $ vanillaContext stage0InTree compiler `notElem` stage1Deps
    test $ vanillaContext Stage1 compiler `elem`    stage1Deps
    test $ vanillaContext Stage2 compiler `notElem` stage1Deps
    test $ stage1Deps /= stage0Deps
    test $ stage1Deps == stage2Deps

testLookupAll :: Action ()
testLookupAll = do
    putBuild "==== lookupAll"
    test $ lookupAll ["b"    , "c"            ] [("a", 1), ("c", 3), ("d", 4)]
                  == [Nothing, Just (3 :: Int)]
    test $ forAll dicts $ \dict -> forAll extras $ \extra ->
        let items = sort $ map fst dict ++ extra
        in lookupAll items (sort dict) == map (`lookup` dict) items
  where
    dicts :: Gen [(Int, Int)]
    dicts = nubBy (\x y -> fst x == fst y) <$> vector 20
    extras :: Gen [Int]
    extras = vector 20

testModuleName :: Action ()
testModuleName = do
    putBuild "==== Encode/decode module name"
    test $ encodeModule "Data/Functor" "Identity.hs" == "Data.Functor.Identity"
    test $ encodeModule "" "Prelude"                 == "Prelude"

    test $ decodeModule "Data.Functor.Identity" == ("Data/Functor", "Identity")
    test $ decodeModule "Prelude"               == ("", "Prelude")

    test $ forAll names $ \n -> uncurry encodeModule (decodeModule n) == n
  where
    names = intercalate "." <$> listOf1 (listOf1 $ elements "abcABC123_'")

testPackages :: Action ()
testPackages = do
    putBuild "==== Check system configuration"
    putBuild "==== Packages, interpretInContext, configuration flags"
    forM_ allStages $ \stage -> do
        pkgs <- stagePackages stage
        when (win32 `elem` pkgs) . test $ windowsHost
        when (unix  `elem` pkgs) . test $ not windowsHost
        test $ pkgs == nubOrd pkgs

testWay :: Action ()
testWay = do
    putBuild "==== Read Way, Show Way"
    test $ \(x :: Way) -> read (show x) == x

testRetiredProfiling :: Action ()
testRetiredProfiling = do
    putBuild "==== Profiling uses a separate target ABI"
    check "ordinary suffixes" $ osuf vanilla == "o" && hisuf vanilla == "hi"
    check "dynamic/threaded/debug suffixes" $
        osuf dynamic == "dyn_o" && osuf threadedDebug == "thr_debug_o"
    forM_ allWays $ \w ->
        check ("roundtrip " ++ show w) $ read (show w) == w
    forM_ ["p", "p_dyn", "thr_p", "thr_debug_p_dyn"] $ \oldWay ->
        check ("retired way " ++ oldWay) $ null (reads oldWay :: [(Way, String)])
    forM_ ["_p", "_thr_p", "_p_dyn"] $ \suffix ->
        check ("retired suffix " ++ suffix) $
            migrationError $ Parsec.parse (parseWaySuffix vanilla <* Parsec.eof) "" suffix
    forM_ ["p_", "thr_p_", "p_dyn_"] $ \prefix ->
        check ("retired prefix " ++ prefix) $
            migrationError $ Parsec.parse (parseWayPrefix vanilla <* Parsec.eof) "" prefix
    forM_ ["prof", "default+profiled_ghc", "default+no_profiled_libs", "default+late_ccs"] $ \spec ->
        check ("retired flavour " ++ spec) $ case parseFlavour [defaultFlavour] flavourTransformers spec of
            Left err -> "make stage3-profile" `isInfixOf` err
            Right _ -> False
    check "ordinary flavour modifier" $ case parseFlavour [defaultFlavour] flavourTransformers "default+no_dynamic_libs" of
        Left _ -> False
        Right _ -> True
  where
    check label ok = unless ok $ fail $ "Profiling way regression: " ++ label
    migrationError (Left err) = "make stage3-profile" `isInfixOf` show err
    migrationError (Right _) = False

testPaths :: Action ()
testPaths = do
    putBuild "==== Absolute, Relative Path Concatenation"
    test $ forAll paths $ \(path1, path2) ->
      path1 -/- path2 == path1 Posix.</> path2
  where
    paths = (,) <$> path <*> path
    path = frequency [(1, relativePath), (1, absolutePath)]
    relativePath = intercalate "/" <$> listOf1 (elements ["a"])
    absolutePath = ('/':) <$> relativePath
