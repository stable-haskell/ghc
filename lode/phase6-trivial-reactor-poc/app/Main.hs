{-# LANGUAGE CPP #-}
module Main where

#ifdef WASM
-- Reactor pattern: GHC will not synthesize a _start entry point because the
-- cabal file uses -no-hs-main + -optl-mexec-model=reactor. We export
-- hs_start (the function the linker --exports) which the JS host calls.
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = putStrLn "Hello from the WASM reactor!"
