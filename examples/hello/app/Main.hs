{-# LANGUAGE CPP #-}
module Main where

#ifdef WASM
-- Reactor pattern: GHC won't synthesize a `_start` entry point because the
-- cabal file uses `-no-hs-main` + `-optl-mexec-model=reactor`. We
-- foreign-export `hs_start`, which the linker exports as a wasm function the
-- JS host can call.
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = putStrLn "Hello from the WASM reactor!"
