module Main where

-- A plain console program — no FFI, no reactor. The very same source builds and
-- runs two ways from the one multi-target GHC install:
--   * JS:     `make run-node`     (javascript-unknown-ghcjs-ghc -> node)
--   * native: `make run-native`   (ghc, this host)
-- The wasm target uses a different (reactor) shape — see the `hello` template.
main :: IO ()
main = putStrLn "Hello from GHC's JavaScript backend (and it runs native too)!"
