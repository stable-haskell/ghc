{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

#def inline int runtime_computed_magic(void) { volatile int x = 1337; return x; }

magic :: Int
magic = #const runtime_computed_magic()

main :: IO ()
main = print magic
