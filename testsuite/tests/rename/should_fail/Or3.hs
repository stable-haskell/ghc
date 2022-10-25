{-# LANGUAGE OrPatterns #-}

module Main where

f x = case x of
  (Left a | Right a) -> a

g x = case x of
  (_ | (x | _)) -> x

h x = case x of
  (Just @Int 3 | Nothing) -> 2

main = print $ foo 3 IsInt1