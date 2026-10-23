module Q (T(..), f, qval) where

data T = MkT Int

f :: T -> String
f (MkT n) = "Q.f " ++ show n

qval :: Int
qval = 42
