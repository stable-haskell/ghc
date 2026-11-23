module P (pval, pshow) where

import Q
import P.Internal

pval :: Int
pval = qval + secret

pshow :: String
pshow = f (MkT pval)
