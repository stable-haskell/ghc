{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE OrPatterns #-}

data T = A | B
data U = V | W

g :: T -> U -> Int
g (A|B) V = 0
g B (V|W) = 1

h A (_|W) B = 0
h B (V|_) B = 1
h (A|B) _ B = 2

z (1|2|1) = 0
z (3|2) = 1
z 1 = 2

main = print 2