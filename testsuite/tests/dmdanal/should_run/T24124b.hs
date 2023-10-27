import Control.Exception

f :: Int -> Int -> IO Int
f x y = do
  evaluate x
  pure $! y + 1
{-# NOINLINE f #-}

main = f (error "should see this") (error "should not see this") >> pure ()
