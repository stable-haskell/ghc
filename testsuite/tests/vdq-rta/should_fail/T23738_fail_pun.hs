{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeAbstractions #-}

module T23738_fail_pun where

vfun :: forall (a :: k) -> ()
vfun (type _) = ()

f :: Maybe a -> ()
f (Just @a a) = vfun a