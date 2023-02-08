{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -O -fno-worker-wrapper -funfolding-creation-threshold=50 #-}

module ExposeOverloaded where

-- Will get an unfolding because of the Functor
-- foo :: Functor a => Maybe (Maybe (Maybe (Maybe (Maybe (Maybe a))))) -> (a -> b) -> Maybe (Maybe (Maybe (Maybe (Maybe b))))
foo :: Functor f => Maybe (Maybe (Maybe (Maybe (Maybe (Maybe (f a))))))
    -> (a -> b)
    -> Maybe (Maybe (Maybe (Maybe (f b))))
foo (Just (Just (Just (Just (Just (Just x)))))) f = Just $ Just $ Just $ Just $ fmap f x
foo _ _ = Nothing

type family C a where
  C Int = Eq Int
  C Bool = Ord Bool

bar :: (C a, Enum a) => a -> a -> Bool
bar a b = fromEnum (succ a) > fromEnum (pred . pred . pred . pred . pred $ b)


newtype F t a = F {unF :: (Functor t => t a) }

-- Will get NO unfolding currently since the class dictionary is hidden under the newtype.
-- We might fix this eventually. But since the specializer doesn't handle this well
-- this isn't important yet.
baz :: Maybe (Maybe (Maybe (Maybe (Maybe (Maybe (F t a))))))
    -> (a -> b)
    -> Maybe (Maybe (Maybe (Maybe (F t b))))
baz (Just (Just (Just (Just (Just (Just (x))))))) f = Just $ Just $ Just $ Just $ F $ fmap f (unF x)
baz _ _ = Nothing