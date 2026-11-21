{-# LANGUAGE ConstraintKinds, PolyKinds #-}
module Type.Reflection
  ( Typeable
  , TypeRep(..)
  , SomeTypeRep(..)
  , typeRep
  , someTypeRep
  , typeOf
  , someTypeRepToDT
  , dtToSomeTypeRep
  ) where

import Prelude

import qualified Data.Typeable as DT
import Data.Proxy (Proxy(..))

type Typeable a = DT.Typeable a

newtype TypeRep a = MkTypeRep { unMkTypeRep :: DT.TypeRep }
  deriving (Eq, Ord, Show)

data SomeTypeRep = forall a. SomeTypeRep (TypeRep a)

instance Eq SomeTypeRep where
  SomeTypeRep r1 == SomeTypeRep r2 = unMkTypeRep r1 == unMkTypeRep r2

instance Ord SomeTypeRep where
  compare (SomeTypeRep r1) (SomeTypeRep r2) = compare (unMkTypeRep r1) (unMkTypeRep r2)

instance Show SomeTypeRep where
  showsPrec p (SomeTypeRep r) = showsPrec p (unMkTypeRep r)

typeRep :: forall a. DT.Typeable a => TypeRep a
typeRep = MkTypeRep (DT.typeRep (Proxy :: Proxy a))

someTypeRep :: forall proxy a. DT.Typeable a => proxy a -> SomeTypeRep
someTypeRep p = SomeTypeRep (MkTypeRep (DT.typeRep p))

typeOf :: forall a. DT.Typeable a => a -> TypeRep a
typeOf x = MkTypeRep (DT.typeOf x)

someTypeRepToDT :: SomeTypeRep -> DT.TypeRep
someTypeRepToDT (SomeTypeRep r) = unMkTypeRep r

dtToSomeTypeRep :: DT.TypeRep -> SomeTypeRep
dtToSomeTypeRep r = SomeTypeRep (MkTypeRep r :: TypeRep ())
