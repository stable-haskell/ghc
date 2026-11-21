{-# LANGUAGE CPP #-}
#if !defined(__MHS__)
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
#endif

-- | Small-array
module GHC.Data.SmallArray
  ( SmallMutableArray (..)
  , SmallArray (..)
  , newSmallArray
  , writeSmallArray
  , freezeSmallArray
  , unsafeFreezeSmallArray
  , indexSmallArray
  , sizeofSmallArray
  , listToArray
  , mapSmallArray
  , foldMapSmallArray
  , rnfSmallArray

  -- * IO Operations
  , SmallMutableArrayIO
  , newSmallArrayIO
  , writeSmallArrayIO
  , unsafeFreezeSmallArrayIO
  )
where

#if defined(__MHS__)
import GHC.Prelude.Basic
import Control.DeepSeq
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Phantom state.  MicroHs has no State# / SmallArray#.
data RealWorld = RealWorld
data State# s = State#

data SmallArray a = SmallArray !Int [a]

data SmallMutableArray s a = SmallMutableArray (IORef [a])

type SmallMutableArrayIO a = SmallMutableArray RealWorld a

newSmallArray
  :: Int
  -> a
  -> State# s
  -> (# State# s, SmallMutableArray s a #)
newSmallArray n x s = unsafePerformIO $ do
  r <- newIORef (replicate n x)
  return (# s, SmallMutableArray r #)

newSmallArrayIO :: Int -> a -> IO (SmallMutableArrayIO a)
newSmallArrayIO n x = do
  r <- newIORef (replicate n x)
  return (SmallMutableArray r)

writeSmallArray
  :: SmallMutableArray s a
  -> Int
  -> a
  -> State# s
  -> State# s
writeSmallArray (SmallMutableArray r) i x s = unsafePerformIO $ do
  xs <- readIORef r
  writeIORef r (setAt i x xs)
  return s

writeSmallArrayIO :: SmallMutableArrayIO a -> Int -> a -> IO ()
writeSmallArrayIO a ix v =
  writeSmallArray a ix v State# `seq` return ()

freezeSmallArray
  :: SmallMutableArray s a
  -> Int
  -> Int
  -> State# s
  -> (# State# s, SmallArray a #)
freezeSmallArray (SmallMutableArray r) offset len s = unsafePerformIO $ do
  xs <- readIORef r
  return (# s, SmallArray len (take len (drop offset xs)) #)

unsafeFreezeSmallArray
  :: SmallMutableArray s a
  -> State# s
  -> (# State# s, SmallArray a #)
unsafeFreezeSmallArray (SmallMutableArray r) s = unsafePerformIO $ do
  xs <- readIORef r
  return (# s, SmallArray (count xs) xs #)

unsafeFreezeSmallArrayIO :: SmallMutableArrayIO a -> IO (SmallArray a)
unsafeFreezeSmallArrayIO arr =
  case unsafeFreezeSmallArray arr State# of
    (# _, a #) -> return a

sizeofSmallArray :: SmallArray a -> Int
sizeofSmallArray (SmallArray n _) = n

indexSmallArray :: SmallArray a -> Int -> a
indexSmallArray (SmallArray _ xs) i = xs !! i

mapSmallArray :: (a -> b) -> SmallArray a -> SmallArray b
mapSmallArray f (SmallArray n xs) = SmallArray n (map f xs)

foldMapSmallArray :: Monoid m => (a -> m) -> SmallArray a -> m
foldMapSmallArray f (SmallArray _ xs) = go xs
  where
    go [] = mempty
    go (y:ys) = f y `mappend` go ys

rnfSmallArray :: NFData a => SmallArray a -> ()
rnfSmallArray (SmallArray _ xs) = rnf xs

listToArray :: Int -> (e -> Int) -> (e -> a) -> [e] -> SmallArray a
listToArray n indexOf valueOf xs =
  SmallArray n (place (replicate n uninit) xs)
  where
    uninit = error "SmallArray: internal error, uninitialised elements"
    place arr [] = arr
    place arr (e:es) = place (setAt (indexOf e) (valueOf e) arr) es

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = go i xs
  where
    go 0 (_:ys) = v : ys
    go k (y:ys) = y : go (k - 1) ys
    go _ [] = []

-- Data.List.length and Data.Foldable.length are both in scope.
count :: [a] -> Int
count = go 0
  where
    go n [] = n
    go n (_:ys) = go (n + 1) ys
#else
import GHC.Exts
import GHC.Prelude
import GHC.IO
import GHC.ST
import Control.DeepSeq

data SmallArray a = SmallArray (SmallArray# a)

data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)

type SmallMutableArrayIO a = SmallMutableArray RealWorld a

newSmallArray
  :: Int  -- ^ size
  -> a    -- ^ initial contents
  -> State# s
  -> (# State# s, SmallMutableArray s a #)
{-# INLINE newSmallArray #-}
newSmallArray (I# sz) x s = case newSmallArray# sz x s of
  (# s', a #) -> (# s', SmallMutableArray a #)

newSmallArrayIO :: Int -> a -> IO (SmallMutableArrayIO a)
newSmallArrayIO sz x = IO $ \s -> newSmallArray sz x s

writeSmallArray
  :: SmallMutableArray s a -- ^ array
  -> Int                   -- ^ index
  -> a                     -- ^ new element
  -> State# s
  -> State# s
{-# INLINE writeSmallArray #-}
writeSmallArray (SmallMutableArray a) (I# i) x = writeSmallArray# a i x

writeSmallArrayIO :: SmallMutableArrayIO a
                  -> Int
                  -> a
                  -> IO ()
writeSmallArrayIO a ix v = IO $ \s -> (# writeSmallArray a ix v s, () #)


-- | Copy and freeze a slice of a mutable array.
freezeSmallArray
  :: SmallMutableArray s a -- ^ source
  -> Int                   -- ^ offset
  -> Int                   -- ^ length
  -> State# s
  -> (# State# s, SmallArray a #)
{-# INLINE freezeSmallArray #-}
freezeSmallArray (SmallMutableArray ma) (I# offset) (I# len) s =
  case freezeSmallArray# ma offset len s of
    (# s', a #) -> (# s', SmallArray a #)

-- | Freeze a mutable array (no copy!)
unsafeFreezeSmallArray
  :: SmallMutableArray s a
  -> State# s
  -> (# State# s, SmallArray a #)
{-# INLINE unsafeFreezeSmallArray #-}
unsafeFreezeSmallArray (SmallMutableArray ma) s =
  case unsafeFreezeSmallArray# ma s of
    (# s', a #) -> (# s', SmallArray a #)

unsafeFreezeSmallArrayIO :: SmallMutableArrayIO a -> IO (SmallArray a)
unsafeFreezeSmallArrayIO arr = IO $ \s -> unsafeFreezeSmallArray arr s

-- | Get the size of a 'SmallArray'
sizeofSmallArray
  :: SmallArray a
  -> Int
{-# INLINE sizeofSmallArray #-}
sizeofSmallArray (SmallArray sa#) =
  case sizeofSmallArray# sa# of
    s -> I# s

-- | Index a small-array (no bounds checking!)
indexSmallArray
  :: SmallArray a -- ^ array
  -> Int          -- ^ index
  -> a
{-# INLINE indexSmallArray #-}
indexSmallArray (SmallArray sa#) (I# i) =
  case indexSmallArray# sa# i of
    (# v #) -> v

-- | Map a function over the elements of a 'SmallArray'
--
mapSmallArray :: (a -> b) -> SmallArray a -> SmallArray b
{-# INLINE mapSmallArray #-}
mapSmallArray f sa = runST $ ST $ \s ->
  let
    n = sizeofSmallArray sa
    go !i saMut# state#
      | i < n =
        let
          a = indexSmallArray sa i
          newState# = writeSmallArray saMut# i (f a) state#
        in
          go (i + 1) saMut# newState#
      | otherwise = state#
  in
  case newSmallArray n (error "SmallArray: internal error, uninitialised elements") s of
    (# s', mutArr #) ->
      case go 0 mutArr s' of
        s'' -> unsafeFreezeSmallArray mutArr s''

-- | Fold the values of a 'SmallArray' into a 'Monoid m' of choice
foldMapSmallArray :: Monoid m => (a -> m) -> SmallArray a -> m
{-# INLINE foldMapSmallArray #-}
foldMapSmallArray f sa = go 0
  where
    n = sizeofSmallArray sa
    go i
      | i < n = f (indexSmallArray sa i) `mappend` go (i + 1)
      | otherwise = mempty

-- | Force the elements of the given 'SmallArray'
--
rnfSmallArray :: NFData a => SmallArray a -> ()
{-# INLINE rnfSmallArray #-}
rnfSmallArray sa = go 0
  where
    n = sizeofSmallArray sa
    go !i
      | i < n = rnf (indexSmallArray sa i) `seq` go (i + 1)
      | otherwise = ()

-- | Convert a list into an array.
listToArray :: Int -> (e -> Int) -> (e -> a) -> [e] -> SmallArray a
{-# INLINE listToArray #-}
listToArray (I# size) index_of value_of xs = runST $ ST \s ->
  let
    index_of' e = case index_of e of I# i -> i
    write_elems ma es s = case es of
      []    -> s
      e:es' -> case writeSmallArray# ma (index_of' e) (value_of e) s of
                 s' -> write_elems ma es' s'
  in
  case newSmallArray# size undefined s of
    (# s', ma #) -> case write_elems ma xs s' of
      s'' -> case unsafeFreezeSmallArray# ma s'' of
        (# s''', a #) -> (# s''', SmallArray a #)
#endif
