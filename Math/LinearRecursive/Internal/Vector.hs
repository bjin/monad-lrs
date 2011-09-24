
module Math.LinearRecursive.Internal.Vector
  ( Vector
  , Vector1
  , VectorLike(..)
  , vector
  , unVector
  , vector1
  , unVector1
  , unVector'
  , (<+>)
  , (<->)
  , (*>)
  , (<*)
  , zeroVector
  ) where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

newtype Vector a = Vector { unVector :: IntMap a }

vector :: Num a => IntMap a -> Vector a
vector = Vector

newtype Vector1 a = Vector1 { unVector1 :: Int }

vector1 :: Num a => Int -> Vector1 a
vector1 = Vector1

class VectorLike v where
    toVector :: Num a => v a -> Vector a

instance VectorLike Vector where
    toVector = id

instance VectorLike Vector1 where
    toVector (Vector1 p) = Vector (IntMap.singleton p 1)

instance Functor Vector where
    fmap f = Vector . IntMap.map f . unVector

unVector' :: (Num a, VectorLike v) => v a -> IntMap a
unVector' = unVector . toVector

(<+>) :: (Num a, VectorLike v1, VectorLike v2) => v1 a -> v2 a -> Vector a
a <+> b = Vector $ IntMap.unionWith (+) (unVector' a) (unVector' b)

(<->) :: (Num a, VectorLike v1, VectorLike v2) => v1 a -> v2 a -> Vector a
a <-> b = a <+> fmap negate (toVector b)

(*>) :: (Num a, VectorLike v) => v a -> a -> Vector a
a *> b = fmap (*b) (toVector a)

(<*) :: (Num a, VectorLike v) => a -> v a -> Vector a
a <* b = fmap (a*) (toVector b)

infixl 6 <+>,<->
infixl 7 *>
infixr 7 <*

zeroVector :: Num a => Vector a
zeroVector = Vector IntMap.empty
