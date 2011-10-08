
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
  , vmap
  , vcomponent
  ) where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

newtype Vector a = Vector { unVector :: IntMap a } deriving Show

instance Num a => Eq (Vector a) where
    Vector a == Vector b = a == b

vector :: Num a => IntMap a -> Vector a
vector ma = Vector (IntMap.filter (/=0) ma)

newtype Vector1 a = Vector1 { unVector1 :: Int } deriving (Eq, Show)

vector1 :: Num a => Int -> Vector1 a
vector1 = Vector1

vcomponent :: Num a => Vector a -> Int -> a
vcomponent (Vector mapping) i = IntMap.findWithDefault 0 i mapping

-- | represents vector type class, there are two instances:
--
--  [@Vector@] General purpose vector.
--
--  [@Vector1@] Unit vector in vector space.
class VectorLike v where
    toVector :: Num a => v a -> Vector a

instance VectorLike Vector where
    toVector = id

instance VectorLike Vector1 where
    toVector (Vector1 p) = vector (IntMap.singleton p 1)

vmap :: (Num a, Num b) => (a -> b) -> Vector a -> Vector b
vmap f = vector . IntMap.map f . unVector

unVector' :: (Num a, VectorLike v) => v a -> IntMap a
unVector' = unVector . toVector

-- | Vector addition, @a \<+\> b@ represents sum of @a@ and @b@.
(<+>) :: (Num a, VectorLike v1, VectorLike v2) => v1 a -> v2 a -> Vector a
a <+> b = vector $ IntMap.unionWith (+) (unVector' a) (unVector' b)

-- | Vector subtraction, @a \<-\> b@ represents difference of @a@ and @b@
(<->) :: (Num a, VectorLike v1, VectorLike v2) => v1 a -> v2 a -> Vector a
a <-> b = a <+> vmap negate (toVector b)

-- | Right-side scalar multiplication, @a *\> s@ is @a@ scaled by @s@.
(*>) :: (Num a, VectorLike v) => v a -> a -> Vector a
a *> b = vmap (*b) (toVector a)

-- | Left-side scalar multiplication, @s \<* a@ is @a@ scaled by @s@.
-- For example, @1 <* a@ equals to @a@, @2 <* a@ equals to @a+a@
(<*) :: (Num a, VectorLike v) => a -> v a -> Vector a
a <* b = vmap (a*) (toVector b)

infixl 6 <+>,<->
infixl 7 *>
infixr 7 <*

-- | The zero vector.
zeroVector :: Num a => Vector a
zeroVector = vector IntMap.empty
