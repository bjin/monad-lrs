
module Math.LinearRecursive.Internal.Polynomial
  ( Polynomial
  , polynomial
  , unPoly
  , fromList
  , toList
  , singleton
  , x
  , degree
  , evalPoly
  ) where

import qualified Data.IntMap as IntMap

import Math.LinearRecursive.Internal.Vector

newtype Polynomial a = Polynomial { unPoly :: Vector a }

polynomial :: Num a => Vector a -> Polynomial a
polynomial = Polynomial

toList :: Num a => Polynomial a -> [(Int, a)]
toList (Polynomial a) = IntMap.assocs (unVector a)

fromList :: Num a => [(Int, a)] -> Polynomial a
fromList lst = polynomial (vector (IntMap.fromListWith (+) lst))

singleton :: Num a => a -> Polynomial a
singleton v = polynomial (vector (IntMap.singleton 0 v))

x :: Num a => Polynomial a
x = polynomial (vector (IntMap.singleton 1 1))

degree :: Num a => Polynomial a -> Int
degree p = maximum $ (-1) : map fst (toList p)

evalPoly :: Num a => Polynomial a -> a -> a
evalPoly p v = sum [ci * v ^ i  | (i, ci) <- toList p]

instance (Show a, Num a) => Show (Polynomial a) where
    show a = "Polynomial " ++ show (toList a)

instance Num a => Eq (Polynomial a) where
    Polynomial a == Polynomial b = a == b

instance Num a => Num (Polynomial a) where
    Polynomial a + Polynomial b = polynomial (a <+> b)
    Polynomial a - Polynomial b = polynomial (a <-> b)
    negate (Polynomial a) = polynomial (vmap negate a)

    a * b = fromList [(i + j, ai * bj) | (i, ai) <- toList a , (j, bj) <- toList b]

    abs = error "Polynomial : absolute value undefined"
    signum = error "Polynomial : signum undefined"

    fromInteger = singleton . fromInteger
