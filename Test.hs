
module Main (main) where

import qualified Math.LinearRecursive.Internal.Matrix as M
import qualified Math.LinearRecursive.Internal.Vector as V
import Math.LinearRecursive.Monad

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Control.Applicative ((<$>), (<*>))

import qualified Data.IntMap as IntMap

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "matrix" 
          [ testProperty "add0" prop_matrix_add0
          , testProperty "mul1" prop_matrix_mul1
          , testProperty "add" prop_matrix_add
          , testProperty "mul" prop_matrix_mul
          , testProperty "muladd" prop_matrix_muladd
          , testProperty "addmul" prop_matrix_addmul
          ]
        , testGroup "vector"
          [ testProperty "add0" prop_vector_add0
          , testProperty "mul1" prop_vector_mul1
          , testProperty "add" prop_vector_add
          , testProperty "mul" prop_vector_mul
          , testProperty "sub" prop_vector_sub
          ]
        , testGroup "linear recursive monad"
          [ testProperty "fib" prop_fib
          , testProperty "const" prop_const
          , testProperty "step" prop_step
          , testProperty "steppower" prop_steppower
          ]
        ]

instance (Num a, Arbitrary a) => Arbitrary (M.Matrix a) where
      arbitrary = frequency [ (30, fmap fromInteger arbitrary)
                            , (70, fmap M.matrix (vectorOf 4 (vectorOf 4 arbitrary)))
                            ]
      shrink a = case M.matrixSize a of 
          Nothing -> [M.diagonal v | v <- shrink (M.unDiagonal a)]
          Just _  -> [a + M.diagonal v | v <- shrink (head (head ma))]
        where
          ma = M.unMatrix a

prop_matrix_add0 :: M.Matrix Rational -> Bool
prop_matrix_add0 a = (a + 0) == a && (0 + a) == a

prop_matrix_mul1 :: M.Matrix Rational -> Bool
prop_matrix_mul1 a = a * 1 == a && 1 * a == a

prop_matrix_add :: M.Matrix Rational -> M.Matrix Rational -> M.Matrix Rational -> Bool
prop_matrix_add a b c = (a + b) + c == a + (b + c)

prop_matrix_mul :: M.Matrix Rational -> M.Matrix Rational -> M.Matrix Rational -> Bool
prop_matrix_mul a b c = (a * b) * c == a * (b * c)

prop_matrix_muladd :: M.Matrix Rational -> M.Matrix Rational -> M.Matrix Rational -> Bool
prop_matrix_muladd a b c = a * (b + c) == a * b + a * c

prop_matrix_addmul :: M.Matrix Rational -> M.Matrix Rational -> M.Matrix Rational -> Bool
prop_matrix_addmul a b c = (a + b) * c == a * c + b * c

instance (Num a, Arbitrary a) => Arbitrary (V.Vector1 a) where
    arbitrary = V.vector1 <$> (arbitrary `suchThat` (>=0))
    shrink a = [V.vector1 a' | a' <- shrink (V.unVector1 a), a' >= 0]

instance (Num a, Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = V.vector . IntMap.fromListWith (+) <$> listOf1 ((,) <$> (abs <$> arbitrary) <*> arbitrary)
    shrink a = [V.vector $ IntMap.insert i v' (V.unVector a) | (i, v) <- IntMap.assocs (V.unVector a), v' <- shrink v]

prop_vector_add0 :: V.Vector Rational -> Bool
prop_vector_add0 a = (a <+> zeroVector) == a && (zeroVector <+> a) == a

prop_vector_mul1 :: V.Vector Rational -> Bool
prop_vector_mul1 a = (a *> 1) == a && (1 <* a) == a

prop_vector_add :: V.Vector Rational -> V.Vector Rational -> V.Vector Rational -> Bool
prop_vector_add a b c = (a <+> b) <+> c == a <+> (b <+> c)

prop_vector_mul :: V.Vector Rational -> Rational -> Rational -> Bool
prop_vector_mul a b c = a *> b *> c == b <* c <* a && a *> (b + c) == a *> b <+> a *> c

prop_vector_sub :: V.Vector Rational -> V.Vector Rational -> Bool
prop_vector_sub a b = a <+> b <-> b == a

fibSeq :: [Integer]
fibSeq = 1 : 1 : zipWith (+) fibSeq (tail fibSeq)

fib :: Integer -> Integer
fib n = flip runLinearRecursive n $ do
    [f0, f1] <- newVariables [1, 1]
    f0 <:- f0 <+> f1
    return f1

prop_fib :: NonNegative Integer -> Bool
prop_fib (NonNegative n) = fibSeq !! fromIntegral n == fib n

prop_const :: NonNegative Integer -> Integer -> Bool
prop_const (NonNegative n) v = runLinearRecursive (getConstant v) n == v

prop_step :: NonNegative Integer -> Bool
prop_step (NonNegative n) = runLinearRecursive getStep n == n

prop_steppower :: NonNegative Integer -> Integer -> Bool
prop_steppower (NonNegative n) a = runLinearRecursive (getStepPower a) n == a ^ n
