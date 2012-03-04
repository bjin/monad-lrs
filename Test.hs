
module Main (main) where

import qualified Math.LinearRecursive.Internal.Matrix as M
import qualified Math.LinearRecursive.Internal.Vector as V
import qualified Math.LinearRecursive.Internal.Polynomial as P
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
          , testProperty "inverse" prop_matrix_inverse
          ]
        , testGroup "vector"
          [ testProperty "add0" prop_vector_add0
          , testProperty "mul1" prop_vector_mul1
          , testProperty "add" prop_vector_add
          , testProperty "mul" prop_vector_mul
          , testProperty "sub" prop_vector_sub
          ]
        , testGroup "polynomial"
          [ testProperty "add0" prop_poly_add0
          , testProperty "mul1" prop_poly_mul1
          , testProperty "add" prop_poly_add
          , testProperty "mul" prop_poly_mul
          , testProperty "list" prop_poly_list
          ]
        , testGroup "linear recursive monad"
          [ testProperty "fib" prop_fib
          , testProperty "const" prop_const
          , testProperty "step" prop_step
          , testProperty "powerof" prop_powerof
          , testProperty "poly" prop_poly
          , testProperty "partwith" prop_partwith
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

newtype Diag1Matrix a = Diag1 (M.Matrix a) deriving Show


instance (Num a, Arbitrary a) => Arbitrary (Diag1Matrix a) where
    arbitrary = do
        n <- choose (1, 20) 
        Diag1 . cutMatrix <$> vectorOf n (vectorOf n arbitrary)
      where
        cutMatrix ma = M.matrix [[if i == j then 1 else if i > j then 0 else aij | (j, aij) <- zip [0..] ri] | (i, ri) <- zip [0..] ma]
    shrink a = []

prop_matrix_inverse :: Diag1Matrix Integer -> Bool
prop_matrix_inverse (Diag1 ma) = ma' * ma == one
  where
    ma' = M.inverseMatrixDiag1 ma
    n = length (M.unMatrix ma)
    one = M.matrix [[if i == j then 1 else 0 | j <- [1..n]] | i <- [1..n]]

instance (Eq a, Num a, Arbitrary a) => Arbitrary (V.Vector1 a) where
    arbitrary = V.vector1 <$> choose (0, 30)
    shrink a = [V.vector1 a' | a' <- shrink (V.unVector1 a), a' >= 0]

instance (Eq a, Num a, Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = V.vector . IntMap.fromListWith (+) <$> listOf ((,) <$> choose (0,30) <*> arbitrary)
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

instance (Eq a, Num a, Arbitrary a) => Arbitrary (P.Polynomial a) where
    arbitrary = fmap P.polynomial arbitrary
    shrink = map P.polynomial . shrink . P.unPoly

prop_poly_add0 :: P.Polynomial Rational -> Bool
prop_poly_add0 a = a + 0 == a && 0 + a == a

prop_poly_mul1 :: P.Polynomial Rational -> Bool
prop_poly_mul1 a = a * 1 == a && 1 * a == a

prop_poly_add :: P.Polynomial Rational -> P.Polynomial Rational -> P.Polynomial Rational -> Bool
prop_poly_add a b c = (a + b) + c == a + (b + c) && a + b == b + a

prop_poly_mul :: P.Polynomial Integer -> P.Polynomial Integer -> P.Polynomial Integer -> Bool
prop_poly_mul a b c = (a * b) * c == a * (b * c) && a * b == b * a

prop_poly_list :: P.Polynomial Rational -> Bool
prop_poly_list a = a == P.fromList (P.toList a)

fibSeq :: [Integer]
fibSeq = 1 : 1 : zipWith (+) fibSeq (tail fibSeq)

fib :: Integer -> Integer
fib n = flip runLinearRecursive n fibmonad

fibmonad :: LinearRecursive Integer (Variable Integer)
fibmonad = do
    [f0, f1] <- newVariables [1, 1]
    f0 <:- f0 <+> f1
    return f1

prop_fib :: NonNegative Integer -> Bool
prop_fib (NonNegative n) = fibSeq !! fromIntegral n == fib n

prop_const :: NonNegative Integer -> Integer -> Bool
prop_const (NonNegative n) v = runLinearRecursive (getConstant v) n == v

prop_step :: NonNegative Integer -> Bool
prop_step (NonNegative n) = runLinearRecursive getStep n == n

prop_powerof :: NonNegative Integer -> Integer -> Bool
prop_powerof (NonNegative n) a = runLinearRecursive (getPowerOf a) n == a ^ n

prop_poly :: Polynomial Integer -> NonNegative Integer -> Bool
prop_poly p (NonNegative v) = runLinearRecursive (getPolynomial p) v == P.evalPoly p v

arbMonad :: Gen ((LinearRecursive Integer (LinearCombination Integer), [Integer]))
arbMonad = oneof [ return (fmap toVector fibmonad, fibSeq)
                 , (\x -> (getPowerOf x, scanl (*) 1 (repeat x))) <$> arbitrary
                 , (\p -> (getPolynomial p, map (P.evalPoly p) [0..])) <$> arbitrary
                 ]

prop_partwith :: Polynomial Integer -> NonNegative Integer -> Gen Bool
prop_partwith p (NonNegative n) = do
    (monad, seq) <- arbMonad
    let lhs = runLinearRecursive (monad >>= getPartialSumWith p) n
    let rhs = sum [P.evalPoly p (n - i) * fi | (i, fi) <- zip [0..n] seq]
    return $ lhs == rhs
