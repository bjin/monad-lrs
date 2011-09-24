
module Math.LinearRecursive.Internal.Matrix 
  ( Matrix 
  , matrix
  , diagonal
  , toMatrix
  , unMatrix'
  ) where

import Data.List (transpose)

data Matrix a = Matrix { unMatrix :: [[a]] }
              | Diagonal { unDiagonal :: [a] }
    deriving (Show, Eq)

toMatrix :: Num a => Matrix a -> Matrix a
toMatrix (Matrix a) = Matrix a
toMatrix (Diagonal a) = Matrix [replicate i 0 ++ [aii] ++ repeat 0 | (i, aii) <- zip [0..] a]

unMatrix' :: Num a => Matrix a -> [[a]]
unMatrix' = unMatrix . toMatrix

matrix :: [[a]] -> Matrix a
matrix = Matrix

diagonal :: [a] -> Matrix a
diagonal = Diagonal

instance Num a => Num (Matrix a) where
    Diagonal a + Diagonal b = diagonal (zipWith (+) a b)
    a + b = matrix (zipWith (zipWith (+)) (unMatrix' a) (unMatrix' b))

    negate (Matrix a) = matrix (map (map negate) a)
    negate (Diagonal a) = diagonal (map negate a)

    fromInteger = diagonal . repeat . fromInteger

    Matrix a * Matrix b = let tb = transpose b
                              c = [[sum (zipWith (*) ra cb) | cb <- tb] | ra <- a]
                          in
                              matrix c
    Diagonal a * Diagonal b = diagonal (zipWith (*) a b)
    Diagonal a * Matrix b = matrix (zipWith (\v row -> map (v*) row) a b)
    Matrix a * Diagonal b = matrix (map (\row -> zipWith (*) row b) a)

    abs = error "Matrix: abs undefined"
    signum = error "Matrix: abs undefined"

