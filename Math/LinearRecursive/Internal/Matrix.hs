
module Math.LinearRecursive.Internal.Matrix 
  ( Matrix 
  , matrix
  , diagonal
  , unMatrix
  , unDiagonal
  , toMatrix
  , unMatrix'
  , matrixSize
  ) where

import Data.List (transpose, intercalate)

data Matrix a = Matrix { unMatrix :: [[a]] }
              | Diagonal { unDiagonal :: a }
    deriving Eq

instance (Num a, Show a) => Show (Matrix a) where
    show (Matrix a) = "Matrix " ++ show a
    show (Diagonal a) = "Diagonal " ++ show a

toMatrix :: Num a => Matrix a -> Matrix a
toMatrix (Matrix a) = Matrix a
toMatrix (Diagonal a) = Matrix [replicate i 0 ++ [aii] ++ repeat 0 | (i, aii) <- zip [0..] (repeat a)]

unMatrix' :: Num a => Matrix a -> [[a]]
unMatrix' = unMatrix . toMatrix

matrixSize :: Num a => Matrix a -> Maybe Int
matrixSize (Diagonal _) = Nothing
matrixSize (Matrix a) = Just (length a)

matrix :: [[a]] -> Matrix a
matrix = Matrix

diagonal :: a -> Matrix a
diagonal = Diagonal

instance Num a => Num (Matrix a) where
    Diagonal a + Diagonal b = Diagonal (a + b)
    Matrix a + Matrix b = Matrix (zipWith (zipWith (+)) a b)
    Matrix a + Diagonal b = Matrix [ [if i == j then aij + b else aij | (j, aij) <- zip [0..] ai]
                                   | (i, ai) <- zip [0..] a
                                   ]
    Diagonal b + Matrix a = Matrix [ [if i == j then b + aij else aij | (j, aij) <- zip [0..] ai]
                                   | (i, ai) <- zip [0..] a
                                   ]

    negate (Matrix a) = Matrix (map (map negate) a)
    negate (Diagonal a) = Diagonal (negate a)

    fromInteger = Diagonal . fromInteger

    Matrix a * Matrix b = let tb = transpose b
                              c = [[sum (zipWith (*) ra cb) | cb <- tb] | ra <- a]
                          in
                              Matrix c
    Diagonal a * Diagonal b = Diagonal (a * b)
    Diagonal a * Matrix b = Matrix ((map.map) (a*) b)
    Matrix a * Diagonal b = Matrix ((map.map) (*b) a)

    abs = error "Matrix: abs undefined"
    signum = error "Matrix: abs undefined"

