
module Math.LinearRecursive
  ( LinearRecursive
  , newVariable
  , newVariables
  , getConstant
  , (<+-)
  , (<:-)
  , runLinearRecursive
  , VectorLike
  , LinearDependency
  , Variable
  , (<+>)
  , (<->)
  , (<*)
  , (*>)
  , zeroVector
  ) where

import Control.Monad (zipWithM_)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Math.LinearRecursive.Internal.Vector
import Math.LinearRecursive.Internal.Matrix

type LinearDependency = Vector
type Variable = Vector1

data LRVariable a = LRV { initialValue :: a, dependency :: LinearDependency a }

dmap :: Num a => (LinearDependency a -> LinearDependency a) -> LRVariable a -> LRVariable a
dmap f (LRV val dep) = LRV val (f dep)

type LRVariables a = IntMap (LRVariable a)

data LinearRecursive a b = LR { unLR :: Int -> (b, Int, LRVariables a -> LRVariables a) }

instance Num a => Monad (LinearRecursive a) where
    return a = LR (const (a, 0, id))
    a >>= b = LR $ \v -> let (ra, nva, ma) = unLR a v
                             (rb, nvb, mb) = unLR (b ra) (v + nva)
                         in
                             (rb, nva + nvb, mb . ma)

newVariable :: Num a => a -> LinearRecursive a (Variable a)
newVariable val0 = LR $ \v -> (vector1 v, 1, IntMap.insert v variable)
  where
    variable = LRV { initialValue = val0, dependency = zeroVector }

newVariables :: Num a => [a] -> LinearRecursive a [Variable a]
newVariables vals = do
    ret <- mapM newVariable vals
    zipWithM_ (<:-) (tail ret) ret
    return ret

getConstant :: Num a => a -> LinearRecursive a (LinearDependency a)
getConstant val = do
    ret <- newVariable val
    ret <:- ret
    return (toVector ret)

(<+-) :: (Num a, VectorLike v) => Variable a -> v a -> LinearRecursive a ()
(<+-) var dep = LR (const ((), 0, IntMap.adjust (dmap (<+>toVector dep)) (unVector1 var)))

(<:-) :: (Num a, VectorLike v) => Variable a -> v a -> LinearRecursive a ()
(<:-) var dep = LR (const ((), 0, IntMap.adjust (dmap (const (toVector dep))) (unVector1 var)))

infix 1 <:-,<+-

buildMatrix :: Num a => LRVariables a -> (Matrix a, Matrix a)
buildMatrix mapping = (matrix trans, matrix $ map (: []) initValues)
  where
    initValues = map initialValue (IntMap.elems mapping)
    rawDep = map (unVector'.dependency) (IntMap.elems mapping)
    varCount = length initValues
    trans = map (\m -> [IntMap.findWithDefault 0 i m | i <- [0..varCount-1]]) rawDep

runLinearRecursive :: (Num a, Integral b, VectorLike v) => LinearRecursive a (v a) -> b -> a
runLinearRecursive monad steps = sum [head (res !! i) * ai | (i, ai) <- IntMap.assocs (unVector' target)]
  where
    (target, nv, g) = unLR monad 0 
    dep = g IntMap.empty
    (trans, init) = buildMatrix dep

    res = unMatrix' (trans^steps * init)
