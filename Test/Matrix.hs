module Test.Matrix
where

import Algebra
import Matrix

-- Multiplication of vectors and matrices over a semiring.

prop_singleInner :: (Eq a, Semiring a) => a -> a -> Bool
prop_singleInner x y = [x] <:> [y] == x <.> y

prop_singleRowMul :: (Eq a, Semiring a) => Row a -> Column a -> Bool
prop_singleRowMul v w = let [z] = v <\> column w
                        in z == v <:> w

prop_singleColumnMul :: (Eq a, Semiring a) => Row a -> Column a -> Bool
prop_singleColumnMul v w = let [z] = row v </> w
                           in z == v <:> w

prop_matrixInner :: (Eq a, Semiring a) => Row a -> Column a -> Bool
prop_matrixInner v w = case row v <.> column w of
                         [[z]] -> z == v <:> w
                         [[ ]] -> null v || null w

prop_matrixOuter :: (Eq a, Semiring a) => Column a -> Row a -> Bool
prop_matrixOuter v w = v >< w == column v <.> row w

prop_matrixAddId :: (Eq a, Semiring a) => Matrix a -> Bool
prop_matrixAddId m = m <+> mapElems (const zero) m == m
