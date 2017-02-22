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

prop_singleMatrixMul :: (Eq a, Semiring a) => Row a -> Column a -> Bool
prop_singleMatrixMul v w = let [[z]] = row v <> column w
                           in z == v <:> w
