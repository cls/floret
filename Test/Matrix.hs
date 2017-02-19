module Test.Matrix
where

import Algebra
import Matrix

-- Multiplication of vectors and matrices over a semiring.

prop_singleInner :: (Eq a, Semiring a) => a -> a -> Bool
prop_singleInner x y = (Row [x] <:> Column [y]) == (x <.> y)

prop_singleRowMul :: (Eq a, Semiring a) => Row a -> Column a -> Bool
prop_singleRowMul v w@(Column xs) = let Row [z] = v <\> Matrix (map return xs)
                                    in z == (v <:> w)

prop_singleColumnMul :: (Eq a, Semiring a) => Row a -> Column a -> Bool
prop_singleColumnMul v@(Row xs) w = let Column [z] = Matrix (return xs) </> w
                                    in z == (v <:> w)

prop_singleMatrixMul :: (Eq a, Semiring a) => Row a -> Column a -> Bool
prop_singleMatrixMul v@(Row xs) w@(Column ys) = let Matrix [[z]] = Matrix (return xs) <> Matrix (map return ys)
                                                in z == (v <:> w)
