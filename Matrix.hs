module Matrix
where

import Algebra

import Data.Coerce (coerce)
import Data.List (transpose)

infixr 5 <:>
infixl 6 <\>
infixr 6 </>

-- Vectors and matrices.

newtype Row a = Row [a]
newtype Column a = Column [a]
newtype Matrix a = Matrix { rows :: [[a]] }

mapRows :: (Row a -> b) -> Matrix a -> Column b
mapRows f = Column . map (f . Row) . rows

mapCols :: (Column a -> b) -> Matrix a -> Row b
mapCols f = Row . map (f . Column) . transpose . rows

mapElems :: (a -> b) -> Matrix a -> Matrix b
mapElems f = Matrix . map (map f) . rows

-- Multiplication of vectors and matrices over a semiring.

(<:>) :: Semiring a => Row a -> Column a -> a
Row v <:> Column w = foldr (<+>) zero $ zipWith (<.>) v w

prop_singleInner :: (Eq a, Semiring a) => a -> a -> Bool
prop_singleInner x y = (Row [x] <:> Column [y]) == (x <.> y)

(<\>) :: Semiring a => Row a -> Matrix a -> Row a
v <\> m = mapCols (v <:>) m

prop_singleRowMul :: (Eq a, Semiring a) => Row a -> Column a -> Bool
prop_singleRowMul v w@(Column xs) = let Row [z] = v <\> Matrix (map return xs)
                                    in z == (v <:> w)

(</>) :: Semiring a => Matrix a -> Column a -> Column a
m </> v = mapRows (<:> v) m

prop_singleColumnMul :: (Eq a, Semiring a) => Row a -> Column a -> Bool
prop_singleColumnMul v@(Row xs) w = let Column [z] = Matrix (return xs) </> w
                                    in z == (v <:> w)

instance Semiring a => Semigroup (Matrix a) where
  m <> n = coerce $ mapRows (<\> n) m

prop_singleMatrixMul :: (Eq a, Semiring a) => Row a -> Column a -> Bool
prop_singleMatrixMul v@(Row xs) w@(Column ys) = let Matrix [[z]] = Matrix (return xs) <> Matrix (map return ys)
                                                in z == (v <:> w)
