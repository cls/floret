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

(<\>) :: Semiring a => Row a -> Matrix a -> Row a
v <\> m = mapCols (v <:>) m

(</>) :: Semiring a => Matrix a -> Column a -> Column a
m </> v = mapRows (<:> v) m

instance Semiring a => Semigroup (Matrix a) where
  m <> n = coerce $ mapRows (<\> n) m
