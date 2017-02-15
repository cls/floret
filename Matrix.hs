module Matrix
where

import Algebra

import Data.List (transpose)

infixr 5 <:>
infixl 6 <\>
infixr 6 </>

-- Vectors and matrices.

newtype Row a = Row { getRow :: [a] }
newtype Column a = Column { getColumn :: [a] }
newtype Matrix a = Matrix [Row a]

mapRows :: (Row a -> b) -> Matrix a -> Column b
mapRows f (Matrix rows) = Column (map f rows)

mapCols :: (Column a -> b) -> Matrix a -> Row b
mapCols f (Matrix rows) = Row $ map (f . Column) $ transpose $ map getRow rows

prettyMatrix :: Show a => Matrix a -> String
prettyMatrix (Matrix rows) = unlines $ map (bracket . unwords . map (fill . show) . getRow) rows
  where
    mx = maximum $ concatMap (\(Row xs) -> map (length . show) xs) rows
    fill str = replicate (mx - length str) ' ' ++ str
    bracket str = "( " ++ str ++ " )"

-- Multiplication of vectors and matrices over a semiring.

(<:>) :: Semiring a => Row a -> Column a -> a
Row v <:> Column w = foldr (<+>) zero $ zipWith (<.>) v w

(<\>) :: Semiring a => Row a -> Matrix a -> Row a
v <\> m = mapCols (v <:>) m

(</>) :: Semiring a => Matrix a -> Column a -> Column a
m </> v = mapRows (<:> v) m

instance Semiring a => Semigroup (Matrix a) where
  m <> n = let Column mn = mapRows (<\> n) m in Matrix mn
