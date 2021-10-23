{- Copyright (C) 2017  Connor Lane Smith -}
{-# LANGUAGE FlexibleInstances #-}
module Matrix
where

import Algebra

import Data.List (transpose)

-- Vectors and matrices.

type Row a = [a]
type Column a = [a]
type Matrix a = [[a]]

mapRows :: (Row a -> b) -> Matrix a -> Column b
mapRows f = map f

mapCols :: (Column a -> b) -> Matrix a -> Row b
mapCols f = map f . transpose

mapElems :: (a -> b) -> Matrix a -> Matrix b
mapElems f = map (map f)

rows :: Matrix a -> Int
rows = length

columns :: Matrix a -> Int
columns = length . head

row :: Row a -> Matrix a
row = return

column :: Column a -> Matrix a
column = map return

-- Multiplication of vectors and matrices over a semiring.

(><) :: Semiring a => Column a -> Row a -> Matrix a
v >< w = column v <.> row w

(<:>) :: Semiring a => Row a -> Column a -> a
v <:> w = foldr (<+>) zero $ zipWith (<.>) v w

(<\>) :: Semiring a => Row a -> Matrix a -> Row a
v <\> m = mapCols (v <:>) m

(</>) :: Semiring a => Matrix a -> Column a -> Column a
m </> v = mapRows (<:> v) m

instance Semiring a => Ringoid (Matrix a) where
  m <.> n = mapRows (<\> n) m
  m <+> n = zipWith (zipWith (<+>)) m n

-- Miscellaneous functions for constructing matrices.

blockAD :: Semiring a => Matrix a -> Matrix a -> Matrix a
blockAD a d = let as = map (++ replicate (columns d) zero) a
                  ds = map (replicate (columns a) zero ++) d
              in as ++ ds

blockABD :: Semiring a => Matrix a -> Matrix a -> Matrix a -> Matrix a
blockABD a b d = let abs = zipWith (++) a b
                     ds  = map (replicate (columns a) zero ++) d
                 in abs ++ ds
