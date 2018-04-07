{- Copyright (C) 2017  Connor Lane Smith -}
module Tree
where

import Algebra
import Matrix

-- Trees annotated by a semigroup.

data Tree c a = Leaf c a
              | Branch (Tree c a) a a (Tree c a)

size :: Semigroup a => Tree c a -> a
size (Leaf _ x)       = x
size (Branch _ x y _) = x <> y

label :: Semigroup a => (c -> a) -> Tree c b -> Tree c a
label f (Leaf c _)       = Leaf c (f c)
label f (Branch l _ _ r) = let l' = label f l
                               r' = label f r
                           in Branch l' (size l') (size r') r'

-- Trees annotated by matrices under multiplication.

type MatrixTree c a = Tree c (Product (Matrix a))

-- TODO: What properties does this require of a semiring?
truncateDot :: (Ord a, Semiring a) => Row a -> Column a -> a
truncateDot a z = foldr (<+>) zero $ zipWith (\x y -> x `min` (x <.> y)) a z

find :: (Ord a, Semiring a) => a -> Row a -> MatrixTree c a -> Column a -> Maybe c
find k a (Leaf c m)       z = let am = a <\> getProduct m
                                  i = truncateDot am z
                              in if k <= i then Just c
                                           else Nothing
find k a (Branch l m n r) z = let am = a <\> getProduct m
                                  nz = getProduct n </> z
                                  i = truncateDot am nz
                              in if k <= i then find k a l nz
                                           else find k am r z
