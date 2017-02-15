module Tree
where

import Algebra
import Matrix

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

find :: (Ord a, Semiring a) => a -> Row a -> Tree c (Matrix a) -> Column a -> Maybe c
find k a (Leaf c n)       z = let i = a <:> booleanise (n </> z)
                              in if i >= k then Just c
                                           else Nothing
find k a (Branch l m n r) z = let am = a <\> m
                                  nz = n </> z
                                  i = am <:> booleanise nz
                              in if i >= k then find k a l nz
                                           else find k am r z

booleanise :: (Eq a, Semiring a) => Column a -> Column a
booleanise (Column xs) = Column (map bool xs)
  where
    bool x = if x == zero then zero else unit -- is this a hack?
