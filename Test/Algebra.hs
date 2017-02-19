module Test.Algebra
where

import Algebra

-- Semigroups.

prop_semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
prop_semigroupAssoc x y z = (x <> (y <> z)) == ((x <> y) <> z)

-- Semirings.

prop_semiringCommute :: (Eq a, Semiring a) => a -> a -> Bool
prop_semiringCommute x y = (x <+> y) == (y <+> x)

prop_semiringAddAssoc :: (Eq a, Semiring a) => a -> a -> a -> Bool
prop_semiringAddAssoc x y z = (x <+> (y <+> z)) == ((x <+> y) <+> z)

prop_semiringMulAssoc :: (Eq a, Semiring a) => a -> a -> a -> Bool
prop_semiringMulAssoc x y z = (x <.> (y <.> z)) == ((x <.> y) <.> z)

prop_semiringAddIdLeft :: (Eq a, Semiring a) => a -> Bool
prop_semiringAddIdLeft x = (zero <+> x) == x

prop_semiringAddIdRight :: (Eq a, Semiring a) => a -> Bool
prop_semiringAddIdRight x = (x <+> zero) == x

prop_semiringMulIdLeft :: (Eq a, Semiring a) => a -> Bool
prop_semiringMulIdLeft x = (unit <.> x) == x

prop_semiringMulIdRight :: (Eq a, Semiring a) => a -> Bool
prop_semiringMulIdRight x = (x <.> unit) == x

prop_semiringDistLeft :: (Eq a, Semiring a) => a -> a -> a -> Bool
prop_semiringDistLeft x y z = (x <.> (y <+> z)) == ((x <.> y) <+> (x <.> z))

prop_semiringDistRight :: (Eq a, Semiring a) => a -> a -> a -> Bool
prop_semiringDistRight x y z = ((x <+> y) <.> z) == ((x <.> z) <+> (y <.> z))

prop_semiringAbsorbLeft :: (Eq a, Semiring a) => a -> Bool
prop_semiringAbsorbLeft x = (zero <.> x) == zero

prop_semiringAbsorbRight :: (Eq a, Semiring a) => a -> Bool
prop_semiringAbsorbRight x = (x <.> zero) == zero

prop_semiringStarLeft :: (Eq a, StarSemiring a) => a -> Bool
prop_semiringStarLeft x = star x == (unit <+> (x <.> star x))

prop_semiringStarRight :: (Eq a, StarSemiring a) => a -> Bool
prop_semiringStarRight x = star x == (unit <+> (star x <.> x))
