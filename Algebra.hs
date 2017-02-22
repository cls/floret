module Algebra
where

-- Semigroups.

class Semigroup a where
  (<>) :: a -> a -> a

-- Semirings.

class Semiring a where
  zero :: a
  unit :: a
  (<+>) :: a -> a -> a
  (<.>) :: a -> a -> a

class Semiring a => StarSemiring a where
  star :: a -> a

-- Boolean algebra forms a star-semiring.

instance Semiring Bool where
  zero = False
  unit = True
  (<+>) = (||)
  (<.>) = (&&)

instance StarSemiring Bool where
  star = const True

fromBool :: Semiring a => Bool -> a
fromBool False = zero
fromBool True  = unit

toBool :: (Eq a, Semiring a, Semiring b) => a -> b
toBool = fromBool . (/= zero)
