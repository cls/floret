module Algebra
where

-- Semigroups.

class Semigroup a where
  (<>) :: a -> a -> a

-- Ringoids.

class Ringoid a where
  (<+>) :: a -> a -> a
  (<.>) :: a -> a -> a

newtype Sum a = Sum { getSum :: a }

instance Ringoid a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x <+> y)

newtype Product a = Product { getProduct :: a }

instance Ringoid a => Semigroup (Product a) where
  Product x <> Product y = Product (x <.> y)

-- Semirings.

class Ringoid a => Semiring a where
  zero :: a
  unit :: a

class Semiring a => StarSemiring a where
  star :: a -> a

-- Boolean algebra forms a star-semiring.

instance Ringoid Bool where
  (<+>) = (||)
  (<.>) = (&&)

instance Semiring Bool where
  zero = False
  unit = True

instance StarSemiring Bool where
  star = const True

fromBool :: Semiring a => Bool -> a
fromBool False = zero
fromBool True  = unit

toBool :: (Eq a, Semiring a, Semiring b) => a -> b
toBool = fromBool . (/= zero)
