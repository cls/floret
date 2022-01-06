{- Copyright (C) 2017  Connor Lane Smith -}
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

class StarSemiring a => Kleene a where
  leq :: a -> a -> Bool

--- Boolean algebra forms a Kleene algebra.

instance Ringoid Bool where
  (<+>) = (||)
  (<.>) = (&&)

instance Semiring Bool where
  zero = False
  unit = True

instance StarSemiring Bool where
  star = const True

instance Kleene Bool where
  leq = (<=)
