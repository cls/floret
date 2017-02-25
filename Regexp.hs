module Regexp
where

import Algebra
import Matrix

import Prelude hiding (null, last)

data Regexp c a = Weight a
                | Letter c
                | Choice (Regexp c a) (Regexp c a)
                | Concat (Regexp c a) (Regexp c a)
                | Kleene (Regexp c a)

letters :: Regexp c a -> [c]
letters (Weight _)   = []
letters (Letter x)   = [x]
letters (Choice r s) = letters r ++ letters s
letters (Concat r s) = letters r ++ letters s
letters (Kleene s)   = letters s

-- Functions for defining extended Glushkov automata.

null :: StarSemiring a => Regexp c a -> a
null (Weight k)   = k
null (Letter _)   = zero
null (Choice r s) = null r <+> null s
null (Concat r s) = null r <.> null s
null (Kleene s)   = star (null s)

first :: StarSemiring a => Regexp c a -> Row a
first (Weight _)   = []
first (Letter _)   = [unit]
first (Choice r s) = first r ++ first s
first (Concat r s) = first r ++ map (null r <.>) (first s)
first r@(Kleene s) = map (null r <.>) (first s)

last :: StarSemiring a => Regexp c a -> Column a
last (Weight _)   = []
last (Letter _)   = [unit]
last (Choice r s) = last r ++ last s
last (Concat r s) = map (<.> null s) (last r) ++ last s
last r@(Kleene s) = map (<.> null r) (last s)

follow :: StarSemiring a => Regexp c a -> Matrix a
follow (Weight _)   = []
follow (Letter _)   = [[unit]]
follow (Choice r s) = let r' = follow r
                          s' = follow s
                      in blockAD r' s'
follow (Concat r s) = let r' = follow r
                          s' = follow s
                      in blockABD r' (last r >< first s <.> s') s'
follow (Kleene s)   = let s' = follow s
                      in s' <+> (last s >< first s <.> s')

-- Glushkov automata as matrices over a semiring.

initial :: StarSemiring a => Regexp c a -> Row a
initial r = unit : map (const zero) (letters r)

final :: StarSemiring a => Regexp c a -> Column a
final r = null r : last r

delta :: (Eq c, StarSemiring a) => Regexp c a -> c -> Matrix a
delta r = let m = first r : follow r
              xs = letters r
          in \y -> let weigh = zipWith (<.>) (map (fromBool . (== y)) xs)
                   in mapRows ((zero :) . weigh) m
