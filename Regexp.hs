module Regexp
where

import Algebra
import Matrix

import Prelude hiding (null, last)

data Regexp c a = Weight a
                | Letter (c -> a)
                | Choice (Regexp c a) (Regexp c a)
                | Concat (Regexp c a) (Regexp c a)
                | Kleene (Regexp c a)

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

follow :: StarSemiring a => Regexp c a -> c -> Matrix a
follow (Weight _)   _ = []
follow (Letter f)   c = [[f c]]
follow (Choice r s) c = let r' = follow r c
                            s' = follow s c
                        in blockAD r' s'
follow (Concat r s) c = let r' = follow r c
                            s' = follow s c
                        in blockABD r' ((last r >< first s) <> s') s'
follow (Kleene s)   c = let s' = follow s c
                        in zipElemsWith (<+>) s' ((last s >< first s) <> s')
