module Algebra.CompleteLatticeWithACC where

class CompleteLatticeWithACC a where
    bottom  :: a
    top     :: a
    join    :: a -> a -> a
    meet    :: a -> a -> a
    joinAll :: [a] -> a
    joinAll = foldl join bottom
    meetAll :: [a] -> a
    meetAll = foldl meet top

