module MF.Main where

import qualified Data.Set as Set

import MF.Program

-- Uitvoer is de LVexit (pagina 52 NNH)
main :: IO ()
main = print $ backwardsProgramSlicing program 10 $ Set.fromList ['p']
