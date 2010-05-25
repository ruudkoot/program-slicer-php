{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MF.Main where

import qualified Data.Set as Set  
import qualified Data.Map as Map

import Data.Maybe
import MF.Expressions
import MF.Statements
import Debug.Trace

import MF.Analysis
import MF.LiveVariableAnalysis
import MF.DirectlyRelevantVariables

-- Define the input, Life variable analysis, zie boek
{-

program :: Program
program = Program blocks flow startLabel finalLabels
        where 
              blocks = Map.fromList 
                        [         (1, AssignmentStatement 'x' (Literal 2))
                                , (2, AssignmentStatement 'y' (Literal 4))
                                , (3, AssignmentStatement 'x' (Literal 1))
                                , (4, ExpressionStatement (GreaterThan (Symbol 'y') (Symbol 'x')))
                                , (5, AssignmentStatement 'z' (Symbol 'y'))
                                , (6, AssignmentStatement 'z' (Multiplication (Symbol 'y') (Symbol 'y')))
                                , (7, AssignmentStatement 'x' (Symbol 'z'))
                        ]
                       
              flow = [ (1, 2)
                     , (2, 3)
                     , (3, 4)
                     , (4, 5)
                     , (4, 6)
                     , (5, 7)
                     , (6, 7)
                     ]
              startLabel = 1
              finalLabels = [7]

-}

-- Sample program for program slicing, zie 'A Survey of Program Slicing Techniques' blz 2.             
program :: Program
program = Program blocks flow startLabel finalLabels rangeOfInfluence
        where 
              blocks = Map.fromList 
                        [         (1, AssignmentStatement 'n' (Literal 2)) -- Zie read als een assignment!
                                , (2, AssignmentStatement 'i' (Literal 1))
                                , (3, AssignmentStatement 's' (Literal 0))
                                , (4, AssignmentStatement 'p' (Literal 1))
                                , (5, ExpressionStatement (GreaterThan (Symbol 'n') (Symbol 'i')))
                                , (6, AssignmentStatement 's' (Addition (Symbol 's') (Symbol 'i')))
                                , (7, AssignmentStatement 'p' (Multiplication (Symbol 'p') (Symbol 'i')))
                                , (8, AssignmentStatement 'i' (Addition (Symbol 'i') (Literal 1)))
                                , (9, ExpressionStatement (Symbol 's'))
                                , (10, ExpressionStatement (Symbol 'p'))
                        ]
                       
              flow = [ (1, 2)
                     , (2, 3)
                     , (3, 4)
                     , (4, 5)
                     , (5, 6)
                     , (6, 7)
                     , (7, 8)
                     , (5, 9)
                     , (9, 10)
                     , (8, 5) -- Loop
                     ]
              startLabel = 1
              finalLabels = [10]
              rangeOfInfluence = Map.fromList 
                        [
                                  (1, Set.empty)
                                , (2, Set.empty)
                                , (3, Set.empty)
                                , (4, Set.empty)
                                , (5, Set.fromList [6,7,8])
                                , (6, Set.empty)
                                , (7, Set.empty)
                                , (8, Set.empty)
                                , (9, Set.empty)
                                , (10, Set.empty)
                        ]



-- Uitvoer is de LVexit (pagina 52 NNH)
main::IO()
-- main = print $ solve lifeVariableAnalysis program
main = print $ backwardsProgramSlicing program 10 $ Set.fromList ['p']

{-main = print $ relevantVariables -- controlDependentBranchStatements program relevantStatements
        where 
                analysis = directlyRelevantVariables 5 $ Set.fromList ['i', 'n']
                
                relevantVariables = (transferAll analysis) program (solve analysis program)
                relevantStatements = directlyRelevantStatements program relevantVariables 
-}








