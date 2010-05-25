{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MF.Main where

import qualified Data.Set as Set  
import qualified Data.Map as Map

import Data.Maybe
import MF.Program
import Debug.Trace

import MF.Analysis
import MF.LiveVariableAnalysis
import MF.DirectlyRelevantVariables

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








