{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MF.LiveVariableAnalysis where

import           Data.Maybe
import qualified Data.Map   as Map
import qualified Data.Set   as Set

import MF.Program
import MF.Analysis

data LiveVariableAnalysis = LiveVariableAnalysis

instance Analysis LiveVariableAnalysis SymbolType where
    flowSelection     _ program = (reverseFlow . flow) program
    extremalLabels    _ program = finalLabels program
    extremalValue     _         = Set.empty
    join              _         = Set.union
    isMoreInformative _         = Set.isSubsetOf 
    
    kill (AssignmentStatement c expr)     = Set.fromList [c]
    kill (ExpressionStatement expr)       = Set.empty
                    
    generate :: Statement -> Set.Set SymbolType
    generate (AssignmentStatement c expr) = freeVariables expr
    generate (ExpressionStatement expr)   = freeVariables expr  

