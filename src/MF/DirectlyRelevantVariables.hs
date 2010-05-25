{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MF.DirectlyRelevantVariables where

import           Data.Maybe
import qualified Data.IntMap    as Map
import qualified Data.Set       as Set

import MF.Program

import MF.Analysis

data DirectlyRelevantVariables = DirectlyRelevantVariables Label (Set.Set SymbolType)

instance Analysis DirectlyRelevantVariables SymbolType where
    flowSelection     _ program = (reverseFlow . flow) program
    extremalLabels    (DirectlyRelevantVariables startLabel startValue) program  = [startLabel]
    extremalValue     (DirectlyRelevantVariables startLabel startValue)          = startValue
    join              _         = Set.union
    isMoreInformative _         = Set.isSubsetOf
    transfer          _ statement input = (input `Set.difference` (kill statement)) `Set.union` (generate statement input)
            where 
                    kill :: Statement -> Set.Set SymbolType
                    kill = defined
                    
                    generate :: Statement -> Set.Set SymbolType -> Set.Set SymbolType
                    {-generate (AssignmentStatement c expr) input | Set.member c input = freeVariables expr
                                                                | otherwise          = Set.empty
                    generate (ExpressionStatement expr) input = Set.empty-}
                    generate st input | Set.null (input `Set.intersection` (defined st))    = Set.empty
                                      | otherwise                                   = referenced st
          







