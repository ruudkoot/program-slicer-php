{-# LANGUAGE MultiParamTypeClasses  #-}
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
    kill     _ statement _      = modified statement                  
    generate _ statement input  | Set.null (input `Set.intersection` (modified statement))  = Set.empty
                                | otherwise                                                 = referenced statement
          







