{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module MF.DirectlyRelevantVariables where

import           Data.Maybe
import qualified Data.Set    as Set

import MF.Program
import MF.Analysis

data DirectlyRelevantVariables = DirectlyRelevantVariables

instance Analysis DirectlyRelevantVariables SymbolType where
    flowSelection     _ program          = (reverseFlow . flow) program
    join              _                  = Set.union
    isMoreInformative _                  = Set.isSubsetOf    
    kill              _ statement _      = modified statement                  
    generate          _ statement input  | Set.null (input `Set.intersection` (modified statement)) = Set.empty
                                         | otherwise                                                = referenced statement

