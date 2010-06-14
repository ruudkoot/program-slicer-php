{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MF.DirectlyRelevantVariables where

import           Data.Maybe
import qualified Data.Set as Set

import MF.Program
import MF.Analysis

data DirectlyRelevantVariables = DirectlyRelevantVariables

instance Analysis DirectlyRelevantVariables SymbolType where
    flowSelection     _ program = (reverseFlow . flow) program
    join              _         = Set.union

    transferParametersIn _ ipfparams old = foldr varfunc Set.empty ipfparams
        where varfunc (c, i, _) rest = if Set.member c old
                                         then Set.insert i rest
                                         else rest

    transferParametersOut  _ ipfparams old = foldr varfunc Set.empty ipfparams
        where varfunc (c, i, ref) rest = if Set.member i old
                                          then Set.insert c rest 
                                          else rest
    
    transferFuncMerge _ = Set.union
    
    cutoff _ = 2
    
    kill     _ (FuncCall _ _) _ = Set.empty
    kill     _ (FuncIn _ _) _   = Set.empty
    kill     _ (FuncBack _ ) _  = Set.empty
    kill     _ (Return) _       = Set.empty
    kill     _ statement _      = modified statement                  
    
    generate _ (FuncCall _ _) _ = Set.empty
    generate _ (FuncIn _ _) _   = Set.empty
    generate _ (FuncBack _ ) _  = Set.empty
    generate _ (Return) _       = Set.empty
    generate _ statement input  | Set.null (input `Set.intersection` (modified statement))  = Set.empty
                                | otherwise                                                 = referenced statement
          





