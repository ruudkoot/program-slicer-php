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
        where varfunc (actual, formal, ref) rest = if ref && Set.member actual old
                                                    then Set.insert formal rest
                                                    else rest

    transferParametersOut  _ ipfparams old = foldr varfunc Set.empty ipfparams
        where varfunc (actual, formal, ref) rest = if Set.member formal old
                                                    then Set.insert actual rest 
                                                    else rest
    
    transferParametersThrough _ ipfparams old = old Set.\\ foldr reffed Set.empty ipfparams
        where reffed (actual, formal, ref) rest = if ref
                                                  then Set.insert actual rest
                                                  else rest
       
    transferFuncMerge _ = Set.union
    
    defaultFunction analysis args old = Set.fromList args `Set.union` old

    cutoff _ = 4
    
    kill     _ (FuncCall _ _) _ = Set.empty
    kill     _ (FuncIn _ _) _   = Set.empty
    kill     _ (FuncBack _ ) _  = Set.empty
    kill     _ (Return) _       = Set.empty
    kill     _ statement _      = Set.delete "@return" $ modified statement
    
    generate _ (FuncCall _ _) _ = Set.empty
    generate _ (FuncIn _ _) _   = Set.empty
    generate _ (FuncBack _ ) _  = Set.empty
    generate _ (Return) _       = Set.empty
    generate _ statement input  | Set.null (input `Set.intersection` (modified statement))  = Set.empty
                                | otherwise                                                 = referenced statement
          





