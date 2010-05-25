{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MF.Analysis where

import           Data.Maybe
import qualified Data.IntMap   as Map
import qualified Data.Set   as Set

import MF.Program

type Worklist         = [(Label, Label)]
type Context property = Map.IntMap (Set.Set property)

class Analysis analysis property | analysis -> property where
    flowSelection          :: analysis -> Program -> Flow
    extremalLabels         :: analysis -> Program -> [Label]
    extremalValue          :: analysis -> Set.Set property
    join                   :: analysis -> Set.Set property -> Set.Set property -> Set.Set property
    isMoreInformative      :: analysis -> Set.Set property -> Set.Set property -> Bool
    transfer               :: analysis -> Statement -> Set.Set property -> Set.Set property
    transferAll            :: analysis -> Program -> Context property -> Context property
    transferAll       analysis program context = Map.mapWithKey toEffect context
            where
                    toEffect :: Label -> Set.Set property -> Set.Set property
                    toEffect label input = transfer analysis (statementAt program label) input

    solve :: analysis -> Program -> Context property
    solve analysis program = let flow     = flowSelection analysis program
                                 worklist = flow
                                 context  = Map.fromList (map transform (unique flow))
                                          where -- Todo replace this with labels function
                                                unique :: Flow -> [Label]
                                                unique flow = (Set.toList . Set.fromList) ((fst . unzip) flow ++ (snd . unzip) flow) 
                                                transform label | isExtremalLabel = (label, extremalValue analysis)
                                                                | otherwise       = (label, Set.empty)
                                                                where isExtremalLabel = label `elem` extremalLabels analysis program
                              in solve' analysis program worklist context
                                 where solve' :: analysis -> Program -> Worklist -> Context property -> Context property  
                                       solve' analysis program [] context = context
                                       solve' analysis program ((start, end):worklistTail) context = let newWorklist     = if moreInformative
                                                                                                                           then worklistTail
                                                                                                                           else worklistTail ++ [(l', l'') | (l', l'') <- flow, l' == end] 
                                                                                                         newContext      = if moreInformative
                                                                                                                           then context
                                                                                                                           else Map.insert end (join analysis (fromJust (Map.lookup end context)) effect) context  
                                                                                                         effect          = transfer analysis (fromJust (Map.lookup start (blocks program))) (fromJust (Map.lookup start context))
                                                                                                         moreInformative = isMoreInformative analysis effect (fromJust (Map.lookup end context))
                                                                                                         flow            = flowSelection analysis program
                                                                                                      in solve' analysis program newWorklist newContext
