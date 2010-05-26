{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MF.Analysis where

import           Data.Maybe
import qualified Data.IntMap   as Map
import qualified Data.Set   as Set

import MF.Program
import Debug.Trace

type Worklist         = [(Label, Label)]
type Context property = Map.IntMap (Set.Set property)

class (Ord property, Show property) => Analysis analysis property | analysis -> property where
    flowSelection          :: analysis -> Program -> Flow
    
    extremalLabels         :: analysis -> Program -> [Label]
    extremalValue          :: analysis -> Set.Set property
    extremalValueAt        :: analysis -> Label -> Program -> Set.Set property
    extremalValueAt analysis label program | isExtremalLabel = extremalValue analysis
                                           | otherwise       = Set.empty
                                                where isExtremalLabel = label `elem` extremalLabels analysis program       

    join                   :: analysis -> Set.Set property -> Set.Set property -> Set.Set property
    isMoreInformative      :: analysis -> Set.Set property -> Set.Set property -> Bool

    kill                   :: analysis -> Statement -> Set.Set property -> Set.Set property
    generate               :: analysis -> Statement -> Set.Set property -> Set.Set property
    
    transfer               :: analysis -> Statement -> Set.Set property -> Set.Set property
    transfer          analysis statement input = (input `Set.difference` (kill analysis statement input)) `Set.union` (generate analysis statement input)

    transferAll            :: analysis -> Program -> Context property -> Context property
    transferAll       analysis program context = Map.mapWithKey toEffect context
            where
                    toEffect :: Label -> Set.Set property -> Set.Set property
                    toEffect label input = transfer analysis (statementAt program label) input

    solve :: analysis -> Program -> Context property
    solve analysis program = let flow     = flowSelection analysis program
                                 worklist = flow
                                 contexts = Map.mapWithKey (\l _ -> extremalValueAt analysis l program) (blocks program)
                                 
                                 solve' :: analysis -> Program -> Worklist -> Context property -> Context property  
                                 solve' analysis program [] contexts = contexts
                                 solve' analysis program ((start, end):worklistTail) contexts = 
                                     let --Calculate new values for effect of start and context of end
                                         newEffect       = transfer analysis (fromJust (Map.lookup start (blocks program))) (fromJust (Map.lookup start contexts))
                                         oldContext      = fromJust (Map.lookup end contexts)
                                         newContext      = join analysis oldContext newEffect
                                         
                                         --Updated worklist/contexts
                                         newWorklist     = worklistTail ++ [(l', l'') | (l', l'') <- flow, l' == end] 
                                         newContexts     = Map.insert end newContext contexts
                                     in if (oldContext /= newContext) --Conext value changed?
                                        then solve' analysis program newWorklist newContexts
                                        else solve' analysis program worklistTail contexts
                             in solve' analysis program worklist contexts

--visualize           :: analysis -> Program -> String -> IO ()
--    visualizeSteps      :: analysis -> Program -> String -> IO ()
