{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module EmbellishedMonotoneFramework.EMF where

import qualified Data.Set as Set  
import qualified Data.IntMap as IM

import Data.Maybe
import Program
import Debug.Trace


-- Uitvoer is de LVexit (pagina 52 NNH)
--main::IO()
-- main = print $ solve lifeVariableAnalysis program
--main = print $ backwardsProgramSlicing program 10 $ Set.fromList ['p']


class Analysis analysis property | analysis -> property where
    flowSelection          :: analysis -> Program -> Flow
    extremalLabels         :: analysis -> Program -> [Label]
    extremalValue          :: analysis -> Set.Set property
    join                   :: analysis -> Set.Set property -> Set.Set property -> Set.Set property
    isMoreInformative      :: analysis -> Set.Set property -> Set.Set property -> Bool
    transfer               :: analysis -> Statement -> Set.Set property -> Set.Set property
    transferAll            :: analysis -> Program -> Context property -> Context property
    transferAll       analysis program context = IM.mapWithKey toEffect context
            where
                    toEffect :: Label -> Set.Set property -> Set.Set property
                    toEffect label input = transfer analysis (statementAt program label) input


controlDependentBranchStatements :: Program -> Set.Set Label -> Set.Set Label
controlDependentBranchStatements program relevantStatements = IM.keysSet $ IM.filter inRange (rangeOfInfluence program)
        where
                inRange :: Set.Set Label -> Bool
                inRange range = (not . Set.null) $ Set.intersection range relevantStatements                


backwardsProgramSlicing :: Program -> Label -> Set.Set SymbolType -> Context SymbolType
backwardsProgramSlicing program startLabel startValue = internalBackwardsProgramSlicing program relevantVariables relevantStatements
        where 
                analysis = DirectlyRelevantVariables startLabel startValue
        
                relevantVariables = (transferAll analysis) program (solve analysis program)
                relevantStatements = directlyRelevantStatements program relevantVariables 




internalBackwardsProgramSlicing :: Program -> Context SymbolType -> Set.Set Label -> Context SymbolType
internalBackwardsProgramSlicing program relevantVariables relevantStatements | newRelevantStatements == relevantStatements = relevantVariables 
                                                                             | otherwise                                   = internalBackwardsProgramSlicing program newRelevantVariables newRelevantStatements
        where 
                branchStatements = controlDependentBranchStatements program relevantStatements
        
        
                newRelevantVariables  = foldl union relevantVariables $ Set.toList branchStatements
                        where
                                union :: Context SymbolType -> Label -> Context SymbolType
                                union left right = IM.unionWith Set.union left $ (transferAll analysis) program $ solve analysis program
                                        where
                                                analysis = DirectlyRelevantVariables right $ referenced $ statementAt program right               
                
                newRelevantStatements = Set.union branchStatements $ directlyRelevantStatements program newRelevantVariables          




type Worklist = [(Label, Label)]
type Context property = IM.IntMap (Set.Set property)



-- Step 1
solve :: (Analysis analysis property) => analysis -> Program -> Context property
solve analysis program = internalSolve analysis program worklist context
        where
                worklist = flow
                                        
                context = Map.fromList (map (\label -> if elem label ((extremalLabels analysis) program) then (label, extremalValue analysis) else (label, Set.empty)) (unique flow))
                
                {-
                Om de leesbaarheid te vergroten heb ik getracht om de lambda functie te herschrijven naar de onderstaande code, dit compileerd echter niet :(
                
                context = Map.fromList (map transform (unique flow))
                
                transform :: Label -> (Label, Set.Set property)
                transform label | isExtremalLabel = (label, extremalValue analysis)
                                | otherwise       = (label, Set.empty)
                                where 
                                        isExtremalLabel = elem label ((extremalLabels analysis) program)
                -}
                
                -- Todo replace this with labels function
                unique :: Flow -> [Label]
                unique flow = (Set.toList . Set.fromList) ((fst . unzip) flow ++ (snd . unzip) flow) 
                
                flow     = (flowSelection analysis) program

-- Step 2
internalSolve :: (Analysis analysis property) => analysis -> Program -> Worklist -> Context property -> Context property  
internalSolve analysis program [] context = context
internalSolve analysis program ((start, end):worklistTail) context = internalSolve analysis program newWorklist newContext
        where 
                newWorklist     = if moreInformative then worklistTail else worklistTail ++ [(l', l'') | (l', l'') <- flow, l' == end] 
                newContext      = if moreInformative then context      else Map.insert end ((join analysis) (fromJust (Map.lookup end context))  effect) context  
                
                effect          = (transfer analysis) (fromJust (Map.lookup start (blocks program))) (fromJust (Map.lookup start context))
                moreInformative = isMoreInformative analysis effect (fromJust (Map.lookup end context))
                
                flow            = flowSelection analysis program
