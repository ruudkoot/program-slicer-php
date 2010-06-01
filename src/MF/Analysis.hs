{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MF.Analysis where

import           Data.Maybe
import qualified Data.IntMap   as IM
import qualified Data.Map      as Map
import qualified Data.Set   as Set

import MF.Program
import Debug.Trace

import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz as GV

type Worklist                   = [(Label, Label)]
--type CallContext                = [Label]
--type CallContexts property      = Map.Map CallContext property       
type LabelProperties property   = IM.IntMap  (Set.Set property)

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

    transferAll            :: analysis -> Program -> LabelProperties property -> LabelProperties property
    transferAll       analysis program labelProperty = IM.mapWithKey toEffect labelProperty
            where
                    toEffect :: Label -> Set.Set property -> Set.Set property
                    toEffect label input = transfer analysis (statementAt program label) input

    solve :: analysis -> Program -> LabelProperties property
    solve analysis program = let flow     = flowSelection analysis program
                                 worklist = flow
                                 labelProperties = IM.mapWithKey (\l _ -> extremalValueAt analysis l program) (blocks program)
                                 
                                 solve' :: analysis -> Program -> Worklist -> LabelProperties property -> LabelProperties property  
                                 solve' analysis program [] labelProperties = labelProperties
                                 solve' analysis program ((start, end):worklistTail) labelProperties = 
                                     let --Calculate new values for effect of start and labelProperty of end
                                         newEffect       = transfer analysis (fromJust (IM.lookup start (blocks program))) (fromJust (IM.lookup start labelProperties))
                                         oldContext      = fromJust (IM.lookup end labelProperties)
                                         ajoin           = join analysis
                                         newContext      = oldContext `ajoin` newEffect `ajoin` extremalValueAt analysis end program
                                         
                                         --Updated worklist/labelProperties
                                         newWorklist     = worklistTail ++ [(l', l'') | (l', l'') <- flow, l' == end] 
                                         newContexts     = IM.insert end newContext labelProperties
                                     in if (oldContext /= newContext) --Conext value changed?
                                        then solve' analysis program newWorklist newContexts
                                        else solve' analysis program worklistTail labelProperties
                             in solve' analysis program worklist labelProperties

fixPoint::(Eq a) => (a -> a) -> a -> a
fixPoint f a | a == na   = a
             | otherwise = fixPoint f na
                where na = f a
        
visualizeWContexts::(Show property) => LabelProperties property -> Program -> String -> IO ()
visualizeWContexts labelProperties program file = 
    let decorateNode (l, n) = [GV.Label (GV.StrLabel (show l ++ ":" ++ show n++" -- "++(show . Set.toList . fromJust .IM.lookup l $ labelProperties)))]
    in visualizeProgramWInfo decorateNode file program
