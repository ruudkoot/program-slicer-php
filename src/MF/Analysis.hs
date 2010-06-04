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

type Worklist               = [(Label, Label)]
type CallContext            = [Label]
type LabelValues property   = Map.Map CallContext (Set.Set property)       
type Values property        = IM.IntMap (LabelValues property)

contexts :: Values property -> Set.Set CallContext
contexts = Set.unions . IM.elems . IM.map Map.keysSet

class (Ord property, Show property, Eq property) => Analysis analysis property | analysis -> property where
    flowSelection          :: analysis -> Program -> Flow

    startValues            :: analysis -> Program -> Values property
    startValues analysis program = IM.mapWithKey (\l _ -> Map.singleton [] (extremalValueAt analysis l program)) (blocks program)    
    
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

    transferAll            :: analysis -> Program -> Values property -> Values property
    transferAll       analysis program values = IM.mapWithKey toEffect values
            where
                    --toEffect :: Label -> Set.Set property -> Set.Set property
                    toEffect label = Map.map (transfer analysis (statementAt program label))

    solve :: analysis -> Program -> Values property -> Values property
    solve analysis program values = 
                 let flow     = flowSelection analysis program
                     worklist = flow                     
                     
                     solve' :: analysis -> Program -> Worklist -> Values property -> Values property  
                     solve' analysis program [] values = values
                     solve' analysis program ((start, end):worklistTail) values = 
                         let    --Calculate new values for effect of start 
                                contextStart    = fromJust (IM.lookup start values)
                                statementStart  = statementAt program start
                                newEffectStart  = Map.map (transfer analysis statementStart) contextStart
                                
                                --Calculate new context values for the end block
                                oldContextEnd   = fromJust (IM.lookup end values)        
                                ajoin           = Map.unionWith (join analysis)
                                newContextEnd   = oldContextEnd `ajoin` newEffectStart
                             
                                --Updated worklist/labelProperties
                                newWorklist     = worklistTail ++ [(l', l'') | (l', l'') <- flow, l' == end] 
                                newContexts     = IM.insert end newContextEnd values

                         in if (oldContextEnd /= newContextEnd) --Context value changed?
                            then solve' analysis program newWorklist newContexts
                            else solve' analysis program worklistTail values

                 in solve' analysis program worklist values

fixPoint::(Eq a) => (a -> a) -> a -> a
fixPoint f a | a == na   = a
             | otherwise = fixPoint f na
                where na = f a
        
visualizeWContexts::(Show property) => Values property -> Program -> String -> IO ()
visualizeWContexts values program file = 
    let decorateNode (l, n) = [GV.Label (GV.StrLabel (show l ++ ":" ++ show n++" -- "++(show . fromJust .IM.lookup l $ values)))]
    in visualizeProgramWInfo decorateNode file program
