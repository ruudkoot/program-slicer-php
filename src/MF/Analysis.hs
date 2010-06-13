{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MF.Analysis where

import           Data.Maybe
import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map
import qualified Data.Set    as Set

import MF.Program
import Debug.Trace

import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz        as GV

type Worklist             = [(Label, Label)]
type CallContext          = [Label]
type LabelValues property = Map.Map CallContext (Set.Set property)       
type Values      property = IntMap.IntMap (LabelValues property)


contexts :: Values property -> Set.Set CallContext
contexts = Set.unions . IntMap.elems . IntMap.map Map.keysSet

class (Ord property, Show property) => Analysis analysis property | analysis -> property where
    flowSelection          :: analysis -> Program -> Flow     
    join                   :: analysis -> Set.Set property -> Set.Set property -> Set.Set property
    isMoreInformative      :: analysis -> Set.Set property -> Set.Set property -> Bool
    kill                   :: analysis -> Statement -> Set.Set property -> Set.Set property
    generate               :: analysis -> Statement -> Set.Set property -> Set.Set property

    transfer               :: analysis -> Statement -> Set.Set property -> Set.Set property
    transfer    analysis statement input  =
        (input `Set.difference` (kill analysis statement input))
            `Set.union`
        (generate analysis statement input)

    transferAll            :: analysis -> Program -> Values property -> Values property
    transferAll analysis program   values = IntMap.mapWithKey toEffect values
        where toEffect label =
                Map.map (transfer analysis (statementAt program label))

    mergeValues             :: analysis -> Values property -> Values property -> Values property
    mergeValues analysis = IntMap.unionWith mergeLabelValues
        where mergeLabelValues = Map.unionWith (join analysis)

    solve :: analysis -> Program -> Values property -> Values property
    solve analysis program values = 
                 let worklist = flowSelection analysis program
                     
                     solve' :: analysis -> Program -> Worklist -> Values property -> Values property  
                     solve' analysis program [] values = values
                     solve' analysis program ((start, end):worklistTail) values = 
                         let    --Calculate new values for effect of start 
                                contextStart    = fromMaybe (error "contextStart") (IntMap.lookup start values)
                                statementStart  = statementAt program start
                                newEffectStart  = Map.map (transfer analysis statementStart) contextStart
                                
                                --Calculate new context values for the end block
                                oldContextEnd   = fromMaybe (error $ "contexEnd"++show end++show values) (IntMap.lookup end values)        
                                ajoin           = Map.unionWith (join analysis)
                                newContextEnd   = oldContextEnd `ajoin` newEffectStart
                             
                                --Updated worklist/labelProperties
                                newWorklist     = worklistTail ++ [(l', l'') | (l', l'') <- worklist, l' == end] 
                                newContexts     = IntMap.insert end newContextEnd values

                         in if (oldContextEnd /= newContextEnd) --Context value changed?
                            then solve' analysis program newWorklist newContexts
                            else solve' analysis program worklistTail values

                 in solve' analysis program worklist values

{-
forEachContext :: (Label -> a -> b) -> Values a -> Values b
forEachContext func = IntMap.mapWithKey (\lab context -> Map.map (func lab) context)
-}

fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f a | a == a'   = a
             | otherwise = fixPoint f a'
             where a' = f a
        
visualizeWContexts::(Show property) => Values property -> Program -> String -> IO ()
visualizeWContexts values program file = 
    let decorateNode (l, n) = [GV.Label (GV.StrLabel (show l ++ ":" ++ show n++" -- "++(show . fromJust .IntMap.lookup l $ values)))]
    in visualizeProgramWInfo decorateNode file program
