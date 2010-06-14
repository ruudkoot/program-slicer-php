{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MF.Analysis where

import           Data.Maybe
import qualified Data.Map      as Map
import qualified Data.Set   as Set

import MF.Program
import Debug.Trace

import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz as GV

type Worklist               = [(Label, Label)]
type CallContext            = [Label]
type LabelValues property   = Map.Map CallContext (Set.Set property)       
type Values property        = Map.Map Label (LabelValues property)


contexts :: Values property -> Set.Set CallContext
contexts = Set.unions . Map.elems . Map.map Map.keysSet

class (Ord property, Show property, Eq property) => Analysis analysis property | analysis -> property where
    flowSelection          :: analysis -> Program -> Flow     

    join                   :: analysis -> Set.Set property -> Set.Set property -> Set.Set property

    kill                   :: analysis -> Statement -> Set.Set property -> Set.Set property
    generate               :: analysis -> Statement -> Set.Set property -> Set.Set property
    


--Transfers intra-procedural flow statements
    transfer               :: analysis -> Statement -> Set.Set property -> Set.Set property
    transfer          analysis statement input = (input `Set.difference` (kill analysis statement input)) `Set.union` (generate analysis statement input)

--Moves the complete value-set to effect values
    transferAll            :: analysis -> Program -> Values property -> Values property
    transferAll       analysis program values = Map.mapWithKey toEffect values
            where
                    --toEffect :: Label -> Set.Set property -> Set.Set property
                    toEffect label = Map.map (transfer analysis (statementAt program label))

--Transfer function between FuncBack and Return
    transferParametersOut        :: analysis -> [IpfParameter] -> Set.Set property -> Set.Set property

--Transfer function between FuncIn en Call
    transferParametersIn         :: analysis -> [IpfParameter] -> Set.Set property -> Set.Set property

--Merges result form function with local result
    transferFuncMerge      :: analysis -> Set.Set property -> Set.Set property -> Set.Set property

--Merges 2 value-sets
    mergeValues             :: analysis -> Values property -> Values property -> Values property
    mergeValues analysis = Map.unionWith mergeLabelValues
        where 
                mergeLabelValues = Map.unionWith (join analysis)

--Merge calling contexts
    mergeCallContexts :: analysis -> LabelValues property -> LabelValues property -> LabelValues property
    mergeCallContexts analysis = Map.unionWith (transferFuncMerge analysis)

    changeContextsIn :: analysis -> Program -> Label -> LabelValues property -> LabelValues property
    changeContextsIn analysis program l = Map.mapKeys (l:) . Map.map (transferParametersIn analysis  (ipfParameters program l))

    changeContextsOut :: analysis -> Program -> Label -> LabelValues property -> LabelValues property
    changeContextsOut analysis program l  = Map.foldWithKey f Map.empty . Map.map (transferParametersOut analysis  (ipfParameters program l))
                            where f (c:cs) vals new | c == l     = Map.insert cs vals new
                                                    | otherwise  = new
                                  f []     vals new              = new       

--FCall gets on call stack!
    solve :: analysis -> Program -> Values property -> Values property
    solve analysis program values = 
         let flow     = flowSelection analysis program
             worklist = flow                     
             
             solve' :: Worklist -> Values property -> Values property  
             solve' [] values = values
             solve' ((start, end):worklistTail) values = 
                 let statementStart  = statementAt program start
                     contextStart    = maybe (error "contextStart") id $ Map.lookup start values                     
                     effectStart  = Map.map (transfer analysis statementStart) contextStart
                     
                     statementEnd    = statementAt program end                     
                     oldContextEnd   = fromJust $ Map.lookup end values   
                     --Calculate new context values for the end block
                     
                     ajoin           = Map.unionWith (join analysis)

--Backwards slicing  
                     ipfContext::Statement -> Statement -> LabelValues property   
                     ipfContext (FuncIn _ _) (FuncCall _ _) =
                        let (call,_,_,back) = ipfByCall end program
                            funcBackContext = maybe (error "funcBackcontext") id $ Map.lookup back values
                            funcInEffect = changeContextsOut analysis program call contextStart 
                        in mergeCallContexts analysis funcBackContext funcInEffect   

                     ipfContext (FuncBack _ _) (FuncCall _ _) = 
                        let (call,_,_,back) = ipfByCall end program
                            funcInContext = maybe (error "funcIn") id $ Map.lookup back values
                            funcInEffect = changeContextsOut analysis program call funcInContext 
                        in mergeCallContexts analysis contextStart funcInEffect   
                     
                     ipfContext (FuncBack _ _) Return  =
                        let (call,_,_,back) = ipfByBack start program
                            funcBackEffect = changeContextsIn analysis program call contextStart
                        in oldContextEnd `ajoin` funcBackEffect

                     ipfContext _              _           = oldContextEnd `ajoin` effectStart
                     
                     --Updated worklist/labelProperties
                     newContextEnd   = if inIpf start program
                                       then ipfContext statementStart statementEnd
                                       else oldContextEnd `ajoin` effectStart

                     newWorklist     = worklistTail ++ [(l', l'') | (l', l'') <- flow, l' == end] 
                     newContexts     = Map.insert end newContextEnd values

                 in if (oldContextEnd /= newContextEnd) --Context value changed?
                    then solve' newWorklist newContexts
                    else solve' worklistTail values
         in solve' worklist values

{-
forEachContext :: (Label -> a -> b) -> Values a -> Values b
forEachContext func = Map.mapWithKey (\lab context -> Map.map (func lab) context)
-}

fixPoint::(Eq a) => (a -> a) -> a -> a
fixPoint f a | a == na   = a
             | otherwise = fixPoint f na
                where na = f a
        
visualizeWContexts::(Show property) => Values property -> Program -> String -> IO ()
visualizeWContexts values program file = 
    let decorateNode (l, n) = [GV.Label (GV.StrLabel (show l ++ ":" ++ show n++" -- "++(show . fromJust .Map.lookup l $ values)))]
    in visualizeProgramWInfo decorateNode file program
