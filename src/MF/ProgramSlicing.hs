module MF.ProgramSlicing where

import MF.DirectlyRelevantVariables
import MF.Program
import MF.Analysis

import Data.Maybe

import qualified Data.Set as Set
import qualified Data.Map as    Map
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IM


import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz as GV


type SlicingContext = (Map.Map CallContext (Set.Set Label) --Set of relevant statements
                      ,Values SymbolType)   --Context with variables

-- type CallContext            = [Label]
-- type LabelValues property   = Map.Map CallContext (Set.Set property)       
-- type Values property        = IM.IntMap (LabelValues property)


-- | Calculate control statements that are directly relevant to their respective variables
{-
directlyRelevantStatements :: Program -> Values SymbolType -> Set.Set Label
directlyRelevantStatements program relevantVariables = Set.fromList $ map fst $ filter isRelevant (flow program)
        where
                isRelevant :: (Label, Label) -> Bool
                isRelevant (i,j) = (not . Set.null) $ Set.intersection a b
                        where 
                                a :: Set.Set SymbolType 
                                a = Set.unions $ Map.elems $ fromJust (IM.lookup j relevantVariables)
                                
                                b :: Set.Set SymbolType 
                                b = modified (statementAt program i)
-}


directlyRelevantStatements :: Program -> Values SymbolType -> Map.Map CallContext (Set.Set Label)
directlyRelevantStatements program relevantVariables = Map.fromList (map (\context -> (context, forEachContext context)) callContexts)
        where
                callContexts = Set.toList $ contexts relevantVariables
                
                forEachContext :: CallContext ->  Set.Set Label
                forEachContext context = Set.fromList $ map fst $ filter isRelevant (flow program)
                    where
                        isRelevant :: (Label, Label) -> Bool
                        isRelevant (i,j) = (not . Set.null) $ Set.intersection a b
                            where 
                                a :: Set.Set SymbolType 
                                a = maybe (error "firstRelevant") id (Map.lookup context (maybe (error "secondRelevant") id (IM.lookup j relevantVariables)))
                                
                                b :: Set.Set SymbolType 
                                b = modified (statementAt program i)
                                

-- | Determine which control statements are relevant to the existing relevant statements
controlDependentBranchStatements :: Program -> Map.Map CallContext (Set.Set Label) -> Map.Map CallContext (Set.Set Label)
controlDependentBranchStatements program = Map.map forEachContext
        where   forEachContext :: Set.Set Label -> Set.Set Label
                forEachContext relevantStatements = Set.fromList $ IntSet.toList $ IM.keysSet $ IM.filter inRange (rangeOfInfluence program)
                    where  inRange :: Set.Set Label -> Bool
                           inRange range = (not . Set.null) $ Set.intersection range relevantStatements

--The actual slicing
backwardsProgramSlicing :: Program -> SlicingContext
backwardsProgramSlicing program = fixPoint (internalBackwardsProgramSlicing program) (relevantStatements,relevantVariables)
        where   --TODO: Extend to multiple trace statements
                startValues = buildSlicingEnvironment program

                relevantVariables = solve DirectlyRelevantVariables program startValues
                relevantStatements = directlyRelevantStatements program (transferAll DirectlyRelevantVariables program relevantVariables) 

-- Fixpoint function to progress looking for a fixpoint which contians all the control statements.


{-
internalBackwardsProgramSlicing program (relevantStatements, relevantVariables) = (newRelevantStatements, newRelevantVariables)
         where  --Calculate the new control-dependant statements
                branchStatements = controlDependentBranchStatements program relevantStatements        
        
                newRelevantVariables  = foldl union relevantVariables $ Set.toList branchStatements
                        where
                                union :: Values SymbolType -> Label -> Values    SymbolType
                                union left right = Map.unionWith Set.union left $ (transferAll analysis) program $ solve analysis program
                                        where
                                                analysis = DirectlyRelevantVariables right $ referenced $ statementAt program right               
                
                newRelevantStatements = Set.union branchStatements $ directlyRelevantStatements program newRelevantVariables          
-}

insertControlStatements :: Program -> Map.Map CallContext (Set.Set Label) -> Values SymbolType
insertControlStatements program ctx = Map.foldWithKey traverseContext IM.empty ctx
    where traverseContext callcontext labels vs = Set.fold traverseEnv vs labels
            where traverseEnv label vs = let newValues = Map.singleton callcontext (referenced $ statementAt program label)         
                                         in IM.insertWith Map.union label newValues vs 
                                         
internalBackwardsProgramSlicing :: Program -> SlicingContext -> SlicingContext                             
internalBackwardsProgramSlicing program (relevantStatements, relevantVariables) = (newRelevantStatements, newRelevantVariables)
         where  --Calculate the new control-dependant statements
                branchStatements = controlDependentBranchStatements program relevantStatements
               
               
                intermediateRelevantVariables = insertControlStatements program branchStatements
                newRelevantVariables = solve DirectlyRelevantVariables program $ mergeValues DirectlyRelevantVariables relevantVariables intermediateRelevantVariables
                
                newRelevantStatements = Map.unionWith Set.union branchStatements $ directlyRelevantStatements program (transferAll DirectlyRelevantVariables program  newRelevantVariables)
                
               

buildSlicingEnvironment :: Program -> Values SymbolType
buildSlicingEnvironment program = foldr ins IM.empty (labels program)
    where statements = traceStatements program
          ins l v = IM.insert l (Map.singleton [] (IM.findWithDefault Set.empty l statements)) v
                    
-- |Produces a list of calls to the trace function and the associated variables.
traceStatements :: Program -> IM.IntMap (Set.Set SymbolType)
traceStatements = IM.foldWithKey f (IM.empty) . blocks
    where
        f l (FuncCall "trace" vars) r = IM.insert l (Set.fromList vars) r
        f _ _                       r = r

                                
visualizeSlice::Program -> String -> IO ()
visualizeSlice program file = 
    let traces = traceStatements program
        (statements,contexts) = backwardsProgramSlicing program
        traceColor = GV.FillColor (GV.RGB 255 0 0)
        useColor   = GV.FillColor (GV.RGB 0   255 0)
        noneColor  = GV.FillColor (GV.RGB 128 128 128)
        decorateNode (l, n) = [GV.Label (GV.StrLabel (show l ++ ":" ++ show n++" -- "++(show . IM.lookup l $ contexts)))
                              ,GV.Style [GV.SItem GV.Filled []]
                              ,if IM.member l traces
                               then traceColor
                               else useColor
                               ]
    in visualizeProgramWInfo decorateNode file program


