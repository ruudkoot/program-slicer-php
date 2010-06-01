module MF.ProgramSlicing where

import qualified Data.Set as Set
import MF.DirectlyRelevantVariables
import MF.Program
import MF.Analysis

import Data.Maybe
import qualified Data.IntMap as Map

import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz as GV


type SlicingContext = (Set.Set Label --Set of relevant statements
                      ,LabelProperties SymbolType)   --Context with variables

-- | Calculate control statements that are directly relevant to their respective variables
directlyRelevantStatements :: Program -> LabelProperties SymbolType -> Set.Set Label 
directlyRelevantStatements program relevantVariables = Set.fromList $ map fst $ filter isRelevant (flow program)
        where
                isRelevant :: (Label, Label) -> Bool
                isRelevant (i,j) = (not . Set.null) $ Set.intersection a b
                        where 
                                a :: Set.Set SymbolType 
                                a = fromJust (Map.lookup j relevantVariables)
                                
                                b :: Set.Set SymbolType 
                                b = modified (statementAt program i)
                                
-- | Determine which control statements are relevant to the existing relevant statements
controlDependentBranchStatements :: Program -> Set.Set Label -> Set.Set Label
controlDependentBranchStatements program relevantStatements = Set.fromList $ Map.keys $ Map.filter inRange (rangeOfInfluence program)
        where
                inRange :: Set.Set Label -> Bool
                inRange range = (not . Set.null) $ Set.intersection range relevantStatements                

--The actual slicing
backwardsProgramSlicing :: Program -> SlicingContext
backwardsProgramSlicing program = fixPoint (internalBackwardsProgramSlicing program) (relevantStatements,relevantVariables)
        where   --TODO: Extend to multiple trace statements
                (startLabel, startValue) = head.Map.toList $ traceStatements program

                analysis = DirectlyRelevantVariables startLabel startValue
                
                --Start with the directly relevant variables and their statements
                relevantVariables = (transferAll analysis) program (solve analysis program)
                relevantStatements = directlyRelevantStatements program relevantVariables 

-- Fixpoint function to progress looking for a fixpoint which contians all the control statements.
internalBackwardsProgramSlicing :: Program -> SlicingContext -> SlicingContext
internalBackwardsProgramSlicing program (relevantStatements, relevantVariables) = (newRelevantStatements, newRelevantVariables)
         where  --Calculate the new control-dependant statements
                branchStatements = controlDependentBranchStatements program relevantStatements        
        
                newRelevantVariables  = foldl union relevantVariables $ Set.toList branchStatements
                        where
                                union :: LabelProperties SymbolType -> Label -> LabelProperties SymbolType
                                union left right = Map.unionWith Set.union left $ (transferAll analysis) program $ solve analysis program
                                        where
                                                analysis = DirectlyRelevantVariables right $ referenced $ statementAt program right               
                
                newRelevantStatements = Set.union branchStatements $ directlyRelevantStatements program newRelevantVariables          

-- |Produces a list of calls to the trace function and the associated variables.
traceStatements :: Program -> Map.IntMap (Set.Set SymbolType)
traceStatements = Map.foldWithKey f (Map.empty) . blocks
    where
        f l (FuncCall "trace" vars) r = Map.insert l (Set.fromList vars) r
        f _ _                       r = r


visualizeSlice::Program -> String -> IO ()
visualizeSlice program file = 
    let traces = traceStatements program
        (statements,contexts) = backwardsProgramSlicing program
        traceColor = GV.FillColor (GV.RGB 255 0 0)
        useColor   = GV.FillColor (GV.RGB 0   255 0)
        noneColor  = GV.FillColor (GV.RGB 128 128 128)
        decorateNode (l, n) = [GV.Label (GV.StrLabel (show l ++ ":" ++ show n++" -- "++(show . Set.toList . fromJust .Map.lookup l $ contexts)))
                              ,GV.Style [GV.SItem GV.Filled []]
                              ,if Map.member l traces
                               then traceColor
                               else if Set.member l statements
                                    then useColor
                                    else noneColor]
    in visualizeProgramWInfo decorateNode file program


