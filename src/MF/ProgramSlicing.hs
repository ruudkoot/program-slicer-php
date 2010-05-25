module MF.ProgramSlicing where

import qualified Data.Set as Set
import MF.DirectlyRelevantVariables
import MF.Program
import MF.Analysis

import Data.Maybe
import qualified Data.IntMap as Map

directlyRelevantStatements :: Program -> Context SymbolType -> Set.Set Label 
directlyRelevantStatements program relevantVariables = Set.fromList $ map fst $ filter isRelevant (flow program)
        where
                isRelevant :: (Label, Label) -> Bool
                isRelevant (i,j) = (not . Set.null) $ Set.intersection a b
                        where 
                                a :: Set.Set SymbolType 
                                a = fromJust (Map.lookup j relevantVariables)
                                
                                b :: Set.Set SymbolType 
                                b = defined (statementAt program i)
                                
                                
controlDependentBranchStatements :: Program -> Set.Set Label -> Set.Set Label
controlDependentBranchStatements program relevantStatements = Set.fromList $ Map.keys $ Map.filter inRange (rangeOfInfluence program)
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
                                union left right = Map.unionWith Set.union left $ (transferAll analysis) program $ solve analysis program
                                        where
                                                analysis = DirectlyRelevantVariables right $ referenced $ statementAt program right               
                
                newRelevantStatements = Set.union branchStatements $ directlyRelevantStatements program newRelevantVariables          







