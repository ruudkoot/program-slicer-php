{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MF.DirectlyRelevantVariables where

import           Data.Maybe
import qualified Data.Map   as Map
import qualified Data.Set   as Set

import MF.Expressions
import MF.Statements

import MF.Analysis

data DirectlyRelevantVariables = DirectlyRelevantVariables Label (Set.Set SymbolType)

instance Analysis DirectlyRelevantVariables SymbolType where
    flowSelection     _ program = (reverseFlow . flow) program
    extremalLabels    (DirectlyRelevantVariables startLabel startValue) program  = [startLabel]
    extremalValue     (DirectlyRelevantVariables startLabel startValue)          = startValue
    join              _         = Set.union
    isMoreInformative _         = Set.isSubsetOf
    transfer          _ statement input = (input `Set.difference` (kill statement)) `Set.union` (generate statement input)
            where 
                    kill :: Statement -> Set.Set SymbolType
                    kill = defined
                    
                    generate :: Statement -> Set.Set SymbolType -> Set.Set SymbolType
                    generate (AssignmentStatement c expr) input | Set.member c input = freeVariables expr
                                                                | otherwise          = Set.empty
                    generate (ExpressionStatement expr) input = Set.empty



-- Zie paper frank, blz 5 onderaan
defined :: Statement -> Set.Set SymbolType
defined (AssignmentStatement c expr) = Set.fromList[c]
defined (ExpressionStatement expr)   = Set.empty

-- Zie paper frank, blz 5 onderaan
referenced :: Statement -> Set.Set SymbolType
referenced (AssignmentStatement c expr) = freeVariables expr
referenced (ExpressionStatement expr)   = freeVariables expr 




-- Zie Paper Frank, blz. 8
{-
directlyRelevantVariables :: Label -> Set.Set SymbolType -> Analysis SymbolType 
directlyRelevantVariables startLabel startValue = Analysis flowSelection extremalLabels extremalValue join isMoreInformative transfer transferAll
        where 
-}



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
controlDependentBranchStatements program relevantStatements = Map.keysSet $ Map.filter inRange (rangeOfInfluence program)
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







