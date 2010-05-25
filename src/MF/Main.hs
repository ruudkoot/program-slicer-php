{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Main where

import qualified Data.Set as Set  
import qualified Data.Map as Map

import Data.Maybe
import Expressions
import Statements
import Debug.Trace

-- Define the input, Life variable analysis, zie boek
{-

program :: Program
program = Program blocks flow startLabel finalLabels
        where 
              blocks = Map.fromList 
                        [         (1, AssignmentStatement 'x' (Literal 2))
                                , (2, AssignmentStatement 'y' (Literal 4))
                                , (3, AssignmentStatement 'x' (Literal 1))
                                , (4, ExpressionStatement (GreaterThan (Symbol 'y') (Symbol 'x')))
                                , (5, AssignmentStatement 'z' (Symbol 'y'))
                                , (6, AssignmentStatement 'z' (Multiplication (Symbol 'y') (Symbol 'y')))
                                , (7, AssignmentStatement 'x' (Symbol 'z'))
                        ]
                       
              flow = [ (1, 2)
                     , (2, 3)
                     , (3, 4)
                     , (4, 5)
                     , (4, 6)
                     , (5, 7)
                     , (6, 7)
                     ]
              startLabel = 1
              finalLabels = [7]

-}

-- Sample program for program slicing, zie 'A Survey of Program Slicing Techniques' blz 2.             
program :: Program
program = Program blocks flow startLabel finalLabels rangeOfInfluence
        where 
              blocks = Map.fromList 
                        [         (1, AssignmentStatement 'n' (Literal 2)) -- Zie read als een assignment!
                                , (2, AssignmentStatement 'i' (Literal 1))
                                , (3, AssignmentStatement 's' (Literal 0))
                                , (4, AssignmentStatement 'p' (Literal 1))
                                , (5, ExpressionStatement (GreaterThan (Symbol 'n') (Symbol 'i')))
                                , (6, AssignmentStatement 's' (Addition (Symbol 's') (Symbol 'i')))
                                , (7, AssignmentStatement 'p' (Multiplication (Symbol 'p') (Symbol 'i')))
                                , (8, AssignmentStatement 'i' (Addition (Symbol 'i') (Literal 1)))
                                , (9, ExpressionStatement (Symbol 's'))
                                , (10, ExpressionStatement (Symbol 'p'))
                        ]
                       
              flow = [ (1, 2)
                     , (2, 3)
                     , (3, 4)
                     , (4, 5)
                     , (5, 6)
                     , (6, 7)
                     , (7, 8)
                     , (5, 9)
                     , (9, 10)
                     , (8, 5) -- Loop
                     ]
              startLabel = 1
              finalLabels = [10]
              rangeOfInfluence = Map.fromList 
                        [
                                  (1, Set.empty)
                                , (2, Set.empty)
                                , (3, Set.empty)
                                , (4, Set.empty)
                                , (5, Set.fromList [6,7,8])
                                , (6, Set.empty)
                                , (7, Set.empty)
                                , (8, Set.empty)
                                , (9, Set.empty)
                                , (10, Set.empty)
                        ]



-- Uitvoer is de LVexit (pagina 52 NNH)
main::IO()
-- main = print $ solve lifeVariableAnalysis program
main = print $ backwardsProgramSlicing program 10 $ Set.fromList ['p']

{-main = print $ relevantVariables -- controlDependentBranchStatements program relevantStatements
        where 
                analysis = directlyRelevantVariables 5 $ Set.fromList ['i', 'n']
                
                relevantVariables = (transferAll analysis) program (solve analysis program)
                relevantStatements = directlyRelevantStatements program relevantVariables 
-}

class Analysis analysis property | analysis -> property where
    flowSelection          :: analysis -> Program -> Flow
    extremalLabels         :: analysis -> Program -> [Label]
    extremalValue          :: analysis -> Set.Set property
    join                   :: analysis -> Set.Set property -> Set.Set property -> Set.Set property
    isMoreInformative      :: analysis -> Set.Set property -> Set.Set property -> Bool
    transfer               :: analysis -> Statement -> Set.Set property -> Set.Set property
    transferAll            :: analysis -> Program -> Context property -> Context property
    transferAll       analysis program context = Map.mapWithKey toEffect context
            where
                    toEffect :: Label -> Set.Set property -> Set.Set property
                    toEffect label input = transfer analysis (statementAt program label) input


data LiveVariableAnalysis = LiveVariableAnalysis

instance Analysis LiveVariableAnalysis SymbolType where
    flowSelection     _ program = (reverseFlow . flow) program
    extremalLabels    _ program = finalLabels program
    extremalValue     _         = Set.empty
    join              _         = Set.union
    isMoreInformative _         = Set.isSubsetOf 
    transfer          _ statement input = (input `Set.difference` (kill statement)) `Set.union` (generate statement)
            where 
                    kill :: Statement -> Set.Set SymbolType
                    kill (AssignmentStatement c expr)     = Set.fromList [c]
                    kill (ExpressionStatement expr)       = Set.empty
                    
                    generate :: Statement -> Set.Set SymbolType
                    generate (AssignmentStatement c expr) = freeVariables expr
                    generate (ExpressionStatement expr)   = freeVariables expr  



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




type Worklist = [(Label, Label)]
type Context property = Map.Map Label (Set.Set property)



-- Step 1
solve :: (Analysis analysis property) => analysis -> Program -> Context property
solve analysis program = let flow     = (flowSelection analysis) program
                             worklist = flow
                             context  = Map.fromList (map transform (unique flow))
                          in internalSolve analysis program worklist context
                             where -- Todo replace this with labels function
                                   unique :: Flow -> [Label]
                                   unique flow = (Set.toList . Set.fromList) ((fst . unzip) flow ++ (snd . unzip) flow) 
                                   transform   = (\label -> if elem label ((extremalLabels analysis) program) then (label, extremalValue analysis) else (label, Set.empty))
               
                {-
                Om de leesbaarheid te vergroten heb ik getracht om de lambda functie te herschrijven naar de onderstaande code, dit compileerd echter niet :(
                
                context = Map.fromList (map transform (unique flow))
                
                transform :: Label -> (Label, Set.Set property)
                transform label | isExtremalLabel = (label, extremalValue analysis)
                                | otherwise       = (label, Set.empty)
                                where 
                                        isExtremalLabel = elem label ((extremalLabels analysis) program)
                -}
                
 
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
