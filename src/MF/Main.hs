
module Main where

import qualified Data.Set as Set  
import qualified Data.Map as Map

import Data.Maybe
import Expressions
import Statements

-- Define the input
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



-- Uitvoer is de LVexit (pagina 52 NNH)
main::IO()
main = print $ solve lifeVariableAnalysis program


data Analysis property = Analysis
        { flowSelection          :: Program -> Flow
        , extremalLabels         :: Program -> [Label]
        , extremalValue          :: Set.Set property
        , join                   :: Set.Set property -> Set.Set property -> Set.Set property
        , isMoreInformative      :: Set.Set property -> Set.Set property -> Bool
        , transfer               :: Statement -> Set.Set property -> Set.Set property
        }
        
lifeVariableAnalysis :: Analysis SymbolType
lifeVariableAnalysis = Analysis flowSelection extremalLabels extremalValue join isMoreInformative transfer
        where
                flowSelection program    = (reverseFlow . flow) program
                extremalLabels program   = finalLabels program
                extremalValue            = Set.empty
                join                     = Set.union
                isMoreInformative        = Set.isSubsetOf 
                transfer statement input = (input `Set.difference` (kill statement)) `Set.union` (generate statement)
                        where 
                                kill :: Statement -> Set.Set SymbolType
                                kill (AssignmentStatement c expr)     = Set.fromList [c]
                                kill (ExpressionStatement expr)       = Set.empty
                                
                                generate :: Statement -> Set.Set SymbolType
                                generate (AssignmentStatement c expr) = freeVariables expr
                                generate (ExpressionStatement expr)   = freeVariables expr   


type Worklist = [(Label, Label)]
type Context property = Map.Map Label (Set.Set property)

-- Step 1
solve :: Analysis property -> Program -> Context property
solve analysis program = internalSolve analysis program worklist context
        where
                worklist = flow
                context  = Map.fromList ([(start, Set.empty) | (start, end) <- flow] ++ [(end, Set.empty) | (start, end) <- flow]) -- Nog niet generiek, alleen geschikt voor life variable analysis
                flow     = (flowSelection analysis) program

-- Step 2
internalSolve :: Analysis property -> Program -> Worklist -> Context property -> Context property  
internalSolve analysis program [] context = context
internalSolve analysis program ((start, end):worklistTail) context = internalSolve analysis program newWorklist newContext
        where 
                newWorklist     = if moreInformative then worklistTail else worklistTail ++ [(l', l'') | (l', l'') <- flow, l' == end] 
                newContext      = if moreInformative then context      else Map.insert end ((join analysis) (fromJust (Map.lookup end context))  effect) context  
                
                effect          = (transfer analysis) (fromJust (Map.lookup start (blocks program))) (fromJust (Map.lookup start context))
                moreInformative = isMoreInformative analysis effect (fromJust (Map.lookup end context))
                
                flow            = flowSelection analysis program