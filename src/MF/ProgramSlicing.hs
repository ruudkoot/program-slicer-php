module MF.ProgramSlicing where

import MF.Analysis
import MF.DirectlyRelevantVariables
import MF.Program

import           Data.List
import           Data.Maybe
import qualified Data.Set    as Set
import qualified Data.Map    as Map

import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz        as GV

import Debug.Trace

type Statements     = Map.Map CallContext (Set.Set Label)
type SlicingContext = ( Statements          -- set of relevant statements
                      , Values SymbolType ) -- context with variables

-- | The actual slicing
backwardsProgramSlicing :: Program -> SlicingContext
backwardsProgramSlicing program =
    let relevantVariables  = let startValues = buildSlicingEnvironment program
                              in solve DirectlyRelevantVariables program startValues
        relevantStatements = directlyRelevantStatements program (transferAll DirectlyRelevantVariables program relevantVariables) 
     in fixPoint (internalBackwardsProgramSlicing program) (relevantStatements, relevantVariables)

        where buildSlicingEnvironment :: Program -> Values SymbolType
              buildSlicingEnvironment program = Map.map (Map.singleton []) (traceStatements program)
        
              directlyRelevantStatements :: Program -> Values SymbolType -> Statements
              directlyRelevantStatements program relevantVariables = Map.fromList (map (\context -> (context, forEachContext context)) callContexts)
                      where
                              callContexts = Set.toList $ contexts relevantVariables
                          
                              forEachContext :: CallContext ->  Set.Set Label
                              forEachContext context = Set.fromList $ map fst $ filter isRelevant (flow program)
                                  where
                                      isRelevant (i,j) = (not . Set.null) $ Set.intersection a b
                                          where 
                                              a = contextValues context $ labelValues j relevantVariables
                                              b = modified (statementAt program i)

              internalBackwardsProgramSlicing :: Program -> SlicingContext -> SlicingContext                             
              internalBackwardsProgramSlicing program (relevantStatements, relevantVariables) =
                let --Calculate the new control-dependant statements
                    branchStatements              = controlDependentBranchStatements program relevantStatements          
                    intermediateRelevantVariables = insertControlStatements program branchStatements
                    newRelevantVariables          = solve DirectlyRelevantVariables program $ 
                                                        mergeValues DirectlyRelevantVariables relevantVariables intermediateRelevantVariables
                    newRelevantStatements         = Map.unionWith Set.union branchStatements $ 
                                                        directlyRelevantStatements program (transferAll DirectlyRelevantVariables program  newRelevantVariables)
                 in (newRelevantStatements, newRelevantVariables)

                where -- | Determine which control statements are relevant to the existing relevant statements
                      controlDependentBranchStatements :: Program -> Statements -> Statements
                      controlDependentBranchStatements program = Map.map forEachContext
                              where   forEachContext :: Set.Set Label -> Set.Set Label
                                      forEachContext relevantStatements = Map.keysSet $ Map.filter inRange (rangeOfInfluence program)
                                          where  inRange :: Set.Set Label -> Bool
                                                 inRange range = (not . Set.null) $ Set.intersection range relevantStatements
                                               
                      insertControlStatements :: Program -> Map.Map CallContext (Set.Set Label) -> Values SymbolType
                      insertControlStatements program ctx = Map.foldWithKey traverseContext Map.empty ctx
                          where traverseContext callcontext labels vs = Set.fold traverseEnv vs labels
                                  where traverseEnv label vs = let newValues = Map.singleton callcontext (referenced $ statementAt program label)         
                                                                in Map.insertWith Map.union label newValues vs 


-- |Produces a list of calls to the trace function and the associated variables.
traceStatements :: Program -> Map.Map Label (Set.Set SymbolType)
traceStatements = Map.foldWithKey f (Map.empty) . blocks
    where
        f l (FuncCall "trace" vars) r = Map.insert l (Set.fromList (take (length vars -1) vars)) r
        f _ _                       r = r

--Merge all relevant statements from contexts.
relevantStatements::Program -> SlicingContext -> Set.Set Label
relevantStatements program (statements, contexts) = 
    let baseStatements = Map.fold Set.union Set.empty statements
        statementRelevant k v rest = 
            case statementAt program k of
                (FuncCall _ args)  -> 
                    if inIpf k program
                    then let (call,fin,fret,back) = ipfByCall k program
                             funcInContext = labelValues fin contexts
                             funcInEffect  = changeContextsOut DirectlyRelevantVariables program call funcInContext
                         in if any (\v' -> not (null ((Set.toList v') `intersect` args))) (Map.elems funcInEffect) 
                            then Set.fromList [call,fret,fin,back] `Set.union` rest
                            else rest
                    else rest
                _               -> rest
    in Map.foldWithKey statementRelevant baseStatements contexts

visualizeSlice::Program -> String -> IO ()
visualizeSlice program file = 
    let traces = traceStatements program
        (statements,contexts) = backwardsProgramSlicing program
        usedStatements = relevantStatements program (statements, contexts)        
        traceColor = GV.FillColor (GV.RGB 255 0 0)
        useColor   = GV.FillColor (GV.RGB 0   255 0)
        noneColor  = GV.FillColor (GV.RGB 128 128 128)
        visualizeContext (k,v) = 
            GV.HtmlRow [GV.HtmlLabelCell [] (GV.HtmlText [GV.HtmlStr (show k)])
                       ,GV.HtmlLabelCell [] (GV.HtmlText [GV.HtmlStr (concat (intersperse ", " (map show (Set.toList v))))])]            
        decorateNode (l, n) = 
            [GV.Label (GV.HtmlLabel (GV.HtmlTable (GV.HTable Nothing [] (
                    (GV.HtmlRow [GV.HtmlLabelCell [] (GV.HtmlText [GV.HtmlFont [GV.HtmlPointSize 48.0] [GV.HtmlStr (show l)]])
                                ,GV.HtmlLabelCell [] (GV.HtmlText [GV.HtmlFont [GV.HtmlPointSize 48.0] [GV.HtmlStr (show n)]])]):                   
                    map visualizeContext (Map.toList (labelValues l contexts))))))
                              ,GV.Style [GV.SItem GV.Filled []]
                              ,if Map.member l traces
                               then traceColor
                               else if Set.member l usedStatements
                                    then useColor
                                    else noneColor
                               ]
    in visualizeProgramWInfo decorateNode file program


