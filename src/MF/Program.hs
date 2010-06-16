module MF.Program where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Monoid
import Data.List

import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz as GV

import Data.Maybe
import Debug.Trace

type Label = Int
type Flow = [(Label, Label)]
type Ipf  = (Label,Label,Label,Label) --call,in,return,back
type IpfParameter = (String,String,Bool) --Actual parameter, formalparameter, reference
type SymbolType = String

data Program = Program 
        { blocks        :: Map.Map Label Statement
        , flow          :: Flow
        , startLabel    :: Label
        , finalLabels   :: [Label]
        , rangeOfInfluence :: Map.Map Label (Set.Set Label)
        , interProceduralFlow :: [(Label,Label,Label,Label)]
        }

type FuncArg = (String, --Variable name
                Bool)   --Reference?
data Statement =
      Assign    {var::String, exp::Expression, aop::String}
    | Expr      {exp::Expression}
    
    | If        {exp::Expression}
    | While     {exp::Expression}
    | Break
    | Continue

--Function call
    | FuncCall  {name::String,callArgs::[String]}
    | FuncBack  {name::String}

--Function definition
    | FuncIn    {name::String,inArgs::[FuncArg]}
    | Return
    deriving (Eq,Ord)

data Expression =
      BinOp     {left::Value, bop::String, right::Value}
    | UnaryOp   {expval::Value, op::String}
    | Val       {evalue::Value}
    deriving (Eq,Ord)


data Value =
      Const     {value::String}
    | Var       {value::SymbolType} 
    deriving (Eq,Ord)

--Functions on statements
freeVar::Value -> Set.Set SymbolType
freeVar (Var v) = Set.singleton v
freeVar (Const _) = Set.empty

freeVariables::Expression -> Set.Set SymbolType
freeVariables (BinOp l _ r) = freeVar l `Set.union` freeVar r
freeVariables (UnaryOp e _) = freeVar e
freeVariables (Val v)       = freeVar v


modified :: Statement -> Set.Set SymbolType
modified (Assign c expr _)= Set.singleton c
modified _                = Set.empty

referenced :: Statement -> Set.Set SymbolType
referenced (Assign c expr "") = freeVariables expr
referenced (Assign c expr _) = Set.insert c (freeVariables expr)
referenced (Expr expr)      = freeVariables expr 
referenced (While expr)     = freeVariables expr
referenced (If expr)        = freeVariables expr
referenced _                = Set.empty

ipfByCall :: Label -> Program -> Ipf
ipfByCall c = maybe (error "ipfByCall") id . find (\(s, _, _, _) -> s == c) . interProceduralFlow

ipfByBack :: Label -> Program -> Ipf
ipfByBack b = maybe (error "ipfByBack") id . find (\(_, _, _, l) -> l == b) . interProceduralFlow

inIpf :: Label -> Program -> Bool
inIpf l = isJust . find (\(a,b,c,d) -> (a == l || b ==  l || c == l || d == l) && b /= -1) . interProceduralFlow

ipfParameters :: Program -> Label -> [IpfParameter]
ipfParameters program call = 
    let (c,i,_,_) = ipfByCall call program
        (FuncCall _ actual) = statementAt program c
        (FuncIn _ formal) = statementAt program i
    in zipWith (\ac (form, ref) -> (ac,form,ref)) actual formal
                                                    
-- Geef een lijst terug met alle labels in het programma
labels :: Program -> [Label]
labels = Map.keys . blocks

statementAt :: Program -> Label -> Statement
statementAt program label = maybe (error "statementAt") id $ Map.lookup label (blocks program) 

reverseFlow :: Flow -> Flow
reverseFlow = map (\(x,y) -> (y,x))

instance Show Statement where
    show (Assign v e o) = v++o++"="++show e
    show (Expr exp)     = show exp

    show (If exp)       = "if("++show exp++")"
    show (While exp)    = "while("++show exp++")"
    show (Break)        = "break"
    show (Continue)     = "continue"

    show (FuncCall n a) = "call:"++n++"("++concat (intersperse "," a)++")"
    show (FuncBack n) = "back: "++n

    show (FuncIn n as)  = "def:"++n++"("++concat (intersperse "," (map fst as))++")"
    show Return         = "return"
    
instance Show Expression where
    show (BinOp l op r)     = show l ++op++ show r
    show (UnaryOp exp op)   = op++ show exp
    show (Val v)            = show v

instance Show Value where
    show (Const val)        = val
    show (Var val)          = val

visualizeProgram::String -> Program -> IO ()
visualizeProgram = visualizeProgramWInfo decorateNode
    where decorateNode (l, n) = [GV.Label (GV.StrLabel (show l ++ ":" ++ show n))]

visualizeProgramWInfo::(G.LNode Statement -> GV.Attributes) -> String -> Program -> IO ()
visualizeProgramWInfo nfunc fil prog = 
    let nodes = map (\(l,n) -> (l, n))$ Map.toList $ blocks prog
        edges = map (\(i,e) -> (i,e,())) $ flow prog
        g::G.Gr Statement ()
        g = G.mkGraph  nodes edges
        
        clusterNode (n,c) = foldr (\(lc,ec) r -> if Set.member n ec then GV.C lc r else r) (GV.N (n,c)) (Map.toList (rangeOfInfluence prog))
        
        graphAtts = [GV.GraphAttrs
                     [GV.Aspect (GV.RatioOnly 1.3)
                     , GV.Concentrate True
                     , GV.Root (GV.NodeName (show (startLabel prog)))]]
        dotted = GV.clusterGraphToDot True g graphAtts clusterNode (Just. GV.Int) (const []) nfunc (const [])
    in do GV.runGraphvizCommand GV.dirCommand dotted GV.Png (fil++".png")
          return ()
