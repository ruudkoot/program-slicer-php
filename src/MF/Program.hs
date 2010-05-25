module MF.Program where

import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Monoid
import Data.List

import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz as GV

import Data.Maybe

type Label = Int
type Flow = [(Label, Label)]
type SymbolType = String

data Program = Program 
        { blocks        :: IM.IntMap Statement
        , flow          :: Flow
        , startLabel    :: Label
        , finalLabels   :: [Label]
        , rangeOfInfluence :: IM.IntMap (S.Set Label)
        }

data Statement =
      Assign    {var::String, exp::Expression, aop::String}
    | Expr      {exp::Expression}
    
    | If        {exp::Expression}
    | While     {exp::Expression}
    | Break
    | Continue

--Function call
    | FuncCall  {name::String,args::[String]}
    | FuncBack  {name::String,var::String}

--Function definition
    | FuncIn    {name::String, args::[String]}
    | Return    {exp::Expression}
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
freeVar::Value -> S.Set SymbolType
freeVar (Var v) = S.singleton v
freeVar (Const _) = S.empty

freeVariables::Expression -> S.Set SymbolType
freeVariables (BinOp l _ r) = freeVar l `S.union` freeVar r
freeVariables (UnaryOp e _) = freeVar e
freeVariables (Val v)       = freeVar v


defined :: Statement -> S.Set SymbolType
defined (Assign c expr _)= S.singleton c
defined (FuncBack _ v)  = S.singleton v
defined _               = S.empty


referenced :: Statement -> S.Set SymbolType
referenced (Assign c expr _)= freeVariables expr
referenced (Expr expr)      = freeVariables expr 
referenced (While expr)     = freeVariables expr
referenced (If expr)        = freeVariables expr
referenced (FuncCall _ vars)= S.fromList vars
referenced _                = S.empty


-- Geef een lijst terug met alle labels in het programma
labels :: Program -> [Label]
labels = IM.keys . blocks

statementAt :: Program -> Label -> Statement
statementAt program label = fromJust $ IM.lookup label (blocks program) 

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
    show (FuncBack n v) = "back: "++n++":"++v

    show (FuncIn n as)  = "def:"++n++"("++concat (intersperse "," as)++")"
    show (Return e)     = "return "++show e
    
instance Show Expression where
    show (BinOp l op r)     = show l ++op++ show r
    show (UnaryOp exp op)   = op++ show exp
    show (Val v)            = show v

instance Show Value where
    show (Const val)        = val
    show (Var val)          = val

visualize::[(Int,Statement)] -> [(Int,Int)] -> IO ()
visualize n e = let g::G.Gr String ()
                    g = G.mkGraph  (map (\(l,n) -> (l, show l ++ ":" ++ show n)) n) (map (\(i,e) -> (i,e,())) e)
                    dotted = GV.graphToDot True g [] (\(l,n) -> [GV.Label (GV.StrLabel n)]) (const [])
                in do GV.runGraphvizCommand GV.dirCommand dotted GV.Jpeg "test.jpg"
                      return ()
