module EmbellishedMonotoneFramework.Program where

import qualified Data.Set as S
import Data.Monoid
import Data.List

import Data.Graph.Inductive
import Data.GraphViz

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
    
data Expression =
      BinOp     {left::Value, bop::String, right::Value}
    | Val       {evalue::Value}
    deriving (Eq,Ord)

data Value =
      Const     {value::String}
    | Var       {value::String} 
    deriving (Eq,Ord)

instance Show Expression where
    show (BinOp l op r)     = show l ++op++ show r
    show (Val v)            = show v
instance Show Value where
    show (Const val)        = val
    show (Var val)          = val

visualize::[(Int,Statement)] -> [(Int,Int)] -> IO ()
visualize n e = let g::Gr String ()
                    g = mkGraph  (map (\(l,n) -> (l, show n)) n) (map (\(i,e) -> (i,e,())) e)
                    dotted = graphToDot True g [] (\(l,n) -> [Label (StrLabel n)]) (const [])
                in do runGraphvizCommand dirCommand dotted Jpeg "test.jpg"
                      return ()
