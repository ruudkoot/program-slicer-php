module EMF.Program where

import qualified Data.Set as S
import Data.Monoid
import Data.List

data Statement =
      Assign    {var::String, exp::Expression}
    | Expr      {exp::Expression}
    
    | If        {exp::Expression}
    | While     {exp::Expression}
    | Break
    | Continue

--Function call
    | FuncCall  {name::String,args::[String]}
    | FuncBack  {var::String}

--Function definition
    | FuncIn    {name::String, args::[String]}
    | Return    {exp::Expression}
    deriving (Eq,Ord)

instance Show Statement where
    show (Assign v exp) = v++"="++show exp
    show (Expr exp)     = show exp

    show (If exp)       = "if("++show exp++")"
    show (While exp)    = "while("++show exp++")"
    show (Break)        = "break"
    show (Continue)     = "continue"

    show (FuncCall n a) = "call:"++n++"("++concat (intersperse "," a)++")"
    show (FuncBack var) = "back: "++var

    show (FuncIn n as)  = "def:"++n++"("++concat (intersperse "," as)++")"
    show (Return e)     = "return "++show e
    
data Expression =
      BinOp     {left::Expression, op::String, right::Expression}
    | UnaryOp   {op::String, ex::Expression}
    | Const     {value::String}
    | Var       {value::String} 
    deriving (Eq,Ord)

instance Show Expression where
    show (BinOp l op r)     = show l ++op++ show r
    show (UnaryOp op exp)   = op++show exp
    show (Const val)        = val
    show (Var val)          = val
