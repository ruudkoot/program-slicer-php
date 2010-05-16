module PHP.Simple.Expression where

import Data.List

data Expression =
      BinOp     Expression String Expression
    | UnaryOp   String Expression
    | Const     String
    | Var       String
    | Func      String [Expression]
    deriving (Eq, Show)

type ExpAlg e = 
    (e -> String -> e -> e
    ,String -> e -> e
    ,String -> e            
    ,String -> e
    ,String -> [e] -> e)

foldExp::ExpAlg a -> Expression -> a
foldExp (bin,un,con,var,fun) = foldExp'
  where
    foldExp' (BinOp e1 s e2) = bin (foldExp' e1) s (foldExp' e2)
    foldExp' (UnaryOp s e)   = un s (foldExp' e)
    foldExp' (Const s)       = con s
    foldExp' (Var s)         = var s
    foldExp' (Func s es)     = fun s (map foldExp' es)


varsAlg::ExpAlg [String]
varsAlg = (bin,un,con,var,fun)
  where
    bin e1 s e2 = e1 `union` e2 
    un s e      = e
    con s       = []
    var s       = [s]
    fun s es    = foldr union [] es

vars::Expression -> [String]
vars = foldExp varsAlg  


funcsAlg::ExpAlg [String]
funcsAlg = (bin,un,con,var,fun)
  where
    bin e1 s e2 = e1 `union` e2
    un s e      = e
    con s       = []
    var s       = []
    fun s es    = foldr union [s] es

funcs::Expression -> [String]
funcs = foldExp funcsAlg
