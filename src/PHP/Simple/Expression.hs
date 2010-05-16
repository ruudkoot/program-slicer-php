module PHP.Simple.Expression where

import qualified Data.Set as Set

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


expIdentityAlg::ExpAlg Expression
expIdentityAlg = (BinOp, UnaryOp, Const, Var, Func)

expVarsAlg::ExpAlg (Set.Set String)
expVarsAlg = (bin,un,con,var,fun)
  where
    bin e1 s e2 = e1 `Set.union` e2 
    un s e      = e
    con s       = Set.empty
    var s       = Set.singleton s
    fun s es    = Set.unions es

expVars::Expression -> (Set.Set String)
expVars = foldExp expVarsAlg  


expFuncsAlg::ExpAlg (Set.Set String)
expFuncsAlg = (bin,un,con,var,fun)
  where
    bin e1 s e2 = e1 `Set.union` e2
    un s e      = e
    con s       = Set.empty
    var s       = Set.empty
    fun s es    = Set.insert s (Set.unions es)

expFuncs::Expression -> (Set.Set String)
expFuncs = foldExp expFuncsAlg
