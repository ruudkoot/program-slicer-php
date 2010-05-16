module PHP.Simple.Statement where

import PHP.Simple.Expression

type PHPProgram = [Statement]

data Statement =                  
      Assign    String Expression
    | Expr      Expression  
    
    | If        Expression [Statement] [Statement]
    | While     Expression [Statement]
    | Break
    | Continue

    | FuncDef   String [String] [Statement]
    | Return    (Maybe Expression)     
    deriving (Eq, Show)

type StatAlg s e =
    (String -> e -> s
    ,e -> s
    ,e -> [s] -> [s] -> s
    ,e -> [s] -> s
    ,s
    ,s
    ,String -> [String] -> [s] -> s
    ,Maybe e -> s)

foldStat::StatAlg s e -> ExpAlg e -> Statement -> s
foldStat (as,ex,i,whil,break,cont,fund,ret) (bin,un,con,var,fun) = foldStat'
  where
    foldExp' (BinOp e1 s e2) = bin (foldExp' e1) s (foldExp' e2)
    foldExp' (UnaryOp s e)   = un s (foldExp' e)
    foldExp' (Const s)       = con s
    foldExp' (Var s)         = var s
    foldExp' (Func s es)     = fun s (map foldExp' es)

    foldStat' (Assign s e)      = as s (foldExp' e)
    foldStat' (Expr e)          = ex (foldExp' e)
    
    foldStat' (If e ss1 ss2)    = i (foldExp' e) (map foldStat' ss1) (map foldStat' ss2)
    foldStat' (While e ss)      = whil (foldExp' e) (map foldStat' ss)
    foldStat' Break             = break
    foldStat' Continue          = cont

    foldStat' (FuncDef s ss sts)= fund s ss (map foldStat' sts)
    foldStat' (Return me)       = ret (fmap foldExp' me)
