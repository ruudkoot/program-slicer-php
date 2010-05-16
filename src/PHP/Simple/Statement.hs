module PHP.Simple.Statement where

import PHP.Simple.Expression

data Statement =
      Block     [Statement]                  
    | Break
    | Continue 
    | Assign    String Expression
    | Expr      Expression  
    | FuncDef   String [String] Statement                          
    | If        Expression Statement Statement
    | Return    
    | While     Expression Statement
    | Nothing
    deriving (Eq, Show)

