module PHP.Simple.Expression where

data Expression =
      BinOp     Expression String Expression
    | UnaryOp   String Expression
    | Const     String
    | Var       String
    | Func      String [Expression]
    deriving (Eq, Show)

