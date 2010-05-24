module Expressions where

import qualified Data.Set as Set

type LiteralType = Int

type SymbolType = Char

data Expression = Multiplication Expression Expression
                | Substraction Expression Expression
                | Addition Expression Expression
                | GreaterThan Expression Expression
                | Literal LiteralType
                | Symbol SymbolType

{-

data Block = [Statement]
-}

freeVariables :: Expression -> Set.Set SymbolType
freeVariables (Literal _)                 = Set.empty
freeVariables (Symbol char)               = Set.fromList [char]
freeVariables (Multiplication left right) = freeVariables left `Set.union` freeVariables right
freeVariables (Substraction left right)   = freeVariables left `Set.union` freeVariables right
freeVariables (GreaterThan left right)    = freeVariables left `Set.union` freeVariables right
freeVariables (Addition left right)       = freeVariables left `Set.union` freeVariables right
 