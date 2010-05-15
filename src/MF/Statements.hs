module Statements where

import qualified Data.Map as Map
import Expressions

data Statement = AssignmentStatement SymbolType Expression
               | ExpressionStatement Expression
               
type Label = Int
type Flow = [(Label, Label)]

data Program = Program 
        { blocks        :: Map.Map Label Statement
        , flow          :: Flow
        , startLabel    :: Label
        , finalLabels   :: [Label]
        }
 
reverseFlow :: Flow -> Flow
reverseFlow [] = []
reverseFlow ((start, end):xs) = (end, start):reverseFlow xs


              
 