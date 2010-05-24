module Statements where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
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
        , rangeOfInfluence :: Map.Map Label (Set.Set Label)
        }
 
-- Geef een lijst terug met alle labels in het programma
labels :: Program -> [Label]
labels program = (Set.toList . Set.fromList) ((fst . unzip . flow) program ++ (snd . unzip . flow) program)

statementAt :: Program -> Label -> Statement
statementAt program label = fromJust $ Map.lookup label (blocks program)
 

reverseFlow :: Flow -> Flow
reverseFlow [] = []
reverseFlow ((start, end):xs) = (end, start):reverseFlow xs


              
 