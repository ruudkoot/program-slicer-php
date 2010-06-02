module Main where

import System.Environment

import qualified PHP.Parser.Ast as AST
import PHP.Parser.Ast hiding (parse,labels)
import qualified Text.ParserCombinators.Parsec as PS

import PHP.Simple.Ast2Simple
import qualified PHP.Simple.SimpleAst as S

import qualified MF.Program as P
import MF.ProgramSlicing
import MF.Analysis

import qualified Data.IntMap as IM
import qualified Data.Set as Set
import Control.Concurrent
import System.FilePath

doParse :: String -> Ast
doParse input = let parsePHP::Parser Ast
                    parsePHP = AST.parse
                in case (PS.parse parsePHP "" input) of
                     Left err -> error $ "Parse error: " ++ show err
                     Right x -> x

main::IO ()
main = do   (file:_) <- getArgs
            inp <- readFile file
            let tree::S.Program
                tree = toSimple $ doParse inp
                program = S.program tree
                analysis = backwardsProgramSlicing program
            visualizeSlice program (takeBaseName file) 
            print "Visualized"
            print (S.ipf tree)        

printLabels::IM.IntMap P.Statement -> IO ()
printLabels = mapM_ (\(n,s) -> putStrLn (show n ++ ": "++show s)).IM.toList
