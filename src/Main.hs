module Main where

import System.Environment

import qualified PHP.Parser.Ast as AST
import PHP.Parser.Ast hiding (parse)
import qualified Text.ParserCombinators.Parsec as PS

import PHP.Simple.Ast2Simple
import PHP.Simple.Statement

doParse :: String -> Ast
doParse input = let parsePHP::Parser Ast
                    parsePHP = AST.parse
                in case (PS.parse parsePHP "" input) of
                     Left err -> error $ "Parse error: " ++ show err
                     Right x -> x

main::IO ()
main = do (file:_) <- getArgs
          inp <- readFile file           
          print $ usedVars.toSimple.doParse $ inp
