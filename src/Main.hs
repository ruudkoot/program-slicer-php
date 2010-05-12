module Main where

import System.Environment

import qualified Parser.Ast as AST
import Parser.Ast hiding (parse)
import qualified Text.ParserCombinators.Parsec as PS

doParse :: String -> Ast
doParse input = let parsePHP::Parser Ast
                    parsePHP = AST.parse
                in case (PS.parse parsePHP "" input) of
                     Left err -> error $ "Parse error: " ++ show err
                     Right x -> x

main::IO ()
main = do (file:_) <- getArgs
          inp <- readFile file           
          print (unparse (doParse inp))
