module Main where

import System.Environment

import qualified PHP.Parser.Ast as AST
import PHP.Parser.Ast hiding (parse,labels)
import qualified Text.ParserCombinators.Parsec as PS

import PHP.Simple.Ast2Simple
import PHP.Simple.Statement

import qualified Data.IntMap as IM

doParse :: String -> Ast
doParse input = let parsePHP::Parser Ast
                    parsePHP = AST.parse
                in case (PS.parse parsePHP "" input) of
                     Left err -> error $ "Parse error: " ++ show err
                     Right x -> x

main::IO ()
main = do   (file:_) <- getArgs
            inp <- readFile file
            let tree = toSimple $ doParse inp           
            print "Used variables:"
            print $ usedVars tree
            print "Labels:"
            printLabels $ labels tree
            print "Flow:"
            print $ flow tree

printLabels::IM.IntMap Statement -> IO ()
printLabels = mapM_ (\(n,s) -> putStrLn (show n ++ ": "++show s)).IM.toList
