module Main where

import System.Environment

import qualified PHP.Parser.Ast as AST
import PHP.Parser.Ast hiding (parse,labels)
import qualified Text.ParserCombinators.Parsec as PS

import PHP.Simple.Ast2Simple
import qualified PHP.Simple.SimpleAst as S

import qualified EmbellishedMonotoneFramework.Program as P

import qualified Data.IntMap as IM

import Control.Concurrent

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
                flow = S.flow tree
                labels = S.labels tree           
            print "Used variables:"
            print $ S.usedVars tree
            print "Labels:"
            printLabels $ S.labels tree
            print "Flow:"
            print $ S.flow tree
            print "Entry label:"
            print $ S.entry tree
            P.visualize (IM.toList labels) flow
            print "Visualized"

printLabels::IM.IntMap P.Statement -> IO ()
printLabels = mapM_ (\(n,s) -> putStrLn (show n ++ ": "++show s)).IM.toList
