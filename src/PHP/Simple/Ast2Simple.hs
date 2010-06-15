module PHP.Simple.Ast2Simple(toSimple) where

import PHP.Parser.Ast
import PHP.Parser.Ast.Lex

import qualified PHP.Simple.SimpleAst as St

import qualified Data.Intercal as IC
import Data.Maybe
import Data.Either

import Common

toSimple :: Ast -> St.Program
toSimple (Ast _ stmts) = St.Program $ map fStat (IC.intercalBs stmts)

fStat::Stmt -> St.Statement
fStat (StmtExpr e _ _)              = St.Expr (fExpr e)
fStat (StmtIf (If ifs els))         = let mkIf (IfBlock (WSCap _ (WSCap _ expr _) _) bsts) rest =
                                            [St.If (fExpr expr) (fBoStat bsts) rest]
                                          elseTail = (maybe [] (fBoStat.snd) els) 
                                      in head $ foldr mkIf elseTail (IC.intercalAs ifs)
fStat (StmtFor (For (WSCap _ (s,c,i) _) block)) 
                                    = let forP (ForPart p) = either (const $ St.Const "Nothing") (fExpr. wsCapMain . head) p
                                      in St.For (forP s) (forP c) (forP i) (fBoStat block)
fStat (StmtWhile (While (WSCap _ (WSCap _ expr _) _) bs))
                                    = St.While (fExpr expr) (fBoStat bs)
fStat (StmtBreak _ _ _)             = St.Break  
fStat (StmtContinue  _ _ _)         = St.Continue

fStat (StmtFuncDef (Func _ _ nm (WSCap _ args _) blc)) 
                                    = let extractArg (WSCap _ arg _)= (unparse (funcArgVar arg), isJust (funcArgRef arg))
                                          args' = either (const []) (map extractArg) args 
                                      in St.FuncDef nm args' (fB2Stat blc)
fStat (StmtEcho exps _)             = St.Expr $ St.Func "echo" (map (fExpr.wsCapMain) exps)
fStat (StmtReturn _ exp _)          = St.Return $ maybe (St.Const "Nothing") (fExpr.fst) exp

fStat (StmtNothing _)               = St.Expr (St.Const "Nothing")
fStat s                             = error $ "No support for statement:" ++ show s

fB2Stat :: Block Stmt -> [St.Statement]
fB2Stat (Block b) = map fStat. IC.intercalBs $ b

fBoStat::BlockOrStmt -> [St.Statement]
fBoStat = either ((:[]).fStat) fB2Stat

fExpr :: Expr -> St.Expression
fExpr (ExprAssign b lval _ exp) = St.Assign (unparse lval) (fExpr exp) (maybe "" unparse b)
fExpr (ExprBinOp bop l _ r)     = St.BinOp (fExpr l) (unparse bop) (fExpr r)
fExpr (ExprPostOp op expr _)    = St.UnaryOp (unparse op) (fExpr expr)
fExpr (ExprPreOp op _ expr)     = St.UnaryOp (unparse op) (fExpr expr)
fExpr (ExprNumLit (NumLit c))   = St.Const c
fExpr (ExprStrLit (StrLit c))   = St.Const ("\""++c++"\"")
fExpr (ExprParen (WSCap _ e _)) = fExpr e
fExpr (ExprRVal (RValROnlyVal (ROnlyValFunc fname _ (Right args)))) 
                                = let args' = map (either fExpr (St.Var .unparse).wsCapMain) args
                                  in St.Func (unparse fname) args'
fExpr (ExprRVal val)            = St.Var (unparse val)
fExpr (ExprTernaryIf (TernaryIf c _ t _ e)) 
                                = St.TernaryIf (fExpr c) (fExpr t) (fExpr e)
fExpr s = error $ "No support for expression:" ++ show s
