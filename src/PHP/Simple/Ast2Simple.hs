module PHP.Simple.Ast2Simple(toSimple) where

import PHP.Parser.Ast
import PHP.Parser.Ast.Lex

import qualified PHP.Simple.Expression as Ex
import qualified PHP.Simple.Statement as St

import qualified Data.Intercal as IC
import Data.Maybe
import Data.Either

import Common

toSimple :: Ast -> [St.Statement]
toSimple (Ast _ stmts) = map fStat (IC.intercalBs stmts)

fStat::Stmt -> St.Statement
fStat (StmtExpr (ExprAssign _ lval _ expr) _ _) 
                                    = St.Assign (unparse lval) (fExpr expr)
fStat (StmtExpr e _ _)              = St.Expr (fExpr e)

fStat (StmtIf (If ifs els))         = let (IfBlock (WSCap _ (WSCap _ expr _) _) bsts) = head $ IC.intercalAs ifs                                            
                                      in St.If (fExpr expr) (fBoStat bsts) (maybe [] (fBoStat.snd) els)
fStat (StmtWhile (While (WSCap _ (WSCap _ expr _) _) bs))
                                    = St.While (fExpr expr) (fBoStat bs)
fStat (StmtBreak _ _ _)             = St.Break  
fStat (StmtContinue  _ _ _)         = St.Continue

fStat (StmtFuncDef (Func _ _ nm (WSCap _ args _) blc)) 
                                    = let args' = either (const []) (map unparse) args 
                                      in St.FuncDef nm args' (fB2Stat blc)
fStat (StmtReturn _ exp _)          = St.Return $ fmap (fExpr.fst) exp

fStat (StmtNothing _)               = St.Expr (Ex.Const "Nothing")
fStat s                             = error $ "No support for statement:" ++ show s

fB2Stat :: Block Stmt -> [St.Statement]
fB2Stat (Block b) = map fStat. IC.intercalBs $ b

fBoStat::BlockOrStmt -> [St.Statement]
fBoStat = either ((:[]).fStat) fB2Stat

fExpr :: Expr -> Ex.Expression
fExpr (ExprBinOp bop l _ r)     = Ex.BinOp (fExpr l) (unparse bop) (fExpr r)
fExpr (ExprPostOp op expr _)    = Ex.UnaryOp (unparse op) (fExpr expr)
fExpr (ExprPreOp op _ expr)     = Ex.UnaryOp (unparse op) (fExpr expr)
fExpr (ExprNumLit (NumLit c))   = Ex.Const c
fExpr (ExprStrLit (StrLit c))   = Ex.Const c
fExpr (ExprParen (WSCap _ e _)) = fExpr e
fExpr (ExprRVal (RValROnlyVal (ROnlyValFunc fname _ (Right args)))) 
                                = let args' = map (either fExpr (Ex.Var .unparse).wsCapMain) args
                                  in Ex.Func (unparse fname) args'
fExpr (ExprRVal val)            = Ex.Var (unparse val)
fExpr s = error $ "No support for expression:" ++ show s
