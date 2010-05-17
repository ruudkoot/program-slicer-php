

-- UUAGC 0.9.23 (Statement.ag)
module PHP.Simple.Statement where

import qualified Data.Set as S

{-# LINE 38 "Statement.ag" #-}


usedVars::StatementList -> S.Set String
usedVars = sem_StatementList
{-# LINE 14 "Statement.hs" #-}

{-# LINE 35 "./Expression.ag" #-}

{-# LINE 18 "Statement.hs" #-}
-- Expression --------------------------------------------------
data Expression  = BinOp (Expression ) (String) (Expression ) 
                 | Const (String) 
                 | Func (String) (ExpressionList ) 
                 | UnaryOp (String) (Expression ) 
                 | Var (String) 
                 deriving ( Eq,Show)
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (BinOp _left _op _right )  =
    (sem_Expression_BinOp (sem_Expression _left ) _op (sem_Expression _right ) )
sem_Expression (Const _value )  =
    (sem_Expression_Const _value )
sem_Expression (Func _name _args )  =
    (sem_Expression_Func _name (sem_ExpressionList _args ) )
sem_Expression (UnaryOp _op _exp )  =
    (sem_Expression_UnaryOp _op (sem_Expression _exp ) )
sem_Expression (Var _value )  =
    (sem_Expression_Var _value )
-- semantic domain
type T_Expression  = ( (S.Set String),(S.Set String))
data Inh_Expression  = Inh_Expression {}
data Syn_Expression  = Syn_Expression {funcs_Syn_Expression :: (S.Set String),vars_Syn_Expression :: (S.Set String)}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression sem (Inh_Expression )  =
    (let ( _lhsOfuncs,_lhsOvars) =
             (sem )
     in  (Syn_Expression _lhsOfuncs _lhsOvars ))
sem_Expression_BinOp :: T_Expression  ->
                        String ->
                        T_Expression  ->
                        T_Expression 
sem_Expression_BinOp left_ op_ right_  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _leftIfuncs :: (S.Set String)
         _leftIvars :: (S.Set String)
         _rightIfuncs :: (S.Set String)
         _rightIvars :: (S.Set String)
         _lhsOfuncs =
             ({-# LINE 25 "./Expression.ag" #-}
              _leftIfuncs `S.union` _rightIfuncs
              {-# LINE 64 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _leftIvars `S.union` _rightIvars
              {-# LINE 68 "Statement.hs" #-})
         ( _leftIfuncs,_leftIvars) =
             (left_ )
         ( _rightIfuncs,_rightIvars) =
             (right_ )
     in  ( _lhsOfuncs,_lhsOvars))
sem_Expression_Const :: String ->
                        T_Expression 
sem_Expression_Const value_  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _lhsOfuncs =
             ({-# LINE 25 "./Expression.ag" #-}
              S.empty
              {-# LINE 82 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              S.empty
              {-# LINE 86 "Statement.hs" #-})
     in  ( _lhsOfuncs,_lhsOvars))
sem_Expression_Func :: String ->
                       T_ExpressionList  ->
                       T_Expression 
sem_Expression_Func name_ args_  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _argsIfuncs :: (S.Set String)
         _argsIvars :: (S.Set String)
         _lhsOfuncs =
             ({-# LINE 34 "./Expression.ag" #-}
              foldr ($) _funcs_augmented_syn [_funcs_augmented_f1]
              {-# LINE 99 "Statement.hs" #-})
         _funcs_augmented_f1 =
             ({-# LINE 34 "./Expression.ag" #-}
              S.insert name_
              {-# LINE 103 "Statement.hs" #-})
         _funcs_augmented_syn =
             ({-# LINE 34 "./Expression.ag" #-}
              _argsIfuncs
              {-# LINE 107 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _argsIvars
              {-# LINE 111 "Statement.hs" #-})
         ( _argsIfuncs,_argsIvars) =
             (args_ )
     in  ( _lhsOfuncs,_lhsOvars))
sem_Expression_UnaryOp :: String ->
                          T_Expression  ->
                          T_Expression 
sem_Expression_UnaryOp op_ exp_  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _expIfuncs :: (S.Set String)
         _expIvars :: (S.Set String)
         _lhsOfuncs =
             ({-# LINE 25 "./Expression.ag" #-}
              _expIfuncs
              {-# LINE 126 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _expIvars
              {-# LINE 130 "Statement.hs" #-})
         ( _expIfuncs,_expIvars) =
             (exp_ )
     in  ( _lhsOfuncs,_lhsOvars))
sem_Expression_Var :: String ->
                      T_Expression 
sem_Expression_Var value_  =
    (let _lhsOvars :: (S.Set String)
         _lhsOfuncs :: (S.Set String)
         _lhsOvars =
             ({-# LINE 30 "./Expression.ag" #-}
              S.singleton value_
              {-# LINE 142 "Statement.hs" #-})
         _lhsOfuncs =
             ({-# LINE 25 "./Expression.ag" #-}
              S.empty
              {-# LINE 146 "Statement.hs" #-})
     in  ( _lhsOfuncs,_lhsOvars))
-- ExpressionList ----------------------------------------------
type ExpressionList  = [Expression ]
-- cata
sem_ExpressionList :: ExpressionList  ->
                      T_ExpressionList 
sem_ExpressionList list  =
    (Prelude.foldr sem_ExpressionList_Cons sem_ExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_ExpressionList  = ( (S.Set String),(S.Set String))
data Inh_ExpressionList  = Inh_ExpressionList {}
data Syn_ExpressionList  = Syn_ExpressionList {funcs_Syn_ExpressionList :: (S.Set String),vars_Syn_ExpressionList :: (S.Set String)}
wrap_ExpressionList :: T_ExpressionList  ->
                       Inh_ExpressionList  ->
                       Syn_ExpressionList 
wrap_ExpressionList sem (Inh_ExpressionList )  =
    (let ( _lhsOfuncs,_lhsOvars) =
             (sem )
     in  (Syn_ExpressionList _lhsOfuncs _lhsOvars ))
sem_ExpressionList_Cons :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList 
sem_ExpressionList_Cons hd_ tl_  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _hdIfuncs :: (S.Set String)
         _hdIvars :: (S.Set String)
         _tlIfuncs :: (S.Set String)
         _tlIvars :: (S.Set String)
         _lhsOfuncs =
             ({-# LINE 25 "./Expression.ag" #-}
              _hdIfuncs `S.union` _tlIfuncs
              {-# LINE 179 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _hdIvars `S.union` _tlIvars
              {-# LINE 183 "Statement.hs" #-})
         ( _hdIfuncs,_hdIvars) =
             (hd_ )
         ( _tlIfuncs,_tlIvars) =
             (tl_ )
     in  ( _lhsOfuncs,_lhsOvars))
sem_ExpressionList_Nil :: T_ExpressionList 
sem_ExpressionList_Nil  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _lhsOfuncs =
             ({-# LINE 25 "./Expression.ag" #-}
              S.empty
              {-# LINE 196 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              S.empty
              {-# LINE 200 "Statement.hs" #-})
     in  ( _lhsOfuncs,_lhsOvars))
-- Statement ---------------------------------------------------
data Statement  = Assign (String) (Expression ) 
                | Break 
                | Continue 
                | Expr (Expression ) 
                | FuncDef (String) (([String])) (StatementList ) 
                | If (Expression ) (StatementList ) (StatementList ) 
                | Return (Expression ) 
                | While (Expression ) (StatementList ) 
                deriving ( Eq,Show)
-- cata
sem_Statement :: Statement  ->
                 T_Statement 
sem_Statement (Assign _var _exp )  =
    (sem_Statement_Assign _var (sem_Expression _exp ) )
sem_Statement (Break )  =
    (sem_Statement_Break )
sem_Statement (Continue )  =
    (sem_Statement_Continue )
sem_Statement (Expr _exp )  =
    (sem_Statement_Expr (sem_Expression _exp ) )
sem_Statement (FuncDef _name _args _block )  =
    (sem_Statement_FuncDef _name _args (sem_StatementList _block ) )
sem_Statement (If _exp _then _else )  =
    (sem_Statement_If (sem_Expression _exp ) (sem_StatementList _then ) (sem_StatementList _else ) )
sem_Statement (Return _exp )  =
    (sem_Statement_Return (sem_Expression _exp ) )
sem_Statement (While _exp _block )  =
    (sem_Statement_While (sem_Expression _exp ) (sem_StatementList _block ) )
-- semantic domain
type T_Statement  = ( (S.Set String))
data Inh_Statement  = Inh_Statement {}
data Syn_Statement  = Syn_Statement {vars_Syn_Statement :: (S.Set String)}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement )  =
    (let ( _lhsOvars) =
             (sem )
     in  (Syn_Statement _lhsOvars ))
sem_Statement_Assign :: String ->
                        T_Expression  ->
                        T_Statement 
sem_Statement_Assign var_ exp_  =
    (let _lhsOvars :: (S.Set String)
         _expIfuncs :: (S.Set String)
         _expIvars :: (S.Set String)
         _lhsOvars =
             ({-# LINE 36 "Statement.ag" #-}
              foldr ($) _vars_augmented_syn [_vars_augmented_f1]
              {-# LINE 252 "Statement.hs" #-})
         _vars_augmented_f1 =
             ({-# LINE 36 "Statement.ag" #-}
              S.insert var_
              {-# LINE 256 "Statement.hs" #-})
         _vars_augmented_syn =
             ({-# LINE 36 "Statement.ag" #-}
              _expIvars
              {-# LINE 260 "Statement.hs" #-})
         ( _expIfuncs,_expIvars) =
             (exp_ )
     in  ( _lhsOvars))
sem_Statement_Break :: T_Statement 
sem_Statement_Break  =
    (let _lhsOvars :: (S.Set String)
         _lhsOvars =
             ({-# LINE 31 "Statement.ag" #-}
              S.empty
              {-# LINE 270 "Statement.hs" #-})
     in  ( _lhsOvars))
sem_Statement_Continue :: T_Statement 
sem_Statement_Continue  =
    (let _lhsOvars :: (S.Set String)
         _lhsOvars =
             ({-# LINE 31 "Statement.ag" #-}
              S.empty
              {-# LINE 278 "Statement.hs" #-})
     in  ( _lhsOvars))
sem_Statement_Expr :: T_Expression  ->
                      T_Statement 
sem_Statement_Expr exp_  =
    (let _lhsOvars :: (S.Set String)
         _expIfuncs :: (S.Set String)
         _expIvars :: (S.Set String)
         _lhsOvars =
             ({-# LINE 31 "Statement.ag" #-}
              _expIvars
              {-# LINE 289 "Statement.hs" #-})
         ( _expIfuncs,_expIvars) =
             (exp_ )
     in  ( _lhsOvars))
sem_Statement_FuncDef :: String ->
                         ([String]) ->
                         T_StatementList  ->
                         T_Statement 
sem_Statement_FuncDef name_ args_ block_  =
    (let _lhsOvars :: (S.Set String)
         _blockIvars :: (S.Set String)
         _lhsOvars =
             ({-# LINE 37 "Statement.ag" #-}
              foldr ($) _vars_augmented_syn [_vars_augmented_f1]
              {-# LINE 303 "Statement.hs" #-})
         _vars_augmented_f1 =
             ({-# LINE 37 "Statement.ag" #-}
              S.union (S.fromList args_)
              {-# LINE 307 "Statement.hs" #-})
         _vars_augmented_syn =
             ({-# LINE 37 "Statement.ag" #-}
              _blockIvars
              {-# LINE 311 "Statement.hs" #-})
         ( _blockIvars) =
             (block_ )
     in  ( _lhsOvars))
sem_Statement_If :: T_Expression  ->
                    T_StatementList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If exp_ then_ else_  =
    (let _lhsOvars :: (S.Set String)
         _expIfuncs :: (S.Set String)
         _expIvars :: (S.Set String)
         _thenIvars :: (S.Set String)
         _elseIvars :: (S.Set String)
         _lhsOvars =
             ({-# LINE 31 "Statement.ag" #-}
              _expIvars `S.union` _thenIvars `S.union` _elseIvars
              {-# LINE 328 "Statement.hs" #-})
         ( _expIfuncs,_expIvars) =
             (exp_ )
         ( _thenIvars) =
             (then_ )
         ( _elseIvars) =
             (else_ )
     in  ( _lhsOvars))
sem_Statement_Return :: T_Expression  ->
                        T_Statement 
sem_Statement_Return exp_  =
    (let _lhsOvars :: (S.Set String)
         _expIfuncs :: (S.Set String)
         _expIvars :: (S.Set String)
         _lhsOvars =
             ({-# LINE 31 "Statement.ag" #-}
              _expIvars
              {-# LINE 345 "Statement.hs" #-})
         ( _expIfuncs,_expIvars) =
             (exp_ )
     in  ( _lhsOvars))
sem_Statement_While :: T_Expression  ->
                       T_StatementList  ->
                       T_Statement 
sem_Statement_While exp_ block_  =
    (let _lhsOvars :: (S.Set String)
         _expIfuncs :: (S.Set String)
         _expIvars :: (S.Set String)
         _blockIvars :: (S.Set String)
         _lhsOvars =
             ({-# LINE 31 "Statement.ag" #-}
              _expIvars `S.union` _blockIvars
              {-# LINE 360 "Statement.hs" #-})
         ( _expIfuncs,_expIvars) =
             (exp_ )
         ( _blockIvars) =
             (block_ )
     in  ( _lhsOvars))
-- StatementList -----------------------------------------------
type StatementList  = [Statement ]
-- cata
sem_StatementList :: StatementList  ->
                     T_StatementList 
sem_StatementList list  =
    (Prelude.foldr sem_StatementList_Cons sem_StatementList_Nil (Prelude.map sem_Statement list) )
-- semantic domain
type T_StatementList  = ( (S.Set String))
data Inh_StatementList  = Inh_StatementList {}
data Syn_StatementList  = Syn_StatementList {vars_Syn_StatementList :: (S.Set String)}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList )  =
    (let ( _lhsOvars) =
             (sem )
     in  (Syn_StatementList _lhsOvars ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (let _lhsOvars :: (S.Set String)
         _hdIvars :: (S.Set String)
         _tlIvars :: (S.Set String)
         _lhsOvars =
             ({-# LINE 31 "Statement.ag" #-}
              _hdIvars `S.union` _tlIvars
              {-# LINE 394 "Statement.hs" #-})
         ( _hdIvars) =
             (hd_ )
         ( _tlIvars) =
             (tl_ )
     in  ( _lhsOvars))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (let _lhsOvars :: (S.Set String)
         _lhsOvars =
             ({-# LINE 31 "Statement.ag" #-}
              S.empty
              {-# LINE 406 "Statement.hs" #-})
     in  ( _lhsOvars))