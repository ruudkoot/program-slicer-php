

-- UUAGC 0.9.23 (Expression.ag)
module PHP.Simple.Expression where

import qualified Data.Set as S

{-# LINE 35 "Expression.ag" #-}

{-# LINE 11 "Expression.hs" #-}
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
             ({-# LINE 25 "Expression.ag" #-}
              _leftIfuncs `S.union` _rightIfuncs
              {-# LINE 57 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              _leftIvars `S.union` _rightIvars
              {-# LINE 61 "Expression.hs" #-})
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
             ({-# LINE 25 "Expression.ag" #-}
              S.empty
              {-# LINE 75 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              S.empty
              {-# LINE 79 "Expression.hs" #-})
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
             ({-# LINE 34 "Expression.ag" #-}
              foldr ($) _funcs_augmented_syn [_funcs_augmented_f1]
              {-# LINE 92 "Expression.hs" #-})
         _funcs_augmented_f1 =
             ({-# LINE 34 "Expression.ag" #-}
              S.insert name_
              {-# LINE 96 "Expression.hs" #-})
         _funcs_augmented_syn =
             ({-# LINE 34 "Expression.ag" #-}
              _argsIfuncs
              {-# LINE 100 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              _argsIvars
              {-# LINE 104 "Expression.hs" #-})
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
             ({-# LINE 25 "Expression.ag" #-}
              _expIfuncs
              {-# LINE 119 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              _expIvars
              {-# LINE 123 "Expression.hs" #-})
         ( _expIfuncs,_expIvars) =
             (exp_ )
     in  ( _lhsOfuncs,_lhsOvars))
sem_Expression_Var :: String ->
                      T_Expression 
sem_Expression_Var value_  =
    (let _lhsOvars :: (S.Set String)
         _lhsOfuncs :: (S.Set String)
         _lhsOvars =
             ({-# LINE 30 "Expression.ag" #-}
              S.singleton value_
              {-# LINE 135 "Expression.hs" #-})
         _lhsOfuncs =
             ({-# LINE 25 "Expression.ag" #-}
              S.empty
              {-# LINE 139 "Expression.hs" #-})
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
             ({-# LINE 25 "Expression.ag" #-}
              _hdIfuncs `S.union` _tlIfuncs
              {-# LINE 172 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              _hdIvars `S.union` _tlIvars
              {-# LINE 176 "Expression.hs" #-})
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
             ({-# LINE 25 "Expression.ag" #-}
              S.empty
              {-# LINE 189 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              S.empty
              {-# LINE 193 "Expression.hs" #-})
     in  ( _lhsOfuncs,_lhsOvars))