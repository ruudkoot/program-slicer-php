

-- UUAGC 0.9.23 (Expression.ag)
module PHP.Simple.Expression where

import qualified Data.Set as S

{-# LINE 37 "Expression.ag" #-}

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
type T_Expression  = ( (S.Set String),Expression ,Expression ,(S.Set String))
data Inh_Expression  = Inh_Expression {}
data Syn_Expression  = Syn_Expression {funcs_Syn_Expression :: (S.Set String),me_Syn_Expression :: Expression ,meSmall_Syn_Expression :: Expression ,vars_Syn_Expression :: (S.Set String)}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression sem (Inh_Expression )  =
    (let ( _lhsOfuncs,_lhsOme,_lhsOmeSmall,_lhsOvars) =
             (sem )
     in  (Syn_Expression _lhsOfuncs _lhsOme _lhsOmeSmall _lhsOvars ))
sem_Expression_BinOp :: T_Expression  ->
                        String ->
                        T_Expression  ->
                        T_Expression 
sem_Expression_BinOp left_ op_ right_  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _lhsOme :: Expression 
         _lhsOmeSmall :: Expression 
         _leftIfuncs :: (S.Set String)
         _leftIme :: Expression 
         _leftImeSmall :: Expression 
         _leftIvars :: (S.Set String)
         _rightIfuncs :: (S.Set String)
         _rightIme :: Expression 
         _rightImeSmall :: Expression 
         _rightIvars :: (S.Set String)
         _lhsOfuncs =
             ({-# LINE 25 "Expression.ag" #-}
              _leftIfuncs `S.union` _rightIfuncs
              {-# LINE 63 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              _leftIvars `S.union` _rightIvars
              {-# LINE 67 "Expression.hs" #-})
         _me =
             ({-# LINE 26 "Expression.ag" #-}
              BinOp _leftIme op_ _rightIme
              {-# LINE 71 "Expression.hs" #-})
         _meSmall =
             ({-# LINE 27 "Expression.ag" #-}
              BinOp _leftImeSmall op_ _rightImeSmall
              {-# LINE 75 "Expression.hs" #-})
         _lhsOme =
             ({-# LINE 26 "Expression.ag" #-}
              _me
              {-# LINE 79 "Expression.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "Expression.ag" #-}
              _meSmall
              {-# LINE 83 "Expression.hs" #-})
         ( _leftIfuncs,_leftIme,_leftImeSmall,_leftIvars) =
             (left_ )
         ( _rightIfuncs,_rightIme,_rightImeSmall,_rightIvars) =
             (right_ )
     in  ( _lhsOfuncs,_lhsOme,_lhsOmeSmall,_lhsOvars))
sem_Expression_Const :: String ->
                        T_Expression 
sem_Expression_Const value_  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _lhsOme :: Expression 
         _lhsOmeSmall :: Expression 
         _lhsOfuncs =
             ({-# LINE 25 "Expression.ag" #-}
              S.empty
              {-# LINE 99 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              S.empty
              {-# LINE 103 "Expression.hs" #-})
         _me =
             ({-# LINE 26 "Expression.ag" #-}
              Const value_
              {-# LINE 107 "Expression.hs" #-})
         _meSmall =
             ({-# LINE 27 "Expression.ag" #-}
              Const value_
              {-# LINE 111 "Expression.hs" #-})
         _lhsOme =
             ({-# LINE 26 "Expression.ag" #-}
              _me
              {-# LINE 115 "Expression.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "Expression.ag" #-}
              _meSmall
              {-# LINE 119 "Expression.hs" #-})
     in  ( _lhsOfuncs,_lhsOme,_lhsOmeSmall,_lhsOvars))
sem_Expression_Func :: String ->
                       T_ExpressionList  ->
                       T_Expression 
sem_Expression_Func name_ args_  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _lhsOme :: Expression 
         _lhsOmeSmall :: Expression 
         _argsIfuncs :: (S.Set String)
         _argsIme :: ExpressionList 
         _argsImeSmall :: ExpressionList 
         _argsIvars :: (S.Set String)
         _lhsOfuncs =
             ({-# LINE 36 "Expression.ag" #-}
              foldr ($) _funcs_augmented_syn [_funcs_augmented_f1]
              {-# LINE 136 "Expression.hs" #-})
         _funcs_augmented_f1 =
             ({-# LINE 36 "Expression.ag" #-}
              S.insert name_
              {-# LINE 140 "Expression.hs" #-})
         _funcs_augmented_syn =
             ({-# LINE 36 "Expression.ag" #-}
              _argsIfuncs
              {-# LINE 144 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              _argsIvars
              {-# LINE 148 "Expression.hs" #-})
         _me =
             ({-# LINE 26 "Expression.ag" #-}
              Func name_ _argsIme
              {-# LINE 152 "Expression.hs" #-})
         _meSmall =
             ({-# LINE 27 "Expression.ag" #-}
              Func name_ _argsImeSmall
              {-# LINE 156 "Expression.hs" #-})
         _lhsOme =
             ({-# LINE 26 "Expression.ag" #-}
              _me
              {-# LINE 160 "Expression.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "Expression.ag" #-}
              _meSmall
              {-# LINE 164 "Expression.hs" #-})
         ( _argsIfuncs,_argsIme,_argsImeSmall,_argsIvars) =
             (args_ )
     in  ( _lhsOfuncs,_lhsOme,_lhsOmeSmall,_lhsOvars))
sem_Expression_UnaryOp :: String ->
                          T_Expression  ->
                          T_Expression 
sem_Expression_UnaryOp op_ exp_  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _lhsOme :: Expression 
         _lhsOmeSmall :: Expression 
         _expIfuncs :: (S.Set String)
         _expIme :: Expression 
         _expImeSmall :: Expression 
         _expIvars :: (S.Set String)
         _lhsOfuncs =
             ({-# LINE 25 "Expression.ag" #-}
              _expIfuncs
              {-# LINE 183 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              _expIvars
              {-# LINE 187 "Expression.hs" #-})
         _me =
             ({-# LINE 26 "Expression.ag" #-}
              UnaryOp op_ _expIme
              {-# LINE 191 "Expression.hs" #-})
         _meSmall =
             ({-# LINE 27 "Expression.ag" #-}
              UnaryOp op_ _expImeSmall
              {-# LINE 195 "Expression.hs" #-})
         _lhsOme =
             ({-# LINE 26 "Expression.ag" #-}
              _me
              {-# LINE 199 "Expression.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "Expression.ag" #-}
              _meSmall
              {-# LINE 203 "Expression.hs" #-})
         ( _expIfuncs,_expIme,_expImeSmall,_expIvars) =
             (exp_ )
     in  ( _lhsOfuncs,_lhsOme,_lhsOmeSmall,_lhsOvars))
sem_Expression_Var :: String ->
                      T_Expression 
sem_Expression_Var value_  =
    (let _lhsOvars :: (S.Set String)
         _lhsOfuncs :: (S.Set String)
         _lhsOme :: Expression 
         _lhsOmeSmall :: Expression 
         _lhsOvars =
             ({-# LINE 32 "Expression.ag" #-}
              S.singleton value_
              {-# LINE 217 "Expression.hs" #-})
         _lhsOfuncs =
             ({-# LINE 25 "Expression.ag" #-}
              S.empty
              {-# LINE 221 "Expression.hs" #-})
         _me =
             ({-# LINE 26 "Expression.ag" #-}
              Var value_
              {-# LINE 225 "Expression.hs" #-})
         _meSmall =
             ({-# LINE 27 "Expression.ag" #-}
              Var value_
              {-# LINE 229 "Expression.hs" #-})
         _lhsOme =
             ({-# LINE 26 "Expression.ag" #-}
              _me
              {-# LINE 233 "Expression.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "Expression.ag" #-}
              _meSmall
              {-# LINE 237 "Expression.hs" #-})
     in  ( _lhsOfuncs,_lhsOme,_lhsOmeSmall,_lhsOvars))
-- ExpressionList ----------------------------------------------
type ExpressionList  = [Expression ]
-- cata
sem_ExpressionList :: ExpressionList  ->
                      T_ExpressionList 
sem_ExpressionList list  =
    (Prelude.foldr sem_ExpressionList_Cons sem_ExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_ExpressionList  = ( (S.Set String),ExpressionList ,ExpressionList ,(S.Set String))
data Inh_ExpressionList  = Inh_ExpressionList {}
data Syn_ExpressionList  = Syn_ExpressionList {funcs_Syn_ExpressionList :: (S.Set String),me_Syn_ExpressionList :: ExpressionList ,meSmall_Syn_ExpressionList :: ExpressionList ,vars_Syn_ExpressionList :: (S.Set String)}
wrap_ExpressionList :: T_ExpressionList  ->
                       Inh_ExpressionList  ->
                       Syn_ExpressionList 
wrap_ExpressionList sem (Inh_ExpressionList )  =
    (let ( _lhsOfuncs,_lhsOme,_lhsOmeSmall,_lhsOvars) =
             (sem )
     in  (Syn_ExpressionList _lhsOfuncs _lhsOme _lhsOmeSmall _lhsOvars ))
sem_ExpressionList_Cons :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList 
sem_ExpressionList_Cons hd_ tl_  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _lhsOme :: ExpressionList 
         _lhsOmeSmall :: ExpressionList 
         _hdIfuncs :: (S.Set String)
         _hdIme :: Expression 
         _hdImeSmall :: Expression 
         _hdIvars :: (S.Set String)
         _tlIfuncs :: (S.Set String)
         _tlIme :: ExpressionList 
         _tlImeSmall :: ExpressionList 
         _tlIvars :: (S.Set String)
         _lhsOfuncs =
             ({-# LINE 25 "Expression.ag" #-}
              _hdIfuncs `S.union` _tlIfuncs
              {-# LINE 276 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              _hdIvars `S.union` _tlIvars
              {-# LINE 280 "Expression.hs" #-})
         _me =
             ({-# LINE 26 "Expression.ag" #-}
              (:) _hdIme _tlIme
              {-# LINE 284 "Expression.hs" #-})
         _meSmall =
             ({-# LINE 27 "Expression.ag" #-}
              (:) _hdImeSmall _tlImeSmall
              {-# LINE 288 "Expression.hs" #-})
         _lhsOme =
             ({-# LINE 26 "Expression.ag" #-}
              _me
              {-# LINE 292 "Expression.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "Expression.ag" #-}
              _meSmall
              {-# LINE 296 "Expression.hs" #-})
         ( _hdIfuncs,_hdIme,_hdImeSmall,_hdIvars) =
             (hd_ )
         ( _tlIfuncs,_tlIme,_tlImeSmall,_tlIvars) =
             (tl_ )
     in  ( _lhsOfuncs,_lhsOme,_lhsOmeSmall,_lhsOvars))
sem_ExpressionList_Nil :: T_ExpressionList 
sem_ExpressionList_Nil  =
    (let _lhsOfuncs :: (S.Set String)
         _lhsOvars :: (S.Set String)
         _lhsOme :: ExpressionList 
         _lhsOmeSmall :: ExpressionList 
         _lhsOfuncs =
             ({-# LINE 25 "Expression.ag" #-}
              S.empty
              {-# LINE 311 "Expression.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "Expression.ag" #-}
              S.empty
              {-# LINE 315 "Expression.hs" #-})
         _me =
             ({-# LINE 26 "Expression.ag" #-}
              []
              {-# LINE 319 "Expression.hs" #-})
         _meSmall =
             ({-# LINE 27 "Expression.ag" #-}
              []
              {-# LINE 323 "Expression.hs" #-})
         _lhsOme =
             ({-# LINE 26 "Expression.ag" #-}
              _me
              {-# LINE 327 "Expression.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "Expression.ag" #-}
              _meSmall
              {-# LINE 331 "Expression.hs" #-})
     in  ( _lhsOfuncs,_lhsOme,_lhsOmeSmall,_lhsOvars))