

-- UUAGC 0.9.23 (Statement.ag)
module PHP.Simple.Statement where

import qualified Data.Set as S
import qualified Data.IntMap as IM

{-# LINE 72 "Statement.ag" #-}

nextUnique :: Int -> (Int, Int)
nextUnique u = (u+1, u)

usedVars::StatementList -> S.Set String
usedVars slist = vars_Syn_StatementList $ wrap_StatementList (sem_StatementList slist) inh
    where inh = Inh_StatementList
                { lines_Inh_StatementList = 0
                , labels_Inh_StatementList = IM.empty}

labels::StatementList -> IM.IntMap Statement
labels slist = labels_Syn_StatementList $ wrap_StatementList (sem_StatementList slist) inh
    where inh = Inh_StatementList
                { lines_Inh_StatementList = 0
                , labels_Inh_StatementList = IM.empty}
{-# LINE 26 "Statement.hs" #-}

{-# LINE 37 "./Expression.ag" #-}

{-# LINE 30 "Statement.hs" #-}
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
             ({-# LINE 25 "./Expression.ag" #-}
              _leftIfuncs `S.union` _rightIfuncs
              {-# LINE 82 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _leftIvars `S.union` _rightIvars
              {-# LINE 86 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              BinOp _leftIme op_ _rightIme
              {-# LINE 90 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              BinOp _leftImeSmall op_ _rightImeSmall
              {-# LINE 94 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 98 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 102 "Statement.hs" #-})
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
             ({-# LINE 25 "./Expression.ag" #-}
              S.empty
              {-# LINE 118 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              S.empty
              {-# LINE 122 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              Const value_
              {-# LINE 126 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              Const value_
              {-# LINE 130 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 134 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 138 "Statement.hs" #-})
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
             ({-# LINE 36 "./Expression.ag" #-}
              foldr ($) _funcs_augmented_syn [_funcs_augmented_f1]
              {-# LINE 155 "Statement.hs" #-})
         _funcs_augmented_f1 =
             ({-# LINE 36 "./Expression.ag" #-}
              S.insert name_
              {-# LINE 159 "Statement.hs" #-})
         _funcs_augmented_syn =
             ({-# LINE 36 "./Expression.ag" #-}
              _argsIfuncs
              {-# LINE 163 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _argsIvars
              {-# LINE 167 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              Func name_ _argsIme
              {-# LINE 171 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              Func name_ _argsImeSmall
              {-# LINE 175 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 179 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 183 "Statement.hs" #-})
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
             ({-# LINE 25 "./Expression.ag" #-}
              _expIfuncs
              {-# LINE 202 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _expIvars
              {-# LINE 206 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              UnaryOp op_ _expIme
              {-# LINE 210 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              UnaryOp op_ _expImeSmall
              {-# LINE 214 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 218 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 222 "Statement.hs" #-})
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
             ({-# LINE 32 "./Expression.ag" #-}
              S.singleton value_
              {-# LINE 236 "Statement.hs" #-})
         _lhsOfuncs =
             ({-# LINE 25 "./Expression.ag" #-}
              S.empty
              {-# LINE 240 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              Var value_
              {-# LINE 244 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              Var value_
              {-# LINE 248 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 252 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 256 "Statement.hs" #-})
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
             ({-# LINE 25 "./Expression.ag" #-}
              _hdIfuncs `S.union` _tlIfuncs
              {-# LINE 295 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _hdIvars `S.union` _tlIvars
              {-# LINE 299 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              (:) _hdIme _tlIme
              {-# LINE 303 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              (:) _hdImeSmall _tlImeSmall
              {-# LINE 307 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 311 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 315 "Statement.hs" #-})
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
             ({-# LINE 25 "./Expression.ag" #-}
              S.empty
              {-# LINE 330 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              S.empty
              {-# LINE 334 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              []
              {-# LINE 338 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              []
              {-# LINE 342 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 346 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 350 "Statement.hs" #-})
     in  ( _lhsOfuncs,_lhsOme,_lhsOmeSmall,_lhsOvars))
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
type T_Statement  = (IM.IntMap Statement) ->
                    Int ->
                    ( (IM.IntMap Statement),Int,Statement ,Statement ,(S.Set String))
data Inh_Statement  = Inh_Statement {labels_Inh_Statement :: (IM.IntMap Statement),lines_Inh_Statement :: Int}
data Syn_Statement  = Syn_Statement {labels_Syn_Statement :: (IM.IntMap Statement),lines_Syn_Statement :: Int,me_Syn_Statement :: Statement ,meSmall_Syn_Statement :: Statement ,vars_Syn_Statement :: (S.Set String)}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIlabels _lhsIlines )  =
    (let ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars) =
             (sem _lhsIlabels _lhsIlines )
     in  (Syn_Statement _lhsOlabels _lhsOlines _lhsOme _lhsOmeSmall _lhsOvars ))
sem_Statement_Assign :: String ->
                        T_Expression  ->
                        T_Statement 
sem_Statement_Assign var_ exp_  =
    (\ _lhsIlabels
       _lhsIlines ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              _lhsOvars :: (S.Set String)
              __tup1 :: ((Int,Int))
              _lhsOlines :: Int
              _line :: Int
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _expIfuncs :: (S.Set String)
              _expIme :: Expression 
              _expImeSmall :: Expression 
              _expIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 51 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 414 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.insert _line     _meSmall
                   {-# LINE 418 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 40 "Statement.ag" #-}
                   foldr ($) _vars_augmented_syn [_vars_augmented_f1]
                   {-# LINE 422 "Statement.hs" #-})
              _vars_augmented_f1 =
                  ({-# LINE 40 "Statement.ag" #-}
                   S.insert var_
                   {-# LINE 426 "Statement.hs" #-})
              __tup1 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, line) -> (__cont, line)}}
              (_lhsOlines,_) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup1
                   {-# LINE 432 "Statement.hs" #-})
              (_,_line) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup1
                   {-# LINE 436 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 440 "Statement.hs" #-})
              _vars_augmented_syn =
                  ({-# LINE 40 "Statement.ag" #-}
                   _expIvars
                   {-# LINE 444 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   Assign var_ _expIme
                   {-# LINE 448 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   Assign var_ _expImeSmall
                   {-# LINE 452 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 456 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 460 "Statement.hs" #-})
              ( _expIfuncs,_expIme,_expImeSmall,_expIvars) =
                  (exp_ )
          in  ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_Break :: T_Statement 
sem_Statement_Break  =
    (\ _lhsIlabels
       _lhsIlines ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              __tup2 :: ((Int,Int))
              _lhsOlines :: Int
              _line :: Int
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _lhsOlabels =
                  ({-# LINE 51 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 478 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.insert _line     _meSmall
                   {-# LINE 482 "Statement.hs" #-})
              __tup2 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, line) -> (__cont, line)}}
              (_lhsOlines,_) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup2
                   {-# LINE 488 "Statement.hs" #-})
              (_,_line) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup2
                   {-# LINE 492 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 496 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   S.empty
                   {-# LINE 500 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   Break
                   {-# LINE 504 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   Break
                   {-# LINE 508 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 512 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 516 "Statement.hs" #-})
          in  ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_Continue :: T_Statement 
sem_Statement_Continue  =
    (\ _lhsIlabels
       _lhsIlines ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              __tup3 :: ((Int,Int))
              _lhsOlines :: Int
              _line :: Int
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _lhsOlabels =
                  ({-# LINE 51 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 532 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.insert _line     _meSmall
                   {-# LINE 536 "Statement.hs" #-})
              __tup3 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, line) -> (__cont, line)}}
              (_lhsOlines,_) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup3
                   {-# LINE 542 "Statement.hs" #-})
              (_,_line) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup3
                   {-# LINE 546 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 550 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   S.empty
                   {-# LINE 554 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   Continue
                   {-# LINE 558 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   Continue
                   {-# LINE 562 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 566 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 570 "Statement.hs" #-})
          in  ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_Expr :: T_Expression  ->
                      T_Statement 
sem_Statement_Expr exp_  =
    (\ _lhsIlabels
       _lhsIlines ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              __tup4 :: ((Int,Int))
              _lhsOlines :: Int
              _line :: Int
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _expIfuncs :: (S.Set String)
              _expIme :: Expression 
              _expImeSmall :: Expression 
              _expIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 51 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 591 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.insert _line     _meSmall
                   {-# LINE 595 "Statement.hs" #-})
              __tup4 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, line) -> (__cont, line)}}
              (_lhsOlines,_) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup4
                   {-# LINE 601 "Statement.hs" #-})
              (_,_line) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup4
                   {-# LINE 605 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 609 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   _expIvars
                   {-# LINE 613 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   Expr _expIme
                   {-# LINE 617 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   Expr _expImeSmall
                   {-# LINE 621 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 625 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 629 "Statement.hs" #-})
              ( _expIfuncs,_expIme,_expImeSmall,_expIvars) =
                  (exp_ )
          in  ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_FuncDef :: String ->
                         ([String]) ->
                         T_StatementList  ->
                         T_Statement 
sem_Statement_FuncDef name_ args_ block_  =
    (\ _lhsIlabels
       _lhsIlines ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              _lhsOvars :: (S.Set String)
              __tup5 :: ((Int,Int))
              _blockOlines :: Int
              _line :: Int
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _lhsOlines :: Int
              _blockOlabels :: (IM.IntMap Statement)
              _blockIlabels :: (IM.IntMap Statement)
              _blockIlines :: Int
              _blockIme :: StatementList 
              _blockImeSmall :: StatementList 
              _blockIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 51 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 657 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.insert _line     _meSmall
                   {-# LINE 661 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 41 "Statement.ag" #-}
                   foldr ($) _vars_augmented_syn [_vars_augmented_f1]
                   {-# LINE 665 "Statement.hs" #-})
              _vars_augmented_f1 =
                  ({-# LINE 41 "Statement.ag" #-}
                   S.union (S.fromList args_)
                   {-# LINE 669 "Statement.hs" #-})
              __tup5 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, line) -> (__cont, line)}}
              (_blockOlines,_) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup5
                   {-# LINE 675 "Statement.hs" #-})
              (_,_line) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup5
                   {-# LINE 679 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 51 "Statement.ag" #-}
                   _blockIlabels
                   {-# LINE 683 "Statement.hs" #-})
              _vars_augmented_syn =
                  ({-# LINE 41 "Statement.ag" #-}
                   _blockIvars
                   {-# LINE 687 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   FuncDef name_ args_ _blockIme
                   {-# LINE 691 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   FuncDef name_ args_ _blockImeSmall
                   {-# LINE 695 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 699 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 703 "Statement.hs" #-})
              _lhsOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _blockIlines
                   {-# LINE 707 "Statement.hs" #-})
              _blockOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _lhsIlabels
                   {-# LINE 711 "Statement.hs" #-})
              ( _blockIlabels,_blockIlines,_blockIme,_blockImeSmall,_blockIvars) =
                  (block_ _blockOlabels _blockOlines )
          in  ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_If :: T_Expression  ->
                    T_StatementList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If exp_ then_ else_  =
    (\ _lhsIlabels
       _lhsIlines ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              __tup6 :: ((Int,Int))
              _thenOlines :: Int
              _line :: Int
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _lhsOlines :: Int
              _thenOlabels :: (IM.IntMap Statement)
              _elseOlabels :: (IM.IntMap Statement)
              _elseOlines :: Int
              _expIfuncs :: (S.Set String)
              _expIme :: Expression 
              _expImeSmall :: Expression 
              _expIvars :: (S.Set String)
              _thenIlabels :: (IM.IntMap Statement)
              _thenIlines :: Int
              _thenIme :: StatementList 
              _thenImeSmall :: StatementList 
              _thenIvars :: (S.Set String)
              _elseIlabels :: (IM.IntMap Statement)
              _elseIlines :: Int
              _elseIme :: StatementList 
              _elseImeSmall :: StatementList 
              _elseIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 51 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 750 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.insert _line     _meSmall
                   {-# LINE 754 "Statement.hs" #-})
              __tup6 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, line) -> (__cont, line)}}
              (_thenOlines,_) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup6
                   {-# LINE 760 "Statement.hs" #-})
              (_,_line) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup6
                   {-# LINE 764 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 51 "Statement.ag" #-}
                   _thenIlabels `IM.union` _elseIlabels
                   {-# LINE 768 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   _expIvars `S.union` _thenIvars `S.union` _elseIvars
                   {-# LINE 772 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   If _expIme _thenIme _elseIme
                   {-# LINE 776 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   If _expImeSmall _thenImeSmall _elseImeSmall
                   {-# LINE 780 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 784 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 788 "Statement.hs" #-})
              _lhsOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _elseIlines
                   {-# LINE 792 "Statement.hs" #-})
              _thenOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _lhsIlabels
                   {-# LINE 796 "Statement.hs" #-})
              _elseOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _thenIlabels
                   {-# LINE 800 "Statement.hs" #-})
              _elseOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _thenIlines
                   {-# LINE 804 "Statement.hs" #-})
              ( _expIfuncs,_expIme,_expImeSmall,_expIvars) =
                  (exp_ )
              ( _thenIlabels,_thenIlines,_thenIme,_thenImeSmall,_thenIvars) =
                  (then_ _thenOlabels _thenOlines )
              ( _elseIlabels,_elseIlines,_elseIme,_elseImeSmall,_elseIvars) =
                  (else_ _elseOlabels _elseOlines )
          in  ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_Return :: T_Expression  ->
                        T_Statement 
sem_Statement_Return exp_  =
    (\ _lhsIlabels
       _lhsIlines ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              __tup7 :: ((Int,Int))
              _lhsOlines :: Int
              _line :: Int
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _expIfuncs :: (S.Set String)
              _expIme :: Expression 
              _expImeSmall :: Expression 
              _expIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 51 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 831 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.insert _line     _meSmall
                   {-# LINE 835 "Statement.hs" #-})
              __tup7 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, line) -> (__cont, line)}}
              (_lhsOlines,_) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup7
                   {-# LINE 841 "Statement.hs" #-})
              (_,_line) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup7
                   {-# LINE 845 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 849 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   _expIvars
                   {-# LINE 853 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   Return _expIme
                   {-# LINE 857 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   Return _expImeSmall
                   {-# LINE 861 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 865 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 869 "Statement.hs" #-})
              ( _expIfuncs,_expIme,_expImeSmall,_expIvars) =
                  (exp_ )
          in  ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_While :: T_Expression  ->
                       T_StatementList  ->
                       T_Statement 
sem_Statement_While exp_ block_  =
    (\ _lhsIlabels
       _lhsIlines ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              __tup8 :: ((Int,Int))
              _blockOlines :: Int
              _line :: Int
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _lhsOlines :: Int
              _blockOlabels :: (IM.IntMap Statement)
              _expIfuncs :: (S.Set String)
              _expIme :: Expression 
              _expImeSmall :: Expression 
              _expIvars :: (S.Set String)
              _blockIlabels :: (IM.IntMap Statement)
              _blockIlines :: Int
              _blockIme :: StatementList 
              _blockImeSmall :: StatementList 
              _blockIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 51 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 900 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 51 "Statement.ag" #-}
                   IM.insert _line     _meSmall
                   {-# LINE 904 "Statement.hs" #-})
              __tup8 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, line) -> (__cont, line)}}
              (_blockOlines,_) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup8
                   {-# LINE 910 "Statement.hs" #-})
              (_,_line) =
                  ({-# LINE 50 "Statement.ag" #-}
                   __tup8
                   {-# LINE 914 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 51 "Statement.ag" #-}
                   _blockIlabels
                   {-# LINE 918 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   _expIvars `S.union` _blockIvars
                   {-# LINE 922 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   While _expIme _blockIme
                   {-# LINE 926 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   While _expImeSmall _blockImeSmall
                   {-# LINE 930 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 934 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 938 "Statement.hs" #-})
              _lhsOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _blockIlines
                   {-# LINE 942 "Statement.hs" #-})
              _blockOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _lhsIlabels
                   {-# LINE 946 "Statement.hs" #-})
              ( _expIfuncs,_expIme,_expImeSmall,_expIvars) =
                  (exp_ )
              ( _blockIlabels,_blockIlines,_blockIme,_blockImeSmall,_blockIvars) =
                  (block_ _blockOlabels _blockOlines )
          in  ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
-- StatementList -----------------------------------------------
type StatementList  = [Statement ]
-- cata
sem_StatementList :: StatementList  ->
                     T_StatementList 
sem_StatementList list  =
    (Prelude.foldr sem_StatementList_Cons sem_StatementList_Nil (Prelude.map sem_Statement list) )
-- semantic domain
type T_StatementList  = (IM.IntMap Statement) ->
                        Int ->
                        ( (IM.IntMap Statement),Int,StatementList ,StatementList ,(S.Set String))
data Inh_StatementList  = Inh_StatementList {labels_Inh_StatementList :: (IM.IntMap Statement),lines_Inh_StatementList :: Int}
data Syn_StatementList  = Syn_StatementList {labels_Syn_StatementList :: (IM.IntMap Statement),lines_Syn_StatementList :: Int,me_Syn_StatementList :: StatementList ,meSmall_Syn_StatementList :: StatementList ,vars_Syn_StatementList :: (S.Set String)}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIlabels _lhsIlines )  =
    (let ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars) =
             (sem _lhsIlabels _lhsIlines )
     in  (Syn_StatementList _lhsOlabels _lhsOlines _lhsOme _lhsOmeSmall _lhsOvars ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIlabels
       _lhsIlines ->
         (let _lhsOmeSmall :: StatementList 
              _lhsOlabels :: (IM.IntMap Statement)
              _lhsOvars :: (S.Set String)
              _lhsOme :: StatementList 
              _lhsOlines :: Int
              _hdOlabels :: (IM.IntMap Statement)
              _hdOlines :: Int
              _tlOlabels :: (IM.IntMap Statement)
              _tlOlines :: Int
              _hdIlabels :: (IM.IntMap Statement)
              _hdIlines :: Int
              _hdIme :: Statement 
              _hdImeSmall :: Statement 
              _hdIvars :: (S.Set String)
              _tlIlabels :: (IM.IntMap Statement)
              _tlIlines :: Int
              _tlIme :: StatementList 
              _tlImeSmall :: StatementList 
              _tlIvars :: (S.Set String)
              _lhsOmeSmall =
                  ({-# LINE 45 "Statement.ag" #-}
                   []
                   {-# LINE 1000 "Statement.hs" #-})
              _lhsOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _hdIlabels `IM.union` _tlIlabels
                   {-# LINE 1004 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   _hdIvars `S.union` _tlIvars
                   {-# LINE 1008 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   (:) _hdIme _tlIme
                   {-# LINE 1012 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   (:) _hdImeSmall _tlImeSmall
                   {-# LINE 1016 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 1020 "Statement.hs" #-})
              _lhsOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _tlIlines
                   {-# LINE 1024 "Statement.hs" #-})
              _hdOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _lhsIlabels
                   {-# LINE 1028 "Statement.hs" #-})
              _hdOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _lhsIlines
                   {-# LINE 1032 "Statement.hs" #-})
              _tlOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _hdIlabels
                   {-# LINE 1036 "Statement.hs" #-})
              _tlOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _hdIlines
                   {-# LINE 1040 "Statement.hs" #-})
              ( _hdIlabels,_hdIlines,_hdIme,_hdImeSmall,_hdIvars) =
                  (hd_ _hdOlabels _hdOlines )
              ( _tlIlabels,_tlIlines,_tlIme,_tlImeSmall,_tlIvars) =
                  (tl_ _tlOlabels _tlOlines )
          in  ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIlabels
       _lhsIlines ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              _lhsOvars :: (S.Set String)
              _lhsOme :: StatementList 
              _lhsOmeSmall :: StatementList 
              _lhsOlines :: Int
              _lhsOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 1058 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   S.empty
                   {-# LINE 1062 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   []
                   {-# LINE 1066 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   []
                   {-# LINE 1070 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 1074 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 1078 "Statement.hs" #-})
              _lhsOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _lhsIlines
                   {-# LINE 1082 "Statement.hs" #-})
          in  ( _lhsOlabels,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))