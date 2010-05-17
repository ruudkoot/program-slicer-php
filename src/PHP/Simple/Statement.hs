

-- UUAGC 0.9.23 (Statement.ag)
module PHP.Simple.Statement where

import qualified Data.Set as S
import qualified Data.IntMap as IM

{-# LINE 89 "Statement.ag" #-}


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

flow::StatementList -> [(Int,Int)]
flow slist = flow_Syn_StatementList $ wrap_StatementList (sem_StatementList slist) inh
    where inh = Inh_StatementList
                { lines_Inh_StatementList = 0
                , labels_Inh_StatementList = IM.empty}
{-# LINE 33 "Statement.hs" #-}

{-# LINE 37 "./Expression.ag" #-}

{-# LINE 37 "Statement.hs" #-}
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
              {-# LINE 89 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _leftIvars `S.union` _rightIvars
              {-# LINE 93 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              BinOp _leftIme op_ _rightIme
              {-# LINE 97 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              BinOp _leftImeSmall op_ _rightImeSmall
              {-# LINE 101 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 105 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 109 "Statement.hs" #-})
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
              {-# LINE 125 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              S.empty
              {-# LINE 129 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              Const value_
              {-# LINE 133 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              Const value_
              {-# LINE 137 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 141 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 145 "Statement.hs" #-})
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
              {-# LINE 162 "Statement.hs" #-})
         _funcs_augmented_f1 =
             ({-# LINE 36 "./Expression.ag" #-}
              S.insert name_
              {-# LINE 166 "Statement.hs" #-})
         _funcs_augmented_syn =
             ({-# LINE 36 "./Expression.ag" #-}
              _argsIfuncs
              {-# LINE 170 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _argsIvars
              {-# LINE 174 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              Func name_ _argsIme
              {-# LINE 178 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              Func name_ _argsImeSmall
              {-# LINE 182 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 186 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 190 "Statement.hs" #-})
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
              {-# LINE 209 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _expIvars
              {-# LINE 213 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              UnaryOp op_ _expIme
              {-# LINE 217 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              UnaryOp op_ _expImeSmall
              {-# LINE 221 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 225 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 229 "Statement.hs" #-})
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
              {-# LINE 243 "Statement.hs" #-})
         _lhsOfuncs =
             ({-# LINE 25 "./Expression.ag" #-}
              S.empty
              {-# LINE 247 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              Var value_
              {-# LINE 251 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              Var value_
              {-# LINE 255 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 259 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 263 "Statement.hs" #-})
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
              {-# LINE 302 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              _hdIvars `S.union` _tlIvars
              {-# LINE 306 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              (:) _hdIme _tlIme
              {-# LINE 310 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              (:) _hdImeSmall _tlImeSmall
              {-# LINE 314 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 318 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 322 "Statement.hs" #-})
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
              {-# LINE 337 "Statement.hs" #-})
         _lhsOvars =
             ({-# LINE 24 "./Expression.ag" #-}
              S.empty
              {-# LINE 341 "Statement.hs" #-})
         _me =
             ({-# LINE 26 "./Expression.ag" #-}
              []
              {-# LINE 345 "Statement.hs" #-})
         _meSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              []
              {-# LINE 349 "Statement.hs" #-})
         _lhsOme =
             ({-# LINE 26 "./Expression.ag" #-}
              _me
              {-# LINE 353 "Statement.hs" #-})
         _lhsOmeSmall =
             ({-# LINE 27 "./Expression.ag" #-}
              _meSmall
              {-# LINE 357 "Statement.hs" #-})
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
sem_Statement (If _exp _thn _els )  =
    (sem_Statement_If (sem_Expression _exp ) (sem_StatementList _thn ) (sem_StatementList _els ) )
sem_Statement (Return _exp )  =
    (sem_Statement_Return (sem_Expression _exp ) )
sem_Statement (While _exp _block )  =
    (sem_Statement_While (sem_Expression _exp ) (sem_StatementList _block ) )
-- semantic domain
type T_Statement  = (IM.IntMap Statement) ->
                    Int ->
                    Int ->
                    ( ([(Int,Int)]),(IM.IntMap Statement),Int,Int,Statement ,Statement ,(S.Set String))
data Inh_Statement  = Inh_Statement {labels_Inh_Statement :: (IM.IntMap Statement),lines_Inh_Statement :: Int,outLabel_Inh_Statement :: Int}
data Syn_Statement  = Syn_Statement {flow_Syn_Statement :: ([(Int,Int)]),labels_Syn_Statement :: (IM.IntMap Statement),line_Syn_Statement :: Int,lines_Syn_Statement :: Int,me_Syn_Statement :: Statement ,meSmall_Syn_Statement :: Statement ,vars_Syn_Statement :: (S.Set String)}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIlabels _lhsIlines _lhsIoutLabel )  =
    (let ( _lhsOflow,_lhsOlabels,_lhsOline,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars) =
             (sem _lhsIlabels _lhsIlines _lhsIoutLabel )
     in  (Syn_Statement _lhsOflow _lhsOlabels _lhsOline _lhsOlines _lhsOme _lhsOmeSmall _lhsOvars ))
sem_Statement_Assign :: String ->
                        T_Expression  ->
                        T_Statement 
sem_Statement_Assign var_ exp_  =
    (\ _lhsIlabels
       _lhsIlines
       _lhsIoutLabel ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              _lhsOvars :: (S.Set String)
              _lhsOline :: Int
              __tup1 :: ((Int,Int))
              _lhsOlines :: Int
              _linel :: Int
              _lhsOflow :: ([(Int,Int)])
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _expIfuncs :: (S.Set String)
              _expIme :: Expression 
              _expImeSmall :: Expression 
              _expIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 60 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 425 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.insert _linel     _meSmall
                   {-# LINE 429 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 48 "Statement.ag" #-}
                   foldr ($) _vars_augmented_syn [_vars_augmented_f1]
                   {-# LINE 433 "Statement.hs" #-})
              _vars_augmented_f1 =
                  ({-# LINE 48 "Statement.ag" #-}
                   S.insert var_
                   {-# LINE 437 "Statement.hs" #-})
              _lhsOline =
                  ({-# LINE 59 "Statement.ag" #-}
                   _linel
                   {-# LINE 441 "Statement.hs" #-})
              __tup1 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, linel) -> (__cont, linel)}}
              (_lhsOlines,_) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup1
                   {-# LINE 447 "Statement.hs" #-})
              (_,_linel) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup1
                   {-# LINE 451 "Statement.hs" #-})
              _lhsOflow =
                  ({-# LINE 36 "Statement.ag" #-}
                   []
                   {-# LINE 455 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 459 "Statement.hs" #-})
              _vars_augmented_syn =
                  ({-# LINE 48 "Statement.ag" #-}
                   _expIvars
                   {-# LINE 463 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   Assign var_ _expIme
                   {-# LINE 467 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   Assign var_ _expImeSmall
                   {-# LINE 471 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 475 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 479 "Statement.hs" #-})
              ( _expIfuncs,_expIme,_expImeSmall,_expIvars) =
                  (exp_ )
          in  ( _lhsOflow,_lhsOlabels,_lhsOline,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_Break :: T_Statement 
sem_Statement_Break  =
    (\ _lhsIlabels
       _lhsIlines
       _lhsIoutLabel ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              _lhsOline :: Int
              __tup2 :: ((Int,Int))
              _lhsOlines :: Int
              _linel :: Int
              _lhsOflow :: ([(Int,Int)])
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _lhsOlabels =
                  ({-# LINE 60 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 500 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.insert _linel     _meSmall
                   {-# LINE 504 "Statement.hs" #-})
              _lhsOline =
                  ({-# LINE 59 "Statement.ag" #-}
                   _linel
                   {-# LINE 508 "Statement.hs" #-})
              __tup2 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, linel) -> (__cont, linel)}}
              (_lhsOlines,_) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup2
                   {-# LINE 514 "Statement.hs" #-})
              (_,_linel) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup2
                   {-# LINE 518 "Statement.hs" #-})
              _lhsOflow =
                  ({-# LINE 36 "Statement.ag" #-}
                   []
                   {-# LINE 522 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 526 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   S.empty
                   {-# LINE 530 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   Break
                   {-# LINE 534 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   Break
                   {-# LINE 538 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 542 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 546 "Statement.hs" #-})
          in  ( _lhsOflow,_lhsOlabels,_lhsOline,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_Continue :: T_Statement 
sem_Statement_Continue  =
    (\ _lhsIlabels
       _lhsIlines
       _lhsIoutLabel ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              _lhsOline :: Int
              __tup3 :: ((Int,Int))
              _lhsOlines :: Int
              _linel :: Int
              _lhsOflow :: ([(Int,Int)])
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _lhsOlabels =
                  ({-# LINE 60 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 565 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.insert _linel     _meSmall
                   {-# LINE 569 "Statement.hs" #-})
              _lhsOline =
                  ({-# LINE 59 "Statement.ag" #-}
                   _linel
                   {-# LINE 573 "Statement.hs" #-})
              __tup3 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, linel) -> (__cont, linel)}}
              (_lhsOlines,_) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup3
                   {-# LINE 579 "Statement.hs" #-})
              (_,_linel) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup3
                   {-# LINE 583 "Statement.hs" #-})
              _lhsOflow =
                  ({-# LINE 36 "Statement.ag" #-}
                   []
                   {-# LINE 587 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 591 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   S.empty
                   {-# LINE 595 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   Continue
                   {-# LINE 599 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   Continue
                   {-# LINE 603 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 607 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 611 "Statement.hs" #-})
          in  ( _lhsOflow,_lhsOlabels,_lhsOline,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_Expr :: T_Expression  ->
                      T_Statement 
sem_Statement_Expr exp_  =
    (\ _lhsIlabels
       _lhsIlines
       _lhsIoutLabel ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              _lhsOline :: Int
              __tup4 :: ((Int,Int))
              _lhsOlines :: Int
              _linel :: Int
              _lhsOflow :: ([(Int,Int)])
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _expIfuncs :: (S.Set String)
              _expIme :: Expression 
              _expImeSmall :: Expression 
              _expIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 60 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 635 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.insert _linel     _meSmall
                   {-# LINE 639 "Statement.hs" #-})
              _lhsOline =
                  ({-# LINE 59 "Statement.ag" #-}
                   _linel
                   {-# LINE 643 "Statement.hs" #-})
              __tup4 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, linel) -> (__cont, linel)}}
              (_lhsOlines,_) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup4
                   {-# LINE 649 "Statement.hs" #-})
              (_,_linel) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup4
                   {-# LINE 653 "Statement.hs" #-})
              _lhsOflow =
                  ({-# LINE 36 "Statement.ag" #-}
                   []
                   {-# LINE 657 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 661 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   _expIvars
                   {-# LINE 665 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   Expr _expIme
                   {-# LINE 669 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   Expr _expImeSmall
                   {-# LINE 673 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 677 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 681 "Statement.hs" #-})
              ( _expIfuncs,_expIme,_expImeSmall,_expIvars) =
                  (exp_ )
          in  ( _lhsOflow,_lhsOlabels,_lhsOline,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_FuncDef :: String ->
                         ([String]) ->
                         T_StatementList  ->
                         T_Statement 
sem_Statement_FuncDef name_ args_ block_  =
    (\ _lhsIlabels
       _lhsIlines
       _lhsIoutLabel ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              _lhsOvars :: (S.Set String)
              _lhsOline :: Int
              __tup5 :: ((Int,Int))
              _blockOlines :: Int
              _linel :: Int
              _lhsOflow :: ([(Int,Int)])
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _lhsOlines :: Int
              _blockOlabels :: (IM.IntMap Statement)
              _blockOoutLabel :: Int
              _blockIfirstLabel :: Int
              _blockIflow :: ([(Int,Int)])
              _blockIlabels :: (IM.IntMap Statement)
              _blockIlastLabel :: Int
              _blockIlines :: Int
              _blockIme :: StatementList 
              _blockImeSmall :: StatementList 
              _blockIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 60 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 716 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.insert _linel     _meSmall
                   {-# LINE 720 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 49 "Statement.ag" #-}
                   foldr ($) _vars_augmented_syn [_vars_augmented_f1]
                   {-# LINE 724 "Statement.hs" #-})
              _vars_augmented_f1 =
                  ({-# LINE 49 "Statement.ag" #-}
                   S.union (S.fromList args_)
                   {-# LINE 728 "Statement.hs" #-})
              _lhsOline =
                  ({-# LINE 59 "Statement.ag" #-}
                   _linel
                   {-# LINE 732 "Statement.hs" #-})
              __tup5 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, linel) -> (__cont, linel)}}
              (_blockOlines,_) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup5
                   {-# LINE 738 "Statement.hs" #-})
              (_,_linel) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup5
                   {-# LINE 742 "Statement.hs" #-})
              _lhsOflow =
                  ({-# LINE 36 "Statement.ag" #-}
                   _blockIflow
                   {-# LINE 746 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 60 "Statement.ag" #-}
                   _blockIlabels
                   {-# LINE 750 "Statement.hs" #-})
              _vars_augmented_syn =
                  ({-# LINE 49 "Statement.ag" #-}
                   _blockIvars
                   {-# LINE 754 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   FuncDef name_ args_ _blockIme
                   {-# LINE 758 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   FuncDef name_ args_ _blockImeSmall
                   {-# LINE 762 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 766 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 770 "Statement.hs" #-})
              _lhsOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _blockIlines
                   {-# LINE 774 "Statement.hs" #-})
              _blockOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _lhsIlabels
                   {-# LINE 778 "Statement.hs" #-})
              _blockOoutLabel =
                  ({-# LINE 69 "Statement.ag" #-}
                   _lhsIoutLabel
                   {-# LINE 782 "Statement.hs" #-})
              ( _blockIfirstLabel,_blockIflow,_blockIlabels,_blockIlastLabel,_blockIlines,_blockIme,_blockImeSmall,_blockIvars) =
                  (block_ _blockOlabels _blockOlines _blockOoutLabel )
          in  ( _lhsOflow,_lhsOlabels,_lhsOline,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_If :: T_Expression  ->
                    T_StatementList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If exp_ thn_ els_  =
    (\ _lhsIlabels
       _lhsIlines
       _lhsIoutLabel ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              _lhsOline :: Int
              _lhsOflow :: ([(Int,Int)])
              __tup6 :: ((Int,Int))
              _thnOlines :: Int
              _linel :: Int
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _lhsOlines :: Int
              _thnOlabels :: (IM.IntMap Statement)
              _thnOoutLabel :: Int
              _elsOlabels :: (IM.IntMap Statement)
              _elsOlines :: Int
              _elsOoutLabel :: Int
              _expIfuncs :: (S.Set String)
              _expIme :: Expression 
              _expImeSmall :: Expression 
              _expIvars :: (S.Set String)
              _thnIfirstLabel :: Int
              _thnIflow :: ([(Int,Int)])
              _thnIlabels :: (IM.IntMap Statement)
              _thnIlastLabel :: Int
              _thnIlines :: Int
              _thnIme :: StatementList 
              _thnImeSmall :: StatementList 
              _thnIvars :: (S.Set String)
              _elsIfirstLabel :: Int
              _elsIflow :: ([(Int,Int)])
              _elsIlabels :: (IM.IntMap Statement)
              _elsIlastLabel :: Int
              _elsIlines :: Int
              _elsIme :: StatementList 
              _elsImeSmall :: StatementList 
              _elsIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 60 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 832 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.insert _linel     _meSmall
                   {-# LINE 836 "Statement.hs" #-})
              _lhsOline =
                  ({-# LINE 59 "Statement.ag" #-}
                   _linel
                   {-# LINE 840 "Statement.hs" #-})
              _lhsOflow =
                  ({-# LINE 85 "Statement.ag" #-}
                   (_linel    , _thnIfirstLabel):(_linel    , _elsIfirstLabel): _thnIflow ++ _elsIflow
                   {-# LINE 844 "Statement.hs" #-})
              __tup6 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, linel) -> (__cont, linel)}}
              (_thnOlines,_) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup6
                   {-# LINE 850 "Statement.hs" #-})
              (_,_linel) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup6
                   {-# LINE 854 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 60 "Statement.ag" #-}
                   _thnIlabels `IM.union` _elsIlabels
                   {-# LINE 858 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   _expIvars `S.union` _thnIvars `S.union` _elsIvars
                   {-# LINE 862 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   If _expIme _thnIme _elsIme
                   {-# LINE 866 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   If _expImeSmall _thnImeSmall _elsImeSmall
                   {-# LINE 870 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 874 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 878 "Statement.hs" #-})
              _lhsOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _elsIlines
                   {-# LINE 882 "Statement.hs" #-})
              _thnOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _lhsIlabels
                   {-# LINE 886 "Statement.hs" #-})
              _thnOoutLabel =
                  ({-# LINE 69 "Statement.ag" #-}
                   _lhsIoutLabel
                   {-# LINE 890 "Statement.hs" #-})
              _elsOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _thnIlabels
                   {-# LINE 894 "Statement.hs" #-})
              _elsOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _thnIlines
                   {-# LINE 898 "Statement.hs" #-})
              _elsOoutLabel =
                  ({-# LINE 69 "Statement.ag" #-}
                   _lhsIoutLabel
                   {-# LINE 902 "Statement.hs" #-})
              ( _expIfuncs,_expIme,_expImeSmall,_expIvars) =
                  (exp_ )
              ( _thnIfirstLabel,_thnIflow,_thnIlabels,_thnIlastLabel,_thnIlines,_thnIme,_thnImeSmall,_thnIvars) =
                  (thn_ _thnOlabels _thnOlines _thnOoutLabel )
              ( _elsIfirstLabel,_elsIflow,_elsIlabels,_elsIlastLabel,_elsIlines,_elsIme,_elsImeSmall,_elsIvars) =
                  (els_ _elsOlabels _elsOlines _elsOoutLabel )
          in  ( _lhsOflow,_lhsOlabels,_lhsOline,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_Return :: T_Expression  ->
                        T_Statement 
sem_Statement_Return exp_  =
    (\ _lhsIlabels
       _lhsIlines
       _lhsIoutLabel ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              _lhsOline :: Int
              __tup7 :: ((Int,Int))
              _lhsOlines :: Int
              _linel :: Int
              _lhsOflow :: ([(Int,Int)])
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _expIfuncs :: (S.Set String)
              _expIme :: Expression 
              _expImeSmall :: Expression 
              _expIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 60 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 932 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.insert _linel     _meSmall
                   {-# LINE 936 "Statement.hs" #-})
              _lhsOline =
                  ({-# LINE 59 "Statement.ag" #-}
                   _linel
                   {-# LINE 940 "Statement.hs" #-})
              __tup7 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, linel) -> (__cont, linel)}}
              (_lhsOlines,_) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup7
                   {-# LINE 946 "Statement.hs" #-})
              (_,_linel) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup7
                   {-# LINE 950 "Statement.hs" #-})
              _lhsOflow =
                  ({-# LINE 36 "Statement.ag" #-}
                   []
                   {-# LINE 954 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 958 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   _expIvars
                   {-# LINE 962 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   Return _expIme
                   {-# LINE 966 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   Return _expImeSmall
                   {-# LINE 970 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 974 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 978 "Statement.hs" #-})
              ( _expIfuncs,_expIme,_expImeSmall,_expIvars) =
                  (exp_ )
          in  ( _lhsOflow,_lhsOlabels,_lhsOline,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_Statement_While :: T_Expression  ->
                       T_StatementList  ->
                       T_Statement 
sem_Statement_While exp_ block_  =
    (\ _lhsIlabels
       _lhsIlines
       _lhsIoutLabel ->
         (let _lhsOlabels :: (IM.IntMap Statement)
              _lhsOline :: Int
              _blockOoutLabel :: Int
              __tup8 :: ((Int,Int))
              _blockOlines :: Int
              _linel :: Int
              _lhsOflow :: ([(Int,Int)])
              _lhsOvars :: (S.Set String)
              _lhsOme :: Statement 
              _lhsOmeSmall :: Statement 
              _lhsOlines :: Int
              _blockOlabels :: (IM.IntMap Statement)
              _expIfuncs :: (S.Set String)
              _expIme :: Expression 
              _expImeSmall :: Expression 
              _expIvars :: (S.Set String)
              _blockIfirstLabel :: Int
              _blockIflow :: ([(Int,Int)])
              _blockIlabels :: (IM.IntMap Statement)
              _blockIlastLabel :: Int
              _blockIlines :: Int
              _blockIme :: StatementList 
              _blockImeSmall :: StatementList 
              _blockIvars :: (S.Set String)
              _lhsOlabels =
                  ({-# LINE 60 "Statement.ag" #-}
                   foldr ($) _labels_augmented_syn [_labels_augmented_f1]
                   {-# LINE 1016 "Statement.hs" #-})
              _labels_augmented_f1 =
                  ({-# LINE 60 "Statement.ag" #-}
                   IM.insert _linel     _meSmall
                   {-# LINE 1020 "Statement.hs" #-})
              _lhsOline =
                  ({-# LINE 59 "Statement.ag" #-}
                   _linel
                   {-# LINE 1024 "Statement.hs" #-})
              _blockOoutLabel =
                  ({-# LINE 86 "Statement.ag" #-}
                   _linel
                   {-# LINE 1028 "Statement.hs" #-})
              __tup8 =
                  case _lhsIlines of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, linel) -> (__cont, linel)}}
              (_blockOlines,_) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup8
                   {-# LINE 1034 "Statement.hs" #-})
              (_,_linel) =
                  ({-# LINE 58 "Statement.ag" #-}
                   __tup8
                   {-# LINE 1038 "Statement.hs" #-})
              _lhsOflow =
                  ({-# LINE 36 "Statement.ag" #-}
                   _blockIflow
                   {-# LINE 1042 "Statement.hs" #-})
              _labels_augmented_syn =
                  ({-# LINE 60 "Statement.ag" #-}
                   _blockIlabels
                   {-# LINE 1046 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   _expIvars `S.union` _blockIvars
                   {-# LINE 1050 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   While _expIme _blockIme
                   {-# LINE 1054 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   While _expImeSmall _blockImeSmall
                   {-# LINE 1058 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 1062 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 1066 "Statement.hs" #-})
              _lhsOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _blockIlines
                   {-# LINE 1070 "Statement.hs" #-})
              _blockOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _lhsIlabels
                   {-# LINE 1074 "Statement.hs" #-})
              ( _expIfuncs,_expIme,_expImeSmall,_expIvars) =
                  (exp_ )
              ( _blockIfirstLabel,_blockIflow,_blockIlabels,_blockIlastLabel,_blockIlines,_blockIme,_blockImeSmall,_blockIvars) =
                  (block_ _blockOlabels _blockOlines _blockOoutLabel )
          in  ( _lhsOflow,_lhsOlabels,_lhsOline,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
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
                        Int ->
                        ( Int,([(Int,Int)]),(IM.IntMap Statement),Int,Int,StatementList ,StatementList ,(S.Set String))
data Inh_StatementList  = Inh_StatementList {labels_Inh_StatementList :: (IM.IntMap Statement),lines_Inh_StatementList :: Int,outLabel_Inh_StatementList :: Int}
data Syn_StatementList  = Syn_StatementList {firstLabel_Syn_StatementList :: Int,flow_Syn_StatementList :: ([(Int,Int)]),labels_Syn_StatementList :: (IM.IntMap Statement),lastLabel_Syn_StatementList :: Int,lines_Syn_StatementList :: Int,me_Syn_StatementList :: StatementList ,meSmall_Syn_StatementList :: StatementList ,vars_Syn_StatementList :: (S.Set String)}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIlabels _lhsIlines _lhsIoutLabel )  =
    (let ( _lhsOfirstLabel,_lhsOflow,_lhsOlabels,_lhsOlastLabel,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars) =
             (sem _lhsIlabels _lhsIlines _lhsIoutLabel )
     in  (Syn_StatementList _lhsOfirstLabel _lhsOflow _lhsOlabels _lhsOlastLabel _lhsOlines _lhsOme _lhsOmeSmall _lhsOvars ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIlabels
       _lhsIlines
       _lhsIoutLabel ->
         (let _lhsOmeSmall :: StatementList 
              _lhsOlastLabel :: Int
              _lhsOfirstLabel :: Int
              _tlOoutLabel :: Int
              _lhsOflow :: ([(Int,Int)])
              _lhsOlabels :: (IM.IntMap Statement)
              _lhsOvars :: (S.Set String)
              _lhsOme :: StatementList 
              _lhsOlines :: Int
              _hdOlabels :: (IM.IntMap Statement)
              _hdOlines :: Int
              _hdOoutLabel :: Int
              _tlOlabels :: (IM.IntMap Statement)
              _tlOlines :: Int
              _hdIflow :: ([(Int,Int)])
              _hdIlabels :: (IM.IntMap Statement)
              _hdIline :: Int
              _hdIlines :: Int
              _hdIme :: Statement 
              _hdImeSmall :: Statement 
              _hdIvars :: (S.Set String)
              _tlIfirstLabel :: Int
              _tlIflow :: ([(Int,Int)])
              _tlIlabels :: (IM.IntMap Statement)
              _tlIlastLabel :: Int
              _tlIlines :: Int
              _tlIme :: StatementList 
              _tlImeSmall :: StatementList 
              _tlIvars :: (S.Set String)
              _lhsOmeSmall =
                  ({-# LINE 53 "Statement.ag" #-}
                   []
                   {-# LINE 1140 "Statement.hs" #-})
              _lhsOlastLabel =
                  ({-# LINE 77 "Statement.ag" #-}
                   if _tlIlastLabel == 0 then _hdIline else _tlIlastLabel
                   {-# LINE 1144 "Statement.hs" #-})
              _lhsOfirstLabel =
                  ({-# LINE 78 "Statement.ag" #-}
                   _hdIline
                   {-# LINE 1148 "Statement.hs" #-})
              _tlOoutLabel =
                  ({-# LINE 79 "Statement.ag" #-}
                   if _tlIfirstLabel == 0 then _lhsIoutLabel else _tlIfirstLabel
                   {-# LINE 1152 "Statement.hs" #-})
              _lhsOflow =
                  ({-# LINE 80 "Statement.ag" #-}
                   if _tlIfirstLabel == 0
                   then (_hdIline, _lhsIoutLabel) : _hdIflow
                   else ((_hdIline, _tlIfirstLabel) : _tlIflow) ++ _hdIflow
                   {-# LINE 1158 "Statement.hs" #-})
              _lhsOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _hdIlabels `IM.union` _tlIlabels
                   {-# LINE 1162 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   _hdIvars `S.union` _tlIvars
                   {-# LINE 1166 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   (:) _hdIme _tlIme
                   {-# LINE 1170 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   (:) _hdImeSmall _tlImeSmall
                   {-# LINE 1174 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 1178 "Statement.hs" #-})
              _lhsOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _tlIlines
                   {-# LINE 1182 "Statement.hs" #-})
              _hdOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _lhsIlabels
                   {-# LINE 1186 "Statement.hs" #-})
              _hdOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _lhsIlines
                   {-# LINE 1190 "Statement.hs" #-})
              _hdOoutLabel =
                  ({-# LINE 69 "Statement.ag" #-}
                   _lhsIoutLabel
                   {-# LINE 1194 "Statement.hs" #-})
              _tlOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   _hdIlabels
                   {-# LINE 1198 "Statement.hs" #-})
              _tlOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _hdIlines
                   {-# LINE 1202 "Statement.hs" #-})
              ( _hdIflow,_hdIlabels,_hdIline,_hdIlines,_hdIme,_hdImeSmall,_hdIvars) =
                  (hd_ _hdOlabels _hdOlines _hdOoutLabel )
              ( _tlIfirstLabel,_tlIflow,_tlIlabels,_tlIlastLabel,_tlIlines,_tlIme,_tlImeSmall,_tlIvars) =
                  (tl_ _tlOlabels _tlOlines _tlOoutLabel )
          in  ( _lhsOfirstLabel,_lhsOflow,_lhsOlabels,_lhsOlastLabel,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIlabels
       _lhsIlines
       _lhsIoutLabel ->
         (let _lhsOlastLabel :: Int
              _lhsOfirstLabel :: Int
              _lhsOflow :: ([(Int,Int)])
              _lhsOlabels :: (IM.IntMap Statement)
              _lhsOvars :: (S.Set String)
              _lhsOme :: StatementList 
              _lhsOmeSmall :: StatementList 
              _lhsOlines :: Int
              _lhsOlastLabel =
                  ({-# LINE 75 "Statement.ag" #-}
                   0
                   {-# LINE 1224 "Statement.hs" #-})
              _lhsOfirstLabel =
                  ({-# LINE 76 "Statement.ag" #-}
                   0
                   {-# LINE 1228 "Statement.hs" #-})
              _lhsOflow =
                  ({-# LINE 36 "Statement.ag" #-}
                   []
                   {-# LINE 1232 "Statement.hs" #-})
              _lhsOlabels =
                  ({-# LINE 32 "Statement.ag" #-}
                   IM.empty
                   {-# LINE 1236 "Statement.hs" #-})
              _lhsOvars =
                  ({-# LINE 33 "Statement.ag" #-}
                   S.empty
                   {-# LINE 1240 "Statement.hs" #-})
              _me =
                  ({-# LINE 34 "Statement.ag" #-}
                   []
                   {-# LINE 1244 "Statement.hs" #-})
              _meSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   []
                   {-# LINE 1248 "Statement.hs" #-})
              _lhsOme =
                  ({-# LINE 34 "Statement.ag" #-}
                   _me
                   {-# LINE 1252 "Statement.hs" #-})
              _lhsOmeSmall =
                  ({-# LINE 35 "Statement.ag" #-}
                   _meSmall
                   {-# LINE 1256 "Statement.hs" #-})
              _lhsOlines =
                  ({-# LINE 31 "Statement.ag" #-}
                   _lhsIlines
                   {-# LINE 1260 "Statement.hs" #-})
          in  ( _lhsOfirstLabel,_lhsOflow,_lhsOlabels,_lhsOlastLabel,_lhsOlines,_lhsOme,_lhsOmeSmall,_lhsOvars)))