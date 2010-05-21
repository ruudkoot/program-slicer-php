{-# LANGUAGE GADTs #-}

module Ruud.AvailableExpressions where

import Prelude hiding (init)
import Data.Set

-- While language

type    Lab   = Integer

type    Var   =     String
type    Num_  =     Integer



data Stmt where
    Assign     :: { x_assign      :: Var
                  , a_assign      :: Exp
                  , l_assign      :: Lab  } -> Stmt
    Skip       :: { l_skip        :: Lab  } -> Stmt
    Sequence   :: { s1_seq        :: Stmt
                  , s2_seq        :: Stmt } -> Stmt
    IfThenElse :: { g_ifThenElse  :: Exp
                  , l_ifThenElse  :: Lab
                  , s1_ifThenElse :: Stmt
                  , s2_ifThenElse :: Stmt } -> Stmt
    WhileDo    :: { g_whileDo     :: Exp
                  , l_whileDo     :: Lab
                  , s_whileDo     :: Stmt } -> Stmt
    deriving (Eq, Ord)

data Exp where
    Var   :: Var              -> Exp
    Num   :: Num_             -> Exp
    True  ::                     Exp
    False ::                     Exp
    Not   :: Exp              -> Exp
    Op    :: Exp -> Op -> Exp -> Exp
    deriving (Eq, Ord)

data Op where
    (:+:)  :: Op
    (:*:)  :: Op
    (:&&:) :: Op
    (:<:)  :: Op
    (:<=:) :: Op
    (:>:)  :: Op
    deriving (Eq, Ord)

-- Standard dataflow functions

labels :: Stmt -> Set Lab
labels (Assign     _ _ l      ) = singleton l
labels (Skip           l      ) = singleton l
labels (Sequence         s1 s2) = labels s1 `union` labels s2
labels (IfThenElse _   l _  _ ) = singleton l
labels (WhileDo    _   l _    ) = singleton l

init :: Stmt -> Lab
init (Assign     _ _ l     ) = l
init (Skip           l     ) = l
init (Sequence         s1 _) = init s1
init (IfThenElse _   l _  _) = l
init (WhileDo    _   l _   ) = l

final :: Stmt -> Set Lab
final (Assign     _ _ l      ) = singleton l
final (Skip           l      ) = singleton l
final (Sequence         s1 s2) = final s2
final (IfThenElse g   _ _  _ ) = final g
final (WhileDo    g   _ _    ) = final g

blocks :: Stmt -> Set Block
blocks s@(Assign     _ _ _      ) = singleton s
blocks s@(Skip           _      ) = singleton s
blocks s@(Sequence         s1 s2) = blocks s1 `union` blocks s2
blocks s@(IfThenElse g   _ s1 s2) = singleton g `union` blocks s1 `union` blocks s2
blocks s@(WhileDo    g   _ s    ) = singleton g `union` blocks s

flow :: Stmt -> Set (Lab, Lab) -- Stmt -> Graph
flow (Assign     _ _ l     ) = l
flow (Skip           l     ) = l
flow (Sequence         s1 _) = init s1
flow (IfThenElse _   l _  _) = l
flow (WhileDo    _   l _   ) = l

-- Test program

test = (Var "v" `Assign` (Num 1) 3) `Sequence` (Var "u" `Assign` (Num 1) 2) `Sequence`
       (IfThenElse ((Var "n")  `Op` (:<=:) (Num 2)) 1
            (Skip 4) 
            (WhileDo ((Var "n") `Op` (:>:) (Num 2))
                ((Var "t") `Assign` (Var "u")) `Sequence`
                ((Var "u") `Assign` (Var "v")) `Sequence`
                ((Var "v") `Assign` ((Var "u") `Op` (:+:) (Var "t")))
            )
       )

