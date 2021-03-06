{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module PHP.Parser.Ast (
  module PHP.Parser.Ast.Common,
  module PHP.Parser.Ast.Expr,
  module PHP.Parser.Ast.Stmt,
  Ast(..)
  ) where

import Common
import Control.Applicative hiding ((<|>), many)
import Control.Arrow
import Control.Monad
import Data.Char
import PHP.Parser.Ast.Common
import PHP.Parser.Ast.Expr
import PHP.Parser.Ast.Lex
import PHP.Parser.Ast.Stmt
import qualified Data.ByteString as BS
import qualified Data.Intercal as IC

data Ast = Ast TopLevel StmtList
  deriving (Eq, Show, Typeable, Data)

instance Unparse Ast where
  unparse (Ast t s) = unparse t ++ unparse s

instance Parse Ast where
  parse = liftM2 Ast parse stmtListParser


