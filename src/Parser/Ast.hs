{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Parser.Ast (
  module Parser.Ast.Common,
  module Parser.Ast.Expr,
  module Parser.Ast.Stmt,
  Ast
  ) where

import Common
import Control.Applicative hiding ((<|>), many)
import Control.Arrow
import Control.Monad
import Data.Char
import Parser.Ast.Common
import Parser.Ast.Expr
import Parser.Ast.Lex
import Parser.Ast.Stmt
import qualified Data.ByteString as BS
import qualified Data.Intercal as IC

data Ast = Ast TopLevel StmtList
  deriving (Eq, Show, Typeable, Data)

instance Unparse Ast where
  unparse (Ast t s) = unparse t ++ unparse s

instance Parse Ast where
  parse = liftM2 Ast parse stmtListParser

$(derive makeBinary ''Ast)

