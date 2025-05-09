module Language.PureScript.CoreFn.Module where

import Prelude

import Data.HashMap.Strict (HashMap)

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Expr (Bind)
import Language.PureScript.Names (Ident, ModuleName)

-- |
-- The CoreFn module representation
--
data Module a = Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments :: [Comment]
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: [(a, ModuleName)]
  , moduleExports :: [Ident]
  , moduleReExports :: HashMap ModuleName [Ident]
  , moduleForeign :: [Ident]
  , moduleDecls :: [Bind a]
  } deriving (Functor, Show)
