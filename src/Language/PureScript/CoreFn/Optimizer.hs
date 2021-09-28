module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude hiding (Type)

import Data.List (lookup)
import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Traversals
import Language.PureScript.Names (Ident(UnusedIdent), Qualified(Qualified))
import Language.PureScript.Label
import Language.PureScript.Types
import qualified Language.PureScript.Constants.Prim as C
import qualified Language.PureScript.Constants.Prelude as C

-- |
-- CoreFn optimization pass.
--
optimizeCoreFn :: Module Ann -> Module Ann
optimizeCoreFn m = m {moduleDecls = optimizeModuleDecls $ moduleDecls m}

optimizeModuleDecls :: [Bind Ann] -> [Bind Ann]
optimizeModuleDecls = map transformBinds
  where
  (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
  transformExprs = optimizeUnusedPartialFn . optimizeClosedRecordUpdate . optimizeRecordGetField

optimizeClosedRecordUpdate :: Expr Ann -> Expr Ann
optimizeClosedRecordUpdate ou@(ObjectUpdate a@(_, _, Just t, _) r updatedFields) =
  case closedRecordFields t of
    Nothing -> ou
    Just allFields -> Literal a (ObjectLiteral (map f allFields))
      where f (Label l) = case lookup l updatedFields of
              Nothing -> (l, Accessor (nullSourceSpan, [], Nothing, Nothing) l r)
              Just e -> (l, e)
optimizeClosedRecordUpdate e = e

-- | Return the labels of a closed record, or Nothing for other types or open records.
closedRecordFields :: Type a -> Maybe [Label]
closedRecordFields (TypeApp _ (TypeConstructor _ C.Record) row) =
  collect row
  where
    collect :: Type a -> Maybe [Label]
    collect (REmptyKinded _ _) = Just []
    collect (RCons _ l _ r) = (l :) <$> collect r
    collect _ = Nothing
closedRecordFields _ = Nothing

-- | See https://github.com/purescript/purescript/issues/3157
optimizeUnusedPartialFn :: Expr a -> Expr a
optimizeUnusedPartialFn (Let _
  [NonRec _ UnusedIdent _]
  (App _ (App _ (Var _ (Qualified _ UnusedIdent)) _) originalCoreFn)) =
  originalCoreFn
optimizeUnusedPartialFn e = e

-- | Optimize
-- `Data_Record.getField(Data_Record.hasFieldRecord(new Data_Symbol.IsSymbol(function() { return "f"; }))())(Data_Symbol.SProxy.value)(x)`
-- into
-- `x.f`
optimizeRecordGetField :: Expr a -> Expr a
optimizeRecordGetField
  (App ann
    (App _
      (App _
        (Var _ C.DataRecord_getField)
        (App _
          (App _
            (Var _ C.DataRecord_hasFieldRecord)
            (App _
              (Var _ C.IsSymbolIdent)
              (Abs _ _
                (Literal _ (StringLiteral label)))))
          _))
      (Var _ C.SProxyIdent))
    object) =
  Accessor ann label object
optimizeRecordGetField e = e
