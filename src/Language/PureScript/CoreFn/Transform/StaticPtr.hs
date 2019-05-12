{-# LANGUAGE PatternSynonyms #-}

module Language.PureScript.CoreFn.Transform.StaticPtr (transformStaticPtrs) where

import Protolude hiding (Type, moduleName)

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Traversals
import Language.PureScript.Names (Ident(Ident), Qualified(Qualified), ModuleName(..), ProperName(..), runModuleName)
import Language.PureScript.PSString
import qualified Language.PureScript.Constants as C

transformStaticPtrs :: Module Ann -> Module Ann
transformStaticPtrs m =
    m { moduleDecls = transformedDecls <> addedDecls
      , moduleExports = moduleExports m <> addedExports
      }
  where
  (transformedDecls, S{addedDecls=addedDecls, addedExports=addedExports}) =
    flip runState initialState $ traverse transformBind $ moduleDecls m
  (transformBind, _, _) = everywhereOnValuesM pure (transformExpr (moduleName m)) pure -- FIXME: top down?

data S = S
  { addedDecls :: [Bind Ann]
  , addedExports :: [Ident]
  , counter :: Int
  }

initialState :: S
initialState = S { addedDecls = [], addedExports = [], counter = 0 }

pattern Static :: Qualified Ident
pattern Static = Qualified (Just (ModuleName [ProperName "StaticPtr"])) (Ident "static")

transformExpr :: ModuleName -> Expr Ann -> State S (Expr Ann)
transformExpr moduleName = \case
  App ann (Var _ Static) expr -> do
    n <- gets counter
    let ident = "_static_" <> show n
    let key = runModuleName moduleName <> "." <> ident
    let decl = NonRec ann (Ident ident) expr
    modify $ \s ->
      s { counter = n + 1
        , addedDecls = decl : addedDecls s
        , addedExports = Ident ident : addedExports s
        }
    pure (Literal ann (StringLiteral (mkString key)))

  expr -> pure expr
