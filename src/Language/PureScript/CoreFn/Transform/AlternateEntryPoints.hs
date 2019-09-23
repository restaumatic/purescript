{-# LANGUAGE LambdaCase #-}

module Language.PureScript.CoreFn.Transform.AlternateEntryPoints (generateAlternateEntryPoints, useAlternateEntryPoints) where

import Protolude hiding (Type, moduleName)

import qualified Data.Map as Map
import Data.List (lookup)
import Control.Monad.Writer
import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Declarations (DeclarationRef(..))
import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.Meta
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Traversals
import Language.PureScript.Names
import Language.PureScript.Label
import Language.PureScript.Types
import Language.PureScript.Externs
import qualified Language.PureScript.Constants as C

generateAlternateEntryPoints :: [ExternsFile] -> Module Ann -> Module Ann
generateAlternateEntryPoints externs m | not (hasDataFunctionUncurried externs) = m
generateAlternateEntryPoints externs m =
    let (decls, exports) = runWriter (traverse transformBind (moduleDecls m))
    in m {moduleDecls = mconcat decls, moduleExports = moduleExports m <> exports}
  where
    transformBind :: Bind Ann -> Writer [Ident] [Bind Ann]
    transformBind (NonRec ann ident expr)
      | (binders, expr') <- stripAbs expr
      , arity <- length binders
      , arity > 1
      , case meta (extractAnn expr) of { Just IsConstructor{} -> False; Just IsTypeClassConstructor{} -> False; _ -> True }
      =  do
        let
          altIdent = Ident (runIdent ident <> "__arity" <> show arity)
        when (ident `elem` moduleExports m) $ tell [altIdent]
        pure
          [ NonRec ann altIdent $ App noAnn (Var noAnn (mkFn arity)) expr
          , NonRec ann ident $ mkAbs binders $
              mkApp (Var noAnn (runFn arity)) $
                Var noAnn (Qualified Nothing altIdent) : map (Var noAnn . Qualified Nothing . snd) binders
          ]
    transformBind b = pure [b]

meta (_, _, _, m) = m

pattern DataFunctionUncurried :: ModuleName
pattern DataFunctionUncurried = ModuleName ([ProperName "Data", ProperName "Function", ProperName "Uncurried"])

hasDataFunctionUncurried = any (\e -> efModuleName e == DataFunctionUncurried)

mkFn :: Int -> Qualified Ident
mkFn n = Qualified (Just DataFunctionUncurried) (Ident ("mkFn" <> show n))

runFn :: Int -> Qualified Ident
runFn n = Qualified (Just DataFunctionUncurried) (Ident ("runFn" <> show n))

stripAbs :: Expr Ann -> ([(Ann, Ident)], Expr Ann)
stripAbs (Abs ann ident expr) =
  let (binders, expr') = stripAbs expr
  in ((ann, ident) : binders, expr')
stripAbs expr = ([], expr)

mkAbs :: [(Ann, Ident)] -> Expr Ann -> Expr Ann
mkAbs [] expr = expr
mkAbs ((ann, ident):xs) expr = Abs ann ident (mkAbs xs expr)

stripApp :: Expr Ann -> (Expr Ann, [Expr Ann])
stripApp (App _ f x) = let (f', args) = stripApp f in (f', args ++ [x]) -- FIXME: O(n^2)
stripApp expr = (expr, [])

mkApp :: Expr Ann -> [Expr Ann] -> Expr Ann
mkApp fn [] = fn
mkApp fn (x:xs) = mkApp (App noAnn fn x) xs

noAnn :: Ann
noAnn = ssAnn nullSourceSpan

useAlternateEntryPoints :: [ExternsFile] -> Module Ann -> Module Ann
useAlternateEntryPoints externs m | not (hasDataFunctionUncurried externs) = m
useAlternateEntryPoints externs m =
    m { moduleDecls = map transformBind $ moduleDecls m
      , moduleImports = moduleImports m <> [(noAnn, DataFunctionUncurried)]
      }
  where
  exportedValuesByModuleName =
    Map.fromList $
      (moduleName m, identsInThisModule) :
      map (\ef -> (efModuleName ef, mapMaybe fromValueExport (efExports ef))) externs
    where
      fromValueExport (ValueRef _ ident) = Just ident
      fromValueExport _ = Nothing

  identsInThisModule = foldMap extract (moduleDecls m)
    where
      extract (NonRec _ ident _) = [ident]
      extract (Rec binds) = map (\((_, ident), _) -> ident) binds

  lookupAlternateEntryPoint (Qualified modname ident) arity =
    let
      altIdent = Ident (runIdent ident <> "__arity" <> show arity)
      altName = Qualified modname altIdent
    in
      case modname >>= \mn -> Map.lookup mn exportedValuesByModuleName of
        Just names ->
          if altIdent `elem` names
            then Just altName
            else Nothing
        _ ->
          Nothing

  (transformBind, _, _) = everywhereOnValues identity transformExprs identity

  transformExprs expr
    | (Var _ name, args) <- stripApp expr
    , arity <- length args
    , arity > 1
    , Just altName <- lookupAlternateEntryPoint name arity
      = mkApp (Var noAnn (runFn arity)) $
          Var noAnn altName : args
  transformExprs expr = expr
