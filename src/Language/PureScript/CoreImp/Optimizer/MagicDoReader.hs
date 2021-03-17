-- | This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
module Language.PureScript.CoreImp.Optimizer.MagicDoReader (magicDoSpecularBuilder) where

import Prelude.Compat
import Protolude (ordNub)

import Data.Maybe (fromJust, isJust)
import Data.Text (Text)

import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Common
import Language.PureScript.PSString (mkString)
import qualified Language.PureScript.Constants as C

magicDoSpecularBuilder :: AST -> AST
magicDoSpecularBuilder = magicDoReader "Specular_Dom_Builder" specularBuilderDictionaries

specularBuilderDictionaries :: C.EffectDictionaries
specularBuilderDictionaries = C.EffectDictionaries
  { C.edApplicativeDict = "applicativeBuilder"
  , C.edBindDict = "bindBuilder"
  , C.edMonadDict = "monadBuilder"
  , C.edWhile = ""
  , C.edUntil = ""
  }

magicDoReader :: Text -> C.EffectDictionaries -> AST -> AST
magicDoReader effectModule C.EffectDictionaries{..} = everywhereTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__rdo"
  -- The name of the function parameter which carries the reader environment
  envName = "$$env"
  -- Desugar monomorphic calls to >>= and return
  convert :: AST -> AST
  -- Desugar pure
  convert (App _ (App _ pure' [val]) [_]) | isPure pure' = val
  -- Desugar discard
  convert (App _ (App _ bind [m]) [Function s1 Nothing [] (Block s2 js)]) | isDiscard bind =
    Function s1 (Just fnName) [envName] $ Block s2 (App s2 m [Var s2 envName] : map applyReturns js )
  -- Desugar bind to wildcard
  convert (App _ (App _ bind [m]) [Function s1 Nothing [] (Block s2 js)])
    | isBind bind =
    Function s1 (Just fnName) [envName] $ Block s2 (App s2 m [Var s2 envName] : map applyReturns js )
  -- Desugar bind
  convert (App _ (App _ bind [m]) [Function s1 Nothing [arg] (Block s2 js)]) | isBind bind =
    Function s1 (Just fnName) [envName] $ Block s2 (VariableIntroduction s2 arg (Just (App s2 m [Var s2 envName])) : map applyReturns js)
  -- Inline __do returns
  convert (Return _ (App _ (Function _ (Just ident) [env1] body) [Var _ env2])) | ident == fnName, env1 == envName, env1 == env2 = body
  -- Inline double applications
  -- TODO: verify if this is needed
  convert (App _ (App s1 (Function s2 Nothing [] (Block ss body)) []) []) =
    App s1 (Function s2 Nothing [] (Block ss (applyReturns `fmap` body))) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (App _ fn [dict]) | isDict (effectModule, edBindDict) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a call to @discard@
  isDiscard (App _ (App _ fn [dict1]) [dict2])
    | isDict (C.controlBind, C.discardUnitDictionary) dict1 &&
      isDict (effectModule, edBindDict) dict2 &&
      isDiscardPoly fn = True
  isDiscard _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (App _ fn [dict]) | isDict (effectModule, edApplicativeDict) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isDict (C.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure function
  isPurePoly = isDict (C.controlApplicative, C.pure')
  -- Check if an expression represents the polymorphic discard function
  isDiscardPoly = isDict (C.controlBind, C.discard)
  -- Check if an expression represents a function in the Effect module
  isEffFunc name (Indexer _ (StringLiteral _ name') (Var _ eff)) = eff == effectModule && name == name'
  isEffFunc _ _ = False

  applyReturns :: AST -> AST
  applyReturns (Return ss ret) = Return ss (App ss ret [Var ss envName])
  applyReturns (Block ss jss) = Block ss (map applyReturns jss)
  applyReturns (While ss cond js) = While ss cond (applyReturns js)
  applyReturns (For ss v lo hi js) = For ss v lo hi (applyReturns js)
  applyReturns (ForIn ss v xs js) = ForIn ss v xs (applyReturns js)
  applyReturns (IfElse ss cond t f) = IfElse ss cond (applyReturns t) (applyReturns `fmap` f)
  applyReturns other = other
