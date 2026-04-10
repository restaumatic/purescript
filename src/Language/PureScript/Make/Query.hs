{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.PureScript.Make.Query
  ( Query(..)
  ) where

import Prelude

import Data.Constraint.Extras.TH (deriveArgDict)
import Data.GADT.Compare (GEq(..), GCompare(..), GOrdering(..))
import Data.GADT.Show (GShow(..))
import Data.Hashable (Hashable(..))
import Data.Map qualified as M
import Data.Some (Some(..))
import Data.Type.Equality ((:~:)(..))

import Language.PureScript.AST (Module)
import Language.PureScript.Environment (Environment)
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Names (ModuleName(..))
import Language.PureScript.Sugar.Names.Env (Env)

-- | Queries for the rock-based incremental compilation pipeline.
--
-- Each constructor represents a computation that can depend on other queries
-- via @fetch@. Rock automatically tracks these dependencies for memoization
-- and incremental recomputation.
data Query a where
  -- | Input query: the pre-parsed module provided by the caller.
  -- In rock's verifyTraces, this would be marked as 'Input' (can change between builds).
  InputModule :: ModuleName -> Query Module

  -- | Dependency graph: maps each module to its (transitively) sorted dependencies.
  ModuleGraph :: Query (M.Map ModuleName [ModuleName])

  -- | Sorted module names in topological order (leaves first).
  SortedModules :: Query [ModuleName]

  -- | Build the sugar names Env for a module from its dependencies' externs.
  ModuleSugarEnv :: ModuleName -> Query Env

  -- | Build the typechecker Environment from dependency externs.
  ModuleTypeEnv :: ModuleName -> Query Environment

  -- | Full per-module compilation: desugar, typecheck, corefn, codegen.
  -- Returns the module's ExternsFile.
  CompileModule :: ModuleName -> Query ExternsFile

deriving instance Show (Query a)

instance Eq (Query a) where
  InputModule a == InputModule b = a == b
  ModuleGraph == ModuleGraph = True
  SortedModules == SortedModules = True
  ModuleSugarEnv a == ModuleSugarEnv b = a == b
  ModuleTypeEnv a == ModuleTypeEnv b = a == b
  CompileModule a == CompileModule b = a == b

instance GShow Query where
  gshowsPrec = showsPrec

-- | GEq instance: structural equality on the query key, returning type-level
-- proof (Refl) when two queries are identical.
instance GEq Query where
  geq (InputModule a) (InputModule b)
    | a == b = Just Refl
  geq (ModuleGraph) (ModuleGraph) = Just Refl
  geq (SortedModules) (SortedModules) = Just Refl
  geq (ModuleSugarEnv a) (ModuleSugarEnv b)
    | a == b = Just Refl
  geq (ModuleTypeEnv a) (ModuleTypeEnv b)
    | a == b = Just Refl
  geq (CompileModule a) (CompileModule b)
    | a == b = Just Refl
  geq _ _ = Nothing

-- | GCompare instance required by some rock operations.
instance GCompare Query where
  gcompare (InputModule a) (InputModule b) = case compare a b of
    EQ -> GEQ; LT -> GLT; GT -> GGT
  gcompare (InputModule _) _ = GLT
  gcompare _ (InputModule _) = GGT

  gcompare ModuleGraph ModuleGraph = GEQ
  gcompare ModuleGraph _ = GLT
  gcompare _ ModuleGraph = GGT

  gcompare SortedModules SortedModules = GEQ
  gcompare SortedModules _ = GLT
  gcompare _ SortedModules = GGT

  gcompare (ModuleSugarEnv a) (ModuleSugarEnv b) = case compare a b of
    EQ -> GEQ; LT -> GLT; GT -> GGT
  gcompare (ModuleSugarEnv _) _ = GLT
  gcompare _ (ModuleSugarEnv _) = GGT

  gcompare (ModuleTypeEnv a) (ModuleTypeEnv b) = case compare a b of
    EQ -> GEQ; LT -> GLT; GT -> GGT
  gcompare (ModuleTypeEnv _) _ = GLT
  gcompare _ (ModuleTypeEnv _) = GGT

  gcompare (CompileModule a) (CompileModule b) = case compare a b of
    EQ -> GEQ; LT -> GLT; GT -> GGT

-- | Hashable instance for individual queries.
instance Hashable (Query a) where
  hashWithSalt salt = \case
    InputModule mn    -> hashWithSalt salt (0 :: Int, mn)
    ModuleGraph       -> hashWithSalt salt (1 :: Int)
    SortedModules     -> hashWithSalt salt (2 :: Int)
    ModuleSugarEnv mn -> hashWithSalt salt (3 :: Int, mn)
    ModuleTypeEnv mn  -> hashWithSalt salt (4 :: Int, mn)
    CompileModule mn  -> hashWithSalt salt (5 :: Int, mn)

-- | Hashable for existentially-wrapped queries (required by rock's memoise).
instance Hashable (Some Query) where
  hashWithSalt salt (Some q) = hashWithSalt salt q

-- | ArgDict derivation for constraints-extras (needed for verifyTraces).
deriveArgDict ''Query
