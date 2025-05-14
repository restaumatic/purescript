{-# LANGUAGE GADTs #-}

-- |
-- Monads for type checking and type inference and associated data types
--
module Language.PureScript.TypeChecker.Monad where

import Prelude

import Control.Arrow (second)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State (MonadState(..), StateT(..), gets, modify)
import Control.Monad.State.Strict qualified as StrictState

import Data.Maybe (fromMaybe)
import Data.IntMap.Lazy qualified as IM
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text, isPrefixOf, unpack)
import Data.List.NonEmpty qualified as NEL

import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (Environment(..), NameKind(..), NameVisibility(..), TypeClassData(..), TypeKind(..))
import Language.PureScript.Errors (Context, ErrorMessageHint, ExportSource, Expr, ImportDeclarationType, MultipleErrors, SimpleErrorMessage(..), SourceAnn, SourceSpan(..), addHint, errorMessage, positionedError, rethrow, warnWithPosition)
import Language.PureScript.Names (Ident(..), ModuleName, ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), coerceProperName, disqualify, runIdent, runModuleName, showQualified, toMaybeModuleName, runProperName, properNameFromString, mkQualified_)
import Language.PureScript.Pretty.Types (prettyPrintType)
import Language.PureScript.Pretty.Values (prettyPrintValue)
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope(..))
import Language.PureScript.Types (Constraint(..), SourceType, Type(..), srcKindedType, srcTypeVar)
import Text.PrettyPrint.Boxes (render)
import Control.Monad.Supply (SupplyT (unSupplyT))
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Writer.CPS qualified as SW
import Control.Monad.Writer (MonadWriter(..), censor)
import Control.Monad.Supply.Class qualified as Supply
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad (forM_, when, join, (<=<), guard)

newtype TypeCheckM a = TypeCheckM { unTypeCheckM :: StateT CheckState (SupplyT (ExceptT MultipleErrors (SW.Writer MultipleErrors))) a }
  deriving newtype (Functor, Applicative, Monad, MonadSupply, MonadState CheckState, MonadWriter MultipleErrors, MonadError MultipleErrors)

-- | Lift a TypeCheckM computation into another monad that satisfies all its constraints
liftTypeCheckM ::
  (MonadSupply m, MonadError MultipleErrors m, MonadState CheckState m, MonadWriter MultipleErrors m) =>
  TypeCheckM a -> m a
liftTypeCheckM (TypeCheckM m) = do
  st <- get
  freshId <- Supply.peek
  let (result, errors) = runIdentity $ SW.runWriterT $ runExceptT $ flip StrictState.runStateT freshId $ unSupplyT $ runStateT m st
  tell errors
  case result of
    Left err ->
      throwError err
    Right ((a, st'), freshId') -> do
      put st'
      Supply.consumeUpTo freshId'
      return a

newtype UnkLevel = UnkLevel (NEL.NonEmpty Unknown)
  deriving (Eq, Show)

-- This instance differs from the NEL instance in that longer but otherwise
-- equal paths are LT rather than GT. An extended path puts it *before* its root.
instance Ord UnkLevel where
  compare (UnkLevel a) (UnkLevel b) =
    go (NEL.toList a) (NEL.toList b)
    where
    go [] [] = EQ
    go _  [] = LT
    go [] _  = GT
    go (x:xs) (y:ys) =
      compare x y <> go xs ys

-- | A substitution of unification variables for types.
data Substitution = Substitution
  { substType :: IM.IntMap SourceType
  -- ^ Type substitution
  , substUnsolved :: IM.IntMap (UnkLevel, SourceType)
  -- ^ Unsolved unification variables with their level (scope ordering) and kind
  , substNames :: IM.IntMap Text
  -- ^ The original names of unknowns
  }

insertUnkName :: (MonadState CheckState m) => Unknown -> Text -> m ()
insertUnkName u t = do
  modify (\s ->
            s { checkSubstitution =
                  (checkSubstitution s) { substNames =
                                            IM.insert u t $ substNames $ checkSubstitution s
                                        }
              }
         )

lookupUnkName :: (MonadState CheckState m) => Unknown -> m (Maybe Text)
lookupUnkName u = gets $ IM.lookup u . substNames . checkSubstitution

-- | An empty substitution
emptySubstitution :: Substitution
emptySubstitution = Substitution IM.empty IM.empty IM.empty

-- | State required for type checking
data CheckState = CheckState
  { checkEnv :: Environment
  -- ^ The current @Environment@
  , checkNextType :: Int
  -- ^ The next type unification variable
  , checkNextSkolem :: Int
  -- ^ The next skolem variable
  , checkNextSkolemScope :: Int
  -- ^ The next skolem scope constant
  , checkCurrentModule :: Maybe ModuleName
  -- ^ The current module
  , checkCurrentModuleImports ::
      [ ( SourceAnn
        , ModuleName
        , ImportDeclarationType
        , Maybe ModuleName
        , M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ExportSource)
        )
      ]
  -- ^ The current module imports and their exported types.
  -- Newtype constructors have to be in scope for some Coercible constraints to
  -- be solvable, so we need to know which constructors are imported and whether
  -- they are actually defined in or re-exported from the imported modules.
  , checkSubstitution :: Substitution
  -- ^ The current substitution
  , checkHints :: [ErrorMessageHint]
  -- ^ The current error message hint stack.
  -- This goes into state, rather than using 'rethrow',
  -- since this way, we can provide good error messages
  -- during instance resolution.
  , checkConstructorImportsForCoercible :: S.Set (ModuleName, Qualified (ProperName 'ConstructorName))
  -- ^ Newtype constructors imports required to solve Coercible constraints.
  -- We have to keep track of them so that we don't emit unused import warnings.
  , unificationCache :: HS.HashSet (SourceType, SourceType)
  }

-- | Create an empty @CheckState@
emptyCheckState :: Environment -> CheckState
emptyCheckState env = CheckState env 0 0 0 Nothing [] emptySubstitution [] mempty HS.empty

-- | Unification variables
type Unknown = Int

-- | Temporarily bind a collection of names to values
bindNames
  :: M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
  -> TypeCheckM a
  -> TypeCheckM a
bindNames newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { names = newNames `M.union` (names . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { names = names . checkEnv $ orig } }
  return a

-- | Temporarily bind a collection of names to types
bindTypes
  :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
  -> TypeCheckM a
  -> TypeCheckM a
bindTypes newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { types = newNames `M.union` (types . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { types = types . checkEnv $ orig } }
  return a

-- | Temporarily bind a collection of names to types
withScopedTypeVars
  :: ModuleName
  -> [(Text, SourceType)]
  -> TypeCheckM a
  -> TypeCheckM a
withScopedTypeVars mn ks ma = do
  orig <- get
  forM_ ks $ \(name, _) ->
    when (mkQualified_ (ByModuleName mn) (properNameFromString name) `M.member` types (checkEnv orig)) $
      tell . errorMessage $ ShadowedTypeVar name
  bindTypes (M.fromList (map (\(name, k) -> (mkQualified_ (ByModuleName mn) (properNameFromString name), (k, ScopedTypeVar))) ks)) ma

withErrorMessageHint
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => ErrorMessageHint
  -> m a
  -> m a
withErrorMessageHint hint action = do
  orig <- get
  modify $ \st -> st { checkHints = hint : checkHints st }
  -- Need to use 'rethrow' anyway, since we have to handle regular errors
  a <- rethrow (addHint hint) action
  modify $ \st -> st { checkHints = checkHints orig }
  return a

-- | These hints are added at the front, so the most nested hint occurs
-- at the front, but the simplifier assumes the reverse order.
getHints :: TypeCheckM [ErrorMessageHint]
getHints = gets (reverse . checkHints)

rethrowWithPositionTC
  :: SourceSpan
  -> TypeCheckM a
  -> TypeCheckM a
rethrowWithPositionTC pos = withErrorMessageHint (positionedError pos)

warnAndRethrowWithPositionTC
  :: SourceSpan
  -> TypeCheckM a
  -> TypeCheckM a
warnAndRethrowWithPositionTC pos = rethrowWithPositionTC pos . warnWithPosition pos

-- | Temporarily make a collection of type class dictionaries available
withTypeClassDictionaries
  :: [NamedDict]
  -> TypeCheckM a
  -> TypeCheckM a
withTypeClassDictionaries entries action = do
  orig <- get

  let mentries =
        HM.fromListWith (HM.unionWith (HM.unionWith (<>)))
          [ (qb, HM.singleton className (HM.singleton tcdValue (pure entry)))
          | entry@TypeClassDictionaryInScope{ tcdValue = tcdValue@(Qualified qb _ _), tcdClassName = className }
              <- entries
          ]

  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = HM.unionWith (HM.unionWith (HM.unionWith (<>))) (typeClassDictionaries . checkEnv $ st) mentries } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = typeClassDictionaries . checkEnv $ orig } }
  return a

-- | Get the currently available map of type class dictionaries
getTypeClassDictionaries
  :: TypeCheckM (HM.HashMap QualifiedBy (HM.HashMap (Qualified (ProperName 'ClassName)) (HM.HashMap (Qualified Ident) (NEL.NonEmpty NamedDict))))
getTypeClassDictionaries = gets $ typeClassDictionaries . checkEnv

-- | Lookup type class dictionaries in a module.
lookupTypeClassDictionaries
  :: QualifiedBy
  -> TypeCheckM (HM.HashMap (Qualified (ProperName 'ClassName)) (HM.HashMap (Qualified Ident) (NEL.NonEmpty NamedDict)))
lookupTypeClassDictionaries mn = gets $ fromMaybe HM.empty . HM.lookup mn . typeClassDictionaries . checkEnv

-- | Lookup type class dictionaries in a module.
lookupTypeClassDictionariesForClass
  :: QualifiedBy
  -> Qualified (ProperName 'ClassName)
  -> TypeCheckM (HM.HashMap (Qualified Ident) (NEL.NonEmpty NamedDict))
lookupTypeClassDictionariesForClass mn cn = fromMaybe HM.empty . HM.lookup cn <$> lookupTypeClassDictionaries mn

-- | Temporarily bind a collection of names to local variables
bindLocalVariables
  :: [(SourceSpan, Ident, SourceType, NameVisibility)]
  -> TypeCheckM a
  -> TypeCheckM a
bindLocalVariables bindings =
  bindNames (M.fromList $ flip map bindings $ \(ss, name, ty, visibility) -> (mkQualified_ (BySourcePos $ spanStart ss) name, (ty, Private, visibility)))

-- | Temporarily bind a collection of names to local type variables
bindLocalTypeVariables
  :: ModuleName
  -> [(ProperName 'TypeName, SourceType)]
  -> TypeCheckM a
  -> TypeCheckM a
bindLocalTypeVariables moduleName bindings =
  bindTypes (M.fromList $ flip map bindings $ \(pn, kind) -> (mkQualified_ (ByModuleName moduleName) pn, (kind, LocalTypeVariable)))

-- | Update the visibility of all names to Defined
makeBindingGroupVisible :: TypeCheckM ()
makeBindingGroupVisible = modifyEnv $ \e -> e { names = M.map (\(ty, nk, _) -> (ty, nk, Defined)) (names e) }

-- | Update the visibility of all names to Defined in the scope of the provided action
withBindingGroupVisible :: TypeCheckM a -> TypeCheckM a
withBindingGroupVisible action = preservingNames $ makeBindingGroupVisible >> action

-- | Perform an action while preserving the names from the @Environment@.
preservingNames :: TypeCheckM a -> TypeCheckM a
preservingNames action = do
  orig <- gets (names . checkEnv)
  a <- action
  modifyEnv $ \e -> e { names = orig }
  return a

-- | Lookup the type of a value by name in the @Environment@
lookupVariable
  :: Qualified Ident
  -> TypeCheckM SourceType
lookupVariable qual = do
  env <- getEnv
  case M.lookup qual (names env) of
    Nothing -> throwError . errorMessage $ NameIsUndefined (disqualify qual)
    Just (ty, _, _) -> return ty

-- | Lookup the visibility of a value by name in the @Environment@
getVisibility
  :: Qualified Ident
  -> TypeCheckM NameVisibility
getVisibility qual = do
  env <- getEnv
  case M.lookup qual (names env) of
    Nothing -> throwError . errorMessage $ NameIsUndefined (disqualify qual)
    Just (_, _, vis) -> return vis

-- | Assert that a name is visible
checkVisibility
  :: Qualified Ident
  -> TypeCheckM ()
checkVisibility name@(Qualified _ var _) = do
  vis <- getVisibility name
  case vis of
    Undefined -> throwError . errorMessage $ CycleInDeclaration var
    _ -> return ()

-- | Lookup the kind of a type by name in the @Environment@
lookupTypeVariable
  :: ModuleName
  -> Qualified (ProperName 'TypeName)
  -> TypeCheckM SourceType
lookupTypeVariable currentModule (Qualified qb name _) = do
  env <- getEnv
  case M.lookup (mkQualified_ qb' name) (types env) of
    Nothing -> throwError . errorMessage $ UndefinedTypeVariable name
    Just (k, _) -> return k
  where
  qb' = ByModuleName $ case qb of
    ByModuleName m -> m
    BySourcePos _ -> currentModule

-- | Get the current @Environment@
getEnv :: TypeCheckM Environment
getEnv = gets checkEnv

-- | Get locally-bound names in context, to create an error message.
getLocalContext :: TypeCheckM Context
getLocalContext = do
  env <- getEnv
  return [ (ident, ty') | (Qualified (BySourcePos _) ident@Ident{} _, (ty', _, Defined)) <- M.toList (names env) ]

-- | Update the @Environment@
putEnv :: Environment -> TypeCheckM ()
putEnv env = modify (\s -> s { checkEnv = env })

-- | Modify the @Environment@
modifyEnv :: (Environment -> Environment) -> TypeCheckM ()
modifyEnv f = modify (\s -> s { checkEnv = f (checkEnv s) })

-- | Run a computation in the typechecking monad, failing with an error, or succeeding with a return value and the final @Environment@.
runCheck :: Functor m => CheckState -> StateT CheckState m a -> m (a, Environment)
runCheck st check = second checkEnv <$> runStateT check st

-- | Make an assertion, failing with an error message
guardWith :: MonadError MultipleErrors m => MultipleErrors -> Bool -> m ()
guardWith _ True = return ()
guardWith e False = throwError e

capturingSubstitution
  :: (a -> Substitution -> b)
  -> TypeCheckM a
  -> TypeCheckM b
capturingSubstitution f ma = do
  a <- ma
  subst <- gets checkSubstitution
  return (f a subst)

withFreshSubstitution
  :: TypeCheckM a
  -> TypeCheckM a
withFreshSubstitution ma = do
  orig <- get
  modify $ \st -> st { checkSubstitution = emptySubstitution }
  a <- ma
  modify $ \st -> st { checkSubstitution = checkSubstitution orig }
  return a

withoutWarnings
  :: TypeCheckM a
  -> TypeCheckM (a, MultipleErrors)
withoutWarnings = censor (const mempty) . listen

unsafeCheckCurrentModule
  :: forall m
   . (MonadError MultipleErrors m, MonadState CheckState m)
  => m ModuleName
unsafeCheckCurrentModule = gets checkCurrentModule >>= \case
  Nothing -> internalError "No module name set in scope"
  Just name -> pure name

debugEnv :: Environment -> [String]
debugEnv env = join
  [ debugTypes env
  , debugTypeSynonyms env
  , debugTypeClasses env
  , debugTypeClassDictionaries env
  , debugDataConstructors env
  , debugNames env
  ]

debugType :: Type a -> String
debugType = init . prettyPrintType 100

debugConstraint :: Constraint a -> String
debugConstraint (Constraint ann clsName kinds args _) =
  debugType $ foldl (TypeApp ann) (foldl (KindApp ann) (TypeConstructor ann (fmap coerceProperName clsName)) kinds) args

debugTypes :: Environment -> [String]
debugTypes = go <=< M.toList . types
  where
  go (qual, (srcTy, which)) = do
    let
      ppTy = prettyPrintType 100 srcTy
      name = showQualified runProperName qual
      decl = case which of
        DataType _ _ _    -> "data"
        TypeSynonym       -> "type"
        ExternData _      -> "extern"
        LocalTypeVariable -> "local"
        ScopedTypeVar     -> "scoped"
    guard (not ("Prim" `isPrefixOf` name))
    pure $ decl <> " " <> unpack name <> " :: " <> init ppTy

debugNames :: Environment -> [String]
debugNames = fmap go . M.toList . names
  where
  go (qual, (srcTy, _, _)) = do
    let
      ppTy = prettyPrintType 100 srcTy
      name = showQualified runIdent qual
    unpack name <> " :: " <> init ppTy

debugDataConstructors :: Environment -> [String]
debugDataConstructors = fmap go . M.toList . dataConstructors
  where
  go (qual, (_, _, ty, _)) = do
    let
      ppTy = prettyPrintType 100 ty
      name = showQualified runProperName qual
    unpack name <> " :: " <> init ppTy

debugTypeSynonyms :: Environment -> [String]
debugTypeSynonyms = fmap go . M.toList . typeSynonyms
  where
  go (qual, (binders, subTy)) = do
    let
      vars = unwords $ flip fmap binders $ \case
               (v, Just k) -> "(" <> unpack v <> " :: " <> init (prettyPrintType 100 k) <> ")"
               (v, Nothing) -> unpack v
      ppTy = prettyPrintType 100 subTy
      name = showQualified runProperName qual
    "type " <> unpack name <> " " <> vars <> " = " <> init ppTy

debugTypeClassDictionaries :: Environment -> [String]
debugTypeClassDictionaries = go . typeClassDictionaries
  where
  -- TODO: order? 
  go tcds = do
    (mbModuleName, classes) <- HM.toList tcds
    (className, instances) <- HM.toList classes
    (ident, dicts) <- HM.toList instances
    let
      moduleName = maybe "" (\m -> "[" <> runModuleName m <> "] ") (toMaybeModuleName mbModuleName)
      className' = showQualified runProperName className
      ident' = showQualified runIdent ident
      kds = unwords $ fmap ((\a -> "@(" <> a <> ")") . debugType) $ tcdInstanceKinds $ NEL.head dicts
      tys = unwords $ fmap ((\a -> "(" <> a <> ")") . debugType) $ tcdInstanceTypes $ NEL.head dicts
    pure $ "dict " <> unpack moduleName <> unpack className' <> " " <> unpack ident' <> " (" <> show (length dicts) <> ")" <> " " <> kds <> " " <> tys

debugTypeClasses :: Environment -> [String]
debugTypeClasses = fmap go . M.toList . typeClasses
  where
  go (className, tc) = do
    let
      className' = showQualified runProperName className
      args = unwords $ (\(a, b) -> "(" <> debugType (maybe (srcTypeVar a) (srcKindedType (srcTypeVar a)) b) <> ")") <$> typeClassArguments tc
    "class " <> unpack className' <> " " <> args

debugValue :: Expr -> String
debugValue = init . render . prettyPrintValue 100

debugSubstitution :: Substitution -> [String]
debugSubstitution (Substitution solved unsolved names) =
  concat
    [ fmap go1 (IM.toList solved)
    , fmap go2 (IM.toList unsolved')
    , fmap go3 (IM.toList names)
    ]
  where
  unsolved' =
    IM.filterWithKey (\k _ -> IM.notMember k solved) unsolved

  go1 (u, ty) =
    "?" <> show u <> " = " <> debugType ty

  go2 (u, (_, k)) =
    "?" <> show u <> " :: " <> debugType k

  go3 (u, t) =
    unpack t <> show u
