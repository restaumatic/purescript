# Phase A — static call-site enumeration

Output of static survey of `compare`/`==` call sites for `Qualified`,
`ProperName`, `ModuleName`, `PSString` in
`/workspace/p/name-compare-lineage/src`. Generated 2026-05-09.

## Prior art (Bucket 1 — already catalogued by `name-compare-survey`)

| site | key type | freq |
|---|---|---|
| `Entailment.hs:102` — `findDicts`: double-`M.lookup` on `InstanceContext` | `Qualified ProperName`, `QualifiedBy` | high |
| `Entailment.hs:288` — `M.lookup className' classesInScope` on `typeClasses env` | `Qualified (ProperName 'ClassName)` | high |
| `Kinds.hs:169` / `Kinds.hs:179` — `M.lookup v (E.types env)` at `TypeConstructor` | `Qualified (ProperName 'TypeName)` | high |
| `Types.hs:504` — `M.lookup c (dataConstructors env)` at `Constructor` | `Qualified (ProperName 'ConstructorName)` | high |

These were all migrated to HashMap in `env-hashmap` — no-win. Noted for
completeness.

---

## Bucket 1 — Environment Map Lookups (additional)

| # | file:line | excerpt | key type | freq |
|---|---|---|---|---|
| 1 | `TypeChecker/Monad.hs:290` | `M.lookup qual (names env)` in `lookupVariable` | `Qualified Ident` | high — every `Var` lookup during typecheck |
| 2 | `TypeChecker/Monad.hs:300` | `M.lookup qual (names env)` in `getVisibility` | `Qualified Ident` | high — called for every bound-name visibility check |
| 3 | `TypeChecker/Monad.hs:321` | `M.lookup (Qualified qb' name) (types env)` in `lookupTypeVariable` | `Qualified (ProperName 'TypeName)` | med |
| 4 | `TypeChecker/Monad.hs:179` | `M.member ... types (checkEnv orig)` | `Qualified (ProperName 'TypeName)` | med — per-binding-group |
| 5 | `TypeChecker/Synonyms.hs:91,97` | `M.lookup ctor syns` inside `replaceAllTypeSynonyms'` | `Qualified (ProperName 'TypeName)` | **high — every type node during synonym expansion** |
| 6 | `TypeChecker/Synonyms.hs:158` | `M.member ctor syns` in `isSyn` / `containsTypeSynonyms` | `Qualified (ProperName 'TypeName)` | high — traverse of every type node |
| 7 | `TypeChecker/Synonyms.hs:137` | `M.lookup ctor kinds` in `lookupKindArgs` | `Qualified (ProperName 'TypeName)` | high — co-fires with synonym expansion |
| 8 | `TypeChecker/Types.hs:84` | `M.lookup name (typeClasses . checkEnv)` in `lookupTypeClass` | `Qualified (ProperName 'ClassName)` | med — per instance constraint |
| 9 | `TypeChecker/Types.hs:640` | `M.lookup ctor (dataConstructors env)` | `Qualified (ProperName 'ConstructorName)` | med |
| 10 | `TypeChecker/Types.hs:900` | `M.lookup c (dataConstructors env)` | `Qualified (ProperName 'ConstructorName)` | med |
| 11 | `TypeChecker/Types.hs:791` | `M.lookup (Qualified ... ProperName ident) (types env)` | `Qualified (ProperName 'TypeName)` | med |
| 12 | `TypeChecker/Deriving.hs:60` | `className `M.lookup` typeClasses env` | `Qualified (ProperName 'ClassName)` | low |
| 13 | `TypeChecker/Deriving.hs:145,148` | two `M.lookup ... (typeClasses env)` calls | `Qualified (ProperName 'ClassName)` | low |
| 14 | `TypeChecker/Deriving.hs:342,346` | `M.lookup` on `types`, `dataConstructors` | mixed | low |
| 15 | `TypeChecker/Roles.hs:70,73,85` | `M.lookup`/`M.insert` on `roleEnv`/`types` | `Qualified (ProperName 'TypeName)` | low |
| 16 | `TypeChecker/Entailment/Coercible.hs:550,654,686` | three `M.lookup ... (types env)` for newtype resolution | `Qualified (ProperName 'TypeName)` | low |
| 17 | `Externs.hs:240–259` | six `M.lookup` when serializing externs | all four name types | low — per module once |
| 18 | `Linter/Exhaustive.hs:70,82` | `M.lookup qpn (types env)`, `M.lookup con (dataConstructors env)` | mixed | low |

---

## Bucket 2 — Other Map/Set/HashMap Operations Outside Environment

| # | file:line | excerpt | key type | freq |
|---|---|---|---|---|
| 19 | `Sugar/Names/Env.hs:310,312,317,321,324` | export-collision checks via `M.member`/`M.lookup` on `exTypes`/`exClasses` | `ProperName 'TypeName/ClassName` | low — per module |
| 20 | `Sugar/Names/Env.hs:416` | `M.lookup name exports` in `addExport` | `ModuleName` | low |
| 21 | `Sugar/Names.hs:416` | `M.lookup qname imps` in the rename pass `update` function | `Qualified a` for all categories | **high — every name occurrence in every module's renaming pass** |
| 22 | `Sugar/Names/Imports.hs:74` | `mn `M.lookup` env` per import record | `ModuleName` | low |
| 23 | `Sugar/Names/Imports.hs:209,223` | `M.lookup` on `exportedTypes` | `ProperName 'TypeName` | low |
| 24 | `Sugar/Names/Exports.hs:175,176,218,219,267,297` | re-export chain Map lookups | `ProperName 'TypeName/ClassName` | low |
| 25 | `Sugar/TypeClasses.hs:333` | `M.lookup (qualify mn className) m` building super-class map | `Qualified (ProperName 'ClassName)` | low |
| 26 | `Make/ExternsDiff.hs:259` | `S.member (P.getQual q, P.disqualify q) searches'` in `checkUsage` | `(Maybe ModuleName, Ref)` | med — fires on every incremental rebuild traversal |
| 27 | `Make/ExternsDiff.hs:170` | `S.member ref <$> M.lookup mn diffsMap` in `isRefChanged` | `Ref` (wraps `ProperName`) | med — per import per module |
| 28 | `Make/ExternsDiff.hs:106` | `S.member clsRef <$> M.lookup clsMod depsDiffsMap` | `Ref` | low |
| 29 | `Make/BuildPlan.hs:127` | `M.member moduleName (bpBuildJobs bp)` | `ModuleName` | low |
| 30 | `Linter/Imports.hs:65,113,303,354` | post-typecheck `M.lookup mn env` | `ModuleName` | low |
| 31 | `TypeChecker/Monad.hs:242,249` | `M.lookup mn . typeClassDictionaries`, `M.lookup cn <$> ...` | `ModuleName`, `Qualified (ProperName 'ClassName)` | **high — fires deep inside entailment solver** |

---

## Bucket 3 — Sorting / Dedup / Nub

| # | file:line | excerpt | key type | freq |
|---|---|---|---|---|
| 32 | `Types.hs:644` | `rowToSortedList = first (sortOn rowListLabel) . rowToList` | `Label` (newtype over `PSString`) | **high — alignRowsWith inner loop, fires in unifyRows / typeHeadsAreEqual / typesAreEqual** |
| 33 | `Types.hs:663–672` | `alignRowsWith` merge-join `compare l1 l2` | `Label` / `PSString` | high |
| 34 | `Entailment.hs:296–297` | `NEL.groupBy ((==) `on` tcdChain) ; sortOn (tcdChain &&& tcdIndex)` | `ChainId` (not name type) | high |
| 35 | `Entailment.hs:747` | `nubBy ((==) `on` rowListLabel) fixed` in `solveNub` | `Label` / `PSString` | low |
| 36 | `Entailment.hs:688` | `rowListLabel e `elem` remaining` in `RowUnion` partition | `Label` / `PSString` | low |
| 37 | `Subsumption.hs:123` | `minusBy' (comparing rowListLabel) t1 t2` | `Label` / `PSString` | med — record subsumption |
| 38 | `TypeChecker.hs:736` | `nubBy nubEq hidden` on `DeclarationRef` | `Qualified (ProperName 'ClassName)` indirectly | low |

---

## Bucket 4 — Direct compare/== on Name Types

| # | file:line | excerpt | key type | freq |
|---|---|---|---|---|
| 39 | `TypeChecker/Unify.hs:119` | `unifyTypes (TypeConstructor _ c1) (TypeConstructor _ c2) \| c1 == c2` — leaf fast-path | `Qualified (ProperName 'TypeName)` | **high — every TypeConstructor leaf pair** |
| 40 | `TypeChecker/Unify.hs:147` | `guardWith ... (c1 == c2)` fallthrough | `Qualified (ProperName 'TypeName)` | high |
| 41 | `TypeChecker/Unify.hs:164` | `constraintClass c1 == constraintClass c2 && constraintData c1 == constraintData c2` | `Qualified (ProperName 'ClassName)` | med |
| 42 | `TypeChecker/Entailment.hs:822` | `typeHeadsAreEqual (TypeConstructor _ c1) (TypeConstructor _ c2) \| c1 == c2` | `Qualified (ProperName 'TypeName)` | high — `matches` inner loop |
| 43 | `TypeChecker/Entailment.hs:873` | `typesAreEqual (TypeConstructor _ c1) (TypeConstructor _ c2) \| c1 == c2` | `Qualified (ProperName 'TypeName)` | high |
| 44 | `TypeChecker/Entailment.hs:337` | `unless (eqType inferredType t2) $ unifyTypes ...` — skip-redundant-unify guard (PR-shipped) | recurses into `Qualified` `==` | high |
| 45 | `Types.hs:970` | `eqType (TypeConstructor _ a) (TypeConstructor _ a') = a == a'` | `Qualified (ProperName 'TypeName)` | **high — terminal of every eqType walking constructor nodes** |
| 46 | `Types.hs:995` | `compareType (TypeConstructor _ a) (TypeConstructor _ a') = compare a a'` | `Qualified (ProperName 'TypeName)` | high — terminal of every compareType |
| 47 | `Types.hs:1042` | `eqConstraint`: `a == a'` where `a :: Qualified (ProperName 'ClassName)` | `Qualified (ProperName 'ClassName)` | high |
| 48 | `Types.hs:1045` | `compareConstraint`: `compare a a'` | `Qualified (ProperName 'ClassName)` | med |
| 49 | `TypeChecker.hs:554` | `typeHeadsApart l r \| eqType l r = False` | recurses to `Qualified ProperName` | med — orphan-check |
| 50 | `Sugar/Names.hs:268` | `M.lookup ident bound` (local `Map Ident SourcePos`) | `Ident` | high — every Var in rename pass |

---

## Bucket 5 — AST Types with Embedded Name Types (hidden compare callers)

These derive `Eq`/`Ord` (or have hand-written instances) and contain
the four name types. Comparing the outer type transitively invokes
name compare.

| AST type | embedding | Eq/Ord | triggered by |
|---|---|---|---|
| `Type a` (`Types.hs:187`) | `TypeConstructor_ ... (Qualified (ProperName 'TypeName))`, `TypeOp_ ... (Qualified (OpName ...))`, `ConstrainedType_ ... (Constraint a)`, `RCons_ ... Label` | hand-written `eqType`/`compareType` (Types.hs:958–1006) | `unifyTypes`, `subsumes`, `typeHeadsAreEqual`, `typesAreEqual`, `eqType` guards |
| `Constraint a` (`Types.hs:348`) | `constraintClass :: Qualified (ProperName 'ClassName)`, `constraintArgs :: [Type a]` | hand-written `eqConstraint`/`compareConstraint` (Types.hs:1035–1045) | `unifyTypes'` ConstrainedType branch; Map keying |
| `Label` (`Label.hs:17`) | newtype over `PSString` | derived | `alignRowsWith`, `rowToSortedList`, `nubBy`, `elem` on row lists |
| `PSString` (`PSString.hs:51`) | `[Word16]` | derived (lexicographic Word16 scan) | every `Label` compare, every `TypeLevelString` unification |
| `Ref` (`Make/ExternsDiff.hs:26`) | various `ProperName` constructors | derived | `S.member`, `M.lookup` in `ExternsDiff` — every incremental rebuild |
| `DeclarationRef` (AST/Declarations.hs) | various `ProperName`/`Ident`/`ModuleName` constructors | derived | export diff, `nubBy`, re-export resolution |
| `QualifiedBy` (`Names.hs:206`) | `ByModuleName ModuleName` | derived | every `Qualified a` compare descends here first |

---

## Key observations

**Dominant attribution path**: `eqType`/`compareType` at Types.hs:970/995
is the deepest terminal for `Qualified (ProperName 'TypeName)` compares.
Every `unifyTypes`, `typeHeadsAreEqual`, `typesAreEqual`,
`replaceAllTypeSynonyms`, or `eqType`-gated guard that reaches a
`TypeConstructor` ends here. The `-fprof-late` profile will likely
show most of the 6.6% landing in `eqType` / `compareType` / the `(==)`
wrapper at `Types.hs:958–959` — because *that's the leaf that
ultimately pulls on `Qualified` `compare`*.

**Second channel**: `Label`/`PSString` via `rowToSortedList` →
`sortOn rowListLabel` and the `alignRowsWith` merge-join inner loop.
Fires every time rows are unified/compared. pr-admin is record-heavy.

**Third channel**: `ImportMap` lookups at `Sugar/Names.hs:416` —
fires for every name occurrence in every AST during the rename pass.
Not typecheck cost — pre-typecheck rename — but visible in the full
pipeline.

**ExternsDiff `Set Ref`**: incremental-build cost. Frequency low for
cold builds, med for `nochange`/`leaf` scenarios.

**`lookupTypeClassDictionaries`**: chain at `Monad.hs:242→249` =
double Map lookup on `ModuleName` then `Qualified (ProperName 'ClassName)`.
Fires inside entailment solver inner loop. **Not migrated by
env-hashmap** (env-hashmap migrated `typeClassDictionaries` outer Map
but the inner per-module Map `Qualified ClassName -> Set TCD` is also
keyed on Qualified).

**The "new" sites the prior survey missed (before Phase B confirms)**:

1. **`Synonyms.hs:91,97,158,137` — `SynonymMap = Map Qualified ...`**
   passed in locally; not Environment. Walk-every-TypeConstructor
   frequency. Pre-existing prof shows ~1.9% inherited via
   `replaceAllTypeSynonyms'.go → $mTypeConstructor.\ → compare`.
2. **`Sugar/Names.hs:416` `ImportMap`** — rename-pass cost; high
   frequency at full-build pipeline level but NOT in typecheck-only
   profiles.
3. **Direct `eqType`/`compareType` terminals at Types.hs:970/995/1042**
   — every `unifyTypes` / `typesAreEqual` / `typeHeadsAreEqual` ends
   in these. The big one — **probably the load-bearing target**.
4. **ExternsDiff** — low for cold, but explains why incremental
   scenarios sometimes look noisy in name-compare cost.
