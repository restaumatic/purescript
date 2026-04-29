# Task: unify-leaf-fast-path

## Goal

Eliminate the 86% of unification-cache hits that the
`unify-cache-anatomy` survey identified as 1-2-node leaf pairs
(constructor-self recurrences from recursive descent), without
triggering the +7% prelude regression observed in three prior
call-site-skip experiments.

## Background

`unify-cache-anatomy` survey (5713e832 baseline) on full pr-admin
compile recorded:

- 1,049,340 cache lookups, 412,665 hits (39.33% hit rate)
- **86% of hits on 1-2-node pairs** (354,804) — these are
  `(TypeConstructor c, TypeConstructor c)` and `(TypeVar v, TypeVar v)`
  recurrences
- 97% of hits on ≤10-node pairs
- Misses skew bigger (27% of misses are on ≥11-node pairs)

The cache currently sits at `unifyTypes''` (Unify.hs:120-123) which
is *inside* `unifyTypes`, *after* `substituteType` and inside the
`withErrorMessageHint` bracket. Cache hits cost: HS.member, plus
the substituteType + hint-stack work that already happened.

Three prior call-site-skip experiments regressed prelude by ~+7%
with three different mechanisms:
- `skip-redundant-funapp-unify`: `unless (eqType t1 t2)` guard at
  3 hot sites: prelude +6.4%
- `unify-pattern-survey` Phase 2: replace cache check with eqType
  guard inside `unifyTypes''`: prelude +7.1%
- `funapp-pattern-match`: nested constructor patterns at 3 sites:
  prelude +7.0%

Three mechanisms, three near-identical regressions ⇒ the regression
is structural to skipping the wrapper for whole call sites, not a
property of any one mechanism.

## Approach

Add five pre-substitute pattern clauses at the top of `unifyTypes`,
short-circuiting trivially-equal leaves before the
substituteType/hint-stack/cache wrapper:

```haskell
unifyTypes :: SourceType -> SourceType -> TypeCheckM ()
unifyTypes (TypeConstructor _ c1) (TypeConstructor _ c2) | c1 == c2 = pure ()
unifyTypes (TypeVar _ v1)         (TypeVar _ v2)         | v1 == v2 = pure ()
unifyTypes (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = pure ()
unifyTypes (TypeLevelInt _ n1)    (TypeLevelInt _ n2)    | n1 == n2 = pure ()
unifyTypes (Skolem _ _ _ s1 _)    (Skolem _ _ _ s2 _)    | s1 == s2 = pure ()
unifyTypes t1 t2 = do
  sub <- gets checkSubstitution
  withErrorMessageHint (ErrorUnifyingTypes t1 t2) $
    unifyTypes'' (substituteType sub t1) (substituteType sub t2)
  where ...
```

Soundness: identical leaves trivially unify to themselves —
substituteType is a no-op on TypeConstructor/TypeVar/TypeLevelString/
TypeLevelInt/Skolem (only TUnknown is substituted), no error possible
when c1 == c2. The wrapper `withErrorMessageHint` is only needed
when actually unifying — for trivial-equality leaves no error can fire.

Mismatched leaves (c1 /= c2) fall through to the regular
`unifyTypes' (TypeConstructor _ c1) (TypeConstructor _ c2)` clause
which calls `guardWith (errorMessage TypesDoNotUnify)` — error
behaviour preserved.

Cache stays in place for non-leaf pairs. HashSet still grows for
TypeApp / KindApp / RCons / ConstrainedType / etc.

## Key files

| File                                              | Change |
| ------------------------------------------------- | ------ |
| `src/Language/PureScript/TypeChecker/Unify.hs:115-118` | Add 5 leaf pattern clauses before the catch-all |

## How to measure

```sh
experiments/scripts/exp run unify-leaf-fast-path --scenarios all --runs 5
```

All four scenarios. Watch prelude especially — falsification target.

## Tests

```bash
stack test --fast
```

## Risks / things to watch

1. **Unify.hs inlining sensitivity** (LESSONS.md: GHC inlining
   regression when modifying Unify.hs hot path). Adding 5 top-level
   pattern clauses could shift inlining decisions for the whole
   function. After build, sanity-check `purs --version` runs and
   the binary size hasn't dramatically changed.
2. **Skolem fast-path correctness**: Skolem is `Skolem _ name mbK
   sko sc`. Same `sko` (skolem constant) from same scope should
   trivially unify, but we should verify the existing clause at
   line 153 has the same shape — yes, it does (`unifyTypes' (Skolem
   _ _ _ s1 _) (Skolem _ _ _ s2 _) | s1 == s2 = return ()`).
3. **stack test --fast contamination** (LESSONS.md): re-installs
   unoptimised binary. Always re-run `stack build` after tests to
   restore the optimised binary before benchmarking.
4. **Catch-all overlap**: GHC may warn about `Skolem` patterns
   binding `_ _ _` if the data ctor changes shape. Verify after edit.
