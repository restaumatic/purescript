# Task: skip-redundant-funapp-unify

## Goal

Eliminate ~75% of unification-cache hits by guarding three
"verify-extracted-head-constructor" calls in `Types.hs` with
`unless (eqType ...)` short-circuits.

## Background

`unify-callsite-survey` (counts-only) showed three sites in
`Types.hs` produce 75.5% of all unification-cache hits on a clean
pr-admin compile, all with 97-99% hit rates:

- `Types:funAppHead` line 1015: `unifyTypes tyFunction' tyFunction`
  (62.0% of hits, 99.4% hit rate)
- `Types:checkAbsArrow` line 841: `unifyTypes t tyFunction`
  (10.3% of hits, 97.8% hit rate)
- `Types:checkArrayHead` line 835: `unifyTypes a tyArray`
  (3.2% of hits, 97.0% hit rate)

In each case the outer pattern (`TypeApp _ (TypeApp _ x _) _` or
`TypeApp _ x _`) extracts a head constructor `x` and then the
unify asserts `x ~ tyFunction` or `x ~ tyArray`. For well-typed
programs `x` is already the expected constructor; the cache
catches the redundancy after paying the cost of a Set-membership
lookup.

`skip-redundant-entailment-unify` already proved this pattern
ships safely (-15.5% full at Entailment.hs line 337):

```haskell
unless (eqType inferredType t2) $ unifyTypes inferredType t2
```

## Approach

Three edits to `src/Language/PureScript/TypeChecker/Types.hs`:

```haskell
-- line 1015 (funAppHead)
checkFunctionApplication' fn (TypeApp _ (TypeApp _ tyFunction' argTy) retTy) arg = do
- unifyTypes tyFunction' tyFunction
+ unless (eqType tyFunction' tyFunction) $ unifyTypes tyFunction' tyFunction
  ...

-- line 841 (checkAbsArrow)
check' (Abs binder ret) ty@(TypeApp _ (TypeApp _ t argTy) retTy)
  | VarBinder ss arg <- binder = do
-     unifyTypes t tyFunction
+     unless (eqType t tyFunction) $ unifyTypes t tyFunction
      ...

-- line 835 (checkArrayHead)
check' (Literal ss (ArrayLiteral vals)) t@(TypeApp _ a ty) = do
- unifyTypes a tyArray
+ unless (eqType a tyArray) $ unifyTypes a tyArray
  ...
```

`unless` is already in scope. `eqType` is exported from
`Language.PureScript.Types` and used elsewhere.

## Key files

| File | Action |
| --- | --- |
| `src/Language/PureScript/TypeChecker/Types.hs` | three `unless (eqType ...)` guards |

## How to measure

```sh
cd /workspace/p/skip-redundant-funapp-unify
stack build
stack test --fast    # 1340 examples — must pass

experiments/scripts/exp run skip-redundant-funapp-unify \
  --scenarios all --runs 5
```

`exp run` writes one row per scenario to `results.md`. Discard
warm-up; the harness already medians over the remaining four.

### Optional: re-run the call-site survey on the head

To confirm the targeted sites' hit traffic dropped:

```sh
# cherry-pick the survey commit from unify-callsite-survey
cd /workspace/p/skip-redundant-funapp-unify
git cherry-pick <unify-callsite-survey commit sha>
stack build
# run pr-admin clean with PURS_UNIFY_CALLSITE_SURVEY=1
# expect: funAppHead, checkAbsArrow, checkArrayHead now show
# their cache-hit counts dropped to ~zero.
# Then drop the cherry-pick commit from the branch before close.
```

## Tests

```bash
stack test --fast    # 1340 examples; must pass
```

## Risks / things to watch

- **Suspiciously large speedup.** LESSONS: any single-scenario
  delta >20% warrants a semantics check. Run `stack test --fast`
  and spot-check the test output / a couple of generated JS files.
- **Inlining sensitivity in Types.hs.** Smaller risk than
  `Unify.hs`, but check binary size doesn't jump unexpectedly
  (>1MB is a smell).
- **Hidden semantic difference of skipping.** The skipped path
  no longer:
  - Pushes an `ErrorUnifyingTypes` hint onto the error stack —
    but only matters if the unify was going to error, which by
    construction (eqType true) it isn't.
  - Inserts the pair into the unification cache — meaning a
    *later* unify of the same pair will not hit the cache.
    But that's the point: subsequent calls to the same site
    will also short-circuit on eqType, so the cache-insertion
    isn't load-bearing.
- **Unify.hs cache check redundancy.** The cache check inside
  `unifyTypes''` is now skipped at three hot sites, so the hit
  rate inside `Unify.hs` will drop. This shifts work distribution
  but shouldn't change correctness.
