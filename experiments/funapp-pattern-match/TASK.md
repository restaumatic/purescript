# Task: funapp-pattern-match

## Goal

Eliminate ~214k redundant `unifyTypes` calls per full pr-admin compile
(99.9% of calls at 3 hot sites are trivially `tyFunction ~ tyFunction`
or `tyArray ~ tyArray`, per `funapp-lineage-survey`) without the
prelude regression that the `unless (eqType …)` upstream-skip variant
hit.

## Background

`funapp-lineage-survey` (5713e832 baseline, full pr-admin compile,
214,268 instrumented calls) characterised the 3 hot sites:

| Callsite        | Calls    | Distinct (h1, h2) pairs | Top pair share |
|-----------------|---------:|------------------------:|---------------:|
| funAppHead      | 175,272  | 5                       | 99.998%        |
| checkAbsArrow   |  29,720  | 140                     | 99.5%          |
| checkArrayHead  |   9,276  | 63                      | 99.3%          |

99.9% of calls have the constant on both sides. The remaining 0.1%
is calls where t1 is a fresh `TUnknown` (must actually be solved).

`skip-redundant-funapp-unify` (closed no-win) added an
`unless (eqType x const)` guard at all three sites. Result on the
post-type-hash HashSet baseline: prelude **+6.4%**, others neutral.
Same shape as `unify-pattern-survey` Phase 2 (+7.1% prelude). The
prevailing theory is that an `eqType`-skip compiles to an
`if`-branch *after* the outer case-tree, while a nested
constructor pattern fuses into the case-tree itself — different
Core, different GHC code-gen.

## Approach

At each of the 3 sites, split the existing single clause into two:

1. **Common case** — head is exactly the expected `TypeConstructor`.
   Match it literally and skip the `unifyTypes` call.
2. **Rare case** — head is `TUnknown`, synonym, wildcard, etc.
   Fall through to the existing path including `unifyTypes`.

Concretely, for `checkFunctionApplication'`
(src/Language/PureScript/TypeChecker/Types.hs:998):

```haskell
-- Common case: head is exactly Function. Skip unifyTypes.
checkFunctionApplication' fn (TypeApp _ (TypeApp _ (TypeConstructor _ tc) argTy) retTy) arg
  | tc == C.Function = do
      arg' <- tvToExpr <$> check arg argTy
      return (retTy, App fn arg')
-- Rare case: head is TUnknown / synonym / wildcard. Existing path.
checkFunctionApplication' fn (TypeApp _ (TypeApp _ tyFunction' argTy) retTy) arg = do
  unifyTypes tyFunction' tyFunction
  arg' <- tvToExpr <$> check arg argTy
  return (retTy, App fn arg')
```

Same shape for `check'` Abs (line 826) and ArrayLiteral (line 821).

Cross-check `C.Function` / `C.Array` come from
`Language.PureScript.Constants.Prim` and equal the `TypeConstructor`
inside `tyFunction` / `tyArray` (`Types.hs`).

## Key files

- `src/Language/PureScript/TypeChecker/Types.hs` — the 3 sites at
  lines 821 (ArrayLiteral), 826 (Abs), 998 (checkFunctionApplication').
- Possibly `Language.PureScript.Constants.Prim` if Function/Array
  constructor names need importing.

## How to measure

```sh
# Sanity check first — 5713e832 baseline exists
ls experiments/baselines/5713e832/purs

# Full clean rebuild (don't trust incremental for perf)
cd /workspace/p/funapp-pattern-match
rm -rf .stack-work && stack build

# Verify binary size is in the ~49 MB ballpark of the type-hash baseline
ls -la $(stack path --local-install-root)/bin/purs

# Run all 4 scenarios
cd /workspace/purescript
experiments/scripts/exp run funapp-pattern-match --scenarios all --runs 5
```

The `unify-callsite-survey` HashSet hit-counts are stable across
runs, so an incidental win/regress on cache statistics is unlikely.
What we're measuring is whether removing 214k calls per full build
beats the GHC code-gen disturbance.

## Tests

```bash
stack test --fast  # all tests must pass
```

The change is semantically a no-op: in the common case, the existing
code did `unifyTypes (TypeConstructor C.Function) tyFunction` which
unifies-equal-to-itself by construction; the new clause skips this
no-op. The rare case is byte-for-byte the previous body.

## Risks / things to watch

- **Binary size delta.** A >1 MB shrink relative to the 5713e832
  baseline (49.1 MB) means GHC inlining changed; treat the
  measurement as suspect (see Unify.hs sensitivity / stack
  incremental lessons in LESSONS.md).
- **Constructor-name mismatch.** The literal pattern compares
  `tc == C.Function` by `Qualified ProperName` equality. If
  `C.Function` is something other than the actual `Qualified
  ProperName` inside `tyFunction`, the common-case clause will
  never fire (silent slowdown rather than incorrectness). Verify
  by adding a one-shot `traceM` during dev or grepping the
  `Constants.Prim` definitions.
- **Synonym expansion path.** If `fnTy` reaches the site with
  `Function` wrapped in a `KindedType` or `ParensInType` wrapper,
  the literal clause won't fire and we fall through to the existing
  unify (acceptable — same as today).
- **Prelude-regression hypothesis is falsifiable.** If prelude still
  regresses ≥3% with the nested pattern, that *refutes* the code-gen
  theory and suggests the regression has another mechanism (e.g.,
  cache-residency interaction). Document carefully if so.
