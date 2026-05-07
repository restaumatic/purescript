# Results for entailment-decl-memo

## Summary

**Verdict:** abandoned (soundness).

Within-decl entailment memo *cannot* be implemented as a simple
"key-by-(className, post-substituted args), return cached Expr" cache
because `solve.go`'s side effects on the global typechecker state are
not idempotent across structurally-identical calls. The
`entailment-redundancy` survey's 9–11× per-decl reuse rate was an
upper bound on naive matching and **does not translate to safely
cacheable work** without replaying the side effects.

## What was built

Implementation on `c84101d8` baseline:

- `Language.PureScript.TypeChecker.EntailmentDeclMemo` — separate
  module holding an `IORef (Map Key Expr)` cache, lookup/store/reset
  hooks, and an env-var-gated stats path
  (`PURS_ENTAILMENT_MEMO_STATS=1`).
- `Entailment.solve.go` — added a cache lookup at the top of the body
  (after `substituteType` so the key is post-substitution) and a
  conditional cache write in the Solved branch, gated on
    1. no deferred subgoal constraints (`null subDeferred` via
       `listen` on `solveSubgoals`)
    2. result Expr does not reference any `Qualified ByNullSourcePos`
       Var (decl-local fresh dict idents).
- `TypeChecker.withDeclTrace` — `Memo.resetForDecl tag` at every decl
  boundary.
- `TypeChecker.typeCheckModule` — `Memo.resetForDecl ("module " <> mn)`
  at module entry (defence in depth).
- `Command.Compile` — `Memo.dumpStats` before `printWarningsAndErrors`.

Build size: 48,644,288 bytes vs baseline 48,625,952 (Δ +18 KB,
nowhere near the >1 MB threshold that signals an inlining shift).

## Soundness failures encountered

### 1. CAF lifting of `resetForDecl`

**Symptom:** stats showed `resets: 1` over a full pr-admin compile.

**Cause:** `resetForDecl :: String -> ()` did not use its `String`
argument inside its body. GHC's optimiser (legitimately) noticed the
function was a constant and lifted the `unsafePerformIO` to a CAF —
which evaluates **once** per program run.

**Fix:** added `lastResetRef :: IORef String` and made the body
`writeIORef lastResetRef tag` so GHC sees a real argument
dependency. After the fix, stats showed
`resets: 38975, lookups: 345249, hits: 31505 (9.12%)` over a full
pr-admin compile — far more realistic, and confirming the boundary
is now firing.

**Lesson:** `unsafePerformIO`-backed "monadic" sentinel calls of type
`a -> ()` need to actually consume `a` in their IO body. A bang
pattern on the argument (`!_tag`) is not enough; GHC strips unused
arguments before deciding what to lift.

### 2. Cross-decl/cross-module identifier capture (mitigated)

**Symptom:** before the CAF fix, `Rename scope is missing ident
'$dictAlternative1'` and similar errors during pr-admin compilation.

**Cause:** the cache held entries keyed by post-substitution args
across decl boundaries. A cached Expr from decl A contains references
to decl-local fresh dict idents (`Qualified ByNullSourcePos
"$dictAlternative1"`) that aren't valid in decl B's scope.

**Fix attempted:** filter out cache writes whose Expr contains a
`Qualified ByNullSourcePos` Var (the `exprUsesLocalDict` walker), plus
the working `resetForDecl` clearing per decl. Both together made the
rename errors disappear.

### 3. Soundness break that killed the approach: type errors after cache hit

**Symptom:** with rename and CAF fixes in place, full pr-admin compile
produced new type errors like:

> No type class instance was found for `Row.Extra.TestHasLabelRL "name" t4 t3`
> The instance head contains unknown type variables.

at `Restaumatic.PR.MenuV2.PackagingUnit:detailsSpec` and several
other decls.

**Cause:** even within a single decl, `solve.go`'s Solved branch
performs three state-mutating operations whose effects the cached
return value cannot reproduce:

1. **`pairwiseM unifyTypes` on `substs`** (matching-substitution
   validity). Often a no-op on cache hit, but writes to the global
   `checkSubstitution` if not.
2. **`withFreshTypes tcd subst`** generates **fresh** type-variable
   `TUnknown`s for instance-head arguments not covered by the match.
   A cache hit *skips this allocation*, so any later constraint that
   would have unified against those fresh vars sees nothing to unify
   with.
3. **`zipWithM_ unifyTypes (tcdInstanceTypes tcd) tys''`** — fundep
   enforcement that propagates info from instance heads into the
   active substitution.

The naive argument that "if post-sub args are equal, the unification
work was already done" is **wrong** in general because of (2). Each
`solve.go` call allocates *new* fresh `TUnknown`s and unifies them.
Skipping a call means subsequent solves are missing the info those
unifications would have propagated, leading to "instance head
contains unknown type variables" errors downstream.

A correct memo would need either:
- to *replay* withFreshTypes + the fundep unifications on every cache
  hit (which is most of the work — defeats the cache), or
- to detect the special case where withFreshTypes is a no-op (subst
  fully determines the instance head, no fresh vars needed) — narrow
  scope, more bookkeeping in the hot path, and still has to verify
  the substs/fundep unifications are no-ops.

Either path is more complex than the survey-driven hypothesis
suggested, and the residual benefit (the simple cases where solve is
truly idempotent) overlaps heavily with what
`skip-redundant-entailment-unify` already captures via its `eqType`
guard.

## Numbers

Single full-build run with cache enabled, after CAF fix, before
abandoning:

| Metric        | Value     |
| ------------- | --------- |
| lookups       | 345,249   |
| hits          | 31,505 (9.12%) |
| stores        | 255,510   |
| resets        | 38,975    |
| max cache size| 11,053    |

The 9.12% hit rate (vs the 9–11× per-decl reuse the survey
suggested) reflects the post-substitution-key narrowing: most
"reuse" the survey saw via `briefType` collapses different
post-substitution args. With structural keying, the actual
intra-decl reuse is much lower.

## Verdict

**Abandoned.** The simple within-decl memo cannot be sound without
replaying solve's substitution/fresh-var side effects, which
removes most of its potential win. The narrow safe subset (no-op
withFreshTypes, idempotent fundep unifications) overlaps with
already-shipped optimisations (`skip-redundant-entailment-unify`).

If we want to attack solve volume on hot decls (`MenuV2.Menus.spec`'s
1,584 ms), the next angle should be the *substituteType* and
*matches* costs themselves rather than memoising the whole solve.

## Links

- Worktree: /workspace/p/entailment-decl-memo
- Survey that motivated this experiment: [../entailment-redundancy/results.md](../entailment-redundancy/results.md)
