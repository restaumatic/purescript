---
id: entailment-decl-memo
status: closed
verdict: abandoned
branch: entailment-decl-memo
worktree: /workspace/p/entailment-decl-memo
baseline_sha: c84101d8
head_sha: c84101d8
hypothesis: >
  Per-decl entailment-redundancy survey shows top decls hammer the
  same `(className, args)` shapes 9.94×–11.15× within a single
  declaration's typecheck. `MenuV2.Menus.spec` alone has 20,144
  solves on 2,026 distinct shapes — 90% potential cache hits.
  A within-decl memo on substituted `(Qualified ClassName,
  [SourceType])` returning the cached dictionary `Expr` should
  short-circuit those repeats. Cleared between decls so the cache
  has bounded size and per-call substitution invariants hold.
headline_delta: abandoned — solve.go's withFreshTypes + fundep unifications are non-idempotent state effects; cache hits cause downstream "instance head contains unknown type variables" errors. Survey's 9–11× reuse was a `briefType` upper bound, not structurally cacheable work. Stats with structural keying showed 9.12% hit rate.
tags: [entailment, memo, decl-scope, redundancy, abandoned]
started: 2026-05-07
closed: 2026-05-07
---

# entailment-decl-memo

## Why

`entailment-redundancy` (see results.md) confirmed that within-decl
solve-redundancy is decisive: 8 of the top-10 expensive decls have
≥5× within-decl reuse, three of them >9×. The survey counted at the
top of `entails.solve.go`, so the work being repeated is the entire
solve — instance-search, candidate matching, fundep enforcement,
dictionary expression construction.

Distinct from the prior `entailment-memo` experiment (April), which
on instrumentation pivoted to a simpler `eqType` guard and shipped
as `skip-redundant-entailment-unify` (-15.5% full). That fix
short-circuits the trailing unification *inside* solve when both
sides are equal. It does not skip the solve itself. The current
survey shows the pre-unification work (Map lookups, candidate
matching, substitution) is still volume-heavy.

## Hypothesis

A `Map (Qualified ClassName, [SourceType]) Expr` keyed by
post-substitution arguments, scoped to a single value-declaration's
typecheck and cleared between decls, will turn 90% of `solve.go`
calls on the hot decls into Map lookups. If a Map lookup costs
≤10µs and a solve costs 70–80µs (1,584ms / 20,144 solves on
`MenuV2.Menus.spec`), the win per decl is ~7× hot-path reduction.

## Scope

**In.**
- A new field on `CheckState` (or a separate per-decl IORef in
  `EntailmentAnatomy`-style separate module — see soundness below)
  holding the memo Map.
- Cache lookup at the top of `entails.solve.go`, **after**
  `substituteType` so unification variables are post-substituted
  before keying.
- Cache write on solve completion, before returning the dictionary
  expression.
- Cache clear at the start of each value-declaration typecheck —
  hooked from the same `withDeclTrace` boundary used for the
  per-decl marker.

**Out.**
- Cross-module caching. Caching binding groups vs single decls is
  a follow-up; first pass clears at every value/binding-group
  boundary.
- Caching solve sub-goals (the recursive solve calls under
  `solveSubgoals`). Not in scope for v1 — the top-level memo
  should be enough to test the hypothesis.
- Any change to the solve algorithm itself.

## Soundness considerations

1. **Substitution before keying.** If we key on raw `tys'` from
   `Constraint`, two calls with the same constraint but different
   substitution states will key differently and miss. Apply
   `substituteType subst` (which solve does internally anyway)
   *before* keying.

2. **Cached `Expr` reuse.** Dictionary expressions are
   value-structural and don't reference per-call thunks. Same input
   args ⇒ same dictionary expression. Verified by `entailment-memo`
   April work (which considered exactly this).

3. **WriterT side effects.** `entails.solve.go` operates inside
   the elaboration monad; it may emit hints, errors, substitutions
   to inferred-type context, or warnings. **This is the key
   correctness risk.** On a cache hit we'd skip those writes. Two
   cases:
   - **Idempotent writes.** Same constraint solved twice writes the
     same things. Replaying on hit is unnecessary; the first call
     already wrote. Probably the common case.
   - **State-dependent writes.** E.g., introducing a fresh skolem
     or unification variable. These would differ per call —
     caching breaks them.

   Plan: instrument first to see what gets written. If the writes
   are pure (warnings/hints based on args alone), cache. If
   skolems/unifications happen, either (a) replay them on hit by
   storing a "what to write" thunk in the cache, or (b) only cache
   on solves that proved write-free.

4. **LESSONS.md warning.** "Don't cache cheap per-decl typecheck
   work" — but solve on hot decls is not cheap (1.3–1.8s per decl
   per the chrome trace). The threshold is "cost-per-call exceeds
   hash+map-diff overhead." A solve at ~78µs vs a HashMap lookup
   at ~5µs gives ~15× headroom — well above the threshold that
   killed `tc-queries`.

5. **Inlining-sensitivity.** `Entailment.hs` is similarly
   touchy as `Unify.hs` per LESSONS. Keep the hot-path edit to
   one or two lines (lookup + write); put all infrastructure
   (Map, IORef, dump function) in a separate module
   `Language.PureScript.TypeChecker.EntailmentMemo` imported only
   for the hook.

## Falsification

The experiment fails if any of:

- **Prelude regression ≥3%.** The 4-prior-unify-experiments
  pattern: caches that win on `full` but regress prelude. If a
  per-decl cache regresses the prelude scenario (which has many
  small decls and may pay setup overhead per decl), the memo's
  net is negative.
- **Soundness break.** Tests fail, or compiled output changes
  (compare `output/` hash before/after).
- **Wash on full.** If full is within ±1% of baseline, the
  hypothesis that hot-decl solve-volume drives full-build cost
  is wrong, or the cache lookup overhead matches the saved work.

## Links

- Worktree: /workspace/p/entailment-decl-memo
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Predecessor survey: [../entailment-redundancy/results.md](../entailment-redundancy/results.md)
- Prior pivot: [../skip-redundant-entailment-unify/EXPERIMENT.md](../skip-redundant-entailment-unify/EXPERIMENT.md)
