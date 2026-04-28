---
id: ptr-eq-unify
status: in-progress
verdict: tbd
branch: ptr-eq-unify
worktree: /workspace/p/ptr-eq-unify
baseline_sha: 5713e832
head_sha: b9fcf10c
hypothesis: >
  At the top of `unifyTypes`, before substituteType / withErrorMessageHint /
  cache lookup, add `reallyUnsafePtrEquality# t1 t2`. When both arguments
  refer to the same heap object (very common for type constants like
  `tyFunction` propagated from `Environment`), this returns True with a
  single machine compare and we return immediately. When pointers differ,
  we fall through to the existing path — no behavioural change. This
  bypasses ALL of the per-call wrapper overhead (substituteType walks even
  closed types; withErrorMessageHint pushes/pops the hint stack; the cache
  lookup itself), not just the inner unifyTypes' check. Soundness is
  unconditional: ptr-equal => structurally equal => unifyTypes is a no-op.
headline_delta: tbd
tags: [unification, fast-path, pointer-equality]
started: 2026-04-28
closed: null
---

# ptr-eq-unify — `reallyUnsafePtrEquality#` fast-path at top of `unifyTypes`

## Context

`unify-pattern-survey` and `skip-redundant-funapp-unify` together
established that on the post-type-hash HashSet baseline the
unification cache is essentially optimal — replacing it (drop entirely:
+24% full) or short-circuiting upstream with `eqType` (prelude +6.4–7.1%)
both regress.

But the underlying signal stands: 174k cache hits at one site
(`funAppHead` in Types.hs, 99.4% hit rate) on the trivial pair
`(tyFunction, tyFunction)` is algorithmically wasteful. Every one of
those calls goes through:

```haskell
unifyTypes t1 t2 = do
  sub <- gets checkSubstitution
  withErrorMessageHint (ErrorUnifyingTypes t1 t2) $
    unifyTypes'' (substituteType sub t1) (substituteType sub t2)
  where
  unifyTypes'' t1' t2' = do
    cache <- gets unificationCache
    unless (HS.member (t1', t2') cache) $ ...
```

Even on a cache hit we pay: `gets checkSubstitution` (StateT read),
`substituteType sub t1` and `substituteType sub t2` (full traversal —
returns the input unchanged for closed types but still walks),
`withErrorMessageHint` (push/pop hint stack), `gets unificationCache`,
hash + bucket lookup.

## Hypothesis

`reallyUnsafePtrEquality# t1 t2 ==# 1#` is one machine compare. When
both args refer to the same heap object, return immediately —
short-circuiting all of the above before any of it runs.

For `tyFunction` propagated from `Environment` (a single shared
constant) we expect the pointers to match in the vast majority of the
174k cache-hit calls.

When pointers differ but types are structurally equal: ptr-eq returns
False, we fall through to the existing path, the cache catches it. So
unlike `eqType` upstream skip, **we never bypass cache insertion**.
The cache continues to memoise structurally-equal-but-different-allocation
pairs.

## Soundness

`reallyUnsafePtrEquality#` returning True on `t1, t2 :: SourceType`
implies `t1` and `t2` point to the same heap object, hence
structurally equal, hence `unifyTypes t1 t2` is a no-op (no fresh
TUnknowns to solve — closed types — and the equality check would
succeed in `unifyTypes'`). Returning False is allowed even when
structurally equal; we just fall through. There is no path that skips
required substitution updates.

## What we measure

Standard four scenarios (median of 4, warm-up discarded), against
baseline `5713e832` (= type-hash + the trivial hlint fix):

- `full` — raw throughput. Expectation: small win.
- `nochange` — no-op rebuild. Expectation: neutral.
- `prelude` — touch-Prelude rebuild (1342 deps). Expectation: small
  win or neutral. Critically: must NOT regress like the `eqType` skip
  did — ptr-eq doesn't bypass cache insertion.
- `leaf` — single-module rebuild. Expectation: neutral or small win.

## Scope

In:
- One change in `src/Language/PureScript/TypeChecker/Unify.hs`
  at the top of `unifyTypes`: pointer-eq fast-path before the existing
  body.
- Imports for `reallyUnsafePtrEquality#` (`GHC.Exts`).
- `stack test --fast` must pass (1340/1340).
- All four scenarios benchmarked.

Out:
- Adding `tfHasUnknowns` to skip `substituteType` (separate experiment
  if this one wins or partial-wins).
- Restructuring `checkFunctionApplication'` to avoid the assertion at
  the algorithm level (a deeper change).
- Removing the cache or changing `unifyTypes''` (we want to keep
  cache amortisation intact).

## Risks / things to watch

- **Inlining sensitivity in Unify.hs.** LESSONS records that even
  unrelated changes to Unify.hs can shift GHC inlining and produce
  +135% regressions. Binary size is the primary signal — anything
  more than ~+/- 1 MB without proportional source change is suspect.
- **`stack clean` before benchmarking.** Per the
  `skip-redundant-funapp-unify` lesson, incremental builds can
  produce slow binaries. Always clean.
- **Suspicious speedups.** Any >20% delta on a scenario warrants a
  semantics check.

## Plan

1. Add `reallyUnsafePtrEquality#` fast-path at top of `unifyTypes`.
2. `stack clean && stack build && stack test --fast` — must pass.
3. `exp run ptr-eq-unify --scenarios all --runs 5`.
4. Check binary size delta vs baseline.
5. Decide verdict.

## Links

- Worktree: /workspace/p/ptr-eq-unify
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Precedent:
  - [skip-redundant-funapp-unify](../skip-redundant-funapp-unify/EXPERIMENT.md)
    — established that upstream `eqType` skip regresses prelude on
    HashSet baseline; ptr-eq doesn't bypass cache so should avoid that
    failure mode.
  - [unify-pattern-survey](../unify-pattern-survey/EXPERIMENT.md)
    — established cache as hash-equal memoiser; this experiment keeps
    cache intact.
  - [unify-callsite-survey](../unify-callsite-survey/EXPERIMENT.md)
    — identified 75% of cache traffic at 3 sites; ptr-eq targets the
    per-call overhead at all sites uniformly.
