---
id: skip-redundant-funapp-unify
status: abandoned
verdict: no-win
branch: skip-redundant-funapp-unify (799e8208 phase 1) + skip-redundant-funapp-unify-th (43f6b613 phase 2)
worktree: /workspace/p/skip-redundant-funapp-unify (phase 1) + /workspace/p/skip-redundant-funapp-unify-th (phase 2)
baseline_sha: 799e8208 (phase 1) / 43f6b613 (phase 2)
head_sha: 15540bba (phase 1) / 9ad7c523 (phase 2)
hypothesis: >
  unify-callsite-survey identified three sites in Types.hs that
  produce 75% of all unification-cache hits with 97-99% hit rates.
  Each site extracts a head constructor from a TypeApp shape and
  then unifies it with a known constant — almost always already
  equal. Adding `unless (eqType x const) $ unifyTypes x const`
  guards at each site should eliminate those calls before the cache
  ever sees them, mirroring skip-redundant-entailment-unify's
  approach for fundep enforcement (-15.5% full).
headline_delta: "phase 1 (799e8208 S.Set): neutral on all 4. phase 2 (43f6b613 HashSet, the right baseline): prelude +6.4%, others neutral. Hypothesis falsified — cache amortises tiny-pair traffic across prelude cascade better than upstream eqType skip."
tags: [unification, redundancy, types, skip]
started: 2026-04-26
closed: 2026-04-26
---

# skip-redundant-funapp-unify — skip the assert-tyFunction / assert-tyArray unifications

## Hypothesis

The `unify-callsite-survey` histogram pinpointed three "verify the
extracted head constructor" sites that account for 75.5% of all
unification-cache hits on a pr-admin compile:

| Site (in `Types.hs`) | Code | Hits | Hit% | Share |
|---|---|---:|---:|---:|
| `funAppHead` | `unifyTypes tyFunction' tyFunction` | 174,161 | 99.4% | 62.0% |
| `checkAbsArrow` | `unifyTypes t tyFunction` | 29,067 | 97.8% | 10.3% |
| `checkArrayHead` | `unifyTypes a tyArray` | 9,009 | 97.0% | 3.2% |

In all three cases the outer pattern (`TypeApp _ (TypeApp _ x _) _`)
already binds `x` to the inner type constructor. Almost all of the
time, `x` *is* the function/array constructor — the call is an
assertion that does no useful work. The cache catches these by
memoising the (x, tyFunction) pair, but every cache hit still pays
the membership lookup cost.

Replacing each with:

```haskell
unless (eqType x const) $ unifyTypes x const
```

skips the cache lookup on the hot 97-99% of cases, paying only one
constructor-tag comparison instead. This is the same pattern
`skip-redundant-entailment-unify` shipped at line 337 of
Entailment.hs (already proven sound and a -15.5% full-build win on
its workload).

## Soundness

When `eqType x const` is true:
- `unifyTypes x x` is guaranteed a no-op: structural traversal
  produces no fresh bindings (no TUnknowns to solve), no new
  errors (the types are already equal), no substitution changes.
- The cache lookup that would have happened, the cache insertion
  if missed, and any hint-pushing on errors are all skipped — but
  none of those have observable effects when the unify is a no-op.

`eqType` is the same structural equality used by the cache's
`Eq (SourceType, SourceType)` Eq fallback, so we're not weakening
soundness; we're just doing the equality check earlier and using
its positive result to bypass the cache.

## What we measure

Standard four scenarios (median of 4, warm-up discarded), against
baseline `799e8208`:

- `full` — raw throughput. Expectation: meaningful win
  (~75% of cache hit traffic at three sites; cache lookup is
  measurable per the type-hash analysis).
- `nochange` — no-op rebuild. Expectation: neutral.
- `prelude` — touch-Prelude rebuild (1342 deps). Expectation:
  neutral or small win.
- `leaf` — single-module rebuild. Expectation: neutral or
  small win.

A re-run of the call-site survey (cherry-picked onto the new
head) confirms cache-hit traffic dropped at the three targeted
sites.

## Scope

In:
- Three `unless (eqType ...)` guards in
  `src/Language/PureScript/TypeChecker/Types.hs` at funAppHead
  (line 1015), checkAbsArrow (line 841), checkArrayHead (line 835).
- `stack test --fast` must pass (no semantics break).
- All four scenarios benchmarked.
- Optionally: re-run the call-site survey to confirm the hit-rate
  drop.

Out:
- Touching `Unify.hs` or the cache structure (LESSONS: inlining
  sensitivity; cache-as-hash-equal-memoizer is essentially
  optimal for what it does).
- Other call sites with lower hit-rates (Subsumption:default at
  17.6% mid-volume isn't a structural assertion; needs different
  handling).
- Dropping the cache. That's a follow-up if the remaining hit
  rate is small enough to not justify the cache structure.

## Risks / things to watch

- **`eqType` cost on the *miss* side.** If `eqType` is expensive on
  large types, the 1-3% miss path adds work. Mitigation: in all
  three cases, the comparison is against a 1-node constant
  (`tyFunction` / `tyArray` is a `TypeConstructor`), so the miss
  fails on the first pattern-match — no walk.
- **Pattern-match coverage drift.** If a future caller passes a
  shape where `tyFunction'` isn't a TypeConstructor, the eqType
  comparison still terminates fast and falls back to unify, which
  preserves whatever error the original code would have raised.
- **Inlining sensitivity in Types.hs.** Smaller risk than
  `Unify.hs` per LESSONS, but worth verifying binary size doesn't
  jump unexpectedly.
- **Suspiciously large speedup.** LESSONS: any >20% delta on a
  scenario warrants a semantics check (run the test suite and
  spot-check one or two large modules in the output dir).

## Plan

1. Add the three `unless (eqType ...)` guards.
2. `stack build && stack test --fast` — must pass.
3. `exp run skip-redundant-funapp-unify --scenarios all --runs 5`.
4. Cherry-pick the survey instrumentation, re-run on pr-admin, log
   the new histogram.
5. Decide verdict.

## Links

- Worktree: /workspace/p/skip-redundant-funapp-unify
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Precedent:
  - [unify-callsite-survey](../unify-callsite-survey/EXPERIMENT.md)
    — identified the three concentrated sites this experiment fixes.
  - [skip-redundant-entailment-unify](../skip-redundant-entailment-unify/EXPERIMENT.md)
    — same pattern (guard a unify call with eqType) at a different
    site; -15.5% full-build win.
  - [unify-pattern-survey](../unify-pattern-survey/EXPERIMENT.md)
    — established that the cache is a hash-equal memoizer, motivating
    upstream skips over cache redesign.
