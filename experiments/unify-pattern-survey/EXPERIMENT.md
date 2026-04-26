---
id: unify-pattern-survey
status: abandoned
verdict: no-win
branch: unify-pattern-survey
worktree: /workspace/p/unify-pattern-survey
baseline_sha: 43f6b613
head_sha: 39f77167
hypothesis: >
  Characterize the 412K cache hits the unification cache catches on a
  full pr-admin compile. If the dominant pattern is leaf-vs-leaf or
  hash-equal pairs, we can replace the (SourceType, SourceType) HashSet
  with a structural shortcut keyed off TypeFlags, eliminating most of
  the ~19% Hashable infrastructure cost without losing the cache's
  +24% net contribution.
headline_delta: "survey: 99.9% of hits are hash-equal; phases 2-4 either regress prelude or trade soundness for marginal speed"
tags: [unification, caching, characterization, type-flags]
started: 2026-04-26
closed: 2026-04-26
---

# unify-pattern-survey — what is the unification cache actually catching?

## Hypothesis

`unify-cache` proved the cache is +24% net positive on full builds
(412,727 hits / 1,049,412 lookups, 39.3% hit rate). But we never
characterized *which* pairs were hitting. The current cache costs ~19%
of compile time in Hashable infrastructure (lifted tuple hashing +
HashSet membership). If most hits fall into a structurally-detectable
category, we can short-circuit before touching the cache and skip both
the hash and the lookup.

Candidate buckets (each pair classified at cache-check time):

| Bucket | Pattern | Possible structural shortcut |
|---|---|---|
| `hash_eq` | `typeHash t1 == typeHash t2` | already cheap; cache lookup is wasted work |
| `both_tunknown_eq` | both `TUnknown _ u`, same `u` | direct `Eq` short-circuit |
| `both_concrete_leaf` | both leaves, no flags set | flag-based shortcut viable |
| `both_concrete_complex` | both complex, no unknowns/wildcards/synonyms | hash-equal sufficient? |
| `has_unknown` | at least one `TUnknown` | needs real unification |
| `has_synonym` | at least one synonym-containing type | needs real unification |
| `has_wildcard` | at least one wildcard | needs real unification |
| `other` | none of the above | needs real unification |

We already have flags for `containsUnknowns` / `containsTypeSynonyms`
/ `containsWildcards` and `typeHash` on every node from type-hash.
Bucketing is a few flag reads per pair — no traversal.

## What we measure

Counts only — no timing. One slow characterization run on pr-admin
with `PURS_UNIFY_SURVEY=1` set, dump a histogram on type-checker
shutdown:

```
unify-pattern-survey:
  total lookups:    1,049,412
  hits (in cache):    412,727
  misses (added):     636,685

  by bucket (hit / miss / total):
    hash_eq            123,456 / 12,345 / 135,801
    both_tunknown_eq    98,765 / 23,456 / 122,221
    ...
```

If `hash_eq` dominates hits → drop cache, hash-equal short-circuit
suffices. If `both_concrete_leaf` dominates → flag-based shortcut.
If hits spread evenly across all buckets → cache is doing genuine
work no shortcut can replace; close as no-win and look elsewhere.

## Scope

In:
- New module `Language.PureScript.TypeChecker.UnifyPatternSurvey`
  containing all IORef counters, bucketing logic, env-var gate, and
  shutdown dump.
- One added import + one added call in `Unify.hs` at the cache-check
  site. Single `unsafePerformIO` call returning `()`.
- A characterization run on pr-admin, output captured into `results.md`.

Out (deliberately):
- Any timing measurement during the survey phase. Survey overhead
  is irrelevant; only counts matter.
- Implementing the structural shortcut. That's a follow-up experiment
  once the histogram tells us which shortcut to build.
- Touching anything else in `Unify.hs`. The Unify.hs inlining
  sensitivity (see `LESSONS.md`) means we keep the diff to an import
  + a single call site.

## Risks / things to watch

- **Unify.hs inlining contamination.** All instrumentation lives in
  a separate module. Diff to `Unify.hs` itself: 1 import + 1 call
  site. We do not need timing during survey, so even if inlining
  flips, results are still valid for characterization. But if we
  later want to ship a shortcut, we re-measure timing on a clean
  branch *without* the survey module.
- **Bucketing taxonomy is opinionated.** If the dominant bucket is
  `other`, the taxonomy was wrong. Plan: dump a few sample pairs
  per bucket so we can spot-check.
- **Env-var gate cost.** The gate must be a CAF, read once. Implement
  via `unsafePerformIO` + NOINLINE on a `Bool` IORef seeded at
  process start; hot-path check is a single read.

## Plan

1. Create `UnifyPatternSurvey.hs` module.
2. Wire single call from `Unify.hs` cache-check site.
3. Run on pr-admin with `PURS_UNIFY_SURVEY=1`.
4. Dump histogram → `results.md`.
5. Decide direction based on dominant bucket.

## Links

- Worktree: /workspace/p/unify-pattern-survey
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Precedent: [unify-cache](../unify-cache/EXPERIMENT.md) — proved
  cache earns its keep but costs ~19% in Hashable infra; this
  experiment characterizes what the cache catches so we know which
  structural shortcut (if any) can replace the hash machinery.
