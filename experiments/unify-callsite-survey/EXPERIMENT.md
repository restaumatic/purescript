---
id: unify-callsite-survey
status: done
verdict: research-win
branch: unify-callsite-survey
worktree: /workspace/p/unify-callsite-survey
baseline_sha: 799e8208
head_sha: a902b849
hypothesis: >
  The unify-pattern-survey showed 99.9% of unification cache hits are
  hash-equal pairs — i.e. the cache is a memoizer for "we already
  unified this." If that redundancy is concentrated at a few external
  call sites of unifyTypes, we can skip those calls upstream
  (skip-redundant-entailment-unify style) and drop the cache entirely,
  recovering its ~19% Hashable cost. If hits spread thinly across
  many sites, the cache structure itself is the only lever.
headline_delta: "75% of cache hits at 3 sites in Types.hs (funAppHead 62%, checkAbsArrow 10%, checkArrayHead 3%); 97-99% hit rates"
tags: [unification, characterization, callsites, redundancy]
started: 2026-04-26
closed: 2026-04-26
---

# unify-callsite-survey — where do redundant unifyTypes calls come from?

## Hypothesis

The previous `unify-pattern-survey` answered *what* the cache catches
(hash-equal pairs, 99.9% of hits). It did **not** answer *who* —
which external call site invokes `unifyTypes` with a pair that's
already been unified. Without that, we can't target redundancy
upstream and we're stuck shipping the HashSet's ~19% Hashable cost.

If the dominant hash-equal traffic is concentrated at one or two
external call sites — e.g. a row-unification path or an
instance-resolution check — we can prove redundancy at that site
(à la `skip-redundant-entailment-unify`) and skip the call. Once
upstream redundancy is gone, the cache can be dropped without losing
correctness or speed.

If hits spread thinly across many call sites (50+ at 1–2% each),
there's no concentrated upstream win and we should look at cache
structure (small-LRU, bloom front-end, per-module reset) instead.

## What we measure

Counts only — no timing. One pr-admin clean build with
`PURS_UNIFY_CALLSITE_SURVEY=1` set. For each external call site of
`unifyTypes`, record:
- total invocations
- hash-equal invocations (`typeHash t1 == typeHash t2`)

Hash-equal at the external entry is a strong proxy for "this call
would hit the cache" (the previous survey showed cache-hit ≈
hash-equal at >99.9% precision).

Output (sorted by hash-equal count descending):

```
=== unify-callsite-survey ===
total external calls:    XXX,XXX
hash-equal calls:         YY,YYY  (ZZ%)

by call site (hash_eq / total / hash_eq% / share-of-all-hits):
  Subsumption:row              ##,###  ##,###   ##.#%   ##.#%
  Entailment:instanceMatch     ##,###  ##,###   ##.#%   ##.#%
  Types:checkValue             ##,###  ##,###   ##.#%   ##.#%
  ...
```

## Scope

In:
- New module `Language.PureScript.TypeChecker.UnifyCallSiteSurvey`
  with all IORef state, env-var gate, and dump function.
- One bang-pattern recording call at each external `unifyTypes` call
  site outside `Unify.hs`. Each site gets a stable string tag.
- Dump call wired into `app/Command/Compile.hs` shutdown.
- Characterization run on pr-admin, results in `results.md`.

Out (deliberately):
- Tagging recursive calls inside `unifyTypes'` itself. Those
  represent structural traversal, not redundant external entry.
  Mixing them in hides the signal under recursion noise.
- Touching `Unify.hs` beyond what's needed (LESSONS: inlining
  sensitivity).
- Implementing any redundancy fix. That's the follow-up experiment
  the histogram tells us how to scope.
- Any timing measurement. Survey overhead doesn't matter; only
  counts do.

## Risks / things to watch

- **Bang-pattern not forced.** `TypeCheckM` is lazy `StateT`. The
  recording call goes via `unsafePerformIO` and must be tied into
  evaluation order with a strict `let !_ = ...` before the unify
  call. Verify by checking that disabling the gate produces zero
  counts and a clean histogram.
- **Call-site coverage incomplete.** If we miss a site, that site's
  hits fall through silently. Mitigation: grep for every external
  `unifyTypes` reference, tag each one with a unique string
  identifying file+function. Cross-reference against the grep output
  before running.
- **Hash-equality ≠ cache hit.** A pair can be hash-equal without
  ever having been unified before — but those are the same pairs the
  cache would short-circuit. The previous survey showed >99.9% of
  cache hits are hash-equal, and conversely that other buckets
  contribute negligibly. Treating hash-equal as the proxy is sound
  for ranking; small absolute discrepancies don't change which sites
  dominate.
- **Env-var gate cost.** Implemented as a `Bool` CAF read once at
  startup; hot path is one read.

## Plan

1. Create `UnifyCallSiteSurvey.hs` — Map String (Int,Int) of
   (hash_eq, total), env gate, dump.
2. Tag every external call site (Subsumption, Entailment, Types,
   Kinds, Roles, …) with a unique string.
3. Wire `dumpSurvey` into `Command.Compile`.
4. Run pr-admin clean with `PURS_UNIFY_CALLSITE_SURVEY=1`.
5. Drop histogram into `results.md`, decide the follow-up.

## Links

- Worktree: /workspace/p/unify-callsite-survey
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Precedent:
  - [unify-pattern-survey](../unify-pattern-survey/EXPERIMENT.md) —
    showed 99.9% of cache hits are hash-equal; this experiment asks
    where those hash-equal calls come from.
  - [skip-redundant-entailment-unify](../skip-redundant-entailment-unify/EXPERIMENT.md)
    — model for upstream redundancy elimination once a hot site is
    identified.
