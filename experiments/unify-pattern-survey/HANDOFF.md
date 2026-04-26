# Handoff: unify-pattern-survey

## TL;DR

**Phase 1 (characterization): clear win as a research tool.**
The survey histogram showed that **99.9% of unification-cache hits
are hash-equal pairs** (412,311 of 412,727 on a full pr-admin
compile). The cache is, in effect, a memoizer for "we already
unified this hash-equal pair."

**Phases 2–4 (optimization): no-win for shipping.** Three
replacement schemes were tried; all either regress a scenario or
trade correctness for marginal speed:

| Phase | Scheme | full | nochange | prelude | leaf | Soundness |
|-------|--------|-----:|---------:|--------:|-----:|-----------|
| Baseline (43f6b613) | HashSet (SourceType, SourceType) | — | — | — | — | sound (Eq fallback on hash collision) |
| Phase 2 | `typeHash ==` + `eqType` shortcut, no cache | -2.8% | -2.0% | **+7.1%** | -0.3% | sound |
| Phase 3 | IntSet of `hashWithSalt h1 h2`, cache kept | +0.1% | +0.4% | +0.7% | -3.3% | unsound (hash collision → false skip) |
| Phase 4 | `typeHash ==` only, no cache, no eqType | -0.9% | -0.7% | -0.6% | -2.8% | unsound (same as Phase 3) |

Tests pass on phase 4 (1340 examples, 0 failures), so the
collision risk is empirically zero on the existing test suite.
But the risk is real: a hash collision between two
structurally-distinct types would silently skip a unification that
should have raised a type error.

The original cache uses `HS.member (t1, t2) cache`, which on hash
collision falls back to `Eq` (which calls `eqType` walk).
Soundness is preserved at the cost of the rare-collision walk.
Phase 4 / Phase 3 throw away that fallback. For a compiler used
broadly, that's not an acceptable trade.

## What's left in the worktree

The branch sits at Phase 4 (commit `39f77167`). To restore the
baseline cache, revert `39f77167`, `5a5aa681`, `bceea567` (keep
phase 1 commit `4508908d` for the survey module).

The survey module (`UnifyPatternSurvey.hs`) was added in phase 1
and is still on the branch but not wired in. Removing it would
require reverting from phase 1 too. Recommend keeping it
side-by-side as a characterization tool — re-enable by re-adding
the import + call in `Unify.hs` and the dump call in
`Command.Compile`, then run with `PURS_UNIFY_SURVEY=1`.

## The big takeaway

**The cache is essentially optimal for its current job.** It's a
memoizer for "already unified this pair," and 99.9% of its hits
are hash-equal pairs that any cheaper scheme would have to either
walk (`eqType`, regresses prelude) or trust (`hashWithSalt`,
soundness-broken).

The ~19% Hashable infrastructure cost the type-hash profile
flagged is the unavoidable cost of doing pair memoization soundly.
Reducing it requires either:
- A fundamentally different structure (small-LRU with cheap
  eviction; bloom-filter front-end so most lookups skip the
  HashSet entirely; per-module reset to keep cache small).
- Or eliminating the redundant unification calls upstream (so
  the cache catches less and matters less).

Both are bigger-scope efforts than this experiment scoped.

## Phase results (clean measurements)

Baseline 43f6b613 (post-type-hash cleanup). Each scenario is
median of 4 (warm-up run discarded). Binary size 49.08–49.12 MB
across all phases — no inlining contamination.

## Open follow-ups (separate experiments)

- **Smaller cache structure.** A bounded LRU keyed by mixed hash,
  evicted at ~64K entries, might catch the hot pairs at lower
  membership cost. Speculative.
- **Bloom filter front-end.** Cheap rejection of negative
  lookups. Adds work to the positive path. Probably a wash.
- **Per-module reset.** The cache currently grows unbounded
  through a typecheck. Resetting per module trades hit rate for
  lookup speed.
- **Upstream redundancy reduction.** Skip-redundant-entailment-unify
  already attacked one source. Profiling could find more — e.g.,
  in row unification or instance resolution.

## Why we're closing as no-win

Phase 1 produced a useful insight ("cache is a hash-equal
memoizer") which earned its place in the lessons file. But none
of the proposed optimizations (Phases 2–4) shipped a clean win:
- Phase 2 regresses prelude.
- Phases 3–4 trade soundness for marginal speed.

The original cache is good enough; further wins here would need
a fundamentally different approach.
