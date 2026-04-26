# Handoff: skip-redundant-funapp-unify

## TL;DR

**Closed: no-win on both baselines, but for different reasons.**
The hypothesis (eliminate ~75% of unification-cache hits at the
three survey-identified concentrated sites in `Types.hs` with
`unless (eqType x const) $ unifyTypes x const` guards) is
falsified.

| Baseline | full | nochange | prelude | leaf | Verdict |
|---|---:|---:|---:|---:|---|
| 799e8208 (S.Set, pre-type-hash) | -0.0% | -1.9% | +2.3% | -1.6% | neutral |
| 43f6b613 (HashSet, post-type-hash) | -0.2% | -1.2% | **+6.4%** | +2.5% | regresses prelude |

Tests pass on both branches (1340/1340).

## Why both phases were needed

I baselined Phase 1 on `799e8208` (the current shipped tip), but
the survey that identified the 3 sites measured the cache cost on
the *post-type-hash* branch (`HashSet` keyed via `Hashable
(SourceType, SourceType)`, ~19% of compile time). On `799e8208`
the cache is `S.Set` ordered by structural compare — no Hashable
walk, lookups already O(1) on tiny constants, nothing to save.

Phase 2 re-runs the same 3-line patch on `43f6b613` (post-type-hash
HashSet cache) where the hypothesis is actually testable. There
the result is prelude +6.4% — the **same shape** as
`unify-pattern-survey` Phase 2 (typeHash + eqType + no cache:
prelude +7.1%).

## The mechanism on the HashSet baseline

Plausible: the prelude cascade rebuild typechecks 1,342 modules
against a long-lived substitution + cache. The same trivial pairs
(`tyFunction`, `tyFunction`) flow through repeatedly. The HashSet
catches them in one bucket walk per call. With my upstream eqType
skip, recursive `unifyTypes` calls (e.g. through
`unifyTypes' (TypeApp _ a b) (TypeApp _ a' b')`) miss the cache,
re-run unifyTypes' on the constants, and re-insert. Net more work,
but only on the cascade scenario where the same pairs flow many
times.

## The cross-experiment shape

`unify-pattern-survey` already showed eqType-based replacement of
the cache lookup regresses prelude (+7.1%). My experiment narrows
that finding: even at the 3 sites where the cache is *most*
exercised, the eqType-upstream-skip pattern still regresses
prelude. The cache plus Hashable infrastructure on the type-hash
branch is essentially optimal across all four scenarios — it's
not just amortising the cost of itself, it's earning real value
on prelude through pair re-use.

The `skip-redundant-entailment-unify` precedent (-15.5% full)
ships because the entailment fundep site has *unique*
redundancy (one call per dictionary, executed once). The funApp /
array / abs sites fire repeatedly across modules and benefit from
cache amortisation that a per-site eqType skip can't replicate.

## What's left

- Branch `skip-redundant-funapp-unify` (commit 15540bba) — Phase 1
  patch on the 799e8208 baseline.
- Branch `skip-redundant-funapp-unify-th` (commit 9ad7c523) —
  Phase 2 patch on the 43f6b613 baseline. **This is the one to
  cite as the falsifier**, since it's the post-type-hash branch
  where the hypothesis was supposed to apply.
- Both branches parked. Don't merge either.
- Don't repeat this experiment on top of any
  HashSet-cache baseline; the prelude regression is structural.

## Lessons captured

In `experiments/LESSONS.md`:
1. **"Survey vs ship is two different questions"** — added to the
   `unify-callsite-survey` lesson. "75% of hits" doesn't imply
   "75% of cost" when the hits are on tiny pairs.
2. **`skip-redundant-funapp-unify` lesson rewritten** to cover
   both baselines and the prelude-regression mechanism.
3. **Stack incremental builds can produce slow binaries** — first
   benchmark on Phase 1 saw +74%/+164% from a stale incremental
   build that produced a 2 MB-smaller binary; `stack clean` fixed
   it. Always clean before benchmarking when results look
   implausible.

## Implications for next experiment

- Don't pursue more eqType-guard sites at hot recurring call points.
- The cache earns its keep on the post-type-hash branch precisely
  because the prelude cascade re-uses pairs; cache-replacement
  schemes have to handle that re-use or they regress prelude.
- Largest unattacked hotspot per README: `compare` (Qualified a)
  at ~20%. That's where to look next.
