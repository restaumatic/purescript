---
id: funapp-lineage-survey
status: in-progress
verdict: research
branch: funapp-lineage-survey
worktree: /workspace/p/funapp-lineage-survey
baseline_sha: 5713e832
head_sha: 5713e832
hypothesis: >
  We've established the unification cache is essentially optimal at what
  it does and that ptr-eq fast-path gives a partial win on raw throughput
  but regresses cascade rebuild scenarios. The remaining algorithmic
  question is upstream: WHY are 174k+ unifyTypes calls flowing into the
  3 hot sites in Types.hs (funAppHead / checkAbsArrow / checkArrayHead)
  on identical or near-identical inputs? If the calls cluster on a small
  set of distinct (t1_hash, t2_hash) pairs, we can engineer the algorithm
  not to make them — skipping the call entirely is strictly better than
  caching it. This experiment characterises the call distribution to
  find that lever.
headline_delta: tbd (research-only)
tags: [unification, callsites, characterization, lineage]
started: 2026-04-28
closed: null
---

# funapp-lineage-survey — characterise the upstream of redundant unifyTypes calls

## Context

`unify-callsite-survey` showed:
- 75% of cache hits at 3 sites in `Types.hs`.
- 99.4% / 97.8% / 97.0% hit rate at funAppHead / checkAbsArrow / checkArrayHead.

`skip-redundant-funapp-unify` showed that simply skipping these calls
upstream (`unless (eqType x const) $ unifyTypes x const`) regresses
prelude on the HashSet baseline. **The cache catches them efficiently;
upstream skips don't beat it.**

`ptr-eq-unify` showed that bypassing the unifyTypes wrapper overhead via
`reallyUnsafePtrEquality#` wins on full (-7.5%) but regresses prelude
(+4.8%) — and the regression is below cost-centre granularity (likely
code-gen / branch prediction effects on a hot path).

**The remaining question is algorithmic, not implementation-level.** If
all those redundant calls are essentially the SAME (t1, t2) pair
flowing through repeatedly, we should not be making the call at all —
the type structure already proves what we're asserting at consumption
time.

## What we want to know

For each of the 3 hot call sites, characterise the distribution of
inputs:

1. **How many distinct (t1, t2) pairs are seen?** If it's a handful,
   the calls are concentrated and the algorithmic fix is obvious. If
   it's millions, the cache is genuinely doing the right job.
2. **What are the top pairs by count?** What are the constructors and
   payloads?
3. **For the dominant pair, what is the calling context?** What
   declaration is being typechecked when this fires? Is it instance
   resolution, value binding, type-class dispatch?

## Method

Add survey instrumentation parallel to `UnifyCallSiteSurvey` but
keyed on `(callsite_tag, t1_hash, t2_hash)`:

- IORef-backed `Map (String, Int, Int) Int` counter.
- Env-gated on `PURS_FUNAPP_LINEAGE=1`.
- Hook the 3 hot sites in `Types.hs` to record before the `unifyTypes`
  call. Hash via `Hashable (Type a)` (which on this branch reads the
  cached `tfHash` — O(1)).
- Dump on shutdown: total entries, distinct (h1, h2) pairs per site,
  top 20 by count.

If a pair has very high count, follow up with a second pass that also
captures the constructor names of `t1` and `t2` for that pair, so we
can identify the types involved.

## Scope

In:
- New survey module
  `src/Language/PureScript/TypeChecker/FunAppLineage.hs`.
- Hooks in `src/Language/PureScript/TypeChecker/Types.hs` at the 3
  hot sites (and the dump on shutdown — wire into `Command.Compile`
  or the test harness).
- `stack test --fast` must still pass.

Out:
- Performance changes — this is research only.
- Wiring through richer per-call context (e.g., source span / module
  / declaration) in this first cut. If the distribution is concentrated,
  a phase 2 can add that targeted at the dominant pair.

## What success looks like

A histogram showing either:
- **Concentrated**: a few distinct (h1, h2) pairs account for the
  majority of the 174k calls. → algorithmic fix is to recognize those
  specific shapes and skip.
- **Diffuse**: hundreds of thousands of distinct pairs. → the cache
  earns its keep, no fast algorithmic win available.

## Links

- Worktree: /workspace/p/funapp-lineage-survey
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Precedent:
  - [unify-callsite-survey](../unify-callsite-survey/EXPERIMENT.md)
    — identified the 3 hot sites this experiment surveys further.
  - [ptr-eq-unify](../ptr-eq-unify/EXPERIMENT.md)
    — partial win shipped; this experiment seeks to find a clean win
    in the prelude regime ptr-eq-unify regressed.
