# Results for skip-redundant-funapp-unify

Append-only. See experiments/SCHEMA.md for format.

## Discarded — incremental-build artefact

The first run produced apparent regressions (full +74%, prelude
+164%, etc.) with the head binary at 46.6 MB — 2 MB *smaller* than
baseline. A `stack clean` + rebuild produced a 48.6 MB binary
(matching baseline) with neutral timings. The earlier numbers were
caused by stale incremental compilation artefacts inside
`.stack-work/dist`, not by the eqType guard change itself. Always
`stack clean` before benchmarking when debugging perf-suspicious
results.

Discarded run kept here as a forensic record:

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ (discarded) | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------------- | ----- |
| 2026-04-26 | full     | 799e8208     | 799e8208 |     57.7 |    100.4 |  +74.1%       | DISCARDED — incremental build artefact |
| 2026-04-26 | nochange | 799e8208     | 799e8208 |      0.6 |      0.8 |  +39.4%       | DISCARDED — incremental build artefact |
| 2026-04-26 | prelude  | 799e8208     | 799e8208 |      4.0 |     10.6 | +164.2%       | DISCARDED — incremental build artefact |
| 2026-04-26 | leaf     | 799e8208     | 799e8208 |      1.6 |      2.4 |  +51.5%       | DISCARDED — incremental build artefact |

## Real measurements (after stack clean rebuild)

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-26 | full     | 799e8208     | 15540bba |     57.1 |     57.1 |   -0.0% | median of 4, 56351-57356 ms |
| 2026-04-26 | nochange | 799e8208     | 15540bba |      0.6 |      0.6 |   -1.9% | median of 4, 560-579 ms |
| 2026-04-26 | prelude  | 799e8208     | 15540bba |      3.9 |      4.0 |   +2.3% | median of 4, 4014-4086 ms |
| 2026-04-26 | leaf     | 799e8208     | 15540bba |      1.6 |      1.6 |   -1.6% | median of 4, 1536-1748 ms |

All four scenarios within ±2.3% — within harness noise. Tests pass
(1340/1340). Binary size 48,622,080 bytes vs baseline 48,617,920 —
+4 KB, no inlining shift.

### Phase 1 reading (incomplete)

Phase 1 alone — neutral on all four scenarios — looked like a clean
no-win, with a tidy explanation: the cache lookups at these sites
are already O(1) on `S.Set`, so eqType saves nothing. **That
reading was incomplete.** The hypothesis came from
unify-pattern-survey's measurement of the *post-type-hash* cache
(HashSet keyed via Hashable, ~19% of compile time), but I baselined
on `799e8208` which still has the pre-type-hash `S.Set`. The "75%
of cache hits" survey was on a different cache implementation than
the experiment baseline. Phase 2 below uses the right baseline.

## Process lesson — incremental builds can mislead

The very first benchmark run produced large apparent regressions:

| Scenario | Δ (incremental) | Δ (clean rebuild) |
|---|---:|---:|
| full | +74.1% | -0.0% |
| prelude | +164.2% | +2.3% |
| leaf | +51.5% | -1.6% |
| nochange | +39.4% | -1.9% |

The incremental binary was 46.6 MB; the clean-rebuild binary
matched baseline at 48.6 MB. Stack's incremental build kept stale
optimisation artefacts under `.stack-work/dist/` that produced a
slower binary than a clean build of the same source. Adding
`stack clean` before the build fixed it.

**Always `stack clean` before benchmarking** when results look
implausible — and binary size is a primary signal: a >1 MB delta
without proportional source change means GHC compiled differently,
making timing comparisons invalid.

## Phase 2 — re-baselined on 43f6b613 (post-type-hash)

The Phase 1 measurement above was on the wrong baseline. The
hypothesis came from `unify-pattern-survey` LESSONS, which measured
the cache as ~19% of compile time on the **type-hash branch**
(`HashSet (SourceType, SourceType)` with a Hashable instance that
walks the type structure). Baseline `799e8208` is *pre-type-hash*
and uses `S.Set` ordered by structural compare — no Hashable cost.
On that baseline the cache lookups on tiny pairs are already
essentially free, so the eqType guards have nothing to save.

To actually test the original hypothesis, re-run on `43f6b613`
where the HashSet cache is in place.

### Phase 2 measurements

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-26 | full     | 43f6b613     | 9ad7c523 |     48.3 |     48.3 |   -0.2% | median of 4, 47953-48491 ms |
| 2026-04-26 | nochange | 43f6b613     | 9ad7c523 |      0.6 |      0.6 |   -1.2% | median of 4, 571-587 ms |
| 2026-04-26 | prelude  | 43f6b613     | 9ad7c523 |      4.0 |      4.2 |   +6.4% | median of 4, 4155-4262 ms |
| 2026-04-26 | leaf     | 43f6b613     | 9ad7c523 |      1.6 |      1.7 |   +2.5% | median of 4, 1635-1736 ms |

Binary 49,106,272 bytes vs baseline 49,106,176 — +96 B, no
inlining shift.

### Phase 2 verdict: **regresses prelude**

The eqType guards on `43f6b613` (HashSet cache) produce **+6.4% on
prelude** — same shape as `unify-pattern-survey` Phase 2 (typeHash
+ eqType + no cache: +7.1% prelude). Both `full` and `nochange` are
neutral; `leaf` is +2.5% (right at noise floor).

The shape `prelude regresses, others don't` recurs across two
independent experiments now. The plausible mechanism: the prelude
cascade rebuild typechecks 1,342 modules against a long-lived
substitution + cache. Each module re-asks the same trivial
"function applied to function" pairs. The cache catches them all
in one HashSet hit. Replacing that hit with an eqType +
"don't insert" path means subsequent recursive `unifyTypes
tyFunction tyFunction` calls (from `unifyTypes' (TypeApp _ a b)
(TypeApp _ a' b')` traversal) miss the cache, run the full
unifyTypes' path, and re-insert. Net: more work overall, but only
on the cascade-heavy scenario where the same pairs flow through
many times.

### Combined verdict (both baselines)

| Baseline | full | nochange | prelude | leaf | Verdict |
|---|---:|---:|---:|---:|---|
| 799e8208 (S.Set) | -0.0% | -1.9% | +2.3% | -1.6% | neutral |
| 43f6b613 (HashSet) | -0.2% | -1.2% | **+6.4%** | +2.5% | regresses prelude |

The hypothesis (eliminate 75% of cache hits → meaningful win) is
**falsified on both baselines**:
- On S.Set the cache lookup is already O(1) on tiny constants —
  nothing to save.
- On HashSet the cache is doing real work, but skipping it
  upstream regresses prelude — the cascade rebuild scenario
  re-uses the same hits enough that catching them in the HashSet
  is cheaper than re-walking via eqType + cache miss.

This **closes the "skip redundant unifyTypes calls upstream" path**
for the post-type-hash branch — the cache plus Hashable is
essentially optimal across all scenarios, and the upstream-skip
pattern that worked at the entailment fundep site (the precedent
for this experiment) does not generalise to the funApp / array /
abs sites.

## Final verdict

**No-win on both baselines.** The eqType-guard pattern from
skip-redundant-entailment-unify is site-specific — it ships when
the call site has *unique* redundancy (one entailment-fundep call
per dictionary, executed once), but the funApp / array / abs sites
fire repeatedly across modules and benefit from the cache's
amortisation. Removing the cache hits at those sites either does
nothing (S.Set baseline, lookup already cheap) or actively hurts
prelude (HashSet baseline, cascade re-uses the cache).
