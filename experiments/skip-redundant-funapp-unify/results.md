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

## Verdict

**No-win.** Eliminating ~75% of unification-cache hits at the three
concentrated sites (`funAppHead`, `checkAbsArrow`, `checkArrayHead`)
produces no measurable speed change on any of the four scenarios.

### Why no win, despite survey suggesting one

The dominant pairs at these sites are tiny constants —
`(tyFunction, tyFunction)` is a 1-node-vs-1-node `TypeConstructor`
comparison. The cache (`S.Set` on baseline 799e8208, ordered by
structural compare) handles these in O(log n) tree-depth × O(1)
per compare. For ~M-pair caches, log₂(M) ≈ 20 root-comparisons,
each terminating immediately on the constructor tag.

The `eqType` call I substitute is functionally identical work: one
constructor-tag comparison. Trading `S.member`'s 20 root-compares
for one `eqType` saves microseconds per call × 212K calls — a
small absolute cost, dwarfed by per-decl typecheck work elsewhere.

The `unify-pattern-survey` lesson — that the cache is essentially
optimal *for what it does* — held. Even removing 75% of its hits
upstream doesn't reveal a hidden cost, because there isn't one to
reveal: the cache lookups on these sites were already ~free.

### Implication for follow-ups

- **Don't pursue more eqType-guard sites for cache reduction.** The
  remaining cache traffic (Subsumption:default at 17.6% of hits,
  etc.) is on larger types where the cache is more useful and an
  eqType short-circuit would also be more expensive (longer walks).
- **Cache structure remains essentially optimal at this scale.**
  unify-pattern-survey + this experiment together close the
  "reduce cache cost upstream" path — there's no win available.
- **Look at unattacked hotspots.** `compare` (Qualified a) is still
  the largest unattacked cost centre at ~20% per the README. That's
  where to spend the next experiment.

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
