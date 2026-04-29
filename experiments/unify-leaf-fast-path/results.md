# Results for unify-leaf-fast-path

Append-only. See experiments/SCHEMA.md for format.

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-29 | full     | 5713e832     | 5713e832 |     49.3 |     64.7 |  +31.3% | DISCARDED — head runs hit by parallel-worktree contention |
| 2026-04-29 | nochange | 5713e832     | 5713e832 |      0.6 |      0.7 |   +6.0% | DISCARDED — same window |
| 2026-04-29 | prelude  | 5713e832     | 5713e832 |      3.6 |      3.7 |   +0.8% | DISCARDED — same window |
| 2026-04-29 | leaf     | 5713e832     | 5713e832 |      1.5 |      1.5 |   -0.9% | DISCARDED — same window |
| 2026-04-29 | full     | 5713e832     | 5713e832 |     50.9 |     49.4 |   -2.9% | partially loaded, median of 6, 48708-53187 ms |
| 2026-04-29 | nochange | 5713e832     | 5713e832 |      0.6 |      0.6 |   +1.8% | partially loaded, median of 6, 561-602 ms |
| 2026-04-29 | prelude  | 5713e832     | 5713e832 |      3.7 |      3.9 |   +4.3% | partially loaded — noise; subsequent clean run shows -0.9% |
| 2026-04-29 | leaf     | 5713e832     | 5713e832 |      1.6 |      1.6 |   -1.9% | partially loaded, median of 6, 1543-1668 ms |
| 2026-04-29 | full     | 5713e832     | 5713e832 |     48.1 |     48.3 |   +0.4% | clean (load 3.2), median of 6, 47877-49752 ms |
| 2026-04-29 | nochange | 5713e832     | 5713e832 |      0.6 |      0.6 |   -1.2% | clean, median of 6, 542-600 ms |
| 2026-04-29 | prelude  | 5713e832     | 5713e832 |      3.9 |      3.9 |   -0.9% | clean, median of 6, 3823-4029 ms |
| 2026-04-29 | leaf     | 5713e832     | 5713e832 |      1.6 |      1.6 |   +2.2% | clean, median of 6, 1512-1695 ms |

## Headline (clean run, load avg 3.2)

| Scenario  | Baseline | Head     | Δ       |
| --------- | -------- | -------- | ------- |
| full      | 48.1 s   | 48.3 s   | **+0.4%** |
| nochange  | 0.57 s   | 0.56 s   | **-1.2%** |
| prelude   | 3.93 s   | 3.89 s   | **-0.9%** |
| leaf      | 1.58 s   | 1.62 s   | **+2.2%** |

All four scenarios are within ±2.2% — at or below the noise floor
on this hardware. The leaf fast-path is essentially neutral.

## Interpretation

### What we learned

1. **No catastrophic regression.** The prior 3 wrap-skip
   experiments (`skip-redundant-funapp-unify`,
   `unify-pattern-survey` Phase 2, `funapp-pattern-match`) all
   reproduced prelude +6.4–7.1% across multiple runs. The leaf
   fast-path doesn't reproduce that regression on a clean system
   — confirming the structural regression mechanism in those
   experiments was specifically about skipping the wrapper for
   whole call sites (regardless of leaf-equality), not about
   skipping unification work in general.

2. **No meaningful win either.** Eliminating 86% of cache hits
   (354,804 lookups per pr-admin compile) translates to no
   measurable wall-clock improvement. Consistent with the
   cost-centre observation that `unifyTypes` /
   `substituteType` / `withErrorMessageHint` all show 0.0%
   inherited time. The HashSet bookkeeping for 354k tiny entries
   is free; the work the fast-path bypasses is below the noise
   floor.

3. **Cache value lives in the remaining 14%.** Of 412,665 hits,
   57,861 are on pairs ≥3 nodes (3-10 / 11-50 / 51+ buckets).
   Whatever load-bearing work the cache does is concentrated
   there.

### What this implies for the cache

The cache's algorithmic value is on the ~58k non-leaf hits per
pr-admin compile (~5.5% of total lookups). Two follow-up
directions are now opened up:

- **`unify-leaf-fast-path` + drop cache entirely.** The original
  `unify-cache` drop-cache experiment cost +24% on full. With
  the leaf fast-path catching 86% of hits, the residual cost of
  dropping the cache should be much smaller. Worth re-measuring
  to find out how much.

- **`unify-leaf-fast-path` + small cache.** Replace the
  unbounded HashSet with a small bounded structure (LRU /
  ring buffer) sized for the ~58k non-leaf workload.

## Verdict

**No-win** as a standalone change. Doesn't regress, doesn't
improve. Useful as a characterization — confirms the cache's
load-bearing work is in the 14% non-leaf hits and that
wrap-skip regressions in prior experiments were structural to
whole-site skipping rather than an inherent cost of bypassing
unification work.
