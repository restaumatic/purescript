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

## Phase 1 verdict

**No-win** as a standalone change. Doesn't regress, doesn't
improve. Useful as a characterization — confirms the cache's
load-bearing work is in the 14% non-leaf hits and that
wrap-skip regressions in prior experiments were structural to
whole-site skipping rather than an inherent cost of bypassing
unification work.

## Phase 2: Leaf fast-path + cache dropped

Followed up by removing the HashSet `unificationCache` lookup
entirely — the catch-all `unifyTypes` now calls `unifyTypes'`
directly through `substituteType`/`withErrorMessageHint`
without going through `unifyTypes''`. The `unificationCache`
field stays in `CheckState` but is read/written nowhere.

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-29 | full     | 5713e832     | f37a6062 |     48.0 |     47.9 |   -0.3% | median of 6, 47447-49064 ms |
| 2026-04-29 | nochange | 5713e832     | f37a6062 |      0.6 |      0.6 |   -5.5% | median of 6, 548-582 ms |
| 2026-04-29 | prelude  | 5713e832     | f37a6062 |      3.9 |      4.1 |   +3.4% | median of 6, 4002-4183 ms |
| 2026-04-29 | leaf     | 5713e832     | f37a6062 |      1.6 |      1.6 |   +2.5% | median of 6, 1557-1704 ms |

(`f37a6062` is the leaf-fast-path commit; the cache-drop edit
sits on top of it as a separate commit recorded after this run.)

### Headline (clean run, load avg 1.6)

| Scenario  | Baseline | Head     | Δ       |
| --------- | -------- | -------- | ------- |
| full      | 48.0 s   | 47.9 s   | **-0.3%** |
| nochange  | 0.58 s   | 0.55 s   | **-5.5%** |
| prelude   | 3.93 s   | 4.06 s   | **+3.4%** |
| leaf      | 1.57 s   | 1.61 s   | **+2.5%** |

### What dropping the cache costs (after leaf fast-path)

The original `unify-cache` drop-cache experiment (no leaf
fast-path) cost **+24%** on full. With the leaf fast-path
catching 86% of cache hits upstream, dropping the cache costs:

- **-0.3% on full** — the leaf fast-path absorbs essentially
  all of the cache's full-build value. The 14% "non-leaf
  hits" the cache catches are not worth the bookkeeping on
  the from-scratch path.
- **-5.5% on nochange** — actual win. Without per-module
  cache state to allocate / GC, the no-op rebuild path is
  measurably faster.
- **+3.4% on prelude** — the cache's residual algorithmic
  value is concentrated here. The prelude cascade (1342
  modules) re-typechecks the same pairs against shared
  externs; the HashSet was amortising those hits across
  modules in a way the leaf fast-path doesn't (since each
  TypeApp recursion still re-walks structure).
- **+2.5% on leaf** — within noise.

### What this tells us

1. **The leaf fast-path captures essentially all of the
   cache's full-build value.** +24% (cache-drop alone) →
   -0.3% (leaf fast-path + cache-drop) on full. The 14% non-
   leaf hits the cache caught aren't the load-bearing thing
   for from-scratch builds.

2. **The cache's remaining value is on prelude only.** +3.4%
   is the "amortise across the cascade" property described
   in the `skip-redundant-funapp-unify` lesson: the same
   trivial pairs flow through 1342 dependent modules and the
   HashSet catches them in one bucket walk per pair per
   module. Without it, the recursive descent re-walks each
   TypeApp subtree.

3. **The wrap-skip prelude +7% mechanism is bigger than the
   cache itself.** Dropping the cache entirely costs +3.4%
   on prelude. The prior 3 wrap-skip experiments hit
   +6.4–7.1% prelude *while still using the cache*. So the
   wrap-skip regression isn't (only) about losing cache
   amortisation — it's about how skipping the wrapper at
   whole call sites interacts with the rest of the
   unification machinery in a way that's roughly twice as
   expensive as just removing the cache.

## Final verdict

**Phase 1** (leaf fast-path alone): **no-win, neutral.**

**Phase 2** (leaf fast-path + cache dropped): **partial.**
Neutral on full, win on nochange, regression on prelude. Net
**no-ship** because the prelude regression isn't compensated
by gains elsewhere — but the experiment characterises that
the cache's full-build value is fully replaceable by the leaf
fast-path, and the only residual cache value is cross-module
amortisation on the prelude cascade.

**Best follow-up direction:** a small per-module cache (LRU
/ ring buffer / size-bounded HashSet) that retains the
cascade-amortisation property without unbounded growth —
combined with the leaf fast-path that already catches the
bulk of trivial recurrences. Tracked as a future experiment.
