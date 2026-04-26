# Results for unify-cache

Append-only. See experiments/SCHEMA.md for format.

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-26 | full     | 43f6b613     | 0938d3b3 |     52.9 |    135.7 | +156.6% | median of 4, 134569-141236 ms |
| 2026-04-26 | nochange | 43f6b613     | 0938d3b3 |      0.6 |      0.8 |  +36.6% | median of 4, 787-835 ms |
| 2026-04-26 | prelude  | 43f6b613     | 0938d3b3 |      3.9 |     10.3 | +166.3% | median of 4, 10182-10912 ms |
| 2026-04-26 | leaf     | 43f6b613     | 0938d3b3 |      1.6 |      2.5 |  +53.0% | median of 4, 2452-2652 ms |
| 2026-04-26 | full     | 43f6b613     | 0938d3b3 |     50.4 |    120.6 | +139.5% | median of 4, 119026-140187 ms |
| 2026-04-26 | nochange | 43f6b613     | 0938d3b3 |      0.6 |      0.8 |  +46.4% | median of 4, 807-828 ms |
| 2026-04-26 | prelude  | 43f6b613     | 0938d3b3 |      3.9 |     10.7 | +172.0% | median of 4, 10697-10958 ms |
| 2026-04-26 | leaf     | 43f6b613     | 0938d3b3 |      1.6 |      2.5 |  +57.6% | median of 4, 2505-2650 ms |
| 2026-04-26 | full     | 43f6b613     | 0938d3b3 |     53.8 |    119.3 | +121.6% | median of 4, 119294-132122 ms |
| 2026-04-26 | nochange | 43f6b613     | 0938d3b3 |      0.6 |      0.9 |  +41.7% | median of 4, 826-947 ms |
| 2026-04-26 | prelude  | 43f6b613     | 0938d3b3 |      3.8 |      9.7 | +155.4% | median of 4, 9141-10400 ms |
| 2026-04-26 | leaf     | 43f6b613     | 0938d3b3 |      1.7 |      2.5 |  +53.0% | median of 4, 2463-2610 ms |

## Clean re-measurement (single-shot manual `time`, after instrumentation removed)

| Date       | Configuration                          | Full (s) | Δ vs 43f6b613 51.8s |
| ---------- | -------------------------------------- | -------- | ------------------- |
| 2026-04-26 | 43f6b613 baselines/ binary             | 49.3     | —                   |
| 2026-04-26 | Worktree at 43f6b613 (no diff)         | 51.8     | reference           |
| 2026-04-26 | Phase 2: cache dropped (1-line change) | 64.3     | **+24%**            |
| 2026-04-26 | Phase 3: UnifyKey newtype + custom Hashable | 55.9 | +8%                 |

**All earlier table rows above are CONTAMINATED** — instrumentation in
Unify.hs (IORef counters + `dumpUnifyCacheStats`) caused GHC to make
different inlining decisions even though the new code was never called
from the hot path. Binary shrunk from 49.1 MB → 46.7 MB and full-build
time inflated from ~50 s → ~120 s. See HANDOFF.md.
