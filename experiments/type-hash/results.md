# Results for type-hash

Append-only. See experiments/SCHEMA.md for format.

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-25 | full     | 799e8208     | 799e8208 |     57.3 |    118.6 | +107.1% | median of 4, 118514-119112 ms |
| 2026-04-25 | nochange | 799e8208     | 799e8208 |      0.6 |      0.8 |  +38.5% | median of 4, 776-847 ms |
| 2026-04-25 | prelude  | 799e8208     | 799e8208 |      3.9 |     10.8 | +179.8% | median of 4, 10736-15482 ms |
| 2026-04-25 | leaf     | 799e8208     | 799e8208 |      1.7 |      2.5 |  +52.6% | median of 4, 2462-20182 ms |
| 2026-04-25 | full     | 799e8208     | 799e8208 |     57.6 |     58.6 |   +1.8% | median of 4, 58231-58906 ms; after UNPACK+INLINE fix |
| 2026-04-25 | nochange | 799e8208     | 799e8208 |      0.6 |      0.6 |   -2.3% | median of 4, 552-596 ms; after UNPACK+INLINE fix |
| 2026-04-25 | prelude  | 799e8208     | 799e8208 |      4.0 |      4.0 |   +0.3% | median of 4, 3956-5477 ms; after UNPACK+INLINE fix |
| 2026-04-25 | leaf     | 799e8208     | 799e8208 |      1.6 |      1.6 |   +3.2% | median of 4, 1612-8728 ms; after UNPACK+INLINE fix |
| 2026-04-25 | full     | 799e8208     | c4001ef4 |     57.3 |    119.6 | +108.9% | median of 4, 119064-120767 ms; step 2: eqType hash short-circuit |
| 2026-04-25 | nochange | 799e8208     | c4001ef4 |      0.6 |      0.8 |  +42.8% | median of 4, 797-820 ms; step 2: eqType hash short-circuit |
| 2026-04-25 | prelude  | 799e8208     | c4001ef4 |      3.7 |      9.2 | +146.4% | median of 4, 9023-13794 ms; step 2: eqType hash short-circuit |
| 2026-04-25 | leaf     | 799e8208     | c4001ef4 |     68.7 |     88.7 |  +29.0% | median of 4, 86499-95531 ms; step 2: eqType hash short-circuit |
| 2026-04-25 | full     | 799e8208     | c4001ef4 |     83.9 |     86.6 |   +3.1% | median of 4, 60556-91617 ms; step 2 v2: typeHash direct accessor |
| 2026-04-25 | full     | 799e8208     | c4001ef4 |     57.5 |     58.9 |   +2.5% | median of 4, 58686-59342 ms; step 2 v3 clean: typeHash direct accessor (no parallel contention) |
| 2026-04-25 | full     | 799e8208     | c4001ef4 |     57.5 |    116.2 | +102.3% | median of 4, 116208-116804 ms; step 3: HashSet for unificationCache |
| 2026-04-25 | nochange | 799e8208     | c4001ef4 |      0.6 |      0.8 |  +40.3% | median of 4, 797-847 ms; step 3: HashSet for unificationCache |
| 2026-04-25 | prelude  | 799e8208     | c4001ef4 |      4.0 |     10.6 | +163.7% | median of 4, 10550-15210 ms; step 3: HashSet for unificationCache |
| 2026-04-25 | leaf     | 799e8208     | c4001ef4 |      1.6 |      2.5 |  +55.3% | median of 4, 2416-19977 ms; step 3: HashSet for unificationCache |
| 2026-04-25 | full     | 799e8208     | c4001ef4 |     57.3 |     47.7 |  -16.8% | median of 4, 47570-48769 ms; step 3 v2: HashSet + INLINE on Hashable Type |
| 2026-04-25 | full     | 799e8208     | c4001ef4 |     57.0 |     48.2 |  -15.4% | median of 4, 47515-49155 ms; step 3 v3: HashSet + INLINE Hashable, all scenarios |
| 2026-04-25 | nochange | 799e8208     | c4001ef4 |      0.6 |      0.6 |   -1.9% | median of 4, 564-594 ms; step 3 v3: HashSet + INLINE Hashable, all scenarios |
| 2026-04-25 | prelude  | 799e8208     | c4001ef4 |      4.0 |      4.0 |   -0.3% | median of 4, 3971-5326 ms; step 3 v3: HashSet + INLINE Hashable, all scenarios |
| 2026-04-25 | leaf     | 799e8208     | c4001ef4 |      1.6 |      1.6 |   -1.1% | median of 4, 1550-8576 ms; step 3 v3: HashSet + INLINE Hashable, all scenarios |
| 2026-04-25 | full     | 799e8208     | 43f6b613 |     67.2 |     50.5 |  -24.9% | post-cleanup (modifyFlags + tightened exports + uniform hashWithSalt). Head tight (49429-50884 ms); baseline elevated (range 64.4–68.4 s vs typical ~57 s) — concurrent profiling on host inflated baseline. Head-side numbers consistent with the prior -15.4% measurement; re-measure under clean conditions for a definitive before/after. |
