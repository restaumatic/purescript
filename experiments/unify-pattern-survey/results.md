# Results for unify-pattern-survey

Append-only. See experiments/SCHEMA.md for format.

## Phase 1 — characterization (counts, no timing)

Run: pr-admin full clean build, `PURS_UNIFY_SURVEY=1`, head SHA at
phase-1 commit (instrumentation only). Single run. Binary 49.1 MB
(same as 43f6b613 baseline — no inlining contamination).

```
=== unify-pattern-survey ===
total lookups:         1,049,412
hits (in cache):         412,727  ( 39.3%)
misses (added):          636,685  ( 60.7%)

by bucket  (hits / misses / total / hit% / share-of-all-hits):
  hash_eq                412,311 /  77,866 / 490,177    84.1%   99.9%
  root_tunknown_both           0 /  82,551 /  82,551     0.0%    0.0%
  root_tunknown_one            0 / 298,236 / 298,236     0.0%    0.0%
  has_wildcard                 0 /       0 /       0     0.0%    0.0%
  has_unsynonymed            416 / 178,032 / 178,448     0.2%    0.1%
  concrete_synonym_free        0 /       0 /       0     0.0%    0.0%
```

### Interpretation

**99.9% of all cache hits are hash-equal pairs.** The unification
cache is, in effect, a memoizer for "we already unified this
hash-equal pair." Only 416 of 412,727 hits (0.1%) come from
hash-distinct pairs — vanishingly small.

Implications:
- The HashSet's lifted-tuple hashing + chain walk is paying for
  membership of pairs that could be detected by a single int
  comparison: `typeHash t1 == typeHash t2`.
- A cheaper alternative: short-circuit `unifyTypes` when hashes
  are equal, without consulting any cache. Optionally fall back
  to `eqType` to avoid hash-collision false positives (collision
  probability with 64-bit hash on ~M-pair workload is ~10⁻⁷).
- The other 0.1% (416 hits in `has_unsynonymed`) is too small to
  motivate keeping the cache.

Phase 2: implement the hash-equality short-circuit, drop the
HashSet, measure all four scenarios.
| 2026-04-26 | full     | 43f6b613     | bceea567 |     49.9 |     48.5 |   -2.8% | median of 4, 48470-49499 ms |
| 2026-04-26 | nochange | 43f6b613     | bceea567 |      0.6 |      0.6 |   -2.0% | median of 4, 563-591 ms |
| 2026-04-26 | prelude  | 43f6b613     | bceea567 |      3.9 |      4.2 |   +7.1% | median of 4, 4139-4288 ms |
| 2026-04-26 | leaf     | 43f6b613     | bceea567 |      1.6 |      1.6 |   -0.3% | median of 4, 1571-1686 ms |
| 2026-04-26 | full     | 43f6b613     | 5a5aa681 |     48.7 |     48.7 |   +0.1% | median of 4, 48291-49656 ms |
| 2026-04-26 | nochange | 43f6b613     | 5a5aa681 |      0.6 |      0.6 |   +0.4% | median of 4, 555-585 ms |
| 2026-04-26 | prelude  | 43f6b613     | 5a5aa681 |      4.0 |      4.0 |   +0.7% | median of 4, 3956-4020 ms |
| 2026-04-26 | leaf     | 43f6b613     | 5a5aa681 |      1.7 |      1.6 |   -3.3% | median of 4, 1528-1696 ms |
| 2026-04-26 | full     | 43f6b613     | 39f77167 |     48.4 |     48.0 |   -0.9% | median of 4, 47949-48524 ms |
| 2026-04-26 | nochange | 43f6b613     | 39f77167 |      0.6 |      0.6 |   -0.7% | median of 4, 542-584 ms |
| 2026-04-26 | prelude  | 43f6b613     | 39f77167 |      3.9 |      3.9 |   -0.6% | median of 4, 3812-3994 ms |
| 2026-04-26 | leaf     | 43f6b613     | 39f77167 |      1.6 |      1.6 |   -2.8% | median of 4, 1509-1648 ms |

## Verdict

| Phase | head SHA  | Scheme                                 | Soundness | Headline           |
|-------|-----------|----------------------------------------|-----------|--------------------|
| 1     | 4508908d  | survey instrumentation only            | n/a       | 99.9% of hits hash-equal |
| 2     | bceea567  | typeHash + eqType, no cache            | sound     | full -2.8%, prelude **+7.1%** |
| 3     | 5a5aa681  | IntSet of mixed hashes, cache kept     | unsound\* | neutral on all scenarios |
| 4     | 39f77167  | typeHash only, no cache, no eqType     | unsound\* | full -0.9%, leaf -2.8%, others within noise |

\* unsound = hash-collision false positives can silently skip a real
type error. Probability ≈ 10⁻¹³ per pair on this workload — but the
existing cache uses HashSet's Eq fallback to be sound at any
collision rate.

Closing as **no-win for shipping.** Phase 1 produced the useful
research finding ("cache is a hash-equal memoizer"); Phases 2–4
all either regress a scenario (Phase 2 prelude) or trade soundness
for marginal speed (Phases 3–4). The original cache with HashSet +
Eq fallback is essentially optimal for what the cache does — the
~19% Hashable cost is the unavoidable price of sound pair
memoization at its current discipline.

`stack test --fast` passes on Phase 4 (1340 examples, 0 failures)
— so the soundness risk is empirically zero on the existing test
suite, but real in principle.

