# Results for lazy-subst-leaf-fastpath

Append-only. See experiments/SCHEMA.md for format.

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-30 | full     | 6e04203c     | febbdb09 |    157.6 |    108.5 |  -31.1% | INVALID — parallel worktree creation contention; baseline 2× slower than expected, variance >25% |
| 2026-04-30 | nochange | 6e04203c     | febbdb09 |      0.5 |      0.4 |   -9.0% | INVALID — parallel worktree creation contention |
| 2026-04-30 | prelude  | 6e04203c     | febbdb09 |      3.0 |      3.1 |   +3.2% | INVALID — parallel worktree creation contention |
| 2026-04-30 | leaf     | 6e04203c     | febbdb09 |      1.5 |      1.5 |   +0.4% | INVALID — parallel worktree creation contention |
| 2026-04-30 | full     | 6e04203c     | febbdb09 |    130.3 |     79.2 |  -39.2% | UNRELIABLE — shared-machine contention (load avg 2.75/7.53/9.66, 6 other Claude sessions); baseline still 50% slower than lazy-subst saw on same binary |
| 2026-04-30 | nochange | 6e04203c     | febbdb09 |      0.5 |      0.5 |   -1.7% | UNRELIABLE — shared-machine contention |
| 2026-04-30 | prelude  | 6e04203c     | febbdb09 |      2.9 |      3.0 |   +1.8% | UNRELIABLE — shared-machine contention |
| 2026-04-30 | leaf     | 6e04203c     | febbdb09 |      1.6 |      1.6 |   -1.7% | UNRELIABLE — shared-machine contention |
| 2026-04-30 | full     | 6e04203c     | febbdb09 |     89.7 |     78.6 |  -12.4% | median of 4, head 78065-93921 ms, base 87827-115801 ms |
| 2026-04-30 | nochange | 6e04203c     | febbdb09 |      0.5 |      0.5 |   -4.5% | median of 4, head 455-491 ms, base 475-2546 ms |
| 2026-04-30 | prelude  | 6e04203c     | febbdb09 |      3.0 |      3.1 |   +0.7% | median of 4, head 3050-3194 ms, base 2998-5444 ms |
| 2026-04-30 | leaf     | 6e04203c     | febbdb09 |      1.6 |      1.6 |   +2.2% | median of 4, head 1589-3817 ms, base 1610-4032 ms |
