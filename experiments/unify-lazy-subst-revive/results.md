# Results for unify-lazy-subst-revive

Append-only. See experiments/SCHEMA.md for format.

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-30 | full     | 6e04203c     | 497d51f5 |     87.5 |    168.9 |  +92.9% | median of 4, 168741-172074 ms |
| 2026-04-30 | nochange | 6e04203c     | 497d51f5 |      0.5 |      0.6 |  +40.5% | median of 4, 636-2860 ms |
| 2026-04-30 | prelude  | 6e04203c     | 497d51f5 |      2.9 |      6.7 | +127.6% | median of 4, 6565-9054 ms |
| 2026-04-30 | leaf     | 6e04203c     | 497d51f5 |      1.6 |      2.9 |  +82.6% | median of 4, 2885-5277 ms |
| 2026-04-30 | full     | 6e04203c     | 497d51f5 |     85.0 |     75.6 |  -11.1% | median of 4, 73135-76335 ms; stack-clean rebuild; head binary 69.1 MB vs base 48.7 MB (size delta intrinsic, no perf regression) |
| 2026-04-30 | nochange | 6e04203c     | 497d51f5 |      0.5 |      0.5 |   -7.0% | median of 4, 454-459 ms; stack-clean rebuild; head binary 69.1 MB vs base 48.7 MB (size delta intrinsic, no perf regression) |
| 2026-04-30 | prelude  | 6e04203c     | 497d51f5 |      3.0 |      3.1 |   +2.9% | median of 4, 3061-5286 ms; stack-clean rebuild; head binary 69.1 MB vs base 48.7 MB (size delta intrinsic, no perf regression) |
| 2026-04-30 | leaf     | 6e04203c     | 497d51f5 |      1.6 |      1.6 |   -0.8% | median of 4, 1558-3992 ms; stack-clean rebuild; head binary 69.1 MB vs base 48.7 MB (size delta intrinsic, no perf regression) |
