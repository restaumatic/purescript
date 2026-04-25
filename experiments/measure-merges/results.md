# Results for measure-merges

Append-only. See experiments/SCHEMA.md for format.

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-23 | full     | e0125163     | f7cf7747 |     73.2 |     56.4 |  -22.9% | median of 4, 56308-56783 ms |
| 2026-04-23 | nochange | e0125163     | f7cf7747 |      0.6 |      0.6 |   -2.5% | median of 4, 550-585 ms |
| 2026-04-23 | prelude  | e0125163     | f7cf7747 |      3.8 |      3.7 |   -2.1% | median of 4, 3649-5081 ms |
| 2026-04-23 | leaf     | e0125163     | f7cf7747 |      1.7 |      1.5 |   -9.4% | median of 4, 1478-9238 ms |
