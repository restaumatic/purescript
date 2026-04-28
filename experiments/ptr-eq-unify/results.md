# Results for ptr-eq-unify

Append-only. See experiments/SCHEMA.md for format.

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-28 | full     | 5713e832     | b9fcf10c |     53.1 |     49.1 |   -7.5% | median of 4, 48737-49986 ms |
| 2026-04-28 | nochange | 5713e832     | b9fcf10c |      0.6 |      0.6 |   -3.6% | median of 4, 561-604 ms |
| 2026-04-28 | prelude  | 5713e832     | b9fcf10c |      4.0 |      4.1 |   +4.8% | median of 4, 4088-4175 ms |
| 2026-04-28 | leaf     | 5713e832     | b9fcf10c |      1.6 |      1.6 |   +3.4% | median of 4, 1610-1666 ms |
| 2026-04-28 | full     | 5713e832     | ed1f0cdc |     47.7 |     48.4 |   +1.5% | median of 4, 47847-48761 ms |
| 2026-04-28 | nochange | 5713e832     | ed1f0cdc |      0.6 |      0.6 |   -1.4% | median of 4, 548-593 ms |
| 2026-04-28 | prelude  | 5713e832     | ed1f0cdc |      3.9 |      4.1 |   +4.7% | median of 4, 4074-4189 ms |
| 2026-04-28 | leaf     | 5713e832     | ed1f0cdc |      1.6 |      1.6 |   -0.1% | median of 4, 1577-1664 ms |
| 2026-04-28 | prelude  | 5713e832     | ed1f0cdc |      0.0 |      0.0 |   +0.0% | median of 2, - ms; phase 2 cost-centre profile |
