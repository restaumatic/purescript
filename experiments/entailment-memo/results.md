# Results: entailment-memo

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes                              |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ---------------------------------- |
| 2026-04-16 | full     | ebb0a6bb     | 953c9149 | 74.1     | 62.6     | -15.5%  | median of 3, clean -O2 build       |
| 2026-04-16 | nochange | ebb0a6bb     | 953c9149 | 1.24     | 1.29     | +4.0%   | median of 2, within noise          |
| 2026-04-16 | prelude  | ebb0a6bb     | 953c9149 | 5.25     | 5.44     | +3.6%   | median of 3, comment change        |
| 2026-04-16 | leaf     | ebb0a6bb     | 953c9149 | 2.03     | 2.00     | -1.5%   | median of 3, comment change        |
