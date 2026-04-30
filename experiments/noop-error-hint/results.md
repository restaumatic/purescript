# Results for noop-error-hint

Append-only. See experiments/SCHEMA.md for format.

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-30 | full     | 6e04203c     | 25249c0b |    252.1 |    287.1 |  +13.9% | CORRUPTED — harness emitted "head 4-4 ms" min/max despite 287s median; rerun needed; note machine load was high (baselines 2-3x slower than fresh) |
| 2026-04-30 | nochange | 6e04203c     | 25249c0b |      0.9 |      0.9 |   +2.8% | median of 4, head 901-2698 ms, base 883-1455 ms; abs times slow (machine load) but base/head interleaved so delta is meaningful |
| 2026-04-30 | prelude  | 6e04203c     | 25249c0b |      9.4 |      9.7 |   +3.0% | median of 4, head 9562-12096 ms, base 9320-10025 ms; abs times slow (machine load) but interleaved |
| 2026-04-30 | leaf     | 6e04203c     | 25249c0b |      5.0 |      5.0 |   +0.1% | median of 4, head 4796-5105 ms, base 4899-5179 ms; abs times slow (machine load) but interleaved |
