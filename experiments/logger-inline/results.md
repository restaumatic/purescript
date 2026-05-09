# Results for logger-inline

## Headline (verdict: no-win)

Two runs against `b831b298` (traversal-inline tip):

| Scenario | Run 1 (load 2.27) | Run 2 (load 5.72) | Combined verdict |
| -------- | ----------------: | ----------------: | ---------------- |
| full     | -1.1%             | **+0.9%**         | sign flipped — noise |
| nochange | -3.4%             | -1.5%             | small win or noise (~10ms absolute on ~600ms wallclock) |
| prelude  | +1.5%             | -0.2%             | sign flipped — noise |
| leaf     | -0.4%             | **+1.8%**         | sign flipped — noise |

**Verdict: no-win.** Signs flip between runs on three of four
scenarios. The nochange wins look most plausible (-3.4% then -1.5%,
both negative) but are a small absolute delta on a small wallclock
— consistent with a tiny startup overhead reduction or measurement
noise.

## Why no win

**Binary diff: same size, different bytes.** Both binaries are
48,839,456 bytes. MD5s differ (head `2f159d9b...`, base `608cbbd8...`),
so GHC did make some different inlining decisions. But the net code
size is unchanged — meaning whatever inlining the INLINE pragmas
did unlock was offset by code that *was* being inlined no longer
being inlined elsewhere. A wash.

**The 1.3% Logger SCC was likely a profile-build artifact.** With
`+RTS -p` enabled, GHC inserts SCC annotations around top-level
expressions and on the Monad instance methods specifically. SCCs
suppress some optimisations and prevent inlining, so the cost-
centre profile shows time spent in `>>=.\` that doesn't exist in
optimised non-profile builds. Adding INLINE doesn't help the
non-profile build because GHC was already inlining as much as it
should.

**This is the same lesson as `error-helpers-inline`.** Pragma
sprays on already-cheap class-method wrappers aren't a win; the
profile cost-centre %time on them is misleading. The traversal-
inline pattern works because it removes runtime polymorphic
dispatch on a recursive bind chain — there's actual work to
collapse. One-shot wrapper methods (in Logger or Errors.hs) have
no chain to collapse.

## Per-round detail

### Run 1 (load 2.27)

```
=== full === median Δ=-1.1%
  round 2: base=42589 head=42110 ms (Δ -1.1%)
  round 3: base=42780 head=41435 ms (Δ -3.1%)
  round 4: base=42583 head=42650 ms (Δ +0.2%)
  round 5: base=46019 head=43458 ms (Δ -5.6%)

=== nochange === median Δ=-3.4%
  round 2: base=559 head=574 ms (Δ +2.7%)
  round 3: base=596 head=574 ms (Δ -3.7%)
  round 4: base=594 head=583 ms (Δ -1.9%)
  round 5: base=607 head=584 ms (Δ -3.8%)
```

### Run 2 (load 5.72)

```
=== full === median Δ=+0.9%
  round 2: base=43494 head=43048 ms (Δ -1.0%)
  round 3: base=46158 head=44768 ms (Δ -3.0%)
  round 4: base=44389 head=47517 ms (Δ +7.0%)
  round 5: base=47362 head=47375 ms (Δ +0.0%)

=== nochange === median Δ=-1.5%
  round 2: base=634 head=586 ms (Δ -7.6%)
  round 3: base=591 head=633 ms (Δ +7.1%)
  round 4: base=610 head=584 ms (Δ -4.3%)
  round 5: base=595 head=594 ms (Δ -0.2%)
```

The full-scenario per-round range in Run 2 (-3.0% to +7.0%) and
nochange's per-round range (-7.6% to +7.1%) confirm the noise
floor on this hardware is ±5-7% for these scenarios. Any "win"
of less than ~3% should be assumed to be noise until shown
otherwise.

## What this experiment rules out

- **`{-# INLINE #-}` on `Logger`'s Monad/Applicative/Functor/
  MonadIO/MonadWriter methods does not pay.** The cost-centre
  attributing 1.3% to `>>=.\` (Logger.hs:36) is an SCC artifact;
  optimised builds already inline these methods adequately.
- **The "INLINE on monad newtype methods" generalisation of the
  traversal-inline pattern doesn't transfer.** Same shape of
  failure as `error-helpers-inline`: the helpers are too small/
  trivial for the specialiser to gain anything from forcing
  inlining.
- **Don't trust profile-build cost-centre %time alone — verify
  the optimised binary actually changed shape.** A flat binary-
  size-delta paired with cost-centre presence usually means the
  centre exists only because of profiling instrumentation.

## Source diff (preserved on branch, not merged)

10 single-line `{-# INLINE #-}` pragmas on the `Logger` instance
methods in `src/Control/Monad/Logger.hs`:
`fmap`, `pure`, `<*>`, `return`, `>>=`, `liftIO`, `tell`, `listen`,
`pass`, `liftBase`. Tests pass (1340).

Branch `logger-inline`. Not merged.

## Raw runs (recorded by `exp run`)

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-05-09 | full     | b831b298     | b831b298 |     42.6 |     42.1 |   -1.1% | run 1, load 2.27 |
| 2026-05-09 | nochange | b831b298     | b831b298 |      0.6 |      0.6 |   -3.4% | run 1 |
| 2026-05-09 | prelude  | b831b298     | b831b298 |      3.7 |      3.8 |   +1.5% | run 1 |
| 2026-05-09 | leaf     | b831b298     | b831b298 |      1.6 |      1.6 |   -0.4% | run 1 |
| 2026-05-09 | full     | b831b298     | b831b298 |     44.4 |     44.8 |   +0.9% | run 2, load 5.72 |
| 2026-05-09 | nochange | b831b298     | b831b298 |      0.6 |      0.6 |   -1.5% | run 2 |
| 2026-05-09 | prelude  | b831b298     | b831b298 |      3.9 |      3.9 |   -0.2% | run 2 |
| 2026-05-09 | leaf     | b831b298     | b831b298 |      1.6 |      1.6 |   +1.8% | run 2 |
