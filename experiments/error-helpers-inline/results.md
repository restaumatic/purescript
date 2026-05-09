# Results for error-helpers-inline

## Headline (verdict: no-win, neutral)

Two runs against `b831b298` baseline (the traversal-inline tip):

| Scenario | Run 1 (load 2.48) | Run 2 (load 1.57, disk fixed) | Reality                |
| -------- | ----------------: | ----------------------------: | ---------------------- |
| full     | +4.7%             | **-0.4%**                     | neutral (within noise) |
| nochange | -2.1%             | +1.4%                         | neutral                |
| prelude  | +1.7%             | +1.1%                         | neutral                |
| leaf     | -1.8%             | +2.7%                         | neutral                |

Run 1's full +4.7% was machine-load contamination: per-round
deltas swung from +3.5% to +8.5% on a noisy machine, while Run 2's
clean per-round deltas were +0.4%, -2.7%, -1.9%, +1.0% — pure
noise. **Hypothesis was wrong** — the INLINABLE pragmas didn't
help.

The binary size delta is the smoking gun: traversal-inline's
analogous change grew the binary by +213 KB (real specialisation),
but error-helpers-inline grew by only +32 bytes. **GHC did not
specialise these helpers at the call sites**, despite the
INLINABLE pragmas being syntactically correct.

## Why no win — likely cause

The TypeChecker monad `TypeCheckM` is defined via newtype-derive:

```haskell
newtype TypeCheckM a = TypeCheckM { unTypeCheckM :: ... }
  deriving newtype (Functor, Applicative, Monad,
                    MonadSupply, MonadState CheckState,
                    MonadWriter MultipleErrors,
                    MonadError MultipleErrors)
```

When `rethrow :: (MonadError e m) => (e -> e) -> m a -> m a` is
called from `Entailment.replaceTypeClassDictionaries` at concrete
`m = WriterT (Any, [...]) (StateT InstanceContext TypeCheckM)`, the
caller sees a `MonadError` dictionary it had to construct from the
`MonadError` instance for that whole stack. With INLINABLE on
`rethrow`, GHC could in principle specialise — *if* the
`catchError` operation in the body could be specialised to
the same monad. But the body just calls `catchError` through the
same dictionary; there's no concrete simplification GHC can do
beyond what the regular dispatch already gives. The pragma
exposes the unfolding, but the unfolding is already trivial:
`flip catchError (throwError . f)`.

**Contrast with traversal-inline.** That experiment marked
helpers whose bodies do *recursive descent* with monadic bind
on every step (`g' (Abs binder v) = Abs <$> ... >>= g'`). The win
came from collapsing many polymorphic `>>=` calls into a flat,
specialised loop — there's structure for the specialiser to
chew on. The error-helper bodies have no recursive descent and no
per-element bind; they're one-shot wrappers. INLINABLE has
nothing to specialise away.

## Per-round detail (Run 2, the clean one)

```
=== full ===
  round 2: base=44749 head=44934 ms (Δ +0.4%)
  round 3: base=44324 head=43112 ms (Δ -2.7%)
  round 4: base=43235 head=42421 ms (Δ -1.9%)
  round 5: base=42641 head=43069 ms (Δ +1.0%)
  median: base=43235 head=43069  Δ=-0.4%

=== nochange === (n=4 after warmup)
  base medians: 529, 561, 584, 594; head medians: 560, 569, 573, 582
  median Δ: +1.4%   (within ~50ms noise floor)

=== prelude ===
  median: base=3900 head=3942  Δ=+1.1%

=== leaf ===
  median: base=1556 head=1598  Δ=+2.7%
```

## What this rules out

- **INLINABLE on stateless one-shot polymorphic helpers does not
  pay**, even when callers use heavy monad stacks. The traversal-
  inline win was specifically about collapsing recursive bind
  chains; one-shot wrappers have no chain to collapse.
- The `withErrorMessageHint` 1.6% cost-centre line is not an
  inlining-overhead cost; it's the legitimate work of mutating
  `checkHints` on the way in and restoring it on the way out.
  Optimising that is a different shape of attack (e.g. avoiding
  the State manipulation when no error fires).

## Source diff (preserved on branch, not merged)

Six `{-# INLINABLE #-}` pragmas added in two files:

- `src/Language/PureScript/Errors.hs:2010-2024` — `rethrow`,
  `warnAndRethrow`, `rethrowWithPosition`, `warnWithPosition`,
  `warnAndRethrowWithPosition`.
- `src/Language/PureScript/TypeChecker/Monad.hs` —
  `withErrorMessageHint` (line 188), `guardWith` (line 356).

Total: 7 single-line pragmas across 2 files. Tests pass (1340).

## Raw runs (recorded by `exp run`)

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-05-08 | full     | b831b298     | b831b298 |     45.7 |     47.8 |   +4.7% | run 1, load 2.48 — load contamination |
| 2026-05-08 | nochange | b831b298     | b831b298 |      0.6 |      0.6 |   -2.1% | run 1 |
| 2026-05-08 | prelude  | b831b298     | b831b298 |      3.8 |      3.9 |   +1.7% | run 1 |
| 2026-05-08 | leaf     | b831b298     | b831b298 |      1.6 |      1.5 |   -1.8% | run 1 |
| 2026-05-08 | (all)    | b831b298     | b831b298 |  (failed) |  (failed) | — | run 2 — disk full, all compiles failed; ignore |
| 2026-05-09 | full     | b831b298     | b831b298 |     43.2 |     43.1 |   -0.4% | run 2 (real), disk fixed, load 1.57 |
| 2026-05-09 | nochange | b831b298     | b831b298 |      0.6 |      0.6 |   +1.4% | run 2 (real) |
| 2026-05-09 | prelude  | b831b298     | b831b298 |      3.9 |      3.9 |   +1.1% | run 2 (real) |
| 2026-05-09 | leaf     | b831b298     | b831b298 |      1.6 |      1.6 |   +2.7% | run 2 (real) |
