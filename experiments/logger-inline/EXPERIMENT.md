---
id: logger-inline
status: abandoned
verdict: no-win
branch: logger-inline
worktree: /workspace/p/logger-inline
baseline_sha: b831b298
head_sha: b831b298
hypothesis: >
  Fresh post-traversal-inline cost-centre profile shows
  `>>=.\` (Control.Monad.Logger src/Control/Monad/Logger.hs:36)
  at 1.3% time, plus several related Logger SCCs (`>>=.\.\` 1.1%,
  `fmap.\` 0.9%) totalling ~3-4% of full builds. Logger is the
  underlying writer-via-IORef monad backing the typechecker's
  TypeCheckM newtype stack — every monadic step in the typechecker
  ultimately calls Logger's `>>=`.
  The Monad/Applicative/Functor instances in Control/Monad/Logger.hs
  have **no INLINE pragmas at all** — and `>>=` for a non-trivial
  monad (`Logger l >>= f = Logger $ \r -> l r >>= \a -> runLogger (f a) r`)
  needs INLINE to fuse the wrapper layer away at the call site.
  This is the same recursive-bind-chain pattern that paid -9% in
  `traversal-inline` (callers chain many >>='s; specialisation
  collapses the chain).
  Target: -1% to -3% on full builds; nochange/leaf neutral.
headline_delta: full -1.1% then +0.9% (sign flipped), nochange -3.4% then -1.5% (small consistent direction but tiny absolute), prelude +1.5% then -0.2% (sign flipped), leaf -0.4% then +1.8% (sign flipped). Verdict no-win. Same lesson as error-helpers-inline — INLINE on already-cheap class wrappers doesn't pay; profile cost-centre %time on Monad bind methods is mostly an SCC artefact.
tags: [monad, logger, inline, typechecker, no-win]
started: 2026-05-09
closed: 2026-05-09
---

# logger-inline

## Hypothesis

The `Logger w a` newtype wraps `IORef w -> IO a`. Its monad
instance:

```haskell
instance (Monoid w) => Monad (Logger w) where
  return = pure
  Logger l >>= f = Logger $ \r -> l r >>= \a -> runLogger (f a) r
```

has no INLINE pragmas. The cost-centre profile (taken on
traversal-inline tip b831b298) credits:

| Cost centre  | Module             | %time | %alloc |
| ------------ | ------------------ | ----- | ------ |
| `>>=.\`      | Control.Monad.Logger | 1.3 | 0.0    |
| `>>=.\.\`    | Control.Monad.Logger | 1.1 | 0.4    |
| `fmap.\`     | Control.Monad.Logger | 0.9 | 1.5    |
| `return`     | Control.Monad.Supply | 1.2 | 3.2    |

Total ~3-4% — the Logger newtype layer adds a closure allocation
per bind, and without INLINE GHC cannot fuse `runLogger (f a) r`
back into a flat IO continuation at the caller. Matches the
"recursive bind chain" precondition we just learned from
`error-helpers-inline` (which failed because the helpers there
were *one-shot wrappers*; Logger's `>>=` lives inside an actual
chain via the typechecker's monadic loops).

## Scope

**In:**

- `{-# INLINE #-}` on every Monad/Applicative/Functor/MonadIO/
  MonadWriter method in `src/Control/Monad/Logger.hs`:
  `fmap`, `pure`, `<*>` (via `ap`), `return`, `>>=`, `liftIO`,
  `tell`, `listen`, `pass`.
- (Stretch, only if Logger doesn't move the needle) consider
  the same on Control.Monad.Supply if its derived methods aren't
  inlining.

**Out:**

- Restructuring Logger (it's reasonable as-is; the issue is
  purely that the methods aren't marked inlinable cross-module).
- Removing or rewriting the IORef-based logging discipline.
- Touching call sites.

## Trap

- **Cross-module specialisation may not happen** if the caller
  monad's bind goes through some intermediate dictionary that
  defeats the inline. Verify by checking `stack build` doesn't
  blow up and the binary size delta is real (>0 KB; should be a
  few KB at most for this small a function).
- **Be careful with newtype-derived TypeCheckM.** TypeCheckM
  derives Monad via `deriving newtype` from a stack atop Logger.
  GHC's newtype-coercion-derivation should pass our INLINE
  through, but it's worth double-checking the binary actually
  changed.

## Plan

See [TASK.md](TASK.md). Tiny diff: ~10 single-line pragmas in one
file.

## Links

- Worktree: /workspace/p/logger-inline
- Source: `src/Control/Monad/Logger.hs`
- Predecessor: [../traversal-inline/EXPERIMENT.md](../traversal-inline/EXPERIMENT.md)
- Cautionary tale: [../error-helpers-inline/EXPERIMENT.md](../error-helpers-inline/EXPERIMENT.md)
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
