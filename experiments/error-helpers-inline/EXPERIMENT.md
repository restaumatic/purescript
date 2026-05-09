---
id: error-helpers-inline
status: abandoned
verdict: no-win
branch: error-helpers-inline
worktree: /workspace/p/error-helpers-inline
baseline_sha: b831b298
head_sha: b831b298
hypothesis: >
  Same pattern as `traversal-inline`. After that experiment shipped
  -9% on full builds via two `INLINABLE` pragmas on
  `everywhereOnValuesTopDownM`/`everywhereOnValuesM`, an audit of
  other polymorphic helpers on the typechecker hot path turned up
  Errors.hs's error-rewriting combinators (`rethrow`, `warnAndRethrow`,
  `rethrowWithPosition`, `warnWithPosition`, `warnAndRethrowWithPosition`)
  and Monad.hs's `withErrorMessageHint`. None has any inline pragma.
  All are polymorphic over `(MonadError, MonadWriter, MonadState)`
  and called heavily from `TypeChecker.hs`, `Entailment.hs`,
  `Kinds.hs`, `Synonyms.hs` etc — always at concrete `TypeCheckM`.
  The `withErrorMessageHint` cost-centre was already 1.6% of full
  builds in the pre-traversal-inline profile; the underlying
  `rethrow` and `addHint` calls would be wrapped per-decl, per-typed-
  expression, etc. Adding `{-# INLINABLE #-}` should let GHC
  specialise these helpers across module boundaries the way it
  cannot today. Target: -1% to -3% additional on full builds atop
  traversal-inline; nochange/leaf neutral.
headline_delta: full -0.4%, nochange +1.4%, prelude +1.1%, leaf +2.7% — neutral on a clean machine. Hypothesis falsified — INLINABLE on stateless one-shot polymorphic helpers does not pay (binary grew only +32 B vs traversal-inline's +213 KB, so GHC didn't actually specialise across module boundaries despite the pragmas).
tags: [errors, monad-helpers, inlinable, specialise, typechecker, no-win]
started: 2026-05-08
closed: 2026-05-09
---

# error-helpers-inline

## Hypothesis

This is a follow-up audit prompted by the success of `traversal-inline`
(-9% on full builds via two `INLINABLE` pragmas).

The hot caller pattern is identical:

| Helper                        | Module        | Polymorphic over                              | Inline pragma? |
| ----------------------------- | ------------- | --------------------------------------------- | -------------- |
| `rethrow`                     | Errors.hs     | `MonadError e m`                              | none           |
| `warnAndRethrow`              | Errors.hs     | `MonadError e m, MonadWriter e m`             | none           |
| `rethrowWithPosition`         | Errors.hs     | `MonadError MultipleErrors m`                 | none           |
| `warnWithPosition`            | Errors.hs     | `MonadWriter MultipleErrors m`                | none           |
| `warnAndRethrowWithPosition`  | Errors.hs     | `MonadError + MonadWriter MultipleErrors m`   | none           |
| `withErrorMessageHint`        | Monad.hs      | `MonadState CheckState m, MonadError m`       | none           |

All called heavily from the typechecker hot path with concrete
`TypeCheckM` (a newtype around StateT/WriterT/ExceptT). Without
`INLINABLE`, GHC cannot expose the unfolding to other modules,
so every wrapped action pays unspecialised dispatch through the
`MonadError`/`MonadWriter` dictionaries.

The post-PR-#18 hotspot table credits `withErrorMessageHint` 1.6%
of full builds. That number captures the SCC-instrumented cost of
the wrapping itself — the underlying `rethrow`/`addHint` work is
elsewhere. Adding `INLINABLE` lets GHC inline the wrapper into the
caller, where the caller-side specialisation can collapse the
`catchError`/`censor` chain into direct continuation calls.

## Scope

**In:**

- `{-# INLINABLE #-}` on the six helpers listed above.
- (Possibly) `addHints`, `addHint`, `onErrorMessages` if they're
  in the same hot path and trivially gain.
- `guardWith` in Monad.hs (also polymorphic, also unannotated).

**Out:**

- Any code change beyond pragmas.
- Restructuring the monad stack or splitting `TypeCheckM`.
- Touching the call sites.

**Stacking:**

This branch is rooted on `b831b298` (the traversal-inline tip),
so the measurement captures the *additional* delta atop
traversal-inline's -9% full-build win. The baseline for headline
numbers in `results.md` is `b831b298`, not `c84101d8`.

## Trap

Same as `traversal-inline`: adding `INLINABLE` causes GHC to emit
specialised copies at heavy callers, bumping binary size and
potentially `stack build` time. Expect <1 MB binary growth.

## Plan

See [TASK.md](TASK.md). Tiny diff: 6-9 single-line pragmas.

## Outcome (closed 2026-05-09, verdict: no-win)

Two runs against `b831b298`. The clean Run 2 (load 1.57, disk
fixed, after Run 1 was contaminated by load 2.48 then a separate
disk-full episode) showed:

| Scenario | Δ |
|---|---:|
| full | -0.4% |
| nochange | +1.4% |
| prelude | +1.1% |
| leaf | +2.7% |

All within noise floor. **Hypothesis was wrong.**

The binary size delta tells the story: traversal-inline's
analogous change grew the binary by +213 KB (real specialisation
of recursive descent helpers); error-helpers-inline grew by only
+32 bytes. GHC did not produce specialised copies of the error
helpers despite the INLINABLE pragmas being present and
syntactically correct.

The reason — clarified in retrospect — is that the error helpers
are **stateless one-shot wrappers**. The bodies are like
`rethrow f = flip catchError (throwError . f)`: a single call
through the same MonadError dictionary the caller already has.
There's nothing for GHC's specialiser to collapse. The
traversal-inline win came from collapsing *recursive* monadic
descent (a chain of `>>=` per AST node), which has structure to
specialise; one-shot wrappers don't.

**Branch preserved at error-helpers-inline for archaeology.** Not
merged.

See [results.md](results.md) for full numbers and per-round detail.

## Links

- Worktree: /workspace/p/error-helpers-inline
- Predecessor (for the same pattern): [../traversal-inline/EXPERIMENT.md](../traversal-inline/EXPERIMENT.md)
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Errors.hs helpers: `src/Language/PureScript/Errors.hs:2010-2024`
- Monad.hs helper: `src/Language/PureScript/TypeChecker/Monad.hs:183-205`
