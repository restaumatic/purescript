---
id: traversal-inline
status: shipped
verdict: win
branch: traversal-inline
worktree: /workspace/p/traversal-inline
baseline_sha: c84101d8
head_sha: b831b298
hypothesis: >
  Post-PR-#18 hotspot table credits the AST decl-traversal cluster
  ~5% of full-build time, with `everywhereOnValuesTopDownM.g'`
  alone at 3.5%. The hottest caller is
  `Entailment.replaceTypeClassDictionaries` running per typechecked
  declaration to substitute deferred dictionary placeholders. That
  call uses `m = WriterT (Any, [...]) (StateT InstanceContext TypeCheckM)`
  — a heavy stack — but the traversal helper is polymorphic over
  `Monad m =>`. If GHC isn't specialising at the call site, every
  recursive bind in `g'`/`f'`/`h'` pays full unspecialised dictionary
  overhead. Adding `{-# INLINABLE #-}` (with SPECIALISE pragmas where
  needed) should let GHC produce a flat, monad-specialised loop.
  Target: -1% to -3% on full builds; nochange/leaf neutral.
headline_delta: full -8.9% to -9.6% across two runs (baseline c84101d8); nochange/prelude/leaf within noise. 2-line diff (two INLINABLE pragmas), +213 KB binary. Hypothesis exceeded by 3-9×.
tags: [ast, traversal, inlinable, specialise, entailment, win]
started: 2026-05-08
closed: 2026-05-08
---

# traversal-inline

## Hypothesis

The post-PR-#18 hotspots include:

| Cost centre                       | Module             | % time |
| --------------------------------- | ------------------ | ------ |
| `everywhereOnValuesTopDownM.g'`   | AST/Traversals.hs  | 3.5%   |
| (cluster aggregate)               | AST/Traversals.hs  | ~5%    |

The hot caller is `replaceTypeClassDictionaries` in
`TypeChecker/Entailment.hs:118-139`, used to substitute every
deferred type-class-dictionary placeholder once typechecking has
inferred enough to resolve them. Each typechecked declaration runs
through this traversal at least once.

The traversal is polymorphic over `Monad m =>`. The actual call
site uses `m = WriterT (Any, [...]) (StateT InstanceContext TypeCheckM)`
— at least four monad-stack layers, each with its own bind. For
the `g'` recursion to produce tight code, GHC needs to specialise
the polymorphic helper at this concrete monad — which it cannot
do across a module boundary unless we mark the helper `INLINABLE`.

This experiment marks `everywhereOnValuesTopDownM` and
`everywhereOnValuesM` `INLINABLE` and re-measures.

## Scope

**In:**

- `{-# INLINABLE #-}` on `everywhereOnValuesTopDownM` and
  `everywhereOnValuesM` in `AST/Traversals.hs`.
- (If the headline doesn't move) try targeted `{-# SPECIALISE #-}`
  pragmas at the Entailment call sites for the exact monad stack.
- (Stretch) the same treatment for `everythingOnValues` and
  `everywhereOnTypes` if they show up in profiling.

**Out:**

- Structural rewrites of the traversal helpers (preserve-sharing
  skip on no-op transformations, etc.) — those are a follow-up if
  this is neutral.
- Touching the callbacks themselves.

## Trap

Adding `INLINABLE` to a polymorphic helper causes GHC to *recompile*
each specialised copy at the call site. That can balloon binary
size and compile time. Watch for both: report binary size delta in
the results, and don't let `stack build` time blow up too much.

## Plan

See [TASK.md](TASK.md). Tiny diff: one or two lines.

## Outcome (closed 2026-05-08, verdict: win)

Two-line diff (two `{-# INLINABLE #-}` pragmas) yielded:

| Scenario   | Run 1 | Run 2 (verification) |
| ---------- | -----:| --------------------:|
| full       | -9.6% | **-8.9%**            |
| nochange   | +8.4% | -3.0%                |
| prelude    | -0.5% | +1.6%                |
| leaf       | -4.3% | -1.5%                |

Full-build win is robust (~-9%, three to nine times the predicted
-1% to -3%). Other scenarios are within measurement noise; nochange
swung from +8.4% to -3.0% between runs, confirming the run-1 value
was machine-load noise on a 643ms wallclock — not a real INLINABLE
penalty.

**Cost:**
- Diff: 2 lines.
- Binary: 48,625,952 → 48,839,456 bytes (+213 KB / +0.4%).
- `stack build` time: unchanged within noise.
- Tests: 1340 pass (`stack test --fast`).

**Mechanism:** The hot caller's monad stack (WriterT (Any, ...)
(StateT InstanceContext TypeCheckM)) was forcing GHC to call the
polymorphic traversal helper through a runtime dictionary — paying
per-node bind overhead for every recursive descent over Expr trees.
INLINABLE exposes the unfolding in the interface file so GHC's
specialiser at each call site produces a flat, monad-specialised
loop.

**Recommended next step:** Open a PR. See [results.md](results.md)
for full numbers and reasoning.

## Links

- Worktree: /workspace/p/traversal-inline
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Hot caller: `src/Language/PureScript/TypeChecker/Entailment.hs:118-139`
