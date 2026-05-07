# Task: entailment-decl-memo

## Goal

_What are we trying to reduce, and by what mechanism?_

## Background

_What the compiler does today, where the cost lives._

## Approach

_The planned changes, at a high level._

## Key files

_Paths to modify with one-line descriptions._

## How to measure

See `experiments/CLAUDE.md` and `run-profile.sh`. Run all four
scenarios (`--scenarios all`) — an optimisation isn't a win unless
it holds on nochange and prelude too.

## Tests

```bash
stack test --fast  # all tests must pass
```

## Risks / things to watch

_Known pitfalls, ord-instance correctness traps, test-coverage gaps._
