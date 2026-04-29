# Task: unify-cache-anatomy

## Goal

Decompose what the per-module unification cache is actually doing on
pr-admin: characterise hits/misses by call depth, pair size, and
per-module size growth — so we can identify which algorithmic
alternative could replace the HashSet without regressing prelude.

## Background

See EXPERIMENT.md for the why. Four prior experiments + cost-centre
data establish that the cache earns its keep but not by memoizing
the trivial outer calls at the 3 hot sites. Code reading shows
the cache is intercepted at `unifyTypes''` (Unify.hs:121-123) for
**every** recursive call from the structural dispatch, including
TypeApp's two-child descent. The hypothesis is that the cache's
load-bearing role is recursive-descent dedup.

## Approach

Survey-only. Instrument the cache via a separate module to avoid
Unify.hs inlining contamination (per LESSONS).

### Data to collect

Per cache lookup:
- **Depth**: 0 for outer calls (from external sites), ≥1 for
  recursive calls inside `unifyTypes'`. Tracked via an IORef
  call-stack counter incremented on entry / decremented on exit.
- **Hit/miss**.
- **Pair size**: total node count of `(t1, t2)`. Approximate via
  cheap subterm count (e.g., direct children only, or a `tfBits`-
  derived metric). Bucket: 1–2, 3–10, 11–50, 51+ nodes.

Per module:
- Total lookups, total hits, hit rate.
- Cache size at end of module.
- Distribution of cache size across the 1758 modules (median, p99,
  max).

### Output

Dump on shutdown via `Compile.hs` (stderr), shaped like
`funapp-lineage-survey`'s output. See EXPERIMENT.md for the layout.

## Key files (changes in worktree only — main repo stays clean)

- `src/Language/PureScript/TypeChecker/UnifyAnatomy.hs` — new
  module: env-gated counters, depth-tracking IORef, dump function.
  All `unsafePerformIO` and IORefs live here, not in Unify.hs.
- `src/Language/PureScript/TypeChecker/Unify.hs` — minimal hook:
  one extra call into `UnifyAnatomy.recordLookup` immediately before
  the existing `HS.member` check, and one before the `HS.insert`.
  No restructuring of the surrounding code.
- `app/Command/Compile.hs` — call `UnifyAnatomy.dumpAnatomy` before
  `exitSuccess`, like FunAppLineage did.
- `purescript.cabal` — add the new module to exposed-modules.

## How to measure

```sh
cd /workspace/p/unify-cache-anatomy
rm -rf .stack-work && stack build
ls -la $(stack path --local-install-root)/bin/purs   # ~49 MB
```

```sh
# Single-thread mandatory — IORef counters race under -N>1.
cd /workspace/restaumatic/apps/pr-admin
rm -rf output
PURS_UNIFY_ANATOMY=1 PATH=/workspace/p/unify-cache-anatomy/.stack-work/install/.../bin:$PATH \
  spago build 2>&1 | tee /tmp/anatomy.txt
```

Capture the histogram section to
`experiments/unify-cache-anatomy/anatomy.txt`.

## Tests

```bash
stack test --fast  # all tests must pass
```

(But run `stack build` again afterward to restore the optimised
binary if you plan to benchmark — see LESSONS `stack test --fast`.)

## Risks / things to watch

- **Unify.hs sensitivity.** Even one extra function call in
  Unify.hs's hot path can swing perf. Verify binary size matches
  baseline 49.1 MB after build. If not, revisit the hook design.
- **Pair-size walk cost.** Computing the exact node count is O(size).
  Use cheap approximation — exact size isn't needed; bucket
  boundaries are enough.
- **Depth tracking via IORef** has serialisation cost. We're not
  trying to measure perf with the survey on, just collect data
  on a single run. Acceptable.
- **Single-thread mandatory.** Multi-threaded builds race on the
  IORef counters. Use `+RTS -N1 -RTS` or set
  `STACK_BUILD_PARALLELISM=1` for the spago build. Even better:
  have the survey skip if `getNumCapabilities > 1`.
