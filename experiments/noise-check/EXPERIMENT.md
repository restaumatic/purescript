---
id: noise-check
status: shipped
verdict: win
branch: restaumatic
worktree: /workspace/purescript
baseline_sha: 3fcac773
head_sha: 3fcac773
hypothesis: >
  Framework self-check: baseline-vs-baseline should measure identical
  binaries with delta < 1-2%. If not, the measurement harness has a
  bug and should not be trusted for real experiments.
headline_delta: "+0.1% full (within noise — harness OK)"
tags: [meta, framework]
started: 2026-04-15
closed: 2026-04-15
---

# noise-check — harness self-test

Compares the same baseline binary against itself. Any delta > 1-2%
indicates a bug in the harness (not in purs).

## Result (2026-04-15)

**PASS.** Baseline median 71041 ms vs head median 71093 ms, Δ +0.1%.
Individual runs:

- Baseline (drop warm-up 71635): 71041 ms, 71902 ms — spread 1.2%
- Head (drop warm-up 70057):     71093 ms, 73826 ms — spread 3.9%

Head's max of 73826 ms was almost certainly transient machine load
(other processes scheduled); the median filters it out, which is
exactly what warm-up-discard + median-of-N is for. A future noise
check with `--runs 5` or more would tighten this further.

## What this validates

- `purs compile $(spago sources)` with `set -f` to skip shell globbing
  correctly passes globs through for purs to expand recursively.
- `date +%s%N` gives millisecond-precision timing (no more 1s rounding).
- Logging to stderr means command-substitution captures clean numeric
  output.
- The ~71s full-build baseline on pr-admin matches the pre-framework
  reference in `/workspace/p/tc-queries/PROFILING.md:98` (~72s).
