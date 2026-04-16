# Claude guidance for this repo

This is the PureScript compiler (restaumatic fork). Build with
`stack build`, test with `stack test --fast`. See `INSTALL.md` for
toolchain setup.

---

## Performance work

All compiler-performance optimisation work is organised as
**experiments** tracked under `experiments/`. Before starting anything
perf-related, read:

1. `experiments/README.md` — index of all experiments with status,
   verdict, and headline delta.
2. `experiments/LESSONS.md` — **required reading.** Distilled
   learnings from closed experiments, including dead-end techniques
   (e.g., don't cache cheap per-decl typecheck work; don't trust
   headline speedups >20% without checking for a semantics bug).
3. `experiments/SCHEMA.md` — layout of each experiment folder and
   the lifecycle states.

### Workload

Performance is measured against **pr-admin**
(`/workspace/restaumatic/apps/pr-admin`, 1758 modules), built via
`spago build` with a custom `purs` binary on PATH. Baseline numbers on
the `restaumatic` branch, optimised build:

| Scenario                              | Time    |
| ------------------------------------- | ------- |
| Full build                            | ~72–73s |
| No-change rebuild                     | ~1.1s   |
| Touch leaf                            | ~1.2s   |
| Comment change to Prelude (1342 deps) | ~2.3s   |

### Four scenarios to always run

An optimisation is only a win if it doesn't regress any scenario. Many
cache-oriented changes win on `full` and lose on `nochange` or
`prelude` — that's the whole reason we run all four:

| Scenario   | How                                                 | What it catches                 |
| ---------- | --------------------------------------------------- | ------------------------------- |
| `full`     | `rm -rf output && spago build`                      | raw throughput                  |
| `nochange` | full, then a second `spago build`                   | overhead on the no-op path      |
| `prelude`  | full, touch Prelude, `spago build`                  | cascade cost (1342 deps)        |
| `leaf`     | full, touch an arbitrary leaf module, `spago build` | single-module rebuild overhead  |

### Noise discipline

- Discard run 1 as warm-up, report median + (min, max) of the
  remaining runs.
- Run ≥4 runs after warm-up (so median-of-4 is meaningful).
- Don't compare a `--profile` build against a non-profile build, or a
  `--fast` build against an optimised one — the overhead is real.
- If baseline-vs-baseline varies by more than ~1–2% between
  back-to-back invocations, the measurement harness has a bug; fix it
  before trusting any numbers.

### Starting a new experiment

```sh
# Scaffolds experiments/<id>/, creates branch <id> off baseline-sha,
# creates worktree at /workspace/p/<id>
experiments/scripts/exp new <id> [--from <baseline-sha>]
```

The scaffold produces:
- `experiments/<id>/EXPERIMENT.md` — frontmatter + hypothesis. Fill
  this in immediately; it's the entry point for anyone finding the
  experiment later.
- `experiments/<id>/TASK.md` — detailed plan (what to change, where,
  why).
- `experiments/<id>/HANDOFF.md` — live work log. Update as you go so
  the next agent (or future-you) can pick up where you left off.
- `experiments/<id>/results.md` — structured results table,
  append-only.

Then work on the branch in `/workspace/p/<id>`. The main repo at
`/workspace/purescript` stays on the current branch; the worktree has
its own checkout.

### Measuring

```sh
# Single scenario, quick iteration
experiments/scripts/exp run <id> --scenarios full --runs 5

# All four scenarios, with cost-centre profiling
experiments/scripts/exp run <id> --scenarios all --runs 5 --profile
```

Results append to `experiments/<id>/results.md` with baseline SHA,
head SHA, median, and notes. If `--profile` is set, `.prof` files land
in `experiments/<id>/profiles/` (gitignored) with a tracked
`.meta.md` sidecar recording the top cost centres and the commit.

### Baselines

Baselines are keyed by the commit SHA they were built from, stored at
`experiments/baselines/<short-sha>/purs` (gitignored). Rebuild on
demand:

```sh
experiments/scripts/exp build-baseline <sha-or-branch>
```

The manifest at `experiments/baselines/manifest.md` tracks what
baselines have been built, on what machine, with what GHC — so any
measurement can be reproduced.

Every `EXPERIMENT.md` records which `baseline_sha` its results are
against, so you can always figure out what a number is comparing to.

### Closing an experiment

```sh
experiments/scripts/exp close <id> --verdict win|partial|no-win|abandoned
```

This sets the frontmatter to closed and prompts for a one-paragraph
entry in `experiments/LESSONS.md`. **Add that entry** — especially for
dead ends. The lesson is worth more than the code.

### Worktrees

Active experiment worktrees live at `/workspace/p/<id>`. The current
set:

```sh
git worktree list
# /workspace/purescript        [restaumatic]
# /workspace/p/tc-queries      [tc-queries]
# /workspace/p/synonym-opt     [synonym-opt]
# /workspace/p/rust-interning  [rust-interning]
```

Each experiment's `EXPERIMENT.md` also records its worktree path in
the frontmatter.

### Per-declaration profiling

The compiler emits eventlog markers for every typechecked declaration.
To see which specific declarations are slow (e.g., complex type-level
row-list computations, heavy instance resolution):

```sh
# Build normally, then run with eventlog
stack build
purs +RTS -l-agu -N1 -RTS compile $(spago sources)

# Text report with declaration breakdown
eventlog2html --json purs.eventlog
node debug/eventlog.js purs.eventlog.json

# Flamegraph (module > declaration hierarchy)
node debug/eventlog-speedscope.js purs.eventlog.json > profile.json
# Open at https://www.speedscope.app/ or chrome://tracing
```

See `debug/README.md` for details on RTS flags and tools.

### Hotspot reference

From the most recent profile on the `restaumatic` branch (see
`experiments/README.md` for the live table):

| Cost Centre                  | Module                | % time | Status                   |
| ---------------------------- | --------------------- | ------ | ------------------------ |
| `compare` (Qualified a)      | Names.hs:234          | 20.8%  | unattacked               |
| `replaceAllTypeSynonyms'.go` | TypeChecker.Synonyms  | 16.9%  | see `synonym-opt`        |
| `compare` (PSString)         | PSString.hs:52        | 8.6%   | unattacked               |
| `compareType`                | Types.hs              | 4.2%   | unattacked               |

When selecting a new experiment, pick an unattacked hotspot, or a
previously-attacked one whose experiment reached `no-win` with a
clear path to a different approach. **Don't re-attempt a dead-end
technique without new evidence** — check `LESSONS.md` first.
