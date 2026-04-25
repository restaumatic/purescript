# Baseline profile — `799e8208`

Cost-centre profile of `purs` built with `stack build --profile`,
running pr-admin compile with `+RTS -p -RTS`. Multi-threaded
(implicit `-N`, 32 procs available). Profiled build is much slower
than optimised — the *relative* cost-centre share is what matters,
not the wall-clock time.

- Date: 2026-04-25 09:18 UTC
- Commit: `799e8208` (post synonym-opt + skip-redundant-entailment-unify merges)
- pr-admin: 1758 modules
- `total time = 558.61 secs (1081034 ticks @ 1000 us, 32 processors)`
- `total alloc = 441,289,324,752 bytes`
- Raw `.prof` not committed (~195 MB) — at `/tmp/reprofile-799e8208/purs.prof`

## Top cost centres (individual %time)

| Rank | %time | Cost centre | Module:line | Notes |
|------|-------|-------------|-------------|-------|
| 1 | 7.7% | `compareType` | Types.hs:990–1027 | new #1 — focus of this experiment |
| 2 | 6.2% | `$mKindApp.\` | Types.hs:247 | profile artifact (matcher SCC) |
| 3 | 6.1% | `$mTypeApp.\` | Types.hs:243 | profile artifact |
| 4 | 5.4% | `$mTypeConstructor.\` | Types.hs:235 | profile artifact |
| 5 | 4.6% | `compare` (PSString) | PSString.hs:52 | label/type-level string compares |
| 6 | 4.1% | `compare` (Qualified) | Names.hs:233 | Environment Map lookups |
| 7 | 3.1% | `$mTypeOp.\` | Types.hs:239 | profile artifact |
| 8 | 2.9% | `everywhereOnValuesTopDownM.g'` | AST/Traversals.hs | expression traversal |
| 9–17 | ~16% combined | other `$mXXX.\` matchers | Types.hs | profile artifact cluster |
| 17 | 1.3% | `compare` (ProperName) | Names.hs:192 | |

The `$mXXX.\` cluster is largely a profile-build artifact: GHC
suppresses inlining of bidirectional pattern-synonym matchers when
SCC annotations are present, so each match site shows up as a
separate cost centre. In the optimised build they fold back into
their callers (most into `compareType`) and contribute no separable
overhead.

## Construction frequency (also from this profile)

Sanity-check that hash combining can stay cheap, by comparing how
often the existing flag combiner runs to how often `compareType`
runs:

| Function | Total entries | Indiv %time | Indiv %alloc |
|------|---:|---:|---:|
| `combineFlags` | 478,848,674 | 0.00% | 0.20% |
| `typeFlags` | 1,082,946,679 | 0.00% | 0.00% |
| `forAllNodeFlags` | 1,561,620 | 0.00% | 0.00% |
| `constraintNodeFlags` | 696,569 | 0.00% | 0.00% |
| `compareType` | 518,838,348 | 7.40% | 5.00% |

`combineFlags` is called at the same scale as `compareType` (479M vs
519M) but contributes literally 0% individual time. This is the
synonym-opt thesis in numbers: cached structural properties paid for
themselves at scale because the per-node combine cost is essentially
free. Adding a hash combine on the same path should remain in the
same cost class.

## Key call-graph observation

`compareType` is largely driven through `unifyTypes''` at
`Unify.hs:121–123`, where a `Set (Type, Type)` membership check runs
on every `unifyTypes` call. This is the primary target of step 3 of
this experiment.

## How to reproduce

```sh
# Build profiled binary (one-off, ~10 min)
cd /workspace/purescript
stack build --profile --system-ghc

PROFILED="$(stack path --local-install-root --profile)/bin/purs"

# Run pr-admin compile, profiled
cd /workspace/restaumatic/apps/pr-admin
rm -rf output
bash -c '
  set -f
  SOURCES=$(spago sources 2>/dev/null | grep -v "^\[")
  "$0" compile $SOURCES +RTS -p -RTS
' "$PROFILED"

# Result: ./purs.prof (~195 MB)
```
