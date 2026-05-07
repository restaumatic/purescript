# Task: entailment-redundancy

## Goal

Survey constraint-solving redundancy on the post-PR-#18 baseline,
focused on the top-5 expensive decls (per chrome-trace):
`updateExternalMenuView`, `Restaurant.Settings.view`,
`MenuV2.Menus.spec`, `MenuV2.Product.spec`,
`Restaurant.Settings.control`. Report duplicate `(className,
arg-shape)` pairs to determine whether a within-decl memo could
help.

## Background

`skip-redundant-entailment-unify` (-15.5% full, shipped) caught the
trailing-unification redundancy. Per-decl eventlog timing on the
current tip shows solve still dominates infer 5-10x on the top
decls. We need to know **what** solve is doing repeatedly.

## Approach

Survey-only. Per-LESSONS Unify.hs sensitivity, treat
`Entailment.hs` as inlining-sensitive — minimal hot-path edit, all
infrastructure in a separate module.

### Hook point

In `Entailment.hs:259-269`, the `solve.go` clause:

```haskell
go work hints' con@(Constraint _ className' kinds' tys' conInfo) =
  let cn = T.unpack (showQualified runProperName className')
      startTag = "tc-entails " <> cn <> ...
  in traceMarker startTag $
  WriterT . StateT ...
```

Right before the `WriterT . StateT`, add a single call to
`Anatomy.recordSolve currentDecl className' tys'`. The `currentDecl`
context already exists via the `traceMarker` infra elsewhere — but
to avoid an additional state thread, we can record solves
**globally** with a per-class+arg fingerprint and report the top-N
recurring shapes; the per-decl breakdown can be inferred from the
existing `tc-` event-log spans we already capture.

### Data to collect

Per `solve.go` invocation:
- **Class name** (Qualified ProperName)
- **Arg fingerprint**: cheap brief representation, similar to
  `briefType` already in Entailment.hs. Use first 1-2 levels of
  type structure to keep buckets meaningful.

Aggregated:
- Total solves
- Distinct (className, fingerprint) pairs
- Top-30 most-frequent pairs with count
- Per-class total counts (which classes drive solve volume)

### Output

Dump on shutdown via `Compile.hs`:

```
=== entailment-redundancy ===
total solves: N
distinct (className, fingerprint) pairs: M
average occurrences per pair: N/M

top 30 most-frequent shapes:
  count   className                                   fingerprint
  ...

solves per class (top 20):
  count   className
  ...
```

## Key files

- `src/Language/PureScript/TypeChecker/EntailmentAnatomy.hs` — new.
  IORef counters, env-gated `PURS_ENTAILMENT_ANATOMY=1`, dump
  function.
- `src/Language/PureScript/TypeChecker/Entailment.hs` — single hook
  call inside `solve.go` (one line).
- `app/Command/Compile.hs` — call `EntailmentAnatomy.dumpAnatomy`
  before `exitSuccess`.
- `purescript.cabal` — expose the new module.

## How to measure

```sh
cd /workspace/p/entailment-redundancy
rm -rf .stack-work && stack build
ls -la $(stack path --local-install-root)/bin/purs   # ~48-49 MB
```

```sh
# Single-thread mandatory — IORef counters race under -N>1.
export PURS_ENTAILMENT_ANATOMY=1
cd /workspace/restaumatic/apps/pr-admin
rm -rf output
PATH=$(stack path --local-install-root)/bin:$PATH \
  $(stack path --local-install-root)/bin/purs +RTS -N1 -RTS \
  compile $(spago sources) 2>&1 | tee /tmp/entailment-anatomy.txt
```

Capture the histogram section to
`experiments/entailment-redundancy/anatomy.txt`.

## Tests

`stack test --fast` (then re-run `stack build` before any benchmark
to restore optimised binary — see LESSONS).

## Risks / things to watch

- **Entailment.hs sensitivity.** Even a one-line hook in `solve.go`
  could shift inlining. Verify binary size against c84101d8 baseline
  after build. If binary shrinks >1 MB, abort and redesign hook.
- **briefType cost.** Walking type structure on every solve adds
  per-call overhead. Use the absolute cheapest fingerprint possible
  (constructor head + arity). The survey's perf is irrelevant; the
  hit-pair distribution data is what matters.
- **Single-thread mandatory.** Multi-threaded races on the IORef.
