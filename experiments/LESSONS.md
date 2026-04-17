# Cross-experiment lessons

Distilled learnings from performance experiments on the PureScript
compiler. Read this before starting a new experiment — particularly
the "dead-end techniques" section, so you don't re-attempt something
that's already been shown not to work on our workload (pr-admin, 1758
modules).

Each entry names the experiment it came from so you can dig into the
details. Append new entries when closing an experiment.

## Workload baseline

All numbers on this page are measured against `pr-admin`
(`/workspace/restaumatic/apps/pr-admin`, 1758 modules) unless noted.
Baseline numbers on the `restaumatic` branch, optimised build:

| Scenario                              | Time    |
| ------------------------------------- | ------- |
| Full build                            | ~72–73s |
| No-change rebuild                     | ~1.1s   |
| Touch leaf (timestamp only)           | ~1.2s   |
| Comment change to Prelude (1342 deps) | ~2.3s   |

Headline deltas quoted elsewhere are relative to those numbers. A
"+1% full" penalty is easily inside noise on a busy machine; a real
regression is usually ≥5%.

## Correctness traps

### A suspiciously large speedup usually means you broke semantics
**From:** `rust-interning` (see
`/workspace/p/rust-interning/PHASE2-RESULTS.md` and
`/workspace/p/rust-interning/profile-results.log`)

The Phase 2 Label-interning work measured an 80.7% improvement (57s →
11s) with "all 1340 tests passing." The speedup was real but came from
an `Ord` instance that compared interning ids rather than the
underlying string bytes. That is wrong whenever label iteration order
is observable (row normalisation, error formatting, deterministic
output). Our test corpus didn't exercise those paths.

Once the `Ord` instance was corrected, the same interning work
measured **+29% slower** than baseline (second-order lookup cost
exceeded the O(1) compare saving). A subsequent "caching" variant
recovered to -3.4%. A later commit regressed to **+148% slower** —
the variance suggests an unstable optimisation.

**Takeaways:**
1. When a perf experiment shows >20% improvement on a well-trodden
   path, assume something is broken and look for it explicitly. 5–10%
   is the normal range for a correct single-axis optimisation.
2. Tests passing is not semantic confirmation — we need test cases
   that depend on iteration order (error-message snapshot tests,
   deterministic-output tests) before trusting any change to `Ord`.
3. The 80.7% headline in `PHASE2-RESULTS.md` should be treated as
   refuted; the experiment's actual state is unresolved.

## Dead-end techniques (do not re-attempt without new evidence)

### Caching cheap per-decl work across builds
**From:** `tc-queries` (see `p/tc-queries/HANDOFF.md`)

For cheap declaration kinds (`DataDeclaration`,
`TypeSynonymDeclaration`, `TypeClassDeclaration`, `ExternDeclaration`,
`ExternDataDeclaration`, `RoleDeclaration`, `TypeInstanceDeclaration`)
the typecheck work per decl runs in low tens of microseconds. Adding a
cache — fingerprint + env-delta compute + env-delta apply — costs
*more* than the work it skips. On pr-admin, caching these decl kinds
adds +4–6% to full builds and +13% to incremental edits, even with
100% cache hit rates. The overhead is per-decl, not per-miss.

**Takeaway:** don't cache work that costs less than a hash+map-diff.
The prize is value-groups, not cheap decls. Any future caching
experiment should bring a measurement that the work-per-item exceeds
the caching overhead before building the cache.

### Changing PSString internal representation (ShortByteString)
**From:** `rust-interning` / earlier `rows-optimization` attempt
(see `/workspace/p/rust-interning/perf.md`)

Replacing PSString's internal `[Word16]` with `ShortByteString` for
better cache locality measured **+3.5% slower** on pr-admin. The
conversion overhead at every call site (`toUTF16CodeUnits`,
`fromUTF16CodeUnits`, JSON serialisation round-trips) dwarfed the
comparison-speed gain — because, per the above, PSString comparison
isn't the bottleneck we thought it was.

**Takeaway:** if you're changing a core representation, count the
conversion sites first. If the existing API shape forces conversions
around the hot path, the representation change has to pay for those
conversions before it can even start winning.

### Serialising elaborated value declarations to disk
**From:** `tc-queries` (see `p/tc-queries/HANDOFF.md`)

The elaborated `Declaration` output of `typesOf` embeds fully-typed
expressions (`TypedValue` annotations, dictionary references, source
spans everywhere). CBOR-serialising these to disk produces megabytes
per decl, aggregating to ~2 GB on pr-admin's ~17k value decls. Disk
I/O and in-memory allocation at that scale dwarfs any typecheck saving
(full build goes 73s → 217s).

**Takeaway:** a within-module cache for value groups is valuable in
principle, but not with the elaborated representation. Any future
attempt needs either (a) a smaller cache shape — e.g., env-delta only,
with codegen refactored to operate on un-elaborated decls plus
separately-cached type info — or (b) aggressive compression. Just
serialising the current types will lose.

## Confirmed boundaries worth exploiting

### `withFreshSubstitution` marks natural query boundaries
**From:** `tc-queries`

Both `typesOf` (`src/Language/PureScript/TypeChecker/Types.hs:93`) and
`kindsOfAll` (`src/Language/PureScript/TypeChecker/Kinds.hs:957`) start
with a clean unification substitution and don't leak it. These are
self-contained computation units — good Rock-query boundaries in
principle. The infrastructure for dispatching through queries is
already wired up on the `tc-queries` branch, but caching was blocked
by the serialisation cost above.

### Per-module extern hash reuse is mandatory
**From:** `tc-queries`

If you want to fingerprint anything per-decl-within-module, you have
to reuse per-module extern hashes across all modules that depend on
them. Re-serialising dep externs for every module with any cached
decl was a 10s+ cost on pr-admin. See `externHashRef` on the
`tc-queries` branch.

## Open territory (no experiment yet)

- **`compare (Qualified a)` at 20.8% of time** — the single biggest
  cost centre. Interning, switching to a smaller key type, or using
  `HashMap` instead of `Map` for `Environment` lookups are candidates.
- **`compare (PSString) at 8.6%`** — row labels and type-level string
  comparisons. Interning or a precomputed-hash wrapper.
- **`compareType` at 4.2%** — likely related to the above two via
  structural comparison of type trees.

Evaluate these against the "dead-end" lessons above before planning.
