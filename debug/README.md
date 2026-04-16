# Per-declaration typecheck profiling

The compiler emits eventlog markers at two levels:

- **Module-level**: `"ModuleName start"` / `"ModuleName end"` (from `Make.hs`)
- **Declaration-level**: `"tc ModuleName kind:name start"` / `"tc ModuleName kind:name end"` (from `TypeChecker.hs`)

Declaration kinds: `val`, `bind` (recursive group), `data`, `datagroup`,
`syn`, `kind`, `class`, `instance`, `extern`, `externdata`, `role`.

## Quick start

```bash
# 1. Build the compiler (eventlog is enabled by default in GHC >= 9.2)
stack build

# 2. Run with eventlog enabled, single-threaded for clean nesting
cd /path/to/your/purescript/project
purs +RTS -l-agu -N1 -RTS compile $(spago sources)

# 3. Convert the binary eventlog to JSON
eventlog2html --json purs.eventlog

# 4a. Text report (module + declaration breakdown)
node /path/to/purescript/debug/eventlog.js purs.eventlog.json

# 4b. Flamegraph (Chrome trace format — works in speedscope and chrome://tracing)
node /path/to/purescript/debug/eventlog-speedscope.js purs.eventlog.json > profile.json
# Open profile.json at https://www.speedscope.app/ or chrome://tracing
```

## RTS flags explained

| Flag    | Purpose                                            |
| ------- | -------------------------------------------------- |
| `-l`    | Enable eventlog output to `purs.eventlog`          |
| `-agu`  | Suppress GC/scheduler/user-tick noise              |
| `-N1`   | Single-threaded: gives clean per-declaration nesting |
| `-N`    | Multi-threaded (default): faster but events interleave |

Without `-N1`, modules typecheck in parallel and declaration events
from different modules interleave. The speedscope converter handles
this, but the flamegraph is cleaner with `-N1`.

## Tools

### `eventlog.js` — text report

```
node debug/eventlog.js purs.eventlog.json
```

Prints per-module timing (sorted ascending), concurrency stats, and
a per-declaration breakdown showing the top 50 slowest declarations
with module name, declaration kind, wall-clock time, and percentage.

### `eventlog-speedscope.js` — flamegraph

```
node debug/eventlog-speedscope.js purs.eventlog.json > profile.json
```

Outputs Chrome trace format JSON. Open in:
- https://www.speedscope.app/ (drag and drop)
- `chrome://tracing` in Chrome (load file)

The flamegraph shows module-level and declaration-level spans nested
properly. The text report (top N declarations) is also printed to
stderr.

Options:
- `--top N` — number of declarations in stderr report (default: 50)
- `--cap CAP` — filter to a specific GHC capability (thread)

## What the flamegraph shows

The flamegraph has two levels:

1. **Module** — total time for the module (includes desugaring, codegen, etc.)
2. **Declaration** — time for typechecking each declaration within the module

This tells you which declarations are expensive. Common patterns:

- **Large `instance:` spans** — complex instance resolution, often
  involving row-list traversals or `EncodeJson`/`DecodeJson` generics
- **Large `bind:` or `val:` spans** — complex type inference with
  many constraints
- **Large `datagroup:` spans** — mutually recursive type definitions
  with many constructors

## Overhead

The `traceMarker` calls are effectively free when not profiling:
- GHC >= 9.2: eventlog is always linked; `traceMarker` checks a flag and returns
- Per call: ~tens of nanoseconds (buffer write)
- Total for ~10k declarations: ~1ms against a ~72s build

## Prerequisites

Install `eventlog2html`:
```bash
cabal install eventlog2html
# or
pip install eventlog2html
```
