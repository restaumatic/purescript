#!/bin/bash
set -euo pipefail
# spago sources emits globs like `.spago/aff/v7.1.0/src/**/*.purs`.
# We pass these to purs verbatim so it can do its own (recursive)
# glob expansion — matching what psa/spago do. Without -f, bash
# would shell-expand `**` as a single `*` (globstar isn't on), which
# partially expands some globs, leaves others as literals, and purs
# rejects the resulting mix.
set -f

# PureScript compiler profiling harness.
#
# Usage:
#   run-profile.sh run --experiment <id> --variant <baseline|head> \
#                  --purs <path> [--scenarios full,nochange,prelude,leaf] \
#                  [--runs 5] [--profile] [--baseline-sha <sha>] [--head-sha <sha>]
#
#   run-profile.sh build-baseline --sha <sha>
#   run-profile.sh build
#
# Design notes (see KNOWN-BUGS section at bottom for history):
# - Invokes `purs compile $(spago sources)` directly, NOT `spago build`.
#   spago 0.21 picks up node_modules/.bin/purs before honoring our PATH
#   shim, which is why the legacy script recorded 1-second "builds" —
#   the shim was silently ignored and spago's incremental logic ran
#   against a previous purs's outputs.
# - `spago sources` does NOT require purs and is cheap; it just emits
#   source globs from spago.dhall.
# - Timing uses `date +%s%N` for ms precision. Bash's $SECONDS is
#   integer-seconds-only and caused the "0s" rows in legacy logs.
# - All log output goes to stderr. The only thing written to stdout
#   from run_once is the integer millisecond timing, so it can be
#   captured via command substitution without contamination.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
PR_ADMIN_DIR="/workspace/restaumatic/apps/pr-admin"
EXPERIMENTS_DIR="$REPO_ROOT/experiments"
BASELINES_DIR="$EXPERIMENTS_DIR/baselines"

log()   { echo "[$(date '+%H:%M:%S')] $*" >&2; }
error() { echo "[ERROR] $*" >&2; exit 1; }

usage() {
    cat >&2 <<EOF
run-profile.sh: PureScript compiler timing harness

Commands:
  run                 Run a set of scenarios against a single purs binary
                      and append to experiments/<id>/results.md.
  build               Build current branch optimised (stack build).
  build-profiled      Build current branch with --profile.
  build-baseline      Build and stash a baseline binary keyed by SHA.

Run options:
  --experiment <id>           experiment id (results go to experiments/<id>/results.md)
  --variant <baseline|head>   which column the measurement represents
  --purs <path>               path to the purs binary to measure
  --baseline-purs <path>      OPTIONAL — measure baseline in same invocation
  --scenarios <list|all>      comma-list of full,nochange,prelude,leaf (default: full)
  --runs <n>                  total runs per scenario (default: 5). Run 1 is warm-up
                              and discarded; median of remaining (n-1) runs is reported.
  --profile                   wrap purs with +RTS -p -hc -RTS and stash the .prof file
                              into experiments/<id>/profiles/ with a .meta.md sidecar.
  --baseline-sha <sha>        short SHA recorded in the results row
  --head-sha <sha>            short SHA recorded in the results row
  --notes <text>              extra notes column

build-baseline options:
  --sha <sha-or-branch>       what to build the baseline from

Examples:
  # Measure current tip of the qualified-intern branch against baseline
  run-profile.sh run --experiment qualified-intern --variant head \\
                     --purs /workspace/p/qualified-intern/.stack-work/install/.../purs \\
                     --baseline-purs experiments/baselines/3fcac773/purs \\
                     --scenarios all --runs 5 \\
                     --baseline-sha 3fcac773 --head-sha abc1234

EOF
    exit 1
}

################################################################################
# Low-level: invoke purs directly and time it in milliseconds.
################################################################################

# Writes ONLY the elapsed milliseconds (int) to stdout.
# All logging goes to stderr.
run_once() {
    local PURS_BIN="$1"
    local SCENARIO="$2"           # full | nochange | prelude | leaf
    local PROFILE_MODE="${3:-}"   # empty or "profile"
    local LABEL="${4:-current}"

    local OUTPUT_DIR="$PR_ADMIN_DIR/output"
    local PRELUDE="/workspace/restaumatic/libs/ps/restaumatic-prelude/src/Restaumatic/Prelude.purs"
    local LEAF=""

    case "$SCENARIO" in
        full)
            rm -rf "$OUTPUT_DIR"
            ;;
        nochange)
            # Caller is responsible for having a populated output/ first;
            # run_scenario arranges this with a prior `full` run.
            :
            ;;
        prelude)
            [[ -f "$PRELUDE" ]] || error "prelude file not found: $PRELUDE"
            cp "$PRELUDE" "$PRELUDE.bak"
            echo "-- force-rebuild $(date +%s%N)" >> "$PRELUDE"
            ;;
        leaf)
            LEAF=$(find "$PR_ADMIN_DIR/src" -name '*.purs' -type f 2>/dev/null | head -1)
            [[ -n "$LEAF" ]] || error "no leaf .purs found under $PR_ADMIN_DIR/src"
            cp "$LEAF" "$LEAF.bak"
            echo "-- force-rebuild $(date +%s%N)" >> "$LEAF"
            ;;
        *)
            error "unknown scenario: $SCENARIO"
            ;;
    esac

    # spago sources emits source globs; grep strips spago's warning lines.
    local SOURCES
    SOURCES=$(cd "$PR_ADMIN_DIR" && spago sources 2>/dev/null | grep -v '^\[' || true)
    [[ -n "$SOURCES" ]] || error "spago sources returned nothing from $PR_ADMIN_DIR"

    local START END ELAPSED_MS PURS_EXIT
    START=$(date +%s%N)

    # Invoke purs directly — no PATH shim, no `spago build`. We pick
    # the binary ourselves; no chance of node_modules/.bin/purs
    # substitution.
    # Stderr goes to a tmp file so we can surface real failures.
    local PURS_STDERR
    PURS_STDERR=$(mktemp)
    set +e
    (
        cd "$PR_ADMIN_DIR"
        if [[ "$PROFILE_MODE" == "profile" ]]; then
            "$PURS_BIN" compile $SOURCES +RTS -p -hc -RTS 1>/dev/null 2>"$PURS_STDERR"
        else
            "$PURS_BIN" compile $SOURCES 1>/dev/null 2>"$PURS_STDERR"
        fi
    )
    PURS_EXIT=$?
    set -e

    END=$(date +%s%N)
    ELAPSED_MS=$(( (END - START) / 1000000 ))

    if [[ $PURS_EXIT -ne 0 ]]; then
        log "purs compile FAILED (exit $PURS_EXIT) — last stderr lines:"
        tail -20 "$PURS_STDERR" >&2 || true
        rm -f "$PURS_STDERR"
        # Restore any perturbed file before propagating failure
        [[ "$SCENARIO" == "prelude" && -f "$PRELUDE.bak" ]] && mv "$PRELUDE.bak" "$PRELUDE"
        [[ "$SCENARIO" == "leaf"    && -n "$LEAF"   && -f "$LEAF.bak" ]] && mv "$LEAF.bak" "$LEAF"
        error "purs compile failed for scenario=$SCENARIO label=$LABEL"
    fi
    rm -f "$PURS_STDERR"

    # Restore any file we perturbed
    case "$SCENARIO" in
        prelude) mv "$PRELUDE.bak" "$PRELUDE" ;;
        leaf)    mv "$LEAF.bak" "$LEAF" ;;
    esac

    # Relocate .prof/.hp if produced
    if [[ "$PROFILE_MODE" == "profile" && -f "$PR_ADMIN_DIR/purs.prof" ]]; then
        local TS PROF_TARGET
        TS=$(date +%Y%m%d-%H%M%S)
        PROF_TARGET="$EXPERIMENTS_DIR/$EXP_ID/profiles/${LABEL}-${SCENARIO}-${TS}.prof"
        mkdir -p "$(dirname "$PROF_TARGET")"
        mv "$PR_ADMIN_DIR/purs.prof" "$PROF_TARGET"
        log "profile → $PROF_TARGET"
        [[ -f "$PR_ADMIN_DIR/purs.hp" ]] && mv "$PR_ADMIN_DIR/purs.hp" "${PROF_TARGET%.prof}.hp"
    fi

    # ONLY this reaches stdout — the integer ms reading.
    echo "$ELAPSED_MS"
}

################################################################################
# Multi-run orchestration for one scenario. Discards run 1 as warm-up.
################################################################################

# Writes "median min max n" to stdout.
run_scenario() {
    local PURS_BIN="$1"
    local SCENARIO="$2"
    local RUNS="$3"
    local PROFILE_MODE="$4"
    local LABEL="$5"

    local -a RESULTS=()
    for ((i=1; i<=RUNS; i++)); do
        # `nochange` needs a populated output/ before the first timed run.
        if [[ "$SCENARIO" == "nochange" && $i -eq 1 ]]; then
            log "  (populating output/ with a full build before first nochange run)"
            run_once "$PURS_BIN" "full" "" "$LABEL" >/dev/null
        fi

        local T
        T=$(run_once "$PURS_BIN" "$SCENARIO" "$PROFILE_MODE" "$LABEL")
        if [[ $i -eq 1 ]]; then
            log "  run $i: ${T} ms (warm-up — discarded)"
        else
            log "  run $i: ${T} ms"
            RESULTS+=("$T")
        fi
    done

    if [[ ${#RESULTS[@]} -eq 0 ]]; then
        echo "0 0 0 0"
        return
    fi
    local SORTED N MIN MAX MEDIAN MID
    SORTED=$(printf '%s\n' "${RESULTS[@]}" | sort -n)
    N=${#RESULTS[@]}
    MIN=$(echo "$SORTED" | head -1)
    MAX=$(echo "$SORTED" | tail -1)
    MID=$(( (N + 1) / 2 ))
    MEDIAN=$(echo "$SORTED" | sed -n "${MID}p")
    echo "$MEDIAN $MIN $MAX $N"
}

################################################################################
# Results file appender (markdown table).
################################################################################

append_result_row() {
    local RESULTS_FILE="$1"
    local DATE="$2"
    local SCENARIO="$3"
    local BASE_SHA="$4"
    local HEAD_SHA="$5"
    local BASE_MED="$6"
    local HEAD_MED="$7"
    local DELTA_PCT="$8"
    local NOTES="$9"

    if [[ ! -f "$RESULTS_FILE" ]]; then
        local DIR_NAME
        DIR_NAME=$(basename "$(dirname "$RESULTS_FILE")")
        cat > "$RESULTS_FILE" <<EOF
# Results for $DIR_NAME

Append-only. See experiments/SCHEMA.md for format.

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
EOF
    fi

    local BASE_S HEAD_S
    BASE_S=$(awk -v ms="$BASE_MED" 'BEGIN { printf "%.1f", ms/1000 }')
    HEAD_S=$(awk -v ms="$HEAD_MED" 'BEGIN { printf "%.1f", ms/1000 }')

    printf '| %-10s | %-8s | %-12s | %-8s | %8s | %8s | %7s | %s |\n' \
        "$DATE" "$SCENARIO" "${BASE_SHA:--}" "${HEAD_SHA:--}" "$BASE_S" "$HEAD_S" "$DELTA_PCT" "$NOTES" \
        >> "$RESULTS_FILE"
}

################################################################################
# Top-level `run` command.
################################################################################

EXP_ID=""
VARIANT="head"
PURS=""
BASELINE_PURS=""
SCENARIOS="full"
RUNS=5
PROFILE_MODE=""
BASE_SHA=""
HEAD_SHA=""
NOTES=""
SHA_ARG=""

cmd_run() {
    [[ -n "$EXP_ID" ]] || error "--experiment <id> is required"
    [[ -n "$PURS" ]] || error "--purs <path> is required"
    [[ -x "$PURS" ]] || error "not executable: $PURS"
    [[ -z "$BASELINE_PURS" || -x "$BASELINE_PURS" ]] || error "not executable: $BASELINE_PURS"

    local EXP_DIR="$EXPERIMENTS_DIR/$EXP_ID"
    mkdir -p "$EXP_DIR"
    local RESULTS_FILE="$EXP_DIR/results.md"
    local DATE
    DATE=$(date '+%Y-%m-%d')

    if [[ "$SCENARIOS" == "all" ]]; then
        SCENARIOS="full,nochange,prelude,leaf"
    fi

    log "experiment=$EXP_ID variant=$VARIANT"
    log "purs=$PURS"
    [[ -n "$BASELINE_PURS" ]] && log "baseline-purs=$BASELINE_PURS"
    log "scenarios=$SCENARIOS runs=$RUNS (run 1 discarded)"
    log "baseline_sha=${BASE_SHA:-unset} head_sha=${HEAD_SHA:-unset}"

    IFS=',' read -r -a SCN_LIST <<< "$SCENARIOS"
    for SCN in "${SCN_LIST[@]}"; do
        log "=== scenario: $SCN ==="

        local BASE_MED="" HEAD_MED="" HEAD_MIN HEAD_MAX HEAD_N
        if [[ -n "$BASELINE_PURS" ]]; then
            log "[baseline] running..."
            read -r BASE_MED _ _ _ <<< "$(run_scenario "$BASELINE_PURS" "$SCN" "$RUNS" "" "baseline")"
            log "[baseline] median=${BASE_MED} ms"
        fi

        log "[$VARIANT] running..."
        read -r HEAD_MED HEAD_MIN HEAD_MAX HEAD_N <<< "$(run_scenario "$PURS" "$SCN" "$RUNS" "$PROFILE_MODE" "$VARIANT")"
        log "[$VARIANT] median=${HEAD_MED} ms (min=${HEAD_MIN}, max=${HEAD_MAX}, n=${HEAD_N})"

        local DELTA_PCT="n/a"
        if [[ -n "$BASE_MED" && "$BASE_MED" -gt 0 ]]; then
            DELTA_PCT=$(awk -v b="$BASE_MED" -v h="$HEAD_MED" \
                'BEGIN { printf "%+.1f%%", (h - b) * 100 / b }')
        fi

        local ROW_NOTES="median of $((RUNS-1)), ${HEAD_MIN}-${HEAD_MAX} ms"
        [[ -n "$NOTES" ]] && ROW_NOTES="$ROW_NOTES; $NOTES"

        append_result_row "$RESULTS_FILE" "$DATE" "$SCN" \
            "$BASE_SHA" "$HEAD_SHA" "${BASE_MED:-0}" "$HEAD_MED" "$DELTA_PCT" "$ROW_NOTES"
    done

    log "results appended → $RESULTS_FILE"
}

################################################################################
# `build`, `build-profiled`, and `build-baseline`.
################################################################################

get_current_purs() {
    local BASE
    BASE=$(stack path --local-install-root 2>/dev/null) || return 1
    echo "$BASE/bin/purs"
}

get_profiled_purs() {
    local BASE
    BASE=$(stack path --local-install-root --profile 2>/dev/null) || return 1
    echo "$BASE/bin/purs"
}

cmd_build() {
    log "stack build (optimised)"
    (cd "$REPO_ROOT" && stack build --system-ghc)
    log "purs → $(get_current_purs)"
}

cmd_build_profiled() {
    log "stack build --profile"
    (cd "$REPO_ROOT" && stack build --profile --system-ghc)
    log "purs (profiled) → $(get_profiled_purs)"
}

cmd_build_baseline() {
    [[ -n "$SHA_ARG" ]] || error "--sha <sha-or-branch> required"

    local TMP_WT="/tmp/purs-baseline-$SHA_ARG"
    if [[ -d "$TMP_WT" ]]; then
        git -C "$REPO_ROOT" worktree remove --force "$TMP_WT" || true
    fi

    log "creating detached worktree at $TMP_WT"
    git -C "$REPO_ROOT" worktree add --detach "$TMP_WT" "$SHA_ARG"

    local RESOLVED_SHA
    RESOLVED_SHA=$(git -C "$TMP_WT" rev-parse --short HEAD)

    log "building from $RESOLVED_SHA"
    (cd "$TMP_WT" && stack build --system-ghc)

    local SRC_PURS
    SRC_PURS=$(cd "$TMP_WT" && stack path --local-install-root)/bin/purs

    local DEST="$BASELINES_DIR/$RESOLVED_SHA"
    mkdir -p "$DEST"
    cp "$SRC_PURS" "$DEST/purs"
    log "baseline binary → $DEST/purs"

    git -C "$REPO_ROOT" worktree remove --force "$TMP_WT"

    # Append to manifest (insert below header).
    local MANIFEST="$BASELINES_DIR/manifest.md"
    local GHC_VER STACK_RES TS
    GHC_VER=$(ghc --version 2>/dev/null || echo unknown)
    STACK_RES=$(grep '^resolver:' "$REPO_ROOT/stack.yaml" 2>/dev/null | head -1 | awk '{print $2}' || echo unknown)
    TS=$(date -u '+%Y-%m-%d %H:%M UTC')

    if [[ -f "$MANIFEST" ]] && grep -q '^| ---' "$MANIFEST"; then
        awk -v row="| $RESOLVED_SHA | $SHA_ARG | $TS | $GHC_VER | $STACK_RES | $(whoami) | new |" '
            /^\| ---/ && !inserted { print; print row; inserted=1; next }
            { print }
        ' "$MANIFEST" > "$MANIFEST.tmp" && mv "$MANIFEST.tmp" "$MANIFEST"
    fi
    log "manifest updated → $MANIFEST"
}

################################################################################
# Argument parsing
################################################################################

[[ $# -ge 1 ]] || usage
CMD="$1"; shift

if [[ "$CMD" == "-h" || "$CMD" == "--help" || "$CMD" == "help" ]]; then
    usage
fi

while [[ $# -gt 0 ]]; do
    case "$1" in
        --experiment)       EXP_ID="$2"; shift 2;;
        --variant)          VARIANT="$2"; shift 2;;
        --purs)             PURS="$2"; shift 2;;
        --baseline-purs)    BASELINE_PURS="$2"; shift 2;;
        --scenarios)        SCENARIOS="$2"; shift 2;;
        --runs)             RUNS="$2"; shift 2;;
        --profile)          PROFILE_MODE="profile"; shift;;
        --baseline-sha)     BASE_SHA="$2"; shift 2;;
        --head-sha)         HEAD_SHA="$2"; shift 2;;
        --notes)            NOTES="$2"; shift 2;;
        --sha)              SHA_ARG="$2"; shift 2;;
        -h|--help)          usage;;
        *) error "unknown option: $1";;
    esac
done

case "$CMD" in
    run)             cmd_run;;
    build)           cmd_build;;
    build-profiled)  cmd_build_profiled;;
    build-baseline)  cmd_build_baseline;;
    *) error "unknown command: $CMD";;
esac

# ----------------------------------------------------------------------
# KNOWN-BUGS from legacy script (all addressed in this rewrite):
# 1) PATH shim silently ignored by spago build. spago 0.21 resolved
#    `purs` from pr-admin/node_modules/.bin/purs before honoring our
#    PATH prefix. Fix: bypass spago build entirely; call
#    `purs compile $(spago sources)` ourselves.
# 2) 1-second bogus timings. $SECONDS is integer-second resolution.
#    Fix: date +%s%N for millisecond precision.
# 3) Log output contaminating captured value. `log` wrote to stdout
#    alongside the timing return. Fix: log to stderr; only numeric
#    results reach stdout.
# 4) Baseline/variant interleaved. Legacy alternated baseline-var-
#    baseline-var, letting caches leak between them. Fix: run all
#    iterations of one binary back-to-back; explicit scenarios for
#    warm vs cold states.
# ----------------------------------------------------------------------
