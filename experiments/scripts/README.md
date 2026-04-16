# experiments/scripts

Profiling harness and experiment-driver scripts.

## `exp`

Main driver. Wraps `run-profile.sh` and adds worktree/experiment
lifecycle management.

```
exp new <id> [--from <baseline-sha>]
    Create branch <id>, worktree /workspace/p/<id>, and
    experiments/<id>/{EXPERIMENT.md,TASK.md,HANDOFF.md,results.md}
    scaffolds.

exp build-baseline <sha-or-branch>
    Build an optimised purs from the given ref into
    experiments/baselines/<short-sha>/purs and append to manifest.md.

exp run <id> [--scenarios all] [--runs 5] [--profile]
    Run baseline + current across the chosen scenarios and append to
    experiments/<id>/results.md.

exp report [<id>]
    Print the per-experiment results table, or the global index.

exp close <id> --verdict win|partial|no-win|abandoned
    Set frontmatter to closed, prompt for a LESSONS.md entry.
```

## `run-profile.sh`

Lower-level timing harness. Invoked by `exp run`. Can also be run
standalone for ad-hoc measurements.

## `purs-profiled.sh`

PATH shim used when `--profile` is requested. Wraps the profiled
`purs` binary with `+RTS -p -hc -RTS` so spago-invoked builds produce
cost-centre + heap profiles.
