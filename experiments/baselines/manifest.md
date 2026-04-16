# Baseline binary manifest

Each row records a pre-built baseline `purs` binary stored at
`experiments/baselines/<short-sha>/purs` (not committed — the binary
is ~170 MB, rebuild from the SHA with `exp build-baseline <sha>`).

Experiments reference baselines by `short-sha` in their
`EXPERIMENT.md` frontmatter (`baseline_sha:` field).

| Short SHA | Branch at build | Built (UTC)          | GHC   | Stack resolver | Built by | Notes                  |
| --------- | --------------- | -------------------- | ----- | -------------- | -------- | ---------------------- |
| 3fcac773  | restaumatic     | 2026-04-15 19:11 UTC | 9.6.6 | lts-22.43      | user     | current restaumatic tip |

_Entries sorted most-recent-first._

