# Archive

Historical artefacts from the pre-framework era. Kept for reference
but not actively updated.

## `profile-results-2025-12-to-2026-04.log`

Old append-only log from the legacy `run-profile.sh` at the repo root.
Covers runs between 2025-12-26 and 2026-04-11. Format is the legacy
one (TIMESTAMP | VERSION | RUN# | TIME(s) | NOTES), which has several
known issues:

- Spurious 1-second entries (PATH shim ignored by spago, see
  `experiments/scripts/run-profile.sh` KNOWN-BUGS section)
- Subprocess log output occasionally contaminating the TIME field
  (multi-line entries around lines 12–14, 18–19, 45–46)
- No commit SHA column, no scenario column
- Integer-second resolution, so anything under ~1 second reads as 0

Deltas in this log (e.g. "-80.7%" for the rust-interning phase 2
variant) should **not** be trusted without cross-referencing against
the per-experiment `EXPERIMENT.md` and `LESSONS.md`, which capture
follow-up measurements that invalidated the initial numbers.
