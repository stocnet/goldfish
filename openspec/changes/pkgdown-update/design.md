# Design — pkgdown-update

## Context

`_pkgdown.yml` groups the reference index into Make / Estimate /
Diagnostics / Data / Gather preprocessing sections, mixing explicit topic
names and `starts_with()` patterns. The 2026-07-25 audit
(`.plan/Naming_guidelines.md` §8) found: no `test_*` pattern (the
residuals-gof `test_gof`/`test_parameter`/`test_time` will be homeless),
no `evaluate_model` entry, `augment` unindexed (wiring fixed in
residuals-gof task 2.5), and no gate that examples run or stay fast.
Deprecated/defunct topics are `@keywords internal`, so `check_pkgdown()`
exempts them. The repo has a tracked-ledger precedent
(`.plan/goldfish_versions.csv`) and a local-only `.plan` default.

## Goals / Non-Goals

**Goals:**

- Reference index complete and future-proofed for the residuals-gof
  surface; `pkgdown::check_pkgdown()` green.
- A repeatable examples gate: all examples run, per-topic timings
  recorded, slow topics wrapped deliberately.

**Non-Goals:**

- No article/vignette restructuring, no theming, no new exported code.
- Not the CRAN pre-flight itself (`release-prep` consumes this gate).

## Decisions

### D1 — Index by pattern where the family is open-ended

"Diagnostics" gains `starts_with("test")` and `evaluate_model` (explicit —
a one-off name), so `diagnose_onset` (covered by the existing
`starts_with("diagnose")`) and any future `test_*` land indexed by
construction. The re-exported broom generics stay unindexed (standard
pkgdown practice for the reexports topic); the `augment` method topic
joins the post-estimation entries once residuals-gof re-wires it.
Alternative rejected: enumerating every test function explicitly — the
pattern is the same convention the yaml already uses for `estimate`/
`set`/`make`/`diagnose`.

### D2 — Examples gate = run + time in one pass

One script-free discipline: `devtools::run_examples()` (runs `\donttest`
locally) under a timer per topic, appending
`topic, seconds, date, version` rows to `.plan/example_timings.csv`.
The ledger stays **local-only** (unlike the versions csv): timings are
machine-dependent instruments, not shared evidence; the shared contract
is the ~5s policy, not the numbers. Topics over ~5s wrap the slow block
in `\donttest` with a one-line rationale comment (why it is slow, what
the fast part still shows), never by deleting the example.

### D3 — Sequenced after residuals-gof phase 2

`check_pkgdown()` fails on index entries whose topics do not exist, so
the yaml edit cannot precede the residuals-gof man pages. Applying after
also means the gate's first full run times the new diagnostics examples —
the ones most likely to be slow (estimation inside examples).

## Risks / Trade-offs

- [`starts_with("test")` could catch an unintended future export] → the
  naming guidelines reserve `test_*` for statistical tests (§1); anything
  else is a naming violation caught earlier.
- [Timings vary by machine] → ledger is local instrumentation; the
  policy threshold (~5s, CRAN's informal bar) is what specs enforce.
- [`run_examples()` mutates the session] → run in a fresh subprocess
  (callr / separate Rscript), matching how the test gate runs.

## Open Questions

- None blocking.
