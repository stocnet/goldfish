# Fit classes share a parent, governed by a contract table

## Why

goldfish has two fitted-model classes and is about to gain a third, and they do
not inherit from one another. `estimate_flavored()` builds its result as a flat
`flavored_result.goldfish` — not `c("flavored_result.goldfish",
"result.goldfish")` — and the object is a *container*: per-fid fits plus
`process_map`, `model`, `layer`, `flavors` and `call`. A DyNES fit from the
`abmcem` loop would be a third such class.

The cost of flatness is already measurable. **Sixteen generics are implemented
twice**, once per class. And the leak has already happened: `summary`, `tidy`,
`glance` and `print.summary` exist **only** for `result.goldfish`, so flavored
fits silently lack all four. Nobody decided that — it is what happens when
every new generic must be written twice and one of them is forgotten. A third
flat class makes it three times, and the next omission is a matter of when.
(`coef_layout` runs the other way, existing only on the flavored class.)

Naive inheritance is dangerous in a specific way, which is why this needs a
decision rather than a parent class. A DyNES fit maximizes a **Monte-Carlo
estimate** of the observed-data likelihood. If it inherits `logLik()` from a
parent, `AIC()` and `BIC()` reach that Monte-Carlo quantity through the
standard route and return numbers that look like information criteria. The same
applies to anything assuming an exact likelihood. These are not inconveniences;
they are wrong answers wearing the right shape, and they are worse than the
duplication because nothing errors.

This work has an accepted decision behind it (ADR-0038, 2026-08-21) that named
`class-naming-scheme` as its home. That change never picked it up — it carries
no task, requirement or decision for the hierarchy — and has since explicitly
disclaimed it (its D19), because a rename that also introduced inheritance
would answer this question silently. This change is that home.

## What Changes

- **A shared parent class** for the fitted-model classes, so a generic written
  once reaches every fit unless a class is deliberately excused.
- **A checked-in contract table** — one row per generic, one column per
  concrete fit class, each cell `inherit`, `override` or `refuse`:
  - `inherit` — the parent's method is correct for this class;
  - `override` — this class needs its own, for a stated reason;
  - `refuse` — the generic is **not** meaningful here and SHALL abort with an
    explanation, rather than returning a plausible number.
- **A test that fails on an undecided cell**, so a new generic or a new fit
  class cannot be added without a decision for every combination. This is what
  stops the leak recurring rather than fixing it once.
- **The four leaked generics** (`summary`, `tidy`, `glance`, `print.summary`)
  gain a decision — most likely `inherit` — which is what closes the gap that
  motivated the ADR.
- **`logLik` for a Monte-Carlo fit is decided explicitly**, and with it the
  downstream `AIC()`/`BIC()` behavior.
- **Not in scope**: the six mechanically identical flavored fan-outs
  (`fitted`, `predict`, `residuals`, `evaluate_model`, `coef`, `vcov`), which
  are a duplication problem handled by `model-spec-descriptor` D10; and class
  *naming*, which is `class-naming-scheme`'s.

## Capabilities

### New Capabilities

- `fit-class-hierarchy`: the shared parent, the contract table and its three
  verdicts, the requirement that every generic-by-class cell be decided, and
  the rule that a meaningless generic refuses rather than returning a value.

### Modified Capabilities

- `diagnostic-object-contract`: the diagnostic generics dispatching on fit
  classes gain their contract-table rows, so what a flavored or DyNES fit
  returns from each is stated rather than implied by which method happens to
  exist.

## Impact

- **Code**: `R/methods_display.R`, `R/methods_postestimate.R`,
  `R/methods_predict.R`, `R/methods_residuals.R`, `R/methods_tests.R`,
  `R/model_evaluate.R`, `R/diagnostic_tables.R`, `R/diagnose_onset.R`, plus the
  fit constructors in `R/model_estimate.R` and `R/estimate_flavored.R`.
- **Tests**: a new contract-table completeness test; new tests for the four
  previously missing generics on flavored fits; snapshots for every `refuse`
  message.
- **Users**: flavored fits gain `summary`, `tidy` and `glance`. Any generic
  decided `refuse` for a class starts aborting where it previously errored with
  R's "no applicable method" — a better message for the same non-result.
- **Not affected**: `src/`, the frozen 1e-6 baselines (no arithmetic changes),
  and the exported function names.
- **Sequencing**: after `class-naming-scheme` (which renames these classes) and
  ideally after `model-spec-descriptor` D10 (which removes six rows from the
  table before it is written). Before `abmcem` ships the DyNES fit, since the
  third class is what makes the omission permanent.
