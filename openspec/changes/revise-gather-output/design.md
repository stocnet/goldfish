# Design — revise-gather-output

## Context

Two exported functions overlap: `gather_model_data()`
(R/preprocess_export.R) wraps `compute_stats(..., output = "gather")`
(R/model_estimate.R:532) but re-validates with its own narrower `match.arg`
(`choice`/`choice_coordination`/`rate`; models DyNAM/REM only), while
`compute_stats()` → `estimate_wrapper()` already validates the full
per-model vocabulary via `check_model_par()` (model_estimate.R:955-964:
DyNAM = rate/rate_ordered/choice/choice_coordination, REM =
rate/rate_ordered/choice, DyNAMi = choice/rate) and hosts the REM
`"choice"` deprecation warn+remap. Verified empirically:
`compute_stats(REM, rate_ordered, output = "gather")` produces the correct
ordinal stack today; `gather_model_data()` rejects the same call.
`check_model_par()` (R/class_checks.R:1083) is base `stop()`, pre-cli.
DyNAMi preprocessing runs through an isolated legacy front-end
(model_estimate.R:763-770, "fenced so the shared recipe path is
DyNAMi-free") — the gather writer lives in the shared path, so DyNAMi
gather support requires routing work. Flavored specifications already have
an identity convention: preprocessing returns fid-indexed
`preprocessed.goldfish` lists with a `process_map` table attribute (fid,
layer, flavor, family, stat_block, has_intercept, constraint_id), and the
estimation container renders labels from that map (flavored-processes
living spec). The residuals-gof change (active) defines the replay surface
this function feeds: consumers accept `preprocessed =`, and its
diagnostic-primitives guiding error currently names
`estimate_*(..., preprocessing_only = TRUE)` as the supply route.
A repo-wide naming pass (`algorithm-naming`, memory note 2026-07-24) runs
AFTER this change settles; interim decisions here are scoped to the new
function's own signature.

## Goals / Non-Goals

**Goals:**

- One statistics-product function, `compute_statistics()`, covering
  everything `gather_model_data()` does plus frames; deprecations for the
  old surfaces.
- Vocabulary correctness by delegation; `check_model_par()` on cli.
- DyNAMi coverage; flavored fid-list outputs with `process_map`.
- Reported intercept/censoring semantics; frame contract; verified
  cross-package recipes as examples.

**Non-Goals:**

- Renaming estimator arguments or any surface beyond the new function
  (deferred to `algorithm-naming`, which re-checks all names once this
  change settles).
- Changes to the gather stack format or writer strategy contract.
- NCC sampling, remstats-style arrays, simulation exports.

## Decisions

### D1 — `compute_statistics()` absorbs `gather_model_data()`; asymmetric retirement of the old names

`compute_stats()` is renamed `compute_statistics()` (precision; full-word
naming). Signature: `compute_statistics(x, model, sub_model = NULL,
data = NULL, output = c("preprocessed", "gather", "data.frame", "db"),
control_prep = set_preprocessing(), progress, max_length = 63L, ...)` —
first argument `x` (formula or specification, matching `estimate_*()`),
model selectors before `data` (the `estimate_*(x, sub_model, data)`
family order), and the final control names from `algorithm-naming`,
which implements BEFORE this change (2026-07-24 alignment session).
Retirement is asymmetric, keyed on release exposure:

- `gather_model_data()` **shipped in released versions** (the 1.7 rename
  line) → lifecycle soft-deprecated wrapper for at least one release cycle
  (`deprecate_soft`, direct pointer, no two-hop chains).
- `compute_stats()` **only ever existed inside the unreleased 2.0.0
  development line** (introduced in a NEWS dev section; never on CRAN) →
  **deleted outright** (decided 2026-07-24; supersedes the brief
  defunct-stub variant — a stub is still kept code, and there is no
  released user it would serve): removed from NAMESPACE and source, no
  stub; calling it yields R's standard could-not-find-function error, and
  the NEWS entry records the rename so dev-line scripts can grep their way
  to `compute_statistics()`. This is a deliberate exception to the
  deprecate-soft-at-2.0.0 policy, which exists to protect released
  users — `compute_stats` has none.

Alternative rejected: keeping the layered two-function split (previous
D1) — the explore session settled on one precise function; presentation
layers multiply doc surfaces and the vocabulary drift was caused by
exactly such a wrapper. Coordination: `algorithm-naming` implements
first, so `set_preprocessing()` and the `control_prep` vocabulary exist
when this function is born — it arrives fully final and
`algorithm-naming` has no code task against it; it does not touch the
`compute_stats` lifecycle.

### D2 — Vocabulary by delegation; `check_model_par()` upgraded to cli

`compute_statistics()` performs no local `match.arg` on model/sub_model;
validation happens once in `estimate_wrapper()` via `check_model_par()`,
which is upgraded (touched-internal migration) to `cli_abort` listing the
allowed sub_models for the given model. The REM `"choice"` deprecation
message at its single site (`estimate_wrapper()`) names both successors:
`"rate"` (exact-time) and `"rate_ordered"` (ordinal). `sub_model = NULL`
keeps the current defaulting (REM → "rate", else "choice"), now documented
together with its intercept consequence. No-drift-by-construction: there is
no second vocabulary to fall out of sync.

### D3 — Intercept/censoring semantics inherited and reported at finalization

`rate` (exact-time): force-added time intercept, right-censored rows with
`timespan`; `rate_ordered`: no intercept, no censored rows, formula `1`
dropped with the estimator's message — all inherited from the shared
parsing path, never reimplemented. `has_intercept` and `right_censored`
fields are attached in `finalize_gather_output()`, so every output form
and every entry point carries them; for flavored outputs they mirror the
`process_map` columns. Documentation mandates name-based statistic-column
access (the positional trap).

### D4 — Flavored outputs are fid-indexed lists with `process_map`

On a flavored specification, every `output` form returns a list indexed by
integer fid carrying the same `process_map` table attribute as flavored
preprocessing and the estimation container — the identity authority.
Display labels (messages, print) are rendered from the process_map, never
parsed back from list keys. Alternative rejected: composite string names
(`submodel_layer_flavor`) as list keys — the flavored-processes spec
explicitly forbids key-parsing, and fid+map is what `estimate_*` already
returns; one keying convention across the package.

### D5 — DyNAMi is covered, with the front-end routing verified first

`compute_statistics()` SHALL support `model = "DyNAMi"` for the outputs
its engines can produce. Because the DyNAMi front-end is fenced off the
shared recipe path, the first implementation task verifies whether the
gather writer can be routed (or the legacy output post-converted) and
records the answer; if a DyNAMi output form is genuinely unavailable, it
aborts with a cli error naming the supported forms — never a silent wrong
result. (DyNAMi's joining/leaving structure is extra parameters within one
fit, not fid flavors — its output is a single stack.)

### D6 — Frame output (`output = "data.frame"`) and example recipes

The long base data frame: `event` (integer), `chosen` (0/1), `sender`,
`receiver` (labels; NA where not applicable), `index_i`, `index_j`
(1-based), `timespan` (exposure; NA for multinomial rows), `is_dependent`
(FALSE = right-censored row), then statistic columns named by
`namesEffects`; `effect_description` as attribute; per-fid list under
flavoring (D4). Help-page recipes (verified in
`.plan/residuals_comparison.qmd`): ordinal ↔ `coxph(Surv(rep(1, n),
chosen) ~ stats + strata(event))` and `clogit` (one case per stratum ⇒
tie methods coincide ⇒ exact conditional logit); exact-time ↔
`glm(chosen ~ stats + offset(log(timespan)), poisson)` over dependent +
right-censored rows; choice ↔ `mlogit` via `dfidx` with `option = index_j`
(real alternative identity). Guarded with `@examplesIf
requireNamespace(...)`, fits in `\donttest`.

### D7 — Final naming from birth; this change owns the `preprocessing_only` retirement

`compute_statistics()` is born under the final vocabulary
(`control_prep = set_preprocessing()`, `x`-first signature; the interim
`control_preprocess` spelling is superseded — 2026-07-24 alignment
session, `algorithm-naming` implements first). The estimator control
arguments (`control_estimation` → `control_algo`, `control_preprocessing`
→ `control_prep`, `preprocessing_init` → `preprocessed`) are renamed by
`algorithm-naming`, NOT here. One estimator flag is retired HERE because
its replacement is this function: `preprocessing_only = TRUE` is
soft-deprecated with a warning naming
`compute_statistics(output = "preprocessed")` (through 2.x it still
returns the preprocessed object) — deprecating it in `algorithm-naming`
would have pointed users at a function that did not exist yet.

### D8 — Replay-surface coherence with residuals-gof

`compute_statistics(output = "preprocessed")` IS the preprocessed replay
object residuals-gof's consumers take via `preprocessed =` — the value
names the class it returns and mirrors the argument it feeds (chosen over
`"default"`, which leaned on undocumented engine vocabulary, and
`"compact"`, which requires knowing the delta/broadcast internals). The
diagnostic-primitives guiding-error wording in the residuals-gof change is
updated (before its task 1.8 implements it) to name
`compute_statistics(output = "preprocessed")` as the supply route
alongside `return_preprocessed = TRUE` (the flag's post-algorithm-naming
name). This change adds no second replay path; `algorithm-naming` dropped
its `make_preprocessed()` sketch in favor of this single route
(2026-07-24 alignment session).

## Risks / Trade-offs

- [Retiring exported names] → `gather_model_data()` keeps working one
  cycle under `deprecate_soft` with a direct mapping; `compute_stats()` is
  deleted with no stub — justified by zero release exposure
  (dev-line-only) and recorded in NEWS; dev-line scripts fail loudly at
  the call site (could-not-find-function), never silently.
- [DyNAMi routing unknowns] → D5's verify-first task; cli error rather
  than silent unsupported output; scope can shrink to a documented
  limitation without touching the rest.
- [Flavored gather/frames volume (per-fid stacks)] → same order as flavored
  preprocessing already produces; row-count formula documented per fid; db
  writer remains the out-of-memory route.
- [Estimator-frame drift despite delegation] → parity test: the qmd
  identity route (clogit on the frame vs `estimate_dynam` choice) becomes
  a unit test with a ≤1e-4 gate.
- [Cross-change edit inside residuals-gof] → one wording edit in an
  unimplemented requirement; validated in both changes after the edit.

## Migration Plan

1. `compute_statistics()` rename + delegation + cli `check_model_par` +
   reported fields (D1–D3); deprecation wrappers.
2. Flavored fid-list outputs (D4); DyNAMi verification then routing (D5).
3. Frame output + examples + NEWS/DESCRIPTION (D6); residuals-gof wording
   edit (D8); qmd consumer switch.
4. Rollback: wrappers are self-contained; the rename is alias-backed, so
   reverting restores the current surface without preprocessing changes.

## Open Questions

- None blocking. (Suggests entries for survival/mlogit decided
  mechanically by R CMD check on the guarded examples; final names
  re-checked by `algorithm-naming` once this settles.)
