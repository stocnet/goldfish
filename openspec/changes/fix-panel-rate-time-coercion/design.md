## Context

`complete_generative_spec()` (landed by `make-multivariate-spec`, complete but not yet
archived) fills a generatively-incomplete flavor's missing rate with a *pinned*
intercept-only rate (`intercept-only-rate-spec` primitive). For a **timed** joint
composition, `pin_completed_rates()` derives the pin's `(count_w, T_w, |R_w|)` per
completed fid and hands them to `pin_intercept_only_rate()`, which requires all three
to be plain numeric.

Today `pin_completed_rates()` always sources those numbers from
`panel_wave_risk_set()` — a state-diff-over-a-wave-grid helper written for **panel**
layers. Two bugs live inside it:

1. `duration <- diff(wave_times)` returns a `difftime` object whenever `wave_times` is
   POSIXct/Date (the common case for real event data), and `is.numeric.difftime` is
   `FALSE` by R's own definition — so this always fails downstream, just not on any of
   the package's current numeric-time test fixtures.
2. `default_window()`, the single-window fallback used when no `wave_times` grid is
   supplied, ranges over `data$ties$time` **unfiltered by layer**, even though its only
   caller always has the target `layer` in scope.

A third, adjacent implementation gap: the living requirement text (and a standalone
unit test) already describe a **second** risk-set source, `relational_window_risk_set()`,
for a fully-observed relational layer (using goldfish's own `n_dep_events` /
`total_time` / `avg_active_entity` preprocessing scalars instead of a synthetic
wave-endpoint Hamming diff). `pin_completed_rates()` never calls it — every completed
rate, panel or relational, goes through `panel_wave_risk_set()`.

## Goals / Non-Goals

**Goals:**
- Make `panel_wave_risk_set()` (and its `default_window()` fallback) produce plain
  numeric `(count, duration, risk_set_size)` regardless of whether the underlying event
  time is numeric, POSIXct, or Date.
- Scope the single-window fallback to the layer actually being pinned.
- Dispatch a completed rate's risk-set source correctly between
  `panel_wave_risk_set()` and `relational_window_risk_set()`, for the cases
  `relational_window_risk_set()` can actually support today.

**Non-Goals:**
- Extending `relational_window_risk_set()` to accept a `flavor` argument so it can
  serve a **flavored** relational (event-observed) layer's per-flavor rate gap. Its
  current construction builds an unflavored `make_specification(rate = <bare formula>,
  ...)`, and a bare formula on a flavored layer models *every* row as one undifferentiated
  process (see `make_specification.R`'s `inform_unkeyed_flavored_layer`), not the single
  gap flavor. Threading flavor-scoping through `relational_window_risk_set()` and
  `estimate_dynam(..., preprocessing_only = TRUE)` is a larger change than this bug fix
  warrants (see D3, and Open Questions).
- Changing `pin_intercept_only_rate()`'s numeric validation, or anything in
  `R/intercept_only_rate.R`. That guard is correct — it is exactly what caught this bug.
- Reconciling mixed POSIXct-vs-Date time axes across streams; that is an existing,
  system-wide invariant enforced at data construction (`validate_goldfish.R`'s
  `time_axis()`/`check_time_axis`), orthogonal to this fix.

## Decisions

### D1 — Coerce time with the existing `coerce_time()` helper, not a new one

`R/state_at.R` already defines `coerce_time()` (character → POSIXct → `as.numeric()`,
POSIXct/Date/numeric → `as.numeric()` directly) and `network_state_at()` already relies
on it to accept "numeric / POSIXct / Date / character times, returning a numeric axis."
`panel_wave_risk_set()` already calls `network_state_at()` per wave boundary — it is the
one function in the file that *doesn't* also normalize its own copy of those boundaries
before doing arithmetic (`diff()`) on them.

Fix: coerce `wave_times` to numeric via `coerce_time()` once, at the top of
`panel_wave_risk_set()`, before it's used for both `network_state_at()` calls and
`diff()`. This reuses the package's one existing time-normalization convention
(matching `class_checks.R:121`, `event_streams.R:36`) instead of inventing a second one,
and keeps the numeric axis internally consistent — `diff()` and `network_state_at()`
now operate on the same normalized values instead of `network_state_at()` re-deriving
its own via `coerce_time()` internally on the raw POSIXct while `diff()` acts on the
un-coerced original.

**Alternative considered:** coerce only at the `diff()` call site
(`as.numeric(diff(wave_times))`). Rejected — `diff()` on POSIXct returns a `difftime`
already carrying a `units` attribute (days, secs, ...) that varies with the *magnitude*
of the gap (R's `difftime` picks a "nice" unit per call), so a bare `as.numeric()` on
that result would silently return whatever unit R chose rather than a fixed one. Coercing
`wave_times` itself to numeric seconds *before* `diff()` avoids that ambiguity entirely —
the subtraction happens in already-numeric seconds, with no unit inference involved.

### D2 — Thread `layer` into `default_window()`

`default_window(data)` becomes `default_window(data, layer)`, filtering
`as.data.frame(data$ties)$time` to `ties$layer == layer` before taking the range. Its
only caller (`panel_wave_risk_set()`) already receives `layer` as an argument, so this
is a pure narrowing with no new plumbing.

### D3 — Dispatch panel vs. relational, scoped to what each helper actually supports

`pin_completed_rates()` gains a per-fid dispatch:

```
relational iff  !(row$layer %in% joint_spec$modeled_panel)  &&  is.na(row$flavor)
```

- **Modeled panel layer** (any flavor, or none): `panel_wave_risk_set()`, unchanged
  shape — this is its original, correct use case. (The vignette's `friendship_spec`,
  though *built* under an `event_declared` construction trick to clear
  `make_specification()`'s panel-focal guard, is *joined* against panel-declared data,
  so `modeled_panel` includes it and it lands here — not on the flavored-relational
  fallback below.)
- **Unflavored relational layer** (`row$flavor` is `NA`, layer not in `modeled_panel`):
  `relational_window_risk_set()` — the path the living requirement text already
  documents and a standalone test already exercises, now actually reachable.
- **Flavored relational layer** (event-observed at *join* time — the layer's
  `observation` is `"event"` in the `data` handed to `make_joint_specification()` — yet
  its process is flavored): falls through to `panel_wave_risk_set()` as a conservative
  default. `network_state_at()` materializes state from `time = NA` history plus timed
  updates regardless of the layer's `observation` declaration, so the
  wave-endpoint/Hamming-diff mechanics remain valid (not wrong) here — just less exact
  than the true continuous-time scalars a flavor-aware `relational_window_risk_set()`
  could someday supply. The `is.na(flavor)` guard above is a **deliberate stopgap**: it
  routes this quadrant to the panel path until a flavor-aware relational risk-set
  exists, and the follow-up that builds one (`process-simulation` Task 2.6,
  "multi-period relational `|R_w|` slicing") is expected to drop this guard and
  re-route flavored relational layers. Because the pin is only ever the *completion
  default* for a flavor whose rate the user left unspecified (a user-supplied `coef`
  never reaches it), the interim Hamming-floor bias is confined to that
  zero-information default and does not touch any specified rate. Recorded as an Open
  Question rather than solved here (see below and Non-Goals).

**Alternative considered:** extend `relational_window_risk_set()` with an optional
`flavor` argument now, threading it into `make_specification()`'s flavored `rate = list(...)`
form and reading the matching flavor's slice out of
`estimate_dynam(..., preprocessing_only = TRUE)`'s output. Deferred — the flavored
preprocessing output shape (`preprocess_flavored.R`) would need inspection to confirm it
exposes `n_dep_events`/`total_time`/`avg_active_entity` *per flavor* rather than pooled,
which is unverified and materially larger in scope than the crash this proposal exists to
fix.

### D4 — Degenerate (no-timed-events) layer: warn on panel, abort on relational

A completed rate's pinned layer can carry **no timed events at all** (every row is
`time = NA` history). What is *definable* differs by path, so the friendly,
layer-naming handling is deliberately asymmetric (option (i)):

- **Panel path** (and the flavored-relational fallback, which shares
  `default_window()` / `panel_wave_risk_set()`): the empty layer filter makes
  `default_window()` return its `[0, 1]` fallback (`duration = 1`, positive), and the
  Hamming diff of two identical materialized endpoints is `count_w = 0`, so the pin is
  a **well-defined zero hazard** `intercept_w = log(0) = -Inf` (the flavor simply
  cannot fire). This emits a `cli` **warning** naming the layer and **continues**.
- **Relational path** (`relational_window_risk_set()`): the preprocessing scalars come
  back `total_time = 0` and/or `avg_active_entity = 0`, so the pin's **denominator is
  zero** and no finite intercept exists. This **aborts** with a friendly `cli` error
  naming the layer, *before* `pin_intercept_only_rate()`'s generic
  `"duration must be positive"` / `"risk_set_size must be positive"` guard fires (so
  the message identifies the offending layer, not an anonymous scalar).

This is distinct from a **per-period** zero-count wave (a multi-wave grid where nothing
changed between two boundaries): that is the intended, already-documented
`count_w = 0 → intercept_w = -Inf` behavior of `pin_intercept_only_rate()` and stays
**silent** — warning on every empty inter-wave period would be noise on any
legitimately sparse panel. Only a layer with **no timed events whatsoever** triggers
D4's warn/abort. Both messages carry a distinct `cli` condition class so tests can
catch them precisely.

**Alternative considered:** unify on abort for both paths (treat a no-timed-events
*panel* layer as a malformed spec too). Rejected — a panel layer whose completed
flavor simply never fires is a *valid, degenerate* contribution (a well-defined zero
hazard), so warn-and-continue keeps the joint composition usable; only the relational
path, where no finite pin exists, has no choice but to abort.

## Risks / Trade-offs

- **[Risk]** The D3 dispatch leaves flavored relational layers on the less-exact
  Hamming-diff pin, so a user modeling a flavored, fully event-observed layer with a
  rate gap gets a coarser pin than the requirement text's ideal. → **Mitigation**: this
  is the *current* behavior for every completed rate today (nothing regresses), it's
  documented as an explicit Open Question, and the pin is zero-free-parameter either
  way — it does not bias θ, only the fixed offset/simulated hazard for that one flavor.
- **[Risk]** Numeric-seconds coercion changes the *magnitude* of `duration`/`T_w` for
  any caller currently passing POSIXct `wave_times` by hand (none exist in-tree today,
  per the grep of `tests/testthat/`), which would change `intercept_w` if such a caller
  existed silently relying on the old (crashing-before-use) behavior. → **Mitigation**:
  the old behavior was a hard crash, not a silently-different-but-working numeric value,
  so there is no working caller to regress.
- **[Trade-off]** `default_window()`'s new `layer` filter changes the single-window
  fallback's boundaries whenever a panel layer's own timed extent differs from the joint
  dataset's global extent (the common case). This is a correctness fix, not a
  compatibility-preserving one — the old unfiltered range was itself the bug (D2's
  motivation), so no fixture should assert the old cross-layer range.

## Migration Plan

No data migration. This is an in-package bug fix confined to
`R/complete_generative_spec.R`; no exported signatures change (`panel_wave_risk_set()`,
`default_window()`, `pin_completed_rates()` are all internal, non-exported). Rollback is
a plain revert of the touched file plus its new test fixtures.

## Open Questions

- The flavor-aware relational risk-set (so a **flavored** relational layer's completed
  rate uses exact continuous-time / per-period scalars instead of the Hamming-diff
  approximation) is **owned by `process-simulation` Task 2.6** ("multi-period
  relational `|R_w|` slicing"), not this change. This change deliberately leaves
  `relational_window_risk_set()` **byte-identical** (see Non-Goals): its return
  contract `list(count, duration, risk_set_size, wave_times)` already has the shape 2.6
  needs, so 2.6 extends it **additively** — filling the currently-`NULL` `wave_times`
  with the join's shared wave grid (a relational layer has no intrinsic grid; it
  inherits the panel partner's or a consumer-supplied one), vectorizing the three
  scalars to length K, and adding optional `wave_times` / `flavor` parameters that
  default to today's single-window behavior. 2.6 also drops D3's `is.na(flavor)`
  stopgap guard. No interface pre-shaping is needed here — the contract is already
  forward-compatible.
- Sub-question deferred to 2.6 (not this change): confirm `preprocess_flavored.R`'s
  output exposes per-flavor `n_dep_events` / `total_time` / `avg_active_entity` rather
  than pooled, before scoping the flavor-aware relational path.
