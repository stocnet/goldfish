## 1. Numeric time coercion (D1, D2)

- [x] 1.1 In `panel_wave_risk_set()` (`R/complete_generative_spec.R`), coerce
      `wave_times` to numeric via the existing `coerce_time()` helper
      (`R/state_at.R`) once, after the `%||% default_window(...)` fallback and
      before it is used for the `network_state_at()` calls and for
      `duration <- diff(wave_times)` (so both an explicit POSIXct grid and the
      fallback land numeric).
- [x] 1.2 Change `default_window(data)` to `default_window(data, layer)`: filter
      `as.data.frame(data$ties)` to `ties$layer == layer` before taking
      `range(times)`, and coerce the result with `coerce_time()`. Update
      `panel_wave_risk_set()`'s call site to pass `layer`.
- [x] 1.3 Add POSIXct- and Date-timed test fixtures to
      `tests/testthat/test-complete_generative_spec.R` covering both the
      explicit-wave-grid and single-window-fallback branches of
      `panel_wave_risk_set()` / `default_window()`, alongside the existing
      numeric-time fixtures (follow the r-lib:testing-r-packages skill:
      testthat 3e, self-contained fixtures).
- [x] 1.4 Add a fixture where the joint dataset's other layers span a different
      time extent than the panel layer being pinned, asserting the single-window
      fallback uses only the pinned layer's own range (regression test for D2).
- [x] 1.5 `air format` the touched files, then `lintr::lint()` them; run the
      not-cran-test skill and confirm the frozen 1e-6 coefficient/C++ golden
      baselines report PASS, not SKIP; commit.

## 2. Panel/relational risk-set dispatch (D3)

- [ ] 2.1 In `pin_completed_rates()`, dispatch per completed fid: call
      `relational_window_risk_set()` (passing the layer's `model`, DyNAM or REM)
      when `is.na(row$flavor)` and `row$layer` is not in
      `joint_spec$modeled_panel`; call `panel_wave_risk_set()` otherwise (modeled
      panel layers, and flavored relational layers — the `is.na(flavor)` stopgap
      per design D3). `relational_window_risk_set()` stays byte-identical
      (design Non-Goal / D3 byte-identical decision).
- [ ] 2.2 Add a test completing a rate on an unflavored, fully event-observed
      (non-panel) layer through `complete_generative_spec()`, asserting the pin's
      `(count, duration, risk_set_size)` match `relational_window_risk_set()`'s
      `n_dep_events` / `total_time` / `avg_active_entity` scalars for that layer,
      not a synthesized wave-endpoint Hamming diff. Cover both a DyNAM and a REM
      (tie-oriented) relational layer so the `model` plumbing is exercised.
- [ ] 2.3 Add a test confirming a **flavored** relational layer's completed rate
      still routes through `panel_wave_risk_set()` (documents the current
      stopgap boundary recorded as a design Non-Goal / Open Question — not a
      regression; `process-simulation` Task 2.6 will later flip this).
- [ ] 2.4 `air format` the touched files, then `lintr::lint()` them; run the
      not-cran-test skill and confirm the frozen baselines report PASS, not SKIP;
      commit.

## 3. Degenerate no-timed-events layer handling (D4)

- [ ] 3.1 Panel path: when `default_window()` (after the D2 layer filter) finds no
      timed events for the pinned layer, emit a friendly, layer-naming `cli`
      **warning** (distinct condition class) and let the pin proceed to its
      well-defined zero hazard (`intercept_w = -Inf`, `count_w = 0` over the
      `[0, 1]` fallback). Do NOT warn on a per-period zero-count wave in a
      multi-wave grid — that stays a silent `-Inf`.
- [ ] 3.2 Relational path: in the `pin_completed_rates()` dispatch, guard the
      `relational_window_risk_set()` result — when `total_time` and/or
      `avg_active_entity` come back `0` (no finite pin exists), **abort** with a
      friendly, layer-naming `cli` error (distinct condition class) *before*
      `pin_intercept_only_rate()`'s generic `duration`/`risk_set_size` guard fires.
- [ ] 3.3 Tests (testthat 3e): (a) a panel/flavored-relational layer with only
      `time = NA` history warns (catch the condition class) and completes to a
      `-Inf` pin that fires nothing; (b) an unflavored relational layer with no
      events aborts naming the layer; (c) a multi-wave grid with one empty
      inter-wave period stays **silent** and pins that period to `-Inf` (no
      warning raised).
- [ ] 3.4 `air format` the touched files, then `lintr::lint()` them; run the
      not-cran-test skill and confirm the frozen baselines report PASS, not SKIP;
      commit.

## 4. Reproduction regression, docs, and release bookkeeping

- [ ] 4.1 Add a regression test reproducing the originally-reported crash end to
      end (the `joint_spec4` shape from the vignette): a multi-flavor DyNAM spec
      on a panel-observed, POSIXct-timed layer with one flavor's rate omitted but
      that flavor kept in `choice`, joined with another layer, run through
      `complete_generative_spec(consumer = "estimate_dynes")` (and
      `consumer = "simulate"`), asserting it completes without error and the
      resulting `completed_rates` entry carries a finite pinned intercept.
- [ ] 4.2 Update the in-file derivation comments in `R/complete_generative_spec.R`
      (`panel_wave_risk_set()`, `default_window()`, `pin_completed_rates()`,
      `relational_window_risk_set()`) to describe the numeric-coercion step, the
      panel/relational dispatch, the flavored-relational stopgap *reasoning*
      (until a flavor-aware relational risk-set exists — no OpenSpec change name
      or task number in the source, project convention), and the D4 warn/abort.
- [ ] 4.3 Bump the package version in `DESCRIPTION` (patch) and add a `NEWS.md`
      entry: POSIXct/Date-timed panel completion no longer crashes; relational
      layers' completed rates now use the correct risk-set source; a
      no-timed-events layer warns (panel) or aborts (relational) naming the layer.
- [ ] 4.4 Full verification: `air format` + `lintr::lint()` on every file touched
      across this change, run the not-cran-test skill for a final NOT_CRAN=true
      pass confirming the frozen baselines PASS (not SKIP), then commit.
