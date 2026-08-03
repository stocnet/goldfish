## 0. Blast radius, before anything is written

- [ ] 0.1 Inventory the frozen baselines in `tests/testthat/_baselines/` and the
      wider suite for exposure to the three numerically-moving fixes (D14):
      `history = "consecutive"` with a `start_time`; DyNAMi with an `end_time`;
      any censoring sub-model with an `end_time` past its last event. Record the
      findings in `progress.md`, including the case where nothing is covered —
      that is why the bugs were reachable.
- [ ] 0.2 Add characterization tests pinning the **current** behavior of the
      three cases found in 0.1 (or, where nothing is covered, new fixtures), so
      each later fix shows its own diff rather than an unexplained baseline
      move. Follow the r-lib:testing-r-packages skill.
- [ ] 0.3 Verify with the not-cran-test skill; confirm the frozen baselines
      report PASS not SKIP.

## 1. The window bug: a fit records the events it modeled

- [ ] 1.1 Thread the resolved observation window into
      `stocnet_dependent_events()` (`R/legacy_wrappers.R:534-551`) from its call
      site (`R/model_estimate.R:2641`) and filter the returned rows to it (D1).
      Read the boundary convention off the preprocessing loop rather than
      assuming it.
- [ ] 1.2 Test row **identity**, not row count: the filtered `time` column must
      equal `event_time[!right_censored_events]` exactly, on a `start_time` fit,
      an `end_time` fit and a both-bounds fit, for a rate and a choice
      sub-model. Assert `augment()` returns one row per interval and that
      `diagnose_outliers()` / `diagnose_changepoints()` run on a windowed fit.
- [ ] 1.3 Verify with the not-cran-test skill and commit.

## 2. The window semantics

- [ ] 2.1 Fix the `event_order` burn-in drift (D2) so a pre-`start_time`
      dependent row advances the counters as an in-window one does
      (`R/model_preprocess.R:718`, `:739`, `:853`).
- [ ] 2.2 Test the two-way equality: a fit with `start_time = t` and a fit whose
      data is truncated so `t` is the first event, with the earlier events as
      history, agree on the `history = "consecutive"` statistic where the
      histories coincide; and the burn-in statistic is not identically zero.
      Record any baseline movement against 0.1's inventory.
- [ ] 2.3 Close the observation window when the schedule exhausts before
      `end_time` (D5), storing the trailing exposure interval on the sub-models
      whose likelihood defines a compensator and nothing on the multinomial
      families.
- [ ] 2.4 Stop the final right-censored row from carrying the sender and
      receiver of the out-of-window event that triggered the stop
      (`R/model_preprocess.R:741-750`).
- [ ] 2.5 Test that an exact-time rate fit with `end_time` past its last event
      differs from the same fit without one, that a later `end_time` gives a
      lower baseline rate, and that a choice fit's log-likelihood is unchanged.
      This is the regression for a bug that made `end_time` a silent no-op.
- [ ] 2.6 Make `preprocess_monolith()` stop traversing at `end_time` as the
      recipe loops do (D4), and correct the `set_preprocessing()` documentation
      that states the opposite (`R/set_opt.R:703-704`). Run
      `devtools::document()` in this task.
- [ ] 2.7 Verify with the not-cran-test skill and commit; where a frozen
      baseline moved, regenerate it as its own commit naming the fix and the
      reason.

## 3. Phase 1 milestone

- [ ] 3.1 Bump `DESCRIPTION` to 1.9.24 and add the `NEWS.md` entry: the
      dependent-events window fix, the `event_order` correction and the
      combination it affected, the `end_time` no-op fix and its direction
      (baseline rates fall), and the monolith traversal stop.

## 4. Parser: degenerate formulas and the free-parameter floor

- [ ] 4.1 Normalize the degenerate `terms()` `factors` shape in
      `get_rhs_names()` (`R/formula_parser.R:1217-1231`) so `~ 1`,
      `~ offset(x)`, `~ 1 + offset(x)` and multi-offset formulas parse instead
      of failing on `nrow()` (D6).
- [ ] 4.2 Add the free-parameter floor where the fixed set is already known
      (`assemble_fixed_parameters()`, `R/formula_validate.R:501`), with a cli
      abort distinguishing "every term fixed" from "this sub-model cannot
      identify a lone intercept" (D6, D7). Use the risk-set descriptor already
      on the specification as the predicate, not a second derivation from the
      sub-model name. Retire the unreachable `stop()` at
      `R/formula_parser.R:55-58`. Follow the r-lib:cli skill.
- [ ] 4.3 Test that an intercept-only exact-time rate model estimates and
      returns a baseline rate; that an intercept-only choice model aborts naming
      the identification; that offset-only formulas abort naming the zero free
      parameters; and that one free term beside an `offset()` is unaffected.
      Pin the messages with a reproducible cli context.
- [ ] 4.4 Run `devtools::document()`, verify with the not-cran-test skill, and
      commit.

## 5. Guards and messages

- [ ] 5.1 Make the `cox_snell` abort message name the requesting sub-model's own
      likelihood family instead of asserting "multinomial" for every
      compensator-less case (D13), which is false for `choice_coordination`.
- [ ] 5.2 Settle DyNAMi `rate` against the guard (D13): add the test and let it
      pass if the residuals are correct, or block it with a stated reason if
      they are not. Decide from the measurement.
- [ ] 5.3 Emit the informational cli message when `"conditional_scores"` is
      requested on a multinomial family (D8), naming the identity and the
      `set_algorithm_newton()` adjustment. A message, not a warning. Follow the
      r-lib:cli skill.
- [ ] 5.4 Make `evaluate_model(return = "conditional_scores")` on a multinomial
      fit abort as `"exposure"` already does (D9), and state the asymmetry with
      the estimation-time request in the documentation.
- [ ] 5.5 Make `risk_set_axis()` abort on a flavored container naming the
      component and the per-process route, instead of returning `NULL` (D12).
- [ ] 5.6 Test each of the above, including that the `conditional_scores`
      message does not become an error under `options(warn = 2)` and that the
      stored `event_scores` are unchanged. Pin messages with a reproducible cli
      context.
- [ ] 5.7 Run `devtools::document()`, verify with the not-cran-test skill, and
      commit.

## 6. Phase 2 milestone

- [ ] 6.1 Bump `DESCRIPTION` to 1.9.25 and add the `NEWS.md` entry for the
      parser floor, the intercept-only support, and the four guard/message
      changes.

## 7. Interval and event accounting

- [ ] 7.1 Establish the blast radius of the sample-size correction before
      changing it: which tests, vignette chunks, snapshots and `.plan` documents
      assert a BIC, an AICc or a `nobs`, and which of those fits carry
      right-censored intervals. This changes reported numbers without changing
      any coefficient, so a coefficient baseline will not catch it.
- [ ] 7.2 Store both counts honestly (D16): `n_events` becomes the dependent
      event count, and a new `n_intervals` carries what `n_events` holds today
      (`length(right_censored_events)`, set at `R/cpp_interface.R:95`).
- [ ] 7.2b Point every reader at `n_events`, and delete the three independent
      recomputations of `sum(!x$right_censored_events)`. The six wrong readers
      are `logLik()`'s `nobs` (hence BIC), the AICc denominator
      (`R/methods_display.R:152`), `glance()`'s `nobs`,
      `logLik(avgPerEvent = TRUE)`, and the rendered counts in `margin_table()`
      and `test_parameter()`. The three that already recompute correctly are
      `scaled_schoenfeld_rows()` and the printed contexts of `diagnose_onset()`,
      `test_gof()` and `test_time()` — those keep their meaning and lose their
      workaround. `AIC()` needs no change, carrying no sample size.
- [ ] 7.3 Test that a windowed and an unwindowed rate model on one event stream
      report the same sample size to BIC and AICc; that `n_events` equals the
      dependent count and `n_intervals` the per-interval length; that
      `logLik(avgPerEvent = TRUE)` equals the total over the event count; and
      that no rendered "events" count shows an interval count. Include the
      measured cases: `nobs` 439 against 876, and `avgPerEvent` −12.889 against
      the current −6.459, on the `social_evolution` rate fixture.
- [ ] 7.4 Run `devtools::document()`, verify with the not-cran-test skill, and
      commit.
- [ ] 7.5 Make every per-interval residual type return one value per dependent
      event by accumulating between events (D17): deviance, score and cox_snell
      accumulated; schoenfeld, scaled_schoenfeld, response and martingale
      unchanged; dfbeta and dfbetas derived from the accumulated rows; cooks
      evaluated on the accumulated row rather than summed.
- [ ] 7.6 Test the identities the accumulation must preserve: accumulated score
      column sums equal the all-interval sums and are zero at the maximum (the
      dependent-only sums are not — measured at 372 on a windowed rate fit);
      accumulated cox_snell totals the dependent-event count and is the
      between-event compensator; every type returns one value per event on every
      sub-model.
- [ ] 7.7 Make `augment()` return one row per dependent event with a column
      giving how many likelihood intervals were accumulated into that span
      (D20). Drop the `NA` rows on `.fitted` / `.resid`, which no longer arise,
      and confirm the table aligns row-for-row with `residuals()` and with the
      dependent-events table of task 1.1.
- [ ] 7.7b Read `test_gof()` on the accumulated basis (D19). This is a
      correctness fix, not a smoothing: the per-interval outer-product constant
      understates the variance because within-span contributions are positively
      correlated (measured scale ratios 1.03-1.17), which makes the test
      anti-conservative on every fit carrying right-censored intervals. Amend
      the `gof_processes()` comment so the `n`-cancels claim is not read as an
      aggregation claim.
- [ ] 7.7c Test that the accumulated standardizing constant exceeds the
      per-interval one on a windowed rate fit, that the statistic falls
      accordingly, and that a choice fit's statistics and p-values are
      unchanged. Honor ADR-0003: assert relationships and equalities, not
      p-value values.
- [ ] 7.8 Retire `include_censored` on the two describers (D18) via the
      r-lib:lifecycle skill — deprecate with a warning, ignore the value — and
      route their series through the same accumulation the residual methods use,
      never a second implementation. Simplify `diagnose_onset()`'s documentation,
      which no longer needs to explain why its axis and its series disagree.
- [ ] 7.9 Test the deprecation warning, that the argument no longer changes the
      answer, and that a choice fit's series is unchanged. Pin messages with a
      reproducible cli context.
- [ ] 7.10 Run `devtools::document()`, verify with the not-cran-test skill, and
      commit.

## 8. Phase 3 milestone

- [ ] 8.1 Bump `DESCRIPTION` to 1.9.26 and add the `NEWS.md` entry: the BIC/AICc
      sample-size correction and that it changes reported values on windowed rate
      and REM fits, the per-event residual contract and the length change it
      implies, and the `include_censored` deprecation.

## 9. Flavored diagnostics: the tidy surface

- [ ] 9.1 Extract the per-method flavor/family append into one shared helper and
      route all five existing flavored methods through it, using
      `flavored_row_order()` everywhere so `model_terms()` and `margin_table()`
      stop disagreeing with the `test_*` family on row order (D10).
- [ ] 9.2 Add the flavored `augment()` method: row-bind per process with
      `flavor` and `family` appended after the existing columns (D10).
- [ ] 9.3 Add the flavored `diagnose_outliers()`, `diagnose_changepoints()` and
      `diagnose_onset()` methods. These are required by a shipped requirement
      and absent, so this is conformance work.
- [ ] 9.4 Record that `flavor` and `family` are not defining columns (D11), in
      the documentation and in a test asserting the class and plot dispatch
      survive dropping the column.
- [ ] 9.5 Test that each flavored table equals the standalone per-process result
      excluding the identity columns, and that two diagnostics of one fit agree
      on row order when the declared flavor order differs from the process
      order. Honor ADR-0003: no test asserts a p-value on the flavored fixture.
- [ ] 9.6 Run `devtools::document()`, verify with the not-cran-test skill, and
      commit.

## 10. Flavored diagnostics: the non-tidy surface

- [ ] 10.1 Add flavored `residuals()`, `fitted()`, `predict()` and
      `evaluate_model()` methods returning a list named by process label (D10),
      with `flavor =` selecting one process and returning the single-fit shape.
- [ ] 10.2 Make an unknown `flavor =` abort naming the flavors the fit carries.
      Follow the r-lib:cli skill.
- [ ] 10.3 Test that the container list entries equal the per-process calls, that
      `flavor =` equals the corresponding entry, and that no method returns
      `NULL` on a container.
- [ ] 10.4 Confirm against an installed autograph that the plot methods still
      facet on the identity columns and that nothing in 9–10 changed their
      dispatch. No autograph change is expected; record it if one is needed.
- [ ] 10.5 Document the flavored surface once, in `?diagnostic-requirements`
      rather than per method: the two return shapes, why they differ, and
      `flavor =`.
- [ ] 10.6 Run `devtools::document()`, verify with the not-cran-test skill, and
      commit.

## 11. Phase 4 milestone

- [ ] 11.1 Bump `DESCRIPTION` to 1.9.27 and add the `NEWS.md` entry for the
      flavored diagnostic surface.

## 12. Reading a large model

- [ ] 12.1 Add `residuals(type = "cox_snell", level = "actor")`: per-actor
      accumulated compensators between that actor's own consecutive events, with
      the final span to the observation window's end marked censored (D21).
      Abort on families defining no waiting time, as the unstratified type does.
- [ ] 12.2 Add the `dispersion` column to `margin_table()` — the variance of
      those residuals, `NA` where undefined, following the `expected_count`
      convention. Test that it reconciles with the margins: an actor's residuals
      sum to its expected count and number its observed count.
- [ ] 12.3 Test the level-vs-shape separation directly: an actor with matching
      observed and expected counts but clustered events departs on `dispersion`
      alone. Document that the column is uninformative at low event counts and
      is read beside `observed`.
- [ ] 12.4 Give the per-term diagnostic tables a defined order by statistic, so
      a batch script can take the front without rendering (D22). Confirm
      `effects =` selection composes with it.
- [ ] 12.5 Run `devtools::document()`, verify with the not-cran-test skill, and
      commit.
- [ ] 12.6 In autograph: page-wise rendering for the per-term plots via
      `ggforce::facet_wrap_paginate()` — already in Imports — with the page
      count derivable before rendering and a later page aborting by name. Add
      the level-vs-shape scatter to `plot.margin_table()` when `dispersion` is
      present.
- [ ] 12.7 In autograph: make a rank-and-trim figure report how many terms it
      omitted, rather than drawing a subset that looks like the whole model.
- [ ] 12.8 Test in autograph that every page renders in a non-interactive
      session, that the pages cover each term exactly once, and that nothing
      prompts. Commit in the autograph repo with `git -C`, never a bare `git`
      after a `cd`.

## 12b. Phase 5 milestone

- [ ] 12b.1 Bump `DESCRIPTION` to 1.9.28 and add the `NEWS.md` entry for the
      `dispersion` column, the stratified waiting-time residuals, and the
      batch-usable screening and pagination surface. Record the autograph
      version the plot changes require.

## 13. Decisions and close-out

- [ ] 13.1 Write the ADR revisiting the `conditional_scores` silence (D8), and
      cite it from this change's `proposal.md`.
- [ ] 13.2 Settle ADR-0010: the flavored `test_time()` method establishes that
      each process has its own clock and interval count, so a caller-supplied
      time transform must be a function rather than a length-`n` vector. This
      inverts its recorded option preference; the feature stays unplanned.
- [ ] 13.3 Settle the `diagnose_*` sub-question of ADR-0008 against the generic
      surface this change enlarges, leaving the cross-package ownership question
      open.
- [ ] 13.4 Settle the flatness-flag placement sub-question of ADR-0003 as an
      output-shape decision consistent with 7.1's shared helper. Leave the
      simulation arm post-2.0.0, and do not touch ADR-0005.
- [ ] 13.5 Update `.plan/bug-dependent-events-window.md` to record that it is
      resolved here, and re-run the applied walkthrough
      `.plan/sp/diagnostic_apply.qmd` to confirm its workarounds are no longer
      needed — the describers should now run on the warm fits, and the flavored
      container section should lose its silent-`NULL` finding.
- [ ] 13.6 Final full `NOT_CRAN=true` run; confirm the frozen baselines report
      PASS not SKIP, and that every regenerated baseline has a commit naming its
      reason.
