> **TDD.** 1.1/1.2 run as a red/green pair (per `openspec/config.yaml`
> `rules.tasks`): 1.1 commits the failing tests alone, 1.2 the code that turns
> them green. 2.1/2.2 are a second pair: removing the `compute_lik_seq()`
> workaround is a refactor under existing tests, but the consumer label (design
> D6) is new behavior, so its test goes first.
>
> **Landing (design D4).** Group 1 touches no DyNES code and may land on
> `develop` in its own small merge. Tasks 2.1/2.2 edit `compute_lik_seq()`,
> which exists only on `feature/dynes` (`dynes-augmentation` task 4.3b); run
> them there, or after `feature/dynes` next merges `develop` in.

## 1. Completion keeps its pins

- [x] 1.1 Write failing tests in `tests/testthat/test-complete_generative_spec.R`
      for re-entry on an already-completed spec, reusing `timed_panel_join()`
      (a modeled panel layer whose rate is completed and pinned):
      (a) completed on `c(0, 2, 5)`, then re-completed with no `wave_times`:
      `expect_identical()` on the whole result, pins keeping both plateaus and
      the grid. Exactly one warning, caught by BOTH
      `goldfish_pinned_rate_warning` and `goldfish_completed_default_warning`,
      with a cli snapshot under `local_cli_context()`: the re-entry headline
      (stored pin kept, not "completing it with"), the Hamming-diff source
      line, and the no-SE line for `"estimate_dynes"` (design D5). This is the
      case that fails today, on both the value and the warning.
      (b) Re-completed on `c(0, 2, 5)`: identical, and the same single warning
      as (a). This extends the
      existing "completion is idempotent on an already-complete spec" test
      from comparing intercepts to comparing the whole object.
      (c) Re-completed on `c(0, 3, 5)`: `expect_error(class =
      "goldfish_completion_repin")`, plus a cli snapshot of the message under
      `local_cli_context()` naming the pinned rate's layer and both grids.
      Add a variant through `timed_panel_join_cast()` / `cast_wave_times()`:
      a spec pinned on a `POSIXct` grid and re-completed on the same grid
      cast back to `POSIXct` is identical (the exact `coerce_time()`
      comparison in design D3), and the same grid with one interior boundary
      moved by ONE SECOND aborts. The second check fails under `all.equal()`'s
      default tolerance (about 25 s at epoch-second scale), which is why D3
      compares exactly. The snapshot also shows the two `i` bullets (where the
      authored spec is kept on a fit, and that a fit's and a simulation's
      `spec` are already completed).
      (d) Completed with no `wave_times` (single-window panel pin), then
      re-completed with a different two-boundary grid: aborts with the same
      class, which shows the default window's grid is recorded (`$pin_grid`).
      (e) A grid-independent pin (an unflavored relational layer's event-count
      pin) and a completed spec with no pinned rate: any `wave_times` returns
      the input unchanged. The event-count pin is re-announced once with its
      event-count source line; the rate-less spec raises no warning
      (`expect_no_warning()`), since a uniform choice default is not
      re-announced.
      (f) The raw spec through `estimate_dynes` then `simulate` still warns
      twice. The existing "re-fires through a second consumer" test covers
      this; confirm it still runs, don't duplicate it.
      (g) A choice-only spec completed once without a clock, then completed
      again with `times = "generated"`: the clock rate is installed and
      pinned, exactly one completion warning fires, and the rest of the spec is
      unchanged (design D1, D5). This guards the generated-clock path the
      rejected early return would have broken. Fixture: the single-process
      choice-only spec from `test-simulate.R` ("a choice-only DyNAM generates
      only when asked": `choice = ~ inertia + tie(friendship)` on
      `sim_fixture_data()`), wrapped with `single_process_joint()` as
      `simulate.goldfishSpec` does. Complete it with `consumer = "simulate"`
      and no `times`, then again with `times = "generated"`. Oracle:
      `expect_identical()` against completing the raw joint spec once with
      `times = "generated"`, with that oracle call's own warning suppressed.
      (h) A legacy pin: take the (a) spec completed on `c(0, 2, 5)`, set the
      pin's `$pin_grid` to `NULL` (its `$wave_times` stays), re-complete on
      `c(0, 3, 5)`: aborts with `goldfish_completion_repin` (design D3's
      `%||%` fallback).
      (i) Grid inheritance (design D1): build a completed spec whose stored
      pin is on `c(0, 2, 5)`, then drop one completed rate's entry from
      `$completed_rates` by hand (the only way to reach this state).
      Re-completed with no `wave_times`, the missing fid is re-pinned on
      `c(0, 2, 5)`, not the default window. With two stored pins on different
      grids, the same re-entry aborts with `goldfish_completion_repin`. Needs a
      fixture with two completed panel rates; if `timed_panel_join()` has only
      one, set the second pin's grid by hand rather than building a new join.
      (j) Cross-consumer re-announcement (design D5; the living
      `intercept-only-rate` "each consumer entry" rule): complete
      `timed_panel_join()` for `"estimate_dynes"`, then complete the RESULT for
      `"simulate"`. The second call raises exactly one warning for the pinned
      rate, with the source line and no standard-error line. Also assert the
      end-to-end path: `simulate()` on that completed spec raises one
      pinned-rate warning, where today it raises none.
      Run to confirm (a), (b), (c) incl. the one-second variant, (d), (e),
      (h), (i) and (j) RED; commit the tests alone.
- [x] 1.2 Implement per design D1–D3 and D5. `make_intercept_only_rate()` gains
      `pin_grid = NULL`, stored as `$pin_grid` with `$wave_times` unchanged.
      `pin_completed_rates()` records the coerced grid on every
      `panel_wave_risk_set()` pin (length K + 1, the default window included)
      and `NULL` on relational pins. It gains a `stored` argument, which
      `complete_generative_spec()` fills with the INPUT's
      `joint_spec$completed_rates`, because step 2's rebuild drops it (design
      D1). It keeps a stored entry whose fid is still a completed rate, pins
      only completed rates with no entry, and leaves `pinned_rates` to
      `mark_pinned_rates()`. It aborts with `goldfish_completion_repin` when an
      explicit `wave_times` (coerced with `coerce_time()`) is not
      `identical()` to a kept pin's grid, read as
      `pin$pin_grid %||% pin$wave_times`, both as numeric, with no tolerance.
      It checks every kept pin first and gives one bullet per mismatch, naming
      the layer (and flavor), then the two `i` bullets of design D3. A fid
      with no stored entry is pinned on the supplied grid, or else on the grid
      the stored grid-carrying pins share (aborting if they disagree), and
      `default_window()` only when no stored pin carries a grid. Step 4
      (design D5): `warn_completed_defaults()`, or a sibling it calls, also
      warns for every completed pinned rate carried over from `stored`. The
      warning uses the re-entry headline, the stored pin's `$source` via
      `pinned_rate_source_note()`, the no-SE line for the estimation
      consumers, and both condition classes. Uniform choice defaults are not
      re-announced, and the fill warning for a fresh pin is unchanged. Update
      the function's "re-pins the same fids deterministically" comments to
      describe what it does now. `air format` and `lintr` the touched files.
      Run `devtools::document()` only if roxygen changed
      (`make_intercept_only_rate()` is internal). Re-run the 1.1 tests to
      confirm GREEN, then run the not-cran-test skill and commit.

## 2. Consumers and closing

- [x] 2.1 (On `feature/dynes`.) Write the failing test in
      `test-evaluator_dynes.R`: `compute_lik_seq()` on a half-specified
      (authored) spec warns once, and a cli snapshot under
      `local_cli_context()` shows the warning naming `compute_lik_seq()`, not
      `estimate_dynes()` (design D6). Run to confirm RED; commit the test
      alone.
- [x] 2.2 (On `feature/dynes`.) `complete_generative_spec()`'s `consumer`
      gains `"compute_lik_seq"`, worded like `"estimate_dynes"` except for the
      function it names. Extend `warn_pinned_rates()`'s `match.arg()` the same
      way if `compute_lik_seq()` calls it. Remove the workaround in
      `compute_lik_seq()` (`R/evaluator_dynes.R`): drop the check for the
      `completed` column and its comment, and call
      `complete_generative_spec(spec, consumer = "compute_lik_seq")`
      unconditionally. The `dynes-augmentation` 4.3a tests
      ("compute_lik_seq() is one sequence of the pool evaluator", "... share
      one density") pass a spec completed on `c(0, 2, 5)`, so they guard the
      workaround removal. They now also see the re-entry warning (design D5):
      wrap those calls in `expect_warning(class =
      "goldfish_pinned_rate_warning")` rather than `suppressWarnings()`, so the
      announcement stays asserted. Do NOT add a `wave_times` argument here: grid
      resolution for authored specs is `dynes-augmentation` task 6.1 (design
      Non-Goals). `air format` and `lintr`; re-run `test-evaluator_dynes.R`
      and `test-complete_generative_spec.R` to GREEN; commit.
- [x] 2.3 Closing: full `NOT_CRAN=true` suite (frozen baselines PASS, not SKIP).
      Expect fallout from design D5. Any existing test that passes an
      already-completed spec to a consumer, or completes one twice, now sees
      one pinned-rate warning per completed rate (candidates:
      `test-estimate_dynes.R`, `test-fit_dynes.R`, the abmcem pool tests).
      Assert it with `expect_warning(class = ...)` where the test is about
      something else; never suppress it blindly.
      Write `NEWS.d/completion-idempotency--pinned-rate-reentry.md`. Design D5
      is user-reachable through the exported `simulate()`: simulating from a
      simulation result's or a fit's already-completed `spec` now warns that
      the rate is pinned, where it was silent. Suggested bullet: "Fixed
      `simulate()` not warning that a rate is pinned when given an
      already-completed specification." The new abort needs no bullet: no
      exported function passes a completed spec with a new grid. No version
      bump and no archive on a feature branch. Confirm that
      `dynes-augmentation` tasks 6.1a/6.1b still carry the consumer
      grid-resolution item (design Non-Goals); commit.
