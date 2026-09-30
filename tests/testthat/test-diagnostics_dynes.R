# Task 2.3/2.4 (abmcem): the run-level ESS diagnostics the ABMCEM loop owns and
# drives -- distinct from the between-iteration ESS redraw guard in
# weights_dynes.R (which regenerates the pool). Three surfaces:
#   * the one-shot cold-start ESS diagnostic (task 2.3): a pathological startup,
#     silenced neither by the guard's "never at startup" rule nor absorbed;
#   * the mid-run IS-ESS-collapse recommendation to switch to augment_seq_mcmc()
#     (task 2.4a), whose numeric floor is owned by dynes-augmentation's 6.2/6.3
#     study and read through a single seam, never hard-coded here;
#   * the run-level restart-coupling warning dedup (task 2.4b): exactly one
#     warning per estimation run.
# The loop is not implemented yet; these are the pure diagnostic functions it
# consumes, tested against hand-computed ESS fixtures.

# A degenerate normalized weight vector of length K with effective sample size
# just above 1 (one sequence carries almost all the mass): the pathological
# regime the diagnostics fire in.
degenerate_weights <- function(k) {
  w <- c(0.97, rep(0.03 / (k - 1), k - 1))
  w / sum(w)
}

# --- Task 2.3a: cold-start ESS diagnostic -----------------------------------

test_that("cold-start diagnostic fires below the pathology floor", {
  local_reproducible_output()
  # K = 20, default ess_threshold = 0.5 -> pathology floor = 0.1 * 20 = 2.0.
  # The degenerate pool's ESS ~ 1.06 sits below the floor: a pathological start.
  w <- degenerate_weights(20)
  expect_snapshot(
    decision <- cold_start_ess_diagnostic(w, ess_threshold = 0.5)
  )
  expect_true(decision$diagnosed)
  # The diagnostic only informs -- it never redraws or grows the pool.
  expect_false(isTRUE(decision$redraw))
  expect_false(isTRUE(decision$grow))
})

test_that("cold-start diagnostic is silent on a healthy startup pool", {
  # A near-uniform pool (the default theta0 = 0 path, where the random proposal
  # is close to the model) has ESS ~ K, well above the pathology floor: silent.
  healthy <- rep(1 / 20, 20)
  expect_no_warning(
    decision <- cold_start_ess_diagnostic(healthy, ess_threshold = 0.5)
  )
  expect_false(decision$diagnosed)
})

test_that("cold-start pathology floor is strictly below the redraw guard", {
  # The floor fraction must be strictly under ess_threshold for every threshold,
  # so the one-shot diagnostic is reserved for a genuinely pathological start,
  # never the ordinary staleness the between-iteration guard handles.
  for (thr in c(0.5, 0.3, 0.05)) {
    expect_lt(cold_start_pathology_fraction(thr), thr)
  }
})

test_that("cold-start diagnostic is distinct from the ESS redraw guard", {
  # On the SAME degenerate startup pool the redraw guard stays silent (its "never
  # at startup" rule) while the cold-start diagnostic fires: two different
  # surfaces, not one relabelled.
  w <- degenerate_weights(20)
  expect_no_warning(
    guard <- ess_guard(
      w,
      ess_threshold = 0.5,
      enabled = TRUE,
      at_startup = TRUE
    )
  )
  expect_false(guard$redraw)
  expect_warning(cold_start_ess_diagnostic(w, ess_threshold = 0.5))
})

# --- Task 2.4a: runtime ESS-collapse recommendation + restart dedup ---------

test_that("mid-run ESS collapse recommends augment_seq_mcmc() once per run", {
  local_reproducible_output()
  # The numeric IS-ESS collapse floor is owned by dynes-augmentation's 6.2/6.3
  # study, not this change; the message surface reads it through a single seam.
  # The test injects a calibrated fraction (0.3 of the pool) via that seam.
  testthat::local_mocked_bindings(
    importance_ess_collapse_threshold = function() 0.3
  )
  state <- new_run_warning_state()
  w <- degenerate_weights(20) # ESS ~ 1.06 < 0.3 * 20 = 6 -> collapse.

  expect_snapshot(
    fired <- recommend_mcmc_on_ess_collapse(state, w, routine = "random")
  )
  expect_true(fired)
  # Run-level dedup: a second collapse in the same run stays silent.
  expect_no_warning(
    again <- recommend_mcmc_on_ess_collapse(state, w, routine = "sim")
  )
  expect_false(again)
})

test_that("ESS-collapse recommendation is dormant until the study calibrates it", {
  # With no calibrated threshold (dynes 6.2/6.3 not landed) the seam returns NA
  # and the recommendation never fires -- the surface ships without a guessed
  # number baked in.
  expect_true(is.na(importance_ess_collapse_threshold()))
  state <- new_run_warning_state()
  w <- degenerate_weights(20)
  expect_no_warning(
    fired <- recommend_mcmc_on_ess_collapse(state, w, routine = "random")
  )
  expect_false(fired)
})

test_that("ESS-collapse recommendation is scoped to importance-weighted routines", {
  testthat::local_mocked_bindings(
    importance_ess_collapse_threshold = function() 0.3
  )
  w <- degenerate_weights(20)
  # MCMC's uniform weighting makes the same statistic trivially the pool size, so
  # the IS-dimension recommendation does not apply to it.
  expect_no_warning(
    fired <- recommend_mcmc_on_ess_collapse(
      new_run_warning_state(),
      w,
      routine = "mcmc"
    )
  )
  expect_false(fired)
  # It also never fires at startup (the cold-start diagnostic owns iteration 1).
  expect_no_warning(
    startup <- recommend_mcmc_on_ess_collapse(
      new_run_warning_state(),
      w,
      routine = "random",
      at_startup = TRUE
    )
  )
  expect_false(startup)
})

test_that("restart-coupling warning fires exactly once per run", {
  local_reproducible_output()
  state <- new_run_warning_state()
  constraint <- "friendship: ~tie(friendship) | recip(friendship)"
  # The first restart at any iteration/draw warns, naming the coupled constraint.
  expect_snapshot(
    first <- warn_restart_coupling_once(
      state,
      restarted = TRUE,
      routine = "sim",
      constraint = constraint
    )
  )
  expect_true(first)
  # Every later restart in the same run is deduplicated to silence.
  expect_no_warning(
    second <- warn_restart_coupling_once(
      state,
      restarted = TRUE,
      routine = "sim",
      constraint = constraint
    )
  )
  expect_false(second)
})

test_that("a random augmenter's redraws emit no restart warning", {
  # random's legality normalizer is a theta-free constant that cancels in the
  # normalized weights, so its redraws carry no plug-in bias to warn about.
  state <- new_run_warning_state()
  expect_no_warning(
    fired <- warn_restart_coupling_once(
      state,
      restarted = TRUE,
      routine = "random",
      constraint = "friendship: ~outdeg(friendship) < 1"
    )
  )
  expect_false(fired)
})

test_that("a coupling-free run emits no restart warning", {
  state <- new_run_warning_state()
  expect_no_warning(
    fired <- warn_restart_coupling_once(
      state,
      restarted = FALSE,
      routine = "sim"
    )
  )
  expect_false(fired)
})
