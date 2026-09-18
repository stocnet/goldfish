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
