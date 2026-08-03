# The rate loop's summation-order invariant.
#
# `DyNAM_rate_default` stores the very doubles its per-actor loop already
# computed (`rates(i) = exp_current_sender`) instead of rebuilding the rate
# vector the shared reductions need as one matrix-vector product afterwards.
#
# What these tests do and do not guard, measured rather than assumed:
#
#  * GUARDED, at 0 tolerance and portably. Asking for a primitive that consumes
#    the rate vector must not move the likelihood by a single bit. Both runs are
#    the same doubles in the same process, so no cross-platform comparison is
#    involved -- which is why this is portable where asserting against a stored
#    vector is not (different BLAS implementations legitimately differ). It
#    fails if the loop is restructured so that what a reduction reads and what
#    the likelihood sums stop being the same values.
#
#  * NOT GUARDED, and not guardable from R: the GEMV rebuild itself. Patching it
#    into the kernel and recompiling leaves every frozen 1e-6 baseline passing
#    and every log-likelihood and score bitwise unchanged, moving only the
#    stored margins -- 18 of 84 actors, by at most 2.6e-15 relative. That is
#    inside both the 1e-10 cross-backend and 1e-6 tolerances, and the per-event
#    form ("total_rate equals the sum of the consumed vector, exactly") is not
#    observable either: the vector is internal, and every route to it
#    through the returned surface reorders the summation, leaving a residual of
#    the same magnitude as the difference it would need to detect.
#
#    So the rebuild cannot move a coefficient -- `rates` never reaches the
#    normalizer or the derivative -- and the reason for storing rather than
#    rebuilding is consistency of the reported diagnostic, not coefficient
#    safety. The comment at the kernel site carries that, and carries it as
#    documentation because no portable assertion can.
#
# The second test is what remains checkable about the reduction's inputs: not
# that they agree in the last bit, but that they are the right values on the
# right scale over the right actors.

rate_fit <- function(backend, diagnostics) {
  suppressWarnings(estimate_dynam(
    baselines_model_grid()$se_dynam_rate$formula,
    sub_model = "rate",
    data = baselines_social_evolution_data(),
    control_algo = set_algorithm_newton(
      backend = backend,
      diagnostics = diagnostics
    ),
    progress = FALSE,
    verbose = FALSE
  ))
}

for (backend in baselines_backends) {
  test_that(
    sprintf(
      "requesting the reductions moves no bit of the likelihood (%s)",
      backend
    ),
    {
      skip_on_cran()
      base <- rate_fit(backend, c("loglik", "scores"))
      with_margins <- rate_fit(backend, c("loglik", "scores", "margins"))
      with_ranks <- rate_fit(backend, c("loglik", "scores", "ranks"))

      # identical(), not expect_equal(): the point is bitwise, and a tolerance
      # here would pass through exactly the last-bit movement being guarded.
      expect_identical(coef(with_margins), coef(base))
      expect_identical(with_margins$log_likelihood, base$log_likelihood)
      expect_identical(with_margins$final_score, base$final_score)
      expect_identical(coef(with_ranks), coef(base))
      expect_identical(with_ranks$log_likelihood, base$log_likelihood)
      expect_identical(with_ranks$final_score, base$final_score)
    }
  )

  test_that(
    sprintf(
      "the margins reduction consumed the likelihood's rates (%s)",
      backend
    ),
    {
      skip_on_cran()
      fit <- rate_fit(backend, c("loglik", "scores", "margins"))

      # The compensator identity: at the MLE the intercept's score sets the
      # summed expected count equal to the observed one. It holds only if the
      # reduction consumed the raw rates (not the max-shifted ones), scaled by
      # the interval, over the active senders only -- so a rebuild that dropped
      # the timespan, forgot the mask or used the shifted weights fails grossly.
      #
      # The tolerance is set by Newton convergence, not by floating point:
      # measured at 2.1e-10 relative here and 3.9e-09 on the fisheries rate
      # model, identically on all three backends. 1e-7 leaves room for a
      # different convergence path while still being ~100x tighter than any
      # scale or masking error could hide in.
      observed_total <- sum(fit$margins$observed)
      expect_identical(
        observed_total,
        as.double(sum(!fit$right_censored_events))
      )
      expect_equal(
        sum(fit$margins$expected),
        observed_total,
        tolerance = 1e-7
      )
    }
  )
}
