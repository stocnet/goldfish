# goldfish against classical fits of the SAME likelihood.
#
# Each sub-model here has a classical twin -- a conditional logit IS a
# stratified Cox, and an exact-time rate model IS a piecewise-exponential
# Poisson regression -- so a disagreement is a goldfish bug rather than a
# difference of model. The survival-derived numbers are frozen references
# minted by `_references/classical_v1/generate_classical_references.R`, whose
# design matrix is walked by hand from the event list: what these compare is
# the statistic and the likelihood together, not goldfish against itself.
#
# The `stats::glm` equivalences run live instead of frozen -- stats ships with
# R, so there is no provenance question and nothing to keep in step.

classical_references <- function() {
  path <- test_path("_references", "classical_v1", "classical_references.rds")
  skip_if_not(file.exists(path), "classical references not minted")
  readRDS(path)
}

# The fixture every reference was minted on. `start_time` is one unit before
# the first event so the opening interval has positive elapsed time, which the
# Poisson offset needs.
classical_fixture <- function() {
  data("social_evolution", package = "goldfish", envir = environment())
  calls <- social_evolution$ties[social_evolution$ties$layer == "calls", ]
  calls <- calls[order(as.numeric(calls$time)), ]
  list(
    data = social_evolution,
    calls = calls,
    times = as.numeric(calls$time),
    start_time = min(as.numeric(calls$time)) - 1,
    n_actors = nrow(social_evolution$nodes)
  )
}

# Independently-optimized parity: two optimizers reaching the same maximum
# from different code. Measured worst case across the routes is ~3e-08, so the
# gate is the package's ordinary 1e-6 discipline rather than anything tighter.
classical_tolerance <- 1e-6

test_that("the ordinal REM is the Cox partial likelihood", {
  skip_on_cran()
  refs <- classical_references()
  fx <- classical_fixture()

  fit <- estimate_rem(
    calls ~ inertia(calls, weighted = TRUE) + recip(calls, weighted = TRUE),
    sub_model = "rate_ordered",
    data = fx$data,
    control_prep = set_preprocessing(start_time = fx$start_time),
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  )
  expect_equal(
    unname(coef(fit)),
    refs$rem_ordered$coefficients,
    tolerance = classical_tolerance
  )
  expect_equal(
    as.numeric(logLik(fit)),
    refs$rem_ordered$loglik,
    tolerance = classical_tolerance
  )
})

test_that("the choice sub-model is a conditional logit", {
  skip_on_cran()
  refs <- classical_references()
  fx <- classical_fixture()

  fit <- estimate_dynam(
    calls ~ inertia(calls, weighted = TRUE) + recip(calls, weighted = TRUE),
    sub_model = "choice",
    data = fx$data,
    control_prep = set_preprocessing(start_time = fx$start_time),
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  )
  expect_equal(
    unname(coef(fit)),
    refs$dynam_choice$coefficients,
    tolerance = classical_tolerance
  )
  expect_equal(
    as.numeric(logLik(fit)),
    refs$dynam_choice$loglik,
    tolerance = classical_tolerance
  )
})

test_that("the ordinal rate sub-model is a conditional logit over senders", {
  skip_on_cran()
  refs <- classical_references()
  fx <- classical_fixture()

  fit <- estimate_dynam(
    calls ~ indeg(calls, weighted = TRUE) + outdeg(calls, weighted = TRUE),
    sub_model = "rate_ordered",
    data = fx$data,
    control_prep = set_preprocessing(start_time = fx$start_time),
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  )
  expect_equal(
    unname(coef(fit)),
    refs$dynam_rate_ordered$coefficients,
    tolerance = classical_tolerance
  )
  expect_equal(
    as.numeric(logLik(fit)),
    refs$dynam_rate_ordered$loglik,
    tolerance = classical_tolerance
  )
})

test_that("the Schoenfeld residuals follow survival's convention", {
  skip_on_cran()
  refs <- classical_references()
  fx <- classical_fixture()
  reference <- refs$residuals_shared_theta

  fit <- estimate_rem(
    calls ~ inertia(calls, weighted = TRUE) + recip(calls, weighted = TRUE),
    sub_model = "rate_ordered",
    data = fx$data,
    control_prep = set_preprocessing(start_time = fx$start_time),
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "scores"))
  )
  # The reference was evaluated AT this vector, so the comparison below is a
  # residual-convention check only as long as the vector still agrees.
  expect_equal(
    unname(coef(fit)),
    reference$theta,
    tolerance = classical_tolerance
  )

  expect_equal(
    unname(residuals(fit, type = "schoenfeld")),
    reference$schoenfeld,
    tolerance = classical_tolerance
  )
  # This is the one that pins the Grambsch-Therneau scaling, the `n` in
  # `theta + n I^-1 s_k` included: an n-fold error leaves every rank and
  # every plot shape intact and only shows up against a second implementation.
  expect_equal(
    unname(residuals(fit, type = "scaled_schoenfeld")),
    reference$scaled_schoenfeld,
    tolerance = classical_tolerance
  )
  # The convention's own identity: the residuals are centred on the estimate.
  # Exactly so only at an exact maximum -- the mean carries `n I^-1` times the
  # total score, which is zero only there -- so the gate is the estimator's
  # convergence tolerance, not machine precision.
  expect_equal(
    unname(colMeans(residuals(fit, type = "scaled_schoenfeld"))),
    unname(coef(fit)),
    tolerance = classical_tolerance
  )
})

test_that("an exact-time rate model is a Poisson regression with an offset", {
  skip_on_cran()
  fx <- classical_fixture()
  n <- fx$n_actors
  n_events <- nrow(fx$calls)
  sender <- fx$calls$from
  receiver <- fx$calls$to

  # The sender-level design, walked here rather than read off the fit: the
  # comparison is only worth making against statistics goldfish did not
  # compute. One row per interval and actor.
  net <- matrix(0, n, n)
  rows <- n_events * n
  design <- list(
    chosen = integer(rows),
    indeg = numeric(rows),
    outdeg = numeric(rows),
    dt = numeric(rows)
  )
  prev <- fx$start_time
  pos <- 0L
  for (k in seq_len(n_events)) {
    idx <- pos + seq_len(n)
    design$indeg[idx] <- colSums(net)
    design$outdeg[idx] <- rowSums(net)
    design$chosen[idx] <- as.integer(seq_len(n) == sender[k])
    design$dt[idx] <- fx$times[k] - prev
    prev <- fx$times[k]
    pos <- pos + n
    net[sender[k], receiver[k]] <- net[sender[k], receiver[k]] + 1
  }
  design <- as.data.frame(design)
  # The piecewise-exponential equivalence needs every interval to have
  # positive elapsed time, or the offset is not finite.
  expect_true(all(design$dt > 0))

  fit <- estimate_dynam(
    calls ~ 1 + indeg(calls, weighted = TRUE) + outdeg(calls, weighted = TRUE),
    sub_model = "rate",
    data = fx$data,
    control_prep = set_preprocessing(start_time = fx$start_time),
    control_algo = set_algorithm_newton(diagnostics = "loglik"),
    # The score comparison below re-evaluates away from the maximum, which is
    # an evaluation pass over the statistics rather than a stored primitive.
    return_preprocessed = TRUE
  )
  twin <- stats::glm(
    chosen ~ indeg + outdeg + offset(log(dt)),
    data = design,
    family = stats::poisson()
  )
  expect_equal(
    unname(coef(fit)),
    unname(coef(twin)),
    tolerance = classical_tolerance
  )

  # The two log-likelihoods differ by a constant in theta: the Poisson form
  # carries a `log dt` per observed event that the rate likelihood does not.
  event_dt <- fx$times - c(fx$start_time, fx$times[-n_events])
  expect_equal(
    as.numeric(logLik(fit)),
    as.numeric(logLik(twin)) - sum(log(event_dt)),
    tolerance = classical_tolerance
  )

  # A constant in theta drops out of the gradient, so at a shared vector the
  # two scores agree exactly rather than up to that constant.
  shared <- coef(fit)
  shared[2] <- shared[2] + 0.01
  model_matrix <- cbind(
    Intercept = 1,
    indeg = design$indeg,
    outdeg = design$outdeg
  )
  mu <- exp(drop(model_matrix %*% unname(shared)) + log(design$dt))
  expect_equal(
    unname(evaluate_model(fit, at = shared, return = "score")$score),
    unname(drop(crossprod(model_matrix, design$chosen - mu))),
    tolerance = classical_tolerance
  )
})

test_that("scaled Schoenfeld residuals carry the regime a coefficient is in", {
  skip_on_cran()
  # A sequence made of two regimes with different inertia coefficients. The
  # Grambsch-Therneau residual is a one-step linearization around the pooled
  # fit, so its within-regime means are pulled toward that fit rather than
  # reaching the per-regime refits -- but they must still separate, in the
  # refits' own direction and on their order of magnitude. That is what the
  # `n` in the scaling buys: without it the residuals collapse onto the
  # pooled estimate and the two regimes become indistinguishable.
  set.seed(20260730)
  n_actors <- 20
  n_per <- 250
  beta <- c(0.35, 0.95)

  net <- matrix(0, n_actors, n_actors)
  n_events <- 2 * n_per
  senders <- integer(n_events)
  receivers <- integer(n_events)
  regime <- rep(c(1L, 2L), each = n_per)
  for (k in seq_len(n_events)) {
    i <- sample.int(n_actors, 1)
    alts <- setdiff(seq_len(n_actors), i)
    eta <- beta[regime[k]] * net[i, alts]
    weights <- exp(eta - max(eta))
    j <- alts[sample.int(length(alts), 1, prob = weights / sum(weights))]
    senders[k] <- i
    receivers[k] <- j
    net[i, j] <- net[i, j] + 1
  }

  labels <- sprintf("A%02d", seq_len(n_actors))
  events <- data.frame(
    from = senders,
    to = receivers,
    time = seq_len(n_events),
    layer = "contact",
    stringsAsFactors = FALSE
  )
  build <- function(rows) {
    manynet::make_stocnet(
      info = list(
        name = "two regimes",
        focal = "contact",
        update = c(contact = "increment"),
        directed = c(contact = TRUE),
        observation = c(contact = "event")
      ),
      nodes = data.frame(label = labels, stringsAsFactors = FALSE),
      ties = events[rows, ]
    )
  }
  fit_model <- function(rows, diagnostics) {
    estimate_dynam(
      contact ~ inertia(contact, weighted = TRUE),
      sub_model = "choice",
      data = build(rows),
      control_algo = set_algorithm_newton(diagnostics = diagnostics)
    )
  }

  pooled <- fit_model(seq_len(n_events), c("loglik", "scores"))
  scaled <- residuals(pooled, type = "scaled_schoenfeld")
  expect_equal(
    unname(colMeans(scaled)),
    unname(coef(pooled)),
    tolerance = classical_tolerance
  )

  regime_means <- c(
    mean(scaled[regime == 1L, 1]),
    mean(scaled[regime == 2L, 1])
  )
  refits <- c(
    unname(coef(fit_model(which(regime == 1L), "loglik"))),
    unname(coef(fit_model(which(regime == 2L), "loglik")))
  )

  # Ordered as the refits are, and straddling the pooled estimate they were
  # linearized around.
  expect_lt(regime_means[1], unname(coef(pooled)))
  expect_gt(regime_means[2], unname(coef(pooled)))
  expect_lt(refits[1], refits[2])

  # On the refits' order of magnitude. An unscaled residual would put the
  # separation at roughly 1/n of this, which is what the bound rules out: the
  # measured ratio is ~0.36 against a gate of 0.1, while dropping the `n`
  # gives ~0.0007.
  spread_scaled <- diff(regime_means)
  spread_refit <- diff(refits)
  expect_gt(spread_scaled, 0.1 * spread_refit)
  expect_lt(spread_scaled, 2 * spread_refit)
})
