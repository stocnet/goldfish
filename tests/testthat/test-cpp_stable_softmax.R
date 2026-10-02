# Cross-engine agreement for the shared C++ stable-softmax helper.
# The converged-coefficient cross-engine checks in test-cpp_interface.R only
# exercise benign predictors at the optimum; here we drive the exported
# cpp multinomial estimators at a FIXED extreme parameter and confirm they
# agree with the R stable-softmax contribution path (finite logL / score /
# information where the earlier code overflowed to NaN or underflowed to -Inf).

choice_cpp <- getFromNamespace("estimate_DyNAM_choice", "goldfish")
rate_ordered_cpp <- getFromNamespace("estimate_DyNAM_rate_ordered", "goldfish")
choice_r <- getFromNamespace(
  "compute_event_contribution.goldfishLikReceiverMultinom",
  "goldfish"
)
rate_ordered_r <- getFromNamespace(
  "compute_event_contribution.goldfishLikSenderMultinom",
  "goldfish"
)

# 1-event fixtures with no updates: apply_flat_updates / apply_broadcast_updates
# are no-ops when the end pointer is 0, so the empty coded matrices are unread.
empty_update <- matrix(numeric(0), 4, 0)
empty_presence_update <- matrix(numeric(0), 2, 0)
zero_pointer <- 0

test_that("cpp DyNAM-choice matches R stable softmax at extreme beta", {
  skip_on_cran()
  n1 <- 4L
  n2 <- 4L
  id_sender <- 2L
  id_receiver <- 1L
  parameters <- 1

  # sender block (rows (id_sender-1)*n2 + 1:n2) holds the receiver predictors;
  # one dominant receiver (1000) forces the observed receiver's probability to
  # underflow, so the old log(exp/normalizer) was -Inf.
  stat_mat_init <- matrix(0, n1 * n2, 1L)
  block <- (id_sender - 1L) * n2
  stat_mat_init[block + 3L, 1L] <- 1000 # receiver j = 2 dominates

  res <- choice_cpp(
    parameters,
    matrix(c(id_sender, id_receiver), 2, 1),
    stat_mat_init,
    empty_update,
    zero_pointer,
    empty_update,
    zero_pointer,
    rep(1, n2),
    empty_presence_update,
    zero_pointer,
    n1,
    n2,
    twomode_or_reflexive = FALSE,
    active_dyad_is_point = FALSE
  )

  sender_block <- stat_mat_init[(block + 1L):(block + n2), , drop = FALSE]
  ref <- choice_r(
    spec = NULL,
    sender_block,
    activeDyad = c(id_sender, id_receiver),
    parameters = parameters,
    isRightCensored = FALSE,
    timespan = 1,
    allowReflexive = FALSE,
    is_two_mode = FALSE
  )

  expect_true(is.finite(res$intervalLogL[1]))
  expect_equal(res$intervalLogL[1], ref$logLikelihood, tolerance = 1e-8)
  expect_equal(
    as.numeric(res$derivative),
    as.numeric(ref$score),
    tolerance = 1e-8
  )
  expect_equal(res$fisher, ref$informationMatrix, tolerance = 1e-8)
  expect_true(all(is.finite(res$fisher)))
})

test_that("cpp rate-ordered matches R stable softmax at extreme beta", {
  skip_on_cran()
  n1 <- 4L
  # The sender loop's statistics are one row per sender (no receiver axis).
  n2 <- 1L
  id_sender <- 1L
  parameters <- 1

  # sender 2 dominates, the observed sender 1 underflows.
  sender_values <- c(0, 1000, 5, 2)
  stat_mat_init <- matrix(sender_values, n1, 1L)

  res <- rate_ordered_cpp(
    parameters,
    matrix(c(id_sender, 1L), 2, 1),
    stat_mat_init,
    empty_update,
    zero_pointer,
    empty_update,
    zero_pointer,
    rep(1, n1),
    empty_presence_update,
    zero_pointer,
    n1,
    n2,
    twomode_or_reflexive = TRUE
  )

  reduced <- matrix(sender_values, n1, 1L)
  ref <- rate_ordered_r(
    spec = NULL,
    reduced,
    activeDyad = c(id_sender, NA),
    parameters = parameters,
    isRightCensored = FALSE,
    timespan = 1,
    allowReflexive = TRUE,
    is_two_mode = FALSE
  )

  expect_true(is.finite(res$intervalLogL[1]))
  expect_equal(res$intervalLogL[1], ref$logLikelihood, tolerance = 1e-8)
  expect_equal(
    as.numeric(res$derivative),
    as.numeric(ref$score),
    tolerance = 1e-8
  )
  expect_equal(res$fisher, ref$informationMatrix, tolerance = 1e-8)
  expect_true(all(is.finite(res$fisher)))
})

test_that("gather multinomial matches cpp at a predictor that underflows", {
  skip_on_cran()
  # One dominant alternative drives the observed alternative's probability below
  # the double floor, so the pre-adoption `log(exp_obs / normalizer)` returned
  # -Inf while `x_obs - log_normalizer` stays finite. The gather kernel now
  # takes the same route as the cpp one, so the two agree here rather than
  # agreeing only on well-conditioned fixtures.
  gather_k <- getFromNamespace("compute_multinomial_selection", "goldfish")
  n <- 4L
  stat <- matrix(0, n, 1L)
  stat[2L, 1L] <- 1000 # alternative 2 dominates
  res <- gather_k(
    parameters = 1,
    stat_all_events = stat,
    n_candidates = n,
    selected = 0L, # the observed alternative is the underflowing one
    index_i = integer(0),
    index_j = integer(0),
    n_actors_1 = 0L,
    n_actors_2 = 0L,
    return_event_scores = FALSE,
    return_ranks = FALSE,
    return_margins = FALSE
  )
  expect_true(is.finite(res$logLikelihood))
  # log p_obs = x_obs - lse = 0 - 1000 (to within the other terms' rounding).
  expect_equal(as.numeric(res$intervalLogL), -1000, tolerance = 1e-9)
})

# --- gather Poisson kernel: shifted weights ----------------------------------
# The exact-time gather kernel derives every quantity from one shifted exp pass.
# The likelihood is deliberately NOT stabilized -- it needs the total rate on the
# absolute scale -- so these tests pin two things: that the restructure moved
# nothing, and that the quantities which ARE ratios or logs stay exact in the
# regime where the total rate has already overflowed or underflowed.

poisson_k <- getFromNamespace("compute_poisson_selection", "goldfish")

# Independent naive reference: the pre-change algebra, written out in R.
poisson_reference <- function(S, b, nc, sel, dt, dep) {
  ll <- 0
  d <- numeric(ncol(S))
  f <- matrix(0, ncol(S), ncol(S))
  st <- 1
  for (e in seq_along(nc)) {
    idx <- st:(st + nc[e] - 1L)
    x <- S[idx, , drop = FALSE]
    lam <- exp(x %*% b)[, 1]
    d <- d - dt[e] * as.vector(lam %*% x)
    f <- f + dt[e] * crossprod(x, x * lam)
    ll <- ll - dt[e] * sum(lam)
    if (dep[e] == 1) {
      ll <- ll + sum(x[sel[e] + 1L, ] * b)
      d <- d + x[sel[e] + 1L, ]
    }
    st <- st + nc[e]
  }
  list(logLikelihood = ll, derivative = d, fisher = f)
}

test_that("the shifted pass leaves the exact-time likelihood unmoved", {
  set.seed(7)
  s_mat <- matrix(round(rnorm(7 * 3), 3), 7, 3)
  b <- c(0.4, -0.7, 0.2)
  nc <- c(4L, 3L)
  sel <- c(1L, 0L)
  dt <- c(1.3, 2.1)
  # dependent, mixed, and fully right-censored: the censored branch contributes
  # its timing term but no observed statistic.
  for (dep in list(c(1, 1), c(1, 0), c(0, 0))) {
    res <- poisson_k(
      b,
      s_mat,
      nc,
      sel,
      dt,
      dep,
      integer(0),
      integer(0),
      0L,
      0L,
      FALSE,
      FALSE,
      FALSE
    )
    ref <- poisson_reference(s_mat, b, nc, sel, dt, dep)
    label <- paste(dep, collapse = "")
    expect_equal(res$logLikelihood, ref$logLikelihood, info = label)
    expect_equal(
      as.vector(res$derivative),
      ref$derivative,
      tolerance = 1e-12,
      info = label
    )
    expect_equal(res$fisher, ref$fisher, tolerance = 1e-12, info = label)
  }
})

test_that("total_rate is the absolute-scale sum, recovered from the normalizer", {
  set.seed(11)
  s_mat <- matrix(round(rnorm(5 * 2), 3), 5, 2)
  b <- c(0.3, 0.6)
  res <- poisson_k(
    b,
    s_mat,
    c(5L),
    0L,
    1.0,
    1,
    integer(0),
    integer(0),
    0L,
    0L,
    FALSE,
    FALSE,
    FALSE
  )
  expect_equal(as.numeric(res$total_rate), sum(exp(s_mat %*% b)))
})

test_that("the conditional component survives an overflowing total rate", {
  # One predictor past log(DBL_MAX): the total rate is Inf and the per-event
  # log-likelihood with it, but log p_obs = x_obs - lse is still exact. The
  # algebraic route (intervalLogL - log T + Dt*T) would be NaN here.
  s_mat <- matrix(c(1000, 999, 997), 3, 1)
  b <- 1
  res <- poisson_k(
    b,
    s_mat,
    c(3L),
    0L,
    1.0,
    1,
    integer(0),
    integer(0),
    0L,
    0L,
    FALSE,
    FALSE,
    FALSE
  )
  expect_true(is.infinite(as.numeric(res$total_rate)))
  expect_false(is.finite(res$logLikelihood))
  cond <- as.numeric(res$conditional_logl)
  expect_true(is.finite(cond))
  lse <- 1000 + log(sum(exp(c(0, -1, -3))))
  expect_equal(cond, 1000 - lse, tolerance = 1e-12)
})

test_that("the conditional component survives underflowing rates", {
  # Every rate subnormal: the raw ratio lambda_obs/sum(lambda) gives a spurious
  # 1 or a NaN, while x_obs - lse is exact.
  s_mat <- matrix(c(-800, -801, -803), 3, 1)
  b <- 1
  res <- poisson_k(
    b,
    s_mat,
    c(3L),
    0L,
    1.0,
    1,
    integer(0),
    integer(0),
    0L,
    0L,
    FALSE,
    FALSE,
    FALSE
  )
  expect_equal(as.numeric(res$total_rate), 0) # underflowed, correctly
  naive_ratio <- exp(-800) / sum(exp(c(-800, -801, -803)))
  expect_true(is.nan(naive_ratio)) # what the pre-change route would have given
  lse <- -800 + log(sum(exp(c(0, -1, -3))))
  expect_equal(as.numeric(res$conditional_logl), -800 - lse, tolerance = 1e-12)
})

# --- Exact-time diagnostics from the likelihood pass -------------------------
# The exact-time kernels take log T = log(normalizer) and p = e / normalizer
# from the raw pass the likelihood already made, and fall back to the
# max-shifted log-sum-exp only where the raw normalizer is not a normal
# positive double. The evaluator's seam forces the shifted route and reports
# how many intervals took it, so the two routes can be compared directly.

exact_time_engine <- function(name) {
  prep <- likelihood_pin_prep(
    baselines_model_grid()[[name]],
    likelihood_pin_data()
  )
  make_engine_evaluator(
    spec = prep$model_spec,
    stats_list = prep,
    parameters = numeric(0),
    backend = "cpp",
    has_intercept = isTRUE(prep$has_intercept),
    is_rate_model = identical(risk_set_axis(prep$model_spec), "sender"),
    seed_intercept = FALSE
  )
}

exact_time_pass <- function(engine, at, force = FALSE, everything = TRUE) {
  engine$evaluate(
    at,
    need_scores = everything,
    need_ranks = everything,
    need_margins = everything,
    need_total_rate = TRUE,
    need_probabilities = everything,
    need_availability = everything,
    need_conditional_scores = everything,
    force_shifted_normalizer = force,
    report_normalizer_route = TRUE
  )
}

exact_time_references <- function() {
  readRDS(test_path(
    "_references",
    "likelihood_v1",
    "likelihood_references.rds"
  ))
}

test_that("the raw and the shifted route give the same exact-time diagnostics", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  references <- exact_time_references()
  for (name in c("se_rem", "se_dynam_rate")) {
    engine <- exact_time_engine(name)
    at <- references[[name]]$at
    raw <- exact_time_pass(engine, at)
    shifted <- exact_time_pass(engine, at, force = TRUE)
    expect_equal(raw$n_shifted_normalizer, 0L, info = name)
    expect_equal(shifted$n_shifted_normalizer, engine$n_intervals, info = name)
    compared <- setdiff(
      names(raw),
      c("n_shifted_normalizer", "event_probabilities")
    )
    for (field in compared) {
      expect_equal(
        raw[[field]],
        shifted[[field]],
        tolerance = 1e-10,
        info = paste(name, field)
      )
    }
    expect_equal(
      lapply(raw$event_probabilities, as.numeric),
      lapply(shifted$event_probabilities, as.numeric),
      tolerance = 1e-10,
      info = name
    )
  }
})

test_that("an overflowing or vanishing raw normalizer takes the shifted route", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  references <- exact_time_references()
  for (name in c("se_rem", "se_dynam_rate")) {
    engine <- exact_time_engine(name)
    # Every linear predictor at +/- 800 through the intercept alone, far past
    # the double range on either side (exp overflows above ~710 and is 0
    # below ~-745), so the fixture does not sit on a threshold.
    for (intercept in c(800, -800)) {
      at <- c(intercept, rep(0, length(references[[name]]$at) - 1L))
      res <- exact_time_pass(engine, at)
      label <- paste(name, intercept)
      expect_equal(res$n_shifted_normalizer, engine$n_intervals, info = label)
      conditional <- as.numeric(res$conditional_logl)
      expect_true(
        all(is.finite(conditional[!is.na(conditional)])),
        info = label
      )
      if (intercept > 0) {
        # The likelihood stays non-finite exactly as before, so the Newton
        # step that reached here is still rejected.
        expect_false(is.finite(res$logLikelihood), info = label)
      } else {
        expect_true(is.finite(res$logLikelihood), info = label)
      }
    }
  }
})

test_that("a partly underflowing risk set keeps its normalizer and probabilities", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  references <- exact_time_references()
  engine <- exact_time_engine("se_rem")
  at <- references$se_rem$at
  # Inertia (the third coefficient) pushed far down: every dyad with a past
  # call drops hundreds of orders of magnitude below the rest of the risk set,
  # while the normalizer itself stays a normal double.
  at[3] <- -720
  raw <- exact_time_pass(engine, at)
  shifted <- exact_time_pass(engine, at, force = TRUE)
  expect_equal(raw$n_shifted_normalizer, 0L)
  expect_equal(
    raw$conditional_logl,
    shifted$conditional_logl,
    tolerance = 1e-10
  )
  totals <- vapply(raw$event_probabilities, sum, numeric(1))
  expect_equal(totals, rep(1, length(totals)), tolerance = 1e-12)
  differences <- unlist(Map(
    function(a, b) abs(as.numeric(a) - as.numeric(b)),
    raw$event_probabilities,
    shifted$event_probabilities
  ))
  expect_lt(max(differences), 1e-10)
})

test_that("the r backend agrees with the compiled fallback at the extremes", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  references <- exact_time_references()
  for (name in c("se_rem", "se_dynam_rate")) {
    prep <- likelihood_pin_prep(
      baselines_model_grid()[[name]],
      likelihood_pin_data()
    )
    needs <- list(
      scores = FALSE,
      ranks = FALSE,
      margins = FALSE,
      total_rate = TRUE,
      probabilities = FALSE,
      availability = FALSE,
      conditional_scores = FALSE,
      information_trace = FALSE
    )
    for (intercept in c(800, -800)) {
      at <- c(intercept, rep(0, length(references[[name]]$at) - 1L))
      cpp <- evaluate_engine_once(prep$model_spec, prep, at, "cpp", needs)
      r <- evaluate_engine_once(prep$model_spec, prep, at, "r", needs)
      label <- paste(name, intercept)
      expect_equal(
        as.numeric(r$conditional_logl),
        as.numeric(cpp$conditional_logl),
        tolerance = 1e-10,
        info = label
      )
      expect_identical(
        is.finite(r$logLikelihood),
        is.finite(cpp$logLikelihood),
        info = label
      )
    }
  }
})

test_that("the loglik diagnostic alone forms no probability vector", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  references <- exact_time_references()
  for (name in c("se_rem", "se_dynam_rate")) {
    engine <- exact_time_engine(name)
    res <- exact_time_pass(engine, references[[name]]$at, everything = FALSE)
    expect_equal(res$probability_buffer_size, 0, info = name)
  }
})
