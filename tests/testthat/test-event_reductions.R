# The shared per-event reductions (src/event_reductions.h). Every backend forms
# a weight vector `w` over the event's risk set and a scale `c`, and the
# primitives are reductions of that pair: rank counts strictly-greater weights,
# margins accumulate c * w by actor, and the score is X_obs - c * w'X. These
# tests drive the C++ helpers directly through the test-only probe, so a
# reduction bug shows up here rather than as a coefficient mismatch nine kernels
# away.
#
# Two contract points the fixtures honour, because every kernel does: `obs` is
# 0-based, and `w` is ZERO outside the risk set (stable_softmax_masked returns 0
# for masked entries, REM zeroes explicitly, the rate kernel only writes active
# senders, gather slices are pre-masked). `allowed` is then only a skip hint.

probe <- getFromNamespace("event_reductions_probe", "goldfish")

# A single event: 5 alternatives, 3 effects, position 3 (1-based) outside the
# risk set and therefore carrying weight 0, as a kernel would hand it over.
reductions_fixture <- function() {
  list(
    X = matrix(
      c(
        0.5,
        -1.2,
        0.3,
        1.1,
        0.4,
        -0.7,
        -0.2,
        0.9,
        1.5,
        0.8,
        -0.5,
        0.2,
        -1.0,
        0.6,
        -0.4
      ),
      nrow = 5,
      ncol = 3,
      byrow = TRUE
    ),
    w = c(2, 5, 0, 4, 3),
    allowed = c(1, 1, 0, 1, 1)
  )
}

reduce <- function(fx, c, obs, dependent = TRUE, ...) {
  args <- list(
    X = fx$X,
    w = fx$w,
    c = c,
    obs = obs,
    allowed = fx$allowed,
    dependent = dependent,
    index_a = integer(0),
    index_b = integer(0),
    n_a = length(fx$w),
    n_b = 0L
  )
  do.call(probe, utils::modifyList(args, list(...)))
}

test_that("rank counts strictly greater weights", {
  fx <- reductions_fixture()
  # weights 2 5 0 4 3
  expect_equal(reduce(fx, 1, obs = 1L)$rank, 1L) # w = 5, the largest
  expect_equal(reduce(fx, 1, obs = 3L)$rank, 2L) # w = 4, only 5 beats it
  expect_equal(reduce(fx, 1, obs = 0L)$rank, 4L) # w = 2, beaten by 5, 4, 3
})

test_that("tied weights share the better rank", {
  fx <- reductions_fixture()
  fx$w <- c(4, 4, 0, 4, 3)
  # Strict `>` means none of the three tied alternatives outranks another.
  for (obs in c(0L, 1L, 3L)) {
    expect_equal(reduce(fx, 1, obs = obs)$rank, 1L, info = paste("obs", obs))
  }
})

test_that("the mask is a skip hint, not a semantic switch", {
  # Given the zeroed-w contract, supplying `allowed` or not must give identical
  # results -- it only lets the accumulation loop step over known zeros. This is
  # the property that lets event_score_row() omit the mask entirely.
  fx <- reductions_fixture()
  c_prob <- 1 / sum(fx$w)
  with_mask <- reduce(fx, c = c_prob, obs = 3L)
  without <- reduce(fx, c = c_prob, obs = 3L, allowed = numeric(0))
  expect_equal(with_mask$rank, without$rank)
  expect_equal(with_mask$expected_a, without$expected_a)
  expect_equal(with_mask$observed_a, without$observed_a)
  expect_equal(with_mask$score, without$score)
})

test_that("margins on the probability scale total one per event", {
  fx <- reductions_fixture()
  res <- reduce(fx, c = 1 / sum(fx$w), obs = 3L)
  expect_equal(sum(res$expected_a), 1)
  expect_equal(as.vector(res$expected_a)[3], 0) # outside the risk set
  expect_equal(as.vector(res$observed_a), c(0, 0, 0, 1, 0))
})

test_that("margins on the expected-count scale carry the interval", {
  fx <- reductions_fixture()
  timespan <- 2.5
  res <- reduce(fx, c = timespan, obs = 3L)
  # sum_j c * w_j = Dt * total_rate over the risk set.
  expect_equal(sum(res$expected_a), timespan * sum(fx$w))
})

test_that("a right-censored interval contributes no observed count", {
  fx <- reductions_fixture()
  res <- reduce(fx, c = 2.5, obs = 3L, dependent = FALSE)
  expect_equal(as.vector(res$observed_a), rep(0, 5))
  # ...but its expected contribution still accumulates.
  expect_gt(sum(res$expected_a), 0)
})

test_that("two sides scatter one contribution into both accumulators", {
  # The REM / REM_ordered / MM shape: each risk-set position is a dyad, feeding
  # a sender margin and a receiver margin from the same contribution, in one
  # pass over the risk set.
  fx <- reductions_fixture()
  sender <- c(0L, 0L, 1L, 1L, 1L) # dyad -> sender slot
  receiver <- c(0L, 1L, 0L, 1L, 2L) # dyad -> receiver slot
  res <- reduce(
    fx,
    c = 1 / sum(fx$w),
    obs = 3L,
    index_a = sender,
    index_b = receiver,
    n_a = 2L,
    n_b = 3L
  )
  # Both sides see the same total, since each is a regrouping of one pass.
  expect_equal(sum(res$expected_a), 1)
  expect_equal(sum(res$expected_b), 1)
  # The observed dyad (position 3) is sender 1, receiver 1 in 0-based slots.
  expect_equal(as.vector(res$observed_a), c(0, 1))
  expect_equal(as.vector(res$observed_b), c(0, 1, 0))
})

test_that("the score is the observed statistic minus the weighted mean", {
  fx <- reductions_fixture()
  c_prob <- 1 / sum(fx$w)
  expected_mean <- c_prob * as.vector(fx$w %*% fx$X)

  res <- reduce(fx, c = c_prob, obs = 3L)
  expect_equal(as.vector(res$score), fx$X[4, ] - expected_mean)

  # A right-censored interval contributes the weighted mean only.
  res_rc <- reduce(fx, c = c_prob, obs = 3L, dependent = FALSE)
  expect_equal(as.vector(res_rc$score), -expected_mean)
})

# --- R mirror parity ---------------------------------------------------------
# The r backend cannot call the inline C++, so it mirrors the same three
# reductions. These tests pin the two implementations to each other on
# constructed inputs -- the mechanism that keeps a mirror from drifting, and the
# reason the R backend can be trusted as the reference in cross-backend parity.
# The mirror is 1-based, the probe 0-based.

mirror_rank <- getFromNamespace("rank_of_observed", "goldfish")
mirror_margins <- getFromNamespace("accumulate_margins", "goldfish")
mirror_score <- getFromNamespace("event_score_row", "goldfish")

test_that("the R mirror agrees with the C++ helper on ranks", {
  fx <- reductions_fixture()
  for (obs in seq_along(fx$w)) {
    expect_identical(
      mirror_rank(fx$w, obs),
      reduce(fx, 1, obs = obs - 1L)$rank,
      info = paste("obs", obs)
    )
  }
  # ...including under ties, where the strict `>` rule has to match exactly.
  fx$w <- c(4, 4, 0, 4, 3)
  for (obs in seq_along(fx$w)) {
    expect_identical(mirror_rank(fx$w, obs), reduce(fx, 1, obs = obs - 1L)$rank)
  }
})

test_that("the R mirror agrees with the C++ helper on scores", {
  fx <- reductions_fixture()
  for (scale in list(1 / sum(fx$w), 2.5)) {
    for (dependent in c(TRUE, FALSE)) {
      expect_equal(
        mirror_score(fx$X, fx$w, scale, observed = 4L, dependent = dependent),
        as.vector(reduce(fx, c = scale, obs = 3L, dependent = dependent)$score),
        tolerance = 1e-10
      )
    }
  }
})

test_that("the R mirror agrees with the C++ helper on margins, both sides", {
  fx <- reductions_fixture()
  sender <- c(1L, 1L, 2L, 2L, 2L) # 1-based for the mirror
  receiver <- c(1L, 2L, 1L, 2L, 3L)
  scale <- 1 / sum(fx$w)

  mirrored <- mirror_margins(
    fx$w,
    c = scale,
    observed = 4L,
    dependent = TRUE,
    sides = list(
      list(index = sender, observed = numeric(2), expected = numeric(2)),
      list(index = receiver, observed = numeric(3), expected = numeric(3))
    )
  )
  cpp <- reduce(
    fx,
    c = scale,
    obs = 3L,
    index_a = sender - 1L,
    index_b = receiver - 1L,
    n_a = 2L,
    n_b = 3L
  )

  expect_equal(
    mirrored[[1]]$expected,
    as.vector(cpp$expected_a),
    tolerance = 1e-10
  )
  expect_equal(
    mirrored[[2]]$expected,
    as.vector(cpp$expected_b),
    tolerance = 1e-10
  )
  expect_equal(mirrored[[1]]$observed, as.vector(cpp$observed_a))
  expect_equal(mirrored[[2]]$observed, as.vector(cpp$observed_b))
})

test_that("the R mirror agrees on a single side with implicit slots", {
  fx <- reductions_fixture()
  scale <- 2.5 # the exact-time compensator scale
  mirrored <- mirror_margins(
    fx$w,
    c = scale,
    observed = 4L,
    dependent = TRUE,
    sides = list(list(
      index = NULL,
      observed = numeric(5),
      expected = numeric(5)
    ))
  )
  cpp <- reduce(fx, c = scale, obs = 3L)
  expect_equal(
    mirrored[[1]]$expected,
    as.vector(cpp$expected_a),
    tolerance = 1e-10
  )
  expect_equal(mirrored[[1]]$observed, as.vector(cpp$observed_a))
})

# --- gather index boundary ---------------------------------------------------
# `margin_slots()` translates a gather stack's 1-based per-row actor index into
# the 0-based slots the C++ reduction scatters into. The NA case is the one that
# matters: the sender models reduce the receiver axis away and carry
# `index_j = NA`, and NA_integer_ read into an unsigned index would be garbage
# rather than an error.

margin_slots <- getFromNamespace("margin_slots", "goldfish")

test_that("margin_slots zero-bases a present axis", {
  expect_identical(margin_slots(c(1L, 1L, 2L, 3L)), c(0L, 0L, 1L, 2L))
})

test_that("margin_slots reports an absent axis as empty", {
  # The sender models' index_j, and the defensive NULL / zero-length cases.
  expect_identical(margin_slots(rep(NA_integer_, 4)), integer(0))
  expect_identical(margin_slots(NULL), integer(0))
  expect_identical(margin_slots(integer(0)), integer(0))
})

# --- gather multinomial kernel primitives -------------------------------------
# The kernel carries no reduction arithmetic of its own -- it calls the shared
# helpers -- so these tests check the wiring and the identities the primitives
# must satisfy, against independent R computations.

multinomial_k <- getFromNamespace("compute_multinomial_selection", "goldfish")

multinomial_fixture <- function() {
  set.seed(5)
  list(
    s_mat = matrix(round(rnorm(7 * 2), 3), 7, 2),
    b = c(0.5, -0.3),
    nc = c(4L, 3L),
    sel = c(2L, 1L),
    # sender and receiver slot per row, 0-based, as the gather stack carries
    ii = c(1L, 1L, 1L, 1L, 2L, 2L, 2L) - 1L,
    jj = c(1L, 2L, 3L, 4L, 1L, 2L, 3L) - 1L
  )
}

test_that("gather event_scores column sums equal the aggregate score", {
  fx <- multinomial_fixture()
  res <- multinomial_k(
    fx$b,
    fx$s_mat,
    fx$nc,
    fx$sel,
    fx$ii,
    fx$jj,
    2L,
    4L,
    TRUE,
    TRUE,
    TRUE
  )
  # The per-event score is the increment the derivative already accumulates, so
  # storing it per event must sum back to the aggregate exactly.
  expect_equal(
    colSums(res$event_scores),
    as.vector(res$derivative),
    tolerance = 1e-12
  )
})

test_that("gather ranks and margins match an independent computation", {
  fx <- multinomial_fixture()
  res <- multinomial_k(
    fx$b,
    fx$s_mat,
    fx$nc,
    fx$sel,
    fx$ii,
    fx$jj,
    2L,
    4L,
    TRUE,
    TRUE,
    TRUE
  )
  expected_rank <- integer(2)
  expected_margin <- numeric(4)
  observed_margin <- numeric(4)
  st <- 1L
  for (e in seq_along(fx$nc)) {
    idx <- st:(st + fx$nc[e] - 1L)
    p <- exp(fx$s_mat[idx, , drop = FALSE] %*% fx$b)[, 1]
    p <- p / sum(p)
    expected_rank[e] <- 1L + sum(p > p[fx$sel[e] + 1L])
    slot <- fx$jj[idx] + 1L
    expected_margin[slot] <- expected_margin[slot] + p
    observed_margin[slot[fx$sel[e] + 1L]] <-
      observed_margin[slot[fx$sel[e] + 1L]] + 1
    st <- st + fx$nc[e]
  }
  expect_identical(as.integer(res$observed_rank), expected_rank)
  expect_equal(
    as.vector(res$margin_expected_receiver),
    expected_margin,
    tolerance = 1e-12
  )
  expect_equal(as.vector(res$margin_observed_receiver), observed_margin)
  # On the probability scale the expected margins total the event count at ANY
  # parameter vector -- each event contributes exactly 1.
  expect_equal(sum(res$margin_expected_receiver), length(fx$nc))
})

test_that("gather primitives are absent unless requested", {
  fx <- multinomial_fixture()
  res <- multinomial_k(
    fx$b,
    fx$s_mat,
    fx$nc,
    fx$sel,
    fx$ii,
    fx$jj,
    2L,
    4L,
    FALSE,
    FALSE,
    FALSE
  )
  expect_length(res$event_scores, 0)
  expect_length(res$observed_rank, 0)
  expect_length(res$margin_expected_receiver, 0)
})

# --- gather Poisson kernel: dual margin scales --------------------------------
# Exact-time sub-models carry margins on two scales from one weight vector. The
# identities are what tell them apart, so they are what these tests assert.

test_that("the two exact-time margin scales have their distinguishing totals", {
  skip_on_cran()
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()$se_dynam_rate
  fit <- suppressWarnings(estimate_dynam(
    spec$formula,
    data = data_list$social_evolution,
    sub_model = spec$sub_model,
    control_algo = set_algorithm_newton(
      backend = "gather",
      diagnostics = c("loglik", "margins")
    ),
    progress = FALSE
  ))
  n_dependent <- sum(fit$margins$observed)
  # Compensator scale: totals the event count only because this is the MLE (the
  # intercept score equation), which is what makes observed-minus-expected the
  # martingale residual.
  expect_equal(sum(fit$margins$expected), n_dependent, tolerance = 1e-6)
  # Probability scale: each dependent event contributes exactly 1, so this holds
  # at any parameter vector -- the parallel-to-choice calibration map.
  expect_equal(sum(fit$margins$expected_probability), n_dependent)
})

test_that("the probability scale totals events, not intervals", {
  skip_on_cran()
  # Away from the MLE the two scales must separate: the compensator drifts, the
  # probability scale does not. A model with right-censored intervals also pins
  # that the probability scale counts events rather than intervals.
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()$se_dynam_rate
  fit <- suppressWarnings(estimate_dynam(
    spec$formula,
    data = data_list$social_evolution,
    sub_model = spec$sub_model,
    control_algo = set_algorithm_newton(
      backend = "gather",
      diagnostics = c("loglik", "margins"),
      initial_parameters = rep(0, 4),
      max_iterations = 0
    ),
    progress = FALSE
  ))
  n_dependent <- sum(fit$margins$observed)
  expect_equal(sum(fit$margins$expected_probability), n_dependent)
  expect_false(isTRUE(all.equal(sum(fit$margins$expected), n_dependent)))
})

test_that("gather and cpp agree on the exact-time margins", {
  skip_on_cran()
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()$se_dynam_rate
  beta <- suppressWarnings(baselines_fit(spec, "cpp", data_list))$parameters
  pinned <- function(backend) {
    suppressWarnings(estimate_dynam(
      spec$formula,
      data = data_list$social_evolution,
      sub_model = spec$sub_model,
      control_algo = set_algorithm_newton(
        backend = backend,
        diagnostics = c("loglik", "margins"),
        initial_parameters = beta,
        max_iterations = 0
      ),
      progress = FALSE
    ))$margins
  }
  m_cpp <- pinned("cpp")
  m_gather <- pinned("gather")
  # Same shape (the cpp backend has no probability variant yet -- task 5.1), and
  # the shared compensator agrees within the cross-backend tolerance.
  expect_equal(m_gather$observed, m_cpp$observed)
  expect_equal(m_gather$expected, m_cpp$expected, tolerance = 1e-10)
})
