# Null coverage and power of `test_gof()`, by simulation. Everything else in
# the suite checks that the statistic is computed correctly; this file checks
# that its p-value means what a p-value means -- uniform under a correctly
# specified model, and small under an alternative the test is meant to see.
#
# NOT_CRAN only. Measured runtime of this file: 35 seconds on a 2024 laptop,
# over 190 simulate-and-fit rounds. Seeds are fixed at every arm, so a failure
# is reproducible rather than a roll of the dice; the thresholds below are set
# well clear of the Monte Carlo error at the stated replication counts.
#
# Per-effect arms only. The omnibus combination is computed but not reported
# (see the experimental badge on `test_gof()`), and its validation -- joint
# null calibration and power against a block-localized alternative -- is a
# separate study that has to follow the release rather than ride in it.

# The DGP. Receivers are drawn from exactly the multinomial the choice
# sub-model specifies, so `nonlinear = FALSE` is a correctly specified fit and
# the test is under its null by construction.
#
# `nonlinear = TRUE` makes the true reciprocity effect `log1p(recip)` while the
# model fits `recip` linearly. The tie weights accumulate, so as the sequence
# runs the reciprocity counts grow and a linear coefficient is a compromise
# whose error changes over the sequence -- which is what makes a functional-form
# error visible to a test of whether a contribution is spread evenly. On a
# binary network the same alternative has no power at all, `log1p` being nearly
# linear on {0, 1}: the accumulating weight is the whole mechanism.
simulate_gof_sequence <- function(
  n_actors,
  n_events,
  theta,
  seed,
  nonlinear = FALSE
) {
  withr::local_seed(seed)
  withr::local_options(lifecycle_verbosity = "quiet")
  labels <- sprintf("A%d", seq_len(n_actors))
  network <- matrix(0, n_actors, n_actors, dimnames = list(labels, labels))
  sender <- integer(n_events)
  receiver <- integer(n_events)
  for (k in seq_len(n_events)) {
    from <- sample.int(n_actors, 1L)
    alternatives <- setdiff(seq_len(n_actors), from)
    reciprocity <- network[alternatives, from]
    eta <- theta[1] *
      network[from, alternatives] +
      theta[2] * if (nonlinear) 2 * log1p(reciprocity) else reciprocity
    # Shifted before exponentiating: the accumulating weights make the raw
    # linear predictor large enough to overflow late in a long sequence.
    to <- alternatives[
      sample.int(length(alternatives), 1L, prob = exp(eta - max(eta)))
    ]
    sender[k] <- from
    receiver[k] <- to
    network[from, to] <- network[from, to] + 1
  }
  events <- data.frame(
    time = seq_len(n_events),
    sender = labels[sender],
    receiver = labels[receiver],
    increment = 1
  )
  actors <- make_nodes(data.frame(label = labels, present = TRUE))
  net <- make_network(nodes = actors, directed = TRUE)
  net <- link_events(net, events, nodes = actors)
  dependent <- make_dependent_events(
    events = events,
    nodes = actors,
    default_network = net
  )
  estimate_dynam(
    dependent ~ inertia(net, weighted = TRUE) + recip(net, weighted = TRUE),
    sub_model = "choice",
    data = make_data(dependent, net, actors),
    control_algo = set_algorithm_newton(diagnostics = "scores")
  )
}

# One arm: `replications` sequences, one p-value per effect from each.
gof_p_values <- function(replications, seed_base, ...) {
  out <- vapply(
    seq_len(replications),
    function(i) {
      fit <- simulate_gof_sequence(seed = seed_base + i, ...)
      test_gof(fit)$effects$p_value
    },
    numeric(2)
  )
  t(out)
}

test_that("null p-values are uniform at n = 1000", {
  skip_on_cran()
  p <- gof_p_values(
    replications = 100L,
    seed_base = 1000L,
    n_actors = 15L,
    n_events = 1000L,
    theta = c(0.3, 0.4)
  )

  for (effect in seq_len(ncol(p))) {
    # Uniformity, not merely a correct size: the whole distribution is the
    # claim, and a test that is right at 5% can still be wrong elsewhere.
    expect_gt(
      suppressWarnings(stats::ks.test(p[, effect], "punif")$p.value),
      0.01
    )
    # And the size itself, against the Monte Carlo error of 100 draws: the
    # binomial standard error at 5% is 2.2 points, so 12% is more than three of
    # them away and a rejection here is not noise.
    expect_lt(mean(p[, effect] < 0.05), 0.12)
  }
})

test_that("null p-values are uniform at n = 5000", {
  skip_on_cran()
  # Fewer replications at the larger size: the arm exists to show the reference
  # does not drift as the sequence grows, which a coarser estimate settles.
  p <- gof_p_values(
    replications = 40L,
    seed_base = 5000L,
    n_actors = 15L,
    n_events = 5000L,
    theta = c(0.3, 0.4)
  )

  for (effect in seq_len(ncol(p))) {
    expect_gt(
      suppressWarnings(stats::ks.test(p[, effect], "punif")$p.value),
      0.01
    )
    expect_lt(mean(p[, effect] < 0.05), 0.20)
  }
})

test_that("the test has power against a non-linear reciprocity effect", {
  skip_on_cran()
  p <- gof_p_values(
    replications = 50L,
    seed_base = 7000L,
    n_actors = 15L,
    n_events = 1200L,
    theta = c(0.3, 0.4),
    nonlinear = TRUE
  )

  # The misspecification is on reciprocity, so that row is where the power is;
  # inertia picks up only what the two effects share through the same network.
  # Measured at these seeds: about 0.7 on reciprocity against 0.04 under the
  # null, so 0.4 is a floor with room, not a threshold fitted to the run.
  expect_gt(mean(p[, 2] < 0.05), 0.4)
})
