# support_constraint consumption for DyNAM choice_coordination (DyNAM-MM) across
# the r / gather / cpp backends. Coordination's
# likelihood is two-sided (`getLikelihoodMM` pairs both directed choices), so
# constraint is symmetrised (`support[i, j] & support[j, i]`) and folded into a
# dense point `active_dyad` consumed as the FULL risk mask — never a per-sender
# row. An all-allowing symmetric constraint is therefore an identity; a
# restricting (symmetric) one changes the estimate; and every engine agrees.

# Build the Fisheries coordination data with an undirected `allowedNet`
# support-constraint network sharing the states node set. `n_excluded` drops
# never-observed dyads symmetrically (so no observed dyad is ever excluded);
# `n_excluded = 0` allows every non-reflexive dyad (identity mask).
# `link_events`
# captures its event argument by name, so the loaded objects are referenced as
# bare symbols (mirroring `baselines_fisheries_data()`).
make_coord_fixture <- function(n_excluded = 0L, seed = 1L) {
  data("Fisheries_Treaties_6070", package = "goldfish", envir = environment())
  states <- make_nodes(states)
  states <- link_events(states, sovchanges, attribute = "present")
  states <- link_events(states, regchanges, attribute = "regime")
  states <- link_events(states, gdpchanges, attribute = "gdp")
  bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
  bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
  contignet <- make_network(contignet, nodes = states, directed = FALSE)
  contignet <- link_events(contignet, contigchanges, nodes = states)
  dep <- bilatchanges[bilatchanges$increment == 1, ]
  create_bilat <- make_dependent_events(
    events = dep,
    nodes = states,
    default_network = bilatnet
  )

  lab <- states$label
  n <- nrow(states)
  obs <- cbind(match(dep$sender, lab), match(dep$receiver, lab))
  allowed <- matrix(1, n, n, dimnames = list(lab, lab))
  diag(allowed) <- 0
  if (n_excluded > 0) {
    set.seed(seed)
    drawn <- 0L
    while (drawn < n_excluded) {
      i <- sample(n, 1)
      j <- sample(n, 1)
      observed <- any(
        (obs[, 1] == i & obs[, 2] == j) | (obs[, 1] == j & obs[, 2] == i)
      )
      if (i != j && !observed && allowed[i, j] == 1) {
        # symmetric exclusion: both directions dropped
        allowed[i, j] <- 0
        allowed[j, i] <- 0
        drawn <- drawn + 1L
      }
    }
  }
  allowedNet <- make_network(matrix = allowed, nodes = states, directed = FALSE)

  make_data(
    create_bilat,
    bilatnet,
    contignet,
    allowedNet,
    states,
    bilatchanges,
    contigchanges,
    sovchanges,
    regchanges,
    gdpchanges
  )
}

coord_formula <- create_bilat ~
  inertia + indeg + trans + tie(contignet)

test_that("an all-allowing coordination constraint is an identity", {
  skip_on_cran()
  d <- make_coord_fixture()
  opt <- set_algorithm_newton(
    backend = "r",
    max_iterations = 30,
    initial_damping = 40
  )
  m_cstr <- estimate_dynam(
    coord_formula,
    sub_model = "choice_coordination",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = opt
  )
  m_unc <- estimate_dynam(
    coord_formula,
    sub_model = "choice_coordination",
    data = d,
    control_algo = opt
  )
  expect_equal(coef(m_cstr), coef(m_unc), tolerance = 1e-6)
  expect_equal(m_cstr$log_likelihood, m_unc$log_likelihood, tolerance = 1e-6)
})

test_that("a restricting coordination constraint changes the estimate", {
  skip_on_cran()
  d <- make_coord_fixture(n_excluded = 2000L)
  opt <- set_algorithm_newton(
    backend = "r",
    max_iterations = 30,
    initial_damping = 40
  )
  m_cstr <- suppressWarnings(estimate_dynam(
    coord_formula,
    sub_model = "choice_coordination",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = opt
  ))
  m_unc <- estimate_dynam(
    coord_formula,
    sub_model = "choice_coordination",
    data = d,
    control_algo = opt
  )
  expect_gt(max(abs(coef(m_cstr) - coef(m_unc))), 1e-4)
})

test_that("coordination constraint runs natively on cpp", {
  skip_on_cran()
  d <- make_coord_fixture(n_excluded = 1000L)
  m_def <- suppressWarnings(estimate_dynam(
    coord_formula,
    sub_model = "choice_coordination",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(
      backend = "r",
      max_iterations = 30,
      initial_damping = 40
    )
  ))
  # estimate_DyNAM_MM reads the symmetrised dense point active_dyad cell-wise,
  # cpp matches the r backend exactly.
  m_dc <- suppressWarnings(estimate_dynam(
    coord_formula,
    sub_model = "choice_coordination",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = set_algorithm_newton(
      backend = "cpp",
      max_iterations = 30,
      initial_damping = 40
    )
  ))
  expect_equal(coef(m_dc), coef(m_def), tolerance = 1e-6)
  expect_equal(m_dc$log_likelihood, m_def$log_likelihood, tolerance = 1e-6)
})

test_that("gather runs a coordination constraint natively", {
  skip_on_cran()
  d <- make_coord_fixture(n_excluded = 1000L)
  opt <- function(backend) {
    set_algorithm_newton(
      backend = backend,
      max_iterations = 30,
      initial_damping = 40
    )
  }
  m_dc <- suppressWarnings(estimate_dynam(
    coord_formula,
    sub_model = "choice_coordination",
    data = d,
    support_constraint = ~ tie(allowedNet),
    control_algo = opt("cpp")
  ))
  # The gather now emits the symmetrically-folded off-diagonal dyad list (only
  # mask-allowed rows) plus the per-sender groups and (i,j)<->(j,i) pairing, and
  # the dyad-triangle kernel reads that ragged list directly — no square
  # candidate matrix, no redirect to cpp, no informational message.
  expect_no_message(
    m_gc <- suppressWarnings(estimate_dynam(
      coord_formula,
      sub_model = "choice_coordination",
      data = d,
      support_constraint = ~ tie(allowedNet),
      control_algo = opt("gather")
    ))
  )
  expect_equal(coef(m_gc), coef(m_dc), tolerance = 1e-6)
  expect_equal(m_gc$log_likelihood, m_dc$log_likelihood, tolerance = 1e-6)
})

test_that("a coordination constraint folds active_dyad symmetric point", {
  skip_on_cran()
  d <- make_coord_fixture(n_excluded = 1000L)
  prep <- estimate_dynam(
    coord_formula,
    sub_model = "choice_coordination",
    data = d,
    support_constraint = ~ tie(allowedNet),
    preprocessing_only = TRUE
  )
  expect_identical(prep$active_dyad_encoding, "point")
  # The folded mask is symmetric (both directions of the mutual likelihood).
  expect_true(isSymmetric(unname(prep$active_dyad_init)))
  expect_equal(sum(diag(prep$active_dyad_init)), 0)
})
