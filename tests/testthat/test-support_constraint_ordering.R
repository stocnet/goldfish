# Event-1 ordering: the FIRST dependent event's availability rides
# in the init (`active_dyad_init`), each later event's change in its likelihood
# slice. A fixture restricts the first event's receivers to a strict subset and
# leaves every later event unconstrained (the event-1 sender is observed only
# once), so the constraint's entire effect is the event-1 normalizer. If an
# engine read event 1 from the wrong slice (off-by-one), the constrained fit
# would collapse onto the unconstrained one — this guards against that.

make_ordering_fixture <- function(n_events = 60L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  lab <- actors$label
  n <- nrow(actors)
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = calls,
    nodes = actors
  )
  callsDep <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  callsDep <- callsDep[seq_len(n_events), ]
  df <- as.data.frame(callsDep)
  sender1 <- match(df$sender[1], lab)
  # The event-1 sender is observed only at event 1, so restricting its allowed
  # receivers isolates the effect to event 1's normalizer.
  stopifnot(sum(match(df$sender, lab) == sender1) == 1L)
  # A strict subset of receivers for the first event: the observed receiver plus
  # two others (so the observed dyad is never excluded).
  subset_recv <- unique(c(match(df$receiver[1], lab), 10L, 11L))
  allowed <- matrix(1, n, n, dimnames = list(lab, lab))
  diag(allowed) <- 0
  allowed[sender1, ] <- 0
  allowed[sender1, subset_recv] <- 1
  allowedNet <- make_network(matrix = allowed, nodes = actors, directed = TRUE)
  list(
    data = make_data(callsDep, call_network, calls, actors, allowedNet),
    subset_recv = subset_recv,
    n = n,
    n_events = n_events
  )
}

fit_ordering <- function(fx, engine, constrained = TRUE) {
  estimate_dynam(
    callsDep ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_estimation = set_algorithm_newton(engine = engine),
    support_constraint = if (constrained) ~ tie(allowedNet) else NULL
  )
}

test_that("the event-1 restriction lands (constrained != unconstrained)", {
  fx <- make_ordering_fixture()
  m_cstr <- fit_ordering(fx, "default")
  m_unc <- fit_ordering(fx, "default", constrained = FALSE)
  # Reading event 1 from the init (not the unconstrained set) moves the fit.
  expect_gt(abs(m_cstr$logLikelihood - m_unc$logLikelihood), 1e-3)
})

test_that("every wired engine consumes the event-1 slice identically", {
  fx <- make_ordering_fixture()
  m_def <- fit_ordering(fx, "default")
  m_gc <- fit_ordering(fx, "gather_compute")
  m_dc <- fit_ordering(fx, "default_c")
  expect_equal(m_gc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
  expect_equal(m_dc$logLikelihood, m_def$logLikelihood, tolerance = 1e-8)
  expect_equal(coef(m_gc), coef(m_def), tolerance = 1e-8)
  expect_equal(coef(m_dc), coef(m_def), tolerance = 1e-8)
})

test_that("opportunities_list[[1]] restricting event 1 equals the constraint", {
  # opportunities_list is soft-deprecated but the reference restriction here; it
  # forces the default engine (the point-fold dense path), so this leg is
  # default-only while the cross-engine agreement rides the support_constraint.
  withr::local_options(lifecycle_verbosity = "quiet")
  fx <- make_ordering_fixture()
  # Only event 1 is restricted (to the same subset); later events see everyone.
  opp <- lapply(
    seq_len(fx$n_events),
    function(e) if (e == 1L) fx$subset_recv else seq_len(fx$n)
  )
  m_opp <- estimate_dynam(
    callsDep ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    control_estimation = set_algorithm_newton(engine = "default"),
    control_preprocessing = set_preprocessing_opt(opportunities_list = opp)
  )
  m_cstr <- fit_ordering(fx, "default")
  # opp[[1]] rides in active_dyad_init exactly as the support atom does.
  expect_equal(m_opp$logLikelihood, m_cstr$logLikelihood, tolerance = 1e-8)
  expect_equal(coef(m_opp), coef(m_cstr), tolerance = 1e-8)
})
