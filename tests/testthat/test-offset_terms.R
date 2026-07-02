# offset() fixed-coefficient terms (design D7, group 5). An offset() wraps a
# term to hold its coefficient fixed (value via offset_coef) rather than
# estimate it; the statistic column is KEPT (contributes coef * stat), so the
# result equals holding that coefficient with the legacy fixed_parameters
# vector. offset() supersedes the positional fixed_parameters.

make_offset_fixture <- function() {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  callNetwork <- make_network(nodes = actors, directed = TRUE)
  callNetwork <- link_events(
    x = callNetwork,
    change_event = calls,
    nodes = actors
  )
  callsDependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = callNetwork
  )
  callsDependent <- callsDependent[1:150, ]
  make_data(callsDependent, callNetwork, calls, actors)
}

test_that("offset() is unwrapped, tagged, and its stat column kept", {
  d <- make_offset_fixture()
  parsed <- parse_formula(
    callsDependent ~ inertia + offset(recip) + trans,
    envir = d
  )
  # the inner call is parsed normally (effect name recovered, not "offset")
  expect_identical(
    vapply(parsed$rhs_names, "[[", character(1), 1),
    c("inertia", "recip", "trans")
  )
  # the middle term is tagged offset, the others are not
  expect_identical(
    unlist(parsed$offset_parameter),
    c(FALSE, TRUE, FALSE)
  )
})

test_that("a single offset fixes the right coefficient (equals fixed_parameters)", {
  d <- make_offset_fixture()
  m_off <- estimate_dynam(
    callsDependent ~ inertia + offset(recip) + trans,
    sub_model = "choice",
    data = d,
    control_estimation = set_estimation_opt(offset_coef = 2)
  )
  # the offset term's coefficient is held at 2 and its stat column is retained
  expect_identical(rownames(m_off$names), c("inertia", "recip", "trans"))
  expect_equal(m_off$parameters[2], 2)
  expect_true(GetFixed(m_off)[2])

  # identical to holding that coefficient via the legacy positional vector
  withr::local_options(lifecycle_verbosity = "quiet")
  m_leg <- estimate_dynam(
    callsDependent ~ inertia + recip + trans,
    sub_model = "choice",
    data = d,
    control_estimation = set_estimation_opt(fixed_parameters = c(NA, 2, NA))
  )
  expect_equal(coef(m_off), coef(m_leg), tolerance = 1e-6)
})

test_that("multiple offsets are aligned to offset_coef by formula order", {
  d <- make_offset_fixture()
  m <- estimate_dynam(
    callsDependent ~ inertia + offset(recip) + offset(trans),
    sub_model = "choice",
    data = d,
    control_estimation = set_estimation_opt(offset_coef = c(2, -1))
  )
  expect_equal(m$parameters[2], 2)
  expect_equal(m$parameters[3], -1)
  expect_equal(unname(GetFixed(m)), c(FALSE, TRUE, TRUE))
})

test_that("a rate offset shifts the rate and is accepted", {
  d <- make_offset_fixture()
  expect_no_warning(
    m <- estimate_dynam(
      callsDependent ~ 1 + offset(indeg) + outdeg,
      sub_model = "rate",
      data = d,
      control_estimation = set_estimation_opt(offset_coef = 0.5)
    )
  )
  # parameters are [Intercept, indeg (fixed), outdeg]
  expect_equal(m$parameters[2], 0.5)
  expect_true(GetFixed(m)[2])
})

test_that("a constant-across-alternatives offset in choice warns, not aborts", {
  d <- make_offset_fixture()
  expect_warning(
    m <- estimate_dynam(
      callsDependent ~ inertia + offset(indeg(callNetwork, type = "ego")),
      sub_model = "choice",
      data = d,
      control_estimation = set_estimation_opt(offset_coef = 1)
    ),
    "constant across the choice alternatives"
  )
  expect_s3_class(m, "result.goldfish")
})

test_that("offset_coef arity and pairing are validated", {
  d <- make_offset_fixture()
  # too many values
  expect_error(
    estimate_dynam(
      callsDependent ~ inertia + offset(recip),
      sub_model = "choice",
      data = d,
      control_estimation = set_estimation_opt(offset_coef = c(1, 2))
    ),
    "one value per"
  )
  # offset_coef with no offset term
  expect_error(
    estimate_dynam(
      callsDependent ~ inertia + recip,
      sub_model = "choice",
      data = d,
      control_estimation = set_estimation_opt(offset_coef = 1)
    ),
    "no .*offset.* terms|has no"
  )
})

test_that("fixed_parameters is soft-deprecated toward offset()", {
  withr::local_options(lifecycle_verbosity = "warning")
  lifecycle::expect_deprecated(
    set_estimation_opt(fixed_parameters = c(NA, 2)),
    "offset"
  )
})

test_that("fixed_parameters and offset_coef cannot both be supplied", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(
    set_estimation_opt(fixed_parameters = c(NA, 2), offset_coef = 2),
    "cannot both be supplied"
  )
})
