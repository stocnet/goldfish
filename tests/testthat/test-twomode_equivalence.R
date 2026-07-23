# Two-mode construction-path equivalence (design D8).
#
# The same two-mode data built as a mode-map stocnet and assembled through the
# legacy two-node-set constructors must estimate to identical coefficients on
# every engine -- the regression floor that catches a wrong side remap in either
# construction path. A mixed one/two-mode-layer object must estimate too.

# Assemble the legacy fixture's components (their recorded node-set names are
# `actors`/`clubs`, and each network/nodes object references its events object
# by name, so every piece must be bound and passed to make_data()).
legacy_twomode_data <- function() {
  fx <- suppressWarnings(make_legacy_fixture_twomode())
  actors <- fx$actors
  clubs <- fx$clubs
  membership <- fx$membership
  joins <- fx$joins
  actor_growth <- fx$actor_growth
  club_funding <- fx$club_funding
  joins_dependent <- fx$joins_dependent
  suppressWarnings(make_data(
    joins_dependent,
    membership,
    joins,
    actors,
    clubs,
    actor_growth,
    club_funding
  ))
}

test_that("a two-mode choice model agrees across construction paths", {
  legacy <- legacy_twomode_data()
  stocnet <- as_goldfish(make_stocnet_fixture_twomode_legacy_equiv())
  form <- membership ~ inertia + alter(budget)
  expect_equal(
    coef(estimate_dynam(form, sub_model = "choice", data = legacy)),
    coef(estimate_dynam(form, sub_model = "choice", data = stocnet)),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
})

test_that("a two-mode rate model agrees across construction paths", {
  legacy <- legacy_twomode_data()
  stocnet <- as_goldfish(make_stocnet_fixture_twomode_legacy_equiv())
  form <- membership ~ 1 + ego(size)
  expect_equal(
    coef(estimate_dynam(form, sub_model = "rate", data = legacy)),
    coef(estimate_dynam(form, sub_model = "rate", data = stocnet)),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
})

test_that("a two-mode REM agrees across construction paths", {
  legacy <- legacy_twomode_data()
  stocnet <- as_goldfish(make_stocnet_fixture_twomode_legacy_equiv())
  form <- membership ~ 1 + inertia + alter(budget)
  expect_equal(
    coef(estimate_rem(form, data = legacy)),
    coef(estimate_rem(form, data = stocnet)),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
})

test_that("a mixed one/two-mode-layer object estimates consistently", {
  # The multipartite fixture carries a two-mode focal `attend` (actor -> event)
  # alongside a one-mode covariate `coauthor` (actor -> actor): a model over the
  # two-mode focal that reads the one-mode covariate must estimate to finite
  # coefficients rather than tripping on the mixed side spaces.
  data <- as_goldfish(make_stocnet_fixture_multipartite())
  fit <- estimate_dynam(
    attend ~ inertia + mixed_trans(list(coauthor, attend)),
    sub_model = "choice",
    data = data
  )
  expect_s3_class(fit, "result.goldfish")
  expect_false(anyNA(coef(fit)))
})
