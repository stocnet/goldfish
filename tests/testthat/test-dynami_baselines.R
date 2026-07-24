# Frozen DyNAM-i coefficient baselines and stocnet-vs-constructor equivalence.
#
# The vignette M1 rate and choice models on the RFID `video` data must reproduce
# their recorded coefficients to 1e-6, so an upstream change to the interaction
# preprocessing / estimation adapter is caught. The joining choice set is the
# groups occupied at the decision point (Hoffman et al. Eq. 8's denominator over
# the present second-mode nodes, own singleton included). These run under
# `skip_on_cran()`, like the one- and two-mode coefficient baselines; a
# `NOT_CRAN=true` run must report them PASS, not SKIP.

rfid_dynami_stocnet <- function() {
  env <- new.env()
  data("RFID_Validity_Study", package = "goldfish", envir = env)
  participants <- env$participants
  participants$label <- as.character(participants$label)
  make_groups_interaction(env$video, participants, seed_randomization = 1)
}

# The M1 rate formula uses bare nodal attributes (resolved off the actor node
# set) and the focal layer name; `known.before` is a bare covariate matrix that
# resolves through the calling frame.
rfid_rate_formula <- function(known.before) {
  interactions ~
    1 +
    intercept(interactions, joining = 1) +
    ego(age, joining = 1, subType = "centered") +
    ego(age, joining = -1, subType = "centered") +
    diff(age, joining = -1, subType = "averaged_sum") +
    diff(level, joining = -1, subType = "averaged_sum") +
    same(gender, joining = -1, subType = "proportion") +
    same(group, joining = -1, subType = "proportion") +
    tie(known.before, joining = -1, subType = "proportion")
}

rfid_choice_formula <- function(known.before) {
  interactions ~
    diff(age, subType = "averaged_sum") +
    diff(level, subType = "averaged_sum") +
    same(gender, subType = "proportion") +
    same(group, subType = "proportion") +
    tie(known.before, subType = "proportion")
}

test_that("frozen DyNAM-i rate baseline (RFID M1)", {
  skip_on_cran()
  env <- new.env()
  data("RFID_Validity_Study", package = "goldfish", envir = env)
  known.before <- env$known.before
  fit <- estimate_dynami(
    rfid_rate_formula(known.before),
    sub_model = "rate",
    data = rfid_dynami_stocnet(),
    control_estimation = set_estimation_opt(engine = "default")
  )
  expect_true(fit$convergence$isConverged)
  expect_equal(
    coef(fit),
    c(
      -6.7613145017376,
      1.5223072232078,
      0.061687165855997,
      0.027957711394444,
      -0.16364302824463,
      0.36146846004928,
      0.027282042286209,
      0.50849745914605,
      -0.1563033734277
    ),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  expect_equal(as.numeric(logLik(fit)), -1306.3199849182, tolerance = 1e-6)
})

test_that("frozen DyNAM-i choice baseline (RFID M1)", {
  skip_on_cran()
  env <- new.env()
  data("RFID_Validity_Study", package = "goldfish", envir = env)
  known.before <- env$known.before
  fit <- estimate_dynami(
    rfid_choice_formula(known.before),
    sub_model = "choice",
    data = rfid_dynami_stocnet(),
    control_estimation = set_estimation_opt(engine = "default")
  )
  expect_true(fit$convergence$isConverged)
  expect_equal(
    coef(fit),
    c(
      -0.10757804964233,
      0.26294577585498,
      0.29212265440841,
      0.024977522834176,
      1.3161426232814
    ),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  expect_equal(as.numeric(logLik(fit)), -195.16225433066, tolerance = 1e-6)
})

# Task 3.4: the stocnet public surface and the legacy constructor path estimate
# the same coefficients. The bridge rebuilds the environment through the same
# constructors the DyNAM-i data path always used (constructor-identical by
# construction; see test-dynami_bridge.R), so a legacy formula written against the
# bridged environment is the constructor path; the stocnet surface only differs in
# how the formula is spelled (bare attributes, focal layer name).
test_that("DyNAM-i rate: stocnet surface == constructor path to 1e-6", {
  skip_on_cran()
  env <- new.env()
  data("RFID_Validity_Study", package = "goldfish", envir = env)
  known.before <- env$known.before
  stocnet <- rfid_dynami_stocnet()

  surface <- estimate_dynami(
    rfid_rate_formula(known.before),
    sub_model = "rate",
    data = stocnet,
    control_estimation = set_estimation_opt(engine = "default")
  )
  legacy_env <- stocnet_to_dynami_env(stocnet, parent_env = environment())
  legacy <- estimate_wrapper(
    interactions_dependent_events ~
      1 +
      intercept(interactions, joining = 1) +
      ego(actors$age, joining = 1, subType = "centered") +
      ego(actors$age, joining = -1, subType = "centered") +
      diff(actors$age, joining = -1, subType = "averaged_sum") +
      diff(actors$level, joining = -1, subType = "averaged_sum") +
      same(actors$gender, joining = -1, subType = "proportion") +
      same(actors$group, joining = -1, subType = "proportion") +
      tie(known.before, joining = -1, subType = "proportion"),
    model = "DyNAMi",
    sub_model = "rate",
    data = legacy_env,
    control_estimation = set_estimation_opt(engine = "default")
  )
  expect_equal(
    coef(surface),
    coef(legacy),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  expect_equal(
    as.numeric(logLik(surface)),
    as.numeric(logLik(legacy)),
    tolerance = 1e-6
  )
})
