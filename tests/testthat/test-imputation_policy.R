# The as-category imputation policy: a categorical attribute missing by design
# is recoded to a reserved level instead of summarized, so the missingness
# survives to the summarizers as an ordinary category. These tests pin the
# contract's behavioral scenarios and its validation aborts.

# cli error snapshots are pinned to a reproducible width/no-color context so the
# rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# An object table naming just the party attribute, for a direct seam call.
party_effects_link <- function() {
  matrix(1, nrow = 1, dimnames = list("nodes$party", "e1"))
}

test_that("as_category recodes missing categorical values to reserved level", {
  fixture <- make_stocnet_fixture_missing_nodal()
  src <- new_data_source(data = fixture, focal = "contact")
  out <- ds_impute_missing(
    src,
    party_effects_link(),
    policy = c(party = "as_category")
  )
  # The observed values are untouched; only the two missing actors take the
  # reserved level -- none is assigned the most common observed party.
  expect_equal(
    out$att_override[["nodes$party"]],
    c("left", "right", "left", "(missing)", "(missing)")
  )
})

test_that("the default summary policy fills the most common value", {
  fixture <- make_stocnet_fixture_missing_nodal()
  src <- new_data_source(data = fixture, focal = "contact")
  out <- suppressWarnings(
    ds_impute_missing(src, party_effects_link(), policy = NULL)
  )
  # The contrast the policy exists to remove: both missing actors become "left".
  expect_equal(
    out$att_override[["nodes$party"]],
    c("left", "right", "left", "left", "left")
  )
})

test_that("deliberate missingness survives to a summarizer as a category", {
  # `same(party)` sees the imputed initial state. party = left,right,left,NA,NA.
  fixture <- make_stocnet_fixture_missing_nodal()
  run <- function(policy) {
    ctrl <- if (is.null(policy)) {
      set_preprocessing()
    } else {
      set_preprocessing(impute = policy)
    }
    suppressWarnings(estimate_dynam(
      contact ~ same(party),
      sub_model = "choice",
      data = as_goldfish(fixture),
      preprocessing_only = TRUE,
      control_preprocessing = ctrl
    ))$initialStats[,, 1]
  }
  summary_stats <- run(NULL)
  category_stats <- run(c(party = "as_category"))

  # Under as_category the two missing actors (4, 5) match each other -- both are
  # "(missing)" -- but not an observed "left" actor (1).
  expect_equal(category_stats[4, 5], 1)
  expect_equal(category_stats[1, 4], 0)
  # Under summary they were forced to "left", so they matched actor 1.
  expect_equal(summary_stats[1, 4], 1)
})

test_that("a missing event value becomes the reserved level, not pooled", {
  fixture <- make_stocnet_fixture_missing_nodal()
  fixture$nodes$party <- c("left", "right", "left", "right", "left")
  change <- data.frame(time = 2.5, node = 1L, var = "party")
  change$value <- list(list(NA_character_))
  fixture$changes <- change

  run <- function(policy) {
    ctrl <- if (is.null(policy)) {
      set_preprocessing()
    } else {
      set_preprocessing(impute = policy)
    }
    suppressWarnings(gather_model_data(
      contact ~ same(party),
      sub_model = "choice",
      data = as_goldfish(fixture),
      control_preprocessing = ctrl
    ))$stat_all_events
  }
  # The mid-walk NA is recoded to the reserved level under as_category and to
  # the most common value under summary, so the change statistics diverge.
  expect_false(identical(run(NULL), run(c(party = "as_category"))))
})

test_that("as_category on a numeric attribute aborts", {
  local_cli_context()
  fixture <- make_stocnet_fixture_missing_nodal()
  expect_snapshot(
    error = TRUE,
    suppressWarnings(estimate_dynam(
      contact ~ alter(income),
      sub_model = "choice",
      data = as_goldfish(fixture),
      preprocessing_only = TRUE,
      control_preprocessing = set_preprocessing(
        impute = c(income = "as_category")
      )
    ))
  )
})

test_that("a policy naming an unread attribute aborts", {
  local_cli_context()
  fixture <- make_stocnet_fixture_missing_nodal()
  expect_snapshot(
    error = TRUE,
    suppressWarnings(estimate_dynam(
      contact ~ same(party),
      sub_model = "choice",
      data = as_goldfish(fixture),
      preprocessing_only = TRUE,
      control_preprocessing = set_preprocessing(
        impute = c(nowhere = "as_category")
      )
    ))
  )
})

test_that("a reserved-level collision aborts", {
  local_cli_context()
  fixture <- make_stocnet_fixture_missing_nodal()
  fixture$nodes$party[1] <- "(missing)"
  src <- new_data_source(data = fixture, focal = "contact")
  expect_snapshot(
    error = TRUE,
    ds_impute_missing(
      src,
      party_effects_link(),
      policy = c(party = "as_category")
    )
  )
})

test_that("the as_category policy requires stocnet data objects", {
  local_cli_context()
  src <- new_data_source(envir = new.env())
  expect_snapshot(
    error = TRUE,
    ds_impute_missing(
      src,
      party_effects_link(),
      policy = c(party = "as_category")
    )
  )
})
