test_that("init_DyNAM_rate.global returns length-n1 stat with no cache", {
  effectFun <- structure(function(attribute) NULL, class = "global")
  result <- init_DyNAM_rate.global(effectFun, attribute = 0.5, n1 = 4, n2 = 4)
  expect_equal(result$stat, c(0.5, 0.5, 0.5, 0.5))
  expect_null(result$cache)
  expect_named(result, "stat")
})

test_that("init_DyNAM_rate.global replicates the scalar across all n1 actors", {
  effectFun <- structure(function(attribute) NULL, class = "global")
  result <- init_DyNAM_rate.global(effectFun, attribute = 2, n1 = 3, n2 = 3)
  expect_equal(result$stat, c(2, 2, 2))
})

test_that("update_DyNAM_rate_global returns 2-column changes for all n1 actors", {
  result <- update_DyNAM_rate_global(
    attribute = 0,
    replace = 1,
    n1 = 4,
    n2 = 4
  )
  expect_equal(nrow(result$changes), 4L)
  expect_equal(colnames(result$changes), c("node1", "replace"))
  expect_equal(result$changes[, "replace"], rep(1, 4))
  expect_equal(result$changes[, "node1"], 1:4)
})

test_that("update_DyNAM_rate_global returns NULL changes when value unchanged", {
  result <- update_DyNAM_rate_global(
    attribute = 1,
    replace = 1,
    n1 = 3,
    n2 = 3
  )
  expect_null(result$changes)
})

test_that("init_REM_choice.global returns n1xn2 matrix with zero diagonal", {
  effectFun <- structure(function(attribute) NULL, class = "global")
  result <- init_REM_choice.global(effectFun, attribute = 2.0, n1 = 3, n2 = 3)
  expect_equal(dim(result$stat), c(3L, 3L))
  expect_equal(diag(result$stat), c(0, 0, 0))
  expect_equal(result$stat[1, 2], 2.0)
  expect_equal(result$stat[2, 1], 2.0)
})

test_that("init_REM_choice.global fills off-diagonal with attribute value", {
  effectFun <- structure(function(attribute) NULL, class = "global")
  result <- init_REM_choice.global(effectFun, attribute = 0.5, n1 = 2, n2 = 2)
  expect_equal(result$stat[1, 2], 0.5)
  expect_equal(result$stat[2, 1], 0.5)
  expect_equal(result$stat[1, 1], 0)
})

test_that("update_REM_choice_global produces 3-column dyadic changes via to_ego", {
  result <- update_REM_choice_global(
    attribute = 0,
    replace = 1,
    n1 = 3,
    n2 = 3,
    is_two_mode = FALSE
  )
  expect_equal(colnames(result$changes), c("node1", "node2", "replace"))
  expect_equal(nrow(result$changes), 3L * (3L - 1L))
  expect_true(all(result$changes[, "replace"] == 1))
})

test_that("update_REM_choice_global returns NULL changes when value unchanged", {
  result <- update_REM_choice_global(
    attribute = 1,
    replace = 1,
    n1 = 3,
    n2 = 3
  )
  expect_null(result$changes)
})

test_that("global() aborts for choice sub-models and works for rate", {
  seasons <- make_global_attributes(data.frame(winter = 0))
  season_change <- data.frame(time = 15, replace = 1)
  seasons <- link_events(seasons, season_change)
  dataGlobal <- make_data(depNetwork, seasons)

  expect_error(
    estimate_dynam(
      depNetwork ~ inertia + global(seasons$winter),
      sub_model = "choice",
      data = dataGlobal
    ),
    "interaction"
  )
  expect_error(
    estimate_dynam(
      depNetwork ~ inertia + global(seasons$winter),
      sub_model = "choice_coordination",
      data = dataGlobal
    ),
    "interaction"
  )
  expect_error(
    compute_stats(
      depNetwork ~ inertia + global(seasons$winter),
      data = dataGlobal,
      model = "DyNAM",
      sub_model = "choice"
    ),
    "interaction"
  )
  prepRate <- compute_stats(
    depNetwork ~ global(seasons$winter),
    data = dataGlobal,
    model = "DyNAM",
    sub_model = "rate_ordered"
  )
  expect_s3_class(prepRate, "preprocessed.goldfish")
  prepRem <- compute_stats(
    depNetwork ~ global(seasons$winter),
    data = dataGlobal,
    model = "REM",
    sub_model = "rate_ordered"
  )
  expect_s3_class(prepRem, "preprocessed.goldfish")
})
