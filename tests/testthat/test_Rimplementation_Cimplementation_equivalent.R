context("Equivalence c--r implemenation of estimation")

# DyNAM-choice----
test_that("DyNAM-choice Social Evolution", {
  res_1 <- estimate(callsDependent ~ inertia + recip + trans, model = "DyNAM", subModel = "choice", silent = TRUE)
  res_2 <- estimate(callsDependent ~ inertia + recip + trans,
    model = "DyNAM", subModel = "choice",
    estimationInit = list(engine = "default_c"), silent = TRUE
  )
  res_3 <- estimate(callsDependent ~ inertia + recip + trans,
    model = "DyNAM", subModel = "choice",
    estimationInit = list(engine = "gather_compute"), silent = TRUE
  )
  expect_equal(res_1$parameters, res_2$parameters)
  expect_equal(res_2$parameters, res_3$parameters)
})
# DyNAM-rate-ordered----
test_that("DyNAM-rate-ordered Social Evolution", {
  res_1 <- estimate(callsDependent ~ indeg + outdeg + node_trans, model = "DyNAM", subModel = "rate", silent = TRUE)
  res_2 <- estimate(callsDependent ~ indeg + outdeg + node_trans,
    model = "DyNAM", subModel = "rate",
    estimationInit = list(engine = "default_c"), silent = TRUE
  )
  res_3 <- estimate(callsDependent ~ indeg + outdeg + node_trans,
    model = "DyNAM", subModel = "rate",
    estimationInit = list(engine = "gather_compute"), silent = TRUE
  )
  expect_equal(res_1$parameters, res_2$parameters)
  expect_equal(res_2$parameters, res_3$parameters)
})
# DyNAM-rate----
test_that("Estimation for for DyNAM-rate with data Social Evolution", {
  res_1 <- estimate(callsDependent ~ 1 + indeg + outdeg + node_trans, model = "DyNAM", subModel = "rate", silent = TRUE)
  res_2 <- estimate(callsDependent ~ 1 + indeg + outdeg + node_trans,
    model = "DyNAM", subModel = "rate",
    estimationInit = list(engine = "default_c"), silent = TRUE
  )
  res_3 <- estimate(callsDependent ~ 1 + indeg + outdeg + node_trans,
    model = "DyNAM", subModel = "rate",
    estimationInit = list(engine = "gather_compute"), silent = TRUE
  )
  expect_equal(res_1$parameters, res_2$parameters)
  expect_equal(res_2$parameters, res_3$parameters)
})



# # # The following tests take too much time, so we don't take use them by default. Ones can uncomment them to use them.
# # REM-ordered----
# test_that("REM-ordered Social Evolution", {
#   res <- estimate(callsDependent ~ inertia + recip + trans, model = "REM", subModel = "choice", silent = TRUE)
#   res_default_c <- estimate(callsDependent ~ inertia + recip + trans, model = "REM", subModel = "choice",
#    estimationInit = list(engine = "default_c"), silent = TRUE)
#   res_compute_gather <- estimate(callsDependent ~ inertia + recip + trans, model = "REM", subModel = "choice",
#    estimationInit = list(engine = "gather_compute"), silent = TRUE)
#   expect_true(sum((res$parameters - res_compute_gather$parameters)**2) < 1e-10)
#   expect_true(sum((res_default_c$parameters - res_compute_gather$parameters)**2) < 1e-10)
# })

# # REM----
# test_that("Estimation for for REM with data from Social Evolution", {
#   res_default_c <- estimate(callsDependent ~ 1 + inertia + recip + trans, model = "REM",
#   subModel = "choice", silent = TRUE)
#   res_default_c <- estimate(callsDependent ~ 1 + inertia + recip + trans, model = "REM", subModel = "choice",
#    estimationInit = list(engine = "default_c"), silent = TRUE)
#   res_compute_gather <- estimate(callsDependent ~ 1 + inertia + recip + trans, model = "REM", subModel = "choice",
#    estimationInit = list(engine = "gather_compute"), silent = TRUE)
#   expect_true(sum((res$parameters - res_compute_gather$parameters)**2) < 1e-10)
#   expect_true(sum((res_default_c$parameters - res_compute_gather$parameters)**2) < 1e-10)
# })

# # DyNAM-MM----
# test_that("Estimation for for DyNAM-MM with data from Social Evolution", {
#   res_default_c <- estimate(callsDependent ~ inertia +  trans, model = "DyNAM", subModel = "choice_coordination",
#   silent = TRUE)
#   res_default_c <- estimate(callsDependent ~ inertia +  trans, model = "DyNAM", subModel = "choice_coordination",
#    estimationInit = list(engine = "default_c"), silent = TRUE)
#   res_compute_gather <- estimate(callsDependent ~ inertia + trans, model = "DyNAM", subModel = "choice_coordination",
#    estimationInit = list(engine = "gather_compute"), silent = TRUE)
#   expect_true(sum((res$parameters - res_compute_gather$parameters)**2) < 1e-10)
#   expect_true(sum((res_default_c$parameters - res_compute_gather$parameters)**2) < 1e-10)
# })

# ignoreRep----
test_that("ignoreRep engine", {
  expect_warning(estimate(callsDependent ~ trans(callNetwork, ignoreRep = TRUE),
                          estimationInit = list(engine = "default_c"), silent = TRUE))
})
