test_that("coef function", {
  expect_type(
    coef.result.goldfish(resModObject),
    "double"
  )
  expect_true(inherits(
    coef.result.goldfish(resModObject),
    "numeric"
  ))
  expect_length(
    coef.result.goldfish(resModObject),
    2
  )
  expect_equal(
    coef.result.goldfish(resModObject),
    c(inertia = 5.3751, trans = -0.0816),
    label = "correct output"
  ) 
  expect_type(
    coef.result.goldfish(resModObject, complete = TRUE),
    "double"
  )
  expect_true(inherits(
    coef.result.goldfish(resModObject, complete = TRUE),
    "numeric"
  ))
  expect_length(
    coef.result.goldfish(resModObject, complete = TRUE),
    3
  )
  expect_equal(
    coef.result.goldfish(resModObject, complete = TRUE),
    c(inertia = 5.3751, recip = 1, trans = -0.0816),
    label = "correct output when complete = TRUE"
  ) 
})

test_that("logLik function", {
  expect_type(
    logLik.result.goldfish(resModObject),
    "double"
  )
  expect_s3_class(
    logLik.result.goldfish(resModObject),
    "logLik"
  )
  expect_length(
    logLik.result.goldfish(resModObject),
    1
  )
  expect_equal(
    logLik.result.goldfish(resModObject),
    structure(-699.4532, class = "logLik", nobs = 439L, df = 3L),
    label = "correct output"
  ) 
  expect_type(
    logLik.result.goldfish(resModObject, avgPerEvent = TRUE),
    "double"
  )
  expect_failure(expect_s3_class(
    logLik.result.goldfish(resModObject, avgPerEvent = TRUE),
    "logLik"
  ))
  expect_length(
    logLik.result.goldfish(resModObject, avgPerEvent = TRUE),
    1
  )
  expect_equal(
    logLik.result.goldfish(resModObject, avgPerEvent = TRUE),
    -699.4532/439L,
    label = "correct output when avgPerEvent = TRUE"
  )
})

test_that("vcov function", {
  expect_type(
    vcov.result.goldfish(resModObject),
    "double"
  )
  expect_true(inherits(
    vcov.result.goldfish(resModObject),
    "matrix"
  ))
  expect_length(
    vcov.result.goldfish(resModObject),
    4
  )
  expect_equal(
    vcov.result.goldfish(resModObject),
    matrix(
      c(0.0241456179209463, -0.00230482755796413,
        -0.00230482755796413, 0.0390106272519763),
      ncol = 2, nrow = 2,
      dimnames = list(c("inertia", "trans"), c("inertia", "trans"))
    ),
    label = "correct output"
  ) 
  expect_type(
    vcov.result.goldfish(resModObject, complete = TRUE),
    "double"
  )
  expect_true(inherits(
    vcov.result.goldfish(resModObject, complete = TRUE),
    "matrix"
  ))
  expect_length(
    vcov.result.goldfish(resModObject, complete = TRUE),
    9
  )
  expect_equal(
    vcov.result.goldfish(resModObject, complete = TRUE),
    matrix(
      c(0.0241456179209463, NA, -0.00230482755796413,
        NA, NA, NA,
        -0.00230482755796413, NA, 0.0390106272519763),
      ncol = 3, nrow = 3,
      dimnames = list(c("inertia", "recip", "trans"),
                      c("inertia", "recip", "trans"))
    ),
    label = "correct output when complete = TRUE"
  ) 
})