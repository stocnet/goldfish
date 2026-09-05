test_that("coef function", {
  expect_type(
    coef.goldfishFit(resModObject),
    "double"
  )
  expect_true(inherits(
    coef.goldfishFit(resModObject),
    "numeric"
  ))
  expect_length(
    coef.goldfishFit(resModObject),
    2
  )
  expect_equal(
    coef.goldfishFit(resModObject),
    c(inrt = 5.3751, trans = -0.0816),
    label = "correct output"
  )
  expect_type(
    coef.goldfishFit(resModObject, complete = TRUE),
    "double"
  )
  expect_true(inherits(
    coef.goldfishFit(resModObject, complete = TRUE),
    "numeric"
  ))
  expect_length(
    coef.goldfishFit(resModObject, complete = TRUE),
    3
  )
  expect_equal(
    coef.goldfishFit(resModObject, complete = TRUE),
    c(inrt = 5.3751, rec = 1, trans = -0.0816),
    label = "correct output when complete = TRUE"
  )
})

test_that("logLik function", {
  expect_type(
    logLik.goldfishFit(resModObject),
    "double"
  )
  expect_s3_class(
    logLik.goldfishFit(resModObject),
    "logLik"
  )
  expect_length(
    logLik.goldfishFit(resModObject),
    1
  )
  expect_equal(
    logLik.goldfishFit(resModObject),
    structure(-699.4532, class = "logLik", nobs = 439L, df = 3L),
    label = "correct output"
  )
  expect_type(
    logLik.goldfishFit(resModObject, avgPerEvent = TRUE),
    "double"
  )
  expect_failure(expect_s3_class(
    logLik.goldfishFit(resModObject, avgPerEvent = TRUE),
    "logLik"
  ))
  expect_length(
    logLik.goldfishFit(resModObject, avgPerEvent = TRUE),
    1
  )
  expect_equal(
    logLik.goldfishFit(resModObject, avgPerEvent = TRUE),
    -699.4532 / 439L,
    label = "correct output when avgPerEvent = TRUE"
  )
})

test_that("vcov function", {
  expect_type(
    vcov.goldfishFit(resModObject),
    "double"
  )
  expect_true(inherits(
    vcov.goldfishFit(resModObject),
    "matrix"
  ))
  expect_length(
    vcov.goldfishFit(resModObject),
    4
  )
  expect_equal(
    vcov.goldfishFit(resModObject),
    matrix(
      c(
        0.0241456179209463,
        -0.00230482755796413,
        -0.00230482755796413,
        0.0390106272519763
      ),
      ncol = 2,
      nrow = 2,
      dimnames = list(c("inrt", "trans"), c("inrt", "trans"))
    ),
    label = "correct output"
  )
  expect_type(
    vcov.goldfishFit(resModObject, complete = TRUE),
    "double"
  )
  expect_true(inherits(
    vcov.goldfishFit(resModObject, complete = TRUE),
    "matrix"
  ))
  expect_length(
    vcov.goldfishFit(resModObject, complete = TRUE),
    9
  )
  expect_equal(
    vcov.goldfishFit(resModObject, complete = TRUE),
    matrix(
      c(
        0.0241456179209463,
        NA,
        -0.00230482755796413,
        NA,
        NA,
        NA,
        -0.00230482755796413,
        NA,
        0.0390106272519763
      ),
      ncol = 3,
      nrow = 3,
      dimnames = list(
        c("inrt", "rec", "trans"),
        c("inrt", "rec", "trans")
      )
    ),
    label = "correct output when complete = TRUE"
  )
})

test_that("coef/vcov names are minimal-unique and match", {
  mod <- estimate_wrapper(
    depNetwork ~ inertia(networkState) + inertia(networkExog) + recip,
    data = dataTest,
    sub_model = "choice"
  )
  cf <- coef(mod)
  vc <- vcov(mod)
  expect_false(anyDuplicated(names(cf)) > 0)
  expect_identical(rownames(vc), names(cf))
  expect_identical(colnames(vc), names(cf))
  expect_true(all(make.names(names(cf)) == names(cf)))

  cfC <- coef(mod, complete = TRUE)
  vcC <- vcov(mod, complete = TRUE)
  expect_identical(rownames(vcC), names(cfC))
  expect_identical(colnames(vcC), rownames(vcC))
  expect_identical(names(cf), names(cfC))
})
