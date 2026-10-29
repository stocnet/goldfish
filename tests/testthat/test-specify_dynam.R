# specify_dynam(): a formula written for goldfish, an MRQAP (infernet), or
# ergm, read and written as goldfish's effects, and the table of every effect
# for a network. These tests hold what each model's terms become, which are
# dropped and why, and that a specification of the table's rows agrees with
# make_specification().

quietly <- function(expr) suppressWarnings(suppressMessages(expr))

se_fixture <- function() {
  data("social_evolution", package = "goldfish", envir = environment())
  get("social_evolution", environment())
}

record_of <- function(x) {
  if (inherits(x, "goldfishSpec")) x$record else attr(x, "record")
}

status_of <- function(x, term) {
  r <- record_of(x)
  r$status[r$term == term]
}


as_of <- function(x, term) {
  r <- record_of(x)
  r$as[r$term == term]
}

test_that("a goldfish formula gives make_specification()'s spec", {
  x <- se_fixture()
  spec <- specify_dynam(calls ~ inertia + recip + trans, x)
  ref <- make_specification(
    choice = ~ inertia + recip + trans,
    model = "DyNAM",
    choice_sub_model = "choice",
    layer = "calls",
    data = x
  )
  expect_s3_class(spec, "goldfishSpec")
  expect_identical(spec$layer, ref$layer)
  expect_identical(names(spec$submodels), "choice")
  expect_equal(spec$submodels$choice$formula, ref$submodels$choice$formula,
               ignore_formula_env = TRUE)
  expect_identical(spec$input, "dynam")
  expect_identical(tidy(spec)$status, rep("fits", 3))
})

test_that("the model a formula was written for is read from its names", {
  expect_identical(
    attr(quietly(specify_dynam(calls ~ edges + mutual)), "input"),
    "ergm"
  )
  expect_identical(
    attr(quietly(specify_dynam(calls ~ exchange(friends) + ego(age))), "input"),
    "mrqap"
  )
  # goldfish and infernet share `indeg`, so it says nothing about the model.
  expect_identical(attr(specify_dynam(calls ~ indeg + recip), "input"), "dynam")
  expect_identical(
    attr(quietly(specify_dynam(calls ~ indeg, input = "ergm")), "input"),
    "ergm"
  )
  ergm_class <- structure(calls ~ recip, class = c("ergm_formula", "formula"))
  expect_identical(attr(specify_dynam(ergm_class), "input"), "ergm")
  expect_error(specify_dynam(calls ~ recip, input = "siena"), "must be one of")
})

test_that("ergm terms become goldfish effects, and the rest drop", {
  f <- quietly(specify_dynam(
    calls ~ edges + mutual + nodematch("dept") + nodeocov("age") +
      absdiff("age") + diff("age") + edgecov("friends") + ttriple + ctriple +
      cycle(4) + gwesp(0.5) + nodematch("dept", diff = TRUE),
    sub_model = "rate"
  ))
  expect_identical(as_of(f, "edges"), "1")
  expect_identical(as_of(f, "mutual"), "recip")
  expect_identical(as_of(f, "nodematch(\"dept\")"), "same(dept)")
  expect_identical(as_of(f, "nodeocov(\"age\")"), "ego(age)")
  expect_identical(as_of(f, "absdiff(\"age\")"), "diff(age)")
  expect_identical(as_of(f, "diff(\"age\")"),
                   "diff(age, transformer_fn = identity)")
  expect_identical(as_of(f, "edgecov(\"friends\")"), "tie(friends)")
  expect_identical(as_of(f, "ttriple"), "trans")
  expect_identical(as_of(f, "ctriple"), "cycle")
  expect_identical(as_of(f, "cycle(4)"), "four")
  expect_identical(status_of(f, "gwesp(0.5)"), "drops")
  expect_identical(status_of(f, "nodematch(\"dept\", diff = TRUE)"), "drops")
  expect_match(deparse(f[[3]], width.cutoff = 500L), "^1 \\+ recip")
  # In a choice model, ergm's intercept has nothing to become.
  g <- quietly(specify_dynam(calls ~ edges + mutual))
  expect_identical(status_of(g, "edges"), "drops")
  expect_match(record_of(g)$reason[1], "choice model")
})

test_that("an MRQAP formula is written for goldfish", {
  f <- quietly(specify_dynam(
    calls ~ exchange(friends) + dist(age) + outdeg(friends) +
      indeg(friends, node = "ego") + mix(dept) + same(dept, diff = TRUE) +
      ego(dept, levels = 1) + sim(age) + tertius(friends, age) +
      reporter(age) + friends + (1 | sender)
  ))
  expect_identical(as_of(f, "exchange(friends)"), "recip(friends)")
  expect_identical(as_of(f, "dist(age)"), "diff(age)")
  # infernet's outdeg() reads the sender, goldfish's the receiver by default.
  expect_identical(
    as_of(f, "outdeg(friends)"),
    "outdeg(friends, type = \"ego\")"
  )
  expect_identical(as_of(f, "indeg(friends, node = \"ego\")"),
                   "indeg(friends, type = \"ego\")")
  expect_identical(as_of(f, "friends"), "tie(friends, weighted = TRUE)")
  dropped <- c("mix(dept)", "same(dept, diff = TRUE)",
               "ego(dept, levels = 1)", "reporter(age)", "(1 | sender)")
  for (term in dropped) {
    expect_identical(status_of(f, term), "drops", info = term)
  }
  expect_identical(status_of(f, "sim(age)"), "interpreted")
  expect_identical(status_of(f, "tertius(friends, age)"), "interpreted")
  expect_warning(
    suppressMessages(specify_dynam(calls ~ exchange(friends) + mix(dept))),
    "Dropped 1 term"
  )
  expect_message(
    suppressWarnings(specify_dynam(calls ~ exchange(friends) + mix(dept))),
    "Writing `exchange\\(friends\\)` as `recip\\(friends\\)`"
  )
})

test_that("an interaction with a term goldfish cannot fit is dropped whole", {
  f <- quietly(specify_dynam(
    calls ~ nodematch("dept"):mutual + gwesp(0.5):mutual,
    input = "ergm"
  ))
  expect_identical(as_of(f, "nodematch(\"dept\"):mutual"), "same(dept):recip")
  expect_identical(status_of(f, "mutual:gwesp(0.5)"), "drops")
})

test_that("a main effect the sub-model cannot identify is dropped", {
  x <- se_fixture()
  spec <- quietly(specify_dynam(calls ~ inertia + ego(floor) + outdeg, x))
  expect_identical(status_of(spec, "ego(floor)"), "drops")
  expect_match(record_of(spec)$reason[record_of(spec)$term == "ego(floor)"],
               "ego-perspective")
  expect_identical(attr(stats::terms(spec$submodels$choice$input_formula),
                        "term.labels"), c("inertia", "outdeg"))
  # A rate model is sender-indexed, so it drops alter() instead.
  rate <- quietly(specify_dynam(calls ~ 1 + indeg + alter(floor), x,
                                sub_model = "rate"))
  expect_identical(status_of(rate, "alter(floor)"), "drops")
  expect_true(rate$submodels$rate$has_intercept)
  expect_error(quietly(specify_dynam(calls ~ ego(floor), x)),
               "No term of the formula")
})

test_that("a list of formulas gives a rate and a choice model", {
  x <- se_fixture()
  spec <- specify_dynam(
    list(rate = calls ~ 1 + indeg + outdeg, choice = calls ~ inertia + recip),
    x
  )
  expect_identical(names(spec$submodels), c("rate", "choice"))
  expect_identical(tidy(spec)$part, c("rate", "rate", "choice", "choice"))
  expect_error(specify_dynam(list(other = calls ~ inertia), x), "sub-model")
  expect_error(
    specify_dynam(list(rate = calls ~ 1, rate_ordered = calls ~ indeg), x),
    "more than one"
  )
})

test_that("a REM reads its one formula as a rate formula", {
  x <- se_fixture()
  spec <- specify_dynam(calls ~ 1 + inertia + recip, x, model = "REM")
  expect_identical(spec$model, "REM")
  expect_identical(names(spec$submodels), "rate")
  expect_error(specify_dynam(calls ~ inertia, x, model = "REM",
                             sub_model = "choice"), "no \"choice\"")
})

test_that("an ergm left-hand side naming the network reads the focal", {
  x <- se_fixture()
  spec <- quietly(specify_dynam(net ~ mutual, x))
  expect_identical(spec$layer, "calls")
})

test_that("the table lists every effect that fits the network", {
  x <- se_fixture()
  tab <- specify_dynam(calls ~ ., x)
  expect_s3_class(tab, c("goldfishTerms", "tbl_df", "tbl", "data.frame"),
                  exact = TRUE)
  expect_named(tab, c("term", "effect", "layer", "attribute"))
  expect_true(all(c("inertia", "recip", "trans", "recip(friendship)",
                    "same(gradeType)", "alter(floor)", "diff(floor)",
                    "tertius(friendship, floor)",
                    "node_trans(friendship, type = 'ego')") %in% tab$term))
  # A label and `active` describe no node.
  expect_false(any(c("label", "active") %in% tab$attribute))
  # A choice model cannot identify an ego-perspective main effect, so it has
  # no row.
  expect_false(any(c("ego(floor)", "indeg(friendship, type = 'ego')") %in%
                     tab$term))
  expect_identical(tab$layer[tab$term == "inertia"], "calls")
  # A rate model has its own effects, and the same default without a formula.
  rate <- specify_dynam(.data = x, sub_model = "rate")
  expect_true(all(c("indeg", "outdeg", "ego(floor)", "triangle") %in%
                    rate$term))
  expect_false("recip" %in% rate$term)
  expect_error(specify_dynam(calls ~ .), "needs `.data`")
  expect_error(specify_dynam(), "must be supplied")
})

test_that("formula() of chosen rows gives their specification", {
  x <- se_fixture()
  tab <- specify_dynam(calls ~ ., x)
  chosen <- tab[tab$term %in% c("inertia", "recip", "same(gradeType)"), ]
  expect_s3_class(chosen, "goldfishTerms")
  spec <- formula(chosen)
  expect_s3_class(spec, "goldfishSpec")
  expect_identical(tidy(spec)$term, c("inertia", "recip", "same(gradeType)"))
  expect_identical(tidy(spec)$status, rep("fits", 3))
  rate <- specify_dynam(.data = x, sub_model = "rate")
  spec <- formula(rate[rate$term %in% c("indeg", "outdeg"), ])
  expect_true(spec$submodels$rate$has_intercept)
  expect_error(formula(tab[0, ]), "no rows")
  # Of diff() and sim() of one attribute, the first row is kept.
  both <- tab[tab$term %in% c("inertia", "diff(floor)", "sim(floor)"), ]
  expect_message(spec <- formula(both), "Leaving out `sim\\(floor\\)`")
  expect_setequal(tidy(spec)$term, c("inertia", "diff(floor)"))
  expect_identical(tidy(quietly(formula(tab[tab$term == "sim(floor)", ])))$term,
                   "sim(floor)")
})

test_that("a goldfish formula prints only the terms it changes", {
  f <- quietly(specify_dynam(calls ~ inertia + nodematch("x")))
  out <- capture.output(print(f))
  expect_identical(out[1], "calls ~ inertia + same(x)")
  expect_true(any(grepl("adapted nodematch", out, fixed = TRUE)))
  expect_false(any(grepl("fits|attr\\(", out)))
  out <- capture.output(print(specify_dynam(calls ~ inertia + recip)))
  expect_identical(out, "calls ~ inertia + recip")
})

test_that("the table prints its effects, and tidy() gives a plain tibble", {
  x <- se_fixture()
  tab <- specify_dynam(calls ~ ., x)
  expect_message(expect_output(print(tab), "type = 'ego'", fixed = TRUE),
                 "[0-9]+ effects of a DyNAM choice model")
  tidied <- tidy(tab)
  expect_s3_class(tidied, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
  expect_null(attr(tidied, "data"))
  expect_identical(nrow(tidied), nrow(tab))
})

test_that("tidy() of make_specification()'s spec lists its terms", {
  x <- se_fixture()
  spec <- make_specification(
    choice = ~ inertia + recip,
    model = "DyNAM",
    choice_sub_model = "choice",
    layer = "calls",
    data = x
  )
  out <- tidy(spec)
  expect_identical(out$term, c("inertia", "recip"))
  expect_identical(out$status, c("fits", "fits"))
})

test_that("a spec of specify_dynam() estimates as make_specification()'s", {
  skip_on_cran()
  x <- se_fixture()
  spec <- specify_dynam(calls ~ inertia + recip + trans, x)
  ref <- make_specification(
    choice = ~ inertia + recip + trans,
    model = "DyNAM",
    choice_sub_model = "choice",
    layer = "calls",
    data = x
  )
  fit <- quietly(estimate_dynam(spec))
  expect_equal(coef(fit), coef(quietly(estimate_dynam(ref))))
})
