# The tidy and summary surface on a container: `summary()`, `tidy()` and
# `glance()`.
#
# These three were written once, for a single-process fit, and forgotten on the
# container while the fit classes were flat -- a flavored fit failed with R's
# "no applicable method" for all three, which nobody decided. They are the
# omission the fit-class contract exists to make impossible, so they get tests
# of their own rather than riding along in the display file.
#
# The return shapes follow the split the other container methods already use: a
# tidy return carries the process identity as appended columns, a non-tidy one
# answers with a list named by process label.

test_that("the three previously missing generics reach a container", {
  skip_on_cran()
  container <- flavored_container_fit()

  expect_s3_class(tidy(container), "tbl_df")
  expect_s3_class(glance(container), "tbl_df")
  expect_type(summary(container), "list")
})

test_that("tidy and glance append the process identity", {
  skip_on_cran()
  container <- flavored_container_fit()
  processes <- goldfish:::flavored_processes(container)

  tidied <- tidy(container)
  glanced <- glance(container)

  # Appended, never prepended, so a consumer indexing the statistic columns by
  # position does not break when a second flavor appears.
  expect_identical(utils::tail(names(tidied), 2), c("flavor", "family"))
  expect_identical(utils::tail(names(glanced), 2), c("flavor", "family"))

  expect_identical(nrow(glanced), length(processes))
  expect_identical(
    nrow(tidied),
    sum(vapply(processes, function(p) nrow(tidy(p$fit)), integer(1)))
  )
  expect_setequal(
    unique(paste(tidied$flavor, tidied$family)),
    vapply(processes, function(p) paste(p$flavor, p$family), character(1))
  )
})

test_that("each row equals the call on that process's own fit", {
  skip_on_cran()
  container <- flavored_container_fit()
  tidied <- tidy(container)
  glanced <- glance(container)

  for (process in goldfish:::flavored_processes(container)) {
    keep <- function(table) {
      table[table$flavor == process$flavor & table$family == process$family, ]
    }
    own_tidy <- tidy(process$fit)
    own_glance <- glance(process$fit)

    expect_equal(keep(tidied)$estimate, own_tidy$estimate)
    expect_equal(keep(tidied)$p.value, own_tidy$p.value)
    expect_equal(keep(glanced)$logLik, own_glance$logLik)
    expect_equal(keep(glanced)$AIC, own_glance$AIC)
  }
})

test_that("summary answers with one goldfishSummFit per process", {
  skip_on_cran()
  container <- flavored_container_fit()
  labels <- goldfish:::flavored_component_labels(container)

  summaries <- summary(container)

  expect_named(summaries, labels)
  for (one in summaries) {
    expect_s3_class(one, "goldfishSummFit")
  }
})

test_that("a flavor resolving to one process gives the single-fit shape", {
  skip_on_cran()
  container <- flavored_single_family_fit()
  flavor <- container$flavors[1]

  one <- summary(container, flavor = flavor)

  # Not a list of one: naming a process that resolves uniquely gets the
  # ordinary single-fit shape back, which is what `flavor =` is for.
  expect_s3_class(one, "goldfishSummFit")
  expect_identical(
    unclass(one)$parameters,
    unclass(summary(
      goldfish:::flavored_processes(container)[[1]]$fit
    ))$parameters
  )
})

test_that("an unknown flavor aborts naming the ones the fit carries", {
  skip_on_cran()
  container <- flavored_container_fit()

  expect_error(summary(container, flavor = "nonexistent"), "nonexistent")
})
