# The flavored diagnostic surface: a container's tidy tables row-bind the
# per-process ones and carry the process identity as columns.
#
# The tidy and non-tidy surfaces take different routes on purpose -- a column
# can hold the identity, a vector cannot -- so the non-tidy methods return a
# list named by process label instead. This file covers the tidy side.

test_that("augment row-binds the processes and appends the identity", {
  skip_on_cran()
  container <- flavored_container_fit()

  tidy <- augment(container)
  parts <- lapply(container$results, augment)

  expect_equal(nrow(tidy), sum(vapply(parts, nrow, integer(1))))
  # Appended, not interleaved: the event columns stay positionally where a
  # single-process fit puts them, which is what lets one reading of the table
  # serve both shapes.
  expect_equal(tail(names(tidy), 2L), c("flavor", "family"))
  expect_equal(
    head(names(tidy), -2L),
    names(parts[[1]])
  )
})

test_that("the identity columns say which process each row came from", {
  skip_on_cran()
  container <- flavored_container_fit()

  tidy <- augment(container)
  map <- container$process_map

  # Flavor-major, the order every other view of the container presents -- which
  # is NOT the `process_map`'s own order, that one being family-major. The
  # distinction is the whole point of routing every flavored method through the
  # shared walk instead of iterating the map.
  presented <- map[goldfish:::flavored_row_order(container), ]
  expect_equal(
    unique(tidy[, c("flavor", "family")]),
    tibble::as_tibble(presented[, c("flavor", "family")])
  )
  # The same order `coef()` labels its components in, which is the assertion
  # that would catch the two surfaces drifting apart again.
  expect_equal(
    names(coef(container)),
    sprintf("calls › %s › %s", presented$flavor, presented$family)
  )

  for (i in seq_len(nrow(map))) {
    block <- tidy[
      tidy$flavor == map$flavor[i] & tidy$family == map$family[i],
    ]
    alone <- augment(container$results[[as.character(map$fid[i])]])
    expect_equal(
      block[, setdiff(names(block), c("flavor", "family"))],
      alone,
      ignore_attr = TRUE
    )
  }
})

test_that("a rate process contributes its censored remainder too", {
  # The per-event contract lets a rate fit end on a span that closes no waiting
  # time. Row-binding must carry that row rather than trimming the blocks to a
  # common length, so the flavored table is not silently one row short of the
  # per-process ones it claims to be made of.
  skip_on_cran()
  container <- flavored_container_fit()

  tidy <- augment(container)
  rate_rows <- tidy[tidy$flavor == "creation" & tidy$family == "rate", ]
  fit <- fit_of(container, "creation", "rate")

  expect_equal(nrow(rate_rows), fit$n_events + sum(rate_rows$censored))
  expect_equal(
    sum(tidy$censored),
    sum(vapply(
      container$results,
      function(f) sum(augment(f)$censored),
      integer(1)
    ))
  )
})

test_that("the three describers dispatch on a container", {
  # Conformance work: the shipped requirement names these on any fit, and a
  # container reached `.default` and aborted as an undiagnosable class.
  skip_on_cran()
  container <- flavored_container_fit()

  outliers <- diagnose_outliers(container)
  changepoints <- diagnose_changepoints(container)
  onset <- diagnose_onset(container)

  expect_s3_class(outliers, "diagnose_outliers")
  expect_s3_class(changepoints, "diagnose_changepoints")
  expect_s3_class(onset, "diagnose_onset")

  # The tables stack; the onset does not fit one rectangle, so it stays a list
  # and every component carries the identity rather than only the first.
  for (table in list(outliers, changepoints)) {
    expect_equal(tail(names(table), 2L), c("flavor", "family"))
    expect_equal(nrow(table), nrow(augment(container)))
  }
  expect_named(onset, c("path", "accrual", "summary"))
  for (component in onset) {
    expect_true(all(c("flavor", "family") %in% names(component)))
  }
})

test_that("the context totals the processes rather than taking one", {
  skip_on_cran()
  container <- flavored_container_fit()

  context <- attr(diagnose_outliers(container), "context")
  parts <- lapply(container$results, function(fit) {
    attr(diagnose_outliers(fit), "context")
  })

  # A field that differs between processes becomes the set of its values; a
  # count totals. Carrying the first process's counts would misreport the
  # table they are attached to.
  expect_setequal(context$sub_model, c("rate", "choice"))
  expect_setequal(context$flavor, c("creation", "dissolution"))
  expect_equal(
    context$n_intervals,
    sum(vapply(parts, `[[`, numeric(1), "n_intervals"))
  )
  expect_equal(
    context$n_analyzed,
    sum(vapply(parts, `[[`, numeric(1), "n_analyzed"))
  )
})

test_that("a term absent from one process names that process", {
  # Each process carries its own formula, so a selection valid for one may be
  # absent from another. "unknown term" without saying where is not actionable
  # on a fit holding four processes.
  skip_on_cran()
  container <- flavored_container_fit()

  testthat::local_reproducible_output(
    width = 80,
    crayon = FALSE,
    unicode = TRUE
  )
  # `trans` is in the choice formulas only, so the rate processes reject it.
  expect_snapshot(error = TRUE, diagnose_outliers(container, effect = "trans"))
})

test_that("the identity columns are not defining ones", {
  # Dropping `flavor` and `family` leaves a table that is still the diagnostic
  # -- one that has merely stopped saying which process each row came from. A
  # defining column is one the object cannot be read without, and provenance is
  # not that, so the class and the metadata survive the drop.
  skip_on_cran()
  container <- flavored_container_fit()

  outliers <- diagnose_outliers(container)
  expect_false(any(c("flavor", "family") %in% attr(outliers, "defining")))

  dropped <- outliers[, setdiff(names(outliers), c("flavor", "family"))]
  expect_s3_class(dropped, "diagnose_outliers")
  expect_equal(attr(dropped, "defining"), attr(outliers, "defining"))
  expect_equal(attr(dropped, "context"), attr(outliers, "context"))

  # The contrast that gives the distinction its content: a defining column
  # demotes the object to a plain tibble, exactly as a `grouped_df` demotes
  # when its groups are gone.
  demoted <- outliers[, setdiff(names(outliers), "outlier")]
  expect_false(inherits(demoted, "diagnose_outliers"))
})

test_that("plot dispatch survives dropping the identity columns", {
  skip_on_cran()
  skip_if_not_installed("autograph")
  container <- flavored_container_fit()

  outliers <- diagnose_outliers(container)
  dropped <- outliers[, setdiff(names(outliers), c("flavor", "family"))]

  expect_s3_class(plot(outliers), "ggplot")
  expect_s3_class(plot(dropped), "ggplot")
})

test_that("each block equals the standalone result for its process", {
  # The container recomputes nothing pooled: each block is what the process's
  # own fit produces, and only the identity columns are added on top.
  skip_on_cran()
  container <- flavored_container_fit()
  processes <- goldfish:::flavored_processes(container)

  for (describer in list(diagnose_outliers, diagnose_changepoints)) {
    joint <- describer(container)
    for (process in processes) {
      block <- joint[
        joint$flavor == process$flavor & joint$family == process$family,
      ]
      expect_equal(
        block[, setdiff(names(block), c("flavor", "family"))],
        describer(process$fit),
        ignore_attr = TRUE
      )
    }
  }

  # The list-shaped one, component by component.
  joint <- diagnose_onset(container)
  for (process in processes) {
    alone <- diagnose_onset(process$fit)
    for (name in names(alone)) {
      component <- joint[[name]]
      block <- component[
        component$flavor == process$flavor & component$family == process$family,
      ]
      expect_equal(
        block[, setdiff(names(block), c("flavor", "family"))],
        alone[[name]],
        ignore_attr = TRUE
      )
    }
  }
})

test_that("two diagnostics agree on row order under a reversed declaration", {
  # The declared flavor order is the presentation order, and it need not match
  # the process map's own -- the map here is family-major and declares
  # dissolution first, so a method iterating the map would order its blocks
  # differently from one using the shared walk. Two diagnostics of one fit
  # disagreeing about which process a row belongs to is the failure this
  # guards, and it is invisible on a fixture where the two orders coincide.
  skip_on_cran()
  data <- flavored_fixture_data()
  container <- suppressWarnings(estimate_dynam(make_specification(
    rate = list(dissolution ~ 1 + indeg, creation ~ 1 + indeg),
    choice = list(dissolution ~ trans, creation ~ trans),
    model = "DyNAM",
    data = data
  )))

  expected <- data.frame(
    flavor = c("dissolution", "dissolution", "creation", "creation"),
    family = c("rate", "choice", "rate", "choice"),
    stringsAsFactors = FALSE
  )
  tables <- list(
    augment(container),
    diagnose_outliers(container),
    diagnose_changepoints(container),
    model_terms(container),
    diagnose_onset(container)$summary
  )
  for (table in tables) {
    expect_equal(
      as.data.frame(unique(table[, c("flavor", "family")])),
      expected,
      ignore_attr = TRUE
    )
  }
})
