local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# Per-flavor estimation: the container equals standalone per-flavor fits, the
# redundant (no-constraint) branch behaves as its own case, and the container's
# methods present one component per process.
#
# The container fixtures live in `helper-flavored-fixtures.R`.

test_that("a container fit equals standalone per-flavor fits", {
  data <- flavored_fixture_data()
  container <- suppressWarnings(estimate_dynam(make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    data = data
  )))

  # Each standalone specification derives the same mask the container's process
  # carries, so the two fits see identical risk sets.
  standalone <- list(
    list(
      "creation",
      "rate",
      make_specification(
        rate = list(creation ~ 1 + indeg),
        model = "DyNAM",
        data = data
      ),
      "rate"
    ),
    list(
      "dissolution",
      "rate",
      make_specification(
        rate = list(dissolution ~ 1 + indeg),
        model = "DyNAM",
        data = data
      ),
      "rate"
    ),
    list(
      "creation",
      "choice",
      make_specification(
        choice = list(creation ~ trans),
        model = "DyNAM",
        data = data
      ),
      "choice"
    ),
    list(
      "dissolution",
      "choice",
      make_specification(
        choice = list(dissolution ~ trans),
        model = "DyNAM",
        data = data
      ),
      "choice"
    )
  )

  for (case in standalone) {
    alone <- suppressWarnings(estimate_dynam(case[[3]], sub_model = case[[4]]))
    joint <- fit_of(container, case[[1]], case[[2]])
    expect_equal(coef(joint), coef(alone), tolerance = 1e-6)
    expect_equal(
      as.numeric(logLik(joint)),
      as.numeric(logLik(alone)),
      tolerance = 1e-6
    )
  }
})

test_that("a redundant layer estimates with no constraint anywhere", {
  data <- flavored_fixture_data(style = "redundant")
  spec <- make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(creation ~ inertia, dissolution ~ inertia),
    model = "DyNAM",
    data = data
  )

  # Repeated same-value events are meaningful here, so nothing is derived --
  # pinned directly rather than inferred from the fits happening to agree.
  expect_null(spec$processes$creation$derived_constraint)
  expect_null(spec$processes$dissolution$derived_constraint)

  preps <- goldfish:::preprocess_flavored(spec)
  expect_true(all(is.na(attr(preps, "process_map")$constraint_id)))
  expect_true(all(vapply(
    preps,
    function(p) is.null(p$support_mask),
    logical(1)
  )))

  # `inertia` is degenerate under a mutually exclusive mask but perfectly
  # ordinary here, which is exactly why the redundant branch needs its own
  # coverage rather than riding on the constrained one.
  container <- suppressWarnings(estimate_dynam(spec))
  alone <- suppressWarnings(estimate_dynam(
    make_specification(
      choice = list(creation ~ inertia),
      model = "DyNAM",
      data = data
    ),
    sub_model = "choice"
  ))
  expect_equal(
    coef(fit_of(container, "creation", "choice")),
    coef(alone),
    tolerance = 1e-6
  )
})

test_that("the container's methods return one component per process", {
  data <- flavored_fixture_data()
  res <- suppressWarnings(estimate_dynam(make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    data = data
  )))

  # Labels are rendered from the process_map, and every view of the container
  # presents them flavor-major so print and the extractors cannot disagree.
  expected <- c(
    "calls › creation › rate",
    "calls › creation › choice",
    "calls › dissolution › rate",
    "calls › dissolution › choice"
  )
  expect_named(coef(res), expected)
  expect_named(vcov(res), expected)
  expect_equal(dim(vcov(res)[[1]]), c(2L, 2L))

  # The processes factorize, so the joint log-likelihood is the sum and the
  # degrees of freedom add.
  parts <- lapply(res$results, logLik)
  expect_equal(
    as.numeric(logLik(res)),
    sum(vapply(parts, as.numeric, numeric(1)))
  )
  expect_equal(
    attr(logLik(res), "df"),
    sum(vapply(parts, function(p) attr(p, "df"), numeric(1)))
  )
  expect_false(is.na(AIC(res)))
})

test_that("the container prints a section per flavor", {
  data <- flavored_fixture_data()
  res <- suppressWarnings(estimate_dynam(make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    data = data
  )))

  testthat::local_reproducible_output(
    width = 80,
    crayon = FALSE,
    unicode = TRUE
  )
  # The estimates are pinned by the equivalence test above; what this snapshot
  # owns is the layout, so numbers are redacted rather than left to drift with
  # the platform's arithmetic.
  expect_snapshot(
    print(res),
    transform = function(lines) {
      gsub("-?[0-9]+\\.[0-9]+(e[-+][0-9]+)?", "<num>", lines)
    }
  )
})

test_that("flavored gather is fid-keyed and carries the process_map", {
  data <- flavored_fixture_data()
  spec <- make_specification(
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    data = data
  )
  gathered <- suppressWarnings(compute_statistics(
    spec,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  ))

  map <- attr(gathered, "process_map")
  # One statistics class for every shape: a container differs from a single
  # process in its `scope`, not in a class string of its own.
  expect_s3_class(gathered, "goldfishStat")
  expect_identical(attr(gathered, "scope"), "flavored")
  expect_identical(attr(gathered, "storage"), "stack")
  expect_named(gathered, as.character(map$fid))
  expect_setequal(map$flavor, c("creation", "dissolution"))
  # The keying is the estimation container's keying, not a parallel convention.
  container <- suppressWarnings(estimate_dynam(spec))
  expect_identical(map$fid, container$process_map$fid)
  expect_identical(map$flavor, container$process_map$flavor)
})

test_that("a flavored and a single result differ only in their scope", {
  # The whole point of the collapse: a container of processes and one process
  # are the same kind of object, and what separates them is a field.
  data <- flavored_fixture_data()
  spec <- make_specification(
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    data = data
  )
  flavored <- suppressWarnings(compute_statistics(
    spec,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  ))
  # One flavor is one process, so this returns the bare stack rather than a
  # container -- which is exactly the pair the scope field has to separate.
  single <- suppressWarnings(compute_statistics(
    make_specification(
      choice = list(creation ~ trans),
      model = "DyNAM",
      data = data
    ),
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  ))
  expect_identical(class(flavored), class(single))
  expect_identical(stat_storage(flavored), stat_storage(single))
  expect_identical(stat_scope(flavored), "flavored")
  expect_identical(stat_scope(single), "single")
})

test_that("print reads the fields rather than a class per combination", {
  local_cli_context()
  data <- flavored_fixture_data()
  spec <- make_specification(
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    data = data
  )
  gathered <- suppressWarnings(compute_statistics(
    spec,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  ))
  expect_snapshot(print(gathered))
})

test_that("a per-fid gather stack equals its single-flavor run", {
  data <- flavored_fixture_data()
  flavored <- suppressWarnings(compute_statistics(
    make_specification(
      choice = list(creation ~ trans, dissolution ~ trans),
      model = "DyNAM",
      data = data
    ),
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  ))
  map <- attr(flavored, "process_map")

  singles <- list(
    creation = creation ~ trans,
    dissolution = dissolution ~ trans
  )
  for (flavor in names(singles)) {
    single <- suppressWarnings(compute_statistics(
      make_specification(
        choice = list(singles[[flavor]]),
        model = "DyNAM",
        data = data
      ),
      model = "DyNAM",
      sub_model = "choice",
      output = "gather"
    ))
    fid <- map$fid[map$flavor == flavor]
    expect_equal(flavored[[as.character(fid)]], single, info = flavor)
  }
})

test_that("flavored db output writes a table per process, plus map and nodes", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  data <- flavored_fixture_data()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  spec <- make_specification(
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    data = data
  )
  descriptors <- suppressWarnings(compute_statistics(
    spec,
    model = "DyNAM",
    sub_model = "choice",
    output = "db",
    control_prep = set_preprocessing(db = con, db_table = "stats")
  ))

  expect_setequal(
    DBI::dbListTables(con),
    c("stats_1", "stats_2", "stats_map", "stats_nodes")
  )
  map <- DBI::dbReadTable(con, "stats_map")
  expect_identical(map$table_name, c("stats_1", "stats_2"))
  # The map's keying is the estimation container's keying, read back from SQL.
  container <- suppressWarnings(estimate_dynam(spec))
  expect_identical(map$fid, container$process_map$fid)
  expect_identical(map$flavor, container$process_map$flavor)

  # Each process's rows are its own gather stack, named by its own effects.
  gathered <- suppressWarnings(compute_statistics(
    spec,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  ))
  for (fid in map$fid) {
    tbl <- DBI::dbReadTable(con, paste0("stats_", fid))
    stack <- gathered[[as.character(fid)]]
    written <- as.matrix(tbl[, stack$names_effects, drop = FALSE])
    dimnames(written) <- NULL
    expect_equal(written, unname(stack$stat_all_events))
    expect_equal(
      as.integer(table(tbl$event_id)),
      as.integer(stack$n_candidates)
    )
  }

  # The R return keeps the fid-keyed shape; each descriptor names its tables.
  expect_named(descriptors, as.character(map$fid))
  expect_identical(
    unname(descriptors[["1"]]$db_tables["stats"]),
    "stats_1"
  )

  # Re-exporting with fewer processes leaves the earlier run's extra table
  # behind: dropping by prefix would be a destructive guess against tables the
  # connection may own for other reasons. The map is the authority instead.
  suppressWarnings(compute_statistics(
    make_specification(
      choice = list(creation ~ trans),
      model = "DyNAM",
      data = data
    ),
    model = "DyNAM",
    sub_model = "choice",
    output = "db",
    control_prep = set_preprocessing(db = con, db_table = "stats")
  ))
  expect_true("stats_2" %in% DBI::dbListTables(con))
  expect_identical(nrow(DBI::dbReadTable(con, "stats_map")), 1L)
})

test_that("a flavored specification returns one frame per process", {
  data <- flavored_fixture_data()
  spec <- make_specification(
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    data = data
  )
  frames <- suppressWarnings(compute_statistics(
    spec,
    model = "DyNAM",
    sub_model = "choice",
    output = "data.frame"
  ))
  gathered <- suppressWarnings(compute_statistics(
    spec,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  ))

  # Same fid keying and the same process_map as every other output form: the
  # frame is a reshape of that fid's stack, not a separate walk.
  map <- attr(frames, "process_map")
  expect_named(frames, as.character(map$fid))
  expect_identical(map, attr(gathered, "process_map"))
  for (fid in as.character(map$fid)) {
    stack <- gathered[[fid]]
    expect_s3_class(frames[[fid]], "data.frame")
    expect_equal(
      unname(as.matrix(frames[[fid]][, stack$names_effects])),
      unname(stack$stat_all_events)
    )
    expect_identical(sum(frames[[fid]]$chosen), length(stack$n_candidates))
  }
})

# The value channel for a multi-process specification. The four processes here
# (two flavors x rate/choice) all carry `indeg`, so they share a coefficient
# label across the log-rate and the log-odds scale -- the case a shared
# positional or name-matched control value cannot tell apart.

flavored_value_spec <- function(data, rate_rhs, choice_rhs) {
  make_specification(
    rate = list(
      stats::as.formula(paste("creation ~", rate_rhs)),
      stats::as.formula(paste("dissolution ~", rate_rhs))
    ),
    choice = list(
      stats::as.formula(paste("creation ~", choice_rhs)),
      stats::as.formula(paste("dissolution ~", choice_rhs))
    ),
    model = "DyNAM",
    data = data
  )
}

test_that("per-process offset values ride the formulas", {
  data <- flavored_fixture_data()
  # every formula keeps an estimated term: a formula whose only terms are
  # offsets does not parse (`terms()` leaves no factors matrix to map), which
  # is a limitation of the parser rather than of the value channel.
  container <- suppressWarnings(estimate_dynam(make_specification(
    rate = list(
      creation ~ 1 + offset(indeg, coef = 0.4) + outdeg,
      dissolution ~ 1 + offset(indeg, coef = -0.4) + outdeg
    ),
    choice = list(
      creation ~ offset(indeg, coef = 0.2) + trans,
      dissolution ~ offset(indeg, coef = -0.2) + trans
    ),
    model = "DyNAM",
    data = data
  )))

  # each process holds the shared label at the value its own formula carries
  expect_equal(
    coef(fit_of(container, "creation", "rate"), complete = TRUE)[["ideg"]],
    0.4
  )
  expect_equal(
    coef(fit_of(container, "dissolution", "rate"), complete = TRUE)[["ideg"]],
    -0.4
  )
  expect_equal(
    coef(fit_of(container, "creation", "choice"), complete = TRUE)[["ideg"]],
    0.2
  )
  expect_equal(
    coef(fit_of(container, "dissolution", "choice"), complete = TRUE)[["ideg"]],
    -0.2
  )
  expect_true(GetFixed(fit_of(container, "creation", "rate"))[[2]])
})

test_that("only one process needs to carry an offset", {
  data <- flavored_fixture_data()
  container <- suppressWarnings(estimate_dynam(make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(
      creation ~ offset(indeg, coef = 0.2) + trans,
      dissolution ~ indeg + trans
    ),
    model = "DyNAM",
    data = data
  )))
  expect_equal(
    coef(fit_of(container, "creation", "choice"), complete = TRUE)[["ideg"]],
    0.2
  )
  # the offset-less processes fix nothing
  expect_false(any(GetFixed(fit_of(container, "dissolution", "choice"))))
  expect_false(any(GetFixed(fit_of(container, "creation", "rate"))))
})

test_that("control-object coefficient values abort on a multi-process spec", {
  data <- flavored_fixture_data()
  spec <- flavored_value_spec(data, "1 + indeg", "indeg")
  expect_snapshot(
    error = TRUE,
    estimate_dynam(
      spec,
      control_algo = set_algorithm_newton(offset_coef = 2)
    )
  )
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(
    estimate_dynam(
      spec,
      control_algo = set_algorithm_newton(fixed_parameters = c(NA, 2))
    ),
    "does not apply to a multi-process"
  )
})

test_that("a flat named vector broadcasts to every matching process", {
  data <- flavored_fixture_data()
  spec <- flavored_value_spec(data, "1 + indeg", "indeg + trans")
  seeded <- suppressWarnings(estimate_dynam(
    spec,
    control_algo = set_algorithm_newton(initial_parameters = c(ideg = 0.5))
  ))
  plain <- suppressWarnings(estimate_dynam(spec))
  # a starting value moves the path, never the optimum
  for (case in list(
    c("creation", "rate"),
    c("dissolution", "rate"),
    c("creation", "choice"),
    c("dissolution", "choice")
  )) {
    expect_equal(
      coef(fit_of(seeded, case[[1]], case[[2]])),
      coef(fit_of(plain, case[[1]], case[[2]])),
      tolerance = 1e-5
    )
  }
})

test_that("a name matching no process aborts", {
  data <- flavored_fixture_data()
  spec <- flavored_value_spec(data, "1 + indeg", "indeg")
  expect_error(
    suppressWarnings(estimate_dynam(
      spec,
      control_algo = set_algorithm_newton(initial_parameters = c(idge = 0.5))
    )),
    "names a coefficient no process has"
  )
})

test_that("a nested list targets one process", {
  data <- flavored_fixture_data()
  spec <- flavored_value_spec(data, "1 + indeg", "indeg")
  targeted <- suppressWarnings(estimate_dynam(
    spec,
    control_algo = set_algorithm_newton(
      initial_parameters = list(creation = list(rate = c(ideg = 0.5)))
    )
  ))
  plain <- suppressWarnings(estimate_dynam(spec))
  expect_equal(
    coef(fit_of(targeted, "creation", "rate")),
    coef(fit_of(plain, "creation", "rate")),
    tolerance = 1e-5
  )
  expect_equal(
    coef(fit_of(targeted, "dissolution", "choice")),
    coef(fit_of(plain, "dissolution", "choice")),
    tolerance = 1e-8
  )
})

test_that("unknown flavor and family keys abort naming the valid ones", {
  data <- flavored_fixture_data()
  spec <- flavored_value_spec(data, "1 + indeg", "indeg")
  expect_snapshot(
    error = TRUE,
    estimate_dynam(
      spec,
      control_algo = set_algorithm_newton(
        initial_parameters = list(creaton = c(ideg = 0.5))
      )
    )
  )
  expect_error(
    suppressWarnings(estimate_dynam(
      spec,
      control_algo = set_algorithm_newton(
        initial_parameters = list(creation = list(rat = c(ideg = 0.5)))
      )
    )),
    "does not model"
  )
})

test_that("an unnamed positional vector aborts on a multi-process spec", {
  data <- flavored_fixture_data()
  spec <- flavored_value_spec(data, "1 + indeg", "indeg")
  expect_error(
    estimate_dynam(
      spec,
      control_algo = set_algorithm_newton(initial_parameters = c(-3, 0.5))
    ),
    "does not apply to a multi-process"
  )
})
