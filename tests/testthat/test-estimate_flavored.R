# Per-flavor estimation: the container equals standalone per-flavor fits, the
# redundant (no-constraint) branch behaves as its own case, and the container's
# methods present one component per process.

# A state-consistent alternating event stream: creations only ever target an
# absent tie and dissolutions only a present one, so a mutually exclusive mask
# never excludes an observed dyad. Sized so the fits are well conditioned --
# small fixtures separate, which would make the estimates meaningless to
# compare.
flavored_event_stream <- function(n_actors = 12L, n_events = 80L, seed = 3L) {
  withr::local_seed(seed)
  state <- matrix(0L, n_actors, n_actors)
  hist_from <- seq_len(n_actors)
  hist_to <- (hist_from %% n_actors) + 1L
  state[cbind(hist_from, hist_to)] <- 1L

  from <- integer(0)
  to <- integer(0)
  weight <- numeric(0)
  for (e in seq_len(n_events)) {
    creating <- (e %% 2L) == 1L
    cand <- which(if (creating) state == 0L else state == 1L, arr.ind = TRUE)
    cand <- cand[cand[, 1] != cand[, 2], , drop = FALSE]
    if (nrow(cand) == 0L) {
      next
    }
    pick <- cand[sample.int(nrow(cand), 1L), ]
    from <- c(from, as.integer(pick[[1]]))
    to <- c(to, as.integer(pick[[2]]))
    weight <- c(weight, if (creating) 1 else -1)
    state[pick[[1]], pick[[2]]] <- if (creating) 1L else 0L
  }

  data.frame(
    from = c(hist_from, from),
    to = c(hist_to, to),
    time = c(rep(NA_real_, n_actors), seq_along(from)),
    layer = "calls",
    weight = c(rep(1, n_actors), weight),
    stringsAsFactors = FALSE
  )
}

flavored_fixture_data <- function(style = "mutually_exclusive", ...) {
  ties <- flavored_event_stream(...)
  n_actors <- max(ties$from, ties$to)
  add_flavor(
    list(
      info = list(
        name = "toy",
        focal = "calls",
        update = c(calls = "increment"),
        directed = c(calls = TRUE),
        observation = c(calls = "event")
      ),
      nodes = data.frame(
        label = paste0("N", seq_len(n_actors)),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = ties
    ),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = style
  )
}

# The container's fit for one process, found through the process_map rather than
# by position.
fit_of <- function(res, flavor, family) {
  map <- res$process_map
  fid <- map$fid[map$flavor == flavor & map$family == family]
  res$results[[as.character(fid)]]
}

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
  expect_s3_class(gathered, "flavored_statistics.goldfish")
  expect_named(gathered, as.character(map$fid))
  expect_setequal(map$flavor, c("creation", "dissolution"))
  # The keying is the estimation container's keying, not a parallel convention.
  container <- suppressWarnings(estimate_dynam(spec))
  expect_identical(map$fid, container$process_map$fid)
  expect_identical(map$flavor, container$process_map$flavor)
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

test_that("flavored db output refuses rather than merging processes", {
  data <- flavored_fixture_data()
  expect_snapshot(
    error = TRUE,
    compute_statistics(
      make_specification(
        choice = list(creation ~ trans, dissolution ~ trans),
        model = "DyNAM",
        data = data
      ),
      model = "DyNAM",
      sub_model = "choice",
      output = "db"
    )
  )
})
