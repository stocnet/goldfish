# Single-pass multi-flavor preprocessing: the dependent / right-censored
# partition per flavor, the shared-effect union, the per-flavor intercept
# scalars, and the fid-indexed return with its process_map.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# Four actors, one mutually exclusive increment layer, creation and dissolution
# alternating so every count below is hand-checkable:
#
#   t = NA  1 -> 2  (+1)  history: the tie dissolution needs
#   t = 1   3 -> 4  (+1)  creation
#   t = 2   1 -> 2  (-1)  dissolution
#   t = 3   2 -> 3  (+1)  creation
#   t = 4   3 -> 4  (-1)  dissolution
#
# The observation window is [1, 4]: the first event carries a zero interval, so
# it is a boundary for nobody.
two_flavor_fixture <- function() {
  nodes <- data.frame(
    label = c("A", "B", "C", "D"),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 3L, 1L, 2L, 3L),
    to = c(2L, 4L, 2L, 3L, 4L),
    time = c(NA, 1, 2, 3, 4),
    layer = "calls",
    weight = c(1, 1, -1, 1, -1),
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "toy4",
    focal = "calls",
    update = c(calls = "increment"),
    directed = c(calls = TRUE),
    observation = c(calls = "event")
  )
  add_flavor(
    list(info = info, nodes = nodes, ties = ties),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1)
  )
}

# The same stream with a third, unmodeled flavor: the t = 3 event is stamped
# "renewal", so it belongs to no modeled process.
three_flavor_fixture <- function() {
  x <- two_flavor_fixture()
  x$ties$flavor[x$ties$time %in% 3] <- "renewal"
  x
}

two_flavor_spec <- function(data = two_flavor_fixture(), ...) {
  make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg + outdeg),
    choice = list(creation ~ inertia, dissolution ~ inertia),
    model = "DyNAM",
    data = data,
    ...
  )
}

# fid -> the preprocessed object of one (flavor, family) row.
prep_of <- function(out, flavor, family) {
  map <- attr(out, "process_map")
  fid <- map$fid[map$flavor == flavor & map$family == family]
  out[[as.character(fid)]]
}

test_that("the driver returns fid-indexed objects with a process_map", {
  out <- goldfish:::preprocess_flavored(two_flavor_spec())
  map <- attr(out, "process_map")

  expect_s3_class(map, "data.frame")
  expect_named(
    map,
    c(
      "fid",
      "layer",
      "flavor",
      "family",
      "stat_block",
      "has_intercept",
      "constraint_id"
    )
  )
  # Two flavors x two families, and the list is keyed by fid -- not by any
  # pasted flavor/family name.
  expect_equal(nrow(map), 4L)
  expect_equal(names(out), as.character(map$fid))
  expect_equal(map$fid, seq_len(4L))
  expect_true(all(vapply(
    out,
    function(p) inherits(p, "preprocessed.goldfish"),
    logical(1)
  )))
  expect_equal(unique(map$layer), "calls")
  expect_setequal(map$family, c("rate", "choice"))
})

test_that("a flavor's rate and choice rows share one constraint id", {
  map <- attr(goldfish:::preprocess_flavored(two_flavor_spec()), "process_map")

  creation <- map$constraint_id[map$flavor == "creation"]
  dissolution <- map$constraint_id[map$flavor == "dissolution"]

  expect_equal(length(unique(creation)), 1L)
  expect_equal(length(unique(dissolution)), 1L)
  # Complementary masks, so the two flavors must NOT share a constraint.
  expect_false(identical(unique(creation), unique(dissolution)))
})

test_that("the rate family partitions events into dependent and censored", {
  out <- goldfish:::preprocess_flavored(two_flavor_spec())

  creation <- prep_of(out, "creation", "rate")
  dissolution <- prep_of(out, "dissolution", "rate")

  # Creation: dependent at t = 1 and t = 3, right-censored at the two
  # dissolutions. Dissolution: dependent at t = 2 and t = 4, right-censored at
  # t = 3 only -- the t = 1 creation opens the window with a zero interval, so
  # it bounds nobody's rate integral.
  expect_equal(creation$event_time, c(1, 2, 3, 4))
  expect_equal(creation$is_dependent, c(1L, 0L, 1L, 0L))
  expect_equal(dissolution$event_time, c(2, 3, 4))
  expect_equal(dissolution$is_dependent, c(1L, 0L, 1L))

  expect_equal(creation$n_dep_events, 2L)
  expect_equal(dissolution$n_dep_events, 2L)
})

test_that("choice sub-models carry no cross-flavor right-censoring", {
  out <- goldfish:::preprocess_flavored(two_flavor_spec())

  creation <- prep_of(out, "creation", "choice")
  dissolution <- prep_of(out, "dissolution", "choice")

  # Only each flavor's own events, all dependent: another flavor's event
  # updates process state but is not an observation of this process.
  expect_equal(creation$event_time, c(1, 3))
  expect_equal(dissolution$event_time, c(2, 4))
  expect_true(all(creation$is_dependent == 1L))
  expect_true(all(dissolution$is_dependent == 1L))
})

test_that("each object carries its own formula, not the union", {
  out <- goldfish:::preprocess_flavored(two_flavor_spec())

  # The union formula that drove the walk is `~ 1 + indeg + outdeg`, but each
  # object holds only its own projected columns, so stamping the union here
  # would describe columns the object does not have — and misalign the effect
  # matching of any later `preprocessed` re-parse.
  creation <- prep_of(out, "creation", "rate")
  dissolution <- prep_of(out, "dissolution", "rate")

  expect_equal(deparse1(creation$formula), "calls ~ 1 + indeg")
  expect_equal(deparse1(dissolution$formula), "calls ~ 1 + indeg + outdeg")

  # The invariant behind it: the stored formula's effect count matches the
  # object's statistics columns, for every process.
  map <- attr(out, "process_map")
  for (i in seq_len(nrow(map))) {
    prep <- out[[as.character(map$fid[i])]]
    n_cols <- if (identical(map$family[i], "rate")) {
      ncol(prep$initialStats)
    } else {
      dim(prep$initialStats)[3]
    }
    n_terms <- length(attr(stats::terms(prep$formula), "term.labels"))
    expect_equal(n_cols, n_terms)
  }
})

test_that("an effect shared across flavors is computed once", {
  union <- goldfish:::plan_flavor_union(two_flavor_spec(), "rate")

  # Three effect terms across the two formulas, but `indeg` is shared, so the
  # walk computes two statistics columns.
  expect_equal(union$union_labels, c("indeg", "outdeg"))
  expect_equal(union$effect_maps$creation, 1L)
  expect_equal(union$effect_maps$dissolution, c(1L, 2L))

  out <- goldfish:::preprocess_flavored(two_flavor_spec())
  # Each flavor's object carries only its OWN columns, projected out of the
  # shared computation.
  expect_equal(ncol(prep_of(out, "creation", "rate")$initialStats), 1L)
  expect_equal(ncol(prep_of(out, "dissolution", "rate")$initialStats), 2L)
})

test_that("intercept scalars are per-flavor over that flavor's risk set", {
  out <- goldfish:::preprocess_flavored(two_flavor_spec())

  creation <- prep_of(out, "creation", "rate")
  dissolution <- prep_of(out, "dissolution", "rate")

  # Both integrate over the same [1, 4] window.
  expect_equal(sum(creation$intervals), 3)
  expect_equal(sum(dissolution$intervals), 3)

  # Dissolution is supportable only where a tie exists, so its risk set is the
  # senders holding one, read lagged at each of its stored events:
  #   t = 2  ties {1->2, 3->4}          senders {1, 3}  -> 2
  #   t = 3  ties {3->4}                senders {3}     -> 1
  #   t = 4  ties {3->4, 2->3}          senders {3, 2}  -> 2
  # each in force for one time unit: (2 + 1 + 2) / 3.
  expect_equal(dissolution$avg_active_entity, 5 / 3)

  # Creation is supportable on the (many) non-ties, so its average risk set is
  # strictly larger -- the complementary-mask asymmetry.
  expect_gt(creation$avg_active_entity, dissolution$avg_active_entity)
})

test_that("an unmodeled flavor's event censors every modeled timed process", {
  spec <- make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    model = "DyNAM",
    data = three_flavor_fixture()
  )
  out <- goldfish:::preprocess_flavored(spec)

  creation <- prep_of(out, "creation", "rate")
  dissolution <- prep_of(out, "dissolution", "rate")

  # t = 3 is a renewal: no modeled process claims it, so it is a dependent
  # event for neither -- but it bounds both rate integrals, landing as a
  # right-censored row rather than a mask flip.
  expect_true(3 %in% creation$event_time)
  expect_true(3 %in% dissolution$event_time)
  expect_equal(creation$is_dependent[creation$event_time == 3], 0L)
  expect_equal(dissolution$is_dependent[dissolution$event_time == 3], 0L)

  # Only the one remaining creation and the two dissolutions are dependent.
  expect_equal(creation$n_dep_events, 1L)
  expect_equal(dissolution$n_dep_events, 2L)
  # And no renewal parameters exist: the map keys the two modeled flavors only.
  expect_setequal(
    attr(out, "process_map")$flavor,
    c("creation", "dissolution")
  )
})

test_that("an empty risk set aborts naming the offending process", {
  local_cli_context()
  # `~ tie(calls)` contradicts creation's derived `~ !tie(calls)`, so the
  # creation process has no supportable dyad anywhere.
  spec <- suppressMessages(two_flavor_spec(
    support_constraint = ~ tie(calls)
  ))

  err <- expect_error(
    suppressWarnings(goldfish:::preprocess_flavored(spec)),
    "gated out"
  )
  # The label is rendered from the process_map for the message, so the user
  # learns WHICH of the parallel processes is empty.
  expect_match(
    paste(conditionMessage(err), collapse = "\n"),
    "calls › creation › rate",
    fixed = FALSE
  )
})
