# Substrate parity: the two recipe loops against the merged single-clock walk.
#
# These are parity fixtures, not baselines. They never touch the frozen 1e-6
# set, and the frozen set does not cover what they cover: none of its models
# reaches the imputation path, the edgeless `four()` cache, a windowed
# constraint atom, or a repeated dyad with unweighted degree.
#
# The recipe loops are the correct side wherever the two disagree: `indeg` and
# `outdeg` default to `weighted = FALSE`, and the frozen baselines run through
# them. A test that fails here is a merged-walk defect.
#
# Fixture builders live in helper-parity-fixtures.R.

# ---- (a) five-node repeated-dyad toy, complex specification -----------------

test_that("the toy specification moves every statistic it declares", {
  # A frozen statistic would let a parity assertion pass vacuously, so the
  # fixture is only a detector once every declared effect has been seen to
  # change at least once over the sequence.
  spec <- parity_toy_spec()

  for (family in c("rate", "choice")) {
    prep <- suppressWarnings(compute_statistics(spec, "DyNAM", family))
    expect_setequal(
      parity_touched_effects(prep),
      seq_len(parity_n_effects(prep)) - 1L
    )
  }
})

test_that("a repeated dyad preprocesses identically on both substrates", {
  # FAILS until task 0.4d. On an accumulating layer the merged walk tracks
  # weighted degree where the recipe loops track unweighted, so `1 -> 2` firing
  # twice leaves the two substrates computing different statistics.
  spec <- parity_toy_spec()
  merged <- suppressWarnings(preprocess_joint(single_process_joint(spec)))

  for (family in c("rate", "choice")) {
    solo <- suppressWarnings(compute_statistics(spec, "DyNAM", family))
    expect_equal(
      parity_strip_deco(parity_prep_by(merged, family)),
      parity_strip_deco(solo)
    )
  }
})

test_that("an unweighted degree stays unweighted over a repeated dyad", {
  # The divergence read directly, without the surrounding specification: the
  # second event on `1 -> 2` must leave `indeg(N2)` at 1 and emit no update.
  data <- parity_toy_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  merged <- suppressWarnings(preprocess_joint(single_process_joint(spec)))
  solo <- suppressWarnings(compute_statistics(spec, "DyNAM", "rate"))

  expect_equal(
    parity_prep_by(merged, "rate")$stat_mat_update,
    solo$stat_mat_update
  )
})

test_that("the merged walk names window effects rather than crashing", {
  # `build_walk_engine()` has always carried the message, but it never fired: a
  # windowed term puts its derived object into the shared registry and
  # `build_state_container()` asks the source for it as if it were a real layer,
  # dying on `non-numeric matrix extent` several frames earlier. The abort now
  # runs before the shared state is built, so the lift task 1.3 plans has
  # something to remove.
  withr::local_options(cli.width = 80, cli.num_colors = 1)
  spec <- parity_toy_spec_windowed()

  expect_snapshot(
    error = TRUE,
    suppressWarnings(preprocess_joint(single_process_joint(spec)))
  )
})

test_that("the walk handle advances the shared state it injects into", {
  # The same defect on the stepping substrate, which `simulate()` will drive.
  # `walk_apply_object_event()` called the state update and discarded what it
  # returned, so an injected event never reached the shared adjacency matrix and
  # every later evaluation read the tie as absent.
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = parity_toy_data()
  )
  handle <- walk_open(single_process_joint(spec))
  event <- list(
    layer = "calls",
    sender = 1L,
    receiver = 2L,
    increment = 1,
    time = 7
  )

  walk_inject(handle, event)
  walk_inject(handle, modifyList(event, list(time = 8)))

  expect_equal(handle$state$networks[["calls"]][1, 2], 2)
})

test_that("a walk never duplicates the state matrix it writes into", {
  # The effect closures receive the adjacency matrix as an argument, which marks
  # it shared, so the state write that follows used to duplicate the whole
  # matrix on every event. The write goes through C++ now and mutates in place.
  skip_if_not(capabilities("profmem"))

  spec <- parity_toy_spec()
  state <- build_state_container(
    "calls",
    nodes = "nodes",
    src = new_data_source(data = parity_toy_data())
  )
  network <- state$networks[["calls"]]
  on.exit(untracemem(network), add = TRUE)

  traced <- capture.output(
    {
      invisible(tracemem(network))
      for (event in seq_len(4L)) {
        state <- state_set_tie(state, "calls", 1L, 2L, event, FALSE)
      }
      untracemem(network)
    },
    type = "output"
  )

  expect_equal(traced, character(0))
  expect_equal(state$networks[["calls"]][1, 2], 4)
})

test_that("an undirected write reaches both cells", {
  state <- build_state_container(
    "calls",
    nodes = "nodes",
    src = new_data_source(data = parity_toy_data())
  )

  state <- state_set_tie(state, "calls", 2L, 4L, 7, TRUE)

  expect_equal(state$networks[["calls"]][2, 4], 7)
  expect_equal(state$networks[["calls"]][4, 2], 7)
})

test_that("an unweighted tie statistic never aliases the state matrix", {
  # The third aliasing site, found by auditing every effect initializer against
  # its input rather than by reading them. With the default identity
  # transformer the weighted branch returns the state's own matrix, and
  # `unname()` hands back its argument unchanged when there are no names to
  # strip, so on a matrix without dimnames the statistic was the state.
  skip_if_not(capabilities("profmem"))

  network <- matrix(0, 5L, 5L)
  network[1, 2] <- 1
  effect_fun <- function(
    network,
    weighted = TRUE,
    transformer_fn = identity,
    is_two_mode = FALSE
  ) {
    NULL
  }
  initialized <- init_DyNAM_choice.tie(
    effect_fun = effect_fun,
    network = network,
    window = NULL,
    n1 = 5L,
    n2 = 5L
  )
  on.exit(untracemem(network), add = TRUE)

  expect_false(identical(tracemem(network), tracemem(initialized$stat)))
  expect_equal(initialized$stat[1, 2], 1)
})

# ---- (b) Social Evolution, asta Copenhagen shape ---------------------------

test_that("a realistic specification preprocesses identically on real data", {
  # FAILS until task 0.4d, for the same reason as the toy: Social Evolution
  # repeats dyads, and there the recipe loop reconstructs `colSums(A > 0)`
  # while the merged walk reconstructs `colSums(A)`.
  spec <- parity_social_evolution_spec()
  merged <- suppressWarnings(preprocess_joint(single_process_joint(spec)))

  for (family in c("rate", "choice")) {
    solo <- suppressWarnings(compute_statistics(spec, "DyNAM", family))
    expect_equal(
      parity_strip_deco(parity_prep_by(merged, family)),
      parity_strip_deco(solo)
    )
  }
})

test_that("the recipe loops reconstruct the unweighted in-degree", {
  # The ground-truth reading, on the side that is already right: replaying the
  # stored updates onto `initial_stats` must reproduce the binarized column
  # sums of the adjacency matrix, not its raw column sums.
  data <- parity_social_evolution_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "call_network",
    model = "DyNAM",
    data = data
  )
  prep <- suppressWarnings(compute_statistics(spec, "DyNAM", "rate"))

  n_events <- 200L
  stats <- prep$initial_stats[, 1]
  upto <- prep$stat_mat_pointer[n_events]
  if (upto > 0) {
    updates <- prep$stat_mat_update[, seq_len(upto), drop = FALSE]
    stats[updates[1, ] + 1L] <- updates[4, ]
  }

  calls <- data$ties[data$ties$layer == "call_network", ]
  observed <- calls[order(calls$time), ][seq_len(n_events), ]
  adjacency <- matrix(0, nrow(data$nodes), nrow(data$nodes))
  from <- match(observed$from, data$nodes$label)
  to <- match(observed$to, data$nodes$label)
  for (e in seq_len(n_events)) {
    adjacency[from[e], to[e]] <- adjacency[from[e], to[e]] + 1
  }

  expect_equal(sum(stats), sum(colSums(adjacency > 0)))
})

# ---- (c) windowed support constraint ---------------------------------------

test_that("a window inside a support constraint changes the risk set", {
  # FAILS until task 0.5b. A `window =` on a constraint atom is silently
  # ignored: the atoms are parsed separately from the estimated formula, so
  # their windowed terms never reach `plan$derivations` and nothing is
  # realized for them. A five-second and a thousand-second window on an
  # accumulating layer cannot describe the same risk set.
  unwindowed <- parity_constraint_mask(~ !tie(calls))
  narrow <- parity_constraint_mask(~ !tie(calls, window = 2))
  wide <- parity_constraint_mask(~ !tie(calls, window = 1000))

  expect_false(identical(mask_timeline(narrow), mask_timeline(unwindowed)))
  expect_false(identical(mask_timeline(narrow), mask_timeline(wide)))
})

test_that("a windowed constraint atom registers a derived object", {
  # FAILS until task 0.5b, and this is the cause read directly. A windowed
  # formula term puts one `kind = "window"` entry in `plan$derivations`; a
  # windowed constraint atom puts none, so `ds_realize_derivations()` has
  # nothing to build and the atom's reference still names the source layer.
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + recip,
    layer = "calls",
    model = "DyNAM",
    data = parity_toy_data(),
    support_constraint = ~ !tie(calls, window = 2)
  )

  expect_equal(parity_derived_names(parity_plan(spec), "window"), "calls_2")
})

test_that("a formula and a constraint sharing a window share one derivation", {
  # Entries deduplicate by derived identity, so the same object windowed at the
  # same width on both sides is one derived object with one expiry stream. This
  # passes today only because the constraint contributes nothing; it must still
  # pass once task 0.5b makes it contribute.
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia(calls, window = 2) + recip,
    layer = "calls",
    model = "DyNAM",
    data = parity_toy_data(),
    support_constraint = ~ !tie(calls, window = 2)
  )

  expect_equal(parity_derived_names(parity_plan(spec), "window"), "calls_2")
})

test_that("an unwindowed support constraint is unchanged", {
  # The control: the fix must not move the mask a constraint without a window
  # produces.
  mask <- parity_constraint_mask(~ !tie(calls))

  expect_true(length(mask$update_pointer) > 0)
  expect_true(all(vapply(mask_timeline(mask), is.logical, logical(1))))
})

# ---- (d) flavored creation / dissolution -----------------------------------

test_that("complementary flavor constraints preprocess identically", {
  # FAILS until task 0.4d. The dyad `1 -> 2` is created, dissolved, created and
  # dissolved again, so the derived `~ !tie(calls)` / `~ tie(calls)` pair flips
  # over it four times and the degree effects see a repeated dyad.
  spec <- parity_flavored_spec()
  oracle <- suppressWarnings(preprocess_flavored(spec))
  merged <- suppressWarnings(preprocess_joint(spec))
  map <- attr(oracle, "process_map")

  for (i in seq_len(nrow(map))) {
    expect_equal(
      parity_strip_deco(
        parity_prep_by(merged, map$family[i], flavor = map$flavor[i])
      ),
      parity_strip_deco(oracle[[as.character(map$fid[i])]])
    )
  }
})

test_that("each flavor's mask timeline has one snapshot per recorded event", {
  # The mask timeline itself, independent of the statistics: a snapshot per row
  # the fid records, on both substrates.
  merged <- suppressWarnings(preprocess_joint(parity_flavored_spec()))
  map <- attr(merged, "process_map")

  for (i in seq_len(nrow(map))) {
    prep <- merged[[as.character(map$fid[i])]]
    expect_equal(
      length(prep$support_mask$update_pointer),
      length(prep$event_time)
    )
  }
})

# ---- (e) missing data -------------------------------------------------------

test_that("a network carrying NA preprocesses identically on both substrates", {
  # FAILS today. The recipe path imputes the missing tie to 0 through
  # `ds_impute_missing()` and walks; the merged walk reaches
  # `merged_build_event_args()` with an unimputed cell and dies on
  # `if (replace_value < 0)`.
  spec <- parity_missing_spec()
  merged <- suppressWarnings(preprocess_joint(single_process_joint(spec)))

  for (family in c("rate", "choice")) {
    solo <- suppressWarnings(compute_statistics(spec, "DyNAM", family))
    expect_equal(
      parity_strip_deco(parity_prep_by(merged, family)),
      parity_strip_deco(solo)
    )
  }
})

test_that("a replace layer carrying NA keeps NA out of the statistics", {
  # The quieter half of the same defect. A replace layer never reaches the sign
  # test that turns an unimputed cell into a crash, so a missing value would
  # travel through the effect closures into the statistics instead.
  spec <- parity_missing_spec(parity_missing_replace_data())
  merged <- suppressWarnings(preprocess_joint(single_process_joint(spec)))

  for (family in c("rate", "choice")) {
    prep <- parity_prep_by(merged, family)
    expect_false(anyNA(prep$initial_stats))
    expect_false(anyNA(prep$stat_mat_update))
  }
})

test_that("a missing nodal covariate alone is unaffected", {
  # The control for the network fix: nodal values were already recoded at walk
  # time on the merged side, and the fix must leave that path alone.
  data <- parity_missing_data()
  data$ties <- data$ties[!is.na(data$ties$time), ]
  spec <- parity_missing_spec(data)
  merged <- suppressWarnings(preprocess_joint(single_process_joint(spec)))
  solo <- suppressWarnings(compute_statistics(spec, "DyNAM", "rate"))

  expect_equal(
    parity_strip_deco(parity_prep_by(merged, "rate")),
    parity_strip_deco(solo)
  )
})

test_that("two state containers never share an imputed network matrix", {
  # FAILS until task 0.4b. The first aliasing site an in-place state write
  # would expose: `ds_impute_missing()` caches the imputed matrix in
  # `src$net_override`, and `ds_network()` returns that same object on every
  # later call, so two containers built from one source hold one SEXP between
  # them.
  #
  # The assertion is on identity, not on values. Under R's copy semantics the
  # alias is harmless today -- writing through one name duplicates -- so a
  # value comparison passes over it. It stops being harmless the moment the
  # state write goes in place, which is what task 0.4c does.
  skip_if_not(capabilities("profmem"))

  src <- new_data_source(data = parity_missing_data())
  src <- ds_impute_missing(src, data.frame(dummy = 1, row.names = "calls"))

  first <- ds_network(src, "calls")
  second <- ds_network(src, "calls")
  on.exit(
    {
      untracemem(first)
      untracemem(second)
    },
    add = TRUE
  )

  expect_false(identical(tracemem(first), tracemem(second)))
})

test_that("an edgeless four() cache never aliases the state matrix", {
  # The second aliasing site (task 0.4b). `init_DyNAM_choice.four()` returned
  # `list(cache = network, stat = network)` on an edgeless network, so the
  # effect cache and the statistic were both the state's own matrix. The cache
  # is meant to hold a frozen snapshot, which an in-place state write would
  # drag along.
  skip_if_not(capabilities("profmem"))

  network <- matrix(0, 4L, 4L)
  initialized <- init_DyNAM_choice.four(
    effect_fun = function(
      network,
      is_two_mode = FALSE,
      transformer_fn = identity
    ) {
      NULL
    },
    network = network,
    window = NULL,
    n1 = 4L,
    n2 = 4L
  )
  on.exit(untracemem(network), add = TRUE)

  expect_false(identical(tracemem(network), tracemem(initialized$cache)))
  expect_equal(initialized$stat, matrix(0, 4L, 4L))
})
