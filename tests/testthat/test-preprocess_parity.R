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
    expect_equal(
      parity_strip_deco(parity_prep_by(merged, family)),
      parity_frozen(paste0("repeated_dyad__", family))
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

  expect_equal(
    parity_prep_by(merged, "rate")$stat_mat_update,
    parity_frozen("unweighted_degree_update")
  )
})

test_that("the merged walk runs a windowed term instead of refusing it", {
  # This test used to pin the abort. The abort existed because a windowed term
  # puts its derived object into the shared registry while the shared source
  # had never been asked to realize it, so `build_state_container()` asked for
  # a layer that did not exist and died on `non-numeric matrix extent`. The
  # source now realizes the derivations, so the expectation moves with the
  # code: what has to hold is that the walk runs and that the derived object is
  # live in the shared state, since a windowed statistic read off a missing
  # layer would be silently zero rather than loud.
  spec <- parity_toy_spec_windowed()
  merged <- build_merged_blocks(single_process_joint(spec))

  expect_gt(length(merged$window_derived), 0L)
  for (derived in merged$window_derived) {
    expect_true(!is.null(merged$state$networks[[derived]]))
  }
  expect_no_error(
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
    expect_equal(
      parity_strip_deco(parity_prep_by(merged, family)),
      parity_frozen(paste0("se_realistic__", family))
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
  merged <- suppressWarnings(preprocess_joint(spec))
  map <- parity_frozen("flavored_oracle_map")

  for (i in seq_len(nrow(map))) {
    expect_equal(
      parity_strip_deco(
        parity_prep_by(merged, map$family[i], flavor = map$flavor[i])
      ),
      parity_frozen(paste0("flavored_oracle__", map$fid[i]))
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
    expect_equal(
      parity_strip_deco(parity_prep_by(merged, family)),
      parity_frozen(paste0("missing_identical__", family))
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

  expect_equal(
    parity_strip_deco(parity_prep_by(merged, "rate")),
    parity_frozen("missing_nodal__rate")
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

# ---- (g) writers are a parameter of the merged walk, not a hardcode ---------

# The merged walk built every consumer's writer with `writer_default()`
# whatever the caller asked for, so `output = "gather"` and `output = "db"` had
# no merged-walk path at all. The pair now threads from the caller through
# `run_merged_walk()` and `build_walk_engine()` into `init_consumers()`.
#
# The comparison is at the WRITER's own stack, not at the rendered output. The
# recipe path's gather goes on through `finalize_gather_output()`, which adds
# the labels, the effect descriptions, the timespan and the column names that
# turn a stack into an export; the merged walk does not reach that stage until
# the dispatch flip. What the writer itself produces is these six fields, and
# the column names are stripped because they are attached by the render.
gather_stack_fields <- c(
  "stat_all_events",
  "n_candidates",
  "selected",
  "index_i",
  "index_j",
  "has_intercept"
)

gather_stack <- function(x) {
  out <- x[gather_stack_fields]
  dimnames(out$stat_all_events) <- NULL
  out
}

merged_with_writer <- function(spec, new_writer) {
  suppressWarnings(preprocess_joint(
    single_process_joint(spec),
    writer = new_writer(),
    new_writer = new_writer
  ))
}

test_that("the merged walk builds a gather stack like the recipe loops", {
  spec <- parity_toy_spec()
  merged <- merged_with_writer(spec, writer_gather)

  for (family in c("rate", "choice")) {
    expect_equal(
      gather_stack(parity_prep_by(merged, family)),
      parity_frozen(paste0("gather_stack__", family))
    )
  }
})

test_that("the merged walk takes a db writer and carries its target", {
  # A db writer IS a gather writer that remembers where to persist; the write
  # itself happens at the export boundary, which the merged walk reaches only
  # at the dispatch flip. So what this task can assert is that the pair is
  # accepted, that the stack is the gather stack, and that the target survives.
  skip_if_not_installed("RSQLite")
  spec <- parity_toy_spec()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))

  merged_db <- suppressWarnings(preprocess_joint(
    single_process_joint(spec),
    writer = writer_db(con, "stats"),
    new_writer = function() writer_db(con, "stats")
  ))
  merged_gather <- merged_with_writer(spec, writer_gather)

  for (family in c("rate", "choice")) {
    expect_equal(
      gather_stack(parity_prep_by(merged_db, family)),
      gather_stack(parity_prep_by(merged_gather, family))
    )
  }
})

test_that("a default-writer walk is unchanged by the threading", {
  # The regression guard for the parameter itself: the default path must be
  # byte-identical to what it produced when the writer was hardcoded.
  spec <- parity_toy_spec()
  threaded <- merged_with_writer(spec, writer_default)
  plain <- suppressWarnings(preprocess_joint(single_process_joint(spec)))

  for (family in c("rate", "choice")) {
    expect_equal(
      parity_strip_deco(parity_prep_by(threaded, family)),
      parity_strip_deco(parity_prep_by(plain, family))
    )
  }
})

# ---- (h) the observation window on the shared clock ------------------------

# `run_merged_walk()` aborted on any explicit start or end time, so a bounded
# model had no merged-walk path. The bound now lands on the shared schedule the
# way `prepare_recipe_context()` lands it on a recipe schedule: rows before the
# start update state without being written, rows after the end are dropped
# after one clipped closing row, and a timed engine writes the closing
# right-censored row at the end time when the schedule runs out first.
#
# The four cases are the ones that exercise different branches: a start alone
# opens the window mid-sequence, an end alone closes it early, both together
# compose, and an end past the last event is the case where the closing row
# comes from the trailing interval rather than from a clipped event.
parity_window_cases <- list(
  start_only = list(start_time = 3, end_time = NULL),
  end_only = list(start_time = NULL, end_time = 4.5),
  both = list(start_time = 2.5, end_time = 5),
  end_past_last = list(start_time = NULL, end_time = 8)
)

for (case_name in names(parity_window_cases)) {
  local({
    case <- parity_window_cases[[case_name]]
    label <- case_name
    test_that(paste("a bounded window preprocesses identically,", label), {
      spec <- parity_toy_spec()
      control <- set_preprocessing(
        start_time = case$start_time,
        end_time = case$end_time
      )
      merged <- suppressWarnings(preprocess_joint(
        single_process_joint(spec),
        control_preprocessing = control
      ))

      for (family in c("rate", "choice")) {
        expect_equal(
          parity_strip_deco(parity_prep_by(merged, family)),
          parity_frozen(paste0("bounded_", label, "__", family))
        )
      }
    })
  })
}

# ---- (i) window effects on the merged walk ---------------------------------

# A windowed term is not a special kind of effect, it is an ordinary effect
# over a DERIVED object: the source network plus a dissolve stream of the same
# events shifted by the window length with negated increments. The recipe path
# realizes both from `plan$derivations`; the merged walk aborted instead,
# because the derived object reached the shared registry while the shared
# source had never been asked to realize it, so the state container died
# looking for a layer that did not exist.
test_that("a windowed term preprocesses identically on both substrates", {
  spec <- parity_toy_spec_windowed()
  merged <- suppressWarnings(preprocess_joint(single_process_joint(spec)))

  for (family in c("rate", "choice")) {
    expect_equal(
      parity_strip_deco(parity_prep_by(merged, family)),
      parity_frozen(paste0("windowed_plain__", family))
    )
  }
})

# The fields a right-censored row past the last real event changes. Named
# before the full-object comparison so a failure says which side of the
# observation window moved, not only that the objects differ.
parity_extent_fields <- c(
  "event_time",
  "is_dependent",
  "intervals",
  "total_time",
  "end_time"
)

test_that("a windowed term on a timed engine preprocesses identically", {
  # A dissolve row sits one window length after the event it expires, so the
  # last of them is past the last real event. The recipe loop bounds its walk
  # at the last real event whenever a window effect is present; a merged walk
  # that instead steps those rows hands every timed engine that reads the
  # derived object a right-censored row per expiry. The choice-side fixture
  # above is the control: no timed engine reads a derived object there.
  spec <- parity_toy_spec_windowed_rate()
  merged <- suppressWarnings(preprocess_joint(single_process_joint(spec)))

  for (family in c("rate", "choice")) {
    merged_prep <- parity_strip_deco(parity_prep_by(merged, family))
    frozen <- parity_frozen(paste0("windowed_timed_rate__", family))
    expect_equal(
      merged_prep[parity_extent_fields],
      frozen[parity_extent_fields]
    )
    expect_equal(merged_prep, frozen)
  }
})

test_that("a windowed REM rate preprocesses identically", {
  # The same extent rule on the dyad-shaped timed engine.
  spec <- parity_rem_spec_windowed()
  merged <- suppressWarnings(preprocess_joint(single_process_joint(spec)))

  merged_prep <- parity_strip_deco(parity_prep_by(merged, "rate"))
  frozen <- parity_frozen("windowed_rem_rate")
  expect_equal(
    merged_prep[parity_extent_fields],
    frozen[parity_extent_fields]
  )
  expect_equal(merged_prep, frozen)
})

test_that("a REM rate without an explicit intercept carries one on both entries", {
  # The intercept-default detector. An exact-time rate model force-adds the time
  # intercept, and the legacy estimation entry (`compute_statistics()`) did that
  # while `preprocess_joint()` kept the formula as written, so the flag differed
  # between the two entries whenever the `1` was omitted. Both must now report a
  # time intercept and preprocess byte-identically, which is what pins the
  # merged walk to the likelihood shape estimation chose.
  spec <- parity_rem_spec_no_intercept()
  merged <- suppressWarnings(preprocess_joint(single_process_joint(spec)))
  solo <- suppressMessages(suppressWarnings(
    compute_statistics(spec, "REM", "rate")
  ))

  merged_prep <- parity_prep_by(merged, "rate")
  expect_true(merged_prep$has_intercept)
  expect_true(merged_prep$is_exact_time)
  expect_true(solo$has_intercept)
  expect_equal(
    parity_strip_deco(merged_prep),
    parity_strip_deco(solo)
  )
})

test_that("a windowed term and a bound compose", {
  # The two lifts of this group meet here: the expiry rows are ordinary
  # covariate rows, so they have to obey the observation window like any other.
  spec <- parity_toy_spec_windowed()
  control <- set_preprocessing(start_time = 2.5, end_time = 5)
  merged <- suppressWarnings(preprocess_joint(
    single_process_joint(spec),
    control_preprocessing = control
  ))

  for (family in c("rate", "choice")) {
    expect_equal(
      parity_strip_deco(parity_prep_by(merged, family)),
      parity_frozen(paste0("windowed_bound__", family))
    )
  }
})

test_that("a window on a list(net1, net2) term derives every member", {
  # A windowed mixed term names its networks as a list, and the parser records
  # one derivation per member of that list. The shared source has to realize
  # all of them: a walk that realized only the first would read the second
  # off a missing layer, and the container would refuse to build.
  spec <- parity_toy_spec_window_list()
  merged <- build_merged_blocks(single_process_joint(spec))
  expect_setequal(merged$window_derived, c("calls_2", "net2_2"))

  merged_prep <- suppressWarnings(preprocess_joint(single_process_joint(spec)))
  for (family in c("rate", "choice")) {
    expect_equal(
      parity_strip_deco(parity_prep_by(merged_prep, family)),
      parity_frozen(paste0("window_list__", family))
    )
  }
})

test_that("a tied expiry follows the dependent event, as in the recipe", {
  # With a window of 2 on events at 1..6, every expiry from 3 on lands on the
  # time of a later dependent event. The recipe loop evaluates that event at
  # the state BEFORE the expiry is applied (its dependent stream sorts first
  # at a tie), then applies the event's own creation row, then the expiry.
  # Read both schedules row by row rather than trusting the statistics: a
  # reversed tie would show up as a different value at the tied events, but
  # a same-valued coincidence would hide it.
  spec <- parity_toy_spec_windowed_rate()
  merged <- build_merged_blocks(single_process_joint(spec))$schedule

  # The recipe loop's own schedule, frozen: `compute_statistics()` no longer
  # reaches `build_event_schedule()`, so the mock that captured it lands nowhere.
  recipe_rows <- parity_frozen("tied_recipe_rows")
  merged_rows <- parity_schedule_rows(merged, merged$layer)
  expect_equal(merged_rows, recipe_rows)

  tied <- intersect(
    merged$time[merged$dependent],
    merged$time[merged$layer == "calls_2" & merged$value < 0]
  )
  expect_equal(tied, c(3, 4, 5, 6))
  for (t in tied) {
    at_t <- merged_rows[merged_rows$time == t, ]
    expect_equal(
      paste(at_t$layer, at_t$value),
      c("calls 1", "calls 1", "calls_2 1", "calls_2 -1")
    )
    expect_equal(at_t$dependent, c(TRUE, FALSE, FALSE, FALSE))
  }
})

test_that("two processes windowing the same source share one derived object", {
  # Deduplication keys on the derived object's identity, so a join whose two
  # processes window the same network by the same length realizes it once and
  # carries one expiry stream, not two: one registry entry, one stream index,
  # and creation plus expiry rows for each source event exactly once.
  joint <- parity_two_process_windowed()
  merged <- build_merged_blocks(joint)
  schedule <- merged$schedule
  derived_rows <- schedule$layer == "friendship_2"
  n_source_events <- sum(joint$data$ties$layer == "friendship")

  expect_equal(merged$window_derived, "friendship_2")
  expect_equal(sum(merged$objects$name == "friendship_2"), 1L)
  expect_length(unique(schedule$stream_index[derived_rows]), 1L)
  expect_equal(sum(derived_rows), 2L * n_source_events)
  expect_equal(sum(schedule$value[derived_rows] > 0), n_source_events)
  expect_equal(sum(schedule$value[derived_rows] < 0), n_source_events)

  # The walk runs, and the process whose events set the shared clock's extent
  # preprocesses as it would alone. The other process's fids differ from
  # their standalone output by construction: a joint specification bounds
  # every process on the shared clock and writes cross-process right-censored
  # rows, which is the joint contract, not a window defect.
  out <- suppressWarnings(preprocess_joint(joint))
  map <- attr(out, "process_map")
  for (family in c("rate", "choice")) {
    fid <- map$fid[map$layer == "calls" & map$family == family]
    expect_equal(
      parity_strip_deco(out[[as.character(fid)]]),
      parity_frozen(paste0("two_proc_calls__", family))
    )
  }
})

test_that("the shared schedule is ordered by (time, stream_index)", {
  # The schedule assigns its own stream indices as it adds each unit's
  # streams, dependent stream first, then that unit's covariate streams. The
  # ordering rule is `(time, stream_index)`, so at a tie a process's event is
  # evaluated before the covariate row that applies it to its own layer.
  schedule <- build_merged_blocks(parity_two_process_windowed())$schedule

  expect_equal(
    order(schedule$time, schedule$stream_index, method = "radix"),
    seq_len(schedule$n)
  )
  for (focal in unique(schedule$layer[schedule$dependent])) {
    is_focal <- schedule$layer == focal
    dependent_index <- unique(schedule$stream_index[
      is_focal & schedule$dependent
    ])
    covariate_index <- unique(schedule$stream_index[
      is_focal & !schedule$dependent
    ])
    expect_length(dependent_index, 1L)
    expect_length(covariate_index, 1L)
    expect_lt(dependent_index, covariate_index)
  }
})

# ---- (j) opportunity sets and user constraints through the single unit ------

# Both restrictions were already wired on the joint side; what was unproven is
# that the SINGLE-process entry reaches them, which is the entry every user
# path takes once dispatch flips. The fixtures are deliberately relational --
# merged output against the recipe loop's, never an assertion naming
# `active_dyad_encoding` -- because the encoding literals live in
# `test-active_dyad_fold.R`, which `constraint-availability-encoding` owns and
# rewrites. Naming one here would make that change's ordering expensive for no
# gain.

# One opportunity set per dependent event, varying with the sender, so the
# folded receiver availability genuinely moves event to event rather than
# collapsing to a constant a wrong fold would also reproduce.
parity_toy_opportunities <- function() {
  senders <- c(1L, 2L, 1L, 3L, 2L, 4L)
  n <- 5L
  lapply(senders, function(s) setdiff(seq_len(n), c(s, (s %% n) + 1L)))
}

test_that("an opportunity list preprocesses identically on both substrates", {
  # `opportunities_list` is deprecated, and that is exactly why the merged walk
  # has to reproduce it: a deprecated surface still has users until it is
  # removed, and the substrate swap must not be what breaks them.
  withr::local_options(lifecycle_verbosity = "quiet")
  spec <- parity_constraint_spec(NULL)
  control <- set_preprocessing(opportunities_list = parity_toy_opportunities())

  merged <- suppressWarnings(preprocess_joint(
    single_process_joint(spec),
    control_preprocessing = control
  ))

  expect_equal(
    parity_strip_deco(parity_prep_by(merged, "choice")),
    parity_frozen("opportunity__choice")
  )
})

test_that("an opportunity list actually restricts, on both substrates", {
  # The guard against a vacuous comparison: two objects that agree because
  # neither applied the restriction would pass the test above. The candidate
  # counts have to be smaller than the unrestricted run's.
  withr::local_options(lifecycle_verbosity = "quiet")
  spec <- parity_constraint_spec(NULL)
  control <- set_preprocessing(opportunities_list = parity_toy_opportunities())

  restricted <- suppressWarnings(preprocess_joint(
    single_process_joint(spec),
    control_preprocessing = control
  ))
  plain <- suppressWarnings(preprocess_joint(single_process_joint(spec)))

  expect_true(isTRUE(parity_prep_by(restricted, "choice")$active_dyad_folded))
  expect_false(identical(
    parity_prep_by(restricted, "choice")$active_dyad_init,
    parity_prep_by(plain, "choice")$active_dyad_init
  ))
})

# `~ !tie(calls)` cannot be used here and the reason is worth stating: the toy
# repeats the dyad 1 -> 2 at t = 1 and t = 3, so by the second event the tie
# exists and the constraint excludes the dyad the data observes. The validation
# rejects that, correctly, on either substrate. `net2` carries (3,1), (1,4) and
# (5,2), none of which the calls layer ever observes, so `~ !tie(net2)` is
# binding without contradicting the events.
test_that("a user support constraint preprocesses identically, both families", {
  spec <- parity_constraint_spec(~ !tie(net2))

  merged <- suppressWarnings(preprocess_joint(single_process_joint(spec)))

  for (family in c("rate", "choice")) {
    expect_equal(
      parity_strip_deco(parity_prep_by(merged, family)),
      parity_frozen(paste0("user_constraint__", family))
    )
  }
})

test_that("a constraint and a bounded window compose on the single unit", {
  # The three restrictions this group lifted meet here: a user constraint, the
  # observation window, and the single-unit entry.
  spec <- parity_constraint_spec(~ !tie(net2))
  control <- set_preprocessing(start_time = 2.5, end_time = 5)

  merged <- suppressWarnings(preprocess_joint(
    single_process_joint(spec),
    control_preprocessing = control
  ))

  for (family in c("rate", "choice")) {
    expect_equal(
      parity_strip_deco(parity_prep_by(merged, family)),
      parity_frozen(paste0("constraint_bound__", family))
    )
  }
})
