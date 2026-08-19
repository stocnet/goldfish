# Shared-atom mask pooling (preprocess_pooled_support_masks). Several
# constraints over the SAME atoms (a mutually_exclusive layer's !tie / tie
# derived constraints, with or without a shared user constraint) maintain the
# atom stream ONCE and each fid projects its own boolean tree at its own
# snapshot times. The two properties the pooling rests on are verified here:
#   (a) SLICE-EXACTNESS: evaluating a constraint at a superset of snapshot times
#       and slicing its own subset equals evaluating it at the subset directly
#       (the support is piecewise-constant and a snapshot depends only on the
#       atoms strictly before it), so the pooled union-time walk sliced per fid
#       equals each fid's own dedicated pass;
#   (b) FOLD-UNCHANGED: the per-output availability fold consumes the sliced
#       support list unchanged -- byte-identical to the per-fid mask path.

# Two constraints over ONE shared atom `tie(call_network)`: `~ tie` and its
# complement `~ !tie` (the creation / dissolution shape of a mutually_exclusive
# layer). Same atom signature -> one maintainer, two boolean trees.
make_pool_fixture <- function(n_events = 80L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(call_network, calls, nodes = actors)
  calls_dependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  calls_dependent <- calls_dependent[seq_len(n_events), ]
  env <- new.env()
  assign("call_network", call_network, envir = env)
  assign("calls_dependent", calls_dependent, envir = env)
  assign("calls", calls, envir = env)
  assign("actors", actors, envir = env)
  compile <- function(constraint) {
    cp <- parse_and_validate_constraint(
      constraint,
      has_dyad_part = TRUE,
      envir = env
    )
    compile_support_constraint(
      cp,
      model = "DyNAM",
      dep_name = "calls_dependent",
      nodes = "actors",
      nodes2 = "actors",
      window_derivations = NULL,
      envir = env
    )
  }
  list(
    env = env,
    dep_times = calls_dependent$time,
    sub_tie = compile(~ tie(call_network)),
    sub_ntie = compile(~ !tie(call_network))
  )
}

per_fid_mask <- function(fx, sub, times) {
  preprocess_support_mask(
    sub,
    model = "DyNAM",
    nodes = "actors",
    nodes2 = "actors",
    symmetric = FALSE,
    snapshot_times = times,
    prep_envir = fx$env
  )
}

test_that("a superset walk sliced to a subset equals the subset walk (a)", {
  fx <- make_pool_fixture()
  superset <- fx$dep_times
  subset_idx <- c(1L, 4L, 9L, 16L, 25L, 49L)
  subset <- superset[subset_idx]

  full <- per_fid_mask(fx, fx$sub_tie, superset)
  sub <- per_fid_mask(fx, fx$sub_tie, subset)

  expect_equal(sub$support, full$support[subset_idx])
  expect_equal(sub$initial, full$initial)
})

test_that("pooled masks equal per-fid masks over one shared atom (a)", {
  fx <- make_pool_fixture()
  # Two fids over the SAME atom but DIFFERENT expressions and DIFFERENT snapshot
  # timelines -- the mutually_exclusive creation / dissolution shape.
  times_tie <- fx$dep_times
  times_ntie <- fx$dep_times[c(TRUE, FALSE)] # every other event

  pooled <- preprocess_pooled_support_masks(
    list(
      list(constraint = fx$sub_tie, snapshot_times = times_tie),
      list(constraint = fx$sub_ntie, snapshot_times = times_ntie)
    ),
    model = "DyNAM",
    nodes = "actors",
    nodes2 = "actors",
    symmetric = FALSE,
    prep_envir = fx$env
  )

  ref_tie <- per_fid_mask(fx, fx$sub_tie, times_tie)
  ref_ntie <- per_fid_mask(fx, fx$sub_ntie, times_ntie)

  expect_equal(pooled[[1L]]$support, ref_tie$support)
  expect_equal(pooled[[1L]]$initial, ref_tie$initial)
  expect_equal(pooled[[1L]]$n_stored, ref_tie$n_stored)
  expect_equal(pooled[[2L]]$support, ref_ntie$support)
  expect_equal(pooled[[2L]]$initial, ref_ntie$initial)
  expect_equal(pooled[[2L]]$n_stored, ref_ntie$n_stored)
  # the two trees genuinely differ (complementary), so this is not a trivial pass
  expect_false(isTRUE(all.equal(
    ref_tie$support,
    ref_ntie$support[seq_len(3L)]
  )))
})

test_that("several fids sharing one constraint id are evaluated once (a)", {
  fx <- make_pool_fixture()
  # The SAME compiled constraint requested at two timelines: pooled must return
  # each fid its own sliced support, not one shared slice.
  times_a <- fx$dep_times
  times_b <- fx$dep_times[seq_len(40L)]

  pooled <- preprocess_pooled_support_masks(
    list(
      list(constraint = fx$sub_tie, snapshot_times = times_a),
      list(constraint = fx$sub_tie, snapshot_times = times_b)
    ),
    model = "DyNAM",
    nodes = "actors",
    nodes2 = "actors",
    symmetric = FALSE,
    prep_envir = fx$env
  )
  expect_equal(
    pooled[[1L]]$support,
    per_fid_mask(fx, fx$sub_tie, times_a)$support
  )
  expect_equal(
    pooled[[2L]]$support,
    per_fid_mask(fx, fx$sub_tie, times_b)$support
  )
})

# A mutually_exclusive layer whose creation (!tie) / dissolution (tie) derived
# constraints share the single atom `tie(calls)` -- the landed two-flavor shape
# that routes through the pooled pass inside preprocess_flavored().
pool_flavored_spec <- function() {
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
  data <- add_flavor(
    list(info = info, nodes = nodes, ties = ties),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1)
  )
  make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg + outdeg),
    choice = list(creation ~ inertia, dissolution ~ inertia),
    model = "DyNAM",
    data = data
  )
}

test_that("the flavored path feeds each fid its own sliced mask (b)", {
  out <- suppressWarnings(preprocess_flavored(pool_flavored_spec()))
  map <- attr(out, "process_map")
  fid_of <- function(flavor, family) {
    as.character(map$fid[map$flavor == flavor & map$family == family])
  }
  cre <- out[[fid_of("creation", "rate")]]
  dis <- out[[fid_of("dissolution", "rate")]]

  # Each fid's fold consumed its OWN sliced timeline (rate fids carry their own
  # dependent + right-censored rows), not one shared slice.
  expect_length(cre$support_mask$support, length(cre$event_time))
  expect_length(dis$support_mask$support, length(dis$event_time))

  # Creation (!tie) and dissolution (tie) share the one atom pool: their
  # pre-event masks are exact complements, so both boolean trees were projected
  # from a single maintained atom state.
  expect_equal(cre$support_mask$initial, !dis$support_mask$initial)
})

test_that("a NULL-constraint request yields a NULL mask", {
  fx <- make_pool_fixture()
  pooled <- preprocess_pooled_support_masks(
    list(
      list(constraint = NULL, snapshot_times = fx$dep_times),
      list(constraint = fx$sub_tie, snapshot_times = fx$dep_times)
    ),
    model = "DyNAM",
    nodes = "actors",
    nodes2 = "actors",
    symmetric = FALSE,
    prep_envir = fx$env
  )
  expect_null(pooled[[1L]])
  expect_false(is.null(pooled[[2L]]))
})
