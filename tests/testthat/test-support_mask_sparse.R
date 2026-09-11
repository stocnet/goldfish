# Detectors for the sparse support-mask representation.
#
# The living spec has required this since `support-constraint-as-stat` was
# archived; the producer half never conformed. Each target-state assertion here
# sits behind a `skip_if()` that tests the CURRENT representation and names the
# task that lifts it, so the suite stays green while the fixture cannot silently
# pass: the moment the code lands the gate stops firing and the assertion runs.
# Assertions that already hold are the byte-identity backbone and run
# unconditionally.

# --- shared helpers --------------------------------------------------------- #

# How many atom maintainers a preprocessing builds and how many times it
# evaluates a mask. The shared-mask requirement is a statement about these two
# integers, so they are counted rather than inferred from timings.
mask_call_counts <- function(expr) {
  counts <- new.env(parent = emptyenv())
  counts$maintainers <- 0L
  counts$evaluations <- 0L
  orig_build <- build_atom_maintainer
  orig_eval <- eval_constraint_mask
  local_mocked_bindings(
    build_atom_maintainer = function(...) {
      counts$maintainers <- counts$maintainers + 1L
      orig_build(...)
    },
    eval_constraint_mask = function(...) {
      counts$evaluations <- counts$evaluations + 1L
      orig_eval(...)
    }
  )
  value <- force(expr)
  list(
    maintainers = counts$maintainers,
    evaluations = counts$evaluations,
    value = value
  )
}

# The mask at stored event `e`, read through whichever representation the object
# carries. Written this way on purpose: the fixtures must mean the same thing
# before and after the snapshot list becomes a flat stream, so they compare
# meanings rather than fields.
mask_at_event <- function(support_mask, e) {
  if (!is.null(support_mask$support)) {
    return(support_mask$support[[e]])
  }
  value <- support_mask$initial
  upd <- support_mask$update
  ptr <- support_mask$update_pointer
  hi <- if (is.null(ptr)) 0L else ptr[e]
  if (hi > 0L) {
    value[upd[1L, seq_len(hi)]] <- as.logical(upd[2L, seq_len(hi)])
  }
  value
}

mask_grid_at_event <- function(support_mask, e, n1, n2) {
  support_to_grid(
    mask_at_event(support_mask, e),
    support_mask$stored_kind %||% 0L,
    n1,
    n2
  )
}

# Social Evolution with a global attribute, so a constraint can mix a scalar, an
# ego-axis and an alter-axis atom. `seasons$winter` flips once mid-sequence.
sparse_mask_data <- function(n_events = 120L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  actors <- get("actors", environment())
  calls <- get("calls", environment())
  friendship <- get("friendship", environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(call_network, calls, nodes = actors)
  friendship_network <- make_network(nodes = actors, directed = TRUE)
  friendship_network <- link_events(
    friendship_network,
    friendship,
    nodes = actors
  )
  calls_dependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  calls_dependent <- calls_dependent[seq_len(n_events), ]
  seasons <- make_global_attributes(data.frame(winter = 0))
  season_change <- data.frame(
    time = as.POSIXct(1222553311, origin = "1970-01-01", tz = "GMT"),
    replace = 1
  )
  seasons <- link_events(seasons, season_change)
  list(
    data = make_data(
      calls_dependent,
      call_network,
      friendship_network,
      calls,
      friendship,
      actors,
      seasons
    ),
    calls = as.data.frame(calls),
    labels = actors$label,
    n_actors = nrow(actors)
  )
}

# --- 2.1 one layer, rate and choice, one mask ------------------------------- #

test_that("a rate + choice layer maintains one mask, not one per family", {
  fx <- sparse_mask_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + recip,
    layer = "call_network",
    support_constraint = ~ indeg(call_network) >= 0,
    data = fx$data
  )
  counts <- suppressMessages(suppressWarnings(
    mask_call_counts(preprocess_joint(single_process_joint(spec)))
  ))

  skip_if(
    counts$maintainers > 1L,
    "blocked on task 6.1 (one mask per (layer, flavor) process)"
  )
  # The constraint belongs to the process, so both sub-models read one
  # maintained mask and its evaluation stream is walked once.
  expect_identical(counts$maintainers, 1L)
  expect_lte(counts$evaluations, length(counts$value[[1L]]$event_time) + 1L)
})

test_that("the rate gate equals the row reduction of the shared mask", {
  # The backbone: whatever representation the mask is in, the sender gate
  # estimation consumes must equal a from-scratch
  # `rowSums(mask & receiver-availability) > 0`, boolean-exact at every event.
  fx <- sparse_mask_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + recip,
    layer = "call_network",
    support_constraint = ~ indeg(call_network) >= 0,
    data = fx$data
  )
  prep <- suppressMessages(suppressWarnings(
    compute_statistics(spec, "DyNAM", "rate")
  ))
  sm <- prep$support_mask
  n1 <- length(sm$sender_presence_init %||% prep$active_sender_init)
  n2 <- length(sm$receiver_presence_init %||% rep(TRUE, fx$n_actors))
  presence <- walk_presence_buffer(
    sm$sender_presence_init %||% prep$active_sender_init,
    prep$active_sender_update,
    prep$active_sender_update_pointer,
    length(prep$event_time)
  )
  gate <- walk_presence_buffer(
    prep$active_sender_init,
    prep$active_sender_update,
    prep$active_sender_update_pointer,
    length(prep$event_time)
  )
  receivers <- sm$receiver_presence_init %||% rep(TRUE, n2)
  for (e in seq_along(prep$event_time)) {
    grid <- mask_grid_at_event(sm, e, n1, n2)
    ref <- rowSums(grid & rep(receivers, each = n1)) > 0
    expect_identical(unname(gate[[e]]), unname(presence[[e]] & ref))
  }
})

# --- 2.3 mixed-kind atoms --------------------------------------------------- #

test_that("a scalar + ego + alter constraint takes the axis-union kind", {
  # global (3) union ego (2) union alter (1) is point (0): the mixture spans
  # both axes, so this is the case that is NOT separable and must stay dense.
  fx <- sparse_mask_data()
  prep <- suppressMessages(suppressWarnings(estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ global(seasons$winter) >= 0 &
      outdeg(call_network, type = "ego") >= 0 &
      indeg(call_network) >= 0
  )))
  expect_identical(prep$support_mask$mask_kind, 0L)
  expect_identical(prep$support_mask$stored_kind, 0L)
})

test_that("a scalar + alter constraint stores at alter kind, never dense", {
  # global union alter is alter: separable, so nothing n1 x n2 is ever built for
  # it. This is the mixture the current code cannot express -- every atom is
  # held dense today and only the STORED mask is reduced.
  fx <- sparse_mask_data()
  prep <- suppressMessages(suppressWarnings(estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ global(seasons$winter) >= 0 &
      indeg(call_network) >= 0
  )))
  sm <- prep$support_mask
  expect_identical(sm$mask_kind, 1L)
  expect_identical(sm$stored_kind, 1L)
  expect_null(dim(mask_at_event(sm, 1L)))
  expect_length(mask_at_event(sm, 1L), fx$n_actors)
})

# --- 2.5 no per-event dense list -------------------------------------------- #

test_that("a point-kind constraint carries a stream, not snapshots", {
  # The living-spec scenario that fails today: `~ tie(net)` is point-kind, so
  # every snapshot is a dense n1 x n2 matrix and there is one per event.
  fx <- sparse_mask_data()
  prep <- suppressMessages(suppressWarnings(estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ tie(call_network)
  )))
  sm <- prep$support_mask

  skip_if(
    !is.null(sm$support),
    "blocked on task 5.2 (mask emitted as a flat update stream)"
  )
  expect_null(sm$support)
  expect_false(is.null(sm$initial))
  expect_false(is.null(sm$update))
  expect_identical(nrow(sm$update), 2L)
  expect_length(sm$update_pointer, length(prep$event_time))
})

# --- 2.6 the incremental mask equals the from-scratch mask ------------------ #

test_that("the maintained mask equals from-scratch at every event", {
  # The correctness backbone. Asserted on a MIXED-kind constraint, which no
  # current test covers: the existing from-scratch check is single-atom
  # `~ tie(net)`, and mixed-kind elementwise evaluation is the one genuinely new
  # piece of logic in the change.
  fx <- sparse_mask_data()
  prep <- suppressMessages(suppressWarnings(estimate_dynam(
    calls_dependent ~ inertia + recip,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ indeg(call_network) < 2 &
      outdeg(call_network, type = "ego") < 3
  )))
  sm <- prep$support_mask
  n1 <- fx$n_actors
  n2 <- fx$n_actors

  # Independent reference: the cumulative call adjacency strictly before each
  # event, reduced on each axis the constraint reads. Both atoms are unweighted
  # degrees, so the adjacency enters as `adj != 0`.
  for (e in seq_along(prep$event_time)) {
    prev <- fx$calls[fx$calls$time < prep$event_time[e], , drop = FALSE]
    adj <- matrix(0, n1, n2)
    for (r in seq_len(nrow(prev))) {
      i <- match(prev$sender[r], fx$labels)
      j <- match(prev$receiver[r], fx$labels)
      adj[i, j] <- adj[i, j] + prev$increment[r]
    }
    ref <- outer(rowSums(adj != 0) < 3, colSums(adj != 0) < 2, "&")
    expect_identical(
      unname(mask_grid_at_event(sm, e, n1, n2)),
      unname(ref),
      info = paste("event", e)
    )
  }
})

# --- 2.4 composition change under constraint -------------------------------- #

# Fisheries: `sovchanges` moves states in and out of the node set, so receiver
# presence genuinely varies over the sequence. Social Evolution has a static
# composition, which is why the existing fold tests compare against the presence
# INIT and cannot see this.
composition_fixture <- function() {
  data("Fisheries_Treaties_6070", package = "goldfish", envir = environment())
  states <- get("states", environment())
  sovchanges <- get("sovchanges", environment())
  regchanges <- get("regchanges", environment())
  bilatnet <- get("bilatnet", environment())
  bilatchanges <- get("bilatchanges", environment())
  contignet <- get("contignet", environment())
  contigchanges <- get("contigchanges", environment())
  states <- make_nodes(states)
  states <- link_events(states, sovchanges, attribute = "present")
  states <- link_events(states, regchanges, attribute = "regime")
  bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
  bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
  contignet <- make_network(contignet, nodes = states, directed = FALSE)
  contignet <- link_events(contignet, contigchanges, nodes = states)
  create_bilat <- make_dependent_events(
    events = bilatchanges[bilatchanges$increment == 1, ],
    nodes = states,
    default_network = bilatnet
  )
  make_data(
    create_bilat,
    bilatnet,
    contignet,
    states,
    sovchanges,
    regchanges,
    bilatchanges,
    contigchanges
  )
}

composition_prep <- function(data, constraint = NULL) {
  suppressMessages(suppressWarnings(estimate_dynam(
    create_bilat ~ inertia + tie(contignet),
    sub_model = "choice",
    data = data,
    preprocessing_only = TRUE,
    support_constraint = constraint
  )))
}

test_that("the maintained availability collapses presence and the constraint", {
  data <- composition_fixture()
  # The unconstrained run's `active_dyad` IS the raw receiver presence, so it
  # supplies the presence timeline the constrained run folds away.
  raw <- composition_prep(data)
  # `< 5` rather than `> 0`: an absent state has no contiguity ties, so a
  # lower-bound constraint excludes it for the wrong reason and the "absent
  # regardless of what the constraint allows" case never arises. An upper bound
  # both bites (highly contiguous states are excluded) and allows absent ones.
  con <- composition_prep(data, ~ indeg(contignet) < 5)
  n_stored <- length(con$event_time)
  expect_identical(length(raw$event_time), n_stored)
  expect_identical(con$active_dyad_encoding, "alter")

  presence <- walk_presence_buffer(
    raw$active_dyad_init,
    raw$active_dyad_update,
    raw$active_dyad_update_pointer,
    n_stored
  )
  folded <- walk_presence_buffer(
    con$active_dyad_init,
    con$active_dyad_update,
    con$active_dyad_update_pointer,
    n_stored
  )
  sm <- con$support_mask
  n2 <- length(raw$active_dyad_init)
  n1 <- length(con$active_sender_init)

  # The composition really moves, otherwise the rest of this test is vacuous.
  expect_gt(length(unique(vapply(presence, sum, numeric(1)))), 1L)

  gated_while_allowed <- 0L
  for (e in seq_len(n_stored)) {
    mask <- mask_grid_at_event(sm, e, n1, n2)[1L, ]
    expect_identical(folded[[e]], presence[[e]] & mask)
    gated_while_allowed <- gated_while_allowed + sum(mask & !presence[[e]])
  }
  # An absent node is excluded regardless of what the constraint allows, and
  # that case actually occurs here.
  expect_gt(gated_while_allowed, 0L)
})

composition_rate_prep <- function(data, constraint = NULL) {
  suppressMessages(suppressWarnings(estimate_dynam(
    create_bilat ~ 1 + indeg(bilatnet),
    sub_model = "rate",
    data = data,
    preprocessing_only = TRUE,
    support_constraint = constraint
  )))
}

test_that("the rate gate follows receiver departures, not only mask flips", {
  # The gate is `presence & (rowSums(mask & receiver-presence) > 0)`, and BOTH
  # of its inner axes move over this sequence. The constraint has to be
  # POINT-kind for a departure to be visible: a separable mask shares one
  # allowed set across every sender, so all of it would have to leave before
  # any gate moved, while a point mask gives sender i its own row, which can
  # be small enough that a single departure empties it.
  data <- composition_fixture()
  # The unconstrained run supplies both raw timelines the constrained run
  # folds together.
  raw <- composition_rate_prep(data)
  con <- composition_rate_prep(data, ~ tie(contignet))
  n_stored <- length(con$event_time)
  expect_identical(length(raw$event_time), n_stored)

  sm <- con$support_mask
  # Point-kind, or the fixture cannot see a departure at all.
  expect_identical(sm$stored_kind %||% 0L, 0L)

  n1 <- length(raw$active_sender_init)
  n2 <- length(raw$active_dyad_init)
  senders <- walk_presence_buffer(
    raw$active_sender_init,
    raw$active_sender_update,
    raw$active_sender_update_pointer,
    n_stored
  )
  receivers <- walk_presence_buffer(
    raw$active_dyad_init,
    raw$active_dyad_update,
    raw$active_dyad_update_pointer,
    n_stored
  )
  gate <- walk_presence_buffer(
    con$active_sender_init,
    con$active_sender_update,
    con$active_sender_update_pointer,
    n_stored
  )

  # The receiver composition really moves, otherwise the rest is vacuous.
  expect_gt(length(unique(vapply(receivers, sum, numeric(1)))), 1L)

  for (e in seq_len(n_stored)) {
    grid <- mask_grid_at_event(sm, e, n1, n2)
    ref <- senders[[e]] &
      (rowSums(grid & rep(receivers[[e]], each = n1)) > 0)
    expect_identical(unname(gate[[e]]), unname(ref))
  }
})

# --- 2.2 two flavors, a derived constraint and an exogenous one ------------- #

# Creation and dissolution on one layer, so the derived complementary
# `~ !tie(calls)` / `~ tie(calls)` pair applies, AND a user constraint on a
# second, exogenous tie layer whose ties flip over time. Each flavor's mask then
# moves for two independent reasons, one endogenous and one exogenous.
flavored_exogenous_fixture <- function(seed = 11L) {
  withr::local_seed(seed)
  base <- flavored_fixture_data()
  n <- nrow(base$nodes)
  pairs <- expand.grid(from = seq_len(n), to = seq_len(n))
  pairs <- pairs[pairs$from != pairs$to, ]
  allowed_history <- data.frame(
    from = pairs$from,
    to = pairs$to,
    time = NA_real_,
    layer = "allowed",
    weight = 1,
    flavor = NA_character_
  )
  # Flips are drawn from dyads that are never observed, so the exogenous layer
  # can never exclude an observed dyad and trip the fail-fast validation.
  observed <- base$ties[base$ties$layer == "calls" & !is.na(base$ties$time), ]
  observed_key <- paste(observed$from, observed$to, sep = "-")
  candidates <- pairs[
    !paste(pairs$from, pairs$to, sep = "-") %in% observed_key,
    ,
    drop = FALSE
  ]
  flip_times <- seq(5, 75, by = 10)
  picked <- candidates[sample.int(nrow(candidates), length(flip_times)), ]
  flips <- data.frame(
    from = picked$from,
    to = picked$to,
    time = flip_times,
    layer = "allowed",
    weight = rep(c(0, 1), length.out = length(flip_times)),
    flavor = NA_character_
  )
  info <- base$info
  info$update <- c(calls = "increment", allowed = "replace")
  info$directed <- c(calls = TRUE, allowed = TRUE)
  info$observation <- c(calls = "event", allowed = "event")
  list(
    data = list(
      info = info,
      nodes = base$nodes,
      ties = rbind(base$ties, allowed_history, flips)
    ),
    n_actors = n
  )
}

flavored_exogenous_spec <- function(fx) {
  make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    support_constraint = ~ tie(allowed),
    data = fx$data
  )
}

# The cumulative state of one layer strictly before `time`, as a logical
# adjacency. `calls` accumulates +-1 increments; `allowed` replaces.
layer_state_before <- function(ties, layer, semantics, time, n) {
  rows <- ties[ties$layer == layer, , drop = FALSE]
  rows <- rows[is.na(rows$time) | rows$time < time, , drop = FALSE]
  rows <- rows[
    order(is.na(rows$time), rows$time, decreasing = c(TRUE, FALSE)),
  ]
  adj <- matrix(0, n, n)
  for (r in seq_len(nrow(rows))) {
    i <- rows$from[r]
    j <- rows$to[r]
    adj[i, j] <- if (identical(semantics, "increment")) {
      adj[i, j] + rows$weight[r]
    } else {
      rows$weight[r]
    }
  }
  adj != 0
}

test_that("each flavor keeps its own mask over one shared atom pool", {
  fx <- flavored_exogenous_fixture()
  spec <- flavored_exogenous_spec(fx)
  counts <- suppressMessages(suppressWarnings(
    mask_call_counts(preprocess_joint(single_process_joint(spec)))
  ))
  map <- single_process_joint(spec)$process_map
  # Two flavors, two constraints, four sub-models: the mask is per flavor and
  # the two sub-models of a flavor share it.
  expect_length(unique(map$constraint_id), 2L)
  expect_length(counts$value, 4L)

  skip_if(
    counts$maintainers > 1L,
    "blocked on tasks 6.1 / 7.2 (one atom pool for the whole process)"
  )
  # Both flavors read the same atoms, so the pool is walked once for the layer
  # rather than once per sub-model family.
  expect_identical(counts$maintainers, 1L)
})

test_that("each flavor's mask is its derived half of the allowed set", {
  # The endogenous half is complementary by construction and the exogenous half
  # is shared, so creation's mask is `!tie(calls) & tie(allowed)` and
  # dissolution's is `tie(calls) & tie(allowed)`. Asserting both against one
  # shared `allowed` reference is what makes this a test of two independent
  # reasons to move rather than of two unrelated constraints.
  #
  # The two flavors do NOT share a timeline: each stores only its own flavor's
  # events, so each mask is checked at its own event times.
  fx <- flavored_exogenous_fixture()
  spec <- flavored_exogenous_spec(fx)
  res <- suppressMessages(suppressWarnings(
    preprocess_joint(single_process_joint(spec))
  ))
  map <- single_process_joint(spec)$process_map
  n <- fx$n_actors
  expected_half <- list(
    creation = function(present, allowed) !present & allowed,
    dissolution = function(present, allowed) present & allowed
  )
  moved <- 0L
  for (flavor in names(expected_half)) {
    fid <- as.character(map$fid[map$flavor == flavor & map$family == "choice"])
    out <- res[[fid]]
    prev_allowed <- NULL
    for (e in seq_along(out$event_time)) {
      tt <- out$event_time[e]
      allowed <- layer_state_before(fx$data$ties, "allowed", "replace", tt, n)
      present <- layer_state_before(fx$data$ties, "calls", "increment", tt, n)
      expect_identical(
        unname(mask_grid_at_event(out$support_mask, e, n, n)),
        unname(expected_half[[flavor]](present, allowed)),
        info = paste(flavor, "event", e)
      )
      if (!is.null(prev_allowed) && !identical(prev_allowed, allowed)) {
        moved <- moved + 1L
      }
      prev_allowed <- allowed
    }
  }
  # The exogenous layer really flips during the sequence, otherwise the mask
  # only ever moves for the endogenous reason and the fixture is half a test.
  expect_gt(moved, 0L)
})

# --- 5.5 the mask as a stream, and the gate as a counter -------------------- #

test_that("a symmetrised mask is stored at point kind whatever its atoms say", {
  # `m & t(m)` of a row-constant mask is an outer product, so symmetrising
  # destroys separability: a coordination constraint whose atoms are all
  # receiver-axis still has to be stored dense. The atoms' axis-union is kept
  # separately, because it is what the tree is EVALUATED at -- only the storage
  # is forced.
  fx <- sparse_mask_data()
  prep <- suppressMessages(suppressWarnings(estimate_dynam(
    calls_dependent ~ inertia,
    sub_model = "choice_coordination",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ indeg(call_network) >= 0
  )))
  sm <- prep$support_mask
  expect_identical(sm$mask_kind, 1L)
  expect_identical(sm$stored_kind, 0L)
  expect_identical(dim(sm$initial), c(fx$n_actors, fx$n_actors))

  # The control: the same constraint on a non-symmetric family keeps its
  # separable storage.
  plain <- suppressMessages(suppressWarnings(estimate_dynam(
    calls_dependent ~ inertia,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ indeg(call_network) >= 0
  )))
  expect_identical(plain$support_mask$stored_kind, 1L)
  expect_null(dim(plain$support_mask$initial))
})

test_that("the mask's update buffer is the size of its own information", {
  # The motivating contrast: a mask that never moves emits nothing, and one that
  # moves emits its flips rather than one stored value per event. Under the old
  # representation both stored n_events values.
  fx <- sparse_mask_data()
  never <- suppressMessages(suppressWarnings(estimate_dynam(
    calls_dependent ~ inertia,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ indeg(call_network) >= 0
  )))
  moves <- suppressMessages(suppressWarnings(estimate_dynam(
    calls_dependent ~ inertia,
    sub_model = "choice",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ indeg(call_network) < 2
  )))
  n_events <- length(never$event_time)

  expect_identical(ncol(never$support_mask$update), 0L)
  expect_gt(ncol(moves$support_mask$update), 0L)
  expect_lt(ncol(moves$support_mask$update), n_events)
  # The pointer is the only per-event storage left, and it is one number each.
  expect_length(moves$support_mask$update_pointer, n_events)
})

test_that("the sender gate emits only zero-crossings", {
  # The counter's whole point: a sender's availability is reported when its
  # allowed-receiver count reaches or leaves zero, not whenever the mask moves.
  # A permissive constraint over a static composition never crosses at all.
  fx <- sparse_mask_data()
  prep <- suppressMessages(suppressWarnings(estimate_dynam(
    calls_dependent ~ 1 + indeg,
    sub_model = "rate",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ indeg(call_network) >= 0
  )))
  expect_true(isTRUE(prep$active_sender_folded))
  expect_identical(ncol(prep$active_sender_update), 0L)
  expect_true(all(prep$active_sender_init))

  # And a constraint that genuinely closes senders out does cross, but far less
  # often than the mask moves.
  binding <- suppressMessages(suppressWarnings(estimate_dynam(
    calls_dependent ~ 1 + indeg,
    sub_model = "rate",
    data = fx$data,
    preprocessing_only = TRUE,
    support_constraint = ~ indeg(call_network) < 1
  )))
  expect_lt(
    ncol(binding$active_sender_update),
    length(binding$event_time)
  )
})

# --- 6.2 a sender-axis constraint needs no dyad mask ------------------------ #

test_that("a sender-axis-only constraint allocates no dyad mask", {
  # The living spec's scenario. An ego-kind constraint varies on the sender axis
  # alone, so its mask IS the gate: there is nothing dyadic to reduce, and no
  # sub-model of the process builds a dyad-shaped object for it.
  fx <- sparse_mask_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "call_network",
    support_constraint = ~ outdeg(call_network, type = "ego") >= 0,
    data = fx$data
  )
  counts <- suppressMessages(suppressWarnings(
    mask_call_counts(preprocess_joint(single_process_joint(spec)))
  ))
  expect_identical(counts$maintainers, 1L)

  for (out in counts$value) {
    sm <- out$support_mask
    expect_identical(sm$mask_kind, 2L)
    expect_identical(sm$stored_kind, 2L)
    expect_null(dim(sm$initial))
    expect_length(sm$initial, fx$n_actors)
  }
})

test_that("the shared mask serves sub-models that store it differently", {
  # A layer's rate and its COORDINATION choice share one atom pool and one
  # evaluation, and differ only in whether the stored mask is symmetrised. That
  # is the case the per-request `symmetric` exists for: sharing the pool must
  # not force one sub-model to store the other's shape.
  fx <- sparse_mask_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    choice_sub_model = "choice_coordination",
    layer = "call_network",
    support_constraint = ~ indeg(call_network) >= 0,
    data = fx$data
  )
  counts <- suppressMessages(suppressWarnings(
    mask_call_counts(preprocess_joint(single_process_joint(spec)))
  ))
  expect_identical(counts$maintainers, 1L)

  kinds <- vapply(
    counts$value,
    function(out) out$support_mask$stored_kind,
    integer(1)
  )
  # One sub-model stores the separable mask, the other the symmetrised grid.
  expect_setequal(kinds, c(0L, 1L))
})

# --- 8.2 an object from before the stream is refused, not misread ----------- #

test_that("a support_mask predating the update stream is refused", {
  local_reproducible_output()
  # The hazard is silence, not failure: a cursor over an absent update buffer
  # finds no flips and returns the initial mask at every event, so the model is
  # estimated on a constraint frozen at time zero. The epoch stamp cannot see
  # it -- both layouts are epoch 2 inside this development line -- so the
  # consumer names the component it needs.
  stale <- list(
    support = list(matrix(TRUE, 2L, 2L), matrix(FALSE, 2L, 2L)),
    initial = matrix(TRUE, 2L, 2L),
    stored_kind = 0L,
    n_stored = 2L
  )
  expect_snapshot(error = TRUE, mask_cursor(stale))
  expect_error(mask_flips(stale), "predates the mask update stream")

  # A mask that legitimately never moves has an EMPTY buffer, not an absent
  # one, and is read without complaint.
  empty <- mask_from_timeline(list(matrix(TRUE, 2L, 2L), matrix(TRUE, 2L, 2L)))
  expect_identical(ncol(empty$update), 0L)
  expect_silent(mask_cursor(empty))
})
