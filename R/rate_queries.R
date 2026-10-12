# =========================================================================== #
# Rate queries over a candidate sequence.
#
# A proposal density for an augmented event needs the model's frozen intensity
# for a named pair at a position inside a candidate (not yet accepted)
# sequence. The candidate is bound to a completed specification, a walk handle
# is opened over it, and the candidate's events are replayed through the
# handle up to the queried point.
#
# Replay is addressed by candidate event, not by time: a query can address
# "after the first of two tied events", which no time cutoff can.
# =========================================================================== #

# Replay the candidate up to position `after`: the state right after candidate
# event `after`'s realization, `after = 0` being the state at open. It resumes
# from where the previous call stopped, so the handle only moves forward.
#
# Every earlier stamp is applied as the batch walk reads it (replay_stamps()):
# advance strictly before the stamp, then inject its realizations; the stamp's
# exogenous rows come in with the next advance. At the stamp of event `after`
# the node composition is brought in and the realizations are injected through
# `after`, but no exogenous row of that stamp is applied, wherever its stream
# sorts: what is stamped at an actor's event lies inside the window the next
# waiting time is drawn over.
walk_replay_to <- function(handle, after, call = rlang::caller_env()) {
  walk_assert_open(handle, call)
  realizations <- walk_realizations(handle)
  done <- handle$replay_cursor %||% 0L
  last <- length(realizations$time)
  if (length(after) != 1L || is.na(after) || after < 0L || after > last) {
    cli::cli_abort(
      c(
        "{.arg after} must be one candidate event in
         {.val {0L}}:{.val {last}}.",
        "x" = "Got {.val {after}}."
      ),
      call = call,
      class = "goldfish_walk_bad_position"
    )
  }
  if (after < done) {
    cli::cli_abort(
      c(
        "{.fn walk_replay_to} cannot move the replay backward.",
        "x" = "Events up to {.val {done}} are applied; asked to stop after
               event {.val {after}}."
      ),
      call = call,
      class = "goldfish_walk_out_of_order"
    )
  }
  if (after > done) {
    replay_stamps(handle, realizations, from = done + 1L, to = after)
    handle$replay_cursor <- after
  }
  invisible(handle)
}

# The candidate's realizations as the columns replay_stamps() reads, built
# once per handle. A realization is any non-dependent row of a focal layer, a
# flavor no process models included: it writes the focal network, so two tied
# events are two positions.
walk_realizations <- function(handle) {
  if (!is.null(handle$realizations)) {
    return(handle$realizations)
  }
  schedule <- handle$schedule
  rows <- which(!schedule$dependent & schedule$layer %in% handle$focal_layers)
  time <- schedule$time[rows]
  handle$realizations <- list(
    time = time,
    layer = schedule$layer[rows],
    sender = schedule$sender[rows],
    receiver = schedule$receiver[rows],
    field = schedule$semantics[rows],
    value = schedule$value[rows],
    stamp_end = stamp_ends(time)
  )
  handle$realizations
}

# Rebind a completed joint specification to a candidate sequence: the candidate
# replaces the data at the joint level and on every process specification, and
# nothing else is recomputed. Flavor style, the mode map, the process map's
# coupling and the completed-rate pins are all fixed by `info` and `nodes` at
# construction, so a candidate differing there would silently run against a
# specification built for another dataset; only the ties may differ. Completion
# must not re-run per candidate either: a pin that moved with the candidate
# would shift the likelihood between proposals with no parameter to explain it.
bind_candidate_data <- function(spec, candidate, call = rlang::caller_env()) {
  differing <- c("info", "nodes")[
    !c(
      identical(candidate$info, spec$data$info),
      identical(candidate$nodes, spec$data$nodes)
    )
  ]
  if (length(differing) > 0L) {
    cli::cli_abort(
      c(
        "The candidate sequence does not match the specification's data in
         {.field {differing}}.",
        "i" = "A candidate may differ from the observed data only in its
               ties."
      ),
      call = call,
      class = "goldfish_candidate_mismatch"
    )
  }
  if (
    !identical(
      composition_normal_form(candidate),
      specification_composition(spec)
    )
  ) {
    cli::cli_abort(
      c(
        "The candidate sequence does not match the specification's data in
         {.field composition}.",
        "i" = "{.fn estimate_dynes} may have snapped the composition to the
               panel waves, so a fit's specification and sequences carry a
               composition the authored data does not.",
        "i" = "Pair {.code fit$spec} with the fit's sequences, or the authored
               specification with the original data."
      ),
      call = call,
      class = "goldfish_candidate_mismatch"
    )
  }
  install_spec_data(spec, candidate)
}

# The specification's composition normal form. Every candidate of a fit binds
# to the same specification, so the form is kept for the `changes` it was
# computed from: a bind of the same specification reuses it, and a different
# `changes` recomputes it. A pointer comparison decides, so a hit costs nothing.
composition_memo <- new.env(parent = emptyenv())

specification_composition <- function(spec) {
  changes <- spec$data$changes
  if (
    !exists("form", envir = composition_memo, inherits = FALSE) ||
      !identical(composition_memo$changes, changes) ||
      !identical(composition_memo$nodes, spec$data$nodes)
  ) {
    composition_memo$form <- composition_normal_form(spec$data)
    composition_memo$changes <- changes
    composition_memo$nodes <- spec$data$nodes
  }
  composition_memo$form
}

# `data` installed on the joint specification and on every process
# specification it holds, so no part keeps reading the data it replaces.
install_spec_data <- function(spec, data) {
  spec$data <- data
  spec$specifications <- lapply(spec$specifications, function(part) {
    part$data <- data
    part
  })
  spec
}

# Spread a flat free-parameter vector over the walked fids: each fid gets its
# full coefficient vector (a timed rate's leading intercept, offsets at their
# formula position), fixed slots from the layout and free slots from
# `theta[index]`. This is the same per-fid layout `set_parameters()` hands to
# simulate(). An autocompleted fid gets `numeric(0)`: its pinned intercept or
# uniform draw is supplied at evaluation time, never read from theta. Returns a
# list keyed by `as.character(fid)` in process_map order.
flat_to_fid_theta <- function(spec, theta, call = rlang::caller_env()) {
  layout <- coef_layout(spec)
  n_free <- sum(!layout$fixed)
  if (length(theta) != n_free) {
    cli::cli_abort(
      c(
        "{.arg theta} must hold one value per free coefficient of the
         specification.",
        "x" = "The layout has {n_free} free coefficient{?s}; {.arg theta} has
               {length(theta)}."
      ),
      call = call,
      class = "goldfish_theta_layout_mismatch"
    )
  }
  map <- spec$process_map
  completed <- map$completed %||% rep(FALSE, nrow(map))
  by_fid <- lapply(seq_len(nrow(map)), function(i) {
    if (isTRUE(completed[i])) {
      return(numeric(0))
    }
    slots <- layout[layout$fid == map$fid[i], ]
    values <- as.numeric(slots$value)
    free <- !slots$fixed
    values[free] <- theta[slots$index[free]]
    values
  })
  stats::setNames(by_fid, as.character(map$fid))
}

# The sub-models' theta_index positions must be exactly the layout's free
# slots, each claimed once: flat_to_fid_theta() reads theta through the
# layout's numbering, so a sub-model numbering its free effects any other way
# would evaluate the rate query at parameters the likelihood never saw.
assert_theta_layout <- function(spec, sub_models, call = rlang::caller_env()) {
  layout <- coef_layout(spec)
  expected <- layout$index[!layout$fixed]
  claimed <- unlist(lapply(sub_models, `[[`, "theta_index"), use.names = FALSE)
  missing <- setdiff(expected, claimed)
  extra <- setdiff(claimed, expected)
  repeated <- unique(claimed[duplicated(claimed)])
  if (length(missing) + length(extra) + length(repeated) > 0L) {
    cli::cli_abort(
      c(
        "The sub-models' {.field theta_index} positions do not match the
         specification's free coefficient layout.",
        "x" = if (length(missing) > 0L) {
          "No sub-model claims free {cli::qty(length(missing))}position{?s}
           {.val {missing}}."
        },
        "x" = if (length(extra) > 0L) {
          "The layout has no free slot at {cli::qty(length(extra))}position{?s}
           {.val {extra}}."
        },
        "x" = if (length(repeated) > 0L) {
          "More than one sub-model claims
           {cli::qty(length(repeated))}position{?s} {.val {repeated}}."
        },
        "i" = "Number each sub-model's free effects in {.fn coef_layout}
               order."
      ),
      call = call,
      class = "goldfish_theta_layout_mismatch"
    )
  }
  invisible(spec)
}

# The frozen intensity of one `(process, sender, receiver)` pair at the
# handle's current state. `process` is `list(layer, flavor)`; `sender` and
# `receiver` are node labels; `theta` is the per-fid list flat_to_fid_theta()
# builds; `pinned` is the specification's `completed_rates`.
#
# Every fid is evaluated through the steps a simulation draw uses, so a
# completed default -- which has no engine and which walk_evaluate() refuses --
# is evaluated at its pinned intercept or uniformly, over its sibling's live
# support. A DyNAM pair is the sender's rate times the receiver's choice
# probability, the choice evaluated for that sender's row alone; a REM rate
# already ranges over dyads. An excluded alternative returns its computed zero;
# a query naming something that is not there aborts instead.
pair_intensity <- function(
  handle,
  routing,
  theta,
  process,
  sender,
  receiver,
  pinned = list(),
  call = rlang::caller_env()
) {
  walk_assert_open(handle, call)
  map <- routing$map
  layer <- process$layer
  flavor <- process$flavor %||% NA_character_
  query <- rate_query_label(layer, flavor, sender, receiver)
  rows <- which(map$layer == layer & identical_flavor(map$flavor, flavor))
  if (length(rows) == 0L) {
    abort_unresolved_rate_query(
      query,
      "The specification carries no process
       {.val {rate_query_process(layer, flavor)}}.",
      call
    )
  }
  rate_row <- rows[map$family[rows] == "rate"]
  choice_row <- rows[map$family[rows] == "choice"]
  if (any(sub("^[^:]*:", "", map$stat_block[rows]) == "choice_coordination")) {
    abort_unresolved_rate_query(
      query,
      "Its choice is coordinated; a coordination process has no single
       sender's rate to draw a waiting time at.",
      call
    )
  }
  # The rate side's timed baseline hazard is the timed / ordered split: an
  # ordered rate ranks events and has no intensity to draw a time from.
  if (length(rate_row) == 0L || !isTRUE(map$has_intercept[rate_row])) {
    abort_unresolved_rate_query(
      query,
      "Its rate is ordered; only a timed rate gives an intensity.",
      call
    )
  }

  rate_fid <- map$fid[rate_row]
  engine <- walk_engine_of_fid(handle, rate_fid) %||%
    walk_engine_of_fid(handle, sibling_fid(map, rate_fid))
  lookup <- engine$node_lookup
  # The lookup carries a second side only for a two-mode layer.
  receiver_side <- if (any(lookup$side == 2L)) 2L else 1L
  i <- resolve_query_label(lookup, sender, 1L, "sender", query, call)
  j <- resolve_query_label(
    lookup,
    receiver,
    receiver_side,
    "receiver",
    query,
    call
  )

  t <- handle$current_time
  value_of <- function(fid, sender = NULL) {
    inputs <- fid_evaluation_inputs(
      routing,
      fid,
      default_evaluate,
      theta,
      pinned,
      t
    )
    evaluate_fid(handle, fid, inputs$theta, inputs$step, sender = sender)
  }
  rate <- value_of(rate_fid)
  if (length(choice_row) == 0L) {
    return(rate[(i - 1L) * engine$n2 + j])
  }
  rate[i] * value_of(map$fid[choice_row], sender = i)[j]
}

# The frozen pair intensities for a batch of queries against one candidate.
# `spec` is the candidate-bound completed joint specification, `theta` the
# per-fid list flat_to_fid_theta() builds, and each query is
# `list(after, layer, flavor, sender, receiver)`. Returns one intensity per
# query, in the caller's order.
#
# `after = k` is the state immediately after candidate event k's realization
# (walk_replay_to()): every row stamped before it, the node composition at its
# stamp, and the realizations at its stamp through its own, but no exogenous
# row sharing its stamp. That is the state a proposal's waiting time is drawn
# at, frozen at the preceding event; later rows lie inside the proposal
# window. Candidate events are counted in realization order, so two tied
# events are two positions. `after = 0` is the state at open.
#
# The handle only moves forward, so one handle answers the whole batch:
# positions are visited in increasing order and every query at a position is
# read there.
query_rate_at_position <- function(
  spec,
  theta,
  queries,
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  handle <- walk_open(spec, control_prep, call = call)
  routing <- simulation_routing(spec, handle)
  n_events <- length(walk_realizations(handle)$time)

  after <- vapply(
    queries,
    function(query) {
      k <- query$after
      if (
        length(k) != 1L ||
          is.na(k) ||
          k != round(k) ||
          k < 0L ||
          k > n_events
      ) {
        abort_unresolved_rate_query(
          rate_query_label(
            query$layer,
            query$flavor %||% NA_character_,
            query$sender,
            query$receiver
          ),
          "Its position {.code after = {k}} is not one of the candidate's
           {.val {0L}}:{.val {n_events}} events.",
          call
        )
      }
      as.integer(k)
    },
    integer(1)
  )

  intensity <- numeric(length(queries))
  for (k in sort(unique(after))) {
    walk_replay_to(handle, k, call = call)
    for (q in which(after == k)) {
      query <- queries[[q]]
      intensity[q] <- pair_intensity(
        handle,
        routing,
        theta,
        process = list(layer = query$layer, flavor = query$flavor),
        sender = query$sender,
        receiver = query$receiver,
        pinned = spec$completed_rates,
        call = call
      )
    }
  }
  intensity
}

rate_query_process <- function(layer, flavor) {
  if (is.na(flavor)) layer else paste0(layer, " (", flavor, ")")
}

rate_query_label <- function(layer, flavor, sender, receiver) {
  paste0(rate_query_process(layer, flavor), ": ", sender, " -> ", receiver)
}

# The local index of `label` on `side` of a layer's node lookup.
resolve_query_label <- function(lookup, label, side, role, query, call) {
  on_side <- lookup[lookup$side == side, , drop = FALSE]
  at <- if (length(label) == 1L && !is.na(label)) match(label, on_side$label)
  if (length(at) == 0L || is.na(at)) {
    abort_unresolved_rate_query(
      query,
      "The {role} {.val {label}} is not a node of the layer.",
      call
    )
  }
  on_side$local[at]
}

abort_unresolved_rate_query <- function(query, why, call) {
  # `why` interpolates the caller's variables, but the label must be this
  # `query`, whatever the caller's own `query` holds.
  envir <- new.env(parent = parent.frame())
  envir$query <- query
  cli::cli_abort(
    c(
      "Cannot answer the rate query {.val {query}}.",
      "x" = why
    ),
    call = call,
    class = "goldfish_rate_query_unresolved",
    .envir = envir
  )
}
