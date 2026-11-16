# =========================================================================== #
# Wave diffing: a panel-observed layer's consecutive wave snapshots into the
# candidate flip set of each modeled interval.
#
# The wave grid starts at the observation start t0, so M waves give M modeled
# intervals over the M + 1 boundaries t0 < t1 < ... < tM: the first wave closes
# a modeled interval whose entering state is the initial condition, and no
# wave is ever history.
#
# Two panel kinds diff differently. A flavored state panel records the tie
# state, so a change is one flip named by its flavor. An unflavored count panel
# records the cumulative count of a monotone process, so a dyad whose count
# rises by Delta carries Delta indistinguishable, unflavored events; a count
# cannot be un-made, which is what makes that diff exact.
#
# A snapshot is inclusive at its wave: a change-list row stamped exactly at a
# wave time is part of that wave's snapshot, and so a flip of the interval
# ending there. The pinned rate of a completed panel process reads the same
# snapshots (`panel_wave_risk_set()`), so a pinned period and the interval it
# pins always count the same flips.
# =========================================================================== #

# The layer's network stream and node-space facts, built once so each wave's
# snapshot is one vectorized fold over it.
panel_layer_context <- function(data, layer, call = rlang::caller_env()) {
  info <- data$info %||% list()
  nodes <- as.data.frame(data$nodes)
  layers <- unique(as.data.frame(data$ties)$layer)
  if (!layer %in% layers) {
    cli::cli_abort(
      c(
        "{.arg layer} must name a layer in {.arg data}.",
        "x" = "{.val {layer}} is not among {.val {layers}}."
      ),
      call = call
    )
  }
  mode_map <- build_mode_map(info, nodes, layers)
  lm <- mode_map$layers[[layer]]
  list(
    layer = layer,
    stream = split_stocnet_streams(data, mode_map)$network[[layer]],
    update = unname(info$update[layer]),
    # Only a symmetric layer is mirrored: a two-mode matrix has no symmetric
    # counterpart, whatever `info$directed` declares.
    symmetric = lm$symmetric,
    one_mode = !isTRUE(lm$is_two_mode),
    side1 = lm$side1,
    side2 = lm$side2,
    n1 = lm$n1,
    n2 = lm$n2
  )
}

# The layer's state at a wave: its history rows plus every timed row stamped at
# or before the wave.
panel_wave_snapshot <- function(context, time) {
  stream <- context$stream
  keep <- is.na(stream$time) | stream$time <= time
  materialize_network_state(
    stream[keep, , drop = FALSE],
    n1 = context$n1,
    n2 = context$n2,
    directed = !context$symmetric
  )
}

# The dyads a flip can sit on: every cell of a two-mode layer, the off-diagonal
# of a directed one-mode layer, the upper triangle of an undirected one.
panel_dyad_mask <- function(context) {
  mask <- matrix(TRUE, context$n1, context$n2)
  if (context$one_mode) {
    mask <- if (context$symmetric) {
      row(mask) < col(mask)
    } else {
      row(mask) != col(mask)
    }
  }
  mask
}

check_wave_grid <- function(wave_times, call) {
  if (
    !is.numeric(wave_times) ||
      length(wave_times) < 2L ||
      anyNA(wave_times) ||
      any(diff(wave_times) <= 0)
  ) {
    cli::cli_abort(
      c(
        "{.arg wave_times} must be at least two strictly increasing
         boundaries.",
        "i" = "Start the grid at the observation start and list every wave
               after it."
      ),
      class = "goldfish_invalid_wave_grid",
      call = call
    )
  }
  invisible(wave_times)
}

# A grid must share the time class of the rows it is matched against.
# coerce_time() counts days for a Date and seconds for a POSIXct, and no
# conversion between them is safe without picking a time zone for midnight,
# so a grid in another class would sit nowhere near the rows. Integer and
# double are one class; a POSIXct zone changes only the printing.
check_grid_time_class <- function(
  wave_times,
  time_column,
  layer = NULL,
  call = rlang::caller_env()
) {
  grid_class <- time_class_kind(wave_times)
  column_class <- time_class_kind(time_column)
  if (identical(grid_class, column_class)) {
    return(invisible(wave_times))
  }
  column <- if (is.null(layer)) {
    "the {.field time} column"
  } else {
    "the {.field time} column of layer {.val {layer}}"
  }
  # The unit explanation holds only between time classes. Anything else, a
  # character grid above all, is not parsed: coerce_time() would read text as
  # GMT, a zone the user never chose.
  time_kinds <- c("numeric", "POSIXct", "Date")
  hint <- if (all(c(grid_class, column_class) %in% time_kinds)) {
    "Give the grid as {.cls {column_class}}: a {.cls Date} counts days and a
     {.cls POSIXct} seconds, so mixed classes compare different units."
  } else {
    "Convert the grid to {.cls {column_class}} first; a {.cls {grid_class}}
     grid is never parsed."
  }
  cli::cli_abort(
    c(
      "{.arg wave_times} must have the time class of the rows it marks.",
      "x" = paste0(
        "{.arg wave_times} is {.cls {grid_class}}, but ",
        column,
        " is {.cls {column_class}}."
      ),
      "i" = hint
    ),
    class = "goldfish_wave_grid_time_class",
    call = call
  )
}

time_class_kind <- function(time) {
  if (inherits(time, "POSIXct")) {
    "POSIXct"
  } else if (inherits(time, "Date")) {
    "Date"
  } else if (is.numeric(time)) {
    "numeric"
  } else {
    class(time)[[1L]]
  }
}

# The (from-value, to-value) -> flavor map of a flavored state layer, read off
# the flavor mapping add_flavor() records: under `increment` a flavor's value
# is the step, under `replace` the state it sets. The flavor is never latent,
# so no two flavors may share a transition.
panel_transition_map <- function(data, layer, call) {
  mapping <- data$info$values_equivalence[[layer]]
  # An unflavored panel layer is a count panel: no flavor to map.
  if (is.null(mapping)) {
    return(NULL)
  }
  update <- unname(data$info$update[layer])
  to_value <- if (identical(update, "replace")) {
    unname(mapping)
  } else {
    as.numeric(unname(mapping) > 0)
  }
  transitions <- data.frame(
    from_value = 1 - to_value,
    to_value = to_value,
    flavor = names(mapping),
    stringsAsFactors = FALSE
  )
  check_flavor_map_injective(transitions, layer, call = call)
}

check_flavor_map_injective <- function(transitions, layer, call) {
  key <- paste(transitions$from_value, transitions$to_value)
  if (anyDuplicated(key) > 0L) {
    cli::cli_abort(
      c(
        "The flavor mapping of layer {.val {layer}} is not injective.",
        "x" = "Two flavors share a state transition, so a wave change could
               not name its flavor."
      ),
      class = "goldfish_noninjective_flavor_map",
      call = call
    )
  }
  transitions
}

check_wave_snapshots <- function(snapshots, wave_times, layer, states, call) {
  incomplete <- which(vapply(snapshots, anyNA, logical(1)))
  if (length(incomplete) > 0L) {
    waves <- wave_times[incomplete]
    cli::cli_abort(
      c(
        "Wave diffing needs complete snapshots.",
        "x" = "Layer {.val {layer}} has missing ({.val {NA}}) tie values at
               {cli::qty(length(waves))}wave{?s} {.val {waves}}.",
        "i" = "A missing value is not a tie absence; complete the waves before
               estimating. Partially observed waves are not supported."
      ),
      class = "goldfish_incomplete_wave_snapshot",
      call = call
    )
  }
  if (is.null(states)) {
    return(check_count_snapshots(snapshots, wave_times, layer, call = call))
  }
  invalid <- which(vapply(
    snapshots,
    function(s) !all(s %in% states),
    logical(1)
  ))
  if (length(invalid) > 0L) {
    waves <- wave_times[invalid]
    cli::cli_abort(
      c(
        "A state panel's snapshots must take the states of its flavor
         mapping.",
        "x" = "Layer {.val {layer}} reaches a state outside {.val {states}} at
               {cli::qty(length(waves))}wave{?s} {.val {waves}}.",
        "i" = "A dichotomous layer holds each tie at {.val {0}} or
               {.val {1}}."
      ),
      class = "goldfish_invalid_wave_state",
      call = call
    )
  }
  invisible(snapshots)
}

# A count panel's snapshots hold whole non-negative counts that never fall
# from one wave to the next.
check_count_snapshots <- function(snapshots, wave_times, layer, call) {
  invalid <- which(vapply(
    snapshots,
    function(s) any(s < 0 | s != round(s)),
    logical(1)
  ))
  if (length(invalid) > 0L) {
    waves <- wave_times[invalid]
    cli::cli_abort(
      c(
        "A count panel's snapshots must hold whole non-negative counts.",
        "x" = "Layer {.val {layer}} holds another value at
               {cli::qty(length(waves))}wave{?s} {.val {waves}}."
      ),
      class = "goldfish_invalid_wave_state",
      call = call
    )
  }
  decreasing <- which(vapply(
    seq_len(length(snapshots) - 1L),
    function(k) any(snapshots[[k + 1L]] < snapshots[[k]]),
    logical(1)
  ))
  if (length(decreasing) > 0L) {
    waves <- wave_times[decreasing + 1L]
    cli::cli_abort(
      c(
        "A count panel's cumulative counts cannot decrease.",
        "x" = "Layer {.val {layer}} records a lower count at
               {cli::qty(length(waves))}wave{?s} {.val {waves}} than at the
               wave before.",
        "i" = "A layer whose ties are dissolved is a state panel: mark its
               creation and dissolution flavors with {.fn add_flavor}."
      ),
      class = "goldfish_decreasing_count_panel",
      call = call
    )
  }
  invisible(snapshots)
}

#' Diff a panel layer's waves into candidate flip sets
#'
#' Reads the wave snapshots of a panel layer and returns, for every modeled
#' interval between consecutive boundaries, the events an endpoint-hitting
#' sequence must place in it: one flip per changed dyad of a flavored state
#' panel, or as many unflavored events as a dyad's cumulative count rises on an
#' unflavored count panel.
#'
#' @param data a validated stocnet object.
#' @param layer the panel-observed layer to diff.
#' @param wave_times the wave grid: the observation start followed by every
#'   wave, strictly increasing.
#' @param call the calling environment for error reporting.
#'
#' @return a `goldfishWaveDiff`: `layer`, `kind` (`"state"` or `"count"`),
#'   `wave_times`, `transitions` (the injective (from-value, to-value) ->
#'   flavor map; `NULL` for a count panel), `snapshots` (one state matrix per
#'   boundary, local indices), `flips` (one row per event: `interval`, global
#'   node indices `from`/`to`, `flavor` (`NA` on a count panel), `from_value`,
#'   `to_value`, the per-dyad `chain` id and its `step`), and `counts` (events
#'   per interval).
#' @noRd
diff_panel_waves <- function(
  data,
  layer,
  wave_times,
  call = rlang::caller_env()
) {
  validate_goldfish_data(data, call = call)
  wave_times <- coerce_time(wave_times)
  check_wave_grid(wave_times, call = call)
  transitions <- panel_transition_map(data, layer, call = call)
  kind <- if (is.null(transitions)) "count" else "state"
  context <- panel_layer_context(data, layer, call = call)
  snapshots <- lapply(wave_times, function(w) {
    panel_wave_snapshot(context, w)
  })
  states <- if (!is.null(transitions)) {
    sort(unique(c(transitions$from_value, transitions$to_value)))
  }
  check_wave_snapshots(snapshots, wave_times, layer, states, call = call)

  mask <- panel_dyad_mask(context)
  flips <- lapply(seq_len(length(wave_times) - 1L), function(k) {
    prev <- snapshots[[k]]
    curr <- snapshots[[k + 1L]]
    cells <- which(mask & prev != curr, arr.ind = TRUE)
    cells <- cells[order(cells[, 1L], cells[, 2L]), , drop = FALSE]
    # A dichotomous state changes by one step, so its chain is one flip long;
    # a count rising by Delta is a Delta-step chain of unit events.
    n_steps <- if (kind == "count") curr[cells] - prev[cells] else 1
    n_steps <- rep_len(as.integer(n_steps), nrow(cells))
    at <- rep(seq_len(nrow(cells)), n_steps)
    step <- sequence(n_steps)
    from_value <- if (kind == "count") {
      prev[cells][at] + step - 1
    } else {
      prev[cells]
    }
    to_value <- if (kind == "count") from_value + 1 else curr[cells]
    flavor <- if (kind == "count") {
      rep(NA_character_, length(at))
    } else {
      transitions$flavor[match(
        paste(from_value, to_value),
        paste(transitions$from_value, transitions$to_value)
      )]
    }
    data.frame(
      interval = rep(k, length(at)),
      from = as.integer(context$side1[cells[at, 1L]]),
      to = as.integer(context$side2[cells[at, 2L]]),
      flavor = flavor,
      from_value = from_value,
      to_value = to_value,
      dyad = at,
      step = step,
      stringsAsFactors = FALSE
    )
  })
  flips <- do.call(rbind, flips)
  flips$interval <- as.integer(flips$interval)
  # The chain id keys each dyad's events within its interval.
  flips$chain <- cumsum(!duplicated(flips[c("interval", "dyad")]))
  flips <- flips[c(
    "interval",
    "from",
    "to",
    "flavor",
    "from_value",
    "to_value",
    "chain",
    "step"
  )]
  rownames(flips) <- NULL
  counts <- tabulate(flips$interval, nbins = length(wave_times) - 1L)

  structure(
    list(
      layer = layer,
      kind = kind,
      wave_times = wave_times,
      transitions = transitions,
      snapshots = snapshots,
      flips = flips,
      counts = counts
    ),
    class = "goldfishWaveDiff"
  )
}

# Assert that an augmented sequence hits every wave snapshot of `diff`: its
# state at the observation start is the observed one, every one of the layer's
# timed rows after it falls strictly inside an interval, and replaying each
# interval's rows in time order from the interval's entering snapshot places
# exactly that interval's flips, each at its chain step. That is a minimal-flip
# path, so an excursion (a net-zero pair) fails too. Used by tests and the
# opt-in debug path, never per draw on the hot path.
validate_endpoint_hitting <- function(
  sequence,
  diff,
  call = rlang::caller_env()
) {
  context <- panel_layer_context(sequence, diff$layer, call = call)
  waves <- diff$wave_times
  n_intervals <- length(waves) - 1L
  labels <- as.data.frame(sequence$nodes)$label
  problems <- character(0)

  if (
    !identical(panel_wave_snapshot(context, waves[1L]), diff$snapshots[[1L]])
  ) {
    problems <- c(
      problems,
      cli::format_inline("The state at the observation start differs.")
    )
  }
  stream <- context$stream
  timed <- stream[!is.na(stream$time) & stream$time > waves[1L], , drop = FALSE]
  interval <- findInterval(timed$time, waves, left.open = TRUE)
  outside <- timed$time %in% waves | interval < 1L | interval > n_intervals
  if (any(outside)) {
    problems <- c(
      problems,
      cli::format_inline(
        "{sum(outside)} event{?s} {?falls/fall} on a wave time or outside the
         grid, not strictly inside an interval."
      )
    )
  }

  flips <- diff$flips
  flips$i <- match(flips$from, context$side1)
  flips$j <- match(flips$to, context$side2)
  for (k in seq_len(n_intervals)) {
    state <- diff$snapshots[[k]]
    pending <- flips[flips$interval == k, , drop = FALSE]
    pending <- pending[order(pending$chain, pending$step), , drop = FALSE]
    rows <- timed[!outside & interval == k, , drop = FALSE]
    rows <- rows[order(rows$time), , drop = FALSE]
    for (r in seq_len(nrow(rows))) {
      i <- rows$from[r]
      j <- rows$to[r]
      if (context$symmetric && i > j) {
        swap <- i
        i <- j
        j <- swap
      }
      from_value <- state[i, j]
      to_value <- if (identical(context$update, "replace")) {
        rows$value[r]
      } else {
        from_value + rows$value[r]
      }
      at <- which(pending$i == i & pending$j == j)[1L]
      if (
        is.na(at) ||
          pending$from_value[at] != from_value ||
          pending$to_value[at] != to_value
      ) {
        problems <- c(
          problems,
          cli::format_inline(
            "Interval {k} places an event on {.val {labels[context$side1[i]]}}
             -> {.val {labels[context$side2[j]]}} outside its flip set."
          )
        )
        next
      }
      pending <- pending[-at, , drop = FALSE]
      state[i, j] <- to_value
      if (context$symmetric) {
        state[j, i] <- to_value
      }
    }
    if (nrow(pending) > 0L) {
      problems <- c(
        problems,
        cli::format_inline(
          "Interval {k} leaves {nrow(pending)} flip{?s} unplaced."
        )
      )
    }
  }

  if (length(problems) > 0L) {
    cli::cli_abort(
      c(
        "The sequence does not hit the wave snapshots of layer
         {.val {diff$layer}}.",
        # Each problem is already formatted; interpolating it by reference
        # keeps a label containing a brace from being read as markup.
        rlang::set_names(
          sprintf("{problems[[%d]]}", seq_along(problems)),
          rep("x", length(problems))
        )
      ),
      class = "goldfish_endpoint_violation",
      call = call
    )
  }
  invisible(sequence)
}
