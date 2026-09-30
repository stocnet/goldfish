# =========================================================================== #
# Wave diffing: a panel-observed layer's consecutive wave snapshots into the
# candidate flip set of each modeled interval.
#
# The wave grid starts at the observation start t0, so M waves give M modeled
# intervals over the M + 1 boundaries t0 < t1 < ... < tM: the first wave closes
# a modeled interval whose entering state is the initial condition, and no
# wave is ever history.
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
    directed = isTRUE(unname(info$directed[layer])),
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
    directed = context$directed
  )
}

# The dyads a flip can sit on: every cell of a two-mode layer, the off-diagonal
# of a directed one-mode layer, the upper triangle of an undirected one.
panel_dyad_mask <- function(context) {
  mask <- matrix(TRUE, context$n1, context$n2)
  if (context$one_mode) {
    mask <- if (context$directed) {
      row(mask) != col(mask)
    } else {
      row(mask) < col(mask)
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

# The (from-value, to-value) -> flavor map of a flavored state layer, read off
# the flavor mapping add_flavor() records: under `increment` a flavor's value
# is the step, under `replace` the state it sets. The flavor is never latent,
# so no two flavors may share a transition.
panel_transition_map <- function(data, layer, call) {
  mapping <- data$info$values_equivalence[[layer]]
  if (is.null(mapping)) {
    cli::cli_abort(
      c(
        "Wave diffing needs a flavored state panel layer.",
        "x" = "Layer {.val {layer}} carries no flavor mapping.",
        "i" = "Mark its creation and dissolution flavors with
               {.fn add_flavor}."
      ),
      class = "goldfish_unsupported_panel_kind",
      call = call
    )
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
  key <- paste(transitions$from_value, transitions$to_value)
  if (anyDuplicated(key) > 0L) {
    cli::cli_abort(
      c(
        "The flavor mapping of layer {.val {layer}} is not injective.",
        "x" = "Two flavors share a state transition, so a wave change could
               not name its flavor."
      ),
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

#' Diff a panel layer's waves into candidate flip sets
#'
#' Reads the wave snapshots of a flavored state panel layer and returns, for
#' every modeled interval between consecutive boundaries, the flips an
#' endpoint-hitting sequence must place in it.
#'
#' @param data a validated stocnet object.
#' @param layer the panel-observed layer to diff.
#' @param wave_times the wave grid: the observation start followed by every
#'   wave, strictly increasing.
#' @param call the calling environment for error reporting.
#'
#' @return a `goldfishWaveDiff`: `layer`, `wave_times`, `transitions` (the
#'   injective (from-value, to-value) -> flavor map), `snapshots` (one state
#'   matrix per boundary, local indices), `flips` (one row per flip: `interval`,
#'   global node indices `from`/`to`, `flavor`, `from_value`, `to_value`, the
#'   per-dyad `chain` id and its `step`), and `counts` (flips per interval).
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
  context <- panel_layer_context(data, layer, call = call)
  snapshots <- lapply(wave_times, function(w) {
    panel_wave_snapshot(context, w)
  })
  states <- sort(unique(c(transitions$from_value, transitions$to_value)))
  check_wave_snapshots(snapshots, wave_times, layer, states, call = call)

  mask <- panel_dyad_mask(context)
  flips <- lapply(seq_len(length(wave_times) - 1L), function(k) {
    prev <- snapshots[[k]]
    curr <- snapshots[[k + 1L]]
    cells <- which(mask & prev != curr, arr.ind = TRUE)
    cells <- cells[order(cells[, 1L], cells[, 2L]), , drop = FALSE]
    from_value <- prev[cells]
    to_value <- curr[cells]
    flavor <- transitions$flavor[match(
      paste(from_value, to_value),
      paste(transitions$from_value, transitions$to_value)
    )]
    data.frame(
      interval = rep(k, nrow(cells)),
      from = as.integer(context$side1[cells[, 1L]]),
      to = as.integer(context$side2[cells[, 2L]]),
      flavor = flavor,
      from_value = from_value,
      to_value = to_value,
      stringsAsFactors = FALSE
    )
  })
  flips <- do.call(rbind, flips)
  flips$interval <- as.integer(flips$interval)
  # A dichotomous state changes by one step, so every chain is one flip long;
  # the chain id still keys each dyad's flips within its interval.
  flips$chain <- seq_len(nrow(flips))
  flips$step <- rep(1L, nrow(flips))
  counts <- tabulate(flips$interval, nbins = length(wave_times) - 1L)

  structure(
    list(
      layer = layer,
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
      if (!context$directed && context$one_mode && i > j) {
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
      if (!context$directed) {
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
