# =========================================================================== #
# The augmentation_recipe: the theta-free, sequence-free plan every augmenter
# reads, compiled once at estimate_dynes() entry.
#
# Everything here depends only on the observed wave snapshots, so it is the
# same for every EM iteration and every drawn sequence: the wave diff of each
# modeled panel layer (its flavor map validated once, in the diff), the
# per-dyad chains, the pair graph a permute move enumerates, and the state of
# every layer at every wave. An augmenter holds a small mutable cursor over the
# plan and never writes to it. Live support applicability is not in the plan:
# it comes from the walk handle at the current state, so there is one support
# view, not two that could disagree.
# =========================================================================== #

# The wave grid the recipe is compiled on: `wave_times` when given, else the
# distinct times of the modeled panel layers' rows, opened by `start_time`
# when one is set (earlier rows are then history) and otherwise by the first
# recorded wave. Only modeled panel layers set the grid: a boundary taken from
# an exogenous panel covariate would sit where the modeled layer records no
# snapshot, and would force its flips to one side of it.
panel_wave_grid <- function(
  spec,
  start_time = NULL,
  wave_times = NULL,
  call = rlang::caller_env()
) {
  if (!is.null(wave_times)) {
    for (layer in spec$modeled_panel) {
      check_grid_time_class(
        wave_times,
        layer_event_times(spec$data, layer),
        layer = layer,
        call = call
      )
    }
    wave_times <- coerce_time(wave_times)
    check_wave_grid(wave_times, call = call)
    return(wave_times)
  }
  times <- unlist(lapply(spec$modeled_panel, function(layer) {
    coerce_time(layer_event_times(spec$data, layer))
  }))
  times <- sort(unique(times))
  if (!is.null(start_time)) {
    start_time <- coerce_time(start_time)
    times <- c(start_time, times[times > start_time])
  }
  if (length(times) < 2L) {
    cli::cli_abort(
      c(
        "The panel data gives no interval to augment.",
        "x" = "The modeled panel {cli::qty(length(spec$modeled_panel))}
               layer{?s} {.val {spec$modeled_panel}} {?records/record} a
               single wave and no observation start precedes it.",
        "i" = "Set a {.arg start_time} before the first wave in
               {.fn set_preprocessing}, or pass {.arg wave_times}."
      ),
      class = "goldfish_invalid_wave_grid",
      call = call
    )
  }
  times
}

#' Compile the augmentation recipe of a joint specification
#'
#' @param spec a `goldfishJointSpec` with at least one modeled panel layer.
#' @param wave_times the wave grid, from [panel_wave_grid()].
#' @param call the calling environment for error reporting.
#'
#' @return a `goldfishAugRecipe`: `wave_times`; `layers`, per modeled panel
#'   layer its `kind` (`"state"` or `"count"`), `transitions` (the flavor map,
#'   `NULL` on a count panel) and `support` (the support kind each flavor's
#'   derived constraint imposes, `"none"` without one); `events`, one row per
#'   latent event (`event` id, `layer`, `interval`, `from`, `to`, `flavor`,
#'   `from_value`, `to_value`, `chain`, `step`), contiguous per chain in step
#'   order; `chains`, one row per per-dyad chain (`chain`, `layer`,
#'   `interval`, `from`, `to`, `length` -- the chain's multiplicity -- and the
#'   event id of its `first` step); `pairs`, per interval a two-column matrix
#'   of the event pairs a permute move may swap (events of one chain never
#'   pair: on a state panel the swap would break chain order, on a count panel
#'   it is an identity); and `start_states`, per wave boundary the state of
#'   every network layer, inclusive at the wave.
#' @noRd
compile_augmentation_recipe <- function(
  spec,
  wave_times,
  call = rlang::caller_env()
) {
  data <- spec$data
  panel <- spec$modeled_panel
  diffs <- lapply(panel, function(layer) {
    diff_panel_waves(data, layer, wave_times, call = call)
  })
  names(diffs) <- panel
  wave_times <- diffs[[1L]]$wave_times

  layers <- lapply(panel, function(layer) {
    diff <- diffs[[layer]]
    flavors <- diff$transitions$flavor %||% NA_character_
    support <- vapply(
      flavors,
      function(flavor) flavor_support_kind(spec_of_layer(spec, layer), flavor),
      character(1)
    )
    names(support) <- diff$transitions$flavor %||% layer
    list(kind = diff$kind, transitions = diff$transitions, support = support)
  })
  names(layers) <- panel

  events <- recipe_events(diffs)
  list(
    wave_times = wave_times,
    layers = layers,
    events = events,
    chains = recipe_chains(events),
    pairs = recipe_pairs(events, length(wave_times) - 1L),
    start_states = recipe_start_states(data, wave_times, call = call)
  ) |>
    structure(class = "goldfishAugRecipe")
}

# Every modeled panel layer's latent events in one table, ordered by interval,
# layer, chain and step, with chain ids unique across layers.
recipe_events <- function(diffs) {
  offset <- 0L
  parts <- lapply(names(diffs), function(layer) {
    flips <- diffs[[layer]]$flips
    flips$chain <- flips$chain + offset
    offset <<- offset + nrow(flips)
    cbind(layer = rep(layer, nrow(flips)), flips, stringsAsFactors = FALSE)
  })
  events <- do.call(rbind, parts)
  layer_order <- match(events$layer, names(diffs))
  events <- events[
    order(events$interval, layer_order, events$chain, events$step),
    ,
    drop = FALSE
  ]
  events$chain <- match(events$chain, unique(events$chain))
  events <- cbind(event = seq_len(nrow(events)), events)
  rownames(events) <- NULL
  events
}

recipe_chains <- function(events) {
  first <- !duplicated(events$chain)
  chains <- events[first, c("chain", "layer", "interval", "from", "to")]
  chains$length <- tabulate(events$chain, nbins = nrow(chains))
  chains$first <- events$event[first]
  rownames(chains) <- NULL
  chains
}

recipe_pairs <- function(events, n_intervals) {
  lapply(seq_len(n_intervals), function(k) {
    ids <- events$event[events$interval == k]
    if (length(ids) < 2L) {
      return(matrix(
        integer(0),
        0L,
        2L,
        dimnames = list(NULL, c("first", "second"))
      ))
    }
    pairs <- t(utils::combn(ids, 2L))
    pairs <- pairs[
      events$chain[pairs[, 1L]] != events$chain[pairs[, 2L]],
      ,
      drop = FALSE
    ]
    storage.mode(pairs) <- "integer"
    dimnames(pairs) <- list(NULL, c("first", "second"))
    pairs
  })
}

# Every network layer's state at every wave boundary, read with the inclusive
# snapshot the wave diff reads. Every augmented sequence hits every wave, so
# these are shared by all of them.
recipe_start_states <- function(data, wave_times, call) {
  layers <- unique(as.data.frame(data$ties)$layer)
  contexts <- lapply(layers, function(layer) {
    panel_layer_context(data, layer, call = call)
  })
  names(contexts) <- layers
  lapply(wave_times, function(w) {
    lapply(contexts, panel_wave_snapshot, time = w)
  })
}

# The mutable per-draw state an augmenter holds over a recipe: how many events
# each chain has left to place. `take(chain)` hands out the chain's next event
# id, so chain order holds by construction; `remaining(interval)` lists the
# chains with events left; `heads(interval)` is the next event id of each of
# them, in the same order; `done(interval)` says none are; `reset()` starts a
# new draw. The recipe itself is never written.
new_recipe_cursor <- function(recipe) {
  chains <- recipe$chains
  left <- chains$length
  remaining <- function(interval = NULL) {
    keep <- left > 0L
    if (!is.null(interval)) {
      keep <- keep & chains$interval %in% interval
    }
    data.frame(
      chain = chains$chain[keep],
      interval = chains$interval[keep],
      left = left[keep]
    )
  }
  structure(
    list(
      recipe = recipe,
      reset = function() {
        left <<- chains$length
        invisible(NULL)
      },
      remaining = remaining,
      heads = function(interval) {
        keep <- which(left > 0L & chains$interval %in% interval)
        chains$first[keep] + chains$length[keep] - left[keep]
      },
      take = function(chain) {
        at <- match(chain, chains$chain)
        if (length(at) != 1L || is.na(at) || left[at] == 0L) {
          cli::cli_abort(
            "Chain {.val {chain}} has no event left to place.",
            class = "goldfish_recipe_cursor_exhausted"
          )
        }
        step <- chains$length[at] - left[at]
        left[at] <<- left[at] - 1L
        chains$first[at] + step
      },
      done = function(interval = NULL) nrow(remaining(interval)) == 0L
    ),
    class = "goldfishRecipeCursor"
  )
}
