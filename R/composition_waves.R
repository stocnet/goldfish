# =========================================================================== #
# Composition to waves: an actor involved in an interval of the wave grid must
# be present for every event and flip drawn in it. This file finds who is
# involved; the rewrite of the presence path builds on it.
# =========================================================================== #

# The rows of `ties` a process of `map` models: the layer has a process, and
# either that process covers every flavor or the row's flavor is one of its
# flavors. Shared with the sim augmenter so the two cannot drift apart.
modeled_tie_rows <- function(ties, map) {
  flavor <- ties[["flavor"]] %||% rep(NA_character_, nrow(ties))
  vapply(
    seq_len(nrow(ties)),
    function(r) {
      layer_flavors <- map$flavor[map$layer == ties$layer[[r]]]
      length(layer_flavors) > 0L &&
        (anyNA(layer_flavors) || flavor[[r]] %in% layer_flavors)
    },
    logical(1)
  )
}

# The global node ids involved in each interval k = (w_{k-1}, w_k] of
# `wave_times`: the sender and receiver of a modeled relational event, and the
# `from` and `to` of a flip of a modeled panel layer. An exogenous relational
# layer is replayed, not selected, so its actors are not involved. A modeled
# panel layer's own rows are wave snapshots, not events; its flips come from
# the wave diff, whose `from`/`to` are already global ids on the layer's own
# node sides. Reads neither the recipe nor a completed specification.
composition_involvement <- function(
  spec,
  wave_times,
  call = rlang::caller_env()
) {
  data <- spec$data
  wave_times <- coerce_time(wave_times)
  check_wave_grid(wave_times, call = call)
  n_intervals <- length(wave_times) - 1L
  labels <- as.data.frame(data$nodes)$label

  ties <- as.data.frame(data$ties)
  ties <- ties[
    modeled_tie_rows(ties, spec$process_map) &
      !ties$layer %in% spec$modeled_panel &
      !is.na(ties$time),
    ,
    drop = FALSE
  ]
  interval <- findInterval(coerce_time(ties$time), wave_times, left.open = TRUE)
  inside <- interval >= 1L & interval <= n_intervals
  actors <- data.frame(
    interval = rep(interval[inside], 2L),
    node = c(
      node_index(ties$from[inside], labels),
      node_index(ties$to[inside], labels)
    )
  )

  for (layer in spec$modeled_panel) {
    flips <- diff_panel_waves(data, layer, wave_times, call = call)$flips
    actors <- rbind(
      actors,
      data.frame(
        interval = c(flips$interval, flips$interval),
        node = c(flips$from, flips$to)
      )
    )
  }

  lapply(seq_len(n_intervals), function(k) {
    sort(unique(actors$node[actors$interval == k]))
  })
}
