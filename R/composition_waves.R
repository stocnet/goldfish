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

# The joint specification with its composition snapped to `wave_times`, on the
# joint data and on every process specification's data. It runs before
# completion, because a completed relational rate is pinned from a risk set
# that reads presence: pinning on the original composition while the target
# walks the snapped one would make the target read two compositions. When
# nothing is snapped the specification comes back untouched, process data
# included.
snap_composition_to_waves <- function(
  spec,
  wave_times,
  call = rlang::caller_env()
) {
  involved <- composition_involvement(spec, wave_times, call = call)
  data <- rewrite_composition(spec$data, involved, wave_times)
  if (identical(data, spec$data)) {
    return(spec)
  }
  install_spec_data(spec, data)
}

# Rewrites the composition (`active` changes) of `data` so that an actor
# involved in an interval of `wave_times` is present over the whole interval.
# `involved` holds one vector of global ids per interval k = (w_{k-1}, w_k],
# as composition_involvement() returns. With S(t) an actor's real presence
# (its initial `active`, then every real change at or before t), the rewritten
# presence P is
#   inside interval k:      TRUE when involved in k, otherwise S(t);
#   at a wave w_k:          TRUE when involved in k + 1, otherwise S(w_k);
#   before w_0, after w_K:  S(t).
# P is right-continuous like S, so a change list produces it: the actor's rows
# in [w_0, w_K] are replaced by the minimal one. Rebuilding the path from times,
# rather than moving each row to a wave, keeps a leave-and-rejoin inside an
# involved interval from sorting into a final leave at w_k, and makes the
# result independent of the order of rows sharing a stamp. `nodes` is never
# edited: the initial presence stays the real one.
rewrite_composition <- function(data, involved, wave_times) {
  wave_raw <- wave_times
  wave_times <- coerce_time(wave_times)
  actors <- sort(unique(unlist(involved, use.names = FALSE)))
  if (length(actors) == 0L) {
    return(data)
  }
  nodes <- as.data.frame(data$nodes)
  initial <- if (is.null(nodes$active)) {
    rep(TRUE, nrow(nodes))
  } else {
    as.logical(nodes$active)
  }

  changes <- data$changes
  has_changes <- !is.null(changes) && nrow(changes) > 0L
  if (has_changes) {
    ch <- as.data.frame(changes)
    ch_time <- coerce_time(ch$time)
    ch_node <- to_global_id(ch$node, nodes)
    composition <- ch$var == "active" & !is.na(ch_time)
    ch_value <- rep(NA, nrow(ch))
    ch_value[composition] <- as.logical(unlist(
      unwrap_values(ch$value[composition]),
      use.names = FALSE
    ))
  } else {
    ch_time <- numeric()
    ch_node <- integer()
    composition <- logical()
    ch_value <- logical()
  }
  first <- wave_times[[1L]]
  last <- wave_times[[length(wave_times)]]
  in_window <- composition & ch_time >= first & ch_time <= last

  drop <- logical(length(ch_time))
  new_time <- vector("list", length(actors))
  new_node <- vector("list", length(actors))
  new_value <- vector("list", length(actors))
  snapped <- vector("list", length(actors))
  for (i in seq_along(actors)) {
    actor <- actors[[i]]
    own <- which(composition & ch_node == actor)
    own <- own[order(ch_time[own])]
    flags <- vapply(involved, function(ids) actor %in% ids, logical(1))
    path <- presence_paths(
      ch_time[own],
      ch_value[own],
      initial[[actor]],
      flags,
      wave_times
    )
    if (identical(path$rewritten, path$real)) {
      next
    }
    drop[in_window & ch_node == actor] <- TRUE
    snapped[[i]] <- snapped_intervals(
      path,
      ch_time[own],
      ch_value[own],
      initial[[actor]],
      flags,
      wave_times
    )
    snapped[[i]]$actor <- rep(actor, nrow(snapped[[i]]))
    new_time[[i]] <- path$rewritten$time
    new_node[[i]] <- rep(actor, length(path$rewritten$time))
    new_value[[i]] <- path$rewritten$value
  }
  if (!any(drop) && all(lengths(new_time) == 0L)) {
    return(data)
  }

  new_time <- unlist(new_time, use.names = FALSE)
  new_node <- unlist(new_node, use.names = FALSE)
  new_value <- unlist(new_value, use.names = FALSE)
  warn_composition_snapped(do.call(rbind, snapped), nodes$label, wave_times)
  data$changes <- splice_composition(
    changes,
    drop,
    restore_time_class(new_time, if (has_changes) changes$time else wave_raw),
    if (has_changes && is.character(changes$node)) {
      nodes$label[new_node]
    } else {
      new_node
    },
    new_value
  )
  data
}

# One actor's real and rewritten presence inside [w_0, w_K], each as the
# minimal (time, value) change list from the state just before w_0. `time`
# and `value` are the actor's real changes in time order (stable, so the last
# row of a stamp wins); `involved` flags the intervals 1..K the actor is
# involved in.
presence_paths <- function(time, value, initial, involved, wave_times) {
  n_intervals <- length(wave_times) - 1L
  state_at <- function(t) {
    idx <- findInterval(t, time)
    c(initial, value)[idx + 1L]
  }
  before <- findInterval(wave_times[[1L]], time, left.open = TRUE)
  start <- c(initial, value)[before + 1L]

  inside <- time >= wave_times[[1L]] & time <= wave_times[[n_intervals + 1L]]
  real_time <- unique(time[inside])
  real <- minimal_changes(real_time, state_at(real_time), start)

  # Wave w_k sits at index k + 1 and is forced present by interval k + 1.
  wave_value <- state_at(wave_times)
  wave_value[c(involved, FALSE)] <- TRUE
  # A real change strictly inside an uninvolved interval stays on the path.
  free <- !real_time %in% wave_times
  interval <- findInterval(real_time[free], wave_times, left.open = TRUE)
  free[free] <- !involved[interval]
  path_time <- c(wave_times, real_time[free])
  path_value <- c(wave_value, state_at(real_time[free]))
  o <- order(path_time)
  rewritten <- minimal_changes(path_time[o], path_value[o], start)

  list(real = real, rewritten = rewritten)
}

# The intervals of one rewritten actor whose real presence was not "present
# throughout" and so was overridden: absent at the interval start or leaving
# inside it. Each carries the real change times the rewrite dropped (a time
# lying on a wave is reported with the first such interval). `involved` flags
# the intervals the actor is involved in; only those are overridden.
snapped_intervals <- function(
  path,
  time,
  value,
  initial,
  involved,
  wave_times
) {
  n_intervals <- length(wave_times) - 1L
  state_at <- function(t) c(initial, value)[findInterval(t, time) + 1L]
  lower <- wave_times[-length(wave_times)]
  upper <- wave_times[-1L]
  overridden <- vapply(
    seq_len(n_intervals),
    function(k) {
      inside <- time > lower[[k]] & time < upper[[k]]
      involved[[k]] && (!state_at(lower[[k]]) || any(!value[inside]))
    },
    logical(1)
  )
  kept <- paste(path$rewritten$time, path$rewritten$value)
  real <- paste(path$real$time, path$real$value)
  dropped <- path$real$time[!real %in% kept]
  intervals <- which(overridden)
  claimed <- rep(FALSE, length(dropped))
  ignored <- vector("list", length(intervals))
  for (j in seq_along(intervals)) {
    k <- intervals[[j]]
    mine <- !claimed & dropped >= lower[[k]] & dropped <= upper[[k]]
    ignored[[j]] <- dropped[mine]
    claimed <- claimed | mine
  }
  data.frame(
    interval = intervals,
    lower = lower[intervals],
    upper = upper[intervals],
    ignored = I(ignored)
  )
}

# One cli warning for a whole rewrite: a bullet per (actor, interval) whose
# presence was overridden, with the real times now ignored. The list is capped
# so a large panel does not flood the console.
warn_composition_snapped <- function(snapped, labels, wave_times) {
  if (is.null(snapped) || nrow(snapped) == 0L) {
    return(invisible())
  }
  shown <- seq_len(min(nrow(snapped), 10L))
  n_more <- nrow(snapped) - length(shown)
  label <- labels[snapped$actor]
  lower <- snapped$lower
  upper <- snapped$upper
  ignored <- snapped$ignored
  lines <- vapply(
    shown,
    function(i) {
      paste0(
        "{.field {label[",
        i,
        "]}} in ({lower[",
        i,
        "]}, {upper[",
        i,
        "]}]",
        if (length(ignored[[i]]) > 0L) {
          paste0(
            ": {cli::qty(length(ignored[[",
            i,
            "]]))}real change{?s} at {.val {ignored[[",
            i,
            "]]}} ignored"
          )
        } else {
          ": present from the interval start"
        }
      )
    },
    character(1)
  )
  names(lines) <- rep("*", length(lines))
  if (n_more > 0L) {
    lines <- c(lines, i = "and {n_more} more")
  }
  cli::cli_warn(
    c(
      "Composition snapped to the panel waves.",
      "!" = paste0(
        "Actors involved in an interval are made present over all of it; ",
        "their real join and leave times inside it are ignored."
      ),
      lines,
      i = paste0(
        "The model changes: a moved actor's rate enters the compensator ",
        "over the whole interval."
      ),
      i = paste0(
        "Relational events stamped at a wave can see an actor moved to that ",
        "wave one interval early."
      )
    ),
    class = "goldfish_composition_snapped_warning"
  )
}

# The change points of a step path given at increasing `time`, starting from
# `start`: only the points where the value differs from the one before.
minimal_changes <- function(time, value, start) {
  keep <- value != c(start, value[-length(value)])
  list(time = time[keep], value = value[keep])
}

# Numeric times back in the class of `template`, the column they are written
# into: inverse of coerce_time() for numeric, POSIXct, Date and character.
restore_time_class <- function(time, template) {
  if (inherits(template, "POSIXct")) {
    .POSIXct(time, tz = attr(template, "tzone"))
  } else if (inherits(template, "Date")) {
    structure(time, class = "Date")
  } else if (is.character(template)) {
    format(.POSIXct(time, tz = "GMT"), "%Y-%m-%d %H:%M:%S")
  } else {
    time
  }
}

# `changes` without its `drop` rows and with the new `active` rows appended.
# The new rows copy the column types of `changes`; columns other than time,
# node, var and value are left missing.
splice_composition <- function(changes, drop, time, node, value) {
  n_new <- length(time)
  if (is.null(changes) || nrow(changes) == 0L) {
    added <- data.frame(time = time, node = node, var = rep("active", n_new))
    added$value <- as.list(value)
    return(added)
  }
  added <- changes[rep(1L, n_new), , drop = FALSE]
  for (col in setdiff(names(added), c("time", "node", "var", "value"))) {
    added[[col]][] <- NA
  }
  added$time <- time
  added$node <- node
  added$var <- rep("active", n_new)
  added$value <- if (is.list(changes$value)) as.list(value) else value
  rbind(changes[!drop, , drop = FALSE], added)
}
