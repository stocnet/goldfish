# =========================================================================== #
# The time-anchored driver.
#
# The observed event times are held and only the marks are redrawn. A run
# walks the dependent rows of the modeled layers in schedule order; at each
# stamp it advances the walk, so observed covariate rows and replayed rows
# strictly before that time are applied first (the rows sharing the stamp
# come after the event, as in the batch schedule), then draws the marks the
# stamp's own process models. The events drawn at one stamp are injected
# together once all of them are drawn, so none of them informs another's
# draw, as the batch reads a support mask strictly before its stamp. Nothing
# is drawn about when, so there is no clock: no target, no guard and no
# total-rate trajectory.
#
# A stamp keeps its observed layer and flavor. Redrawing which process fires,
# from the competing rates at that stamp, would move events between layers,
# and each layer's statistics would then read a stream it never observed.
# =========================================================================== #

# `spec` is already generatively complete. Returns a `goldfishSim`.
simulate_anchored_replicate <- function(
  spec,
  provider,
  steps,
  replicate,
  times,
  control_prep,
  requested_replay = NULL,
  call = rlang::caller_env()
) {
  handle <- walk_open(spec, control_prep, times = "observed", call = call)
  window_end <- resolve_walk_extent(handle$merged, control_prep)$end
  routing <- simulation_routing(spec, handle)
  map <- routing$map
  replay <- simulation_replay_plan(spec, handle, requested_replay)
  walk_replay(handle, replay$rows, call = call)
  stamps <- anchored_stamps(spec, handle, map)

  mark <- steps$mark %||% default_mark
  accept <- steps$accept %||% default_accept
  evaluate <- steps$evaluate %||% default_evaluate
  dims <- simulation_fid_dims(handle, routing)
  updates <- simulation_mark_updates(spec)
  pinned <- spec$completed_rates %||% list()
  latent <- provider$init(replicate, handle)
  n_stamps <- nrow(stamps)
  collector <- new_event_collector(capacity = n_stamps, observed = n_stamps)

  # Every event sharing a stamp is drawn over the state strictly before it,
  # and only then are they injected: an event at t never informs another
  # draw at t, of its own process or another. The drawn events of a stamp
  # wait in buffers sized by the largest stamp, built once.
  last_of_stamp <- c(stamps$time[-1L] != stamps$time[-n_stamps], TRUE)
  stamp_size <- if (n_stamps > 0L) max(rle(stamps$time)$lengths) else 0L
  pending_event <- vector("list", stamp_size)
  pending_fid <- vector("list", stamp_size)
  pending_latent <- vector("list", stamp_size)
  n_pending <- 0L

  t <- handle$current_time
  k <- 0L
  n_drawn <- 0L
  n_stamps_skipped <- 0L
  for (i in seq_len(n_stamps)) {
    t <- stamps$time[i]
    # The stamp is evaluated as the batch evaluates an observed event: over
    # the rows strictly before it, the composition at it included. The rows
    # sharing its stamp are applied after it is injected.
    walk_advance(handle, t, inclusive = FALSE, call = call)
    resolved <- provider$at(k, t, handle, latent)
    theta <- provider_parameters(resolved, routing, call)
    latent <- resolved$latent %||% latent

    evaluation <- anchored_evaluation(
      handle,
      stamps[i, ],
      theta,
      evaluate,
      dims,
      routing,
      updates,
      pinned
    )
    event <- if (!is.null(evaluation)) mark(evaluation$fid, evaluation, handle)
    # The simulated state can leave a stamp with nothing to draw: a held
    # sender with no receiver its choice allows, or no sender with a positive
    # rate. Writing the observed event there would put state no process
    # generated into the run, so the stamp is skipped and counted.
    if (is.null(event) || anyNA(c(event$sender, event$receiver))) {
      n_stamps_skipped <- n_stamps_skipped + 1L
    } else {
      n_drawn <- n_drawn + 1L
      if (isTRUE(accept(event, handle))) {
        event$time <- t
        k <- k + 1L
        n_pending <- n_pending + 1L
        pending_event[[n_pending]] <- event
        pending_fid[[n_pending]] <- evaluation$fid
        pending_latent[n_pending] <- list(latent)
      }
    }
    if (last_of_stamp[i]) {
      for (j in seq_len(n_pending)) {
        walk_inject(handle, pending_event[[j]], call = call)
        collect_event(
          collector,
          pending_event[[j]],
          pending_fid[[j]],
          map,
          pending_latent[[j]]
        )
      }
      n_pending <- 0L
    }
  }
  # Every anchored run covers the whole window, so the replayed rows after
  # the last stamp are applied too.
  walk_advance(handle, max(t, window_end), call = call)

  new_goldfish_sim(
    events = collector_frame(collector),
    process_map = simulation_result_map(map, replay$flavors),
    replayed = replayed_frame(handle, replay),
    times = times,
    capped = FALSE,
    stop_reason = "observed",
    n_proposals = n_drawn,
    end_time = t,
    window_end = window_end,
    trajectory = rate_trajectory_frame(new_rate_trajectory(NA_real_)),
    latent = collector_latent(collector),
    spec = spec,
    replicate = replicate,
    n_stamps_skipped = n_stamps_skipped
  )
}

# The observed stamps an anchored run draws at: the dependent rows of a layer
# some process models, and on a flavored layer only those whose flavor is
# modeled. A dependent row of an unmodeled flavor is not a stamp; its update
# row is replayed instead. The flavor is read back from the row's value
# through the layer's `values_equivalence`, as the replayed rows' is.
#
# One row per stamp: time, layer, flavor, sender.
anchored_stamps <- function(spec, handle, map) {
  schedule <- handle$schedule
  info <- spec$data$info %||% list()
  rows <- which(schedule$dependent & schedule$layer %in% map$layer)
  layers <- as.character(schedule$layer[rows])
  values <- vapply(schedule$value[rows], as.numeric, numeric(1))
  flavor <- vapply(
    seq_along(rows),
    function(i) {
      mapping <- info$values_equivalence[[layers[i]]]
      if (is.null(mapping)) {
        return(NA_character_)
      }
      names(mapping)[match(values[i], mapping)]
    },
    character(1)
  )
  modeled <- vapply(
    seq_along(rows),
    function(i) {
      any(map$layer == layers[i] & identical_flavor(map$flavor, flavor[i]))
    },
    logical(1)
  )
  keep <- rows[modeled]
  data.frame(
    time = as.numeric(schedule$time[keep]),
    layer = layers[modeled],
    flavor = flavor[modeled],
    sender = as.integer(schedule$sender[keep]),
    stringsAsFactors = FALSE
  )
}

# What the mark step reads at one stamp, or NULL when the stamp's process has
# no sender to draw. A process with a rate draws its marks exactly as a
# free-running run does once its fid has fired. A choice-only process models
# no sender, so the observed one is held: its "rate" is the indicator of that
# sender, and the mark step draws the receiver from its choice row.
anchored_evaluation <- function(
  handle,
  stamp,
  theta,
  evaluate,
  dims,
  routing,
  updates,
  pinned
) {
  map <- routing$map
  process <- map$layer == stamp$layer &
    identical_flavor(map$flavor, stamp$flavor)
  rate_fid <- map$fid[process & map$family == "rate"]
  if (length(rate_fid) == 1L) {
    rates <- rate_values(
      handle,
      rate_fid,
      theta,
      evaluate,
      routing,
      pinned,
      stamp$time
    )
    if (!(sum(rates[[1L]]) > 0)) {
      return(NULL)
    }
    return(mark_evaluation(
      handle,
      rate_fid,
      theta,
      evaluate,
      dims,
      rates,
      routing,
      updates
    ))
  }
  choice_fid <- map$fid[process & map$family == "choice"]
  shape <- dims[[as.character(choice_fid)]]
  held <- numeric(shape$n_actors1)
  held[stamp$sender] <- 1
  list(
    fid = choice_fid,
    family = "choice",
    layer = stamp$layer,
    flavor = stamp$flavor,
    n_actors1 = shape$n_actors1,
    n_actors2 = shape$n_actors2,
    rate = held,
    choice = choice_evaluation(handle, choice_fid, theta, evaluate, routing),
    update = updates[[as.character(choice_fid)]]
  )
}
