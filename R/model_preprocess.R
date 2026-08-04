#' Preprocess a model given its specification
#'
#' S3 generic dispatched on the model specification class.
#' Model variants converted to the recipe architecture implement a dedicated
#' method; the remaining variants fall back to the monolithic loop through
#' `preprocess.model_spec()` until their recipe lands.
#'
#' @param spec a `model_spec` object from `new_model_spec()`.
#' @param ... arguments passed to the recipe methods, see
#'   `preprocess_monolith()` and `run_sender_recipe_loop()`.
#'
#' @return a list of class preprocessed.goldfish
#' @noRd
preprocess <- function(spec, ...) {
  UseMethod("preprocess")
}

#' @noRd
preprocess.model_spec <- function(spec, ...) {
  legacy_sub_model <- if (inherits(spec, "sender_spec")) "rate" else "choice"
  preprocess_monolith(model = spec$model, sub_model = legacy_sub_model, ...)
}

#' @noRd
preprocess.dynam_rate_spec <- function(spec, ...) {
  run_sender_recipe_loop(
    spec,
    ...,
    right_censored = TRUE,
    intercept_scalars = TRUE
  )
}

#' @noRd
preprocess.dynam_rate_ordered_spec <- function(spec, ...) {
  run_sender_recipe_loop(
    spec,
    ...,
    right_censored = FALSE,
    intercept_scalars = FALSE
  )
}

#' @noRd
preprocess.dynam_choice_spec <- function(spec, ...) {
  run_dyad_recipe_loop(
    spec,
    ...,
    right_censored = FALSE,
    intercept_scalars = FALSE
  )
}

#' @noRd
preprocess.dynam_choice_coord_spec <- function(spec, ...) {
  run_dyad_recipe_loop(
    spec,
    ...,
    right_censored = FALSE,
    intercept_scalars = FALSE
  )
}

#' @noRd
preprocess.rem_rate_spec <- function(spec, ...) {
  run_dyad_recipe_loop(
    spec,
    ...,
    right_censored = TRUE,
    intercept_scalars = TRUE
  )
}

#' @noRd
preprocess.rem_rate_ordered_spec <- function(spec, ...) {
  run_dyad_recipe_loop(
    spec,
    ...,
    right_censored = FALSE,
    intercept_scalars = FALSE
  )
}

#' DyNAMi recipe wrappers
#'
#' Thin wrappers delegating to the existing monolithic DyNAMi preprocessing
#' loop with unchanged arguments. The dedicated DyNAMi recipe
#' (post-event update order, `sub_type` normalisation) is deferred to the
#' effects unification change. `preprocess_interaction()` keeps computing its
#' own start and end times from the event streams, as it did before the
#' dispatch wiring.
#'
#' @inheritParams run_sender_recipe_loop
#' @param groups_network character, name of the groups network object.
#' @param ... absorbs the recipe arguments that the DyNAMi loop does not
#'   consume (`window_parameters`, `ignore_rep_parameter`, `is_two_mode`,
#'   `startTime`, `endTime`, `opportunitiesList`).
#' @name preprocess_dynami
#' @noRd
run_dynami_monolith <- function(
  sub_model,
  events,
  effects,
  events_objects_link,
  events_effects_link,
  objects_effects_link,
  nodes,
  nodes2 = nodes,
  right_censored = FALSE,
  progress = FALSE,
  groups_network = NULL,
  prep_envir = new.env()
) {
  prep <- preprocess_interaction(
    sub_model = sub_model,
    events = events,
    effects = effects,
    events_objects_link = events_objects_link,
    events_effects_link = events_effects_link,
    objects_effects_link = objects_effects_link,
    nodes = nodes,
    nodes2 = nodes2,
    right_censored = right_censored,
    progress = progress,
    groups_network = groups_network,
    prep_envir = prep_envir
  )
  prep$prep_version <- PREP_VERSION
  prep
}

#' @rdname preprocess_dynami
#' @noRd
preprocess.dynami_rate_spec <- function(
  spec,
  events,
  effects,
  events_objects_link,
  events_effects_link,
  objects_effects_link,
  nodes,
  nodes2 = nodes,
  right_censored = FALSE,
  progress = FALSE,
  groups_network = NULL,
  prep_envir = new.env(),
  ...
) {
  run_dynami_monolith(
    "rate",
    events,
    effects,
    events_objects_link,
    events_effects_link,
    objects_effects_link,
    nodes,
    nodes2,
    right_censored,
    progress,
    groups_network,
    prep_envir
  )
}

#' @rdname preprocess_dynami
#' @noRd
preprocess.dynami_rate_ordered_spec <- function(
  spec,
  events,
  effects,
  events_objects_link,
  events_effects_link,
  objects_effects_link,
  nodes,
  nodes2 = nodes,
  right_censored = FALSE,
  progress = FALSE,
  groups_network = NULL,
  prep_envir = new.env(),
  ...
) {
  run_dynami_monolith(
    "rate",
    events,
    effects,
    events_objects_link,
    events_effects_link,
    objects_effects_link,
    nodes,
    nodes2,
    right_censored,
    progress,
    groups_network,
    prep_envir
  )
}

#' @rdname preprocess_dynami
#' @noRd
preprocess.dynami_choice_spec <- function(
  spec,
  events,
  effects,
  events_objects_link,
  events_effects_link,
  objects_effects_link,
  nodes,
  nodes2 = nodes,
  right_censored = FALSE,
  progress = FALSE,
  groups_network = NULL,
  prep_envir = new.env(),
  ...
) {
  run_dynami_monolith(
    "choice",
    events,
    effects,
    events_objects_link,
    events_effects_link,
    objects_effects_link,
    nodes,
    nodes2,
    right_censored,
    progress,
    groups_network,
    prep_envir
  )
}

#' Sender-indexed recipe kernel
#'
#' Shared event loop for the sender-indexed model variants.
#' It consumes the three recipe input structures built once before the loop
#' the state container owned by the recipe, the
#' merged event schedule, and the compiled update plan. Statistic updates
#' are written into one flat `stat_mat_update` buffer with doubling growth
#' covering dependent and right-censored events; `initial_stats`
#' is kept in the sender-native `n1 x nEffects` form. Global-attribute
#' events update the `globals` component of the state container and emit
#' right-censored statistic updates without sender/receiver recording.
#'
#' @section Extension points (documented, not implemented):
#' Two future capabilities attach to this loop and its writer; neither is
#' implemented in this change.
#' \describe{
#'   \item{Per-event simulation hook}{a hook invoked once per
#'     stored event, positioned immediately after the per-event statistic
#'     update is emitted to the writer (the `writer$write_event()` call in
#'     this loop) and before the loop advances to the next scheduled event.
#'     At that point the visible state is the current state container — the
#'     networks, nodal/nodal2 attribute frames, and globals row reflecting
#'     all updates up to and including this event. The hook may read that
#'     snapshot and append new events to the schedule (an event-stream
#'     append): appended events must carry a `time` not earlier than the
#'     current event and are merged respecting the dependent-first tie-break
#'     so the schedule stays time-sorted. This hook — not a writer — is the
#'     seam reserved for a future `simulate()` goodness-of-fit method.}
#'   \item{Parallel chunk preprocessing}{the loop plus its
#'     writer can be run over a contiguous chunk of the event schedule,
#'     warm-started from the state container at the chunk's first event, with
#'     per-chunk results merged by a coordinating `finalize()`. The
#'     obligation is that `writer$write_event` is associative across a chunk
#'     boundary and the per-chunk flat buffers concatenate in event order.
#'     See the writer-strategy extension points in [preprocess_writers] for
#'     the alternatives-sampling gather writer and the parallel/streaming
#'     writer contracts.}
#' }
#'
#' @param spec a `spec_map` (built by `build_spec_map()`) carrying a
#'   `sender_spec` class; the effect closures, per-term window parameters, link
#'   matrices, plan, call templates, and node sets are unpacked from it.
#' @inheritParams preprocess_monolith
#' @param right_censored logical, whether right-censored events are stored.
#' @param intercept_scalars logical, whether `n_dep_events`, `total_time`,
#'   and `avg_active_entity` are computed and stored.
#' @param ... absorbs arguments of `preprocess_monolith()` that the kernel
#'   does not consume (`is_two_mode`, `right_censored`,
#'   `ignore_rep_parameter`, `opportunitiesList`).
#'
#' @return a list of class preprocessed.goldfish
#' @noRd
# Fold a sender-loop support_constraint into `active_sender`.
# The per-event effective availability is the row-reduction a sender is at risk
# iff it has >= 1 allowed, present receiver:
#   active_sender[i] at event e = presence_e[i] & (rowSums(support_e &
#     active_dyad_init) > 0)
# — computed here (during preprocessing) and stored on the availability object
# as net crossings (a flip is emitted only on a 0 <-> positive change of the
# per-sender available-receiver count, so the buffer stays tiny), REPLACING the
# estimation-time recombination. `support_mask$support` carries the per-event
# dyadic mask snapshots (lagged, aligned to the stored events);
# `active_dyad_init` is the receiver presence used for the row-reduction
# (matching the predecessor's static receiver availability). Returns `out` with
# `active_sender_init`/`_update`/`_update_pointer` rewritten to the folded
# object, `active_sender_folded = TRUE`, and (when intercept scalars are stored)
# `avg_active_entity` recomputed as the event-averaged active-sender count.
fold_active_sender_support <- function(out, support_mask, active_dyad_init) {
  support <- support_mask$support
  n_stored <- length(out$event_time)
  if (n_stored == 0L) {
    return(out)
  }
  n1 <- length(out$active_sender_init)

  # Walk the presence crossings buffer in event order to recover presence_e,
  # intersect with the per-event sender gate, and record the folded vector.
  presence <- out$active_sender_init
  upd <- out$active_sender_update
  ptr <- out$active_sender_update_pointer
  folded <- vector("list", n_stored)
  prev_ptr <- 0L
  for (e in seq_len(n_stored)) {
    this_ptr <- if (!is.null(ptr)) ptr[e] else 0L
    if (this_ptr > prev_ptr) {
      cols <- (prev_ptr + 1L):this_ptr
      presence[upd[1L, cols]] <- as.logical(upd[2L, cols])
    }
    prev_ptr <- this_ptr
    gate <- rowSums(support[[e]] & rep(active_dyad_init, each = n1)) > 0
    folded[[e]] <- presence & gate
  }

  # Re-encode the folded timeline as crossings: the init carries the first
  # event's value (its slice is empty) and each later event emits only the
  # senders whose folded availability changed since the previous event.
  n_changes <- integer(n_stored)
  change_nodes <- vector("list", n_stored)
  change_repl <- vector("list", n_stored)
  for (e in seq_len(n_stored)[-1L]) {
    ch <- which(folded[[e]] != folded[[e - 1L]])
    n_changes[e] <- length(ch)
    change_nodes[[e]] <- ch
    change_repl[[e]] <- as.numeric(folded[[e]][ch])
  }
  node_vec <- unlist(change_nodes, use.names = FALSE)
  repl_vec <- unlist(change_repl, use.names = FALSE)
  # Preserve the raw sender presence for the fail-fast constraint validation
  # (its "present but always gated out" warning is defined on raw presence, not
  # the folded object); estimation engines never read it.
  out$support_mask$sender_presence_init <- out$active_sender_init
  out$active_sender_init <- folded[[1L]]
  out$active_sender_update <- if (length(node_vec) > 0L) {
    rbind(node_vec, repl_vec)
  } else {
    matrix(0, 2L, 0L)
  }
  out$active_sender_update_pointer <- cumsum(n_changes)
  out$active_sender_changes <- list()
  out$active_sender_folded <- TRUE

  if (!is.null(out$avg_active_entity)) {
    out$avg_active_entity <- mean(vapply(folded, sum, numeric(1)))
  }
  out
}

# Shared opening for the sender and dyad recipe loops. The two drivers begin
# identically: unpack the compiled `spec`, realize derivations and fetch the
# per-object event streams, resolve the [start_time, end_time) observation
# window, impute missing data, initialize the per-effect cache/stat matrices,
# then build the state container, event schedule, composition-change streams,
# and net/att update lookups. They diverge only in the `sub_model` passed to
# `initialize_cache_stat()` and in how each shapes `initial_stats` (a 2D sender
# kernel vs a 3D dyad array) — that shaping stays in each loop. The context is
# returned as a list the caller splats into its frame; it is also the seam a
# future multi-consumer walk over one shared state builds on.
prepare_recipe_context <- function(
  spec,
  startTime,
  endTime,
  prep_envir,
  sub_model,
  progress = FALSE
) {
  # The compiled recipe inputs ride on `spec` (a spec_map): the
  # effect closures, per-term window parameters, link matrices, plan, and call
  # templates are unpacked here instead of threaded as separate arguments.
  effects <- spec$effects
  window_parameters <- spec$window_parameters
  events_objects_link <- spec$events_objects_link
  events_effects_link <- spec$events_effects_link
  objects_effects_link <- spec$objects_effects_link
  plan <- spec$plan
  effects_template <- spec$effects_template
  nodes <- spec$nodes
  nodes2 <- spec$nodes2

  # State creation owns the derived-input data: realize each derived object
  # (e.g. windowed networks + dissolve streams) from plan$derivations, then
  # fetch the event streams from spec$fetch_plan — both through the data source,
  # before any cache/state/schedule reads them.
  src <- new_data_source(
    data = spec$data,
    envir = prep_envir,
    focal = spec$focal,
    modeled_flavor = spec$modeled_flavor
  )
  src <- ds_realize_derivations(src, plan$derivations)
  events <- fetch_events(spec$fetch_plan, envir = prep_envir, src = src)

  n1 <- ds_n_nodes(src, nodes)
  n2 <- ds_n_nodes(src, nodes2)

  hasEndTime <- FALSE
  hasStartTime <- FALSE
  isValidEvent <- TRUE

  is_window_effect <- !vapply(window_parameters, is.null, logical(1))
  which_event_no_window_effect <-
    events_effects_link[, !is_window_effect, drop = FALSE]
  which_event_no_window_effect <- rowSums(!is.na(which_event_no_window_effect))
  which_event_no_window_effect <- c(1, which(which_event_no_window_effect > 0))

  events_min <- min(vapply(
    events[which_event_no_window_effect],
    function(x) min(x$time),
    double(1)
  ))
  events_max <- max(vapply(
    events[which_event_no_window_effect],
    function(x) max(x$time),
    double(1)
  ))
  if (is.null(endTime)) {
    endTime <- events_max
    if (any(is_window_effect)) hasEndTime <- TRUE
  } else if (endTime != events_max) {
    if (!is.numeric(endTime)) {
      endTime <- as.numeric(endTime)
    }
    if (events_min > endTime) {
      stop("End time smaller than first event time.", call. = FALSE)
    }
    hasEndTime <- TRUE
  }
  if (is.null(startTime)) {
    startTime <- events_min
  } else if (startTime != events_min) {
    if (!is.numeric(startTime)) {
      startTime <- as.numeric(startTime)
    }
    if (events_max < startTime) {
      stop("Start time geater than last event time.", call. = FALSE)
    }
    hasStartTime <- TRUE
    if (events_min < startTime) isValidEvent <- FALSE
  }

  validate_imputation_policy(spec$impute_policy, plan$objects)
  # Carry each object's policy on the registry so the walk-time recode reads it
  # by oid alongside the value type.
  plan$objects$policy <- vapply(
    plan$objects$key,
    function(key) imputation_policy_for(spec$impute_policy, key),
    character(1)
  )

  src <- ds_impute_missing(
    src,
    objects_effects_link,
    policy = spec$impute_policy
  )

  if (progress) {
    cat("Initializing cache objects and statistical matrices.\n")
  }

  stat_cache <- initialize_cache_stat(
    objects_effects_link = objects_effects_link,
    effects = effects,
    groups_network = NULL,
    window_parameters = window_parameters,
    n1 = n1,
    n2 = n2,
    model = spec$model,
    sub_model = sub_model,
    envir = prep_envir,
    src = src
  )
  # Function-effects (operands + mains) have a closure each; interaction columns
  # are appended after them as derived product columns with no closure.
  # n_fun = closures, n_inter = interactions, nEffects = total output columns.
  n_fun <- length(effects)
  inter_ids <- if (length(plan$interactions) > 0) {
    as.integer(names(plan$interactions))
  } else {
    integer(0)
  }
  n_inter <- length(inter_ids)
  nEffects <- n_fun + n_inter

  composition1 <- ds_composition(src, nodes, n1)
  composition2 <- ds_composition(src, nodes2, n2)
  active_sender_init <- composition1$init
  active_dyad_init <- composition2$init
  active_sender_changes <- composition1$changes
  active_dyad_changes <- composition2$changes

  state <- build_state_container(
    rownames(objects_effects_link),
    nodes,
    nodes2,
    envir = prep_envir,
    src = src
  )
  schedule <- build_event_schedule(events, events_objects_link, plan$objects)
  assert_imputable_schedule(schedule, plan$objects, attr(state, "strata"))
  assert_globals_defined(state, plan$objects, schedule)

  net_update_lookup <- matrix(NA_integer_, nrow(plan$objects), nEffects)
  att_update_lookup <- matrix(NA_integer_, nrow(plan$objects), nEffects)
  net_update_lookup[cbind(plan$effect_objects$oid, plan$effect_objects$gid)] <-
    plan$effect_objects$net_update
  att_update_lookup[cbind(plan$effect_objects$oid, plan$effect_objects$gid)] <-
    plan$effect_objects$att_update

  list(
    effects = effects,
    window_parameters = window_parameters,
    events_objects_link = events_objects_link,
    events_effects_link = events_effects_link,
    objects_effects_link = objects_effects_link,
    plan = plan,
    effects_template = effects_template,
    nodes = nodes,
    nodes2 = nodes2,
    src = src,
    events = events,
    n1 = n1,
    n2 = n2,
    hasEndTime = hasEndTime,
    hasStartTime = hasStartTime,
    isValidEvent = isValidEvent,
    is_window_effect = is_window_effect,
    which_event_no_window_effect = which_event_no_window_effect,
    events_min = events_min,
    events_max = events_max,
    startTime = startTime,
    endTime = endTime,
    stat_cache = stat_cache,
    n_fun = n_fun,
    inter_ids = inter_ids,
    n_inter = n_inter,
    nEffects = nEffects,
    active_sender_init = active_sender_init,
    active_dyad_init = active_dyad_init,
    active_sender_changes = active_sender_changes,
    active_dyad_changes = active_dyad_changes,
    state = state,
    schedule = schedule,
    net_update_lookup = net_update_lookup,
    att_update_lookup = att_update_lookup
  )
}

run_sender_recipe_loop <- function(
  spec,
  startTime = NULL,
  endTime = NULL,
  right_censored = FALSE,
  intercept_scalars = FALSE,
  progress = FALSE,
  prep_envir = new.env(),
  writer = writer_default(),
  consumer_specs = NULL,
  ...
) {
  ctx <- prepare_recipe_context(
    spec,
    startTime,
    endTime,
    prep_envir,
    sub_model = "rate",
    progress = progress
  )
  # Splat the shared setup (spec unpack, streams, window, cache, state,
  # schedule, composition, lookups) into this frame; each loop then shapes its
  # own `initial_stats` (2D sender kernel vs 3D dyad array).
  list2env(ctx, environment())

  initial_stats <- matrix(0, nrow = n1, ncol = nEffects)
  initial_stats[, seq_len(n_fun)] <- do.call(
    cbind,
    lapply(stat_cache, "[[", "stat")
  )
  stat_cache <- lapply(stat_cache, "[[", "cache")

  # Interaction state (sender kernel): keep a live per-sender vector for every
  # operand feeding an interaction, seeded from its initial column and updated in
  # place as its effect emits deltas. Each interaction column is the elementwise
  # per-sender product of its operands; its initial column is seeded here and its
  # deltas are emitted as per-sender point updates when any operand changes.
  op_kind <- plan$effects$broadcast_kind
  op_state <- new.env(parent = emptyenv())
  # Per-event accumulator of interaction operand senders touched, keyed by
  # interaction gid; emptied after each event's interaction emission.
  dirty_inter <- list()
  if (n_inter > 0) {
    operand_gids <- sort(unique(unlist(plan$interactions)))
    for (og in operand_gids) {
      assign(as.character(og), initial_stats[, og], envir = op_state)
    }
    for (ig in inter_ids) {
      ops <- plan$interactions[[as.character(ig)]]
      prod_vec <- get(as.character(ops[1]), envir = op_state)
      for (o in ops[-1]) {
        prod_vec <- prod_vec * get(as.character(o), envir = op_state)
      }
      initial_stats[, ig] <- prod_vec
    }
  }

  call_effect_template <- function(
    template,
    gid,
    shape,
    event_args,
    net_update,
    att_update,
    event_order,
    inter_event_time
  ) {
    args <- c(
      list(
        network = if (template$n_networks == 1L) {
          state$networks[[template$net_keys]]
        } else if (template$n_networks > 1L) {
          lapply(template$net_keys, function(k) state$networks[[k]])
        } else {
          list()
        },
        attribute = if (template$n_attributes == 1L) {
          state[[template$att_components]][[template$att_keys]]
        } else if (template$n_attributes > 1L) {
          lapply(
            seq_len(template$n_attributes),
            function(j) {
              state[[template$att_components[j]]][[template$att_keys[j]]]
            }
          )
        } else {
          list()
        },
        cache = stat_cache[[gid]],
        n1 = n1,
        n2 = n2,
        net_update = net_update,
        att_update = att_update,
        event_order = event_order,
        inter_event_time = inter_event_time
      ),
      event_args
    )
    do.call(template$fun, args[template$args_by_shape[[shape]]])
  }

  # One consumer per output object: the single default output, or one per
  # modeled flavor reading the shared union walk. Each owns its writer, its
  # projection onto its own statistics columns, and its own pending buffers.
  consumers <- init_consumers(
    consumer_specs,
    writer = writer,
    right_censored = right_censored,
    spec = spec,
    dims = list(
      nEffects = nEffects,
      n1 = n1,
      n2 = n2,
      is_sender = inherits(spec, "sender_spec"),
      n_dependent = nrow(events[[1L]]),
      max_store = schedule$n + 1L
    ),
    initial_stats_fn = function() initial_stats
  )
  rc_consumers <- Filter(function(cs) cs$right_censored, consumers)

  bcast_kind <- plan$effects$broadcast_kind

  i_total_events <- 0L
  i_dependent_events <- 0L
  time <- startTime
  interval <- 0
  final_step <- FALSE

  if (progress) {
    cat("Preprocessing events.\n", startTime, endTime, schedule$n)
    pb <- utils::txtProgressBar(max = schedule$n, char = "*", style = 3)
    dot_events <- ifelse(schedule$n > 50, ceiling(schedule$n / 50), 1)
  }

  for (k in seq_len(schedule$n)) {
    i_total_events <- i_total_events + 1L
    next_event_time <- schedule$time[k]
    if (hasStartTime || hasEndTime) {
      if (isValidEvent && next_event_time <= endTime) {
        interval <- next_event_time - time
      } else if (isValidEvent && next_event_time > endTime) {
        interval <- endTime - time
        next_event_time <- endTime
        final_step <- TRUE
      } else if (!isValidEvent && next_event_time >= startTime) {
        interval <- next_event_time - startTime
        isValidEvent <- TRUE
      }
    } else {
      interval <- next_event_time - time
    }

    time <- next_event_time

    isDependent <- schedule$dependent[k] && !final_step

    if (progress && i_total_events %% dot_events == 0) {
      utils::setTxtProgressBar(pb, i_total_events)
    }

    # `event_order` is derived as the difference of these two counters, so a
    # dependent event must advance both whether or not the observation window
    # has opened. Advancing only the total would leave a gap across the
    # burn-in fold, and an effect that reads adjacency in the event stream --
    # trans(history = "consecutive") -- would find none.
    if (isDependent) {
      i_dependent_events <- 1L + i_dependent_events
    }

    if (isValidEvent && isDependent) {
      if (schedule$shape[k] == "node") {
        ev_sender <- schedule$node[k]
        ev_receiver <- schedule$node[k]
      } else {
        ev_sender <- schedule$sender[k]
        ev_receiver <- schedule$receiver[k]
      }
      route_dependent_event(
        consumers,
        rc_consumers,
        flavor = schedule$flavor[k],
        event_info = list(
          is_dependent = 1L,
          interval = interval,
          time = time,
          sender = ev_sender,
          receiver = ev_receiver
        )
      )
    } else if (!isDependent) {
      if (isValidEvent && length(rc_consumers) > 0L && interval > 0) {
        if (final_step) {
          # The closing row is the window ending, not an event. The event that
          # triggered the stop lies outside the window, and its sender and
          # receiver would read as a real observation everywhere the row
          # surfaces.
          ev_sender <- NA_integer_
          ev_receiver <- NA_integer_
        } else if (schedule$shape[k] == "global") {
          ev_sender <- NA_integer_
          ev_receiver <- NA_integer_
        } else if (schedule$shape[k] == "node") {
          ev_sender <- schedule$node[k]
          ev_receiver <- schedule$node[k]
        } else {
          ev_sender <- schedule$sender[k]
          ev_receiver <- schedule$receiver[k]
        }
        route_right_censored_event(
          rc_consumers,
          list(
            is_dependent = 0L,
            interval = interval,
            time = time,
            sender = ev_sender,
            receiver = ev_receiver
          )
        )
      }

      if (!final_step) {
        oid <- schedule$target[k]
        component <- plan$objects$component[oid]
        key <- plan$objects$key[oid]
        shape <- schedule$shape[k]
        is_undirected_net <- plan$objects$is_undirected[oid]

        if (shape == "global") {
          replace_value <- schedule$value[[k]]
          if (is.na(replace_value)) {
            replace_value <- 0
          }
          event_args <- list(replace = replace_value)
        } else if (shape == "node") {
          event_node <- schedule$node[k]
          if (schedule$semantics[k] == "increment") {
            increment_value <- schedule$value[[k]]
            if (is.na(increment_value)) {
              increment_value <- 0
            }
            replace_value <-
              state[[component]][[key]][event_node] + increment_value
          } else {
            replace_value <- schedule$value[[k]]
            if (is.na(replace_value)) {
              if (identical(plan$objects$policy[oid], "as_category")) {
                # Under the as-category policy a missing event value is the
                # reserved level, not a summary over the other nodes -- the same
                # recode the initial table received.
                replace_value <- IMPUTATION_MISSING_LEVEL
              } else {
                # Impute from the node's own mode category by the summary its
                # recorded type selects -- a bare mean would write NA into a
                # categorical vector, failing the next update's comparison.
                replace_value <- impute_nodal_value(
                  state[[component]][[key]],
                  event_node,
                  attr(state, "strata")[[component]],
                  plan$objects$value_type[oid]
                )
              }
            }
          }
          event_args <- list(node = event_node, replace = replace_value)
        } else {
          event_sender <- schedule$sender[k]
          event_receiver <- schedule$receiver[k]
          if (schedule$semantics[k] == "increment") {
            increment_value <- schedule$value[[k]]
            if (is.na(increment_value)) {
              increment_value <- 0
            }
            replace_value <-
              state$networks[[key]][event_sender, event_receiver] +
              increment_value
          } else {
            replace_value <- schedule$value[[k]]
            if (is.na(replace_value)) replace_value <- 0
          }
          if (replace_value < 0) {
            warning(
              "You are dissolving a tie which doesn't exist!",
              call. = FALSE
            )
          }
          event_args <- list(
            sender = event_sender,
            receiver = event_receiver,
            replace = replace_value
          )
        }

        for (gid in plan$routing[[oid]]) {
          template <- effects_template[[gid]]
          net_update_pos <- net_update_lookup[oid, gid]
          if (is.na(net_update_pos)) {
            net_update_pos <- NULL
          }
          att_update_pos <- att_update_lookup[oid, gid]
          if (is.na(att_update_pos)) {
            att_update_pos <- NULL
          }

          effect_update <- call_effect_template(
            template,
            gid,
            shape,
            event_args,
            net_update_pos,
            att_update_pos,
            i_total_events - i_dependent_events,
            interval
          )

          if (!is.null(attr(effect_update$cache, "last_update"))) {
            attr(stat_cache[[gid]], "last_update") <- attr(
              effect_update$cache,
              "last_update"
            )
          }

          updates <- effect_update$changes
          if (
            !is.null(effect_update$cache) && !is.null(effect_update$changes)
          ) {
            stat_cache[[gid]] <- effect_update$cache
          }

          if (is_undirected_net) {
            event_args2 <- event_args
            event_args2$sender <- event_args$receiver
            event_args2$receiver <- event_args$sender
            effect_update2 <- call_effect_template(
              template,
              gid,
              shape,
              event_args2,
              net_update_pos,
              att_update_pos,
              i_total_events - i_dependent_events,
              interval
            )
            if (
              !is.null(effect_update2$cache) &&
                !is.null(effect_update2$changes)
            ) {
              stat_cache[[gid]] <- effect_update2$cache
            }
            updates <- rbind(updates, effect_update2$changes)
          }

          if (!is.null(updates)) {
            # Interaction second-hop (sender kernel): if this effect is an
            # operand, apply its per-sender delta to its live vector and record
            # the touched senders for each interaction it feeds. Sender deltas
            # carry a unique node1 per row (discovery 0.1), so no per-operand
            # dedup is needed.
            if (n_inter > 0L && gid <= n_fun) {
              feeds <- plan$operand_of[[as.character(gid)]]
              if (!is.null(feeds)) {
                ov <- get(as.character(gid), envir = op_state)
                ov[updates[, "node1"]] <- updates[, "replace"]
                assign(as.character(gid), ov, envir = op_state)
                for (ig in feeds) {
                  igc <- as.character(ig)
                  dirty_inter[[igc]] <- c(
                    dirty_inter[[igc]],
                    updates[, "node1"]
                  )
                }
              }
            }
            if (hasStartTime && next_event_time < startTime) {
              initial_stats[cbind(updates[, "node1"], gid)] <-
                updates[, "replace"]
            } else if (bcast_kind[gid] != 0L) {
              bc_block <- broadcast_entries_from_updates(
                updates,
                bcast_kind[gid],
                gid
              )
              for (cs in consumers) {
                consumer_accumulate_broadcast(cs, bc_block)
              }
            } else {
              block <- rbind(
                updates[, "node1"] - 1,
                0,
                gid - 1,
                updates[, "replace"]
              )
              for (cs in consumers) {
                consumer_accumulate_point(cs, block)
              }
            }
          }
        }

        # Emit each touched interaction's product delta (sender kernel):
        # recompute the per-sender product over its operands at the union of
        # senders changed this event and route it as a per-sender point update.
        # Emitted after the routing loop so all operand deltas are applied first;
        # a trivial unique() covers the cross-operand overlap.
        if (n_inter > 0L && length(dirty_inter) > 0L) {
          for (igc in names(dirty_inter)) {
            ig <- as.integer(igc)
            senders <- unique(dirty_inter[[igc]])
            ops <- plan$interactions[[igc]]
            prodv <- get(as.character(ops[1]), envir = op_state)[senders]
            for (o in ops[-1]) {
              prodv <- prodv * get(as.character(o), envir = op_state)[senders]
            }
            if (hasStartTime && next_event_time < startTime) {
              initial_stats[cbind(senders, ig)] <- prodv
            } else {
              block <- rbind(senders - 1, 0, ig - 1, prodv)
              for (cs in consumers) {
                consumer_accumulate_point(cs, block)
              }
            }
          }
          dirty_inter <- list()
        }

        if (shape == "global") {
          state$globals[[key]] <- event_args$replace
        } else if (shape == "node") {
          state[[component]][[key]][event_args$node] <- event_args$replace
        } else {
          state$networks[[key]][event_args$sender, event_args$receiver] <-
            event_args$replace
          if (is_undirected_net) {
            state$networks[[key]][event_args$receiver, event_args$sender] <-
              event_args$replace
          }
        }
      }
    }

    if (final_step) break
  }

  # The window closes at the end time even when the schedule runs out before
  # reaching it. Only the branch that meets an out-of-window event used to
  # write the closing row, so the exposure after the last event left the
  # likelihood entirely, biasing the baseline rate upward. Whether the row is
  # stored is a property of the likelihood: a family with no compensator keeps
  # no right-censoring consumer, and a row there would contribute exactly zero
  # while changing the interval count.
  trailing_interval <- endTime - time
  if (
    isValidEvent &&
      !final_step &&
      length(rc_consumers) > 0L &&
      trailing_interval > 0
  ) {
    route_right_censored_event(
      rc_consumers,
      list(
        is_dependent = 0L,
        interval = trailing_interval,
        time = endTime,
        sender = NA_integer_,
        receiver = NA_integer_
      )
    )
  }

  if (progress) {
    utils::setTxtProgressBar(pb, schedule$n)
    close(pb)
  }

  # Rate models gate on the sender axis: the constraint mask is
  # realized dyad-shaped here (the same self-contained pass, aux dyad state)
  # and attached additively; the gather consumer reduces it to a per-sender gate.
  # A NULL sub-plan leaves the output unchanged.
  finalize_consumers(
    consumers,
    consumer_specs,
    tail = list(
      spec = spec,
      initial_stats = initial_stats,
      active_sender_init = active_sender_init,
      active_sender_changes = active_sender_changes,
      active_dyad_init = active_dyad_init,
      active_dyad_changes = active_dyad_changes,
      start_time = startTime,
      end_time = endTime,
      intercept_scalars = intercept_scalars
    ),
    default_constraint = plan$support_constraint,
    project_initial_stats = function(stats, effect_map) {
      stats[, effect_map, drop = FALSE]
    },
    finish_output = function(out, constraint) {
      if (is.null(constraint)) {
        return(out)
      }
      out$support_mask <- preprocess_support_mask(
        constraint,
        model = spec$model,
        nodes = nodes,
        nodes2 = nodes2,
        symmetric = FALSE,
        snapshot_times = out$event_time,
        src = src,
        prep_envir = prep_envir
      )
      # Fold the constraint into `active_sender` during preprocessing: the
      # row-reduction becomes net crossings on the availability
      # object, and the estimation-time recombination is dropped.
      fold_active_sender_support(out, out$support_mask, active_dyad_init)
    },
    scalar_entity = "sender"
  )
}

# Expand an operand effect's `(node1, node2, replace)` delta into the full-matrix
# cells + values it changes, per its broadcast kind: a point (0)
# delta touches its own cells; an alter (1) delta sets whole receiver columns;
# an ego (2) delta sets whole sender rows; a global (3) delta sets the whole
# matrix. Used to keep an operand's live matrix current so interaction products
# can be recomputed.
expand_operand_update <- function(updates, kind, n1, n2) {
  node1 <- updates[, "node1"]
  node2 <- updates[, "node2"]
  repl <- updates[, "replace"]
  if (kind == 0L) {
    return(list(cells = cbind(node1, node2), vals = repl))
  }
  if (kind == 3L) {
    cells <- cbind(rep(seq_len(n1), times = n2), rep(seq_len(n2), each = n1))
    return(list(cells = cells, vals = rep(repl[length(repl)], n1 * n2)))
  }
  if (kind == 1L) {
    cells <- cbind(
      rep(seq_len(n1), times = length(node2)),
      rep(node2, each = n1)
    )
    return(list(cells = cells, vals = rep(repl, each = n1)))
  }
  cells <- cbind(
    rep(node1, each = n2),
    rep(seq_len(n2), times = length(node1))
  )
  list(cells = cells, vals = rep(repl, each = n2))
}

# Deduplicate the accumulated interaction cell matrix (rows are (i, j) pairs)
# via a single linear key, so a cell touched by several operands in one event is
# emitted once.
dedup_cells <- function(cells, n1) {
  key <- cells[, 1] + (cells[, 2] - 1) * n1
  cells[!duplicated(key), , drop = FALSE]
}

#' Dyad-indexed recipe kernel
#'
#' Shared event loop for the dyad-indexed model variants.
#' It mirrors `run_sender_recipe_loop()` over the same three recipe input
#' structures but produces dyad-shaped statistics:
#' `initial_stats` is kept in the engine-native `n1 x n2 x nEffects` (3D)
#' form and the flat `stat_mat_update` buffer carries `node2` in its second
#' row. Right-censored events are stored only when the configuration sets
#' `right_censored = TRUE` (rate models with a time intercept); choice
#' configurations store dependent rows only, so the combined buffer carries
#' exclusively `is_dependent = 1` rows. Global-attribute events are handled
#' as in the sender kernel so future choice-model interaction
#' support only touches the effects layer.
#'
#' The per-event simulation hook and parallel chunk preprocessing extension
#' points attach to this kernel on the same terms documented
#' for `run_sender_recipe_loop()`.
#'
#' @param spec a `spec_map` (built by `build_spec_map()`) carrying a `dyad_spec`
#'   class; the effect closures, per-term window parameters, link matrices,
#'   plan, call templates, and node sets are unpacked from it.
#' @inheritParams run_sender_recipe_loop
#'
#' @return a list of class preprocessed.goldfish
#' @noRd
NULL

# Walk a flat presence crossings buffer (init + (node, replace) updates keyed by
# a per-event cumulative pointer) to the per-event length-n logical vector it
# encodes. Shared by the sender/dyad availability folds.
walk_presence_buffer <- function(init, update, pointer, n_stored) {
  cur <- init
  res <- vector("list", n_stored)
  prev <- 0L
  for (e in seq_len(n_stored)) {
    hi <- if (!is.null(pointer)) pointer[e] else 0L
    if (hi > prev) {
      cols <- (prev + 1L):hi
      cur[update[1L, cols]] <- as.logical(update[2L, cols])
    }
    prev <- hi
    res[[e]] <- cur
  }
  res
}

# Re-encode a per-event sequence of length-n logical vectors as init + net
# crossings (node, replace) with a per-event cumulative pointer (event 1 in the
# init, its slice empty; later events emit only changed entries).
crossings_from_vectors <- function(vecs) {
  n_stored <- length(vecs)
  n_changes <- integer(n_stored)
  nodes <- vector("list", n_stored)
  repl <- vector("list", n_stored)
  for (e in seq_len(n_stored)[-1L]) {
    ch <- which(vecs[[e]] != vecs[[e - 1L]])
    n_changes[e] <- length(ch)
    nodes[[e]] <- ch
    repl[[e]] <- as.numeric(vecs[[e]][ch])
  }
  nv <- unlist(nodes, use.names = FALSE)
  rv <- unlist(repl, use.names = FALSE)
  list(
    init = vecs[[1L]],
    update = if (length(nv) > 0L) rbind(nv, rv) else matrix(0, 2L, 0L),
    pointer = cumsum(n_changes)
  )
}

# Fold a dyad-loop support_constraint into `active_dyad` at its minimal encoding
# during preprocessing, from the per-event mask snapshots.
# Per family: DyNAM-choice/coordination fold receiver presence ∩ support
# (NOT sender presence); REM folds BOTH presences ∩ support. The stored encoding
# is decided statically from `mask_kind`:
#   alter  (choice, alter/scalar support): one length-n2 receiver vector;
#   outer  (REM, or choice + ego support): two factor vectors (active_sender f1,
#          active_dyad f2), cell (i,j) = f1[i] & f2[j];
#   point  (a point support atom): dense n1 x n2 init + net (node1, node2)
#          point flips.
# Operands stay loop-internal; only net effective flips (or factor flips) are
# emitted. The raw receiver presence is stashed on `support_mask` for the
# fail-fast validation, which needs it unfolded.
fold_active_dyad_support <- function(
  out,
  support_mask,
  spec,
  mask_kind,
  opportunitiesList = NULL
) {
  n_stored <- length(out$event_time)
  if (n_stored == 0L) {
    return(out)
  }
  n1 <- length(out$active_sender_init)
  n2 <- length(out$active_dyad_init)
  support <- support_mask$support
  has_opportunity <- !is.null(opportunitiesList)
  encoding <- active_dyad_encoding_decide(
    risk_set_encoding(spec),
    mask_kind,
    has_opportunity
  )
  # Standard/ordinal REM and DyNAM coordination all fold BOTH presences ∩ their
  # support atoms into a dense point `active_dyad` — the risk-set mask each engine
  # consumes directly, replacing the per-event `active_dyad_mask` snapshot. These families
  # share a dyadic / two-sided risk set (only the normalizer differs:
  # timespan-weighted Poisson, multinomial, or the mutual `getLikelihoodMM`
  # product), so one fold serves them. Coordination (`DyNAM-MM`) is additionally
  # symmetrised so `(i, j)` is available iff both directions are
  # allowed — required for the mutual likelihood. Every DyNAM choice encoding
  # folds below: alter/scalar as the receiver vector, point (dyadic atom or
  # opportunity list) and ego-kind (outer) as dense point row flips.
  if (risk_set_is_dyadic(spec)) {
    return(fold_active_dyad_support_rem(
      out,
      support,
      n1,
      n2,
      n_stored,
      symmetric = risk_set_symmetrize(spec)
    ))
  }

  recv <- walk_presence_buffer(
    out$active_dyad_init,
    out$active_dyad_update,
    out$active_dyad_update_pointer,
    n_stored
  )
  out$support_mask$receiver_presence_init <- out$active_dyad_init

  if (identical(encoding, "alter")) {
    # An alter/scalar mask is column-broadcast, so any row is the alter vector.
    folded <- lapply(
      seq_len(n_stored),
      function(e) recv[[e]] & support[[e]][1L, ]
    )
    cr <- crossings_from_vectors(folded)
    out$active_dyad_init <- cr$init
    out$active_dyad_update <- cr$update
    out$active_dyad_update_pointer <- cr$pointer
    out$active_dyad_encoding <- "alter"
    out$active_dyad_folded <- TRUE
  } else {
    # point/outer: fold receiver presence ∩ the sender's support row ∩
    # opportunity into the dense point buffer. Covers a genuinely dyadic (point)
    # atom, any atom together with a user opportunity list, and an ego-kind
    # (outer) atom whose support row is sender-constant. The choice risk set
    # reads only the event sender's row, so only that row is emitted.
    senders <- out$event_sender
    opp_row <- function(e) {
      if (!has_opportunity) {
        return(rep(TRUE, n2))
      }
      opp <- opportunitiesList[[e]]
      if (is.null(opp)) rep(TRUE, n2) else seq_len(n2) %in% opp
    }
    desired <- lapply(
      seq_len(n_stored),
      function(e) recv[[e]] & support[[e]][senders[[e]], ] & opp_row(e)
    )
    out <- build_active_dyad_point(out, recv, desired, senders, n1, n2)
  }

  out
}

# Fold a standard-REM support_constraint into a dense point `active_dyad`. The
# REM risk set is the whole dyad matrix, so per event the mask is
# `presence1[i] & presence2[j] & support[i, j]` — both presences folded in — and
# the default engine consumes it directly as the per-event risk mask, replacing
# the standalone `active_dyad_mask` snapshot. The raw presences are stashed on
# `support_mask` for the fail-fast validation, which needs them unfolded.
fold_active_dyad_support_rem <- function(
  out,
  support,
  n1,
  n2,
  n_stored,
  symmetric = FALSE
) {
  p1 <- walk_presence_buffer(
    out$active_sender_init,
    out$active_sender_update,
    out$active_sender_update_pointer,
    n_stored
  )
  p2 <- walk_presence_buffer(
    out$active_dyad_init,
    out$active_dyad_update,
    out$active_dyad_update_pointer,
    n_stored
  )
  out$support_mask$sender_presence_init <- out$active_sender_init
  out$support_mask$receiver_presence_init <- out$active_dyad_init
  # Coordination: the mutual likelihood needs `(i, j)` active iff
  # both directions are allowed, so symmetrise the per-event mask. The presence
  # product `outer(p1, p2)` is symmetric for a one-mode model, so `m & t(m)`
  # reduces to symmetrising the support atoms.
  masks <- lapply(
    seq_len(n_stored),
    function(e) {
      m <- outer(p1[[e]], p2[[e]]) & (support[[e]] == 1)
      if (symmetric) {
        m <- m & t(m)
      }
      m
    }
  )
  build_active_dyad_point_full(out, masks, n1, n2)
}

# Assemble the `active_dyad` point encoding from per-event dense n1 x n2 masks
# (REM). Unlike the choice point fold (one sender row per event), the REM risk
# set spans the whole matrix, so every changed cell between consecutive events
# is emitted as a net `(node1, node2, replace)` flip; the first event's mask
# seeds the dense init and each later event emits its diff in event order.
build_active_dyad_point_full <- function(out, masks, n1, n2) {
  n_stored <- length(masks)
  init <- masks[[1L]]
  cur <- init
  n_changes <- integer(n_stored)
  node1 <- vector("list", n_stored)
  node2 <- vector("list", n_stored)
  repl <- vector("list", n_stored)
  for (e in seq_len(n_stored)[-1L]) {
    d <- masks[[e]]
    ch <- which(cur != d)
    n_changes[e] <- length(ch)
    node1[[e]] <- ((ch - 1L) %% n1) + 1L
    node2[[e]] <- ((ch - 1L) %/% n1) + 1L
    repl[[e]] <- as.numeric(d[ch])
    cur <- d
  }
  n1v <- unlist(node1, use.names = FALSE)
  n2v <- unlist(node2, use.names = FALSE)
  rv <- unlist(repl, use.names = FALSE)

  out$active_dyad_init <- init
  out$active_dyad_update <- if (length(n1v) > 0L) {
    rbind(n1v, n2v, rv)
  } else {
    matrix(0, 3L, 0L)
  }
  out$active_dyad_update_pointer <- cumsum(n_changes)
  out$active_dyad_encoding <- "point"
  out$active_dyad_folded <- TRUE
  out
}

# Fold the deprecated opportunity list into `active_dyad` at the point encoding.
# Opportunity is event-indexed and sender-specific: for each
# stored (choice) event `e` with sender `s`, the allowed receiver set is the
# folded receiver availability intersected with `opportunitiesList[[e]]`.
# Because the choice risk set reads only sender `s`'s row per event, the
# maintained dense n1 x n2 buffer is updated in row `s` alone — the first
# event's set lands in the init and each later event emits net
# `(node1 = s, node2 = j, replace)` flips against that row's previous stored
# value. Rows for senders not yet observed are never read before their first
# event overwrites them, so their init value is immaterial (seeded with the
# event-1 receiver presence).
fold_active_dyad_opportunity <- function(out, opportunitiesList) {
  n_stored <- length(out$event_time)
  if (n_stored == 0L) {
    return(out)
  }
  n1 <- length(out$active_sender_init)
  n2 <- length(out$active_dyad_init)
  recv <- walk_presence_buffer(
    out$active_dyad_init,
    out$active_dyad_update,
    out$active_dyad_update_pointer,
    n_stored
  )
  senders <- out$event_sender
  # `seq_len(n2) %in% opportunitiesList[[e]]` mirrors the estimation-time
  # recompute exactly (an all-TRUE row when the event has no restriction).
  desired <- lapply(seq_len(n_stored), function(e) {
    opp <- opportunitiesList[[e]]
    if (is.null(opp)) recv[[e]] else recv[[e]] & (seq_len(n2) %in% opp)
  })
  build_active_dyad_point(out, recv, desired, senders, n1, n2)
}

# Assemble the `active_dyad` point encoding from per-event desired receiver rows.
# Shared by the opportunity fold and the point-kind support
# fold. Because the choice risk set reads only the event sender's row, only that
# row is emitted: the first event's row seeds the dense `n1 x n2` init (all other
# rows carry the event-1 receiver presence, immaterial until each sender's first
# event overwrites its row) and each later event emits net
# `(node1 = sender, node2 = j, replace)` flips against the row's stored value.
build_active_dyad_point <- function(out, recv, desired, senders, n1, n2) {
  n_stored <- length(desired)
  cur <- matrix(recv[[1L]], nrow = n1, ncol = n2, byrow = TRUE)
  cur[senders[[1L]], ] <- desired[[1L]]
  init <- cur
  n_changes <- integer(n_stored)
  node1 <- vector("list", n_stored)
  node2 <- vector("list", n_stored)
  repl <- vector("list", n_stored)
  for (e in seq_len(n_stored)[-1L]) {
    s <- senders[[e]]
    ch <- which(cur[s, ] != desired[[e]])
    n_changes[e] <- length(ch)
    node1[[e]] <- rep.int(s, length(ch))
    node2[[e]] <- ch
    repl[[e]] <- as.numeric(desired[[e]][ch])
    cur[s, ] <- desired[[e]]
  }
  n1v <- unlist(node1, use.names = FALSE)
  n2v <- unlist(node2, use.names = FALSE)
  rv <- unlist(repl, use.names = FALSE)

  out$active_dyad_init <- init
  out$active_dyad_update <- if (length(n1v) > 0L) {
    rbind(n1v, n2v, rv)
  } else {
    matrix(0, 3L, 0L)
  }
  out$active_dyad_update_pointer <- cumsum(n_changes)
  out$active_dyad_encoding <- "point"
  out$active_dyad_folded <- TRUE
  out
}

run_dyad_recipe_loop <- function(
  spec,
  startTime = NULL,
  endTime = NULL,
  right_censored = FALSE,
  intercept_scalars = FALSE,
  progress = FALSE,
  prep_envir = new.env(),
  writer = writer_default(),
  opportunitiesList = NULL,
  consumer_specs = NULL,
  ...
) {
  ctx <- prepare_recipe_context(
    spec,
    startTime,
    endTime,
    prep_envir,
    sub_model = "choice",
    progress = progress
  )
  # Splat the shared setup (spec unpack, streams, window, cache, state,
  # schedule, composition, lookups) into this frame; each loop then shapes its
  # own `initial_stats` (2D sender kernel vs 3D dyad array).
  list2env(ctx, environment())

  initial_stats <- array(0, dim = c(n1, n2, nEffects))
  initial_stats[,, seq_len(n_fun)] <- array(
    unlist(lapply(stat_cache, "[[", "stat")),
    dim = c(n1, n2, n_fun)
  )
  stat_cache <- lapply(stat_cache, "[[", "cache")

  # Interaction state. Keep a live full n1 x n2 value for every
  # operand feeding an interaction, seeded from its initial slice, and updated in
  # place as its effect emits deltas (per its broadcast kind). Each interaction's
  # column is the elementwise product of its operands' live matrices; its
  # initial slice is seeded here and its deltas are emitted as point updates when
  # any operand changes (second-hop routing).
  op_kind <- plan$effects$broadcast_kind
  op_state <- new.env(parent = emptyenv())
  # Per-event accumulator of interaction operand cells touched, keyed
  # by interaction gid; refilled in the routing loop, emptied after each event's
  # interaction emission.
  dirty_inter <- list()
  if (n_inter > 0) {
    operand_gids <- sort(unique(unlist(plan$interactions)))
    for (og in operand_gids) {
      assign(as.character(og), initial_stats[,, og], envir = op_state)
    }
    for (ig in inter_ids) {
      ops <- plan$interactions[[as.character(ig)]]
      prod_mat <- get(as.character(ops[1]), envir = op_state)
      for (o in ops[-1]) {
        prod_mat <- prod_mat * get(as.character(o), envir = op_state)
      }
      initial_stats[,, ig] <- prod_mat
    }
  }

  call_effect_template <- function(
    template,
    gid,
    shape,
    event_args,
    net_update,
    att_update,
    event_order,
    inter_event_time
  ) {
    args <- c(
      list(
        network = if (template$n_networks == 1L) {
          state$networks[[template$net_keys]]
        } else if (template$n_networks > 1L) {
          lapply(template$net_keys, function(k) state$networks[[k]])
        } else {
          list()
        },
        attribute = if (template$n_attributes == 1L) {
          state[[template$att_components]][[template$att_keys]]
        } else if (template$n_attributes > 1L) {
          lapply(
            seq_len(template$n_attributes),
            function(j) {
              state[[template$att_components[j]]][[template$att_keys[j]]]
            }
          )
        } else {
          list()
        },
        cache = stat_cache[[gid]],
        n1 = n1,
        n2 = n2,
        net_update = net_update,
        att_update = att_update,
        event_order = event_order,
        inter_event_time = inter_event_time
      ),
      event_args
    )
    do.call(template$fun, args[template$args_by_shape[[shape]]])
  }

  # One consumer per output object: the single default output, or one per
  # modeled flavor reading the shared union walk. Each owns its writer, its
  # projection onto its own statistics columns, and its own pending buffers.
  consumers <- init_consumers(
    consumer_specs,
    writer = writer,
    right_censored = right_censored,
    spec = spec,
    dims = list(
      nEffects = nEffects,
      n1 = n1,
      n2 = n2,
      is_sender = inherits(spec, "sender_spec"),
      n_dependent = nrow(events[[1L]]),
      max_store = schedule$n + 1L
    ),
    initial_stats_fn = function() initial_stats
  )
  rc_consumers <- Filter(function(cs) cs$right_censored, consumers)

  bcast_kind <- plan$effects$broadcast_kind

  i_total_events <- 0L
  i_dependent_events <- 0L
  time <- startTime
  interval <- 0
  final_step <- FALSE

  if (progress) {
    cat("Preprocessing events.\n", startTime, endTime, schedule$n)
    pb <- utils::txtProgressBar(max = schedule$n, char = "*", style = 3)
    dot_events <- ifelse(schedule$n > 50, ceiling(schedule$n / 50), 1)
  }

  for (k in seq_len(schedule$n)) {
    i_total_events <- i_total_events + 1L
    next_event_time <- schedule$time[k]
    if (hasStartTime || hasEndTime) {
      if (isValidEvent && next_event_time <= endTime) {
        interval <- next_event_time - time
      } else if (isValidEvent && next_event_time > endTime) {
        interval <- endTime - time
        next_event_time <- endTime
        final_step <- TRUE
      } else if (!isValidEvent && next_event_time >= startTime) {
        interval <- next_event_time - startTime
        isValidEvent <- TRUE
      }
    } else {
      interval <- next_event_time - time
    }

    time <- next_event_time

    isDependent <- schedule$dependent[k] && !final_step

    if (progress && i_total_events %% dot_events == 0) {
      utils::setTxtProgressBar(pb, i_total_events)
    }

    # `event_order` is derived as the difference of these two counters, so a
    # dependent event must advance both whether or not the observation window
    # has opened. Advancing only the total would leave a gap across the
    # burn-in fold, and an effect that reads adjacency in the event stream --
    # trans(history = "consecutive") -- would find none.
    if (isDependent) {
      i_dependent_events <- 1L + i_dependent_events
    }

    if (isValidEvent && isDependent) {
      if (schedule$shape[k] == "node") {
        ev_sender <- schedule$node[k]
        ev_receiver <- schedule$node[k]
      } else {
        ev_sender <- schedule$sender[k]
        ev_receiver <- schedule$receiver[k]
      }
      route_dependent_event(
        consumers,
        rc_consumers,
        flavor = schedule$flavor[k],
        event_info = list(
          is_dependent = 1L,
          interval = interval,
          time = time,
          sender = ev_sender,
          receiver = ev_receiver
        )
      )
    } else if (!isDependent) {
      if (isValidEvent && length(rc_consumers) > 0L && interval > 0) {
        if (final_step) {
          # The closing row is the window ending, not an event. The event that
          # triggered the stop lies outside the window, and its sender and
          # receiver would read as a real observation everywhere the row
          # surfaces.
          ev_sender <- NA_integer_
          ev_receiver <- NA_integer_
        } else if (schedule$shape[k] == "global") {
          ev_sender <- NA_integer_
          ev_receiver <- NA_integer_
        } else if (schedule$shape[k] == "node") {
          ev_sender <- schedule$node[k]
          ev_receiver <- schedule$node[k]
        } else {
          ev_sender <- schedule$sender[k]
          ev_receiver <- schedule$receiver[k]
        }
        route_right_censored_event(
          rc_consumers,
          list(
            is_dependent = 0L,
            interval = interval,
            time = time,
            sender = ev_sender,
            receiver = ev_receiver
          )
        )
      }

      if (!final_step) {
        oid <- schedule$target[k]
        component <- plan$objects$component[oid]
        key <- plan$objects$key[oid]
        shape <- schedule$shape[k]
        is_undirected_net <- plan$objects$is_undirected[oid]

        if (shape == "global") {
          replace_value <- schedule$value[[k]]
          if (is.na(replace_value)) {
            replace_value <- 0
          }
          event_args <- list(replace = replace_value)
        } else if (shape == "node") {
          event_node <- schedule$node[k]
          if (schedule$semantics[k] == "increment") {
            increment_value <- schedule$value[[k]]
            if (is.na(increment_value)) {
              increment_value <- 0
            }
            replace_value <-
              state[[component]][[key]][event_node] + increment_value
          } else {
            replace_value <- schedule$value[[k]]
            if (is.na(replace_value)) {
              if (identical(plan$objects$policy[oid], "as_category")) {
                # Under the as-category policy a missing event value is the
                # reserved level, not a summary over the other nodes -- the same
                # recode the initial table received.
                replace_value <- IMPUTATION_MISSING_LEVEL
              } else {
                # Impute from the node's own mode category by the summary its
                # recorded type selects -- a bare mean would write NA into a
                # categorical vector, failing the next update's comparison.
                replace_value <- impute_nodal_value(
                  state[[component]][[key]],
                  event_node,
                  attr(state, "strata")[[component]],
                  plan$objects$value_type[oid]
                )
              }
            }
          }
          event_args <- list(node = event_node, replace = replace_value)
        } else {
          event_sender <- schedule$sender[k]
          event_receiver <- schedule$receiver[k]
          if (schedule$semantics[k] == "increment") {
            increment_value <- schedule$value[[k]]
            if (is.na(increment_value)) {
              increment_value <- 0
            }
            replace_value <-
              state$networks[[key]][event_sender, event_receiver] +
              increment_value
          } else {
            replace_value <- schedule$value[[k]]
            if (is.na(replace_value)) replace_value <- 0
          }
          if (replace_value < 0) {
            warning(
              "You are dissolving a tie which doesn't exist!",
              call. = FALSE
            )
          }
          event_args <- list(
            sender = event_sender,
            receiver = event_receiver,
            replace = replace_value
          )
        }

        for (gid in plan$routing[[oid]]) {
          template <- effects_template[[gid]]
          net_update_pos <- net_update_lookup[oid, gid]
          if (is.na(net_update_pos)) {
            net_update_pos <- NULL
          }
          att_update_pos <- att_update_lookup[oid, gid]
          if (is.na(att_update_pos)) {
            att_update_pos <- NULL
          }

          effect_update <- call_effect_template(
            template,
            gid,
            shape,
            event_args,
            net_update_pos,
            att_update_pos,
            i_total_events - i_dependent_events,
            interval
          )

          if (!is.null(attr(effect_update$cache, "last_update"))) {
            attr(stat_cache[[gid]], "last_update") <- attr(
              effect_update$cache,
              "last_update"
            )
          }

          updates <- effect_update$changes
          if (
            !is.null(effect_update$cache) && !is.null(effect_update$changes)
          ) {
            stat_cache[[gid]] <- effect_update$cache
          }

          if (is_undirected_net) {
            event_args2 <- event_args
            event_args2$sender <- event_args$receiver
            event_args2$receiver <- event_args$sender
            effect_update2 <- call_effect_template(
              template,
              gid,
              shape,
              event_args2,
              net_update_pos,
              att_update_pos,
              i_total_events - i_dependent_events,
              interval
            )
            if (
              !is.null(effect_update2$cache) &&
                !is.null(effect_update2$changes)
            ) {
              stat_cache[[gid]] <- effect_update2$cache
            }
            updates <- rbind(updates, effect_update2$changes)
          }

          if (!is.null(updates)) {
            # Interaction second-hop: if this effect is an operand,
            # apply its delta to its live matrix and record the touched cells for
            # each interaction it feeds, so the product columns are refreshed
            # after the routing loop.
            if (n_inter > 0L && gid <= n_fun) {
              feeds <- plan$operand_of[[as.character(gid)]]
              if (!is.null(feeds)) {
                exp <- expand_operand_update(updates, op_kind[gid], n1, n2)
                om <- get(as.character(gid), envir = op_state)
                om[exp$cells] <- exp$vals
                assign(as.character(gid), om, envir = op_state)
                for (ig in feeds) {
                  igc <- as.character(ig)
                  dirty_inter[[igc]] <- rbind(dirty_inter[[igc]], exp$cells)
                }
              }
            }
            if (hasStartTime && next_event_time < startTime) {
              initial_stats[cbind(
                updates[, "node1"],
                updates[, "node2"],
                gid
              )] <- updates[, "replace"]
            } else if (bcast_kind[gid] != 0L) {
              bc_block <- broadcast_entries_from_updates(
                updates,
                bcast_kind[gid],
                gid
              )
              for (cs in consumers) {
                consumer_accumulate_broadcast(cs, bc_block)
              }
            } else {
              block <- rbind(
                updates[, "node1"] - 1,
                updates[, "node2"] - 1,
                gid - 1,
                updates[, "replace"]
              )
              for (cs in consumers) {
                consumer_accumulate_point(cs, block)
              }
            }
          }
        }

        # Emit each touched interaction's product delta: recompute
        # the product over its operands at the union of cells changed this event
        # and route it as a point update (its own column). Emitted after the
        # routing loop so all operand deltas for the event are applied first.
        if (n_inter > 0L && length(dirty_inter) > 0L) {
          for (igc in names(dirty_inter)) {
            ig <- as.integer(igc)
            cells <- dedup_cells(dirty_inter[[igc]], n1)
            ops <- plan$interactions[[igc]]
            prodv <- get(as.character(ops[1]), envir = op_state)[cells]
            for (o in ops[-1]) {
              prodv <- prodv * get(as.character(o), envir = op_state)[cells]
            }
            if (hasStartTime && next_event_time < startTime) {
              initial_stats[cbind(cells[, 1], cells[, 2], ig)] <- prodv
            } else {
              block <- rbind(cells[, 1] - 1, cells[, 2] - 1, ig - 1, prodv)
              for (cs in consumers) {
                consumer_accumulate_point(cs, block)
              }
            }
          }
          dirty_inter <- list()
        }

        if (shape == "global") {
          state$globals[[key]] <- event_args$replace
        } else if (shape == "node") {
          state[[component]][[key]][event_args$node] <- event_args$replace
        } else {
          state$networks[[key]][event_args$sender, event_args$receiver] <-
            event_args$replace
          if (is_undirected_net) {
            state$networks[[key]][event_args$receiver, event_args$sender] <-
              event_args$replace
          }
        }
      }
    }

    if (final_step) break
  }

  # The window closes at the end time even when the schedule runs out before
  # reaching it. Only the branch that meets an out-of-window event used to
  # write the closing row, so the exposure after the last event left the
  # likelihood entirely, biasing the baseline rate upward. Whether the row is
  # stored is a property of the likelihood: a family with no compensator keeps
  # no right-censoring consumer, and a row there would contribute exactly zero
  # while changing the interval count.
  trailing_interval <- endTime - time
  if (
    isValidEvent &&
      !final_step &&
      length(rc_consumers) > 0L &&
      trailing_interval > 0
  ) {
    route_right_censored_event(
      rc_consumers,
      list(
        is_dependent = 0L,
        interval = trailing_interval,
        time = endTime,
        sender = NA_integer_,
        receiver = NA_integer_
      )
    )
  }

  if (progress) {
    utils::setTxtProgressBar(pb, schedule$n)
    close(pb)
  }

  finalize_consumers(
    consumers,
    consumer_specs,
    tail = list(
      spec = spec,
      initial_stats = initial_stats,
      active_sender_init = active_sender_init,
      active_sender_changes = active_sender_changes,
      active_dyad_init = active_dyad_init,
      active_dyad_changes = active_dyad_changes,
      start_time = startTime,
      end_time = endTime,
      intercept_scalars = intercept_scalars
    ),
    default_constraint = plan$support_constraint,
    project_initial_stats = function(stats, effect_map) {
      stats[,, effect_map, drop = FALSE]
    },
    # The support-constraint mask is realized in a self-contained pass over the
    # constraint sub-plan. It is attached additively so the statistics
    # output is untouched; the gather consumer reads it per event. A NULL
    # sub-plan (no constraint) leaves the output unchanged.
    finish_output = function(out, constraint) {
      if (!is.null(constraint)) {
        out$support_mask <- preprocess_support_mask(
          constraint,
          model = spec$model,
          nodes = nodes,
          nodes2 = nodes2,
          symmetric = identical(spec$sub_model, "choice_coordination"),
          snapshot_times = out$event_time,
          src = src,
          prep_envir = prep_envir
        )
        # Fold the constraint into `active_dyad` at its minimal encoding during
        # preprocessing; estimation consumes it via the
        # encoding accessors.
        return(fold_active_dyad_support(
          out,
          out$support_mask,
          spec,
          constraint$mask_kind,
          opportunitiesList = opportunitiesList
        ))
      }
      if (
        !is.null(opportunitiesList) &&
          spec$sub_model %in% c("choice", "choice_coordination")
      ) {
        # The deprecated opportunity list is a point-kind availability
        # contribution: fold it into the `active_dyad` point buffer during the
        # preprocessing pass so estimation reads it through the point accessor
        # instead of recomputing `seq_len(n2) %in% opportunitiesList[[i]]` on
        # every Newton-Raphson iteration. Only the constraint-free case folds
        # here — when a support_constraint is present its (still standalone)
        # mask path intersects the user opportunity list, so both ride together
        # until the mask path is retired with the engine wiring.
        out <- fold_active_dyad_opportunity(out, opportunitiesList)
      }
      out
    },
    # A tie-oriented rate integrates over dyads, so its intercept scalar counts
    # active dyads rather than active senders.
    scalar_entity = "dyad"
  )
}

#' preprocess event and related objects describe in the formula to estimate
#'
#' Create a preprocess.goldfish class object with the update statistics
#' for estimation.
#'
#' @inheritParams estimate
#' @param events list with all
#' @param effects list of effects functions return by
#'   `create_effects_functions()`.
#' @param events_objects_link data.frame output of `get_events_and_objects_link()`.
#' @param events_effects_link data.frame output of `get_events_effects_link()`.
#' @param objects_effects_link data.frame output of `get_objects_effects_link()`.
#' @param nodes character with the object that contains the nodes information
#' @param nodes2 character with the object that contains the nodes information,
#'   different from `nodes` when `is_two_mode = TRUE`.
#' @param is_two_mode logical is it a two mode network?
#' @param startTime numerical start time to preprocess the data
#' @param endTime numerical end time to preprocess the data
#' @param right_censored logical does it consider right censored events?
#' @param progress logical should print progress
#'
#' @return a list of class preprocessed.goldfish
#'
#' @noRd
preprocess_monolith <- function(
  model,
  sub_model,
  events,
  effects,
  window_parameters,
  ignore_rep_parameter,
  events_objects_link,
  events_effects_link,
  objects_effects_link,
  # multiple_parameter,
  nodes,
  nodes2 = nodes,
  is_two_mode,
  # add more parameters
  startTime = min(vapply(events, function(x) min(x$time), double(1))),
  endTime = max(vapply(events, function(x) max(x$time), double(1))),
  right_censored = FALSE,
  opportunitiesList = NULL,
  progress = FALSE,
  prep_envir = new.env()
) {
  # For debugging
  # if (identical(environment(), globalenv())) {
  #   startTime <- min(vapply(events, function(x) min(x$time), double(1)))
  #   endTime <- max(vapply(events, function(x) max(x$time), double(1)))
  #   progress <- FALSE
  # }

  # print(match.call())
  # initialize statistics functions from data objects
  # number of actors
  n1 <- nrow(get(nodes, envir = prep_envir))
  n2 <- nrow(get(nodes2, envir = prep_envir))
  nEffects <- length(effects)

  # check start time and end time are valid values, set flags
  hasEndTime <- FALSE
  hasStartTime <- FALSE
  isValidEvent <- TRUE

  is_window_effect <- !vapply(window_parameters, is.null, logical(1))
  which_event_no_window_effect <- events_effects_link[,
    !is_window_effect,
    drop = FALSE
  ]
  which_event_no_window_effect <- rowSums(!is.na(which_event_no_window_effect))
  which_event_no_window_effect <- c(1, which(which_event_no_window_effect > 0))

  has_ignore_rep <- any(ignore_rep_parameter)

  events_min <- min(vapply(
    events[which_event_no_window_effect],
    function(x) min(x$time),
    double(1)
  ))
  events_max <- max(vapply(
    events[which_event_no_window_effect],
    function(x) max(x$time),
    double(1)
  ))
  if (is.null(endTime)) {
    endTime <- events_max
    if (any(is_window_effect)) hasEndTime <- TRUE
  } else if (endTime != events_max) {
    if (!is.numeric(endTime)) {
      endTime <- as.numeric(endTime)
    }
    if (events_min > endTime) {
      stop("End time smaller than first event time.", call. = FALSE)
    }
    # to solve: if endTime > events_max
    # should it produce censored events? warning?
    # add a fake event to the event list
    # end_time_event <- data.frame(
    #   time = endTime,
    #   sender = NA,
    #   receiver = NA,
    #   replace = NA
    # )
    # events <- c(events, endtime = list(end_time_event))
    hasEndTime <- TRUE
  }
  if (is.null(startTime)) {
    startTime <- events_min
  } else if (startTime != events_min) {
    if (!is.numeric(startTime)) {
      startTime <- as.numeric(startTime)
    }
    if (events_max < startTime) {
      stop("Start time geater than last event time.", call. = FALSE)
    }
    hasStartTime <- TRUE
    if (events_min < startTime) isValidEvent <- FALSE
    # if (events_min > startTime) isValidEvent <- TRUE
    # To solve: if startTime < events_min should be a warning?
  }
  ignore_events <- 1L # event_pos should be correct for initialization

  # impute missing data in objects: 0 for networks and mean for attributes
  imputed <- impute_missing_data(objects_effects_link, envir = prep_envir)

  if (progress) {
    cat("Initializing cache objects and statistical matrices.\n")
  }

  stat_cache <- initialize_cache_stat(
    objects_effects_link = objects_effects_link,
    effects = effects,
    groups_network = NULL,
    window_parameters = window_parameters,
    n1 = n1,
    n2 = n2,
    model = model,
    sub_model = sub_model,
    envir = prep_envir
  )
  is_rate <- model == "DyNAM" && sub_model == "rate"
  if (is_rate) {
    initial_stats <- do.call(cbind, lapply(stat_cache, "[[", "stat"))
  } else {
    initial_stats <- array(
      unlist(lapply(stat_cache, "[[", "stat")),
      dim = c(n1, n2, nEffects)
    )
  }

  stat_cache <- lapply(stat_cache, "[[", "cache")

  # UPDATED ALVARO: logical values indicating the type of information in events
  is_increment_event <- vapply(
    events,
    function(x) "increment" %in% names(x),
    logical(1)
  )
  is_node_event <- vapply(events, function(x) "node" %in% names(x), logical(1))
  is_global_event <- vapply(
    events,
    function(x) !any(c("node", "sender", "receiver") %in% names(x)),
    logical(1)
  )
  is_global_event[1] <- FALSE

  # initialize return objects

  # calculate total of events
  time <- unique(events[[1]]$time)
  if (right_censored) {
    n_right_censored_events <- unique(unlist(lapply(events, function(x) {
      x$time
    })))
    n_total_events <- as.integer(sum(n_right_censored_events <= endTime))
    n_right_censored_events <- setdiff(n_right_censored_events, time)
    if (length(n_right_censored_events) > 1) {
      # count right censored events in the preprocessed window
      n_right_censored_events <- as.integer(sum(
        n_right_censored_events >= startTime &
          n_right_censored_events <= endTime
      ))
      # -1 because the last event is the endTime event, correct if no events
      n_right_censored_events <- ifelse(
        n_right_censored_events > 1,
        n_right_censored_events - 1L,
        0L
      )
    } else {
      n_right_censored_events <- 0L
    }
  } else {
    n_right_censored_events <- 0L
    n_total_events <- as.integer(nrow(events[[1]]))
  }

  n_dependent_events <- ifelse(
    hasStartTime || hasEndTime,
    as.integer(sum(time >= startTime & time <= endTime)),
    as.integer(length(time))
  )
  n_total_change_events <- n_dependent_events + n_right_censored_events
  stats_change <- vector("list", n_total_change_events)
  intervals <- vector("numeric", n_total_change_events)
  is_dependent <- vector("integer", n_total_change_events)
  event_time <- vector("numeric", n_total_change_events)
  event_sender <- vector("integer", n_total_change_events)
  event_receiver <- vector("integer", n_total_change_events)
  final_step <- FALSE
  nodes_obj <- get(nodes, envir = prep_envir)
  nodes2_obj <- get(nodes2, envir = prep_envir)
  active_sender_init <- if (!is.null(nodes_obj$present)) {
    nodes_obj$present
  } else {
    rep(TRUE, n1)
  }
  active_dyad_init <- if (!is.null(nodes2_obj$present)) {
    nodes2_obj$present
  } else {
    rep(TRUE, n2)
  }
  comp_events1 <- attr(nodes_obj, "events")[
    attr(nodes_obj, "dynamic_attribute") == "present"
  ]
  comp_events2 <- attr(nodes2_obj, "events")[
    attr(nodes2_obj, "dynamic_attribute") == "present"
  ]
  active_sender_changes <- if (
    length(comp_events1) > 0 && !is.na(comp_events1[1])
  ) {
    cc <- get(comp_events1[1], envir = prep_envir)
    node_idx1 <- if (is.character(cc$node)) {
      match(cc$node, nodes_obj$label)
    } else {
      as.integer(cc$node)
    }
    lapply(seq_len(nrow(cc)), function(i) {
      list(time = cc$time[i], node = node_idx1[i], replace = cc$replace[i])
    })
  } else {
    list()
  }
  active_dyad_changes <- if (
    length(comp_events2) > 0 && !is.na(comp_events2[1])
  ) {
    cc <- get(comp_events2[1], envir = prep_envir)
    node_idx2 <- if (is.character(cc$node)) {
      match(cc$node, nodes2_obj$label)
    } else {
      as.integer(cc$node)
    }
    lapply(seq_len(nrow(cc)), function(i) {
      list(time = cc$time[i], node = node_idx2[i], replace = cc$replace[i])
    })
  } else {
    list()
  }

  # # Remove duplicates of event lists!

  # initialize loop parameters
  # pointers = [1,1,1](events have three elements:
  # call_dependent(439*4), calls(439*4), friendship(766*4))
  pointers <- rep(1, length(events))
  valid_pointers <- rep(TRUE, length(events))
  if (hasEndTime) {
    valid_pointers <- vapply(events, function(x) x$time[1], double(1)) <=
      endTime
  }
  pointer_temp_right_censored <- 1L
  time <- startTime
  interval <- 0L
  # updates_dependent/updates_intervals: list of 6, each element if NULL
  updates_dependent <- vector("list", nEffects)
  updates_intervals <- vector("list", nEffects)

  # initialize progressbar output, CHANGED ALVARO: add iterators

  # i_right_censored <- 0
  i_dependent_events <- 0L
  i_total_events <- 0L
  if (progress) {
    cat("Preprocessing events.\n", startTime, endTime, n_total_events)
    # # how often print, max 50 prints
    pb <- utils::txtProgressBar(max = n_total_events, char = "*", style = 3)
    dot_events <- ifelse(n_total_events > 50, ceiling(n_total_events / 50), 1)
  }

  # iterate over all event lists
  while (any(valid_pointers)) {
    i_total_events <- i_total_events + 1L
    # times: the timepoint for next events to update in all event lists
    times <- Map(function(e, p) e[p, ]$time, events, pointers) |>
      vapply(identity, numeric(1))
    next_event <- which(valid_pointers)[head(
      which.min(times[valid_pointers]),
      1
    )]
    next_event_time <- times[next_event]
    if (hasStartTime || hasEndTime) {
      if (isValidEvent && next_event_time <= endTime) {
        interval <- next_event_time - time
      } else if (isValidEvent && next_event_time > endTime) {
        interval <- endTime - time
        next_event_time <- endTime
        final_step <- TRUE
      } else if (!isValidEvent && next_event_time >= startTime) {
        interval <- next_event_time - startTime
        isValidEvent <- TRUE
      }
    } else {
      interval <- next_event_time - time
    }

    time <- next_event_time

    isDependent <- next_event == 1 && !final_step

    if (isValidEvent) {
      event_pos <- pointers[1] + pointer_temp_right_censored - ignore_events
    } else if (isDependent && !isValidEvent) {
      ignore_events <- ignore_events + 1L
      event_pos <- 0
    }

    # # CHANGED ALVARO: progress bar
    if (progress && i_total_events %% dot_events == 0) {
      utils::setTxtProgressBar(pb, i_total_events)
    }

    if (progress && i_total_events == n_total_events) {
      utils::setTxtProgressBar(pb, i_total_events)
      close(pb)
    }

    # Distinguish three cases
    #   1. Dependent events (store stats)
    #   2. right-censored events (store stats)
    #   3. update change events (including right-censored events of 2.)
    #      calculate statistics updates
    #      update objects

    # `event_order` is derived as the difference of these two counters, so a
    # dependent event must advance both whether or not the observation window
    # has opened. Advancing only the total would leave a gap across the
    # burn-in fold, and an effect that reads adjacency in the event stream --
    # trans(history = "consecutive") -- would find none.
    if (isDependent) {
      i_dependent_events <- 1L + i_dependent_events
    }

    # 1. store statistic updates for DEPENDENT events
    if (isValidEvent && isDependent) {
      stats_change[[event_pos]] <- updates_dependent
      intervals[[event_pos]] <- interval
      is_dependent[[event_pos]] <- 1L
      event_time[[event_pos]] <- time
      updates_dependent <- vector("list", nEffects)
      updates_intervals <- vector("list", nEffects)
      event <- events[[next_event]][pointers[next_event], ]
      if (is_node_event[next_event]) {
        event_sender[[event_pos]] <- event$node
        event_receiver[[event_pos]] <- event$node
      } else {
        event_sender[[event_pos]] <- event$sender
        event_receiver[[event_pos]] <- event$receiver
      }
    } else if (!isDependent) {
      if (isValidEvent && right_censored && interval > 0) {
        stats_change[[event_pos]] <- updates_intervals
        intervals[[event_pos]] <- interval
        is_dependent[[event_pos]] <- 0L
        event_time[[event_pos]] <- time
        rc_event <- events[[next_event]][pointers[next_event], ]
        if (is_global_event[next_event]) {
          event_sender[[event_pos]] <- NA_integer_
          event_receiver[[event_pos]] <- NA_integer_
        } else if (is_node_event[next_event] && length(rc_event) == 1) {
          event_sender[[event_pos]] <- rc_event
          event_receiver[[event_pos]] <- rc_event
        } else if (is_node_event[next_event]) {
          event_sender[[event_pos]] <- rc_event$node
          event_receiver[[event_pos]] <- rc_event$node
        } else {
          event_sender[[event_pos]] <- rc_event$sender
          event_receiver[[event_pos]] <- rc_event$receiver
        }
        updates_intervals <- vector("list", nEffects)
        pointer_temp_right_censored <- pointer_temp_right_censored + 1
      } # else if (isValidEvent && !final_step && interval > 0) {
      #   time_intervals[[i_dependent_events + 1]] <- interval +
      #     time_intervals[[i_dependent_events + 1]]
      # }

      # 3. update stats and data objects for OBJECT CHANGE EVENTS
      # (all non-dependent events)

      # Two steps are performed for non-dependent events
      #   (0. get objects and update increment columns)
      #   a. Calculate statistic updates for each event that relates
      #     to the data update
      #   b. Update the data objects

      object_name_table <- events_objects_link[next_event, -1]
      object_name <- object_name_table$name
      object <- get_element_from_data_object_table(
        object_name_table,
        envir = prep_envir
      )[[1]]
      is_undirected_net <- FALSE
      if (inherits(object, "network.goldfish")) {
        is_undirected_net <- !attr(object, "directed")
      }

      # # CHANGED ALVARO: avoid dependence in variables position
      if (is_global_event[next_event]) {
        event <- events[[next_event]][
          pointers[next_event],
          "replace",
          drop = FALSE
        ]
        # A global attribute has a single value, so a missing update cannot be
        # imputed from other values: reject it rather than writing an arbitrary
        # zero into the model.
        if (is.na(event$replace)) {
          cli::cli_abort(c(
            "Global attribute {.val {object_name}} has a missing value at time
             {.val {time}}.",
            "x" = "A global attribute has a single value, so a missing update
                   cannot be imputed from other values.",
            "i" = "Give the event an explicit value."
          ))
        }
      } else if (is_increment_event[next_event]) {
        vars_keep <- c(
          if (is_node_event[next_event]) "node" else c("sender", "receiver"),
          "increment"
        )
        event <- events[[next_event]][pointers[next_event], vars_keep]
        # missing data imputation
        if (is_node_event[next_event]) {
          old_value <- object[event$node]
          # if the replace is missing impute by 0 because is increment
          if (is.na(event$increment)) event$increment <- 0
        }
        if (!is_node_event[next_event]) {
          old_value <- object[event$sender, event$receiver]
          # if the replace is missing impute by 0 (not-tie)
          if (is.na(event$increment)) event$increment <- 0
        }
        event$replace <- old_value + event$increment
        event$increment <- NULL
      } else {
        vars_keep <- c(
          if (is_node_event[next_event]) "node" else c("sender", "receiver"),
          "replace"
        )
        event <- events[[next_event]][pointers[next_event], vars_keep]
        # missing data imputation
        if (is_node_event[next_event] && is.na(event$replace)) {
          # The legacy engine carries one node set with no modes, so the pool is
          # every other node -- one implicit category. Routing through the typed
          # resolver still selects a mean or a most-common value by type, so a
          # categorical attribute no longer gets a mean written into it.
          event$replace <- impute_nodal_value(
            object,
            event$node,
            NULL,
            attribute_value_type(object)
          )
        }
        if (!is_node_event[next_event] && is.na(event$replace)) {
          # if the replace is missing impute by 0 (not-tie)
          event$replace <- 0
        }
      }

      # network update an negative replacement throws a warning
      if (
        !is_node_event[next_event] &&
          !is_global_event[next_event] &&
          event$replace < 0
      ) {
        warning("You are dissolving a tie which doesn't exist!", call. = FALSE)
      }

      ## 3a. calculate statistics changes
      if (!final_step) {
        for (id in which(!is.na(events_effects_link[next_event, ]))) {
          # create the ordered list for the objects
          objects_to_pass <- objects_effects_link[, id][
            !is.na(objects_effects_link[, id])
          ]
          names <- rownames(objects_effects_link)
          names <- names[!is.na(objects_effects_link[, id])]
          ordered_names <- names[order(objects_to_pass)]
          ordered_object_table <- get_data_objects(list(list(
            "",
            ordered_names
          )))
          .objects <- get_element_from_data_object_table(
            ordered_object_table,
            envir = prep_envir
          )
          # identify class to feed effects functions
          obj_cat <- assign_category_object(.objects)
          att_ids <- which(obj_cat == "attribute")
          net_ids <- which(obj_cat == "network")
          if (attr(obj_cat, "none_class")) {
            stop(
              "An object is not assigned either as network or attibute",
              paste(names[attr(obj_cat, "many_classes") != 1], collapse = ", "),
              "check the class of the object.",
              call. = FALSE
            )
          }

          # call effects function with required arguments
          .args_fun <- list(
            network = if (length(.objects[net_ids]) == 1) {
              .objects[net_ids][[1]]
            } else {
              .objects[net_ids]
            },
            attribute = if (length(.objects[att_ids]) == 1) {
              .objects[att_ids][[1]]
            } else {
              .objects[att_ids]
            },
            cache = stat_cache[[id]],
            n1 = n1,
            n2 = n2,
            net_update = if (length(.objects[net_ids]) <= 1) {
              NULL
            } else {
              which(ordered_names == object_name)
            },
            att_update = if (length(.objects[att_ids]) <= 1) {
              NULL
            } else {
              which(ordered_names == object_name)
            },
            # add more parameters:
            # - consecutive updates in closure effects
            # - inter_event_time (since last event right-censored included):
            #   exponentially weighted decay effects
            event_order = i_total_events - i_dependent_events,
            inter_event_time = interval
          )
          effect_update <- call_fun(
            effects,
            id,
            "effect",
            c(.args_fun, event),
            " cannot update \n",
            colnames(objects_effects_link)[id]
          )

          # CHANGED - MABEL - need to update cache attributes for last_update when
          # trans or cycle and history = "consecutive"
          if (!is.null(attr(effect_update$cache, 'last_update'))) {
            attr(stat_cache[[id]], "last_update") <- attr(
              effect_update$cache,
              'last_update'
            )
          }

          updates <- effect_update$changes
          # if cache and changes are not null update cache
          if (
            !is.null(effect_update$cache) && !is.null(effect_update$changes)
          ) {
            stat_cache[[id]] <- effect_update$cache
          }

          if (is_undirected_net) {
            event2 <- event
            event2$sender <- event$receiver
            event2$receiver <- event$sender
            if (
              !is.null(effect_update$cache) &&
                !is.null(effect_update$changes)
            ) {
              # styler: off
              .args_fun$cache <- stat_cache[[id]]
            }
            effect_update2 <- call_fun(
              effects,
              id,
              "effect",
              c(.args_fun, event2),
              " cannot update \n",
              colnames(objects_effects_link)[id]
            )

            if (
              !is.null(effect_update2$cache) &&
                !is.null(effect_update2$changes)
            ) {
              # styler: off
              stat_cache[[id]] <- effect_update2$cache
            }
            updates2 <- effect_update2$changes
            updates <- rbind(updates, updates2)
          }

          if (!is.null(updates)) {
            if (hasStartTime && next_event_time < startTime) {
              if (is_rate) {
                initial_stats[cbind(updates[, "node1"], id)] <- updates[,
                  "replace"
                ]
              } else {
                initial_stats[cbind(
                  updates[, "node1"],
                  updates[, "node2"],
                  id
                )] <-
                  updates[, "replace"]
              }
            } else {
              # CHANGED WEIGUTIAN: UPDATE THE STAT MAT
              # AND IMPUTE THE MISSING VALUES
              # if (anyNA(stat_cache[[id]][["stat"]])) {
              #   position_NA <- which(
              #     is.na(stat_cache[[id]][["stat"]]),
              #     arr.ind  = TRUE
              #   )
              #   average <- mean(stat_cache[[id]][["stat"]], na.rm = TRUE)
              #   updates[is.na(updates[, "replace"]), "replace"] <- average
              #   stat_cache[[id]][["stat"]][position_NA] <- average
              # }

              updates_dependent[[id]] <- rbind(updates_dependent[[id]], updates)
              updates_intervals[[id]] <- rbind(updates_intervals[[id]], updates)
            }
          }
        }
      }

      # 3b. Update the data object
      if (!final_step) {
        if (is_global_event[next_event]) {
          object <- event$replace
        } else if (!is.null(event$node)) {
          object[event$node] <- event$replace
        } else if (!is.null(event$sender)) {
          # [sender, receiver] value: replace value of the event
          object[event$sender, event$receiver] <- event$replace
          if (is_undirected_net) {
            object[event$receiver, event$sender] <- event$replace
          }
        }
        # Assign object
        assign("object", object, envir = prep_envir)
        eval(
          parse(text = paste(object_name, "<- object")),
          envir = prep_envir,
          enclos = parent.frame()
        )
      }
    } # end 3. (!dependent)

    # update events pointers
    pointers[next_event] <- 1 + pointers[next_event]
    valid_pointers <- pointers <= vapply(events, nrow, integer(1)) &
      times <= endTime

    # Stop at the end of the window rather than draining the remaining
    # pointers. Nothing past it is stored, so visiting those events only costs
    # time -- and the recipe loops already stop here, which is the disagreement
    # that mattered: two preprocessing paths cannot mean different things by
    # the end of the observation window.
    if (final_step) break
  }

  if (progress && utils::getTxtProgressBar(pb) < n_total_events) {
    close(pb)
  }

  return(structure(
    list(
      initial_stats = initial_stats,
      stats_change = stats_change,
      intervals = intervals,
      is_dependent = is_dependent,
      event_time = event_time,
      event_sender = event_sender,
      event_receiver = event_receiver,
      event_pos = seq_len(length(stats_change)),
      active_sender_init = active_sender_init,
      active_sender_changes = active_sender_changes,
      active_dyad_init = active_dyad_init,
      active_dyad_changes = active_dyad_changes,
      active_dyad_encoding = if (identical(model, "REM")) "outer" else "alter",
      start_time = startTime,
      end_time = endTime
    ),
    class = "preprocessed.goldfish"
  ))
}

#' initialize the cache object or the stat matrices
#'
#' @param objects_effects_link data.frame output of `get_objects_effects_link()`
#' @param effects list of effects functions return by `create_effects_functions()`
#' @param groups_network matrix that defines groups partition in DyNAMi
#' @param window_parameters NULL or numeric value with the size of the window
#' @param n1 int `nrow(network)`
#' @param n2 int `ncol(network)`
#' @param model character
#' @param sub_model character
#' @param envir environment where get the objects
#'
#' @return a list of size length(effects):
#'   list with initial cache object and stat matrices
#'
#' @noRd
initialize_cache_stat <- function(
  objects_effects_link,
  effects,
  groups_network,
  window_parameters,
  n1,
  n2,
  model,
  sub_model,
  envir = environment(),
  src = NULL
) {
  obj_table <- get_data_objects(
    list(rownames(objects_effects_link)),
    remove_first = FALSE
  )
  src <- src %||% new_data_source(envir = envir)
  .objects <- ds_objects_from_table(src, obj_table)
  # list of 4, call matrix, friendship matrix, actor$gradetype vector,
  #  actor$floor vector
  obj_cat <- assign_category_object(.objects)
  if (attr(obj_cat, "none_class")) {
    stop(
      "An object is not assigned either as network or attibute",
      paste(
        rownames(objects_effects_link)[attr(obj_cat, "many_classes") != 1],
        collapse = ", "
      ),
      "check the class of the object.",
      call. = FALSE
    )
  }

  # objects: list of 6, each element is a 84*84 matrix
  objects_ret <- lapply(
    seq_along(effects),
    function(i_eff) {
      o <- objects_effects_link[, i_eff]
      att_ids <- which(!is.na(o) & obj_cat == "attribute")
      net_ids <- which(!is.na(o) & obj_cat == "network")
      attributes <- .objects[att_ids[order(o[att_ids])]]
      networks <- .objects[net_ids[order(o[net_ids])]]
      label_effect <- colnames(objects_effects_link)[i_eff]
      objects_names <- paste(
        rownames(na.omit(objects_effects_link[, i_eff, drop = FALSE])),
        collapse = ", "
      )
      message_effect <- paste0(
        " cannot initialized with objects ",
        objects_names,
        "\n"
      )
      # init
      .args_fun <- list(
        effect_fun = effects[[i_eff]][["effect"]],
        network = if (length(networks) == 1) networks[[1]] else networks,
        attribute = if (length(attributes) == 1) {
          attributes[[1]]
        } else {
          attributes
        },
        groups_network = groups_network,
        window = window_parameters[[i_eff]],
        n1 = n1,
        n2 = n2
      )
      call_fun(
        effects,
        i_eff,
        "init_effect",
        .args_fun,
        message_effect,
        label_effect
      )
    }
  )
}


#' call fun
#' Call a function with a variable set of arguments (effects functions)
#' @param effects list of effects functions return by `create_effects_functions()`
#' @param effect_pos int indicates the effect to be used.
#' @param effect_type character indicates which effect function use.
#'   Available values
#'   `c("init_cache", "update_cache", "init_stat", "effect")`
#' @param .args_fun named list with the arguments to feed FUN.
#' @param error function that returns the error mesage if any.
#' @param warning function that returns the warning mesage if any.
#' @param effect_label character use by the error or warning function to give
#'   an additional information to the user.
#'
#' @return the output of call `effects[[effect_pos]][[effect_type]]`
#'   with arguments `.args_fun` in the case if not errors.
#' @noRd
#' @examples
#' \donttest{
#' .args_fun <- list(
#'   network = m,
#'   n1 = 5, n2 = 5,
#'   sender = 1, receiver = 5, replace = 0
#' )
#' effects <- list(list(effect = out))
#'
#' ver2 <- call_fun(
#'   effects = effects, effect_pos = effect_pos, effect_type = "effect",
#'   .args_fun = .args_fun, text_mss = " ver ",
#'   effect_label = "out"
#' )
#'
#'
#' .args_fun <- list(network = m, n1 = 5, n2 = 5, sender = 1, receiver = 5)
#' effects <- list(list(effect = out))
#'
#' ver2 <- call_fun(
#'   effects = effects, effect_pos = effect_pos, effect_type = "effect",
#'   .args_fun = .args_fun, text_mss = " ver ",
#'   effect_label = "out"
#' )
#' }
call_fun <- function(
  effects,
  effect_pos,
  effect_type,
  .args_fun,
  text_mss,
  effect_label
) {
  err <- NULL
  warn <- NULL
  .args_names <- formals(effects[[effect_pos]][[effect_type]])
  .args_keep <- pmatch(names(.args_names), names(.args_fun))
  # check for more than one net
  error_handler <- function(e) {
    erro <- simpleError(
      paste0(
        "Effect ",
        dQuote(effect_label),
        " (",
        effect_pos,
        ") ",
        text_mss,
        e$message
      )
    )
    stop(erro)
  }
  tryCatch(
    {
      withCallingHandlers(
        {
          call_res <- do.call(
            effects[[effect_pos]][[effect_type]],
            .args_fun[na.omit(.args_keep)]
          )
        },
        error = identity,
        warning = function(w) {
          warn <<- w
          invokeRestart("muffleWarning")
        }
      )
    },
    error = error_handler
  )
  if (!is.null(warn)) {
    warning(warn)
  }
  return(call_res)
}


#' Impute missing values
#' If network missing values are replace by zero,
#' if attributes missing values are impute by the mean.
#' @param objects_effects_link matrix. Rows objects, columns effects, 1 or NA.
#'  `get_objects_effects_link(rhs_names)` output.
#' @param envir evaluation enviroment to get and assign impute objects.
#'
#' @return a vector of `nrow(objects_effects_link)` length with
#'   logical value signaling imputation of missing values.
#' @noRd
#'
#' @examples
#' \donttest{
#' actors_ex <- data.frame(
#'   label = sprintf("Actor %d", 1:5),
#'   present = rep(TRUE, 5),
#'   attr1 = c(9.9, NA, 0.5, 0.45, 0.25),
#'   stringsAsFactors = FALSE
#' )
#'
#' network_algo <- matrix(
#'   c(
#'     0, 3, 0, 0, 0,
#'     1, 0, 1, 1, 0,
#'     0, 0, NA, 1, 0,
#'     0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE,
#'   dimnames = list(
#'     sprintf("Actor %d", 1:5),
#'     sprintf("Actor %d", 1:5)
#'   )
#' )
#'
#' objects_effects_link <- matrix(
#'   c(1, NA, NA, 1),
#'   nrow = 2, ncol = 2,
#'   dimnames = list(
#'     c("network_algo", "actors_ex$attr1"),
#'     c("inertia", "alter")
#'   )
#' )
#' prep_envir <- environment()
#'
#' check <- impute_missing_data(objects_effects_link, envir = prep_envir)
#' }
impute_missing_data <- function(objects_effects_link, envir = new.env()) {
  # get data object table, row objects columns class (matrix, attribute)
  obj_table <- get_data_objects(
    list(rownames(objects_effects_link)),
    remove_first = FALSE
  )
  # print(obj_table)
  done <- structure(vector("logical", nrow(obj_table)), names = obj_table$name)
  for (i_eff in seq_len(nrow(obj_table))) {
    object_name_table <- obj_table[i_eff, ]
    object <- get_element_from_data_object_table(
      object_name_table,
      envir = envir
    )[[1]]
    object_name <- object_name_table$name
    # print(table(is.na(object)))
    # cat(object_name, "\n")
    if (is.matrix(object) && any(is.na(object))) {
      object[is.na(object)] <- 0
      done[i_eff] <- TRUE
      # cat("matrix\n")
      # Assign object
      assign(object_name, object, envir = envir)
    } else if (is.vector(object) && any(is.na(object))) {
      if (is.numeric(object)) {
        cli::cli_warn(c(
          "i" = "Missing data has been detected. The mean is used to impute
                 numerical values"
        ))
        object[is.na(object)] <- mean(object, na.rm = TRUE)
      } else {
        cli::cli_warn(c(
          "i" = "Missing data has been detected. The most common value is used
                 to impute categorical values"
        ))
        object[is.na(object)] <- names(which.max(table(object)))
      }
      done[i_eff] <- TRUE
      # cat("vector\n")
      # Assign object
      assign("object", object, envir = envir)
      eval(
        parse(text = paste(object_name, "<- object")),
        envir = envir,
        enclos = parent.frame()
      )
    }
  }
  return(done)
}
