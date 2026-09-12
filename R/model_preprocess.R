#' Preprocess a model given its specification
#'
#' Dispatch is a lookup, not a polymorphism: a standard specification runs
#' through the merged single-clock walk whatever its model variant, so the
#' generic carries one method that reads the process structure it is handed and
#' walks it, rather than a method per model variant.
#'
#' DyNAM-i is the one difference that is still real: its events arrive as
#' group interactions and reach a preprocessing loop of their own. That is a
#' property of the input, not of the model variant, so the descriptor's input
#' shape carries it and the same method routes on it — until the
#' effect-registry work closes the gap and the branch goes too.
#'
#' @param spec a `goldfishKind` object from `new_model_spec()`.
#' @param ... preprocessing arguments threaded to the merged walk (or the
#'   DyNAM-i monolith), see [preprocess_one_unit()].
#'
#' @return a list of class goldfishStat
#' @noRd
preprocess <- function(spec, ...) {
  UseMethod("preprocess")
}

#' @noRd
preprocess.goldfishKind <- function(
  spec,
  ...,
  recipe_spec = NULL,
  family = NULL,
  control_prep = NULL,
  new_writer = writer_default
) {
  if (identical(behavior_input_shape(spec), "grouped")) {
    return(run_dynami_monolith(spec, ...))
  }
  # Standard specifications run through the merged single-clock walk, the
  # substrate the frozen baselines have always exercised and the one
  # `simulate()` drives. The recipe estimation surface compiles a spec_map from
  # a formula rather than a goldfishSpec, so `recipe_spec` carries the
  # single-process structure the walk reads (focal, family, constraint,
  # intercept) reassembled from that compiled map. A single-process call unwraps
  # the one fid the walk emits.
  dots <- list(...)
  out <- preprocess_one_unit(
    recipe_spec,
    family,
    spec,
    control_preprocessing = control_prep %||% set_preprocessing(),
    progress = isTRUE(dots$progress),
    writer = dots$writer %||% writer_default(),
    new_writer = new_writer,
    # Defer the support-constraint validation to estimation, as the recipe
    # loops did: a single-process spec preprocessed on its own must not be
    # rejected for a constraint only an estimation reads.
    validate_support = FALSE
  )
  map <- attr(out, "process_map")
  prep <- out[[as.character(map$fid[map$family == family])]]
  # The merged walk stamps each output with the estimation-re-entry metadata
  # (formula, model, sub-model, node sides, node lookup, model spec) and the
  # support-validation flag. The estimation wrapper adds the re-entry metadata
  # after preprocessing, and the gather/data.frame/db paths rebuild their stack
  # from the raw statistics, so strip them here and let the wrapper decorate
  # uniformly.
  deco <- c(
    "formula",
    "model",
    "sub_model",
    "nodes",
    "nodes2",
    "node_lookup",
    "model_spec",
    "support_validated"
  )
  for (field in deco) {
    prep[[field]] <- NULL
  }
  prep
}

#' DyNAM-i preprocessing delegate
#'
#' Thin delegate to the existing monolithic DyNAM-i preprocessing loop with
#' unchanged arguments. The dedicated DyNAM-i recipe (post-event update order,
#' `sub_type` normalisation) is deferred to the effects unification change.
#' `preprocess_interaction()` keeps computing its own start and end times from
#' the event streams, as it did before the dispatch wiring.
#'
#' @param groups_network character, name of the groups network object.
#' @param ... absorbs the recipe arguments that the DyNAM-i loop does not
#'   consume (`window_parameters`, `ignore_rep_parameter`, `is_two_mode`,
#'   `startTime`, `endTime`, `opportunitiesList`, `writer`).
#' @noRd
run_dynami_monolith <- function(
  spec,
  events,
  effects,
  events_objects_link,
  events_effects_link,
  objects_effects_link,
  nodes,
  nodes2 = nodes,
  is_exact_time = FALSE,
  progress = FALSE,
  groups_network = NULL,
  prep_envir = new.env(),
  ...
) {
  prep <- preprocess_interaction(
    # The interaction loop predates the ordinal split and knows two sub-models
    # only. Both sender-indexed DyNAM-i variants take its rate path -- they
    # differ downstream, in the likelihood, not in how the statistics are
    # gathered -- and the receiver-indexed one takes its choice path.
    sub_model = if (identical(risk_set_axis(spec), "sender")) {
      "rate"
    } else {
      "choice"
    },
    events = events,
    effects = effects,
    events_objects_link = events_objects_link,
    events_effects_link = events_effects_link,
    objects_effects_link = objects_effects_link,
    nodes = nodes,
    nodes2 = nodes2,
    is_exact_time = is_exact_time,
    progress = progress,
    groups_network = groups_network,
    prep_envir = prep_envir
  )
  prep$prep_version <- PREP_VERSION
  prep
}

# Fold a sender-loop support_constraint into `active_sender`.
# The per-event effective availability is the row-reduction a sender is at risk
# iff it has >= 1 allowed, present receiver:
#   active_sender[i] at event e = presence_e[i] & (rowSums(support_e &
#     receiver_presence_e) > 0)
# — computed here (during preprocessing) and stored on the availability object
# as net crossings (a flip is emitted only on a 0 <-> positive change of the
# per-sender available-receiver count, so the buffer stays tiny), REPLACING the
# estimation-time recombination.
#
# That count is MAINTAINED, not recomputed. The mask arrives as a stream of the
# entries that flipped, and a flip adjusts the counts of the senders it reaches:
# a point flip touches one sender, an alter flip every sender by the same
# amount, an ego flip sets one sender's count outright. A sender crossing is
# emitted only when its count crosses zero, which is why `~ indeg(msg) < 20`
# produced four crossings and not three thousand snapshots. The grid the old
# `rowSums()` reduced is never built.
#
# BOTH inner axes move. The receiver presence the count is taken over is itself
# a crossings buffer, and a receiver leaving decrements the count of every
# sender allowed to reach it — the same locality argument the mask flips use,
# read off the mask's own kind. Freezing that axis at time zero leaves a sender
# whose only allowed receivers have departed still at risk. The two factors are
# updated once each per event, each against the other's current value: mask
# flips first, against the receiver presence before this event's crossings,
# then the crossings against the mask after them.
#
# `active_dyad_init` seeds that receiver presence. Returns `out` with
# `active_sender_init`/`_update`/`_update_pointer` rewritten to the folded
# object, `active_sender_folded = TRUE`, and (when intercept scalars are stored)
# `avg_active_entity` recomputed as the event-averaged active-sender count.
fold_active_sender_support <- function(out, support_mask, active_dyad_init) {
  n_stored <- length(out$event_time)
  if (n_stored == 0L) {
    return(out)
  }
  n1 <- length(out$active_sender_init)
  n2 <- length(active_dyad_init)
  stored_kind <- support_mask$stored_kind %||% 0L
  active_2 <- active_dyad_init
  n_present_receivers <- sum(active_2)

  count <- initial_receiver_count(
    support_mask$initial,
    stored_kind,
    active_2,
    n1,
    n2
  )
  flips_at <- mask_flips(support_mask)

  # Charging a receiver crossing to the counts needs the mask's current value,
  # not just its flips, so the cursor is opened only when the receiver
  # composition actually moves: at point kind its value is an n1 x n2 object
  # and most models never pay for it.
  receiver_upd <- out$active_dyad_update
  receiver_ptr <- out$active_dyad_update_pointer
  tracks_receivers <- !is.null(receiver_upd) &&
    !is.null(receiver_ptr) &&
    ncol(receiver_upd) > 0L
  mask_at <- if (tracks_receivers) mask_cursor(support_mask) else NULL
  prev_receiver_ptr <- 0L

  # Walk the presence crossings buffer in event order to recover presence_e,
  # intersect with the maintained sender gate, and re-encode the result as
  # crossings without ever holding the folded timeline: only the previous
  # event's vector is needed to name what changed.
  presence <- out$active_sender_init
  upd <- out$active_sender_update
  ptr <- out$active_sender_update_pointer
  n_changes <- integer(n_stored)
  change_nodes <- vector("list", n_stored)
  change_repl <- vector("list", n_stored)
  folded_init <- NULL
  previous <- NULL
  active_total <- 0
  prev_ptr <- 0L
  for (e in seq_len(n_stored)) {
    this_ptr <- if (!is.null(ptr)) ptr[e] else 0L
    if (this_ptr > prev_ptr) {
      cols <- (prev_ptr + 1L):this_ptr
      presence[upd[1L, cols]] <- as.logical(upd[2L, cols])
    }
    prev_ptr <- this_ptr
    count <- apply_receiver_count_flips(
      count,
      flips_at(e),
      stored_kind,
      active_2,
      n1,
      n_present_receivers
    )
    if (tracks_receivers) {
      this_receiver_ptr <- receiver_ptr[e]
      if (this_receiver_ptr > prev_receiver_ptr) {
        cols <- (prev_receiver_ptr + 1L):this_receiver_ptr
        crossed <- apply_receiver_presence_flips(
          count,
          as.integer(receiver_upd[1L, cols]),
          as.logical(receiver_upd[2L, cols]),
          active_2,
          mask_at(e),
          stored_kind,
          n1
        )
        count <- crossed$count
        active_2 <- crossed$active_2
        n_present_receivers <- sum(active_2)
      }
      prev_receiver_ptr <- this_receiver_ptr
    }
    current <- presence & (count > 0L)
    if (e == 1L) {
      folded_init <- current
    } else {
      crossing <- emit_crossings(previous, current)
      n_changes[e] <- length(crossing$entries)
      change_nodes[[e]] <- crossing$entries
      change_repl[[e]] <- as.numeric(crossing$values)
    }
    previous <- current
    active_total <- active_total + sum(current)
  }

  node_vec <- unlist(change_nodes, use.names = FALSE)
  repl_vec <- unlist(change_repl, use.names = FALSE)
  # Preserve the raw sender presence for the fail-fast constraint validation
  # (its "present but always gated out" warning is defined on raw presence, not
  # the folded object); estimation engines never read it.
  out$support_mask$sender_presence_init <- out$active_sender_init
  out$active_sender_init <- folded_init
  out$active_sender_update <- if (length(node_vec) > 0L) {
    rbind(node_vec, repl_vec)
  } else {
    matrix(0, 2L, 0L)
  }
  out$active_sender_update_pointer <- cumsum(n_changes)
  out$active_sender_changes <- list()
  out$active_sender_folded <- TRUE

  if (!is.null(out$avg_active_entity)) {
    out$avg_active_entity <- active_total / n_stored
  }
  out
}

# How many allowed, present receivers each sender starts with, read off the
# initial mask at its own kind. A separable mask answers without a grid: an
# alter mask gives every sender the same count, an ego mask gives a sender all
# the present receivers or none, and a global mask gives everyone the same.
initial_receiver_count <- function(initial, stored_kind, active_2, n1, n2) {
  present <- sum(active_2)
  switch(
    as.character(stored_kind),
    "3" = rep(if (isTRUE(as.logical(initial))) present else 0L, n1),
    "2" = ifelse(as.logical(initial), present, 0L),
    "1" = rep(sum(as.logical(initial) & active_2), n1),
    "0" = rowSums(
      matrix(as.logical(initial), n1, n2) &
        rep(active_2, each = n1)
    ),
    cli::cli_abort("Unknown mask kind {.val {stored_kind}}.", .internal = TRUE)
  )
}

# Adjust the per-sender counts for one event's mask flips. Which senders a flip
# reaches follows from the mask's kind, exactly as `map_entries()` says it does:
# a point entry is one dyad, an alter entry a whole column, an ego entry a whole
# row, a global entry everything.
apply_receiver_count_flips <- function(
  count,
  flips,
  stored_kind,
  active_2,
  n1,
  n_present_receivers
) {
  entries <- flips$entries
  if (length(entries) == 0L) {
    return(count)
  }
  values <- flips$values
  if (stored_kind == 0L) {
    senders <- ((entries - 1L) %% n1) + 1L
    receivers <- ((entries - 1L) %/% n1) + 1L
    live <- active_2[receivers]
    gained <- tabulate(senders[live & values], nbins = n1)
    lost <- tabulate(senders[live & !values], nbins = n1)
    return(count + gained - lost)
  }
  if (stored_kind == 1L) {
    live <- active_2[entries]
    return(count + sum(live & values) - sum(live & !values))
  }
  if (stored_kind == 2L) {
    count[entries] <- ifelse(values, n_present_receivers, 0L)
    return(count)
  }
  rep(if (values[[length(values)]]) n_present_receivers else 0L, n1)
}

# Adjust the per-sender counts for one event's receiver presence crossings, and
# hand back the moved receiver presence with them. A receiver arriving raises
# the count of every sender allowed to reach it and a departure lowers it, so
# only the NET change may be charged: a node that crosses twice within one
# event has to be applied in order, which is why this walks the crossings one
# at a time rather than vectorizing over them. There are rarely more than a
# handful per event.
apply_receiver_presence_flips <- function(
  count,
  nodes,
  values,
  active_2,
  mask,
  stored_kind,
  n1
) {
  for (k in seq_along(nodes)) {
    node <- nodes[[k]]
    delta <- as.integer(values[[k]]) - as.integer(active_2[[node]])
    if (delta != 0L) {
      count <- count + delta * receiver_reach(mask, stored_kind, node, n1)
      active_2[[node]] <- values[[k]]
    }
  }
  list(count = count, active_2 = active_2)
}

# Which senders one receiver is allowed for, read at the mask's own kind and
# returned as a 0/1 vector the counts can be shifted by. The reach of a
# receiver is the mirror of the reach of a mask flip: a point mask answers from
# the receiver's own column, an alter mask by whether that one receiver is
# allowed at all, an ego mask by each sender's own bit, a global mask for
# everyone or no one.
receiver_reach <- function(mask, stored_kind, node, n1) {
  switch(
    as.character(stored_kind),
    "3" = rep(as.integer(as.logical(mask)), n1),
    "2" = as.integer(as.logical(mask)),
    "1" = rep(as.integer(as.logical(mask[[node]])), n1),
    "0" = as.integer(as.logical(mask[((node - 1L) * n1) + seq_len(n1)])),
    cli::cli_abort("Unknown mask kind {.val {stored_kind}}.", .internal = TRUE)
  )
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
# `build_state = FALSE` skips the per-unit state container, event schedule and
# their two schedule checks: the merged walk runs every unit over ONE shared
# state and schedule it builds itself (and checks itself), so a unit's own copy
# would be materialized -- one full n1 x n2 matrix per network -- only to be
# discarded. Everything else in the context (streams, cache, initial
# statistics, lookups) is what the walk engine actually reads.
prepare_recipe_context <- function(
  spec,
  startTime,
  endTime,
  prep_envir,
  sub_model,
  progress = FALSE,
  build_state = TRUE
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
  # The output columns are exactly the effects nobody excludes: constraint-role
  # atoms carry `role = "constraint"` in `plan$effects`, and reading that role
  # here is what keeps them out of `nEffects`, `initial_stats`, and the output
  # statistics. They are appended after the estimated effects, so the estimated
  # columns remain the leading `1..nEffects` and nothing downstream reindexes;
  # an unconstrained plan has no such row and the count is unchanged.
  nEffects <- sum(plan$effects$role != "constraint")

  composition1 <- ds_composition(src, nodes, n1)
  composition2 <- ds_composition(src, nodes2, n2)
  active_sender_init <- composition1$init
  active_dyad_init <- composition2$init
  active_sender_changes <- composition1$changes
  active_dyad_changes <- composition2$changes

  state <- NULL
  schedule <- NULL
  if (build_state) {
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
  }

  net_update_lookup <- matrix(NA_integer_, nrow(plan$objects), nEffects)
  att_update_lookup <- matrix(NA_integer_, nrow(plan$objects), nEffects)
  net_update_lookup[cbind(plan$effect_objects$oid, plan$effect_objects$gid)] <-
    plan$effect_objects$net_update
  att_update_lookup[cbind(plan$effect_objects$oid, plan$effect_objects$gid)] <-
    plan$effect_objects$att_update

  # Per-object routing table for the covariate branch of a walk: for each
  # effect an object's event reaches, its call template, its update positions
  # (NULL where the effect does not take one) and its broadcast kind, resolved
  # here once. Read per event, the same answers cost two matrix lookups, an
  # NA test and three list chains per effect, which on a degree-only rate
  # model is as much as the effect update itself.
  broadcast_kind <- plan$effects$broadcast_kind
  route <- lapply(seq_along(plan$routing), function(oid) {
    lapply(plan$routing[[oid]], function(gid) {
      net_update <- net_update_lookup[oid, gid]
      att_update <- att_update_lookup[oid, gid]
      list(
        gid = gid,
        template = effects_template[[gid]],
        net_update = if (is.na(net_update)) NULL else net_update,
        att_update = if (is.na(att_update)) NULL else att_update,
        broadcast_kind = broadcast_kind[gid]
      )
    })
  })

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
    att_update_lookup = att_update_lookup,
    route = route
  )
}

# Expand an operand effect's `(node1, node2, replace)` delta into the full-matrix
# cells + values it changes, per its broadcast kind: a point (0)
# delta touches its own cells; an alter (1) delta sets whole receiver columns;
# an ego (2) delta sets whole sender rows; a global (3) delta sets the whole
# matrix. Used to keep an operand's live matrix current so interaction products
# can be recomputed.
expand_operand_update <- function(updates, kind, n1, n2) {
  projected <- project_entries(
    updates[, "node1"],
    updates[, "node2"],
    updates[, "replace"],
    kind,
    0L,
    n1,
    n2
  )
  list(cells = projected$entries, vals = projected$values)
}

# Deduplicate the accumulated interaction cell matrix (rows are (i, j) pairs)
# via a single linear key, so a cell touched by several operands in one event is
# emitted once.
dedup_cells <- function(cells, n1) {
  key <- cells[, 1] + (cells[, 2] - 1) * n1
  cells[!duplicated(key), , drop = FALSE]
}

# The streaming form of `walk_presence_buffer()`: the presence vector at each
# event, one at a time, for a consumer walking the events in order anyway.
presence_cursor <- function(init, update, pointer) {
  current <- init
  seen <- 0L
  function(e) {
    hi <- if (!is.null(pointer)) pointer[e] else 0L
    if (hi > seen) {
      cols <- (seen + 1L):hi
      current[update[1L, cols]] <<- as.logical(update[2L, cols])
      seen <<- hi
    }
    current
  }
}

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

# The streaming form of `crossings_from_vectors()`: the caller pushes one
# event's vector at a time and only the previous one is held, so a fold never
# materializes its timeline. On 1899 actors and 1500 events the list form is 2.8
# million logicals for a receiver vector and 4 billion for a REM risk mask,
# which is why the coordination cell had to be measured on a tenth of the
# sequence.
crossings_accumulator <- function(n_stored) {
  n_changes <- integer(n_stored)
  nodes <- vector("list", n_stored)
  repl <- vector("list", n_stored)
  init <- NULL
  previous <- NULL
  list(
    push = function(e, current) {
      if (e == 1L) {
        init <<- current
      } else {
        crossing <- emit_crossings(previous, current)
        n_changes[e] <<- length(crossing$entries)
        nodes[[e]] <<- crossing$entries
        repl[[e]] <<- as.numeric(crossing$values)
      }
      previous <<- current
    },
    finish = function() {
      nv <- unlist(nodes, use.names = FALSE)
      rv <- unlist(repl, use.names = FALSE)
      list(
        init = init,
        update = if (length(nv) > 0L) rbind(nv, rv) else matrix(0, 2L, 0L),
        pointer = cumsum(n_changes)
      )
    }
  )
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
    crossing <- emit_crossings(vecs[[e - 1L]], vecs[[e]])
    n_changes[e] <- length(crossing$entries)
    nodes[[e]] <- crossing$entries
    repl[[e]] <- as.numeric(crossing$values)
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
  # As in the sender fold: a cursor over the flip stream, advanced with the
  # events this loop already walks in order.
  stored_kind <- support_mask$stored_kind %||% 0L
  mask_at <- mask_cursor(support_mask)
  # Both branches below read ONE row per event, so the row is read at the mask's
  # own kind instead of expanding a grid to take a slice of it.
  support_row_at <- function(e, sender) {
    support_row(mask_at(e), stored_kind, sender, n1, n2)
  }
  has_opportunity <- !is.null(opportunitiesList)
  encoding <- active_dyad_encoding_decide(
    risk_set_encoding(spec),
    mask_kind,
    has_opportunity
  )
  # Standard/ordinal REM and DyNAM coordination all fold BOTH presences ∩ their
  # support atoms into a dense point `active_dyad` — the risk-set mask each engine
  # consumes directly, replacing the per-event `active_dyad_mask` snapshot. These families
  # share a dyadic / two-sided risk set (only the likelihood family differs:
  # timespan-weighted Poisson, multinomial, or the mutual `getLikelihoodMM`
  # product), so one fold serves them. Coordination (`DyNAM-MM`) is additionally
  # symmetrised so `(i, j)` is available iff both directions are
  # allowed — required for the mutual likelihood. Every DyNAM choice encoding
  # folds below: alter/scalar as the receiver vector, ego-kind as the outer
  # factorization (receiver presence f2, sender presence f1, no dyad-shaped
  # object), and point (dyadic atom or opportunity list) as dense point row
  # flips.
  if (risk_set_is_dyadic(spec)) {
    return(fold_active_dyad_support_rem(
      out,
      support_mask,
      n1,
      n2,
      n_stored,
      symmetric = risk_set_symmetrize(spec)
    ))
  }

  recv_at <- presence_cursor(
    out$active_dyad_init,
    out$active_dyad_update,
    out$active_dyad_update_pointer
  )
  # The fold below overwrites `active_dyad_update` with the folded availability,
  # so the raw receiver crossings are gone by the time the fail-fast validation
  # runs on a choice object. Stash them beside the init the validation already
  # reads, so it can walk the receiver presence per event rather than freezing
  # it at time zero.
  out$support_mask$receiver_presence_init <- out$active_dyad_init
  out$support_mask$receiver_presence_update <- out$active_dyad_update
  out$support_mask$receiver_presence_update_pointer <-
    out$active_dyad_update_pointer

  if (identical(encoding, "alter")) {
    # An alter/scalar mask is column-broadcast, so its stored value IS the alter
    # vector at kind 1 and any row of the grid otherwise. Accumulated as it goes
    # rather than collected: only the previous event's vector is needed to name
    # what changed.
    accumulator <- crossings_accumulator(n_stored)
    for (e in seq_len(n_stored)) {
      accumulator$push(e, recv_at(e) & support_row_at(e, 1L))
    }
    cr <- accumulator$finish()
    out$active_dyad_init <- cr$init
    out$active_dyad_update <- cr$update
    out$active_dyad_update_pointer <- cr$pointer
    out$active_dyad_encoding <- "alter"
    out$active_dyad_folded <- TRUE
  } else if (identical(encoding, "outer")) {
    # An ego-kind atom gates only the sender axis (its support row is constant
    # across receivers), so the availability factorizes: the receiver presence
    # is f2 (`active_dyad`, stored as its own crossings stream) and the sender
    # presence is f1 (`active_sender`, left as it is), with cell
    # (i, j) = active_sender[i] & active_dyad[j]. No dyad-shaped object is
    # allocated. The sender factor is not folded into f1 because the choice risk
    # set reads only the observed sender's row and the observed sender is always
    # allowed (the fail-fast validation errors otherwise), so a sender the atom
    # gates out is never a row the likelihood reads; the sender-side validation
    # reports it instead.
    accumulator <- crossings_accumulator(n_stored)
    for (e in seq_len(n_stored)) {
      accumulator$push(e, recv_at(e))
    }
    cr <- accumulator$finish()
    out$active_dyad_init <- cr$init
    out$active_dyad_update <- cr$update
    out$active_dyad_update_pointer <- cr$pointer
    out$active_dyad_encoding <- "outer"
    out$active_dyad_folded <- TRUE
  } else {
    # point: fold receiver presence ∩ the sender's support row ∩ opportunity
    # into the dense point buffer. Covers a genuinely dyadic (point) atom and
    # any atom together with a user opportunity list. The choice risk set reads
    # only the event sender's row, so only that row is emitted.
    senders <- out$event_sender
    opp_row <- function(e) {
      if (!has_opportunity) {
        return(rep(TRUE, n2))
      }
      opp <- opportunitiesList[[e]]
      if (is.null(opp)) rep(TRUE, n2) else seq_len(n2) %in% opp
    }
    out <- build_active_dyad_point(
      out,
      recv_at,
      function(e) recv_at(e) & support_row_at(e, senders[[e]]) & opp_row(e),
      senders,
      n1,
      n2,
      n_stored
    )
  }

  out
}

# Fold a standard-REM support_constraint into a dense point `active_dyad`. The
# REM risk set is the whole dyad matrix, so per event the mask is
# `presence1[i] & presence2[j] & support[i, j]` — both presences folded in — and
# the default engine consumes it directly as the per-event risk mask, replacing
# the standalone `active_dyad_mask` snapshot. The raw presences are stashed on
# `support_mask` for the fail-fast validation, which needs them unfolded.
#
# The mask is MAINTAINED, not rebuilt. Its three inputs all arrive as flip
# streams -- sender presence, receiver presence, and the support mask -- and a
# flip reaches a bounded set of cells: a sender crossing one row, a receiver
# crossing one column, a support flip whatever its kind projects onto. Only
# those cells are recomputed, so the `outer()` product and the expanded grid are
# built once at the first event rather than at every one. Coordination is
# additionally symmetrised, which is not elementwise, so its recomputed set is
# closed under transposition.
fold_active_dyad_support_rem <- function(
  out,
  support_mask,
  n1,
  n2,
  n_stored,
  symmetric = FALSE
) {
  stored_kind <- support_mask$stored_kind %||% 0L
  p1_flips <- flip_reader(
    out$active_sender_update,
    out$active_sender_update_pointer
  )
  p2_flips <- flip_reader(
    out$active_dyad_update,
    out$active_dyad_update_pointer
  )
  support_flips <- mask_flips(support_mask)
  p1 <- out$active_sender_init
  p2 <- out$active_dyad_init
  support <- support_mask$initial
  out$support_mask$sender_presence_init <- out$active_sender_init
  out$support_mask$receiver_presence_init <- out$active_dyad_init

  transposed <- function(entries) {
    rows <- ((entries - 1L) %% n1) + 1L
    cols <- ((entries - 1L) %/% n1) + 1L
    (rows - 1L) * n1 + cols
  }
  raw_at <- function(entries) {
    rows <- ((entries - 1L) %% n1) + 1L
    cols <- ((entries - 1L) %/% n1) + 1L
    p1[rows] &
      p2[cols] &
      read_value_at_entries(support, stored_kind, entries, 0L, n1, n2)
  }
  advance_inputs <- function(e) {
    f1 <- p1_flips(e)
    f2 <- p2_flips(e)
    fs <- support_flips(e)
    p1[f1$entries] <<- f1$values
    p2[f2$entries] <<- f2$values
    support[fs$entries] <<- fs$values
    list(senders = f1$entries, receivers = f2$entries, mask = fs$entries)
  }

  advance_inputs(1L)
  current <- outer(p1, p2) &
    (support_to_grid(support, stored_kind, n1, n2) == 1)
  if (symmetric) {
    current <- symmetrize_mask(current)
  }
  # `current` is written IN PLACE from here on, so the init has to be its own
  # object: aliasing it would rewrite the value every emitted flip is a diff
  # against. `& TRUE` is an elementwise operation, so it allocates.
  init <- current & TRUE

  n_changes <- integer(n_stored)
  node1 <- vector("list", n_stored)
  node2 <- vector("list", n_stored)
  repl <- vector("list", n_stored)
  for (e in seq_len(n_stored)[-1L]) {
    moved <- advance_inputs(e)
    reached <- unique(c(
      if (length(moved$senders) > 0L) {
        map_entries(moved$senders, 2L, 0L, n1, n2)
      },
      if (length(moved$receivers) > 0L) {
        map_entries(moved$receivers, 1L, 0L, n1, n2)
      },
      if (length(moved$mask) > 0L) {
        map_entries(moved$mask, stored_kind, 0L, n1, n2)
      }
    ))
    if (length(reached) == 0L) {
      next
    }
    if (symmetric) {
      reached <- unique(c(reached, transposed(reached)))
      desired <- raw_at(reached) & raw_at(transposed(reached))
    } else {
      desired <- raw_at(reached)
    }
    flips <- changed_entries(current, reached, desired)
    n_changes[e] <- length(flips$entries)
    node1[[e]] <- ((flips$entries - 1L) %% n1) + 1L
    node2[[e]] <- ((flips$entries - 1L) %/% n1) + 1L
    repl[[e]] <- as.numeric(flips$values)
    current <- write_entries(current, flips$entries, flips$values)
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
  recv_at <- presence_cursor(
    out$active_dyad_init,
    out$active_dyad_update,
    out$active_dyad_update_pointer
  )
  senders <- out$event_sender
  # `seq_len(n2) %in% opportunitiesList[[e]]` mirrors the estimation-time
  # recompute exactly (an all-TRUE row when the event has no restriction).
  build_active_dyad_point(
    out,
    recv_at,
    function(e) {
      opp <- opportunitiesList[[e]]
      if (is.null(opp)) recv_at(e) else recv_at(e) & (seq_len(n2) %in% opp)
    },
    senders,
    n1,
    n2,
    n_stored
  )
}

# Assemble the `active_dyad` point encoding from per-event desired receiver rows.
# Shared by the opportunity fold and the point-kind support
# fold. Because the choice risk set reads only the event sender's row, only that
# row is emitted: the first event's row seeds the dense `n1 x n2` init (all other
# rows carry the event-1 receiver presence, immaterial until each sender's first
# event overwrites its row) and each later event emits net
# `(node1 = sender, node2 = j, replace)` flips against the row's stored value.
build_active_dyad_point <- function(
  out,
  recv_at,
  desired_at,
  senders,
  n1,
  n2,
  n_stored
) {
  cur <- matrix(recv_at(1L), nrow = n1, ncol = n2, byrow = TRUE)
  first <- desired_at(1L)
  cur[senders[[1L]], ] <- first
  init <- cur
  n_changes <- integer(n_stored)
  node1 <- vector("list", n_stored)
  node2 <- vector("list", n_stored)
  repl <- vector("list", n_stored)
  for (e in seq_len(n_stored)[-1L]) {
    s <- senders[[e]]
    desired <- desired_at(e)
    ch <- which(cur[s, ] != desired)
    n_changes[e] <- length(ch)
    node1[[e]] <- rep.int(s, length(ch))
    node2[[e]] <- ch
    repl[[e]] <- as.numeric(desired[ch])
    cur[s, ] <- desired
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
