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

# How many allowed, present receivers each sender starts with, read off the
# initial mask at its own kind. A separable mask answers without a grid: an
# alter mask gives every sender the same count, an ego mask gives a sender all
# the present receivers or none, and a global mask gives everyone the same.
# Under `drop_diagonal` each sender's own cell is taken back out again,
# which costs one term per sender and no grid.
initial_receiver_count <- function(
  initial,
  stored_kind,
  active_2,
  n1,
  n2,
  drop_diagonal
) {
  present <- sum(active_2)
  abort_if_diagonal_off_grid(drop_diagonal, n1, n2)
  count <- switch(
    as.character(stored_kind),
    "3" = if (isTRUE(as.logical(initial))) rep(present, n1) else rep(0L, n1),
    "2" = ifelse(as.logical(initial), present, 0L),
    "1" = rep(sum(as.logical(initial) & active_2), n1),
    "0" = point_receiver_count(initial, active_2, n1, n2),
    cli::cli_abort("Unknown mask kind {.val {stored_kind}}.", .internal = TRUE)
  )
  count - own_receiver_count(initial, stored_kind, active_2, n1, drop_diagonal)
}

# Each sender's own cell, allowed and present, as a 0/1 vector, or a zero
# when the layer has no self-dyads. A branch, never a term multiplied by
# zero: the diagonal index of a two-mode grid is out of range, and
# `0L * NA` is `NA`.
own_receiver_count <- function(
  value,
  stored_kind,
  active_2,
  n1,
  drop_diagonal
) {
  if (!drop_diagonal) {
    return(0L)
  }
  own <- switch(
    as.character(stored_kind),
    "3" = rep(isTRUE(as.logical(value)), n1),
    "2" = as.logical(value),
    "1" = as.logical(value)[seq_len(n1)],
    "0" = as.logical(value)[(seq_len(n1) - 1L) * n1 + seq_len(n1)]
  )
  as.integer(own & active_2[seq_len(n1)])
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

# Assemble the `active_dyad` point encoding from per-event desired receiver rows.
# Used by the DyNAM-i joining availability; a DyNAM choice keeps its point
# availability as an effect in the walk (`new_availability_effect()`), which
# stores the true grid. Because the choice risk set reads only the event
# sender's row, only that
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
  src = NULL,
  at_kind = NULL
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
      kind <- if (is.null(at_kind)) 0L else at_kind[[i_eff]]
      if (kind > 0L) {
        seeded <- init_at_kind(label_effect, kind, .args_fun)
        if (!is.null(seeded)) {
          return(seeded)
        }
      }
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

# An effect's initial value at a broadcast kind, without the dense n1 x n2
# grid its dyad init builds. Every vector-kind dyad init is a per-node vector
# (or a scalar) broadcast down the grid: the attribute itself for `alter`, the
# sender-side init's value for `ego`, `global` and the degree counts, whose
# cache it also returns. `NULL` for any other effect, or when the value does
# not have the kind's length (an empty network's degree init sizes its value
# by the senders), so the caller takes the grid and reduces it.
init_at_kind <- function(label, kind, args) {
  name <- sub("\\(.*$", "", label)
  n1 <- args$n1
  n2 <- args$n2
  seeded <- switch(
    name,
    alter = list(stat = as.numeric(args$attribute)),
    ego = init_DyNAM_rate.ego(
      args$effect_fun,
      args$attribute,
      n1,
      n2
    ),
    global = init_DyNAM_rate.global(args$effect_fun, args$attribute, 1L, n2),
    indeg = init_DyNAM_rate.indeg(
      args$effect_fun,
      args$network,
      args$window,
      n1,
      n2
    ),
    outdeg = init_DyNAM_rate.outdeg(
      args$effect_fun,
      args$network,
      args$window,
      n1,
      n2
    ),
    NULL
  )
  if (is.null(seeded) || length(seeded$stat) != kind_length(kind, n1, n2)) {
    return(NULL)
  }
  seeded$stat <- as.numeric(seeded$stat)
  seeded$at_kind <- kind
  seeded
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
