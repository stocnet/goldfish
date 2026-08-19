####################### #
#
# Goldfish package
# Maintaining the support_constraint active mask over the event sequence
#
####################### #

# The support mask is realized as a derived object: its atoms are
# plain effects, so they are maintained with the SAME recipe builders the
# estimated formula uses (state container, event schedule, effect templates,
# stat cache, per-object routing) — but over the constraint sub-plan alone, in a
# self-contained pass sharing no state with the main statistics loop. Keeping
# it separate means the unconstrained statistics path is untouched, so the 1e-6
# baselines cannot move; the pass only runs when a `support_constraint` is
# present.
#
# The atoms' live per-cell values are kept dense here (one n1 x n2 matrix per
# atom) and the boolean tree is evaluated over them. The axis-union storage kind
# (vector/scalar for separable ego/global constraints) is a memory
# optimization deferred to a later slice; correctness of the mask timeline does
# not depend on it. Likewise the atoms are re-derived from their update closures
# per touched cell — the same locality as the interaction second-hop — rather
# than a whole-matrix recompute.
#
# Atom MAINTENANCE (walking the atom event streams, one dense matrix per atom)
# is split from mask EVALUATION (projecting the atoms through a constraint's
# boolean tree). Maintenance is the expensive part and is expressed once, by
# `build_atom_maintainer()`; evaluation is a cheap per-constraint elementwise
# pass, `eval_constraint_mask()`. A single constraint runs one maintainer and
# one expression (`preprocess_support_mask()`); several constraints over the
# SAME atoms share one maintainer and each keeps its own expression / snapshot
# times (`preprocess_pooled_support_masks()`), so the atom stream is walked once
# instead of once per output.

#' Build the atom-maintenance state for a constraint sub-plan
#'
#' Seeds the atoms' dense per-cell matrices from the objects' pre-event state and
#' returns a handle that advances them over their own event streams. The handle
#' exposes `advance(t)` (apply every atom event strictly before `t`, so the risk
#' set is lagged), `atom_matrix(label)` (an atom's current dense value, addressed
#' by its deparsed label), `n1`/`n2`, and the ordered `atom_labels`. Evaluation
#' of the boolean tree is a separate concern (`eval_constraint_mask()`); this
#' handle knows nothing about any particular constraint's expression, which is
#' what lets several constraints over the same atoms share one maintainer.
#'
#' @param sub_plan the compiled constraint sub-plan from
#'   `compile_support_constraint()`.
#' @param model the model string (`"DyNAM"` / `"REM"`).
#' @param nodes,nodes2 sender/receiver nodeset names, resolved in `prep_envir`.
#' @param prep_envir environment holding the realized data objects.
#' @param src optional data source; built over `prep_envir` when `NULL`.
#' @return a maintainer handle (list of closures + `n1`, `n2`, `atom_labels`).
#' @noRd
build_atom_maintainer <- function(
  sub_plan,
  model,
  nodes,
  nodes2,
  prep_envir = new.env(),
  src = NULL
) {
  effects <- sub_plan$effect_functions
  objects_effects_link <- sub_plan$objects_effects_link
  events_objects_link <- sub_plan$events_objects_link
  atom_kinds <- sub_plan$atom_kinds
  atom_labels <- sub_plan$atom_labels
  n_atoms <- length(effects)

  if (is.null(src)) {
    src <- new_data_source(envir = prep_envir)
  }
  n1 <- ds_n_nodes(src, nodes)
  n2 <- ds_n_nodes(src, nodes2)

  events <- fetch_events(sub_plan$fetch_plan, envir = prep_envir, src = src)
  schedule <- build_event_schedule(
    events,
    events_objects_link,
    sub_plan$objects
  )

  state <- build_state_container(
    rownames(objects_effects_link),
    nodes,
    nodes2,
    envir = prep_envir,
    src = src
  )
  effects_template <- build_effects_template(
    effects,
    objects_effects_link,
    state
  )

  # Initial atom values (dense n1 x n2 each) and their live caches, seeded from
  # the objects' state before any event.
  stat_cache <- initialize_cache_stat(
    objects_effects_link = objects_effects_link,
    effects = effects,
    groups_network = NULL,
    window_parameters = vector("list", n_atoms),
    n1 = n1,
    n2 = n2,
    model = model,
    sub_model = sub_plan$atom_sub_model,
    envir = prep_envir,
    src = src
  )
  atom_state <- new.env(parent = emptyenv())
  for (a in seq_len(n_atoms)) {
    assign(
      as.character(a),
      matrix(stat_cache[[a]]$stat, n1, n2),
      envir = atom_state
    )
  }
  stat_cache <- lapply(stat_cache, "[[", "cache")

  net_update_lookup <- matrix(NA_integer_, nrow(sub_plan$objects), n_atoms)
  att_update_lookup <- matrix(NA_integer_, nrow(sub_plan$objects), n_atoms)
  net_update_lookup[cbind(
    sub_plan$effect_objects$oid,
    sub_plan$effect_objects$gid
  )] <- sub_plan$effect_objects$net_update
  att_update_lookup[cbind(
    sub_plan$effect_objects$oid,
    sub_plan$effect_objects$gid
  )] <- sub_plan$effect_objects$att_update

  call_atom_template <- function(
    gid,
    shape,
    event_args,
    net_update,
    att_update
  ) {
    template <- effects_template[[gid]]
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
        event_order = 0L,
        inter_event_time = 0
      ),
      event_args
    )
    do.call(template$fun, args[template$args_by_shape[[shape]]])
  }

  # Apply one constraint-atom object event: route the change to every atom
  # reading the object (pre-event state, mirroring the main loop's op_state),
  # update the atom's live matrix, then commit the change to the private state.
  apply_atom_event <- function(k) {
    oid <- schedule$target[k]
    component <- sub_plan$objects$component[oid]
    key <- sub_plan$objects$key[oid]
    shape <- schedule$shape[k]
    is_undirected_net <- sub_plan$objects$is_undirected[oid]

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
        replace_value <- state[[component]][[key]][event_node] + increment_value
      } else {
        replace_value <- schedule$value[[k]]
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
          state$networks[[key]][event_sender, event_receiver] + increment_value
      } else {
        replace_value <- schedule$value[[k]]
      }
      event_args <- list(
        sender = event_sender,
        receiver = event_receiver,
        replace = replace_value
      )
    }

    for (gid in sub_plan$routing[[oid]]) {
      net_update_pos <- net_update_lookup[oid, gid]
      if (is.na(net_update_pos)) {
        net_update_pos <- NULL
      }
      att_update_pos <- att_update_lookup[oid, gid]
      if (is.na(att_update_pos)) {
        att_update_pos <- NULL
      }

      effect_update <- call_atom_template(
        gid,
        shape,
        event_args,
        net_update_pos,
        att_update_pos
      )
      if (!is.null(effect_update$cache)) {
        stat_cache[[gid]] <<- effect_update$cache
      }
      updates <- effect_update$changes
      if (is_undirected_net) {
        ea2 <- event_args
        ea2$sender <- event_args$receiver
        ea2$receiver <- event_args$sender
        eu2 <- call_atom_template(
          gid,
          shape,
          ea2,
          net_update_pos,
          att_update_pos
        )
        if (!is.null(eu2$cache)) {
          stat_cache[[gid]] <<- eu2$cache
        }
        updates <- rbind(updates, eu2$changes)
      }
      if (!is.null(updates)) {
        exp <- expand_operand_update(updates, atom_kinds[gid], n1, n2)
        am <- get(as.character(gid), envir = atom_state)
        am[exp$cells] <- exp$vals
        assign(as.character(gid), am, envir = atom_state)
      }
    }

    if (shape == "global") {
      state$globals[[key]] <<- event_args$replace
    } else if (shape == "node") {
      state[[component]][[key]][event_args$node] <<- event_args$replace
    } else {
      state$networks[[key]][event_args$sender, event_args$receiver] <<-
        event_args$replace
      if (is_undirected_net) {
        state$networks[[key]][event_args$receiver, event_args$sender] <<-
          event_args$replace
      }
    }
  }

  # The support is piecewise-constant, changing only at the atoms' events. The
  # cursor advances the atom stream monotonically; `advance(t)` applies every
  # atom event strictly before `t` (a lagged risk set), so calling it at an
  # ascending series of times walks the stream exactly once.
  atom_ks <- which(!is.na(schedule$target))
  atom_times <- schedule$time[atom_ks]
  n_atom <- length(atom_ks)
  cursor <- new.env(parent = emptyenv())
  cursor$next_atom <- 1L
  advance <- function(tt) {
    while (cursor$next_atom <= n_atom && atom_times[cursor$next_atom] < tt) {
      apply_atom_event(atom_ks[cursor$next_atom])
      cursor$next_atom <- cursor$next_atom + 1L
    }
    invisible(NULL)
  }

  # Address an atom's current dense value by its deparsed label. Labels are
  # unique within a sub-plan (the parser deduplicates atoms by deparse), and a
  # constraint sharing this maintainer's atoms addresses them the same way, so a
  # by-label lookup decouples evaluation from any one constraint's atom order.
  atom_matrix <- function(label) {
    get(as.character(match(label, atom_labels)), envir = atom_state)
  }

  list(
    n1 = n1,
    n2 = n2,
    atom_labels = atom_labels,
    advance = advance,
    atom_matrix = atom_matrix
  )
}

#' Evaluate one constraint's boolean tree over a maintainer's current atom state
#'
#' Binds each of the constraint's atoms (addressed by label, in the constraint's
#' own `.a{k}` order) to the maintainer's current dense value and evaluates the
#' mask expression, symmetrising a coordination / undirected mask.
#'
#' @param maintainer a handle from `build_atom_maintainer()` whose atoms are a
#'   (super)set of `atom_labels`.
#' @param expr the evaluable mask expression over `.a{k}` placeholders.
#' @param atom_labels the constraint's atom labels, aligned with `.a{k}`.
#' @param symmetric symmetrise the dyad grid (coordination / undirected REM).
#' @return an n1 x n2 logical support mask.
#' @noRd
eval_constraint_mask <- function(maintainer, expr, atom_labels, symmetric) {
  atom_values <- stats::setNames(
    lapply(atom_labels, maintainer$atom_matrix),
    paste0(".a", seq_along(atom_labels))
  )
  m <- assemble_support_mask(atom_values, expr)
  m <- matrix(as.logical(m), maintainer$n1, maintainer$n2)
  if (symmetric) {
    m <- symmetrize_mask(m)
  }
  m
}

#' Maintain the support-constraint mask across the event sequence
#'
#' Runs a stripped recipe pass over the constraint sub-plan, snapshotting the
#' support mask (the dyadic constraint before presence conjunction)
#' at each requested snapshot time. Presence (`active_1`/`active_2`) is joined
#' later by the gather / rate consumer via [assemble_model_mask()].
#'
#' The support changes only at constraint-atom object events, so it is
#' piecewise-constant in time. The pass advances the atoms over their own event
#' streams and snapshots the support at every `snapshot_times[e]` using the atom
#' state STRICTLY before that time (an event's risk set is lagged — its own
#' change is not yet applied). Passing the preprocessed object's
#' stored-event times aligns the timeline with its events by construction, even
#' though the right-censored events come from the main model's streams (not
#' carried by the constraint sub-plan).
#'
#' @param sub_plan the compiled constraint sub-plan from
#'   `compile_support_constraint()` (its `effect_functions`, registries, links,
#'   `expr`, `atom_kinds`, `mask_kind`, and `fetch_plan`).
#' @param model the model string (`"DyNAM"` / `"REM"`).
#' @param nodes,nodes2 sender/receiver nodeset names, resolved in `prep_envir`.
#' @param symmetric symmetrise the support (coordination / undirected REM).
#' @param snapshot_times numeric vector of times (the preprocessed object's
#'   stored-event times, in event order) at which to snapshot the support.
#' @param prep_envir environment holding the realized data objects.
#' @return a list with `support` (one n1 x n2 logical per snapshot time, aligned
#'   with the preprocessed object's events), `initial` (the mask before any
#'   event), `mask_kind`, `n_stored`, and `symmetric`.
#' @noRd
preprocess_support_mask <- function(
  sub_plan,
  model,
  nodes,
  nodes2,
  symmetric = FALSE,
  snapshot_times = numeric(0),
  prep_envir = new.env(),
  src = NULL
) {
  maintainer <- build_atom_maintainer(
    sub_plan,
    model,
    nodes,
    nodes2,
    prep_envir = prep_envir,
    src = src
  )
  expr <- sub_plan$expr
  atom_labels <- sub_plan$atom_labels
  eval_here <- function() {
    eval_constraint_mask(maintainer, expr, atom_labels, symmetric)
  }

  support_init <- eval_here()
  # Snapshots are taken in time order (mapping back to the caller's event order)
  # so the atom stream is advanced once.
  n_snap <- length(snapshot_times)
  support <- vector("list", n_snap)
  for (idx in order(snapshot_times)) {
    maintainer$advance(snapshot_times[idx])
    support[[idx]] <- eval_here()
  }

  list(
    support = support,
    initial = support_init,
    mask_kind = sub_plan$mask_kind,
    n_stored = n_snap,
    symmetric = symmetric
  )
}

#' Maintain a shared atom pool once, project a per-fid mask from it
#'
#' Several constraints over the SAME atoms (a `mutually_exclusive` layer's
#' `!tie(L)` / `tie(L)` derived constraints, or those AND-composed with a shared
#' user constraint) differ only in their boolean expression, not in the atom
#' stream they walk. Maintaining that stream once and projecting each
#' constraint's own expression from it removes the redundant walks the per-output
#' `preprocess_support_mask()` incurs, while evaluation correctly stays per
#' constraint — each fid keeps its own mask at its own snapshot times.
#'
#' Constraints are grouped by atom signature (their sorted labels): within a
#' group the maintenance machinery is identical, so one maintainer serves all of
#' them; across groups (disjoint atoms) separate maintainers run. Each group is
#' advanced over the UNION of its fids' snapshot times once, every distinct
#' expression is evaluated at each union time, and each fid slices its own subset
#' back out — exact because the support is piecewise-constant and the snapshot at
#' a time depends only on the atoms strictly before it (so a superset walk
#' sliced to a subset equals the subset walk).
#'
#' Partial atom overlap across DISTINCT signatures (the multivariate case, where
#' processes over different layers share some but not all atoms) is not pooled
#' here; it belongs to the merged single-clock walk. Within a single flavored
#' process all of a layer's flavors carry the same atom signature, so this pools
#' them to one walk.
#'
#' @param requests a list of `list(constraint, snapshot_times)`, one per fid, in
#'   the caller's order; `constraint = NULL` yields a `NULL` result (no mask).
#' @param model,nodes,nodes2,symmetric,prep_envir,src as in
#'   [preprocess_support_mask()]; `symmetric` is uniform across the family.
#' @return a list aligned with `requests`; each element is the `support_mask`
#'   list `preprocess_support_mask()` returns, or `NULL` for a `NULL` constraint.
#' @noRd
preprocess_pooled_support_masks <- function(
  requests,
  model,
  nodes,
  nodes2,
  symmetric = FALSE,
  prep_envir = new.env(),
  src = NULL
) {
  result <- vector("list", length(requests))
  has_constraint <- vapply(
    requests,
    function(r) !is.null(r$constraint),
    logical(1)
  )
  if (!any(has_constraint)) {
    return(result)
  }

  con_idx <- which(has_constraint)
  signature <- vapply(
    con_idx,
    function(i) {
      paste(sort(requests[[i]]$constraint$atom_labels), collapse = "\r")
    },
    character(1)
  )

  for (group in split(con_idx, signature)) {
    constraints <- lapply(group, function(i) requests[[i]]$constraint)
    maintainer <- build_atom_maintainer(
      constraints[[1L]],
      model,
      nodes,
      nodes2,
      prep_envir = prep_envir,
      src = src
    )

    # Distinct constraints (by identity) within the group: creation and
    # dissolution share atoms but not their expression, so each is evaluated,
    # while several fids sharing one `constraint_id` map to a single evaluation.
    distinct <- list()
    slot <- integer(length(group))
    for (i in seq_along(group)) {
      hit <- NA_integer_
      for (j in seq_along(distinct)) {
        if (identical(distinct[[j]], constraints[[i]])) {
          hit <- j
          break
        }
      }
      if (is.na(hit)) {
        distinct[[length(distinct) + 1L]] <- constraints[[i]]
        hit <- length(distinct)
      }
      slot[i] <- hit
    }

    eval_distinct <- function() {
      lapply(distinct, function(con) {
        eval_constraint_mask(maintainer, con$expr, con$atom_labels, symmetric)
      })
    }

    initial <- eval_distinct()
    union_times <- sort(unique(unlist(
      lapply(group, function(i) requests[[i]]$snapshot_times)
    )))
    n_union <- length(union_times)
    masks <- replicate(length(distinct), vector("list", n_union), FALSE)
    for (ti in seq_len(n_union)) {
      maintainer$advance(union_times[ti])
      snap <- eval_distinct()
      for (di in seq_along(distinct)) {
        masks[[di]][[ti]] <- snap[[di]]
      }
    }

    for (i in seq_along(group)) {
      ri <- group[i]
      di <- slot[i]
      times <- requests[[ri]]$snapshot_times
      result[[ri]] <- list(
        support = masks[[di]][match(times, union_times)],
        initial = initial[[di]],
        mask_kind = constraints[[i]]$mask_kind,
        n_stored = length(times),
        symmetric = symmetric
      )
    }
  }

  result
}
