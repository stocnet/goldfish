# =========================================================================== #
# Single-pass multi-flavor preprocessing.
#
# A multi-flavor specification models K competing processes (flavors) on one
# focal layer. Preprocessing walks the event sequence ONCE -- the
# union of all flavors' effects is computed a single time over one shared
# process state, and one `preprocessed.goldfish` object is emitted per flavor.
# This is the multi-consumer generalization of the recipe loop (one clock, one
# state, N formula plans reading it) that `simulate()` and the DyNES augmenter
# also require.
#
# The seam: a UNION spec_map is built from the union of the flavors' effect
# terms (deduplicated by canonical term label), and each flavor rides a
# `consumer` carrying (a) the union-column indices its formula maps to (its
# `effect_map`), (b) its `has_intercept` / right-censoring, and (c) its derived
# support-constraint plan. The recipe loop routes each event to the consumers
# (dependent to the event's own flavor, right-censored to the others on timed
# rate sub-models) and projects the shared union statistics to each flavor's
# columns.
# =========================================================================== #

# Build the effect union across a sub-model family's per-flavor formulas.
#
# `bundles_by_flavor` is a named list flavor -> specification bundle (as built
# by build_specification_bundle(): `input_formula`, `has_intercept`,
# `sub_model`). The union deduplicates effect terms by their canonical
# `term.labels` (which align with `rhs_names` order and capture every argument),
# preserving first-appearance order. The union formula carries an explicit `1`
# iff some flavor has a time intercept, so the union walk stores right-censored
# events whenever any flavor needs them; each flavor's own right-censoring is
# governed by its `has_intercept`.
#
# Returns the `union_formula` to compile, the ordered `union_labels`, the
# `union_intercept` flag, and per-flavor `effect_maps` (each a vector of union
# column indices in that flavor's formula order) and `has_intercept`.
build_flavor_union <- function(bundles_by_flavor) {
  flavors <- names(bundles_by_flavor)
  formulas <- lapply(bundles_by_flavor, `[[`, "input_formula")
  labels_by_flavor <- lapply(formulas, function(f) {
    attr(stats::terms(f), "term.labels")
  })
  has_intercept <- vapply(bundles_by_flavor, `[[`, logical(1), "has_intercept")

  union_labels <- unique(unlist(labels_by_flavor, use.names = FALSE))
  union_intercept <- any(has_intercept)
  rhs_terms <- if (union_intercept) c("1", union_labels) else union_labels

  env <- environment(formulas[[1]])
  union_formula <- if (length(rhs_terms) == 0) {
    stats::as.formula("~1", env = env)
  } else {
    stats::as.formula(
      paste("~", paste(rhs_terms, collapse = " + ")),
      env = env
    )
  }

  # Per flavor: the union column index of each of its local effects, in its own
  # formula order. `match` against `union_labels` (not the intercept-prefixed
  # `rhs_terms`) because the intercept produces no statistics column, so union
  # gid k corresponds to `union_labels[k]`.
  effect_maps <- lapply(labels_by_flavor, function(lbls) {
    match(lbls, union_labels)
  })
  names(effect_maps) <- flavors

  list(
    flavors = flavors,
    union_formula = union_formula,
    union_labels = union_labels,
    union_intercept = union_intercept,
    effect_maps = effect_maps,
    has_intercept = stats::setNames(has_intercept, flavors)
  )
}

# Plan one sub-model family (`"rate"` / `"choice"`) of a multi-flavor
# specification: collect the per-flavor bundles from `spec$processes`, build
# their effect union, and parse the union formula into the bundle preprocessing
# consumes. The union bundle is what drives the single shared walk; the returned
# `effect_maps` / `has_intercept` are what each flavor's consumer rides on.
#
# Flavors are a stocnet-only surface (keys resolve against `ties$flavor` or the
# layer's update encoding), so the union re-parse runs against the
# specification's stocnet data with a fresh environment, as the original parse
# did.
plan_flavor_union <- function(spec, family) {
  bundles_by_flavor <- lapply(spec$processes, function(p) p$submodels[[family]])
  if (any(vapply(bundles_by_flavor, is.null, logical(1)))) {
    cli::cli_abort(
      "Sub-model {.val {family}} is missing for some modeled flavor{?s}.",
      .internal = TRUE
    )
  }

  union <- build_flavor_union(bundles_by_flavor)
  union$family <- family
  union$sub_model <- bundles_by_flavor[[1L]]$sub_model
  union$bundle <- build_specification_bundle(
    union$union_formula,
    arg = family,
    model = spec$model,
    sub_model = union$sub_model,
    layer = spec$focal,
    envir = new.env(),
    data = spec$data
  )
  union
}

# Invert a flavor's effect_map into a union-gid -> local-column lookup over the
# union's `n_union` statistics columns: `local[u]` is the flavor's column for
# union gid `u`, or `NA` when the flavor's formula does not use that effect.
# A flavor's effects map to distinct union columns (term labels are unique
# within a formula), so the inverse is well defined.
flavor_gid_lookup <- function(effect_map, n_union) {
  lookup <- rep(NA_integer_, n_union)
  lookup[effect_map] <- seq_along(effect_map)
  lookup
}

# Project a flat statistics update block (rows: node1-1/kind, node2/fixed,
# gid-1, replace) onto a flavor's local columns: keep only columns whose union
# gid the flavor uses, and rewrite the gid row to the flavor's local column
# index. Both the point buffer and the broadcast buffer carry `gid - 1` in row
# 3, so one projection serves both. `gid_lookup` maps union gid -> local column
# (NA when unused). Returns a 4-row matrix.
project_update_block <- function(block, gid_lookup) {
  if (ncol(block) == 0L) {
    return(block)
  }
  union_gid <- block[3L, ] + 1L
  local_col <- gid_lookup[union_gid]
  keep <- !is.na(local_col)
  if (!any(keep)) {
    return(matrix(0, 4L, 0L))
  }
  out <- block[, keep, drop = FALSE]
  out[3L, ] <- local_col[keep] - 1L
  out
}

# =========================================================================== #
# Consumers: one per output object (one flavor, or the single output of a
# plain / single-flavor spec). A consumer owns a `writer_default`, its
# union-gid -> local-column projection (`gid_lookup`, NULL for the identity
# single-output fast path), its right-censoring, and its own pending point /
# broadcast buffers. The recipe loop drives a list of consumers over one shared
# state walk: it accumulates each computed statistics delta into every consumer
# (projected to that consumer's columns) and routes each event -- dependent to
# the event's own flavor, right-censored to the others on timed rate sub-models.
#
# A single consumer with `gid_lookup = NULL` reproduces the historical
# single-writer walk byte-for-byte: projection is a no-op, and the
# dependent/right-censored writes and their buffer resets match the previous
# inline logic exactly. The flat statistics log is a cumulative,
# replace-idempotent stream, so a consumer only needs the deltas since its own
# last write; a dependent write resets both buffers, a right-censored write only
# the right-censored buffer.
# =========================================================================== #

# Build the recipe loop's consumer list, initializing each consumer's writer.
#
# `consumer_specs = NULL` is the single-output default: one consumer wrapping
# the loop's own writer with the identity projection, initialized exactly as the
# single-writer loop did, so plain and single-flavor preprocessing is unchanged.
# Otherwise each spec (`writer`, `effect_map`, `has_intercept`) becomes a
# consumer initialized with its LOCAL statistics dimensions -- its own column
# count, its own initial statistics (the union columns it uses), and its own
# right-censoring -- over the shared union walk.
#
# `initial_stats_fn` stays a thunk all the way down: the recipe applies
# pre-start updates to `initialStats` after this call, and the writers read it
# only at finalize.
init_consumers <- function(
  consumer_specs,
  writer,
  right_censored,
  spec,
  dims,
  initial_stats_fn
) {
  writer_dims <- function(n_effects, has_intercept) {
    list(
      nEffects = n_effects,
      n1 = dims$n1,
      n2 = dims$n2,
      is_sender = dims$is_sender,
      has_intercept = has_intercept,
      buf_capacity = max(1000, as.double(n_effects) * dims$n_dependent),
      max_store = dims$max_store
    )
  }

  if (is.null(consumer_specs)) {
    writer$init(
      spec,
      c(
        writer_dims(dims$nEffects, right_censored),
        list(initial_stats_fn = initial_stats_fn)
      )
    )
    return(list(new_consumer(writer, right_censored = right_censored)))
  }

  consumers <- lapply(consumer_specs, function(cspec) {
    effect_map <- cspec$effect_map
    cspec$writer$init(
      spec,
      c(
        writer_dims(length(effect_map), cspec$has_intercept),
        list(
          initial_stats_fn = function() {
            initial_stats_fn()[, effect_map, drop = FALSE]
          }
        )
      )
    )
    new_consumer(
      cspec$writer,
      gid_lookup = flavor_gid_lookup(effect_map, dims$nEffects),
      right_censored = cspec$has_intercept
    )
  })
  names(consumers) <- names(consumer_specs)
  # Routing resolves a dependent event's flavor to its consumer through this
  # lookup rather than indexing the consumer set by flavor name: flavor names
  # are arbitrary user strings, while the consumer set (like everything else
  # downstream) is keyed by formula id.
  attr(consumers, "flavor_index") <- stats::setNames(
    names(consumer_specs),
    vapply(consumer_specs, `[[`, character(1), "flavor")
  )
  consumers
}

# The consumer set for one family walk of a multi-flavor specification: one
# consumer per modeled flavor, keyed by its formula id. Each rides its own
# projection onto the union statistics (`effect_map`), its own intercept /
# right-censoring, and the COMPILED sub-plan of its `(layer, flavor)`
# constraint, looked up from the family plan by `constraint_id` so a constraint
# shared by several flavors is compiled and realized once.
build_consumer_specs <- function(
  consumer_plan,
  compiled_constraints,
  new_writer = writer_default
) {
  specs <- lapply(consumer_plan, function(cp) {
    constraint <- if (is.na(cp$constraint_id)) {
      NULL
    } else {
      compiled_constraints[[as.character(cp$constraint_id)]]
    }
    list(
      fid = cp$fid,
      flavor = cp$flavor,
      effect_map = cp$effect_map,
      has_intercept = cp$has_intercept,
      constraint = constraint,
      writer = new_writer()
    )
  })
  names(specs) <- names(consumer_plan)
  specs
}

new_consumer <- function(writer, gid_lookup = NULL, right_censored = FALSE) {
  e <- new.env(parent = emptyenv())
  e$writer <- writer
  e$gid_lookup <- gid_lookup
  e$right_censored <- right_censored
  e$pending_dep <- list()
  e$pending_dep_cols <- 0L
  e$pending_rc <- list()
  e$pending_rc_cols <- 0L
  e$pending_dep_bc <- list()
  e$pending_dep_bc_cols <- 0L
  e$pending_rc_bc <- list()
  e$pending_rc_bc_cols <- 0L
  e
}

# Append one point-buffer block to a consumer's pending buffers (dependent
# always, right-censored when the consumer stores right-censored events),
# projecting to the consumer's columns first. A NULL `gid_lookup` stores the
# block unchanged (single-output fast path).
consumer_accumulate_point <- function(cs, block) {
  pb <- if (is.null(cs$gid_lookup)) {
    block
  } else {
    project_update_block(block, cs$gid_lookup)
  }
  if (ncol(pb) == 0L) {
    return(invisible(NULL))
  }
  cs$pending_dep[[length(cs$pending_dep) + 1L]] <- pb
  cs$pending_dep_cols <- cs$pending_dep_cols + ncol(pb)
  if (cs$right_censored) {
    cs$pending_rc[[length(cs$pending_rc) + 1L]] <- pb
    cs$pending_rc_cols <- cs$pending_rc_cols + ncol(pb)
  }
  invisible(NULL)
}

consumer_accumulate_broadcast <- function(cs, bc_block) {
  pb <- if (is.null(cs$gid_lookup)) {
    bc_block
  } else {
    project_update_block(bc_block, cs$gid_lookup)
  }
  if (ncol(pb) == 0L) {
    return(invisible(NULL))
  }
  cs$pending_dep_bc[[length(cs$pending_dep_bc) + 1L]] <- pb
  cs$pending_dep_bc_cols <- cs$pending_dep_bc_cols + ncol(pb)
  if (cs$right_censored) {
    cs$pending_rc_bc[[length(cs$pending_rc_bc) + 1L]] <- pb
    cs$pending_rc_bc_cols <- cs$pending_rc_bc_cols + ncol(pb)
  }
  invisible(NULL)
}

# Flush a dependent event to a consumer's writer and reset all four buffers.
consumer_write_dependent <- function(cs, event_info) {
  cs$writer$write_event(
    if (cs$pending_dep_cols > 0L) {
      do.call(cbind, cs$pending_dep)
    } else {
      matrix(0, 4L, 0L)
    },
    event_info,
    if (cs$pending_dep_bc_cols > 0L) {
      do.call(cbind, cs$pending_dep_bc)
    } else {
      matrix(0, 4L, 0L)
    }
  )
  cs$pending_dep <- list()
  cs$pending_dep_cols <- 0L
  cs$pending_rc <- list()
  cs$pending_rc_cols <- 0L
  cs$pending_dep_bc <- list()
  cs$pending_dep_bc_cols <- 0L
  cs$pending_rc_bc <- list()
  cs$pending_rc_bc_cols <- 0L
  invisible(NULL)
}

# =========================================================================== #
# Finalization: one output object per consumer.
# =========================================================================== #

# Close the walk. The single-output default finalizes the one writer and
# realizes the plan's support mask exactly as before. A flavored walk finalizes
# each consumer against its OWN inputs -- the union initial statistics projected
# to its columns, and its own derived-plus-user constraint -- and returns a
# flavor-named list of `preprocessed.goldfish` objects.
#
# `project_initial_stats` differs per loop (a sender kernel is indexed on its
# second margin, a dyad array on its third), and `finish_output` carries each
# loop's own mask realization and availability fold, applied to whichever
# constraint the output it is finishing belongs to.
finalize_consumers <- function(
  consumers,
  consumer_specs,
  tail,
  default_constraint,
  project_initial_stats,
  finish_output,
  scalar_entity = "sender"
) {
  if (is.null(consumer_specs)) {
    return(finish_output(
      consumers[[1L]]$writer$finalize(tail),
      default_constraint
    ))
  }

  outputs <- lapply(names(consumers), function(fl) {
    cspec <- consumer_specs[[fl]]
    flavor_tail <- tail
    flavor_tail$initialStats <- project_initial_stats(
      tail$initialStats,
      cspec$effect_map
    )
    out <- finish_output(
      consumers[[fl]]$writer$finalize(flavor_tail),
      cspec$constraint
    )
    if (!is.null(out$avg_active_entity)) {
      out$avg_active_entity <- time_weighted_risk_set(out, scalar_entity)
    }
    out
  })
  names(outputs) <- names(consumers)
  outputs
}

# A flavor's intercept scalar: the average size of its post-constraint risk set
# over the observation window, weighted by how long each interval lasts --
# (1/T) * integral |R_g(t)| dt, the quantity the baseline-rate starting value
# `log(n_dep_events / total_time / avg_active_entity)` inverts.
#
# The single-process writer averages per stored event instead, which coincides
# only when intervals are equally long. Competing processes make them uneven by
# construction: each flavor's risk set is re-read at every other flavor's event
# and at every flip of its own derived mask, so the sizes must be weighted by
# the time they were in force.
#
# The entity is the one the sub-model's rate is defined over: active senders for
# the actor-oriented rate, active dyads for the tie-oriented (REM) rate.
time_weighted_risk_set <- function(out, entity) {
  n_stored <- length(out$event_time)
  if (n_stored == 0L) {
    return(out$avg_active_entity)
  }
  presence <- if (identical(entity, "dyad")) {
    walk_dyad_presence(out, n_stored)
  } else {
    walk_presence_buffer(
      out$active_sender_init,
      out$active_sender_update,
      out$active_sender_update_pointer,
      n_stored
    )
  }
  sizes <- vapply(presence, sum, numeric(1))
  total_time <- sum(out$intervals)
  if (total_time <= 0) {
    return(mean(sizes))
  }
  sum(out$intervals * sizes) / total_time
}

# Walk the folded dyad availability (the REM point encoding: a dense n1 x n2
# init plus per-event `(node1, node2, replace)` flips) into one logical matrix
# per stored event.
walk_dyad_presence <- function(out, n_stored) {
  cur <- out$active_dyad_init
  update <- out$active_dyad_update
  pointer <- out$active_dyad_update_pointer
  res <- vector("list", n_stored)
  prev <- 0L
  for (e in seq_len(n_stored)) {
    hi <- if (!is.null(pointer)) pointer[e] else 0L
    if (hi > prev) {
      cols <- (prev + 1L):hi
      cur[cbind(update[1L, cols], update[2L, cols])] <-
        as.logical(update[3L, cols])
    }
    prev <- hi
    res[[e]] <- cur
  }
  res
}

# Route a dependent event over the consumer set: it is the dependent
# observation of its own flavor's process, and an interval boundary -- a
# right-censored row -- for every other right-censoring process, since the event
# ends an interval of their rate integrals too. A single-output walk carries no
# flavor and has no other consumers, so it writes exactly the one dependent row
# it always did.
route_dependent_event <- function(
  consumers,
  rc_consumers,
  flavor,
  event_info
) {
  own <- resolve_flavor_consumer(consumers, flavor)
  if (!is.null(own)) {
    consumer_write_dependent(own, event_info)
  }
  if (event_info$interval > 0) {
    event_info$is_dependent <- 0L
    for (cs in rc_consumers) {
      if (!identical(cs, own)) {
        consumer_write_rc(cs, event_info)
      }
    }
  }
  invisible(NULL)
}

# The consumer whose process owns this event, via the walk's (layer, flavor) ->
# fid lookup. A single-output walk carries no lookup and no flavor, so it
# resolves to its one consumer. `NULL` means no modeled process claims the
# event: it is then only an interval boundary for the right-censoring
# consumers, which is how an unmodeled flavor's event enters every modeled
# flavor's rate integral.
resolve_flavor_consumer <- function(consumers, flavor) {
  index <- attr(consumers, "flavor_index")
  if (is.null(index)) {
    return(consumers[[1L]])
  }
  if (is.na(flavor)) {
    return(NULL)
  }
  key <- index[flavor]
  if (is.na(key)) NULL else consumers[[key]]
}

# Route a non-dependent (state-only) event: an interval boundary for every
# right-censoring process.
route_right_censored_event <- function(rc_consumers, event_info) {
  for (cs in rc_consumers) {
    consumer_write_rc(cs, event_info)
  }
  invisible(NULL)
}

# Flush a right-censored event to a consumer's writer and reset the
# right-censored buffers only.
consumer_write_rc <- function(cs, event_info) {
  cs$writer$write_event(
    if (cs$pending_rc_cols > 0L) {
      do.call(cbind, cs$pending_rc)
    } else {
      matrix(0, 4L, 0L)
    },
    event_info,
    if (cs$pending_rc_bc_cols > 0L) {
      do.call(cbind, cs$pending_rc_bc)
    } else {
      matrix(0, 4L, 0L)
    }
  )
  cs$pending_rc <- list()
  cs$pending_rc_cols <- 0L
  cs$pending_rc_bc <- list()
  cs$pending_rc_bc_cols <- 0L
  invisible(NULL)
}

# =========================================================================== #
# The driver: one call, both family walks, a fid-indexed return.
#
# Identity is the `process_map` table, not a naming convention: every
# likelihood-producing formula of the specification gets an integer formula id
# (fid), and the returned list is indexed by it. Layer and flavor names are
# arbitrary user strings -- they may collide with each other or carry dots, so
# a pasted key like "phone.calls.creation.rate" cannot be parsed back -- which
# is why human-readable labels are RENDERED from the table on demand and never
# read back as data.
# =========================================================================== #

# One constraint id per distinct `(layer, flavor)` constraint. Flavors sharing
# an identical parsed constraint (the `redundant` style, where every flavor
# inherits the user constraint unchanged) share an id, so that constraint is
# compiled and its mask realized once instead of once per flavor. A flavor with
# no constraint at all gets `NA`.
#
# Returns the flavor -> id lookup and the distinct parsed plans, named by id.
assign_constraint_ids <- function(processes) {
  plans <- lapply(processes, `[[`, "constraint")
  ids <- stats::setNames(rep(NA_integer_, length(plans)), names(processes))
  distinct <- list()
  for (i in seq_along(plans)) {
    if (is.null(plans[[i]])) {
      next
    }
    hit <- NA_integer_
    for (j in seq_along(distinct)) {
      if (identical(distinct[[j]], plans[[i]])) {
        hit <- j
        break
      }
    }
    if (is.na(hit)) {
      distinct[[length(distinct) + 1L]] <- plans[[i]]
      hit <- length(distinct)
    }
    ids[[i]] <- hit
  }
  names(distinct) <- as.character(seq_along(distinct))
  list(ids = ids, plans = distinct)
}

# Render a human-readable label for a process, e.g. "friendship > creation >
# rate". For cli messages and coefficient names only -- labels are rendered
# from the map, never parsed back into identity.
render_process_label <- function(process_map, fid) {
  row <- process_map[match(fid, process_map$fid), , drop = FALSE]
  paste(row$layer, row$flavor, row$family, sep = " › ")
}

# Preprocess a multi-flavor specification in one call.
#
# Each sub-model family (rate, choice) runs ONE walk over the event sequence:
# the union of that family's per-flavor effects is computed once and each
# flavor's consumer projects it onto its own columns. The two families keep
# separate walks -- gids are scoped per statistic block, and an effect in
# DyNAM-rate and the same effect in DyNAM-choice resolve to different update
# functions, so there is nothing to share between them.
#
# Returns a list of `preprocessed.goldfish` objects indexed by fid, carrying the
# `process_map` identity table as an attribute.
preprocess_flavored <- function(
  spec,
  control_preprocessing = set_preprocessing(),
  progress = getOption("progress", default = FALSE),
  verbose = getOption("verbose", default = FALSE)
) {
  if (is.null(spec$processes)) {
    cli::cli_abort(
      "{.fn preprocess_flavored} requires a multi-flavor specification.",
      .internal = TRUE
    )
  }
  flavors <- names(spec$processes)
  families <- names(spec$processes[[1L]]$submodels)
  constraints <- assign_constraint_ids(spec$processes)

  outputs <- list()
  map_rows <- vector("list", length(families))
  next_fid <- 0L

  for (fi in seq_along(families)) {
    family <- families[[fi]]
    union <- plan_flavor_union(spec, family)
    fids <- stats::setNames(next_fid + seq_along(flavors), flavors)
    next_fid <- next_fid + length(flavors)
    fid_keys <- as.character(fids)

    # The consumer plan is metadata only (fid, flavor, projection, intercept,
    # constraint id); the writers and the compiled constraints are attached
    # downstream, once the family's spec_map has compiled them.
    consumer_plan <- lapply(flavors, function(fl) {
      list(
        fid = fids[[fl]],
        flavor = fl,
        effect_map = union$effect_maps[[fl]],
        has_intercept = unname(union$has_intercept[[fl]]),
        constraint_id = constraints$ids[[fl]],
        # This flavor's own two-sided formula: the object's statistics are the
        # projection onto these effects, so this is what describes it, not the
        # union formula that drove the shared walk.
        formula = spec$processes[[fl]]$submodels[[family]]$formula
      )
    })
    names(consumer_plan) <- fid_keys

    preps <- estimate_wrapper(
      x = union$bundle$formula,
      model = spec$model,
      sub_model = union$sub_model,
      data = spec$data,
      control_preprocessing = control_preprocessing,
      preprocessing_only = TRUE,
      progress = progress,
      verbose = verbose,
      support_constraint = if (length(constraints$plans) > 0) {
        constraints$plans
      } else {
        NULL
      },
      modeled_flavor = flavors,
      flavor_plan = list(consumers = consumer_plan)
    )
    outputs[fid_keys] <- preps[fid_keys]

    map_rows[[fi]] <- data.frame(
      fid = unname(fids),
      layer = spec$focal,
      flavor = flavors,
      family = family,
      # The statistic block a consumer's gids are scoped to: an effect is
      # deduplicated only among formulas resolving to the same update function.
      stat_block = paste(spec$model, union$sub_model, sep = ":"),
      has_intercept = unname(union$has_intercept[flavors]),
      constraint_id = unname(constraints$ids[flavors]),
      stringsAsFactors = FALSE
    )
  }

  process_map <- do.call(rbind, map_rows)

  # Fail fast, per process: a flavor whose derived mask contradicts the user
  # constraint leaves an empty risk set, and the message must say WHICH process
  # is empty -- the label is rendered from the map for that message alone.
  for (i in seq_len(nrow(process_map))) {
    key <- as.character(process_map$fid[i])
    validate_prep_support(
      outputs[[key]],
      is_rate_family = identical(process_map$family[i], "rate"),
      process_label = render_process_label(process_map, process_map$fid[i])
    )
    # Stamped so estimation does not re-run the same check and duplicate every
    # warning it just emitted; the message here is the more useful of the two,
    # since only this one can name the process.
    outputs[[key]]$support_validated <- TRUE
  }

  structure(
    outputs,
    process_map = process_map,
    class = "flavored_preprocessed.goldfish"
  )
}
