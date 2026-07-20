# =========================================================================== #
# Single-pass multi-flavor preprocessing.
#
# A multi-flavor specification models K competing processes (flavors) on one
# focal layer. Design D3: preprocessing walks the event sequence ONCE -- the
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
  union$constraints <- lapply(spec$processes, `[[`, "constraint")
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
