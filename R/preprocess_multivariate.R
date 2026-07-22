# =========================================================================== #
# Cross-process preprocessing planning for a joint specification.
#
# `preprocess_flavored.R` walks the flavors of ONE focal layer over one shared
# state, deduplicating effects across flavors within a sub-model family. A joint
# specification composes several processes (layers), so the same two ideas
# generalize by widening their key:
#
#   * the effect UNION widens from "flavors of one layer, one family" to "every
#     fid across all processes that resolves to the same update function" -- one
#     union per statistic block (`stat_block` = model + sub-model), never
#     crossing dispatch families (`plan_block_unions()`);
#   * consumer ROUTING widens from a flavor key (unambiguous within one layer) to
#     a `(layer, flavor)` key, since a flavor name repeats across layers -- the
#     schedule carries layer and flavor for every dependent stream, and a
#     cross-process event right-censors every OTHER timed rate fid regardless of
#     layer (`build_route_index()` / `route_partition()`).
#
# These are the planning helpers the merged single-clock walk consumes; the
# fid is the canonical key throughout, matching the flavored consumer contract.
# =========================================================================== #

# fid -> its (bundle, model, sub_model, layer, flavor, family, constraint). Walks
# `flatten_joint_processes()` and, within each process, its families, assigning
# fids in exactly the order `build_joint_process_map()` does -- so this list and
# the `process_map` agree on what every fid denotes. Keyed by the character fid.
joint_fid_bundles <- function(joint_spec) {
  procs <- flatten_joint_processes(joint_spec$specifications)
  res <- list()
  fid <- 0L
  for (proc in procs) {
    for (family in names(proc$submodels)) {
      fid <- fid + 1L
      bundle <- proc$submodels[[family]]
      res[[as.character(fid)]] <- list(
        fid = fid,
        bundle = bundle,
        model = proc$model,
        sub_model = bundle$sub_model,
        layer = proc$layer,
        flavor = proc$flavor,
        family = family,
        constraint = proc$constraint
      )
    }
  }
  res
}

# Plan one effect union per statistic block of a joint specification. Fids are
# grouped by their `stat_block` (model + sub-model, the dispatch family), and
# within each block every fid's effects are deduplicated into one set of union
# columns; a fid's `effect_map` records which union column each of its own
# effects occupies. Deduplication never crosses a block boundary: the same effect
# label in DyNAM-choice and in REM resolves to different update functions, so
# each block computes its own column (design D5).
#
# Fids are processed in ascending fid order within a block (the canonical order
# the `process_map` is built in), so union first-appearance order is
# deterministic. Returns a list keyed by `stat_block`; each element is the
# `build_effect_union()` result (keyed by character fid) plus the block's
# `model` / `sub_model` / `family` metadata and its member `fids`.
plan_block_unions <- function(joint_spec) {
  process_map <- joint_spec$process_map
  fid_bundles <- joint_fid_bundles(joint_spec)

  # `split()` preserves within-group order, and the map is already fid-ordered,
  # so each block's fids stay ascending.
  blocks <- split(process_map$fid, process_map$stat_block)

  lapply(blocks, function(fids) {
    fid_keys <- as.character(fids)
    bundles <- lapply(fid_keys, function(k) fid_bundles[[k]]$bundle)
    names(bundles) <- fid_keys

    union <- build_effect_union(bundles)
    first <- fid_bundles[[fid_keys[[1L]]]]
    union$model <- first$model
    union$sub_model <- first$sub_model
    union$family <- first$family
    union$fids <- fids
    union
  })
}

# =========================================================================== #
# Routing: (layer, flavor) -> fid.
#
# A joint specification's events each occur on one (layer, flavor); the walk
# routes each event by that pair. The route index is one row per dependent
# stream (per fid) carrying its layer, flavor, family and right-censoring, so a
# matched (layer, flavor) resolves to the fid(s) that own the event as a
# dependent observation. Matching is on the two columns, not a pasted key: layer
# and flavor are arbitrary user strings that may collide or carry separators, so
# only the integer fid is a safe identity (the same reason the flavored router
# keys consumers by fid rather than by flavor name).
# =========================================================================== #

# The (layer, flavor) -> fid schedule for a joint specification's walk: the
# routing-relevant columns of the `process_map`, one row per fid.
build_route_index <- function(process_map) {
  process_map[, c("fid", "layer", "flavor", "family", "has_intercept")]
}

# The fid(s) an event on `(layer, flavor)` owns as a dependent observation -- its
# rate and choice fids both, since the same event times the rate and is the
# choice. `NA` flavor (a plain, unflavored process) matches only `NA`-flavor
# rows. Returns `NA_integer_` when no modeled process claims the event: it is
# then only a right-censoring boundary for the timed rate fids.
resolve_route_fids <- function(route_index, layer, flavor) {
  same_layer <- route_index$layer == layer
  same_flavor <- if (is.na(flavor)) {
    is.na(route_index$flavor)
  } else {
    !is.na(route_index$flavor) & route_index$flavor == flavor
  }
  fids <- route_index$fid[same_layer & same_flavor]
  if (length(fids) == 0L) NA_integer_ else fids
}

# Partition every fid of the walk for an event on `(layer, flavor)`, mirroring
# `route_dependent_event()` / `route_right_censored_event()`:
#   * `dependent`     -- the fids owning the event (its own (layer, flavor));
#   * `right_censored` -- every OTHER timed rate fid (`has_intercept`), whose
#     rate integral this event closes an interval of, regardless of layer;
#   * `state_only`    -- the remaining fids (choice/ordered, untimed rates),
#     whose state advances but which record nothing for this event.
# Right-censoring keys on `has_intercept` exactly as the flavored consumer does,
# so an untimed (ordered) rate is never right-censored.
route_partition <- function(route_index, layer, flavor) {
  dependent <- resolve_route_fids(route_index, layer, flavor)
  dependent <- dependent[!is.na(dependent)]

  timed_rate <- route_index$fid[route_index$has_intercept]
  right_censored <- setdiff(timed_rate, dependent)
  state_only <- setdiff(route_index$fid, c(dependent, right_censored))

  list(
    dependent = dependent,
    right_censored = right_censored,
    state_only = state_only
  )
}
