# =========================================================================== #
# Merged single-clock walk substrate for a joint specification.
#
# `preprocess_flavored.R` walks the flavors of ONE focal layer over one shared
# state, and `preprocess_multivariate.R` widened the effect union and consumer
# routing across processes. This file assembles the substrate the merged walk
# steps through: it compiles each process into a `spec_map` and builds ONE shared
# process-state container and ONE shared event schedule spanning the UNION of the
# processes' objects and events.
#
# It is deliberately ADDITIVE: it runs no event loop and touches none of the
# recipe loops (`run_sender_recipe_loop` / `run_dyad_recipe_loop`) nor
# `preprocess_flavored`, so the frozen single-process/flavored baselines are
# provably unaffected. The merged loop that consumes this substrate, its per-fid
# routing, and the walk handle are built on top of it.
#
# Two ideas structure the substrate:
#
#   * COMPILE PER PROCESS. `build_spec_map()` resolves a formula against ONE
#     focal layer (sides, modes, dependent rows), but a statistic block of a
#     joint specification spans several focals. Compilation is therefore done
#     per process (per focal) -- one `spec_map` per `(focal, sub-model family)`,
#     unioning that focal's flavors exactly as the flavored walk does -- and the
#     resulting maps are grouped into blocks keyed by `stat_block`.
#
#   * ONE SHARED STATE AND SCHEDULE. Coupling is possible only when the processes
#     read one shared state, so the objects the per-process maps reference are
#     unioned by object identity into a single state container, and the event
#     streams into a single time-ordered schedule whose targets index that shared
#     object registry. Each process keeps a `shared_oid -> block_oid` map so the
#     merged loop can translate a shared-state object back to that process's own
#     object index.
# =========================================================================== #

# --------------------------------------------------------------------------- #
# Compile-only seam.
#
# The parse -> build_spec_map portion of `estimate_wrapper()`, isolated for the
# joint recipe case (a `stocnet` data object, a DyNAM or REM process, no
# incremental `preprocessing_init`), with the estimation scaffolding dropped. It
# returns a compiled `spec_map` WITHOUT running `preprocess()`, so a caller gets
# the walk-ready compilation without the walk. The primitive calls and their
# order mirror `estimate_wrapper()` so a process compiled here matches the one
# the single-process/flavored path compiles.
# --------------------------------------------------------------------------- #
compile_recipe_spec_map <- function(
  formula,
  model,
  sub_model,
  data,
  support_constraint = NULL,
  modeled_flavor = NULL,
  impute_policy = NULL
) {
  work_env <- new.env()
  work_data <- data

  aliased <- resolve_dependent_alias(formula, work_data, modeled_flavor)
  formula <- aliased$formula
  modeled_flavor <- aliased$modeled_flavor

  parsed_formula <- parse_formula(
    formula,
    envir = work_env,
    realize_windows = FALSE,
    data = work_data
  )
  dep_name <- parsed_formula$dep_name
  has_intercept <- parsed_formula$has_intercept
  window_parameters <- parsed_formula$window_parameters

  # A compile models exactly ONE focal, so the modeled layer is stamped onto this
  # PRIVATE per-process working copy (copy-on-modify -- never the caller's
  # object) so `build_spec_map()`'s internal sources resolve sides/modes against
  # it. This is not the merged walk's shared state: that state hosts N focals and
  # threads focal per fid instead, so it never carries a single stamped focal.
  work_data$info$focal <- dep_name

  abort_if_interactions_unsupported(parsed_formula, model, sub_model)

  # Intercept semantics mirror the recipe path: choice / choice_coordination and
  # rate_ordered ignore the time intercept; a bare `rate` formula gains it (the
  # baseline hazard the waiting-time likelihood needs).
  if (
    has_intercept &&
      ((model %in%
        c("DyNAM", "DyNAMi") &&
        sub_model %in% c("choice", "choice_coordination")) ||
        sub_model == "rate_ordered")
  ) {
    parsed_formula$has_intercept <- has_intercept <- FALSE
  }
  if (sub_model == "rate" && !has_intercept) {
    parsed_formula$has_intercept <- has_intercept <- TRUE
  }

  legacy_sub_model <- sub_model
  if (sub_model == "rate_ordered") {
    legacy_sub_model <- "rate"
  }
  if (model == "REM") {
    legacy_sub_model <- "choice"
  }

  work_src <- new_data_source(
    data = work_data,
    envir = work_env,
    focal = dep_name,
    modeled_flavor = modeled_flavor
  )
  # A length-2 answer means the process spans two modes: side one is the sender
  # set, side two the receiver set.
  .nodes <- ds_layer_sides(work_src, dep_name)
  is_two_mode <- FALSE
  if (length(.nodes) == 2) {
    .nodes2 <- .nodes[2]
    .nodes <- .nodes[1]
    is_two_mode <- TRUE
  } else {
    .nodes2 <- .nodes
  }

  effects <- create_effects_functions(
    parsed_formula$rhs_names,
    model,
    legacy_sub_model,
    envir = work_env,
    derivations = parsed_formula$window_derivations,
    data = work_data
  )
  objects_effects_link <- get_objects_effects_link(parsed_formula$rhs_names)
  link <- build_events_objects_link(
    dep_name,
    parsed_formula$rhs_names,
    .nodes,
    .nodes2,
    envir = work_env,
    derivations = parsed_formula$window_derivations,
    data = work_data
  )
  events_effects_link <- get_events_effects_link(
    parsed_formula$rhs_names,
    link$events_objects_link
  )

  model_spec <- new_model_spec(
    model = model,
    sub_model = sub_model,
    is_two_mode = is_two_mode,
    nodes = .nodes,
    nodes2 = .nodes2,
    has_intercept = has_intercept
  )

  spec_map <- build_spec_map(
    parsed_formula,
    model_spec,
    effects,
    window_parameters,
    objects_effects_link,
    link$events_objects_link,
    events_effects_link,
    link$fetch_plan,
    support_constraint = support_constraint,
    envir = work_env,
    data = work_data,
    modeled_flavor = modeled_flavor
  )
  # The per-attribute imputation policy rides on the compiled spec so the walk
  # reaches it without threading through the loop, exactly as `preprocess_recipe`
  # attaches it before dispatching `preprocess()`.
  spec_map$impute_policy <- impute_policy
  spec_map
}

# --------------------------------------------------------------------------- #
# Per-process compilation.
# --------------------------------------------------------------------------- #

# The formula, sub-model and flavor mapping to compile for one `(focal, family)`
# process. A multi-flavor specification unions its flavors' effects for the
# family exactly as the flavored walk does (`plan_flavor_union()`), so the union
# formula drives the shared compile and each flavor's `effect_map` records its
# projection; a plain specification is the single-output case (its bundle drives
# the compile directly, with no projection).
process_family_plan <- function(spec, family) {
  if (is.null(spec$processes)) {
    bundle <- spec$submodels[[family]]
    modeled_flavor <- if (length(spec$modeled_flavor) > 0) {
      spec$modeled_flavor
    } else {
      NULL
    }
    return(list(
      formula = bundle$formula,
      sub_model = bundle$sub_model,
      modeled_flavor = modeled_flavor,
      flavors = NA_character_,
      effect_maps = NULL,
      has_intercept = bundle$has_intercept
    ))
  }
  union <- plan_flavor_union(spec, family)
  list(
    formula = union$bundle$formula,
    sub_model = union$sub_model,
    modeled_flavor = names(spec$processes),
    flavors = union$flavors,
    effect_maps = union$effect_maps,
    has_intercept = union$has_intercept
  )
}

# The parsed support-constraint plans a specification contributes, keyed by the
# joint `constraint_id` the `process_map` assigns them, so the compiled
# sub-plans land in `plan$support_constraints` under the same ids the merged walk
# routes on. `NULL` when the specification declares no constraint.
spec_constraint_plans <- function(spec, focal, process_map) {
  rows <- process_map[process_map$layer == focal, , drop = FALSE]
  plans <- list()
  for (proc in spec_processes(spec)) {
    if (is.null(proc$constraint)) {
      next
    }
    flavor <- proc$flavor
    match_row <- if (is.na(flavor)) {
      is.na(rows$flavor)
    } else {
      !is.na(rows$flavor) & rows$flavor == flavor
    }
    id <- rows$constraint_id[match_row][1]
    if (!is.na(id)) {
      plans[[as.character(id)]] <- proc$constraint
    }
  }
  if (length(plans) == 0) NULL else plans
}

# Compile the `(spec, family)` process into a unit: its `spec_map` plus the
# routing facts the block assembly needs (focal, family, the fids it owns in the
# `process_map`, its stat block and shape, and the per-flavor projection).
compile_process_unit <- function(spec, family, joint_spec, impute_policy) {
  fp <- process_family_plan(spec, family)
  spec_map <- compile_recipe_spec_map(
    formula = fp$formula,
    model = spec$model,
    sub_model = fp$sub_model,
    data = joint_spec$data,
    support_constraint = spec_constraint_plans(
      spec,
      spec$focal,
      joint_spec$process_map
    ),
    modeled_flavor = fp$modeled_flavor,
    impute_policy = impute_policy
  )

  pm <- joint_spec$process_map
  fid_rows <- pm[pm$layer == spec$focal & pm$family == family, , drop = FALSE]

  list(
    key = paste(spec$focal, family, sep = ":"),
    focal = spec$focal,
    family = family,
    model = spec$model,
    sub_model = fp$sub_model,
    stat_block = paste(spec$model, fp$sub_model, sep = ":"),
    is_sender = inherits(spec_map, "sender_spec"),
    fids = fid_rows$fid,
    flavors = fp$flavors,
    effect_maps = fp$effect_maps,
    spec_map = spec_map
  )
}

# --------------------------------------------------------------------------- #
# Shared object union.
# --------------------------------------------------------------------------- #

# Union every process's `plan$objects` by object identity (`key`) into one shared
# registry, first-appearance ordered over the units. Each shared object appears
# once however many processes read it (`friendship` read by two choice formulas
# is one shared network), and each unit gets a `shared_oid -> block_oid` map:
# `shared_to_local[[key]][s]` is that unit's own object index for shared oid `s`,
# or `NA` where the unit does not read it. The reverse `local_to_shared` maps a
# unit's own oid to the shared oid.
build_shared_objects <- function(units) {
  registry_rows <- list()
  keys <- character(0)
  for (unit in units) {
    objs <- unit$spec_map$plan$objects
    for (i in seq_len(nrow(objs))) {
      key <- objs$key[i]
      if (key %in% keys) {
        next
      }
      keys <- c(keys, key)
      registry_rows[[length(registry_rows) + 1L]] <- objs[i, , drop = FALSE]
    }
  }
  registry <- do.call(rbind, registry_rows)
  registry$oid <- seq_len(nrow(registry))
  rownames(registry) <- NULL

  shared_to_local <- list()
  local_to_shared <- list()
  for (unit in units) {
    objs <- unit$spec_map$plan$objects
    to_shared <- match(objs$key, registry$key)
    to_local <- rep(NA_integer_, nrow(registry))
    to_local[to_shared] <- objs$oid
    shared_to_local[[unit$key]] <- to_local
    local_to_shared[[unit$key]] <- to_shared
  }

  list(
    registry = registry,
    shared_to_local = shared_to_local,
    local_to_shared = local_to_shared
  )
}

# --------------------------------------------------------------------------- #
# Shared state container.
# --------------------------------------------------------------------------- #

# One state container holding the union of the processes' objects. Every read
# object -- each process's focal network and the covariates any formula reads --
# is materialized once, so a coupled process reads the same live state a
# dependent process writes.
#
# The focal side views are seeded from a representative process; the remaining
# views are reached through the object references, which is exact for a join over
# one shared node set. A join spanning several mode-pairs would seed a focal view
# per distinct side -- deferred with the per-mode-pair block keying it belongs to.
build_shared_state <- function(units, shared_objects, data, src) {
  first <- units[[1]]$spec_map
  build_state_container(
    shared_objects$registry$name,
    nodes = first$nodes,
    nodes2 = first$nodes2,
    src = src
  )
}

# --------------------------------------------------------------------------- #
# Shared event schedule.
# --------------------------------------------------------------------------- #

# One event-per-row contribution of a fetched stream to the merged schedule:
# `events` is the fetched stream, `target` its shared object oid (`NA` for a
# process's dependent stream, which evaluates but writes no state), `layer` the
# object/focal name the routing keys on, and `dependent` whether it is a
# process's own event stream. Mirrors `build_event_schedule()`'s per-stream shape
# and semantics detection.
schedule_stream_part <- function(
  events,
  target,
  layer,
  dependent,
  stream_index
) {
  n <- nrow(events)
  if (n == 0L) {
    return(NULL)
  }
  cols <- names(events)
  semantics <- if ("increment" %in% cols) "increment" else "replace"
  sender <- rep(NA_integer_, n)
  receiver <- rep(NA_integer_, n)
  node <- rep(NA_integer_, n)
  if ("node" %in% cols) {
    shape <- "node"
    node <- as.integer(events$node)
  } else if ("sender" %in% cols) {
    shape <- "dyad"
    sender <- as.integer(events$sender)
    receiver <- as.integer(events$receiver)
  } else {
    shape <- "global"
  }
  list(
    time = events$time,
    shape = rep(shape, n),
    target = rep(target, n),
    semantics = rep(semantics, n),
    sender = sender,
    receiver = receiver,
    node = node,
    value = as.list(events[[semantics]]),
    layer = rep(layer, n),
    flavor = if ("flavor" %in% cols) {
      as.character(events$flavor)
    } else {
      rep(NA_character_, n)
    },
    dependent = rep(dependent, n),
    stream_index = rep(stream_index, n)
  )
}

# Merge the processes' event streams into one time-ordered schedule over the
# shared object registry. Each process's dependent stream contributes evaluation
# rows (`target = NA`, keyed by its focal layer, carrying the flavor a competing
# process routes on); every distinct covariate stream contributes state-update
# rows targeting its shared object. A stream shared across processes -- an
# exogenous covariate, or a focal network another process reads -- is fetched and
# added once. Ordering is `(time, stream_index)`, dependent streams indexed
# before their own covariate stream so an event is evaluated at the state before
# it is applied.
build_joint_schedule <- function(units, shared_objects) {
  parts <- list()
  next_index <- 0L
  dep_seen <- character(0)
  stream_seen <- character(0)

  add_part <- function(events, target, layer, dependent) {
    next_index <<- next_index + 1L
    part <- schedule_stream_part(events, target, layer, dependent, next_index)
    if (!is.null(part)) {
      parts[[length(parts) + 1L]] <<- part
    }
  }

  for (unit in units) {
    spec_map <- unit$spec_map
    src <- new_data_source(
      data = spec_map$data,
      envir = new.env(),
      focal = spec_map$focal,
      modeled_flavor = spec_map$modeled_flavor
    )
    src <- ds_realize_derivations(src, spec_map$plan$derivations)
    events <- fetch_events(spec_map$fetch_plan, src = src)
    eol <- spec_map$events_objects_link

    dep_stream <- spec_map$fetch_plan[[1]]$stream
    if (!(unit$focal %in% dep_seen)) {
      dep_seen <- c(dep_seen, unit$focal)
      add_part(events[[dep_stream]], NA_integer_, unit$focal, TRUE)
    }

    for (r in seq_len(nrow(eol))[-1]) {
      stream_key <- eol$events[r]
      if (stream_key %in% stream_seen) {
        next
      }
      stream_seen <- c(stream_seen, stream_key)
      oid <- match(eol$name[r], shared_objects$registry$name)
      add_part(events[[stream_key]], oid, eol$name[r], FALSE)
    }
  }

  combine <- function(field) {
    unlist(lapply(parts, `[[`, field), use.names = FALSE)
  }
  time <- combine("time")
  stream_index <- combine("stream_index")
  ordering <- order(time, stream_index, method = "radix")
  value <- do.call(c, lapply(parts, `[[`, "value"))

  list(
    time = time[ordering],
    shape = combine("shape")[ordering],
    target = combine("target")[ordering],
    semantics = combine("semantics")[ordering],
    sender = combine("sender")[ordering],
    receiver = combine("receiver")[ordering],
    node = combine("node")[ordering],
    value = value[ordering],
    layer = combine("layer")[ordering],
    flavor = combine("flavor")[ordering],
    dependent = combine("dependent")[ordering],
    stream_index = stream_index[ordering],
    n = length(ordering)
  )
}

# --------------------------------------------------------------------------- #
# Top-level: compile the blocks and build the shared substrate.
# --------------------------------------------------------------------------- #

#' Assemble the merged-walk substrate for a joint specification
#'
#' Compiles every process of a `joint_specification.goldfish` into a walk-ready
#' `spec_map`, groups the maps into statistic blocks, and builds the single
#' shared process-state container and event schedule the merged single-clock walk
#' steps through. Additive substrate: it runs no event loop.
#'
#' @param joint_spec a `joint_specification.goldfish` from
#'   [make_joint_specification()].
#' @param control_preprocessing preprocessing options (for the imputation policy
#'   carried onto each compiled `spec_map`).
#'
#' @return a `merged_blocks.goldfish` list with: `blocks` (per `stat_block`, its
#'   model/sub-model/family, `is_sender` shape flag, member fids and per-process
#'   unit keys, and the cross-process effect union); `units` (the per-process
#'   compiled `spec_map`s keyed by `focal:family`, each with its `shared_oid ->
#'   block_oid` map); `objects` (the shared object registry); `state` (the shared
#'   state container over the object union); `schedule` (the merged event
#'   schedule); and the `process_map`.
#' @noRd
build_merged_blocks <- function(
  joint_spec,
  control_preprocessing = set_preprocessing_opt()
) {
  if (!inherits(joint_spec, "joint_specification.goldfish")) {
    cli::cli_abort(
      "{.fn build_merged_blocks} requires a {.cls joint_specification.goldfish}.",
      .internal = TRUE
    )
  }

  impute_policy <- control_preprocessing$impute

  # Compile per process (per focal), then per family, in the specification-major
  # order the `process_map` assigns fids, so a unit's fids are exactly its block
  # rows.
  units <- list()
  for (spec in joint_spec$specifications) {
    families <- names(spec_processes(spec)[[1L]]$submodels)
    for (family in families) {
      unit <- compile_process_unit(spec, family, joint_spec, impute_policy)
      units[[unit$key]] <- unit
    }
  }

  shared_objects <- build_shared_objects(units)
  for (key in names(units)) {
    units[[key]]$shared_to_local <- shared_objects$shared_to_local[[key]]
    units[[key]]$local_to_shared <- shared_objects$local_to_shared[[key]]
  }

  # One source over the shared data drives the shared state; the cross-process
  # effect unions (one per block, deduplicating on resolved effect identity)
  # come from the same planner the flavored walk generalizes.
  shared_src <- new_data_source(
    data = joint_spec$data,
    envir = new.env(),
    focal = units[[1L]]$spec_map$focal
  )
  state <- build_shared_state(
    units,
    shared_objects,
    joint_spec$data,
    shared_src
  )
  schedule <- build_joint_schedule(units, shared_objects)
  block_unions <- plan_block_unions(joint_spec)

  block_keys <- unique(vapply(units, `[[`, character(1), "stat_block"))
  blocks <- lapply(block_keys, function(bk) {
    block_units <- Filter(function(u) identical(u$stat_block, bk), units)
    first <- block_units[[1L]]
    list(
      stat_block = bk,
      model = first$model,
      sub_model = first$sub_model,
      family = first$family,
      is_sender = first$is_sender,
      unit_keys = vapply(block_units, `[[`, character(1), "key"),
      fids = sort(unlist(lapply(block_units, `[[`, "fids"), use.names = FALSE)),
      union = block_unions[[bk]]
    )
  })
  names(blocks) <- block_keys

  structure(
    list(
      blocks = blocks,
      units = units,
      objects = shared_objects$registry,
      state = state,
      schedule = schedule,
      process_map = joint_spec$process_map
    ),
    class = "merged_blocks.goldfish"
  )
}
