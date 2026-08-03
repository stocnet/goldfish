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
  # Keep the pristine model spec reachable so the merged driver can decorate each
  # fid's output with it (the estimation-re-entry metadata), matching the
  # single-process/flavored path without rebuilding it.
  attr(spec_map, "model_spec") <- model_spec
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

# Compile one parsed support constraint into the engine-ready sibling sub-plan
# the mask realization consumes, using the owning layer's compile parameters
# (drawn from any of its already-compiled units, since rate and choice share
# them). A focal-stamped working copy resolves a focal-relative atom against the
# owning layer, exactly as `build_spec_map()`'s own constraint compile does. The
# compile is metadata-only and reference-transparent, so one shared sub-plan is
# byte-equivalent to the per-family compiles it replaces.
compile_one_joint_constraint <- function(constraint_plan, unit, data) {
  work_data <- data
  work_data$info$focal <- unit$focal
  compile_support_constraint(
    constraint_plan,
    model = unit$model,
    dep_name = unit$focal,
    nodes = unit$spec_map$nodes,
    nodes2 = unit$spec_map$nodes2,
    window_derivations = unit$spec_map$parsed_terms$window_derivations,
    envir = new.env(),
    data = work_data
  )
}

# Compile each distinct `(layer, flavor)` support constraint ONCE, keyed by the
# `constraint_id` the process_map assigns it, into the one merged plan every fid
# sharing that id reads (design D3b). The compile is family-invariant, so a
# single sub-plan serves a layer's rate and choice fids; each fid still snapshots
# the mask against its OWN stored `event_time` at finalize (compile once,
# snapshot per fid). `NULL` when the join declares no constraint.
build_joint_support_constraints <- function(joint_spec, units) {
  compiled <- list()
  for (spec in joint_spec$specifications) {
    plans <- spec_constraint_plans(spec, spec$focal, joint_spec$process_map)
    if (is.null(plans)) {
      next
    }
    owner <- Filter(function(u) identical(u$focal, spec$focal), units)[[1L]]
    for (id in names(plans)) {
      if (is.null(compiled[[id]])) {
        compiled[[id]] <- compile_one_joint_constraint(
          plans[[id]],
          owner,
          joint_spec$data
        )
      }
    }
  }
  if (length(compiled) == 0L) NULL else compiled
}

# Compile the `(spec, family)` process into a unit: its `spec_map` plus the
# routing facts the block assembly needs (focal, family, the fids it owns in the
# `process_map`, its stat block and shape, and the per-flavor projection).
compile_process_unit <- function(spec, family, joint_spec, impute_policy) {
  fp <- process_family_plan(spec, family)
  # The support constraints are compiled ONCE per `constraint_id` at the merged
  # level (family-invariant, so a layer's rate and choice fids read one shared
  # sub-plan), not here; the per-unit compile carries only its own statistics.
  spec_map <- compile_recipe_spec_map(
    formula = fp$formula,
    model = spec$model,
    sub_model = fp$sub_model,
    data = joint_spec$data,
    support_constraint = NULL,
    modeled_flavor = fp$modeled_flavor,
    impute_policy = impute_policy
  )

  pm <- joint_spec$process_map
  fid_rows <- pm[pm$layer == spec$focal & pm$family == family, , drop = FALSE]

  # Each fid's own two-sided formula (the projection its statistics describe, not
  # the union formula that drove the shared compile) -- the estimation-re-entry
  # metadata the single-process/flavored path stamps per output.
  processes <- spec_processes(spec)
  proc_flavor <- vapply(processes, `[[`, character(1), "flavor")
  own_formulas <- stats::setNames(
    lapply(seq_len(nrow(fid_rows)), function(i) {
      fl <- fid_rows$flavor[i]
      proc <- if (is.na(fl)) {
        processes[[which(is.na(proc_flavor))[1L]]]
      } else {
        processes[[match(fl, proc_flavor)]]
      }
      proc$submodels[[family]]$formula
    }),
    as.character(fid_rows$fid)
  )

  # The legacy sub-model the output is stamped with mirrors `estimate_wrapper()`:
  # a `rate_ordered` process reports as rate, an REM process as choice.
  legacy_sub_model <- fp$sub_model
  if (legacy_sub_model == "rate_ordered") {
    legacy_sub_model <- "rate"
  }
  if (spec$model == "REM") {
    legacy_sub_model <- "choice"
  }

  list(
    key = paste(spec$focal, family, sep = ":"),
    focal = spec$focal,
    family = family,
    model = spec$model,
    sub_model = fp$sub_model,
    legacy_sub_model = legacy_sub_model,
    stat_block = paste(spec$model, fp$sub_model, sep = ":"),
    is_sender = inherits(spec_map, "sender_spec"),
    fids = fid_rows$fid,
    flavors = fp$flavors,
    effect_maps = fp$effect_maps,
    own_formulas = own_formulas,
    model_spec = attr(spec_map, "model_spec"),
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
#'   schedule); `support_constraints` (each `(layer, flavor)` constraint compiled
#'   once, keyed by `constraint_id`); and the `process_map`.
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

  # One compiled sub-plan per `constraint_id`, shared across the families of the
  # layer that owns it (compile once, snapshot per fid; design D3b).
  support_constraints <- build_joint_support_constraints(joint_spec, units)

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
      support_constraints = support_constraints,
      process_map = joint_spec$process_map
    ),
    class = "merged_blocks.goldfish"
  )
}

# =========================================================================== #
# The merged single-clock walk.
#
# `build_merged_blocks()` compiled the substrate (per-process spec_maps, one
# shared state, one shared schedule); this section runs ONE event loop over that
# schedule, hosting every statistic block over the one shared state and emitting
# one preprocessed object per fid.
#
# It is the multi-process generalization of the two flavored family walks
# (`run_sender_recipe_loop` / `run_dyad_recipe_loop`), merged onto one clock.
# The oracle stays the byte-identical reference: this driver reuses the oracle's
# per-unit setup (`prepare_recipe_context()`), consumer machinery
# (`init_consumers` / `consumer_accumulate_*` / `finalize_consumers`) and folds,
# and reimplements ONLY the loop body and its routing.
#
# Three contracts govern the merge (design D5 / D8a and the pinned RC rule):
#
#   * PER-UNIT INTERVAL CLOCK. The oracle's global `interval = t - time` is the
#     gap between consecutive events of ONE walk's schedule. The merged schedule
#     interleaves every process's events, so a rate fid's rate integral must NOT
#     be split by an event its block never references. Each unit therefore keeps
#     its own clock, advancing (and only then) at an event the unit is party to:
#     its own dependent stream, a covariate it reads, or -- for a timed rate --
#     any OTHER process's dependent event (the cross-process boundary).
#
#   * RC CONTRACT (byte-identity). A modeled dependent event is a dependent
#     observation for its own `(layer, flavor)` fids and a right-censoring
#     boundary for every OTHER timed rate fid (cross-flavor and cross-process
#     alike). A covariate event right-censors a timed rate fid ONLY when that
#     fid's own unit reads the object -- a rate fid never takes an RC row from a
#     choice-only covariate. Single-process / flavored specifications have no
#     other process, so no cross-process boundaries are injected and the outputs
#     match the two-walk oracle byte-for-byte.
#
#   * PER-FID FOCAL (D8a). Focal is never stamped on the shared state. Each unit
#     was compiled against its own focal (`compile_recipe_spec_map`, Pattern A),
#     so a fid's dependent-row/side/mode resolution rides its own compiled
#     spec_map over the one shared state.
# =========================================================================== #

# Wrap a single flavored or plain `specification.goldfish` as a degenerate joint
# specification so the merged substrate builder can walk it. The one-process
# case is what the frozen-baseline gate compares against `preprocess_flavored`:
# it must run through the SAME merged driver as a true multi-process join. The
# constructor's >=2 rule and its cross-process checks do not apply here (there is
# one process), so the joint object is assembled directly.
single_process_joint <- function(spec) {
  structure(
    list(
      specifications = list(spec),
      process_map = build_joint_process_map(list(spec), character(0)),
      data = spec$data,
      modeled_panel = character(0)
    ),
    class = "joint_specification.goldfish"
  )
}

# The shared object property table the covariate branch reads by shared oid: the
# union registry widened with each object's imputation policy (the one field the
# per-process compile does not set, since it is a preprocessing-control input,
# not a spec property). Mirrors the per-object policy stamp
# `prepare_recipe_context()` applies before its own walk.
build_shared_object_props <- function(objects, impute_policy) {
  objects$policy <- vapply(
    objects$key,
    function(key) imputation_policy_for(impute_policy, key),
    character(1)
  )
  objects
}

# Build a covariate event's `event_args` from the shared state and the shared
# object properties, resolving increment semantics and a missing value exactly
# as the recipe loops do (increment adds to the current state; a missing replace
# imputes from the node's mode category, or the reserved level under the
# as-category policy). Computed once per covariate event and shared by every
# unit that reads the object, so the value each unit's effect sees -- and the one
# written back to the shared state -- is identical.
merged_build_event_args <- function(schedule, k, oid, props, state) {
  shape <- schedule$shape[k]
  if (shape == "global") {
    replace_value <- schedule$value[[k]]
    if (is.na(replace_value)) {
      replace_value <- 0
    }
    return(list(replace = replace_value))
  }
  component <- props$component[oid]
  key <- props$key[oid]
  if (shape == "node") {
    event_node <- schedule$node[k]
    if (schedule$semantics[k] == "increment") {
      increment_value <- schedule$value[[k]]
      if (is.na(increment_value)) {
        increment_value <- 0
      }
      replace_value <- state[[component]][[key]][event_node] + increment_value
    } else {
      replace_value <- schedule$value[[k]]
      if (is.na(replace_value)) {
        if (identical(props$policy[oid], "as_category")) {
          replace_value <- IMPUTATION_MISSING_LEVEL
        } else {
          replace_value <- impute_nodal_value(
            state[[component]][[key]],
            event_node,
            attr(state, "strata")[[component]],
            props$value_type[oid]
          )
        }
      }
    }
    return(list(node = event_node, replace = replace_value))
  }
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
    if (is.na(replace_value)) {
      replace_value <- 0
    }
  }
  if (replace_value < 0) {
    warning("You are dissolving a tie which doesn't exist!", call. = FALSE)
  }
  list(
    sender = event_sender,
    receiver = event_receiver,
    replace = replace_value
  )
}

# Apply one covariate event to the shared state, once, after every referencing
# unit has read the pre-update state for its own statistics. Mirrors the state
# write at the tail of the recipe loops' covariate branch.
merged_apply_state_update <- function(state, oid, shape, event_args, props) {
  component <- props$component[oid]
  key <- props$key[oid]
  if (shape == "global") {
    state$globals[[key]] <- event_args$replace
  } else if (shape == "node") {
    state[[component]][[key]][event_args$node] <- event_args$replace
  } else {
    state$networks[[key]][event_args$sender, event_args$receiver] <-
      event_args$replace
    if (props$is_undirected[oid]) {
      state$networks[[key]][event_args$receiver, event_args$sender] <-
        event_args$replace
    }
  }
  invisible(NULL)
}

# One effect template evaluation over the SHARED state. A verbatim port of the
# recipe loops' `call_effect_template()` closure: state reads resolve by object
# key against the one shared container, so a coupled process reads exactly what a
# dependent process last wrote.
merged_call_template <- function(
  template,
  state,
  cache,
  n1,
  n2,
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
      cache = cache,
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

# Compute one unit's statistics delta for a covariate event on its LOCAL object
# `loid` and accumulate it into every consumer of the unit. A port of the recipe
# loops' covariate body (routing loop, undirected mirror, interaction second-hop
# and product emission, point/broadcast accumulation), shape-branching on
# `engine$is_sender` exactly as the two loops diverge. Reads the shared state;
# the state write is the caller's, once per event. The window pre-start branch is
# intentionally absent -- the merged walk supports the full-window case only.
merged_covariate_step <- function(
  engine,
  loid,
  shape,
  event_args,
  is_undirected,
  interval,
  state
) {
  ctx <- engine$ctx
  plan <- ctx$plan
  n1 <- ctx$n1
  n2 <- ctx$n2
  n_fun <- ctx$n_fun
  n_inter <- ctx$n_inter
  is_sender <- engine$is_sender
  bcast_kind <- plan$effects$broadcast_kind
  event_order <- engine$i_total - engine$i_dep

  for (gid in plan$routing[[loid]]) {
    template <- ctx$effects_template[[gid]]
    net_update_pos <- ctx$net_update_lookup[loid, gid]
    if (is.na(net_update_pos)) {
      net_update_pos <- NULL
    }
    att_update_pos <- ctx$att_update_lookup[loid, gid]
    if (is.na(att_update_pos)) {
      att_update_pos <- NULL
    }

    effect_update <- merged_call_template(
      template,
      state,
      engine$stat_cache[[gid]],
      n1,
      n2,
      shape,
      event_args,
      net_update_pos,
      att_update_pos,
      event_order,
      interval
    )
    if (!is.null(attr(effect_update$cache, "last_update"))) {
      attr(engine$stat_cache[[gid]], "last_update") <- attr(
        effect_update$cache,
        "last_update"
      )
    }
    updates <- effect_update$changes
    if (!is.null(effect_update$cache) && !is.null(effect_update$changes)) {
      engine$stat_cache[[gid]] <- effect_update$cache
    }

    if (is_undirected) {
      event_args2 <- event_args
      event_args2$sender <- event_args$receiver
      event_args2$receiver <- event_args$sender
      effect_update2 <- merged_call_template(
        template,
        state,
        engine$stat_cache[[gid]],
        n1,
        n2,
        shape,
        event_args2,
        net_update_pos,
        att_update_pos,
        event_order,
        interval
      )
      if (!is.null(effect_update2$cache) && !is.null(effect_update2$changes)) {
        engine$stat_cache[[gid]] <- effect_update2$cache
      }
      updates <- rbind(updates, effect_update2$changes)
    }

    if (!is.null(updates)) {
      if (n_inter > 0L && gid <= n_fun) {
        feeds <- plan$operand_of[[as.character(gid)]]
        if (!is.null(feeds)) {
          if (is_sender) {
            ov <- get(as.character(gid), envir = engine$op_state)
            ov[updates[, "node1"]] <- updates[, "replace"]
            assign(as.character(gid), ov, envir = engine$op_state)
            for (ig in feeds) {
              igc <- as.character(ig)
              engine$dirty_inter[[igc]] <- c(
                engine$dirty_inter[[igc]],
                updates[, "node1"]
              )
            }
          } else {
            exp <- expand_operand_update(updates, bcast_kind[gid], n1, n2)
            om <- get(as.character(gid), envir = engine$op_state)
            om[exp$cells] <- exp$vals
            assign(as.character(gid), om, envir = engine$op_state)
            for (ig in feeds) {
              igc <- as.character(ig)
              engine$dirty_inter[[igc]] <- rbind(
                engine$dirty_inter[[igc]],
                exp$cells
              )
            }
          }
        }
      }

      if (bcast_kind[gid] != 0L) {
        bc_block <- broadcast_entries_from_updates(
          updates,
          bcast_kind[gid],
          gid
        )
        for (cs in engine$consumers) {
          consumer_accumulate_broadcast(cs, bc_block)
        }
      } else {
        block <- if (is_sender) {
          rbind(updates[, "node1"] - 1, 0, gid - 1, updates[, "replace"])
        } else {
          rbind(
            updates[, "node1"] - 1,
            updates[, "node2"] - 1,
            gid - 1,
            updates[, "replace"]
          )
        }
        for (cs in engine$consumers) {
          consumer_accumulate_point(cs, block)
        }
      }
    }
  }

  if (n_inter > 0L && length(engine$dirty_inter) > 0L) {
    for (igc in names(engine$dirty_inter)) {
      ig <- as.integer(igc)
      ops <- plan$interactions[[igc]]
      if (is_sender) {
        senders <- unique(engine$dirty_inter[[igc]])
        prodv <- get(as.character(ops[1]), envir = engine$op_state)[senders]
        for (o in ops[-1]) {
          prodv <- prodv *
            get(as.character(o), envir = engine$op_state)[senders]
        }
        block <- rbind(senders - 1, 0, ig - 1, prodv)
      } else {
        cells <- dedup_cells(engine$dirty_inter[[igc]], n1)
        prodv <- get(as.character(ops[1]), envir = engine$op_state)[cells]
        for (o in ops[-1]) {
          prodv <- prodv * get(as.character(o), envir = engine$op_state)[cells]
        }
        block <- rbind(cells[, 1] - 1, cells[, 2] - 1, ig - 1, prodv)
      }
      for (cs in engine$consumers) {
        consumer_accumulate_point(cs, block)
      }
    }
    engine$dirty_inter <- list()
  }
  invisible(NULL)
}

# The fid of the consumer a dependent event on `flavor` belongs to within a
# unit, or `NA` when no modeled flavor of the unit owns it (an unmodeled-flavor
# event on a modeled layer -- a boundary for the unit's rates, an observation of
# none). A plain process has one fid and a NA flavor.
engine_owning_fid <- function(engine, flavor) {
  if (engine$plain) {
    return(engine$fids[1L])
  }
  i <- match(flavor, engine$flavors)
  if (is.na(i)) NA_integer_ else engine$fids[i]
}

# Route a dependent event within one owning unit: a dependent write to the fid
# that owns `flavor`, and -- when the interval is positive -- a right-censored
# write to every OTHER right-censoring consumer of the unit (the cross-flavor
# boundary). The cross-process boundary is applied separately, by every other
# timed rate unit. Mirrors `route_dependent_event()` but resolves the owner by
# fid so a plain (NA-flavor) process routes correctly.
merged_route_dependent <- function(engine, flavor, event_info) {
  own_fid <- engine_owning_fid(engine, flavor)
  own <- if (is.na(own_fid)) NULL else engine$consumers[[as.character(own_fid)]]
  if (!is.null(own)) {
    consumer_write_dependent(own, event_info)
  }
  if (event_info$interval > 0) {
    event_info$is_dependent <- 0L
    for (cs in engine$rc_consumers) {
      if (!identical(cs, own)) {
        consumer_write_rc(cs, event_info)
      }
    }
  }
  invisible(NULL)
}

# A right-censoring boundary for every right-censoring consumer of a unit (the
# caller gates on a positive interval). Mirrors `route_right_censored_event()`.
merged_route_rc <- function(engine, event_info) {
  for (cs in engine$rc_consumers) {
    consumer_write_rc(cs, event_info)
  }
  invisible(NULL)
}

# Build one per-unit walk engine: the recipe context (effects, cache, lookups,
# composition, imputation policy) reused from `prepare_recipe_context()`, the
# unit's `initial_stats` and interaction operand state shaped as its recipe loop
# would, and its per-fid consumers over the block union. The per-unit state and
# schedule `prepare_recipe_context()` also builds are discarded: the walk runs
# over the ONE shared state and schedule, which every unit's effect templates
# read by object key.
build_walk_engine <- function(unit, merged, control_preprocessing, progress) {
  spec_map <- unit$spec_map
  loop_sub_model <- if (unit$is_sender) "rate" else "choice"
  prep_envir <- new.env()
  ctx <- prepare_recipe_context(
    spec_map,
    startTime = NULL,
    endTime = NULL,
    prep_envir = prep_envir,
    sub_model = loop_sub_model,
    progress = progress
  )
  if (any(ctx$is_window_effect)) {
    cli::cli_abort(
      "The merged walk does not yet support window effects.",
      .internal = TRUE
    )
  }

  n1 <- ctx$n1
  n2 <- ctx$n2
  n_fun <- ctx$n_fun
  n_inter <- ctx$n_inter
  nEffects <- ctx$nEffects
  inter_ids <- ctx$inter_ids
  plan <- ctx$plan

  # Shape `initial_stats` and the interaction operand state exactly as the sender
  # (2D kernel) or dyad (3D array) recipe loop does before the walk begins.
  op_state <- new.env(parent = emptyenv())
  if (unit$is_sender) {
    initial_stats <- matrix(0, nrow = n1, ncol = nEffects)
    initial_stats[, seq_len(n_fun)] <- do.call(
      cbind,
      lapply(ctx$stat_cache, "[[", "stat")
    )
  } else {
    initial_stats <- array(0, dim = c(n1, n2, nEffects))
    initial_stats[,, seq_len(n_fun)] <- array(
      unlist(lapply(ctx$stat_cache, "[[", "stat")),
      dim = c(n1, n2, n_fun)
    )
  }
  stat_cache <- lapply(ctx$stat_cache, "[[", "cache")

  if (n_inter > 0) {
    operand_gids <- sort(unique(unlist(plan$interactions)))
    if (unit$is_sender) {
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
    } else {
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
  }

  # One consumer per fid the unit owns, over the block union. A flavored unit
  # projects the shared statistics onto each flavor's own columns
  # (`unit$effect_maps`); a plain unit takes the identity projection
  # (`seq_len(nEffects)`), which leaves its single output unchanged. Every fid
  # keeps its own right-censoring and its compiled `(layer, flavor)` constraint.
  pm <- merged$process_map
  fid_has_intercept <- function(fid) {
    pm$has_intercept[match(fid, pm$fid)]
  }
  fid_constraint_id <- function(fid) {
    pm$constraint_id[match(fid, pm$fid)]
  }

  consumer_plan <- lapply(seq_along(unit$fids), function(i) {
    fid <- unit$fids[i]
    flavor <- unit$flavors[i]
    effect_map <- if (is.null(unit$effect_maps)) {
      seq_len(nEffects)
    } else {
      unit$effect_maps[[flavor]]
    }
    list(
      fid = fid,
      flavor = flavor,
      effect_map = effect_map,
      has_intercept = fid_has_intercept(fid),
      constraint_id = fid_constraint_id(fid)
    )
  })
  names(consumer_plan) <- as.character(unit$fids)

  # The compiled constraints come from the ONE merged plan (compile once per
  # constraint_id), not the per-unit plan, so a layer's rate and choice fids
  # reference the SAME sub-plan and each snapshots it against its own timeline.
  consumer_specs <- build_consumer_specs(
    consumer_plan,
    merged$support_constraints
  )
  consumers <- init_consumers(
    consumer_specs,
    writer = writer_default(),
    right_censored = FALSE,
    spec = spec_map,
    dims = list(
      nEffects = nEffects,
      n1 = n1,
      n2 = n2,
      is_sender = unit$is_sender,
      n_dependent = nrow(ctx$events[[1L]]),
      max_store = merged$schedule$n + 1L
    ),
    initial_stats_fn = function() engine$initial_stats
  )
  rc_consumers <- Filter(function(cs) cs$right_censored, consumers)

  engine <- new.env(parent = emptyenv())
  engine$key <- unit$key
  engine$focal <- unit$focal
  engine$is_sender <- unit$is_sender
  engine$sub_model <- unit$sub_model
  engine$model <- unit$model
  engine$plain <- is.null(unit$effect_maps)
  engine$fids <- unit$fids
  engine$flavors <- unit$flavors
  engine$shared_to_local <- unit$shared_to_local
  # Per-fid estimation-re-entry decoration, threaded from the compile. Focal
  # resolved per fid (Pattern A, D8a), so the node lookup is this unit's own.
  engine$legacy_sub_model <- unit$legacy_sub_model
  engine$model_spec <- unit$model_spec
  engine$own_formulas <- unit$own_formulas
  engine$node_lookup <- ds_node_lookup(ctx$src)
  engine$ctx <- ctx
  engine$prep_envir <- prep_envir
  engine$spec_map <- spec_map
  engine$initial_stats <- initial_stats
  engine$stat_cache <- stat_cache
  engine$op_state <- op_state
  engine$dirty_inter <- list()
  engine$consumers <- consumers
  engine$consumer_specs <- consumer_specs
  engine$rc_consumers <- rc_consumers
  engine$is_timed_rate <- length(rc_consumers) > 0L
  engine$i_total <- 0L
  engine$i_dep <- 0L
  engine$last_time <- NA_real_
  engine
}

# Finalize one unit's consumers into its per-fid preprocessed objects, applying
# the family's support-mask fold and intercept scalars. The sender and dyad
# branches reproduce the two recipe loops' `finalize_consumers()` calls
# (`project_initial_stats` on the second vs third margin, the sender-gate vs
# dyad fold, and the risk-set entity the intercept scalar counts).
finalize_walk_engine <- function(engine, start_time, end_time, opportunities) {
  ctx <- engine$ctx
  spec_map <- engine$spec_map
  tail <- list(
    spec = spec_map,
    initial_stats = engine$initial_stats,
    active_sender_init = ctx$active_sender_init,
    active_sender_changes = ctx$active_sender_changes,
    active_dyad_init = ctx$active_dyad_init,
    active_dyad_changes = ctx$active_dyad_changes,
    start_time = start_time,
    end_time = end_time,
    intercept_scalars = engine$is_timed_rate
  )

  if (engine$is_sender) {
    outputs <- finalize_consumers(
      engine$consumers,
      engine$consumer_specs,
      tail = tail,
      default_constraint = ctx$plan$support_constraint,
      project_initial_stats = function(stats, effect_map) {
        stats[, effect_map, drop = FALSE]
      },
      finish_output = function(out, constraint, support_mask = NULL) {
        if (is.null(constraint)) {
          return(out)
        }
        if (is.null(support_mask)) {
          support_mask <- preprocess_support_mask(
            constraint,
            model = engine$model,
            nodes = ctx$nodes,
            nodes2 = ctx$nodes2,
            symmetric = FALSE,
            snapshot_times = out$event_time,
            src = ctx$src,
            prep_envir = engine$prep_envir
          )
        }
        out$support_mask <- support_mask
        fold_active_sender_support(
          out,
          out$support_mask,
          ctx$active_dyad_init
        )
      },
      realize_masks = function(requests) {
        preprocess_pooled_support_masks(
          requests,
          model = engine$model,
          nodes = ctx$nodes,
          nodes2 = ctx$nodes2,
          symmetric = FALSE,
          prep_envir = engine$prep_envir,
          src = ctx$src
        )
      },
      scalar_entity = "sender"
    )
  } else {
    outputs <- finalize_consumers(
      engine$consumers,
      engine$consumer_specs,
      tail = tail,
      default_constraint = ctx$plan$support_constraint,
      project_initial_stats = function(stats, effect_map) {
        stats[,, effect_map, drop = FALSE]
      },
      finish_output = function(out, constraint, support_mask = NULL) {
        if (!is.null(constraint)) {
          if (is.null(support_mask)) {
            support_mask <- preprocess_support_mask(
              constraint,
              model = engine$model,
              nodes = ctx$nodes,
              nodes2 = ctx$nodes2,
              symmetric = identical(spec_map$sub_model, "choice_coordination"),
              snapshot_times = out$event_time,
              src = ctx$src,
              prep_envir = engine$prep_envir
            )
          }
          out$support_mask <- support_mask
          return(fold_active_dyad_support(
            out,
            out$support_mask,
            spec_map,
            constraint$mask_kind,
            opportunitiesList = opportunities
          ))
        }
        if (
          !is.null(opportunities) &&
            spec_map$sub_model %in% c("choice", "choice_coordination")
        ) {
          out <- fold_active_dyad_opportunity(out, opportunities)
        }
        out
      },
      realize_masks = function(requests) {
        preprocess_pooled_support_masks(
          requests,
          model = engine$model,
          nodes = ctx$nodes,
          nodes2 = ctx$nodes2,
          symmetric = identical(spec_map$sub_model, "choice_coordination"),
          prep_envir = engine$prep_envir,
          src = ctx$src
        )
      },
      scalar_entity = "dyad"
    )
  }

  # `finalize_consumers()` returns one object for a single output and a
  # fid-named list otherwise; normalize to a fid-keyed list either way.
  if (inherits(outputs, "preprocessed.goldfish")) {
    outputs <- stats::setNames(list(outputs), as.character(engine$fids[1L]))
  }

  # Decorate each fid's output with the estimation-re-entry metadata the
  # single-process/flavored path stamps (formula, model, sub-model, node sides,
  # node lookup, model spec), each carrying its OWN formula. Side/mode resolution
  # rode the per-fid compiled spec_map (Pattern A, D8a), so the node sides and
  # lookup are this unit's own, never a shared stamped focal.
  stats::setNames(
    lapply(names(outputs), function(key) {
      out <- outputs[[key]]
      out$formula <- engine$own_formulas[[key]]
      out$model <- engine$model
      out$sub_model <- engine$legacy_sub_model
      out$nodes <- ctx$nodes
      out$nodes2 <- ctx$nodes2
      out$node_lookup <- engine$node_lookup
      out$model_spec <- engine$model_spec
      out
    }),
    names(outputs)
  )
}

# Run the merged single-clock walk over the substrate `build_merged_blocks()`
# assembled, returning one `preprocessed.goldfish` object per fid.
run_merged_walk <- function(
  merged,
  control_preprocessing = set_preprocessing_opt(),
  progress = FALSE,
  verbose = FALSE
) {
  if (
    !is.null(control_preprocessing$start_time) ||
      !is.null(control_preprocessing$end_time)
  ) {
    cli::cli_abort(
      "The merged walk does not yet support an explicit start or end time.",
      .internal = TRUE
    )
  }

  schedule <- merged$schedule
  state <- merged$state
  props <- build_shared_object_props(
    merged$objects,
    control_preprocessing$impute
  )

  engines <- lapply(
    merged$units,
    build_walk_engine,
    merged = merged,
    control_preprocessing = control_preprocessing,
    progress = progress
  )

  # The shared clock spans every process's events; each unit's per-unit clock
  # starts here so its first recorded interval is measured from the window open.
  start_time <- if (schedule$n > 0L) min(schedule$time) else 0
  end_time <- if (schedule$n > 0L) max(schedule$time) else 0
  for (engine in engines) {
    engine$last_time <- start_time
  }

  for (k in seq_len(schedule$n)) {
    t <- schedule$time[k]
    shape <- schedule$shape[k]

    if (schedule$dependent[k]) {
      layer <- schedule$layer[k]
      flavor <- schedule$flavor[k]
      if (shape == "node") {
        ev_sender <- schedule$node[k]
        ev_receiver <- schedule$node[k]
      } else {
        ev_sender <- schedule$sender[k]
        ev_receiver <- schedule$receiver[k]
      }

      for (engine in engines) {
        if (identical(engine$focal, layer)) {
          interval <- t - engine$last_time
          engine$last_time <- t
          engine$i_total <- engine$i_total + 1L
          engine$i_dep <- engine$i_dep + 1L
          merged_route_dependent(
            engine,
            flavor,
            list(
              is_dependent = 1L,
              interval = interval,
              time = t,
              sender = ev_sender,
              receiver = ev_receiver
            )
          )
        } else if (engine$is_timed_rate) {
          interval <- t - engine$last_time
          engine$last_time <- t
          engine$i_total <- engine$i_total + 1L
          if (interval > 0) {
            merged_route_rc(
              engine,
              list(
                is_dependent = 0L,
                interval = interval,
                time = t,
                sender = ev_sender,
                receiver = ev_receiver
              )
            )
          }
        }
      }
      next
    }

    # Covariate event: right-censor and update statistics for every unit that
    # reads the object, all against the pre-update state, then write the state
    # once. A rate fid never takes an RC row here unless its own unit reads the
    # object, keeping a choice-only covariate off the rate fid's timeline.
    oid <- schedule$target[k]
    event_args <- merged_build_event_args(schedule, k, oid, props, state)
    is_undirected <- props$is_undirected[oid]
    if (shape == "global") {
      ev_sender <- NA_integer_
      ev_receiver <- NA_integer_
    } else if (shape == "node") {
      ev_sender <- schedule$node[k]
      ev_receiver <- schedule$node[k]
    } else {
      ev_sender <- schedule$sender[k]
      ev_receiver <- schedule$receiver[k]
    }

    for (engine in engines) {
      loid <- engine$shared_to_local[oid]
      if (is.na(loid)) {
        next
      }
      interval <- t - engine$last_time
      engine$last_time <- t
      engine$i_total <- engine$i_total + 1L
      if (engine$is_timed_rate && interval > 0) {
        merged_route_rc(
          engine,
          list(
            is_dependent = 0L,
            interval = interval,
            time = t,
            sender = ev_sender,
            receiver = ev_receiver
          )
        )
      }
      merged_covariate_step(
        engine,
        loid,
        shape,
        event_args,
        is_undirected,
        interval,
        state
      )
    }

    merged_apply_state_update(state, oid, shape, event_args, props)
  }

  outputs <- list()
  for (engine in engines) {
    engine_outputs <- finalize_walk_engine(
      engine,
      start_time,
      end_time,
      control_preprocessing$opportunities_list
    )
    outputs[names(engine_outputs)] <- engine_outputs
  }

  # Engine-readiness: fail fast, per fid, when a derived mask contradicts the
  # user constraint and leaves an empty risk set -- naming the process the empty
  # set belongs to via the label rendered from the map. The check is stamped so
  # estimation does not re-run it, exactly as the flavored driver does.
  process_map <- merged$process_map
  for (i in seq_len(nrow(process_map))) {
    key <- as.character(process_map$fid[i])
    validate_prep_support(
      outputs[[key]],
      is_rate_family = identical(process_map$family[i], "rate"),
      process_label = render_process_label(process_map, process_map$fid[i])
    )
    outputs[[key]]$support_validated <- TRUE
  }

  # Return fid-keyed in canonical fid order, with the process_map attached, so
  # the merged output indexes exactly as the flavored / joint planners do.
  ordered_keys <- as.character(sort(as.integer(names(outputs))))
  structure(
    outputs[ordered_keys],
    process_map = merged$process_map,
    class = "joint_preprocessed.goldfish"
  )
}

# Preprocess a joint (or single) specification through the merged single-clock
# walk. Accepts a `joint_specification.goldfish` directly, or a single
# `specification.goldfish` (wrapped as a one-process join) so the same driver
# serves the frozen-baseline gate.
preprocess_joint <- function(
  spec,
  control_preprocessing = set_preprocessing_opt(),
  progress = getOption("progress", default = FALSE),
  verbose = getOption("verbose", default = FALSE)
) {
  joint_spec <- if (inherits(spec, "joint_specification.goldfish")) {
    spec
  } else if (inherits(spec, "specification.goldfish")) {
    single_process_joint(spec)
  } else {
    cli::cli_abort(
      "{.fn preprocess_joint} requires a {.cls specification.goldfish} or
       {.cls joint_specification.goldfish}.",
      .internal = TRUE
    )
  }
  merged <- build_merged_blocks(joint_spec, control_preprocessing)
  run_merged_walk(merged, control_preprocessing, progress, verbose)
}
