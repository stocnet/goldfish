#' Compose process specifications into a multivariate specification
#'
#' `r lifecycle::badge("experimental")`
#'
#' `make_joint_specification()` combines two or more [make_specification()]
#' objects built over one shared data object into a single multivariate
#' specification portraying their co-evolution. It is the specification surface
#' for DyNES, where panel-observed relational states co-evolve with time-stamped
#' relational events and augmentation *couples* the processes. The returned
#' object is estimated by `estimate_dynes()` (its surface and augmentation live
#' in separate changes); the event-stream estimators [estimate_dynam()] /
#' [estimate_rem()] reject it.
#'
#' @details
#' At least one **panel-observed layer MUST be referenced** in the composed
#' formulas -- as a process's focal/dependent layer OR as an exogenous covariate
#' read by another process's effects or support-constraint atoms. A combination
#' referencing no panel-observed layer is rejected: those processes are exactly
#' separable and should be estimated with the per-process estimators. Note that
#' construction checks *structural* panel presence only; whether any panel layer
#' is a *modeled* process (the DyNES-viability requirement) is deferred to
#' `estimate_dynes()`.
#'
#' Each joined specification must model a **distinct focal layer** -- a layer is
#' modeled by at most one specification (a duplicated focal layer aborts, naming
#' it), while reading a layer as an exogenous covariate is unrestricted (it is
#' exactly the coupling that makes joining meaningful). All of a layer's flavors
#' are carried by a single specification.
#'
#' DyNAM-i processes are excluded. All processes share one node set in this
#' version.
#'
#' @param ... two or more `specification.goldfish` objects (from
#'   [make_specification()]) over one shared data object.
#' @param data the shared `stocnet` data object the specifications were built
#'   over. Defaults to the data carried by the first specification.
#'
#' @return an S3 object of class `joint_specification.goldfish`. It carries the
#'   composed `specifications`, the shared `data`, and the extended integer-fid
#'   `process_map` (one row per likelihood-producing formula across all
#'   processes, with the shared `constraint_id`s and a `coupled` flag marking
#'   fids that read a modeled panel layer's latent state). It deliberately does
#'   NOT inherit `specification.goldfish`.
#'
#' @seealso [make_specification()]
#' @export
make_joint_specification <- function(..., data = NULL) {
  specs <- list(...)

  if (length(specs) < 2) {
    cli::cli_abort(c(
      "{.fn make_joint_specification} needs at least two specifications.",
      "i" = "A single process is estimated with {.fn estimate_dynam} or
             {.fn estimate_rem}."
    ))
  }
  not_spec <- !vapply(
    specs,
    inherits,
    logical(1),
    what = "specification.goldfish"
  )
  if (any(not_spec)) {
    cli::cli_abort(c(
      "Every argument in {.arg ...} must be a {.cls specification.goldfish}.",
      "x" = "Argument{?s} {.val {which(not_spec)}} {?is/are} not.",
      "i" = "Build each process with {.fn make_specification}."
    ))
  }

  # DyNAM-i is deferred to its own preprocessing-path change and cannot appear
  # in a joint specification; guard defensively on the model marker.
  bad_model <- !vapply(
    specs,
    function(s) s$model %in% c("DyNAM", "REM"),
    logical(1)
  )
  if (any(bad_model)) {
    cli::cli_abort(c(
      "DyNAM-i processes cannot be composed into a joint specification.",
      "i" = "Only {.val DyNAM} and {.val REM} processes are supported."
    ))
  }

  shared_data <- data %||% specs[[1]]$data
  if (
    is.environment(shared_data) ||
      !is.list(shared_data) ||
      is.null(shared_data$info)
  ) {
    cli::cli_abort(c(
      "{.fn make_joint_specification} requires a {.cls stocnet} data object.",
      "i" = "The legacy environment interface is not supported here."
    ))
  }

  # One node set in this version: every specification's dependent process must
  # sit on the same node set(s).
  node_sigs <- lapply(specs, function(s) {
    sort(unique(c(s$dependent$nodes, s$dependent$nodes2)))
  })
  if (!all(vapply(node_sigs, identical, logical(1), node_sigs[[1]]))) {
    cli::cli_abort(c(
      "All composed processes must share one node set.",
      "i" = "Mixed node sets are not supported in this version."
    ))
  }

  # Focal uniqueness: a layer is modeled by at most one specification. Covariate
  # reuse is allowed and is not checked here (it is the coupling).
  focals <- vapply(specs, function(s) s$focal, character(1))
  dup_focal <- unique(focals[duplicated(focals)])
  if (length(dup_focal) > 0) {
    cli::cli_abort(c(
      "Each joined specification must model a distinct focal layer.",
      "x" = "Layer{?s} {.val {dup_focal}} {?is/are} modeled by more than one
             specification.",
      "i" = "A layer's flavors must all be carried by a single specification;
             read a layer as a covariate to couple processes."
    ))
  }

  # At least one panel-observed layer must be referenced -- as a focal layer or
  # as an exogenous covariate. Otherwise the processes are exactly separable.
  panel_layers <- names(which(shared_data$info$observation == "panel"))
  referenced <- unique(unlist(lapply(specs, spec_referenced_layers)))
  if (length(intersect(referenced, panel_layers)) == 0) {
    cli::cli_abort(c(
      "A joint specification must reference a panel-observed layer.",
      "x" = "None of the composed formulas read a {.val panel} layer, so the
             processes are exactly separable.",
      "i" = "Estimate each specification on its own with {.fn estimate_dynam} or
             {.fn estimate_rem}."
    ))
  }

  # Modeled panel layers: panel-observed layers that are themselves a focal
  # process of the join. Reading one of these couples a fid; reading a panel
  # layer that only appears as an exogenous covariate does not (D4).
  modeled_panel <- intersect(focals, panel_layers)

  process_map <- build_joint_process_map(specs, modeled_panel)

  structure(
    list(
      specifications = specs,
      process_map = process_map,
      data = shared_data,
      call = match.call()
    ),
    class = "joint_specification.goldfish"
  )
}

# Normalize a specification into a flat list of its dependent processes, each a
# `list(flavor, submodels, constraint)`. A plain (or single-flavor) spec is one
# process; a multi-flavor spec is its per-flavor `processes`.
spec_processes <- function(spec) {
  if (!is.null(spec$processes)) {
    return(lapply(names(spec$processes), function(fl) {
      list(
        flavor = fl,
        submodels = spec$processes[[fl]]$submodels,
        constraint = spec$processes[[fl]]$constraint
      )
    }))
  }
  fl <- if (length(spec$modeled_flavor) == 1) {
    spec$modeled_flavor
  } else {
    NA_character_
  }
  list(list(
    flavor = fl,
    submodels = spec$submodels,
    constraint = spec$constraint
  ))
}

# Every layer a specification references: its focal (dependent) layer plus every
# network object read by an effect or a support-constraint atom. Panel-reference
# detection intersects this with the panel-observed layers, so only whether a
# name is a panel layer matters -- attribute references (not layers) drop out.
spec_referenced_layers <- function(spec) {
  refs <- spec$focal
  for (proc in spec_processes(spec)) {
    for (bundle in proc$submodels) {
      refs <- c(refs, bundle_object_names(bundle))
    }
    refs <- c(refs, constraint_object_names(proc$constraint))
  }
  unique(refs)
}

# Network object names read by a parsed submodel bundle's effects. Windowed
# effects reference a derived network (e.g. `friendship_5`); map those back to
# their source layer so a windowed panel covariate still counts as a reference.
bundle_object_names <- function(bundle) {
  parsed <- bundle$parsed
  if (length(parsed$rhs_names) == 0) {
    return(character(0))
  }
  tbl <- get_data_objects(parsed$rhs_names)
  objs <- tbl$object[!is.na(tbl$object)]
  if (length(parsed$window_derivations) > 0) {
    dmap <- stats::setNames(
      vapply(parsed$window_derivations, `[[`, character(1), "source_name"),
      vapply(parsed$window_derivations, `[[`, character(1), "derived_name")
    )
    hit <- objs %in% names(dmap)
    objs[hit] <- unname(dmap[objs[hit]])
  }
  unique(objs)
}

# Network object names read by a support-constraint plan's atoms. Constraint
# atoms carry no windows (a windowed atom is rejected at parse), so the base
# layer name is read directly.
constraint_object_names <- function(plan) {
  if (is.null(plan) || length(plan$atoms) == 0) {
    return(character(0))
  }
  tbl <- get_data_objects(get_rhs_names(atoms_to_formula(plan$atoms)))
  unique(tbl$object[!is.na(tbl$object)])
}

# Assemble the extended `process_map`: one integer-fid row per
# likelihood-producing formula across all composed processes, iterated
# specification-major then process (flavor)-major then family, so a process's
# rate and choice rows are contiguous. `constraint_id` is shared per
# `(layer, flavor)` -- the rate and choice of one process read one constraint
# plan, so they dedup to one id; distinct constraints get distinct ids. The
# `coupled` column marks a fid whose effects or constraint atoms read a modeled
# panel layer's latent state (`modeled_panel`); direct reference only, no
# transitivity.
build_joint_process_map <- function(specs, modeled_panel = character(0)) {
  procs <- list()
  for (spec in specs) {
    for (proc in spec_processes(spec)) {
      proc$model <- spec$model
      proc$layer <- spec$focal
      procs[[length(procs) + 1L]] <- proc
    }
  }

  constraint_ids <- dedup_constraint_ids(lapply(procs, `[[`, "constraint"))

  rows <- list()
  fid <- 0L
  for (i in seq_along(procs)) {
    proc <- procs[[i]]
    for (family in names(proc$submodels)) {
      bundle <- proc$submodels[[family]]
      fid <- fid + 1L
      refs <- unique(c(
        bundle_object_names(bundle),
        constraint_object_names(proc$constraint)
      ))
      rows[[length(rows) + 1L]] <- data.frame(
        fid = fid,
        layer = proc$layer,
        flavor = proc$flavor,
        family = family,
        # The statistic block a consumer's gids scope to: an effect is
        # deduplicated only among formulas resolving to the same update function.
        stat_block = paste(proc$model, bundle$sub_model, sep = ":"),
        has_intercept = bundle$has_intercept,
        constraint_id = constraint_ids[i],
        coupled = any(refs %in% modeled_panel),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# Assign a shared integer id to each distinct constraint plan (by value
# identity), `NA` for a process with no constraint. Two processes with identical
# parsed plans share an id; a process's rate and choice share the same plan
# object, so they share its id.
dedup_constraint_ids <- function(plans) {
  ids <- rep(NA_integer_, length(plans))
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
    ids[i] <- hit
  }
  ids
}
