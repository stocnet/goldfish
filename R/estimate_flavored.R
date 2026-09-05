# =========================================================================== #
# Per-flavor estimation over the fid-indexed preprocessed list.
#
# The competing-flavor likelihood factorizes: with fully observed time-stamped
# events, other flavors' events enter a flavor's likelihood only as exogenous
# state changes and right-censoring boundaries, so the Hessian is block-diagonal
# and estimating each flavor separately is exact rather than an approximation.
# This loop is therefore a wrapper over what a user could run as K separate
# estimations with the equivalent derived constraint supplied by hand -- the
# value it adds is the single preprocessing pass and one assembled result, not a
# different estimator.
#
# Each process is estimated through the ordinary estimation path with its own
# preprocessed object as `preprocessed`: the object's formula, statistics
# columns, folded availability and mask are already its own, so the engines,
# offset handling, and printing metadata all apply unchanged.
# =========================================================================== #

# Estimate every process of a multi-flavor specification.
#
# `sub_model` is deliberately not consulted: each process carries its own
# sub-model on its specification bundle, exactly as the single-flavor path uses
# `bundle$sub_model` rather than the caller's. A specification defining both
# families is one multi-process model, so all of it is estimated in one call.
estimate_flavored <- function(
  spec,
  model,
  data = NULL,
  control_algo = set_algorithm_newton(),
  control_prep = set_preprocessing(),
  preprocessed = NULL,
  preprocessing_only = FALSE,
  return_preprocessed = FALSE,
  progress = FALSE,
  verbose = FALSE,
  call = NULL,
  output = "default",
  max_length = 63L
) {
  if (!is.null(preprocessed)) {
    cli::cli_abort(c(
      "{.arg preprocessed} is not supported for a multi-flavor
       specification.",
      "i" = "Its processes are preprocessed together in one pass; pass the
             specification itself and let estimation drive that pass."
    ))
  }
  if (!is.null(data) && !identical(data, spec$data)) {
    cli::cli_abort(c(
      "{.arg data} cannot override the data of a multi-flavor specification.",
      "i" = "The flavor keys and derived constraints were resolved against the
             specification's own data; rebuild it with
             {.fn make_specification} against the new data instead."
    ))
  }
  # Coefficient values cannot travel through the shared control object: its one
  # `offset_coef` would have to serve every process, and the processes have
  # different coefficient vectors -- the same term can appear in all of them,
  # on the log-rate and the log-odds scale at once. A value written in a
  # formula belongs to that formula, which is the only object a process owns.
  if (!is.null(control_algo$offset_coef)) {
    cli::cli_abort(c(
      "{.arg offset_coef} does not apply to a multi-process specification.",
      "i" = "Write the value in the process formula it belongs to:
             {.code offset(term, coef = value)}."
    ))
  }
  if (!is.null(control_algo$fixed_parameters)) {
    cli::cli_abort(c(
      "{.arg fixed_parameters} does not apply to a multi-process
       specification.",
      "i" = "Coefficient positions differ per process; write the value in the
             process formula instead: {.code offset(term, coef = value)}."
    ))
  }
  check_flavored_initials_form(control_algo$initial_parameters)

  # The flavored pass preprocesses with the default writer and renders per fid
  # afterwards, so an unwritable db target would otherwise surface only once the
  # whole pass had run.
  if (preprocessing_only && identical(output, "db")) {
    validate_db_target(control_prep$db, control_prep$db_table)
  }

  preps <- preprocess_flavored(
    spec,
    control_prep = control_prep,
    progress = progress,
    verbose = verbose
  )
  if (preprocessing_only) {
    return(flavored_statistics_output(
      preps,
      spec = spec,
      model = model,
      control_prep = control_prep,
      output = output,
      max_length = max_length,
      progress = progress,
      verbose = verbose
    ))
  }
  process_map <- attr(preps, "process_map")
  initials <- resolve_flavored_initials(
    control_algo$initial_parameters,
    process_map
  )

  results <- lapply(seq_len(nrow(process_map)), function(i) {
    flavor <- process_map$flavor[i]
    family <- process_map$family[i]
    key <- as.character(process_map$fid[i])
    prep <- preps[[key]]
    if (progress) {
      cli::cli_inform(
        "Estimating {.field {render_process_label(
        process_map, process_map$fid[i])}}."
      )
    }
    process_control <- control_algo
    process_control$initial_parameters <- initials$values[[key]]
    process_control$initial_broadcast <- initials$broadcast[[key]]
    estimate_wrapper(
      x = prep$formula,
      model = model,
      sub_model = spec$processes[[flavor]]$submodels[[family]]$sub_model,
      data = spec$data,
      control_algo = process_control,
      control_prep = control_prep,
      preprocessed = prep,
      return_preprocessed = return_preprocessed,
      support_constraint = spec$processes[[flavor]]$constraint,
      recorded_flavor = flavor,
      progress = progress,
      verbose = verbose
    )
  })
  names(results) <- as.character(process_map$fid)
  check_broadcast_reached(initials$checks, results)

  structure(
    list(
      results = results,
      process_map = process_map,
      model = model,
      layer = spec$layer,
      flavors = names(spec$processes),
      call = call %||% spec$call
    ),
    class = "goldfishFlavFit"
  )
}

# Render the flavored preprocessing pass into the requested statistics product.
# The identity convention is the same at every output form: a list keyed by
# integer fid carrying the `process_map` table, never keys parsed back into
# meaning. Each fid is rendered by re-entering the shared wrapper with its own
# preprocessed object and its own formula, which is exactly what a single-flavor
# run of that process does -- so the per-fid stack cannot drift from the
# single-flavor one, and each flavor's own effect names come from its own parse
# rather than from the union formula that drove the shared walk.
flavored_statistics_output <- function(
  preps,
  spec,
  model,
  control_prep,
  output,
  max_length,
  progress,
  verbose
) {
  if (identical(output, "default")) {
    return(preps)
  }
  process_map <- attr(preps, "process_map")
  is_db <- identical(output, "db")
  # A db export renders each process as a gather stack and persists it before
  # the next one is rendered, so the run holds one stack at a time -- the reason
  # the db route exists. The per-process table, the map and the node table are
  # written by the shared export helpers, in the same schema a single-process
  # export writes.
  render_output <- if (is_db) "gather" else output
  outputs <- lapply(seq_len(nrow(process_map)), function(i) {
    flavor <- process_map$flavor[i]
    family <- process_map$family[i]
    fid <- process_map$fid[i]
    prep <- preps[[as.character(fid)]]
    rendered <- estimate_wrapper(
      x = prep$formula,
      model = model,
      sub_model = spec$processes[[flavor]]$submodels[[family]]$sub_model,
      data = spec$data,
      control_prep = control_prep,
      preprocessed = prep,
      preprocessing_only = TRUE,
      output = render_output,
      max_length = max_length,
      support_constraint = spec$processes[[flavor]]$constraint,
      progress = progress,
      verbose = verbose
    )
    if (!is_db) {
      return(rendered)
    }
    write_gather_to_db(
      rendered,
      control_prep$db,
      control_prep$db_table,
      fid = fid
    )
  })
  names(outputs) <- as.character(process_map$fid)
  if (is_db) {
    outputs <- finish_db_export(
      outputs,
      control_prep$db,
      control_prep$db_table,
      process_map
    )
  }

  structure(
    outputs,
    process_map = process_map,
    class = "goldfishFlavStat"
  )
}

# Starting values for a multi-process specification, before the process map is
# known. Positions are meaningless across processes -- each parses its own
# formula into its own coefficient vector -- so only the by-name forms are
# accepted here.
check_flavored_initials_form <- function(x) {
  if (is.null(x) || is.list(x)) {
    return(invisible(NULL))
  }
  if (is.null(names(x)) || !all(nzchar(names(x)))) {
    cli::cli_abort(c(
      "An unnamed {.arg initial_parameters} does not apply to a multi-process
       specification.",
      "x" = "Its processes have their own coefficient vectors, so a position
             does not identify a coefficient.",
      "i" = "Name the values, e.g. {.code c(inertia = 0.5)}, or key them by
             process: {.code list(creation = list(rate = c(inertia = 0.5)))}."
    ))
  }
  invisible(NULL)
}

# Distribute starting values over the processes. A flat named vector
# broadcasts: each process seeds the labels it carries, which is sound because
# a starting value moves the optimization path and never the optimum. A list
# keyed by flavor, optionally by family within it, targets processes instead --
# a family key names exactly one process, so its names are matched strictly,
# while a flavor key covers that flavor's families, whose coefficient vectors
# differ, so those broadcast within the flavor.
#
# Returns, per process: the values to seed with, whether the names may go
# unmatched there, and the groups whose names must reach some process.
resolve_flavored_initials <- function(initial_parameters, process_map) {
  keys <- as.character(process_map$fid)
  empty <- stats::setNames(vector("list", length(keys)), keys)
  none <- list(
    values = empty,
    broadcast = stats::setNames(as.list(rep(FALSE, length(keys))), keys),
    checks = list()
  )
  if (is.null(initial_parameters)) {
    return(none)
  }

  out <- none
  if (!is.list(initial_parameters)) {
    for (key in keys) {
      out$values[[key]] <- initial_parameters
      out$broadcast[[key]] <- TRUE
    }
    out$checks <- list(list(names = names(initial_parameters), fids = keys))
    return(out)
  }

  flavors <- unique(process_map$flavor)
  unknown <- setdiff(names(initial_parameters), flavors)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "{cli::qty(length(unknown))}{.arg initial_parameters} names
       {?a flavor/flavors} this specification does not model.",
      "x" = "Unknown: {.code {unknown}}.",
      "i" = "{cli::qty(length(flavors))}Modelled flavor{?s}: {.code {flavors}}."
    ))
  }
  for (flavor in names(initial_parameters)) {
    entry <- initial_parameters[[flavor]]
    flavor_keys <- keys[process_map$flavor == flavor]
    if (!is.list(entry)) {
      for (key in flavor_keys) {
        out$values[[key]] <- entry
        out$broadcast[[key]] <- TRUE
      }
      out$checks <- c(
        out$checks,
        list(list(names = names(entry), fids = flavor_keys))
      )
      next
    }
    families <- process_map$family[process_map$flavor == flavor]
    unknown_family <- setdiff(names(entry), families)
    if (length(unknown_family) > 0) {
      cli::cli_abort(c(
        "{cli::qty(length(unknown_family))}{.arg initial_parameters}${flavor}
         names {?a family/families} this flavor does not model.",
        "x" = "Unknown: {.code {unknown_family}}.",
        "i" = "{cli::qty(length(families))}Modelled famil{?y/ies}:
               {.code {families}}."
      ))
    }
    for (family in names(entry)) {
      key <- keys[process_map$flavor == flavor & process_map$family == family]
      out$values[[key]] <- entry[[family]]
    }
  }
  out
}

# A broadcast name is allowed to miss any given process, but not every one of
# them: a name that reached nothing is a typo, and silently seeding nowhere is
# what naming the values was meant to rule out. The coefficient labels come off
# the fits, which is the only place they exist -- so this reports after the
# processes are estimated rather than before.
check_broadcast_reached <- function(checks, results) {
  for (check in checks) {
    if (length(check$names) == 0) {
      next
    }
    available <- unique(unlist(lapply(
      results[check$fids],
      function(fit) term_label(fit$names, ".coef_name", "coef")
    )))
    unmatched <- setdiff(check$names, available)
    if (length(unmatched) > 0) {
      cli::cli_abort(c(
        "{cli::qty(length(unmatched))}{.arg initial_parameters} names
         {?a coefficient/coefficients} no process has.",
        "x" = "Unknown: {.code {unmatched}}.",
        "i" = "Available: {.code {available}}."
      ))
    }
  }
  invisible(NULL)
}
