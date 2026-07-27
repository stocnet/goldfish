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

  results <- lapply(seq_len(nrow(process_map)), function(i) {
    flavor <- process_map$flavor[i]
    family <- process_map$family[i]
    prep <- preps[[as.character(process_map$fid[i])]]
    if (progress) {
      cli::cli_inform(
        "Estimating {.field {render_process_label(
        process_map, process_map$fid[i])}}."
      )
    }
    estimate_wrapper(
      x = prep$formula,
      model = model,
      sub_model = spec$processes[[flavor]]$submodels[[family]]$sub_model,
      data = spec$data,
      control_algo = control_algo,
      control_prep = control_prep,
      preprocessed = prep,
      support_constraint = spec$processes[[flavor]]$constraint,
      progress = progress,
      verbose = verbose
    )
  })
  names(results) <- as.character(process_map$fid)

  structure(
    list(
      results = results,
      process_map = process_map,
      model = model,
      layer = spec$layer,
      flavors = names(spec$processes),
      call = call %||% spec$call
    ),
    class = "flavored_result.goldfish"
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
    class = "flavored_statistics.goldfish"
  )
}
