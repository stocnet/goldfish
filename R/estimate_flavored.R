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
# preprocessed object as `preprocessing_init`: the object's formula, statistics
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
  control_estimation = set_estimation_opt(),
  control_preprocessing = set_preprocessing_opt(),
  preprocessing_init = NULL,
  preprocessing_only = FALSE,
  progress = FALSE,
  verbose = FALSE,
  call = NULL
) {
  if (!is.null(preprocessing_init)) {
    cli::cli_abort(c(
      "{.arg preprocessing_init} is not supported for a multi-flavor
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

  preps <- preprocess_flavored(
    spec,
    control_preprocessing = control_preprocessing,
    progress = progress,
    verbose = verbose
  )
  if (preprocessing_only) {
    return(preps)
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
      control_estimation = control_estimation,
      control_preprocessing = control_preprocessing,
      preprocessing_init = prep,
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
