#' Internal model specification classes
#'
#' Low-level constructors for the typed model specification objects that
#' carry the resolved model variant through preprocessing and estimation.
#' The class vector follows `c("<variant>_spec", "<indexing>_spec",
#' "model_spec")`. `sender_spec` variants are always one-mode: they force
#' `is_two_mode = FALSE` and use `nodes` for both modes.
#'
#' @param is_two_mode logical, whether sender and receiver node sets differ.
#' @param nodes,nodes2 names of the node sets of the dependent events.
#' @param ... additional fields stored in the spec object.
#'
#' @return an object of class `model_spec`.
#' @name model_spec
#' @noRd
model_spec_structure <- function(
  variant,
  indexing,
  model,
  sub_model,
  is_two_mode,
  nodes,
  nodes2,
  ...
) {
  structure(
    list(
      model = model,
      sub_model = sub_model,
      is_two_mode = is_two_mode,
      nodes = nodes,
      nodes2 = nodes2,
      ...
    ),
    class = c(variant, indexing, "model_spec")
  )
}

#' @rdname model_spec
#' @noRd
dynam_rate_spec <- function(nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "dynam_rate_spec",
    "sender_spec",
    "DyNAM",
    "rate",
    is_two_mode = FALSE,
    nodes = nodes,
    nodes2 = nodes2,
    ...
  )
}

#' @rdname model_spec
#' @noRd
dynam_rate_ordered_spec <- function(nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "dynam_rate_ordered_spec",
    "sender_spec",
    "DyNAM",
    "rate_ordered",
    is_two_mode = FALSE,
    nodes = nodes,
    nodes2 = nodes2,
    ...
  )
}

#' @rdname model_spec
#' @noRd
dynam_choice_spec <- function(
  is_two_mode = FALSE,
  nodes = NULL,
  nodes2 = nodes,
  ...
) {
  model_spec_structure(
    "dynam_choice_spec",
    "dyad_spec",
    "DyNAM",
    "choice",
    is_two_mode = is_two_mode,
    nodes = nodes,
    nodes2 = nodes2,
    ...
  )
}

#' @rdname model_spec
#' @noRd
dynam_choice_coord_spec <- function(
  is_two_mode = FALSE,
  nodes = NULL,
  nodes2 = nodes,
  ...
) {
  model_spec_structure(
    "dynam_choice_coord_spec",
    "dyad_spec",
    "DyNAM",
    "choice_coordination",
    is_two_mode = is_two_mode,
    nodes = nodes,
    nodes2 = nodes2,
    ...
  )
}

#' @rdname model_spec
#' @noRd
dynami_rate_spec <- function(nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "dynami_rate_spec",
    "sender_spec",
    "DyNAMi",
    "rate",
    is_two_mode = FALSE,
    nodes = nodes,
    nodes2 = nodes2,
    ...
  )
}

#' @rdname model_spec
#' @noRd
dynami_rate_ordered_spec <- function(nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "dynami_rate_ordered_spec",
    "sender_spec",
    "DyNAMi",
    "rate_ordered",
    is_two_mode = FALSE,
    nodes = nodes,
    nodes2 = nodes2,
    ...
  )
}

#' @rdname model_spec
#' @noRd
dynami_choice_spec <- function(
  is_two_mode = FALSE,
  nodes = NULL,
  nodes2 = nodes,
  ...
) {
  model_spec_structure(
    "dynami_choice_spec",
    "dyad_spec",
    "DyNAMi",
    "choice",
    is_two_mode = is_two_mode,
    nodes = nodes,
    nodes2 = nodes2,
    ...
  )
}

#' @rdname model_spec
#' @noRd
rem_rate_spec <- function(
  is_two_mode = FALSE,
  nodes = NULL,
  nodes2 = nodes,
  ...
) {
  model_spec_structure(
    "rem_rate_spec",
    "dyad_spec",
    "REM",
    "rate",
    is_two_mode = is_two_mode,
    nodes = nodes,
    nodes2 = nodes2,
    ...
  )
}

#' @rdname model_spec
#' @noRd
rem_rate_ordered_spec <- function(
  is_two_mode = FALSE,
  nodes = NULL,
  nodes2 = nodes,
  ...
) {
  model_spec_structure(
    "rem_rate_ordered_spec",
    "dyad_spec",
    "REM",
    "rate_ordered",
    is_two_mode = is_two_mode,
    nodes = nodes,
    nodes2 = nodes2,
    ...
  )
}

#' Construct a typed model specification
#'
#' Validates `(model, sub_model, is_two_mode, nodes, nodes2)` and returns
#' the corresponding `model_spec` object. The class of the returned object
#' is the resolved model variant, computed once; all downstream dispatch
#' uses it. Not exported.
#'
#' @inheritParams model_spec
#' @param model character, one of `"DyNAM"`, `"REM"`, `"DyNAMi"`.
#' @param sub_model character, a valid sub model for `model`.
#' @param engine character, the estimation algorithm variant. Only
#'   `"default"` (full-recompute) is currently implemented. `"incremental"`
#'   is reserved for the future `rem_rate_fast_spec` REM variant (design
#'   D14), which will reuse the shared preprocessing recipe and override
#'   `compute_step()` with cached partial sums.
#'
#' @return an object of class `model_spec`.
#' @noRd
new_model_spec <- function(
  model,
  sub_model,
  is_two_mode = FALSE,
  nodes = NULL,
  nodes2 = NULL,
  engine = "default",
  ...
) {
  stopifnot(
    rlang::is_string(model),
    rlang::is_string(sub_model),
    rlang::is_scalar_logical(is_two_mode),
    rlang::is_string(engine)
  )
  if (!identical(engine, "default")) {
    cli::cli_abort(c(
      "{.arg engine} {.val {engine}} is not yet supported.",
      "i" = "Only {.val default} is currently available.",
      "i" = "{.val incremental} is reserved for the future
       {.cls rem_rate_fast_spec} REM estimation variant."
    ))
  }
  constructors <- list(
    DyNAM = list(
      rate = dynam_rate_spec,
      rate_ordered = dynam_rate_ordered_spec,
      choice = dynam_choice_spec,
      choice_coordination = dynam_choice_coord_spec
    ),
    DyNAMi = list(
      rate = dynami_rate_spec,
      rate_ordered = dynami_rate_ordered_spec,
      choice = dynami_choice_spec
    ),
    REM = list(
      rate = rem_rate_spec,
      rate_ordered = rem_rate_ordered_spec
    )
  )
  if (!model %in% names(constructors)) {
    cli::cli_abort(c(
      "{.arg model} must be one of {.val {names(constructors)}}.",
      "x" = "{.val {model}} is not a valid model."
    ))
  }
  if (!sub_model %in% names(constructors[[model]])) {
    cli::cli_abort(c(
      "{.arg sub_model} for model {.val {model}} must be one of
       {.val {names(constructors[[model]])}}.",
      "x" = "{.val {sub_model}} is not a valid sub model."
    ))
  }
  constructor <- constructors[[model]][[sub_model]]
  is_sender <- sub_model %in% c("rate", "rate_ordered") && model != "REM"
  if (is_sender) {
    return(constructor(nodes = nodes, ...))
  }
  if (is_two_mode) {
    if (is.null(nodes) || is.null(nodes2)) {
      cli::cli_abort(c(
        "Two-mode models require both node sets.",
        "x" = "{.arg {c('nodes', 'nodes2')[c(is.null(nodes),
         is.null(nodes2))]}} {?is/are} NULL.",
        "i" = "Supply {.arg nodes} and {.arg nodes2} when
         {.code is_two_mode = TRUE}."
      ))
    }
    if (identical(nodes, nodes2)) {
      cli::cli_abort(c(
        "Two-mode models require distinct node sets.",
        "x" = "{.arg nodes} and {.arg nodes2} are identical.",
        "i" = "Use {.code is_two_mode = FALSE} for one-mode models."
      ))
    }
  }
  constructor(
    is_two_mode = is_two_mode,
    nodes = nodes,
    nodes2 = if (is.null(nodes2)) nodes else nodes2,
    ...
  )
}

#' Legacy model type string from a spec class
#'
#' Maps the spec class to the internal model type string still consumed by
#' the estimation routines. To be removed when `estimate_int()` dispatches
#' on the spec class.
#'
#' @param spec a `model_spec` object.
#'
#' @return a character scalar.
#' @noRd
legacy_model_type <- function(spec) {
  switch(
    class(spec)[1],
    dynam_rate_spec = ,
    dynami_rate_spec = "DyNAM-M-Rate",
    dynam_rate_ordered_spec = ,
    dynami_rate_ordered_spec = "DyNAM-M-Rate-ordered",
    dynam_choice_spec = ,
    dynami_choice_spec = "DyNAM-M",
    dynam_choice_coord_spec = "DyNAM-MM",
    rem_rate_spec = "REM",
    rem_rate_ordered_spec = "REM-ordered",
    cli::cli_abort(
      "No legacy model type for class {.cls {class(spec)[1]}}."
    )
  )
}
