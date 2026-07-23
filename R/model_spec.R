#' Internal model specification classes
#'
#' Low-level constructors for the typed model specification objects that
#' carry the resolved model variant through preprocessing and estimation.
#' The class vector follows `c("<variant>_spec", "<indexing>_spec",
#' "model_spec")`. `sender_spec` variants are sender-indexed but may be
#' two-mode: the receiver side (`nodes2`) and `is_two_mode` are carried so a
#' rate model over an n1 x n2 network sizes its statistics on both modes.
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
      risk_set = risk_set_descriptor(indexing, sub_model, is_two_mode),
      ...
    ),
    class = c(variant, indexing, "model_spec")
  )
}

#' Risk-set dispatch descriptor
#'
#' The single parse-time decision point for the risk-set geometry a model
#' spec carries. Derived once, at construction, from the resolved
#' `(indexing, sub_model, is_two_mode)`; every downstream site (availability
#' encoding selection, fold-family selection, validation family, rate
#' detection, estimation guards) reads it through the accessors below and
#' none re-derives the family or geometry from model/sub_model strings or
#' from array dimensionality. Fields:
#' \describe{
#'   \item{`axis`}{the risk-set axis: `"sender"` (rate models, sender-indexed),
#'     `"receiver_given_sender"` (choice — one sender's receiver row),
#'     `"dyad"` (REM / REM-ordered / two-mode coordination — the full dyad
#'     matrix), or `"dyad_symmetric"` (one-mode coordination — the dyad matrix
#'     symmetrized for the mutual likelihood).}
#'   \item{`fold_target`}{the maintained availability object a support
#'     constraint folds into: `"active_sender"` for rate, `"active_dyad"`
#'     for every dyad-loop family.}
#'   \item{`encoding`}{the base `active_dyad` encoding when no constraint
#'     sharpens it: `"outer"` for the dyadic risk sets (both presences fold),
#'     `"alter"` for choice (receiver presence only), `NA` for rate.}
#'   \item{`symmetrize`}{`TRUE` only for one-mode coordination; the value the
#'     undirected-REM discussion will reuse.}
#' }
#' @noRd
risk_set_descriptor <- function(indexing, sub_model, is_two_mode) {
  if (identical(indexing, "sender_spec")) {
    return(list(
      axis = "sender",
      fold_target = "active_sender",
      encoding = NA_character_,
      symmetrize = FALSE
    ))
  }
  if (identical(sub_model, "choice")) {
    return(list(
      axis = "receiver_given_sender",
      fold_target = "active_dyad",
      encoding = "alter",
      symmetrize = FALSE
    ))
  }
  if (identical(sub_model, "choice_coordination")) {
    # One-mode coordination symmetrizes the dyad matrix for the mutual
    # likelihood; a (rejected-before-construction) two-mode coordination would
    # not. Deriving from is_two_mode keeps the value correct either way.
    symmetrize <- !isTRUE(is_two_mode)
    return(list(
      axis = if (symmetrize) "dyad_symmetric" else "dyad",
      fold_target = "active_dyad",
      encoding = "outer",
      symmetrize = symmetrize
    ))
  }
  # REM rate / rate_ordered: the whole dyad matrix, both presences fold.
  list(
    axis = "dyad",
    fold_target = "active_dyad",
    encoding = "outer",
    symmetrize = FALSE
  )
}

#' @noRd
risk_set_axis <- function(spec) spec$risk_set$axis

#' @noRd
risk_set_fold_target <- function(spec) spec$risk_set$fold_target

#' @noRd
risk_set_encoding <- function(spec) spec$risk_set$encoding

#' @noRd
risk_set_symmetrize <- function(spec) isTRUE(spec$risk_set$symmetrize)

#' Whether the risk set spans the full dyad matrix (both presences fold)
#' @noRd
risk_set_is_dyadic <- function(spec) {
  risk_set_axis(spec) %in% c("dyad", "dyad_symmetric")
}

#' @rdname model_spec
#' @noRd
dynam_rate_spec <- function(
  is_two_mode = FALSE,
  nodes = NULL,
  nodes2 = nodes,
  ...
) {
  model_spec_structure(
    "dynam_rate_spec",
    "sender_spec",
    "DyNAM",
    "rate",
    is_two_mode = is_two_mode,
    nodes = nodes,
    nodes2 = nodes2,
    ...
  )
}

#' @rdname model_spec
#' @noRd
dynam_rate_ordered_spec <- function(
  is_two_mode = FALSE,
  nodes = NULL,
  nodes2 = nodes,
  ...
) {
  model_spec_structure(
    "dynam_rate_ordered_spec",
    "sender_spec",
    "DyNAM",
    "rate_ordered",
    is_two_mode = is_two_mode,
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
dynami_rate_spec <- function(
  is_two_mode = FALSE,
  nodes = NULL,
  nodes2 = nodes,
  ...
) {
  model_spec_structure(
    "dynami_rate_spec",
    "sender_spec",
    "DyNAMi",
    "rate",
    is_two_mode = is_two_mode,
    nodes = nodes,
    nodes2 = nodes2,
    ...
  )
}

#' @rdname model_spec
#' @noRd
dynami_rate_ordered_spec <- function(
  is_two_mode = FALSE,
  nodes = NULL,
  nodes2 = nodes,
  ...
) {
  model_spec_structure(
    "dynami_rate_ordered_spec",
    "sender_spec",
    "DyNAMi",
    "rate_ordered",
    is_two_mode = is_two_mode,
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
#'   is reserved for the future `rem_rate_fast_spec` REM variant, which will
#'   reuse the shared preprocessing recipe and override
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
    # A rate model is sender-indexed, but a two-mode dependent process still
    # reads an n1 x n2 network, so the receiver side and the flag must reach the
    # spec (dropping them collapsed n2 to n1 and broke effect init). DyNAMi is
    # one-mode, so it always resolves is_two_mode = FALSE here.
    return(constructor(
      is_two_mode = is_two_mode,
      nodes = nodes,
      nodes2 = if (is.null(nodes2)) nodes else nodes2,
      ...
    ))
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
