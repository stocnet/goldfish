#' Internal model specification classes
#'
#' Low-level constructors for the typed model specification objects that
#' carry the resolved model variant through preprocessing and estimation.
#' The class vector follows `c("goldfishKind<Variant>", "goldfishAxis<Axis>",
#' "goldfishKind")`. `goldfishAxisSender` variants are sender-indexed but may
#' be two-mode: the receiver side (`nodes2`) and `is_two_mode` are carried so a
#' rate model over an n1 x n2 network sizes its statistics on both modes.
#'
#' @param is_two_mode logical, whether sender and receiver node sets differ.
#' @param nodes,nodes2 names of the node sets of the dependent events.
#' @param ... additional fields stored in the spec object.
#'
#' @return an object of class `goldfishKind`.
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
    class = c(variant, indexing, "goldfishKind")
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
#'   \item{`normalizer`}{the likelihood normalizer, from `sub_model`:
#'     `"poisson"` (rate — timespan-weighted waiting times), `"multinomial"`
#'     (choice / ordinal rate — the softmax over the risk set), or
#'     `"coordination"` (the mutual `getLikelihoodMM` product). The compiled
#'     interface selects the timespan handling, the `compute_` kernel, and the
#'     intercept-init family from this, never from a model-type string.}
#' }
#' @noRd
risk_set_descriptor <- function(indexing, sub_model, is_two_mode) {
  # The likelihood normalizer follows the sub-model: `rate` is the
  # timespan-weighted Poisson, `choice_coordination` the mutual product, and
  # everything else (`choice`, ordinal `rate_ordered`) the multinomial softmax.
  normalizer <- switch(
    sub_model,
    rate = "poisson",
    choice_coordination = "coordination",
    "multinomial"
  )
  if (identical(indexing, "goldfishAxisSender")) {
    return(list(
      axis = "sender",
      fold_target = "active_sender",
      encoding = NA_character_,
      symmetrize = FALSE,
      normalizer = normalizer
    ))
  }
  if (identical(sub_model, "choice")) {
    return(list(
      axis = "receiver_given_sender",
      fold_target = "active_dyad",
      encoding = "alter",
      symmetrize = FALSE,
      normalizer = normalizer
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
      symmetrize = symmetrize,
      normalizer = normalizer
    ))
  }
  # REM rate / rate_ordered: the whole dyad matrix, both presences fold.
  list(
    axis = "dyad",
    fold_target = "active_dyad",
    encoding = "outer",
    symmetrize = FALSE,
    normalizer = normalizer
  )
}

#' Risk-set axis of a fitted model or a model specification
#'
#' The axis that gives meaning to a position in a per-event diagnostic component
#' — `event_probabilities`, `event_scores`, ranks, margins. Two fits over the
#' same node set can return per-event vectors of identical length that index
#' different things: on a DyNAM-rate model position `i` is a *sender*, on a
#' DyNAM-choice model it is a *receiver given the sender*. The axis is what
#' distinguishes them, so a consumer resolves an index without re-deriving the
#' model family from `model` / `sub_model`.
#'
#' Join a per-event index to the fit's `node_lookup` on the side the axis names:
#' side 1 for `"sender"`, side 2 for `"receiver_given_sender"` on a two-mode
#' model. A one-mode model draws both dyad axes from side 1, because its sender
#' and receiver sets are the same nodes, so its lookup carries side 1 only.
#'
#' @param x a fitted model of class `"goldfishFit"` (from
#'   [estimate_dynam()], [estimate_rem()]), a preprocessed object, or a model
#'   specification.
#'
#' @return A length-one character vector, one of:
#'   \describe{
#'     \item{`"sender"`}{rate models — the risk set is the sender set, so a
#'       per-event position is an actor who could have acted.}
#'     \item{`"receiver_given_sender"`}{choice models — one sender's receiver
#'       row, so a per-event position is a candidate receiver.}
#'     \item{`"dyad"`}{REM, REM-ordered and two-mode coordination — the full
#'       ordered dyad grid.}
#'     \item{`"dyad_symmetric"`}{one-mode coordination — the dyad matrix
#'       symmetrized for the mutual likelihood, so a position names an
#'       unordered pair.}
#'   }
#'   `NULL` for a fit produced before goldfish 2.0.0, which did not record it;
#'   consumers treat a missing value as an unknown axis, as they do for
#'   `backend`.
#'
#' @examples
#' data("social_evolution")
#' rate <- estimate_dynam(
#'   calls ~ 1 + indeg + outdeg,
#'   sub_model = "rate",
#'   data = social_evolution
#' )
#' choice <- estimate_dynam(
#'   calls ~ inertia + recip,
#'   sub_model = "choice",
#'   data = social_evolution
#' )
#' # Same node set, same per-event length, different meaning.
#' risk_set_axis(rate)
#' risk_set_axis(choice)
#'
#' @seealso [estimate_dynam()] for the fitted object's other components.
#' @export
risk_set_axis <- function(x) {
  # Three shapes carry the axis: a model spec has the `risk_set` descriptor, a
  # fit has the resolved `risk_set_axis` copied onto it at assembly, and a
  # preprocessed object has the spec it was built from. Specs come first because
  # every internal caller passes one.
  #
  # Matched by name with `[[`, never `$`: `$risk_set` on a FIT would partially
  # match its `risk_set_axis` component and return the axis string, which then
  # fails on `$axis`. Testing membership first also keeps a pre-2.0.0 fit
  # carrying none of the three on the NULL path rather than a subscript error.
  # A flavored container carries no specification of its own: it is K fits, one
  # per process, each with its own risk set. Returning NULL here would let a
  # caller read "no axis" as an answer about the model rather than as the
  # container being the wrong object to ask.
  if (inherits(x, "goldfishFlavFit")) {
    cli::cli_abort(c(
      "A flavored fit carries no single risk-set axis.",
      "x" = "It holds one fit per process, and each has its own.",
      "i" = "Ask a process: {.code x$results[[1]]}, or index by the flavor
             label the fit reports."
    ))
  }
  nms <- names(x)
  if ("risk_set" %in% nms) {
    return(x[["risk_set"]][["axis"]])
  }
  if ("risk_set_axis" %in% nms) {
    return(x[["risk_set_axis"]])
  }
  if ("model_spec" %in% nms) {
    return(x[["model_spec"]][["risk_set"]][["axis"]])
  }
  NULL
}

#' @noRd
risk_set_normalizer <- function(spec) spec$risk_set$normalizer

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

#' Engine capability for constrained (support_constraint) estimation
#'
#' The single table answering whether an engine consumes a `support_constraint`
#' for a given model family. Every wired recipe family folds its availability
#' during preprocessing and reads the folded buffers natively on all engines, so
#' the capability is engine-independent: one row per spec class. The DyNAMi
#' monolith and the ordinal DyNAM-rate path do not yet consume a folded
#' constraint. Keyed by `class(spec)[1]`; the value is a human-readable family
#' label for supported classes and `NA` for unsupported ones, so the abort
#' message enumerates the supported families from the table and wiring a new
#' family is a one-row edit.
#' @noRd
constrained_support_map <- function() {
  c(
    goldfishKindDnChoice = "DyNAM choice",
    goldfishKindDnCoord = "DyNAM choice_coordination",
    goldfishKindDnRate = "DyNAM rate",
    goldfishKindDnCox = NA_character_,
    goldfishKindRemRate = "REM rate",
    goldfishKindRemCox = "REM rate_ordered",
    goldfishKindDniRate = NA_character_,
    goldfishKindDniCox = NA_character_,
    goldfishKindDniChoice = NA_character_
  )
}

#' @noRd
constrained_estimation_supported <- function(spec) {
  !is.na(constrained_support_map()[class(spec)[1]])
}

#' Abort when a support_constraint reaches an unwired model family
#'
#' The message enumerates the supported families from the capability table so
#' it stays in sync with [constrained_support_map()].
#' @noRd
abort_constraint_unsupported <- function(spec, call = rlang::caller_env()) {
  supported <- unname(constrained_support_map())
  supported <- supported[!is.na(supported)]
  cli::cli_abort(
    c(
      "{.arg support_constraint} is not consumed for {.val {spec$model}}
       {.val {spec$sub_model}} estimation.",
      "i" = "Risk-set restriction is wired for {.val {supported}}.",
      "i" = "The preprocessed mask is available in {.code prep$support_mask}."
    ),
    call = call
  )
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
    "goldfishKindDnRate",
    "goldfishAxisSender",
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
    "goldfishKindDnCox",
    "goldfishAxisSender",
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
    "goldfishKindDnChoice",
    "goldfishAxisDyad",
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
    "goldfishKindDnCoord",
    "goldfishAxisDyad",
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
    "goldfishKindDniRate",
    "goldfishAxisSender",
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
    "goldfishKindDniCox",
    "goldfishAxisSender",
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
    "goldfishKindDniChoice",
    "goldfishAxisDyad",
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
    "goldfishKindRemRate",
    "goldfishAxisDyad",
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
    "goldfishKindRemCox",
    "goldfishAxisDyad",
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
#' the corresponding `goldfishKind` object. The class of the returned object
#' is the resolved model variant, computed once; all downstream dispatch
#' uses it. Not exported.
#'
#' @inheritParams model_spec
#' @param model character, one of `"DyNAM"`, `"REM"`, `"DyNAMi"`.
#' @param sub_model character, a valid sub model for `model`.
#'
#' @return an object of class `goldfishKind`.
#' @noRd
new_model_spec <- function(
  model,
  sub_model,
  is_two_mode = FALSE,
  nodes = NULL,
  nodes2 = NULL,
  ...
) {
  stopifnot(
    rlang::is_string(model),
    rlang::is_string(sub_model),
    rlang::is_scalar_logical(is_two_mode)
  )
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
