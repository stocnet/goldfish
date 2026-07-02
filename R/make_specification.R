#' Build a model specification object
#'
#' `r lifecycle::badge("experimental")`
#'
#' `make_specification()` bundles the rate and/or choice formulas of a model
#' together with its `model`, sub-model(s), the dependent process (named by
#' `layer`), an optional `support_constraint`, and the `data` object, returning a
#' reusable `specification.goldfish` object. The object holds the parsed formula
#' bundles that preprocessing consumes, so it can be passed directly to
#' [estimate_dynam()] / [estimate_rem()] in place of a formula.
#'
#' Unlike the formula interface (`estimate_dynam(dep ~ ...)`), the rate and
#' choice formulas leave the **left-hand side empty**; the dependent process is
#' named by `layer`. In the current data-object era `layer` is the name of a
#' [make_dependent_events()] object in `data`; once the single data object lands
#' the same string will resolve to a relation/layer name without any change to
#' the call surface.
#'
#' @param rate a one-sided formula (empty left-hand side) with the rate-model
#'   effects, or `NULL`.
#' @param choice a one-sided formula (empty left-hand side) with the
#'   choice-model effects, or `NULL`. Not applicable to `model = "REM"`.
#' @param model a character string, either `"DyNAM"` or `"REM"`. DyNAM-i is
#'   deferred to its own preprocessing-path change.
#' @param rate_sub_model a character string, the rate sub-model:
#'   `"rate"` or `"rate_ordered"`.
#' @param choice_sub_model a character string, the choice sub-model:
#'   `"choice"` or `"choice_coordination"`.
#' @param layer a character string naming the dependent process. It MUST resolve
#'   in `data` to a [make_dependent_events()] object.
#' @param support_constraint a one-sided formula constraining the risk set. It is
#'   parsed and stored but does not yet alter the risk set (that engine is
#'   delivered by a separate change); `NULL` by default.
#' @param data a `data.goldfish` object created with [make_data()].
#'
#' @return an S3 object of class `specification.goldfish`.
#'
#' @seealso [estimate_dynam()], [estimate_rem()], [make_dependent_events()],
#'   [make_data()]
#' @export
#' @examples
#' data("Social_Evolution")
#' callNetwork <- make_network(nodes = actors, directed = TRUE)
#' callNetwork <- link_events(
#'   x = callNetwork, change_event = calls, nodes = actors
#' )
#' callsDependent <- make_dependent_events(
#'   events = calls, nodes = actors, default_network = callNetwork
#' )
#' \dontshow{
#' callsDependent <- callsDependent[1:50, ]
#' }
#' socialEvData <- make_data(callsDependent, callNetwork, calls, actors)
#'
#' spec <- make_specification(
#'   choice = ~ inertia + recip + trans,
#'   model = "DyNAM", choice_sub_model = "choice",
#'   layer = "callsDependent", data = socialEvData
#' )
#' spec
make_specification <- function(
  rate = NULL,
  choice = NULL,
  model = c("DyNAM", "REM"),
  rate_sub_model = c("rate", "rate_ordered"),
  choice_sub_model = c("choice", "choice_coordination"),
  layer = NULL,
  support_constraint = NULL,
  data = NULL
) {
  model <- match.arg(model)
  rate_sub_model <- match.arg(rate_sub_model)
  choice_sub_model <- match.arg(choice_sub_model)

  if (!inherits(data, "data.goldfish")) {
    cli::cli_abort(c(
      "{.arg data} must be a {.cls data.goldfish} object.",
      "i" = "Create it with {.fn make_data}."
    ))
  }
  if (!rlang::is_string(layer)) {
    cli::cli_abort(
      "{.arg layer} must be a single character string naming the dependent
       process."
    )
  }
  if (is.null(rate) && is.null(choice)) {
    cli::cli_abort(
      "At least one of {.arg rate} or {.arg choice} must be supplied."
    )
  }
  if (model == "REM" && !is.null(choice)) {
    cli::cli_abort(c(
      "{.arg choice} is not applicable to {.val REM} models.",
      "i" = "REM is tie-oriented; supply the effects through {.arg rate}."
    ))
  }

  # Parse against a working clone so the specification carries no side effects
  # on the caller's data (design D8: the parse is pure on the DyNAM/REM path).
  work_env <- rlang::env_clone(data)

  # Resolve `layer` to a dependent-events object (bridge-era lookup; task 4.2).
  dep_obj <- tryCatch(get(layer, envir = work_env), error = function(e) NULL)
  if (!inherits(dep_obj, "dependent.goldfish")) {
    cli::cli_abort(c(
      "{.arg layer} = {.val {layer}} does not name a dependent-events object
       in {.arg data}.",
      "i" = "Create the dependent process with {.fn make_dependent_events} and
             pass its name to {.arg layer}."
    ))
  }

  submodels <- list()
  if (!is.null(rate)) {
    submodels$rate <- build_specification_bundle(
      rate,
      arg = "rate",
      model = model,
      sub_model = rate_sub_model,
      layer = layer,
      envir = work_env
    )
  }
  if (!is.null(choice)) {
    submodels$choice <- build_specification_bundle(
      choice,
      arg = "choice",
      model = model,
      sub_model = choice_sub_model,
      layer = layer,
      envir = work_env
    )
  }

  if (!is.null(support_constraint)) {
    if (!inherits(support_constraint, "formula")) {
      cli::cli_abort(
        "{.arg support_constraint} must be a formula or {.val NULL}."
      )
    }
  }

  structure(
    list(
      model = model,
      submodels = submodels,
      layer = layer,
      dependent = spec_dependent_info(dep_obj, layer),
      support_constraint = support_constraint,
      valid = TRUE,
      data = data,
      call = match.call()
    ),
    class = "specification.goldfish"
  )
}

# Parse a one-sided submodel formula into the bundle preprocessing consumes,
# enforcing the empty-LHS contract (task 4.2) and the D3 validity matrix
# (task 4.1). Windows are recorded but not realized (recipe path, design D8), so
# the parse is pure; realization happens later at state creation.
build_specification_bundle <- function(
  one_sided,
  arg,
  model,
  sub_model,
  layer,
  envir
) {
  enforce_empty_lhs(one_sided, arg = arg, layer = layer, envir = envir)
  full_formula <- build_layer_formula(one_sided, layer)

  parsed <- parse_formula(full_formula, envir = envir, realize_windows = FALSE)

  # Interaction products compute in the dyad recipe loop (design D9); guard the
  # not-yet-supported model families (sender / DyNAMi).
  abort_if_interactions_unsupported(parsed, model, sub_model)

  # Effective sub_model mirrors estimate_wrapper: a rate formula without the time
  # intercept is the ordinal case; choice / choice_coordination and rate_ordered
  # ignore the intercept entirely.
  has_intercept <- parsed$has_intercept
  if (
    (sub_model %in% c("choice", "choice_coordination")) ||
      sub_model == "rate_ordered"
  ) {
    has_intercept <- FALSE
  }
  validity_sub_model <- sub_model
  if (sub_model == "rate" && !has_intercept) {
    validity_sub_model <- "rate_ordered"
  }

  # A specification is built to be estimated, so validate at estimation strength
  # (rejects unidentified bare main effects such as ego/global in choice).
  # Offset (fixed-coefficient) terms are not estimated main effects (design D7),
  # so exclude them from the check.
  is_offset <- unlist(parsed$offset_parameter)
  if (is.null(is_offset)) {
    is_offset <- logical(length(parsed$rhs_names))
  }
  main_effect <- !is_offset
  validate_effects(
    model,
    validity_sub_model,
    vapply(parsed$rhs_names[main_effect], "[[", character(1), 1),
    vapply(parsed$type_parameter[main_effect], as.character, character(1)),
    estimating = TRUE
  )

  list(
    input_formula = one_sided,
    formula = full_formula,
    sub_model = sub_model,
    parsed = parsed,
    has_intercept = has_intercept
  )
}

# Enforce the empty-LHS contract; a dependent-events object on the LHS gets a
# targeted message pointing at `layer` (task 4.2).
enforce_empty_lhs <- function(f, arg, layer, envir) {
  if (!inherits(f, "formula")) {
    cli::cli_abort("{.arg {arg}} must be a one-sided formula.")
  }
  if (length(f) == 3L) {
    lhs_name <- deparse(f[[2]])
    is_dep <- tryCatch(
      inherits(get(lhs_name, envir = envir), "dependent.goldfish"),
      error = function(e) FALSE
    )
    if (is_dep) {
      cli::cli_abort(c(
        "The {.arg {arg}} formula must have an empty left-hand side.",
        "x" = "Found the dependent-events object {.val {lhs_name}} on the
               left-hand side.",
        "i" = "Name the dependent process with {.arg layer} instead, e.g.
               {.code layer = \"{lhs_name}\"}."
      ))
    }
    cli::cli_abort(c(
      "The {.arg {arg}} formula must have an empty left-hand side.",
      "i" = "The dependent process is named by {.arg layer}
             (currently {.val {layer}})."
    ))
  }
  invisible(f)
}

# Build the two-sided `layer ~ rhs` formula parse_formula() expects from the
# user's one-sided formula.
build_layer_formula <- function(one_sided, layer) {
  rhs <- one_sided[[length(one_sided)]]
  stats::as.formula(
    call("~", as.name(layer), rhs),
    env = environment(one_sided)
  )
}

# Dependent-process facts for the print overview (task 4.3), read from the
# resolved dependent-events object.
spec_dependent_info <- function(dep_obj, layer) {
  nodes <- attr(dep_obj, "nodes")
  is_two_mode <- length(nodes) == 2
  time_span <- if (!is.null(dep_obj$time) && nrow(dep_obj) > 0) {
    range(dep_obj$time)
  } else {
    NULL
  }
  list(
    layer = layer,
    n_events = nrow(dep_obj),
    time_span = time_span,
    nodes = nodes[1],
    nodes2 = if (is_two_mode) nodes[2] else nodes[1],
    is_two_mode = is_two_mode,
    network = attr(dep_obj, "default_network")
  )
}
