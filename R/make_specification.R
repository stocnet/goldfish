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
#' named by `layer`. For a `stocnet` object `layer` names one of its layers and
#' defaults to `info$focal`.
#'
#' @details
#' # Modeling one flavor of a layer
#'
#' When a layer's ties carry a `flavor` column, `rate` / `choice` may be given
#' as a **flavor-keyed list** whose formula left-hand side is the flavor to
#' model, e.g. `rate = list(creation ~ 1 + indeg())`. Only rows of that flavor
#' are modeled as events; every other focal row -- including rows with no
#' flavor -- still updates the network state. Supplying a plain formula on a
#' flavored layer models **all** of its rows, and says so.
#'
#' Exactly one flavor can be modeled: several keys, or `rate` and `choice`
#' keying different flavors, are errors. Estimating several dependent processes
#' jointly needs stacked per-flavor likelihoods and arrives with a later change.
#'
#' @param rate a one-sided formula (empty left-hand side) with the rate-model
#'   effects, a flavor-keyed list of one such formula, or `NULL`.
#' @param choice a one-sided formula (empty left-hand side) with the
#'   choice-model effects, a flavor-keyed list of one such formula, or `NULL`.
#'   Not applicable to `model = "REM"`.
#' @param model a character string, either `"DyNAM"` or `"REM"`. DyNAM-i is
#'   deferred to its own preprocessing-path change.
#' @param rate_sub_model a character string, the rate sub-model:
#'   `"rate"` or `"rate_ordered"`.
#' @param choice_sub_model a character string, the choice sub-model:
#'   `"choice"` or `"choice_coordination"`.
#' @param layer a character string naming the dependent process: a layer of the
#'   `stocnet` object, defaulting to its `info$focal`.
#' @param support_constraint a one-sided formula restricting the per-event risk
#'   set, written in a restricted boolean-tree grammar. The leaves are effect
#'   *atoms* (`tie(net)`, `indeg(net)`, ...) combined into a logical expression
#'   with:
#'   * boolean operators `&`, `|`, `!`;
#'   * comparisons `> < >= <= == !=`, either effect-vs-constant
#'     (`indeg(net) > 2`) or effect-vs-effect (`indeg(net) > outdeg(net)`);
#'   * elementwise arithmetic `+ - * /` (with parentheses);
#'   * a bare effect, which means `effect != 0`.
#'
#'   A cell `(i, j)` is in the risk set when the expression evaluates `TRUE`
#'   there. It is parsed and validated at construction; the restriction is
#'   applied during estimation.
#'
#'   **`*` is elementwise arithmetic here, not interaction expansion.** In an
#'   *effects* formula `a * b` expands to `a + b + a:b` (estimated interaction
#'   columns); in a *constraint* `a * b` is the elementwise product of the two
#'   atoms' values, with no columns and no coefficients. For 0/1 indicators
#'   `a * b` coincides with `a & b`, and `&` is the clearer idiom — prefer
#'   `tie(a) & tie(b)` over `tie(a) * tie(b)`. `NULL` by default.
#' @param data a `stocnet` object, raw or stamped by [as_goldfish()]. It is
#'   validated here either way: a stamp records provenance, not validity.
#'
#' @return an S3 object of class `specification.goldfish`.
#'
#' @seealso [estimate_dynam()], [estimate_rem()], [make_dependent_events()],
#'   [make_data()]
#' @export
#' @examples
#' data("Social_Evolution")
#' call_network <- make_network(nodes = actors, directed = TRUE)
#' call_network <- link_events(
#'   x = call_network, change_event = calls, nodes = actors
#' )
#' calls_dependent <- make_dependent_events(
#'   events = calls, nodes = actors, default_network = call_network
#' )
#' \dontshow{
#' calls_dependent <- calls_dependent[1:50, ]
#' }
#' social_ev_data <- make_data(calls_dependent, call_network, calls, actors)
#'
#' spec <- make_specification(
#'   choice = ~ inertia + recip + trans,
#'   model = "DyNAM", choice_sub_model = "choice",
#'   layer = "calls_dependent", data = social_ev_data
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

  is_legacy <- is.environment(data)
  if (!is_legacy && !(is.list(data) && !is.data.frame(data))) {
    cli::cli_abort(c(
      "{.arg data} must be a {.cls stocnet} object.",
      "i" = "Build it with {.fn manynet::make_stocnet}, or gate it early with
             {.fn as_goldfish}."
    ))
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

  # A stamp is not evidence of validity -- manynet verbs and plain list
  # assignment mutate an object while preserving its class vector -- so the
  # stocnet input is validated here unconditionally, stamped or not.
  if (!is_legacy) {
    validate_goldfish_data(data, focal = layer)
  }

  # Parse against a working clone so the specification carries no side effects
  # on the caller's data (the parse is pure on the DyNAM/REM path). The stocnet
  # path resolves names from components instead, so its environment stays empty.
  work_env <- if (is_legacy) rlang::env_clone(data) else new.env()
  spec_data <- if (is_legacy) NULL else data

  layer <- resolve_specification_layer(data, layer, is_legacy, work_env)
  flavored <- resolve_modeled_flavor(rate, choice, spec_data, layer)

  submodels <- list()
  if (!is.null(rate)) {
    submodels$rate <- build_specification_bundle(
      flavored$rate,
      arg = "rate",
      model = model,
      sub_model = rate_sub_model,
      layer = layer,
      envir = work_env,
      data = spec_data
    )
  }
  if (!is.null(choice)) {
    submodels$choice <- build_specification_bundle(
      flavored$choice,
      arg = "choice",
      model = model,
      sub_model = choice_sub_model,
      layer = layer,
      envir = work_env,
      data = spec_data
    )
  }

  constraint_plan <- NULL
  if (!is.null(support_constraint)) {
    if (!inherits(support_constraint, "formula")) {
      cli::cli_abort(
        "{.arg support_constraint} must be a formula or {.val NULL}."
      )
    }
    # One constraint serves both submodels; dyadic atoms are legal when the spec
    # has a dyad-indexed part (a choice submodel, or REM), rejected for a
    # rate-only spec (sender-axis rule).
    has_dyad_part <- model == "REM" || !is.null(choice)
    constraint_plan <- parse_and_validate_constraint(
      support_constraint,
      has_dyad_part = has_dyad_part,
      envir = work_env
    )
  }

  structure(
    list(
      model = model,
      submodels = submodels,
      layer = layer,
      # The focal and the modeled flavor together select the dependent rows, so
      # they travel with the specification: state creation must resolve the same
      # dependent stream this parse did, not the one `info$focal` declares.
      focal = layer,
      modeled_flavor = flavored$flavor,
      dependent = if (is_legacy) {
        spec_dependent_info(get(layer, envir = work_env), layer)
      } else {
        stocnet_dependent_info(data, layer, flavored$flavor)
      },
      support_constraint = support_constraint,
      constraint = constraint_plan,
      valid = TRUE,
      data = data,
      call = match.call()
    ),
    class = "specification.goldfish"
  )
}

# Resolve the dependent process. A stocnet layer name falls back to
# `info$focal`; the legacy environment still names a dependent-events object,
# the only vocabulary it has until the constructors assemble stocnet.
resolve_specification_layer <- function(
  data,
  layer,
  is_legacy,
  work_env,
  call = rlang::caller_env()
) {
  if (is_legacy) {
    if (!rlang::is_string(layer)) {
      cli::cli_abort(
        "{.arg layer} must be a single character string naming the dependent
         process.",
        call = call
      )
    }
    dep_obj <- tryCatch(get(layer, envir = work_env), error = function(e) NULL)
    if (!inherits(dep_obj, "dependent.goldfish")) {
      cli::cli_abort(
        c(
          "{.arg layer} = {.val {layer}} does not name a dependent-events object
           in {.arg data}.",
          "i" = "Create the dependent process with {.fn make_dependent_events}
                 and pass its name to {.arg layer}."
        ),
        call = call
      )
    }
    return(layer)
  }
  # validate_goldfish_data() already checked that a supplied `layer` (or
  # `info$focal`) names a layer, so only their joint absence is left to report.
  resolved <- layer %||% data$info$focal
  if (is.null(resolved)) {
    cli::cli_abort(
      c(
        "The dependent process is not identified.",
        "i" = "Name it with {.arg layer}, or declare {.field info$focal} on the
               data."
      ),
      call = call
    )
  }
  resolved
}

# Resolve which focal rows a specification models.
#
# A flavor-keyed list (`rate = list(creation ~ ...)`) names the flavor to model;
# every other focal row -- including one with no flavor -- updates state but is
# not a modeled event. A plain formula models every row, which on a flavored
# layer is a real choice rather than an oversight, so it is announced.
#
# Returns the one-sided formulas to parse plus the modeled flavor (or NULL).
resolve_modeled_flavor <- function(
  rate,
  choice,
  data,
  layer,
  call = rlang::caller_env()
) {
  rate_keyed <- unwrap_flavor_key(rate, "rate", call = call)
  choice_keyed <- unwrap_flavor_key(choice, "choice", call = call)
  flavors <- c(rate_keyed$flavor, choice_keyed$flavor)

  if (length(unique(flavors)) > 1) {
    cli::cli_abort(
      c(
        "{.arg rate} and {.arg choice} must model the same flavor.",
        "x" = "{.arg rate} keys {.val {rate_keyed$flavor}} but {.arg choice}
               keys {.val {choice_keyed$flavor}}.",
        "i" = "One specification models one dependent process."
      ),
      call = call
    )
  }
  flavor <- if (length(flavors) > 0) flavors[[1]] else NULL

  if (!is.null(flavor)) {
    check_flavor_present(data, layer, flavor, call = call)
  } else {
    inform_unkeyed_flavored_layer(data, layer)
  }

  list(
    rate = rate_keyed$formula,
    choice = choice_keyed$formula,
    flavor = flavor
  )
}

# A submodel argument is either a plain one-sided formula or a one-element list
# whose formula LHS is the flavor symbol.
unwrap_flavor_key <- function(x, arg, call = rlang::caller_env()) {
  if (is.null(x) || inherits(x, "formula")) {
    return(list(formula = x, flavor = NULL))
  }
  if (!is.list(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a one-sided formula or a flavor-keyed list.",
      call = call
    )
  }
  if (length(x) != 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must key exactly one flavor.",
        "x" = "{length(x)} formulas were supplied.",
        "i" = "Estimating several dependent processes jointly is not supported
               yet; it needs stacked per-flavor likelihoods."
      ),
      call = call
    )
  }
  f <- x[[1]]
  if (!inherits(f, "formula") || length(f) != 3L) {
    cli::cli_abort(
      c(
        "{.arg {arg}}'s entry must be a formula whose left-hand side is the
         flavor.",
        "i" = "For example {.code {arg} = list(creation ~ 1 + indeg())}."
      ),
      call = call
    )
  }
  flavor <- deparse(f[[2]])
  # Dropping the left-hand side in place leaves the one-sided formula the
  # bundle parses, environment and all.
  f[[2]] <- NULL
  list(formula = f, flavor = flavor)
}

# A keyed flavor that matches no focal row models nothing at all.
check_flavor_present <- function(
  data,
  layer,
  flavor,
  call = rlang::caller_env()
) {
  if (is.null(data)) {
    return(invisible(NULL))
  }
  ties <- as.data.frame(data$ties)
  present <- unique(ties$flavor[ties$layer == layer])
  present <- present[!is.na(present)]
  if (!flavor %in% present) {
    cli::cli_abort(
      c(
        "No {.val {layer}} row carries the flavor {.val {flavor}}.",
        "i" = if (length(present) > 0) {
          "Flavor{?s} on this layer: {.val {present}}."
        } else {
          "This layer carries no {.field flavor} column."
        }
      ),
      call = call
    )
  }
  invisible(NULL)
}

# Modeling every row of a flavored layer is legitimate -- category-style flavors
# are often modeled together -- but it is also what a forgotten list looks like,
# so make it visible.
inform_unkeyed_flavored_layer <- function(data, layer) {
  if (is.null(data)) {
    return(invisible(NULL))
  }
  ties <- as.data.frame(data$ties)
  if (!"flavor" %in% names(ties)) {
    return(invisible(NULL))
  }
  present <- unique(ties$flavor[ties$layer == layer])
  present <- present[!is.na(present)]
  if (length(present) == 0) {
    return(invisible(NULL))
  }
  cli::cli_inform(c(
    "i" = "Layer {.val {layer}} carries the flavor{?s} {.val {present}};
           all its events are modeled.",
    "i" = "Model one with a keyed list, e.g.
           {.code rate = list({present[1]} ~ ...)}."
  ))
  invisible(NULL)
}

# Parse a one-sided submodel formula into the bundle preprocessing consumes,
# enforcing the empty-LHS contract and the validity matrix.
# Windows are recorded but not realized (recipe path), so
# the parse is pure; realization happens later at state creation.
build_specification_bundle <- function(
  one_sided,
  arg,
  model,
  sub_model,
  layer,
  envir,
  data = NULL
) {
  enforce_empty_lhs(one_sided, arg = arg, layer = layer, envir = envir)
  full_formula <- build_layer_formula(one_sided, layer)

  parsed <- parse_formula(
    full_formula,
    envir = envir,
    realize_windows = FALSE,
    data = data
  )

  # Interaction products compute in the dyad recipe loop; guard the
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
  # Offset (fixed-coefficient) terms are not estimated main effects,
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
# targeted message pointing at `layer`.
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

# Dependent-process facts for the print overview, read from the
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

# The same facts read from a stocnet: the focal layer's modeled rows. The layer
# is its own network, and its side pair comes from the mode map rather than from
# two node-set names.
stocnet_dependent_info <- function(data, layer, modeled_flavor = NULL) {
  src <- new_data_source(
    data = data,
    focal = layer,
    modeled_flavor = modeled_flavor
  )
  dependent <- src$streams$dependent
  timed <- dependent$time[!is.na(dependent$time)]
  sides <- ds_side_names(src)
  ties <- as.data.frame(data$ties)
  flavors <- unique(ties$flavor[ties$layer == layer])
  list(
    layer = layer,
    n_events = length(timed),
    time_span = if (length(timed) > 0) range(timed) else NULL,
    nodes = sides[1],
    nodes2 = sides[2],
    is_two_mode = ds_model_is_two_mode(src),
    network = layer,
    modeled_flavor = modeled_flavor,
    flavors = flavors[!is.na(flavors)]
  )
}
