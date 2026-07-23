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
#' # Modeling flavors of a layer
#'
#' When a layer's ties carry a `flavor` column, `rate` / `choice` may be given
#' as a **flavor-keyed list** whose formula left-hand side is the flavor to
#' model, e.g. `rate = list(creation ~ 1 + indeg())`. Only rows of that flavor
#' are modeled as events; every other focal row -- including rows with no
#' flavor -- still updates the network state. Supplying a plain formula on a
#' flavored layer models **all** of its rows, and says so.
#'
#' Several flavors can be modeled as parallel competing processes on the same
#' focal layer, e.g. `rate = list(creation ~ ..., dissolution ~ ...)`. Each key
#' becomes its own process with its own formulas and, on a `mutually_exclusive`
#' layer (see [add_flavor()]), a derived support constraint -- creation is
#' supportable only where no tie exists, dissolution only where one does --
#' AND-composed with any `support_constraint`. When both `rate` and `choice` are
#' keyed lists they MUST key the same flavor set. An unflavored layer under a
#' flavor-keyed list infers the mapping from its update semantics
#' (increment `+1`/`-1`, replace `1`/`0`) and says so.
#'
#' @param rate a one-sided formula (empty left-hand side) with the rate-model
#'   effects, a flavor-keyed list of one such formula, or `NULL`.
#' @param choice a one-sided formula (empty left-hand side) with the
#'   choice-model effects, a flavor-keyed list of one such formula, or `NULL`.
#'   Not applicable to `model = "REM"`.
#' @param model a character string, `"DyNAM"`, `"REM"`, or `"DyNAMi"`. For
#'   `"DyNAMi"` the `data` is an actors x groups object (see
#'   [make_groups_interaction()]) and `rate` is the flavor-keyed list
#'   `list(join ~ ..., leave ~ ...)` expressing the joining and leaving rate
#'   models; `choice` is a plain formula denoting the joining choice (the
#'   leaving choice is deterministic, so a flavor-keyed `choice` is rejected).
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
#' # The prebuilt `social_evolution` stocnet, whose focal layer is `calls`
#' data("social_evolution")
#' spec <- make_specification(
#'   choice = ~ inertia + recip + trans,
#'   model = "DyNAM", choice_sub_model = "choice",
#'   layer = "calls", data = social_evolution
#' )
#' spec
make_specification <- function(
  rate = NULL,
  choice = NULL,
  model = c("DyNAM", "REM", "DyNAMi"),
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

  # DyNAM-i keeps a thin specification: its rate is desugared from the
  # flavor-keyed list into the single legacy formula the monolith consumes, its
  # choice is a plain formula, and both stay unparsed so estimation parses them
  # against the environment the boundary bridge builds.
  if (model == "DyNAMi") {
    return(make_dynami_specification(
      rate = rate,
      choice = choice,
      data = data,
      layer = layer,
      rate_sub_model = rate_sub_model,
      choice_sub_model = choice_sub_model,
      support_constraint = support_constraint
    ))
  }

  # A wrapper-created dependent name in `layer` (recorded in info$dependents)
  # resolves to its focal layer and stamped flavor, so the legacy call surface
  # -- the dependent-events object name -- keeps identifying the process. A real
  # layer name is not in the map and passes through unchanged. The user's name
  # is kept for the specification's display `layer`, while the resolved focal
  # drives validation, parsing, and the dependent-process facts.
  display_layer <- layer
  layer_flavor <- NULL
  if (!is_legacy && !is.null(layer)) {
    entry <- data$info$dependents[[layer]]
    if (!is.null(entry)) {
      layer <- entry$layer
      if (!is.na(entry$flavor)) {
        layer_flavor <- entry$flavor
      }
    }
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

  if (
    !is.null(support_constraint) && !inherits(support_constraint, "formula")
  ) {
    cli::cli_abort(
      "{.arg support_constraint} must be a formula or {.val NULL}."
    )
  }
  # Dyadic atoms are legal when the spec has a dyad-indexed part (a choice
  # submodel, or REM), rejected for a rate-only spec (sender-axis rule). The
  # same rule governs the derived `tie(layer)` masks (dyadic point atoms).
  has_dyad_part <- model == "REM" || !is.null(choice)

  flavored <- resolve_modeled_flavors(
    rate,
    choice,
    spec_data,
    layer,
    info = if (is_legacy) NULL else data$info,
    wrapper_flavor = layer_flavor
  )
  flavors <- flavored$flavors

  make_bundle <- function(one_sided, arg, sub_model) {
    build_specification_bundle(
      one_sided,
      arg = arg,
      model = model,
      sub_model = sub_model,
      layer = layer,
      envir = work_env,
      data = spec_data
    )
  }

  # The parsed user constraint (or NULL) is the base each flavor's derived
  # constraint AND-composes with; a plain/unflavored spec uses it directly.
  base_constraint_plan <- if (!is.null(support_constraint)) {
    parse_and_validate_constraint(
      support_constraint,
      has_dyad_part = has_dyad_part,
      envir = work_env
    )
  } else {
    NULL
  }

  # Multi-flavor: K parallel competing processes on one focal layer. Each
  # process carries its own rate/choice bundles and its own derived-plus-user
  # support constraint; the objects feed the per-flavor estimation loop.
  if (length(flavors) > 1) {
    fam <- c(
      if (!is.null(rate)) "rate",
      if (!is.null(choice)) "choice"
    )
    processes <- build_flavor_processes(
      flavored,
      flavors = flavors,
      rate = rate,
      choice = choice,
      rate_sub_model = rate_sub_model,
      choice_sub_model = choice_sub_model,
      support_constraint = support_constraint,
      base_constraint_plan = base_constraint_plan,
      has_dyad_part = has_dyad_part,
      layer = layer,
      make_bundle = make_bundle,
      envir = work_env
    )
    dependent <- stocnet_dependent_info(data, layer, modeled_flavor = NULL)
    dependent$modeled_flavors <- flavors
    dependent$n_events <- flavor_event_count(data, layer, flavors)
    return(structure(
      list(
        model = model,
        submodels = stats::setNames(vector("list", length(fam)), fam),
        processes = processes,
        layer = display_layer %||% layer,
        focal = layer,
        modeled_flavor = NULL,
        modeled_flavors = flavors,
        flavor_style = flavored$style,
        dependent = dependent,
        support_constraint = support_constraint,
        constraint = base_constraint_plan,
        valid = TRUE,
        data = data,
        call = match.call()
      ),
      class = "specification.goldfish"
    ))
  }

  # Plain or single-flavor: one dependent process, the historical shape. A
  # single modeled flavor on a `mutually_exclusive` layer still derives its
  # create-only-where-absent / dissolve-only-where-present mask.
  modeled_flavor <- if (length(flavors) == 1) flavors else NULL
  submodels <- list()
  if (!is.null(rate)) {
    submodels$rate <- make_bundle(flavored$rate, "rate", rate_sub_model)
  }
  if (!is.null(choice)) {
    submodels$choice <- make_bundle(flavored$choice, "choice", choice_sub_model)
  }

  derived_constraint <- if (
    !is.null(modeled_flavor) &&
      identical(flavored$style, "mutually_exclusive") &&
      !is.null(flavored$mapping)
  ) {
    derive_flavor_constraint(layer, flavored$mapping, modeled_flavor, work_env)
  } else {
    NULL
  }
  constraint_plan <- if (!is.null(derived_constraint)) {
    parse_and_validate_constraint(
      and_compose_constraint(derived_constraint, support_constraint),
      has_dyad_part = has_dyad_part,
      envir = work_env
    )
  } else {
    base_constraint_plan
  }

  structure(
    list(
      model = model,
      submodels = submodels,
      layer = display_layer %||% layer,
      # The focal and the modeled flavor together select the dependent rows, so
      # they travel with the specification: state creation must resolve the same
      # dependent stream this parse did, not the one `info$focal` declares.
      focal = layer,
      modeled_flavor = modeled_flavor,
      modeled_flavors = if (is.null(modeled_flavor)) {
        character(0)
      } else {
        modeled_flavor
      },
      flavor_style = flavored$style,
      derived_constraint = derived_constraint,
      dependent = if (is_legacy) {
        spec_dependent_info(get(layer, envir = work_env), layer)
      } else {
        stocnet_dependent_info(data, layer, modeled_flavor)
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

# Build the thin DyNAM-i specification. DyNAM-i estimation still runs on the
# interaction monolith via the boundary bridge, so the specification only
# desugars the flavor-keyed rate into the single legacy formula and carries the
# plain choice formula, both unparsed: `estimate_from_specification()` forwards
# them to the wrapper, which bridges the stocnet and parses against the
# environment. A flavor-keyed `choice` is rejected -- the leaving choice is
# deterministic, so `choice` is only the joining choice.
make_dynami_specification <- function(
  rate,
  choice,
  data,
  layer,
  rate_sub_model,
  choice_sub_model,
  support_constraint,
  call = rlang::caller_env()
) {
  if (!inherits(data, "stocnet")) {
    cli::cli_abort(
      c(
        "{.fn make_specification} for {.val DyNAMi} needs a {.cls stocnet}.",
        "i" = "Build the actors x groups object with \\
               {.fn make_groups_interaction}."
      ),
      call = call
    )
  }
  focal <- layer %||% data$info$focal
  validate_goldfish_data(data, focal = focal)

  if (is.list(choice) && !inherits(choice, "formula")) {
    cli::cli_abort(
      c(
        "A flavor-keyed {.arg choice} is not valid for {.val DyNAMi}.",
        "x" = "The leaving choice is deterministic (an actor leaves the group \\
               it is in), so there is no leaving choice to model.",
        "i" = "Pass a plain {.arg choice} formula -- it is the joining choice."
      ),
      call = call
    )
  }

  focal_symbol <- as.symbol(focal)
  submodels <- list()
  if (!is.null(rate)) {
    submodels$rate <- list(
      formula = desugar_dynami_rate(rate, data, focal, call = call),
      sub_model = rate_sub_model,
      parsed = NULL
    )
  }
  if (!is.null(choice)) {
    # The one-sided choice formula gains the focal layer on its LHS so the
    # boundary rewrite resolves it to the dependent process.
    choice_formula <- stats::as.formula(
      call("~", focal_symbol, choice[[length(choice)]]),
      env = environment(choice)
    )
    submodels$choice <- list(
      formula = choice_formula,
      sub_model = choice_sub_model,
      parsed = NULL
    )
  }

  structure(
    list(
      model = "DyNAMi",
      submodels = submodels,
      layer = focal,
      focal = focal,
      modeled_flavor = NULL,
      modeled_flavors = character(0),
      support_constraint = support_constraint,
      # The availability constraint is derived at estimation (choice only), so
      # the specification carries only the user constraint; DyNAM-i keeps it
      # unparsed for the wrapper to parse against the bridged environment.
      constraint = support_constraint,
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

# Resolve which focal rows a specification models, across one or more flavors.
#
# A flavor-keyed list (`rate = list(creation ~ ..., dissolution ~ ...)`) names
# the flavors to model as parallel processes; every other focal row -- including
# one with no flavor -- updates state but is not a modeled event. A plain
# formula models every row, which on a flavored layer is a real choice, not an
# oversight, so it is announced.
#
# Returns, for the <=1 case, the single one-sided `rate`/`choice` formulas; for
# the >1 case, the per-flavor `rate_formulas`/`choice_formulas`. Always returns
# the resolved `flavors`, plus the layer's `mapping` and `style` (for deriving
# support constraints).
resolve_modeled_flavors <- function(
  rate,
  choice,
  data,
  layer,
  info,
  wrapper_flavor = NULL,
  call = rlang::caller_env()
) {
  rate_u <- unwrap_flavor_list(rate, "rate", call = call)
  choice_u <- unwrap_flavor_list(choice, "choice", call = call)

  # When both sub-models are flavor-keyed they must key the same processes.
  if (!is.null(rate_u$flavors) && !is.null(choice_u$flavors)) {
    if (!setequal(rate_u$flavors, choice_u$flavors)) {
      cli::cli_abort(
        c(
          "{.arg rate} and {.arg choice} must key the same flavor set.",
          "x" = "{.arg rate} keys {.val {rate_u$flavors}} but {.arg choice}
                 keys {.val {choice_u$flavors}}.",
          "i" = "Each flavor is a parallel process modeled by both sub-models."
        ),
        call = call
      )
    }
  }

  keyed <- !is.null(rate_u$flavors) || !is.null(choice_u$flavors)

  if (!keyed) {
    # A wrapper-resolved dependent name supplies its flavor internally (the
    # process was already selected by the dependent-events object); otherwise a
    # bare formula on a flavored layer models every row, announced.
    flavor <- wrapper_flavor
    if (is.null(flavor)) {
      inform_unkeyed_flavored_layer(data, layer)
    }
    return(list(
      keyed = FALSE,
      flavors = if (is.null(flavor)) character(0) else flavor,
      rate = rate_u$formula,
      choice = choice_u$formula,
      rate_formulas = NULL,
      choice_formulas = NULL,
      mapping = NULL,
      style = NULL
    ))
  }

  flavors <- union(rate_u$flavors, choice_u$flavors)

  # Modeling several flavors, a plain sub-model would model every row while its
  # keyed sibling splits them -- inconsistent, so both must be keyed.
  if (length(flavors) > 1) {
    if (!is.null(rate) && is.null(rate_u$flavors)) {
      abort_plain_with_multi("rate", call = call)
    }
    if (!is.null(choice) && is.null(choice_u$flavors)) {
      abort_plain_with_multi("choice", call = call)
    }
  }

  resolved <- resolve_flavor_keys(data, layer, flavors, info, call = call)

  if (length(flavors) == 1) {
    fl <- flavors
    return(list(
      keyed = TRUE,
      flavors = fl,
      rate = if (is.null(rate_u$flavors)) {
        rate_u$formula
      } else {
        rate_u$formulas[[fl]]
      },
      choice = if (is.null(choice_u$flavors)) {
        choice_u$formula
      } else {
        choice_u$formulas[[fl]]
      },
      rate_formulas = NULL,
      choice_formulas = NULL,
      mapping = resolved$mapping,
      style = resolved$style
    ))
  }

  list(
    keyed = TRUE,
    flavors = flavors,
    rate = NULL,
    choice = NULL,
    rate_formulas = rate_u$formulas,
    choice_formulas = choice_u$formulas,
    mapping = resolved$mapping,
    style = resolved$style
  )
}

# A submodel argument is either a plain one-sided formula or a flavor-keyed list
# whose entries' formula LHS is the flavor symbol. Returns `formula` (plain) or
# `formulas` (named by flavor) plus the `flavors` vector (NULL when plain).
unwrap_flavor_list <- function(x, arg, call = rlang::caller_env()) {
  if (is.null(x) || inherits(x, "formula")) {
    return(list(formula = x, formulas = NULL, flavors = NULL))
  }
  if (!is.list(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a one-sided formula or a flavor-keyed list.",
      call = call
    )
  }
  if (length(x) == 0) {
    cli::cli_abort(
      "{.arg {arg}} is an empty list; supply at least one flavor formula.",
      call = call
    )
  }
  formulas <- vector("list", length(x))
  flavors <- character(length(x))
  for (i in seq_along(x)) {
    f <- x[[i]]
    if (!inherits(f, "formula") || length(f) != 3L) {
      cli::cli_abort(
        c(
          "{.arg {arg}}'s entries must be formulas whose left-hand side is the
           flavor.",
          "i" = "For example {.code {arg} = list(creation ~ 1 + indeg())}."
        ),
        call = call
      )
    }
    flavors[i] <- deparse(f[[2]])
    # Dropping the left-hand side in place leaves the one-sided formula the
    # bundle parses, environment and all.
    f[[2]] <- NULL
    formulas[[i]] <- f
  }
  dup <- unique(flavors[duplicated(flavors)])
  if (length(dup) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} keys a flavor more than once.",
        "x" = "Duplicate key{?s}: {.val {dup}}."
      ),
      call = call
    )
  }
  names(formulas) <- flavors
  list(formula = NULL, formulas = formulas, flavors = flavors)
}

abort_plain_with_multi <- function(arg, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "{.arg {arg}} must be a flavor-keyed list when modeling several flavors.",
      "i" = "Key it on the same flavors, e.g.
             {.code {arg} = list(creation ~ ..., dissolution ~ ...)}."
    ),
    call = call
  )
}

# Resolve the modeled flavor keys against the layer's state, returning the
# value->flavor `mapping` and the `flavor_style` used to derive constraints.
# An explicitly flavored layer resolves keys against the values present; an
# unflavored layer infers the mapping from its update semantics.
resolve_flavor_keys <- function(data, layer, flavors, info, call) {
  if (is.null(data)) {
    # The legacy environment path has no stocnet to validate against or infer
    # from; the keys pass through unvalidated and derive no constraint.
    return(list(mapping = NULL, style = NULL))
  }
  ties <- as.data.frame(data$ties)
  present <- unique(ties$flavor[ties$layer == layer])
  present <- present[!is.na(present)]

  style <- if (
    !is.null(info$flavor_style) && layer %in% names(info$flavor_style)
  ) {
    unname(info$flavor_style[[layer]])
  } else {
    NULL
  }
  mapping <- info$values_equivalence[[layer]]
  # Deriving a constraint needs both halves: the style says the flavors compete,
  # the mapping says which flavor corresponds to which state value. Declaring
  # the style alone used to derive nothing and say nothing, leaving the user
  # with an unrestricted risk set they believe is restricted.
  if (identical(style, "mutually_exclusive") && is.null(mapping)) {
    cli::cli_abort(
      c(
        "Layer {.val {layer}} is declared {.val mutually_exclusive} but carries
         no {.field values_equivalence}.",
        "x" = "Without the mapping there is no way to tell which flavor creates
               the tie and which dissolves it, so no support constraint can be
               derived.",
        "i" = "Set both with {.fn add_flavor}."
      ),
      call = call
    )
  }

  if (length(present) > 0) {
    unknown <- setdiff(flavors, present)
    if (length(unknown) > 0) {
      cli::cli_abort(
        c(
          "{cli::qty(unknown)}Flavor key{?s} {.val {unknown}} {?matches/match}
           no {.val {layer}} row.",
          "i" = "Flavor{?s} on this layer: {.val {present}}."
        ),
        call = call
      )
    }
    return(list(mapping = mapping, style = style))
  }

  inferred <- infer_flavor_mapping(data, layer, info, flavors, call = call)
  # Inference resolves the MAPPING only. It deliberately assigns no style: a
  # support constraint restricts the risk set, which is a modeling decision, and
  # nothing in an unflavored layer's update values expresses whether repeated
  # same-direction events are meaningful. A layer whose ±1 increments accumulate
  # (treaties signed again and again) looks identical here to one that toggles.
  # The user declares the style through `add_flavor()`.
  list(mapping = inferred, style = NULL)
}

# Infer a creation/dissolution mapping for an unflavored layer from its update
# semantics: increment +1/-1 or replace 1/0. The higher value is creation, the
# lower dissolution. Ambiguous encodings or keys outside the inferred names
# abort with guidance to add_flavor().
infer_flavor_mapping <- function(data, layer, info, flavors, call) {
  update <- if (!is.null(info$update) && layer %in% names(info$update)) {
    unname(info$update[[layer]])
  } else {
    NA_character_
  }
  expected <- flavor_update_values(update)
  ties <- as.data.frame(data$ties)
  weight <- if ("weight" %in% names(ties)) ties$weight else rep(1, nrow(ties))
  vals <- unique(weight[ties$layer == layer & !is.na(ties$time)])
  if (is.null(expected) || !all(vals %in% expected)) {
    cli::cli_abort(
      c(
        "Cannot infer a flavor mapping for unflavored layer {.val {layer}}.",
        "x" = if (is.null(expected)) {
          "Its update {.val {update}} carries no dichotomous state."
        } else {
          "Its update values {.val {vals}} are not the dichotomous
           {.val {expected}}."
        },
        "i" = "Stamp flavors explicitly with {.fn add_flavor}."
      ),
      call = call
    )
  }
  # expected is ascending (increment c(-1, 1), replace c(0, 1)): the lower value
  # dissolves the tie, the higher creates it.
  mapping <- stats::setNames(expected, c("dissolution", "creation"))
  unknown <- setdiff(flavors, names(mapping))
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "{cli::qty(unknown)}Flavor key{?s} {.val {unknown}} {?does/do} not match
         the mapping inferred for {.val {layer}}.",
        "i" = "Inferred flavors: {.val {names(mapping)}}.",
        "i" = "For other flavor names, stamp them with {.fn add_flavor}."
      ),
      call = call
    )
  }
  cli::cli_inform(c(
    "i" = "Layer {.val {layer}} is unflavored; assuming
           {.val creation} = {mapping[['creation']]} and
           {.val dissolution} = {mapping[['dissolution']]}.",
    "i" = "No support constraint is derived from an assumed mapping; use
           {.fn add_flavor} with {.arg flavor_style} to declare one."
  ))
  mapping
}

# Build the per-flavor process objects for a multi-flavor specification: each
# carries its rate/choice bundles and its derived-plus-user support constraint.
build_flavor_processes <- function(
  flavored,
  flavors,
  rate,
  choice,
  rate_sub_model,
  choice_sub_model,
  support_constraint,
  base_constraint_plan,
  has_dyad_part,
  layer,
  make_bundle,
  envir
) {
  derive <- identical(flavored$style, "mutually_exclusive") &&
    !is.null(flavored$mapping)
  processes <- vector("list", length(flavors))
  names(processes) <- flavors
  for (fl in flavors) {
    sub <- list()
    if (!is.null(rate)) {
      sub$rate <- make_bundle(
        flavored$rate_formulas[[fl]],
        "rate",
        rate_sub_model
      )
    }
    if (!is.null(choice)) {
      sub$choice <- make_bundle(
        flavored$choice_formulas[[fl]],
        "choice",
        choice_sub_model
      )
    }
    derived <- if (derive) {
      derive_flavor_constraint(layer, flavored$mapping, fl, envir)
    } else {
      NULL
    }
    cplan <- if (!is.null(derived)) {
      parse_and_validate_constraint(
        and_compose_constraint(derived, support_constraint),
        has_dyad_part = has_dyad_part,
        envir = envir
      )
    } else {
      base_constraint_plan
    }
    processes[[fl]] <- list(
      submodels = sub,
      derived_constraint = derived,
      constraint = cplan
    )
  }
  processes
}

# The derived support constraint for one flavor on a mutually-exclusive layer:
# the flavor at the higher update value creates the tie, supportable only where
# no tie exists (`~ !tie(layer)`); the lower value dissolves it, supportable
# only where one does (`~ tie(layer)`). `tie(layer)` reads the layer's state.
derive_flavor_constraint <- function(layer, mapping, flavor, envir) {
  atom <- call("tie", as.name(layer))
  rhs <- if (mapping[[flavor]] == max(mapping)) call("!", atom) else atom
  stats::as.formula(call("~", rhs), env = envir)
}

# AND-compose a derived flavor constraint with any user support_constraint into
# one one-sided formula. Either may be NULL.
and_compose_constraint <- function(derived_formula, user_constraint) {
  if (is.null(derived_formula)) {
    return(user_constraint)
  }
  derived_rhs <- derived_formula[[length(derived_formula)]]
  if (is.null(user_constraint)) {
    return(derived_formula)
  }
  user_rhs <- user_constraint[[length(user_constraint)]]
  stats::as.formula(
    call("~", call("&", derived_rhs, user_rhs)),
    env = environment(derived_formula)
  )
}

# Count a layer's timed rows modeled by the given flavors, for the print
# overview. An unflavored (inferred) layer has all its timed rows modeled.
flavor_event_count <- function(data, layer, flavors) {
  ties <- as.data.frame(data$ties)
  timed <- ties$layer == layer & !is.na(ties$time)
  if (!"flavor" %in% names(ties) || all(is.na(ties$flavor[timed]))) {
    return(sum(timed))
  }
  sum(timed & ties$flavor %in% flavors)
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
  # Role-aware, mirroring the plain-formula estimation path: offset
  # (fixed-coefficient) terms and interaction operand-only terms are not bare
  # main effects, so they are held out of the identification check -- an operand
  # such as `global`/`ego` feeding an interaction that restores variation
  # follows the separate operand rule. A term that is both a requested main
  # effect and an operand (the `a*b` case) stays in the check.
  is_offset <- unlist(parsed$offset_parameter)
  if (is.null(is_offset)) {
    is_offset <- logical(length(parsed$rhs_names))
  }
  is_main <- unlist(parsed$is_main_parameter)
  if (is.null(is_main)) {
    is_main <- rep(TRUE, length(parsed$rhs_names))
  }
  is_operand <- unlist(parsed$is_operand_parameter)
  if (is.null(is_operand)) {
    is_operand <- logical(length(parsed$rhs_names))
  }
  main_effect <- !is_offset & !(is_operand & !is_main)
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
    # The real mode names behind the synthetic side keys, so the print shows the
    # data's own vocabulary ("actor -> concept") instead of "nodes_side1 ...".
    mode_pair = ds_layer_mode_pair(src, layer),
    is_two_mode = ds_model_is_two_mode(src),
    network = layer,
    modeled_flavor = modeled_flavor,
    flavors = flavors[!is.na(flavors)]
  )
}
