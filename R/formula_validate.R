# Per-(model, sub_model) main-effect validity.
#
# A single local rule table encoding the softmax-identification rule: an effect
# is a valid *main* effect only if its statistic varies on the axis the
# sub_model estimates over; otherwise it is constant across the compared
# alternatives/units and cancels, so it is not identified. Rejected as *main*
# effects (but permitted inside interaction terms once those land):
#
#   - DyNAM / DyNAMi choice, choice_coordination: `global` and ego-perspective
#     effects (`ego()`, degree `type = "ego"`) — constant across the receiver
#     alternatives, so they cancel in the multinomial softmax.
#   - DyNAM rate_ordered / REM rate_ordered: `global` — constant across the
#     compared units, cancels in the ordinal partial likelihood.
#   - DyNAM rate / rate_ordered: alter-perspective effects (`alter()`, degree
#     `type = "alter"`) — a rate model is sender-indexed, there is no receiver
#     axis for an alter statistic to vary on.
#
# This local table mirrors the identification matrix; it is intended to
# be superseded by a future effect-term registry's metadata. It runs after the
# formula's `*` expansion (the parser uses `stats::terms()`), so an interaction
# that expands to an illegal bare main effect is still caught.

# Classify an effect's variation axis from its name and resolved `type` argument.
# Degree-family effects with no explicit `type` are "degree": their perspective
# is model-dependent (sender-axis in a rate model, receiver-alter in a dyad
# model), and neither reading is ever rejected, so they always pass.
effect_variation <- function(name, type) {
  if (identical(name, "global")) {
    return("global")
  }
  if (identical(name, "ego")) {
    return("ego")
  }
  if (identical(name, "alter")) {
    return("alter")
  }
  if (name %in% c("indeg", "outdeg", "degree")) {
    if (identical(type, "ego")) {
      return("ego")
    }
    if (identical(type, "alter")) {
      return("alter")
    }
    return("degree")
  }
  "other"
}

# Variation axes with no bare effect implementation for a (model, sub_model)
# pair: rejected in every phase (preprocessing included), because the statistic
# cannot even be computed. Empty now that `global` is computable in DyNAM choice
# (`init_DyNAM_choice.global` was added later); kept as the seam for any future
# genuinely-unavailable effect.
unavailable_variations <- function(model, sub_model) {
  character(0)
}

# Variation axes that ARE computable (available via compute_statistics() as design
# columns for interactions / random effects) but are not identified
# as bare main effects, so they are rejected only when actually estimating.
unidentified_variations <- function(model, sub_model) {
  is_dynam <- model %in% c("DyNAM", "DyNAMi")
  # DyNAM choice: both `global` and ego-perspective columns are constant across
  # the receiver alternatives, so neither is identified as a bare main effect
  # (both remain computable via compute_statistics and usable as interaction
  # operands).
  if (is_dynam && sub_model %in% c("choice", "choice_coordination")) {
    return(c("global", "ego"))
  }
  # DyNAM rate is sender-indexed: no receiver axis (alter), and the ordinal case
  # additionally drops the constant global covariate.
  if (is_dynam && sub_model == "rate_ordered") {
    return(c("global", "alter"))
  }
  if (is_dynam && sub_model == "rate") {
    return("alter")
  }
  # REM is tie-oriented (a receiver axis exists, so alter is valid); only the
  # ordinal case drops the constant global covariate. REM rate allows all.
  if (model == "REM" && sub_model == "rate_ordered") {
    return("global")
  }
  character(0)
}

# Validate the parsed main effects against the rule table, aborting once with
# every violation. `effect_names` / `effect_types` are aligned per-term vectors
# (effect name and resolved `type`, `""` when absent), typically
# `vapply(rhs_names, "[[", character(1), 1)` and the parser's `type_parameter`.
# `estimating = FALSE` (preprocessing) checks only the unavailable effects, so
# computable-but-unidentified columns can still be produced.
validate_effects <- function(
  model,
  sub_model,
  effect_names,
  effect_types,
  estimating = TRUE
) {
  disallowed <- unavailable_variations(model, sub_model)
  if (estimating) {
    disallowed <- union(disallowed, unidentified_variations(model, sub_model))
  }
  if (length(disallowed) == 0 || length(effect_names) == 0) {
    return(invisible(NULL))
  }
  variations <- mapply(
    effect_variation,
    effect_names,
    effect_types,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  bad <- which(variations %in% disallowed)
  if (length(bad) == 0) {
    return(invisible(NULL))
  }

  # One self-contained bullet per offending effect (name + reason). Avoids cli
  # pluralization markup in a header that also interpolates a vector, which cli
  # rejects with "Multiple quantities for pluralization".
  offenders <- vapply(
    bad,
    function(i) {
      reason <- switch(
        variations[i],
        global = "constant across alternatives",
        ego = "an ego-perspective statistic, constant across the alternatives",
        alter = "an alter-perspective statistic with no receiver axis in a rate model"
      )
      sprintf("{.code %s}: %s.", effect_names[i], reason)
    },
    character(1)
  )
  offenders <- stats::setNames(offenders, rep("x", length(offenders)))

  cli::cli_abort(c(
    "Unsupported main effect in {.code model = {.val {model}}},
     {.code sub_model = {.val {sub_model}}}:",
    offenders,
    "i" = "These effects are computable (e.g. via {.fn compute_statistics}) but are
           not identified as bare main effects; they will be usable through
           interaction terms in a future release. Use a rate sub-model (or add
           the time intercept) to estimate a global main effect."
  ))
}

# Validate interaction *operands* (role-aware). Operands are held out
# of the main-effect check (an operand is not a bare main effect); instead a
# sender-indexed (DyNAM / DyNAMi rate / rate_ordered) model requires each operand
# to vary on the sender axis, so an `alter`-perspective operand is rejected (a
# rate model has no receiver axis for it to vary on). `global` is permitted as an
# operand even in `rate_ordered`, where it is rejected as a bare main effect,
# because interacting it with a sender-varying operand restores identified
# per-sender variation. Dyad-indexed models place no operand restriction here.
validate_operands <- function(model, sub_model, operand_names, operand_types) {
  is_sender <- model %in%
    c("DyNAM", "DyNAMi") &&
    sub_model %in% c("rate", "rate_ordered")
  if (!is_sender || length(operand_names) == 0) {
    return(invisible(NULL))
  }
  variations <- mapply(
    effect_variation,
    operand_names,
    operand_types,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  bad <- which(variations == "alter")
  if (length(bad) == 0) {
    return(invisible(NULL))
  }
  offenders <- stats::setNames(
    sprintf(
      "{.code %s}: an alter-perspective statistic with no sender axis.",
      operand_names[bad]
    ),
    rep("x", length(bad))
  )
  cli::cli_abort(c(
    "Unsupported interaction operand in {.code model = {.val {model}}},
     {.code sub_model = {.val {sub_model}}}:",
    offenders,
    "i" = "A sender-indexed (rate) model has no receiver axis, so interaction
           operands must vary on the sender axis
           ({.code ego}, {.code global}, degree {.code type = \"ego\"})."
  ))
}

# Interaction products are computed in the recipe loops: the dyad-indexed kernel
# (DyNAM choice / choice_coordination, REM) and the sender-indexed kernel (DyNAM
# rate / rate_ordered). Only DyNAMi, whose preprocessing routes to the
# `preprocess_interaction` monolith rather than a recipe loop, is not yet
# supported (deferred to a future DyNAMi engine refactor).
abort_if_interactions_unsupported <- function(
  parsed_formula,
  model = NULL,
  sub_model = NULL
) {
  interactions <- parsed_formula$interactions
  if (length(interactions) == 0) {
    return(invisible(NULL))
  }
  is_supported <- !is.null(model) && model %in% c("DyNAM", "REM")
  if (is_supported) {
    return(invisible(NULL))
  }
  labels <- vapply(interactions, function(x) x$label, character(1))
  cli::cli_abort(c(
    "Interaction terms are not yet supported for {.code model = {.val {model}}},
     {.code sub_model = {.val {sub_model}}}.",
    "x" = "Offending term{?s}: {.code {labels}}.",
    "i" = "Interaction effects are currently available for DyNAM and REM models
           (DyNAMi is not yet supported)."
  ))
}

# The fixed-coefficient contract handed from the estimation front-end to the
# estimation kernels. `idx` holds positions into the final coefficient vector
# [intercept?, function-effects..., interactions...], `values` the value each of
# those coefficients is held at, and `names` the term label each position came
# from, so an error can name the offending term instead of a bare index.
# Whether every coefficient is fixed (likelihood-only evaluation) and whether
# the intercept is fixed are derivations on `idx`, never stored: the number of
# coefficients belongs to the estimator, not to the contract.
new_fixed_spec <- function(idx, values, names) {
  idx <- as.integer(idx)
  values <- as.numeric(values)
  names <- as.character(names)
  if (length(idx) != length(values) || length(idx) != length(names)) {
    cli::cli_abort(
      "A fixed-coefficient contract needs one value and one term label per
       position ({length(idx)} position{?s}, {length(values)} value{?s},
       {length(names)} label{?s}).",
      .internal = TRUE
    )
  }
  if (length(idx) == 0) {
    cli::cli_abort(
      "A fixed-coefficient contract cannot be empty; use {.code NULL} when no
       coefficient is fixed.",
      .internal = TRUE
    )
  }
  if (anyNA(idx) || any(idx < 1L)) {
    cli::cli_abort(
      "Fixed-coefficient positions must be positive integers.",
      .internal = TRUE
    )
  }
  if (anyDuplicated(idx)) {
    duplicated_names <- unique(names[duplicated(idx)])
    cli::cli_abort(c(
      "A coefficient cannot be fixed twice.",
      "x" = "Repeated term{?s}: {.code {duplicated_names}}."
    ))
  }
  if (anyNA(values)) {
    cli::cli_abort(c(
      "A fixed coefficient needs a value.",
      "x" = "Missing value for term{?s}: {.code {names[is.na(values)]}}."
    ))
  }
  structure(
    list(idx = idx, values = values, names = names),
    class = "fixed_spec"
  )
}

is_fixed_spec <- function(x) inherits(x, "fixed_spec")

# Term labels for every coefficient position, in coefficient order
# [intercept?, function-effects..., interactions...]. A function effect is
# rendered as the call the user wrote (arguments kept, names restored) so an
# error names the term rather than its bare effect name -- two `inertia()` terms
# on different networks are otherwise indistinguishable in a message.
coefficient_term_labels <- function(parsed_formula, rhs_names, has_intercept) {
  effects <- vapply(rhs_names, deparse_rhs_term, character(1))
  interactions <- vapply(
    parsed_formula$interactions,
    function(x) x$label,
    character(1)
  )
  c(if (has_intercept) "Intercept", effects, interactions)
}

deparse_rhs_term <- function(term) {
  parts <- unlist(term)
  if (length(parts) <= 1L) {
    return(as.character(parts[[1]]))
  }
  args <- as.character(parts[-1])
  keys <- names(parts)[-1]
  if (!is.null(keys)) {
    named <- nzchar(keys)
    args[named] <- paste(keys[named], args[named], sep = " = ")
  }
  sprintf("%s(%s)", parts[[1]], paste(args, collapse = ", "))
}

# Flatten a contract into the positional NA-vector encoding (NA = estimate, a
# value = fix) for the consumers that still read it.
fixed_spec_to_vector <- function(fixed_spec, n_params) {
  if (is.null(fixed_spec)) {
    return(NULL)
  }
  out <- rep(NA_real_, n_params)
  out[fixed_spec$idx] <- fixed_spec$values
  out
}

# Assemble the fixed-coefficient contract at the single point where term names
# and coefficient positions are both known. Three sources fix a coefficient:
# `offset()` terms (fixed at `offset_coef`, aligned by formula order),
# interaction operand-only terms (kept in the design but held out of estimation
# by fixing at 0 — a 0 coefficient contributes 0 * stat, i.e. the column is
# excluded from the model while retained for downstream), and the superseded
# positional `fixed_parameters` vector, which is converted here so the wire
# carries one encoding only. An effect at rhs position j maps to coefficient j
# (+1 when the intercept is prepended); interaction columns are estimated.
# Returns `NULL` when no coefficient is fixed. A constant-across-alternatives
# offset in choice cancels in the softmax, so it warns rather than aborts.
assemble_fixed_parameters <- function(
  parsed_formula,
  rhs_names,
  has_intercept,
  model,
  sub_model,
  fixed_parameters,
  offset_coef
) {
  is_offset <- unlist(parsed_formula$offset_parameter)
  if (is.null(is_offset)) {
    is_offset <- logical(length(rhs_names))
  }
  estimate <- unlist(parsed_formula$estimate_parameter)
  if (is.null(estimate)) {
    estimate <- rep(TRUE, length(rhs_names))
  }
  intercept_shift <- as.integer(has_intercept)
  n_params <- length(rhs_names) +
    length(parsed_formula$interactions) +
    intercept_shift
  labels <- coefficient_term_labels(parsed_formula, rhs_names, has_intercept)

  offset_positions <- which(is_offset) + intercept_shift
  if (length(offset_positions) == 0 && !is.null(offset_coef)) {
    cli::cli_abort(c(
      "{.arg offset_coef} was supplied but the formula has no
       {.fn offset} terms.",
      "i" = "Wrap a term in {.fn offset} to fix its coefficient."
    ))
  }
  if (
    length(offset_positions) > 0 &&
      length(offset_coef) != length(offset_positions)
  ) {
    cli::cli_abort(c(
      "{.arg offset_coef} must supply one value per {.fn offset} term.",
      "x" = "The formula has {length(offset_positions)} offset term{?s}
             ({.code {labels[offset_positions]}}) but {.arg offset_coef} has
             {length(offset_coef)} value{?s}.",
      "i" = "Set it via {.code set_algorithm_newton(offset_coef = ...)}."
    ))
  }
  if (!is.null(fixed_parameters) && length(fixed_parameters) != n_params) {
    cli::cli_abort(c(
      "{.arg fixed_parameters} must supply one entry per coefficient.",
      "x" = "The model has {n_params} coefficient{?s}
             ({.code {labels}}) but {.arg fixed_parameters} has
             {length(fixed_parameters)} entr{?y/ies}.",
      "i" = "Wrap a term in {.fn offset} to fix its coefficient by name
             instead."
    ))
  }

  # Applied in this order, later sources overwriting earlier ones: operand-only
  # interaction terms, the superseded positional vector, the `offset()` values.
  values <- rep(NA_real_, n_params)
  values[which(!estimate) + intercept_shift] <- 0
  if (!is.null(fixed_parameters)) {
    supplied <- !is.na(fixed_parameters)
    values[supplied] <- fixed_parameters[supplied]
  }
  values[offset_positions] <- offset_coef

  if (
    model %in%
      c("DyNAM", "DyNAMi") &&
      sub_model %in% c("choice", "choice_coordination")
  ) {
    offset_names <- vapply(rhs_names[is_offset], "[[", character(1), 1)
    offset_types <- vapply(
      parsed_formula$type_parameter[is_offset],
      as.character,
      character(1)
    )
    variations <- mapply(
      effect_variation,
      offset_names,
      offset_types,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    constant <- variations %in% c("ego", "global")
    if (any(constant)) {
      cli::cli_warn(c(
        "!" = "Offset term{?s} {.code {offset_names[constant]}} {?is/are}
               constant across the choice alternatives.",
        "i" = "Such offsets cancel in the multinomial softmax and have no
               effect on the estimates."
      ))
    }
  }

  idx <- which(!is.na(values))
  if (length(idx) == 0) {
    return(NULL)
  }
  new_fixed_spec(idx, values[idx], labels[idx])
}
