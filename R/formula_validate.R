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
    class = "goldfishCoefFixed"
  )
}

is_fixed_spec <- function(x) inherits(x, "goldfishCoefFixed")

# The starting-value contract, the same shape as the fixed-coefficient one:
# which coefficients the user seeded, at what value, under which label. What is
# NOT seeded matters as much as what is -- a coefficient absent from `idx` keeps
# its default start, which for a rate intercept is the data-derived warm start.
new_initial_spec <- function(idx, values, names) {
  idx <- as.integer(idx)
  values <- as.numeric(values)
  names <- as.character(names)
  if (length(idx) != length(values) || length(idx) != length(names)) {
    cli::cli_abort(
      "A starting-value contract needs one value and one term label per
       position ({length(idx)} position{?s}, {length(values)} value{?s},
       {length(names)} label{?s}).",
      .internal = TRUE
    )
  }
  if (length(idx) == 0) {
    cli::cli_abort(
      "A starting-value contract cannot be empty; use {.code NULL} when no
       coefficient is seeded.",
      .internal = TRUE
    )
  }
  if (anyNA(values)) {
    cli::cli_abort(c(
      "A seeded coefficient needs a value.",
      "x" = "Missing value for term{?s}: {.code {names[is.na(values)]}}."
    ))
  }
  structure(
    list(idx = idx, values = values, names = names),
    class = "goldfishCoefInit"
  )
}

is_initial_spec <- function(x) inherits(x, "goldfishCoefInit")

# Match the names of a user-supplied vector against the coefficient labels a fit
# renders (the `tidy()` / `coef()` names), returning their positions. A name
# that matches nothing aborts listing what was available: a typo that silently
# seeded or fixed nothing is precisely the failure the by-name surface exists to
# prevent. `candidates` restricts the match to a subset of positions, so
# `offset_coef` can be matched against the offset terms alone.
match_coef_labels <- function(
  wanted,
  coef_labels,
  arg,
  candidates = NULL,
  names = NULL
) {
  pool <- if (is.null(candidates)) seq_along(coef_labels) else candidates
  # With the effect description in hand, resolve through the one matcher every
  # user-facing term argument uses, so a name that selects a term in the
  # diagnostics selects the same term here. Without it -- callers that hold
  # only the labels -- the `coef()` labels remain the vocabulary, which is what
  # they were before the matcher existed.
  if (!is.null(names)) {
    idx <- resolve_term_index(wanted, names, arg)
    outside <- setdiff(idx, pool)
    if (length(outside) > 0) {
      cli::cli_abort(c(
        "{cli::qty(length(outside))}{.arg {arg}} names {?a term/terms} outside
         the set it may name.",
        "x" = "Not available here: {.val {coef_labels[outside]}}.",
        "i" = "Available: {.val {coef_labels[pool]}}."
      ))
    }
    if (anyDuplicated(wanted)) {
      cli::cli_abort(c(
        "{.arg {arg}} names the same coefficient more than once.",
        "x" = "Repeated: {.code {unique(wanted[duplicated(wanted)])}}."
      ))
    }
    return(idx)
  }
  idx <- pool[match(wanted, coef_labels[pool])]
  if (anyNA(idx)) {
    unknown <- wanted[is.na(idx)]
    cli::cli_abort(c(
      "{cli::qty(length(unknown))}{.arg {arg}} names {?a coefficient/
       coefficients} this model does not have.",
      "x" = "Unknown: {.code {unknown}}.",
      "i" = "Available: {.code {coef_labels[pool]}}."
    ))
  }
  if (anyDuplicated(wanted)) {
    cli::cli_abort(c(
      "{.arg {arg}} names the same coefficient more than once.",
      "x" = "Repeated: {.code {unique(wanted[duplicated(wanted)])}}."
    ))
  }
  idx
}

# Resolve user-supplied starting values into a contract. Two forms are accepted:
# the full-length unnamed vector, which seeds every coefficient (including the
# intercept, so a rate model's data-derived warm start is deliberately
# replaced), and a named vector matched against the coefficient labels, which
# seeds only the coefficients it names and leaves every other one at its
# default. A partial-length unnamed vector is the counting mistake the named
# form exists to end, so it aborts rather than being padded.
resolve_initial_parameters <- function(
  initial_parameters,
  coef_labels,
  n_params,
  broadcast = FALSE,
  names = NULL
) {
  if (is.null(initial_parameters)) {
    return(NULL)
  }
  # Broadcasting across the processes of one specification: a name is meant for
  # whichever processes carry that coefficient, so a process without it seeds
  # nothing rather than rejecting the name. Whether a name reached *some*
  # process is checked once, across all of them.
  if (broadcast && !is.null(names(initial_parameters))) {
    # A broadcast name is meant for whichever processes carry that term, so a
    # process without it drops the name rather than aborting. Recognized under
    # any spelling the process answers to, or a name a user read off one
    # process's summary would silently reach none of them.
    known <- if (is.null(names)) {
      coef_labels
    } else {
      unlist(term_spellings(names), use.names = FALSE)
    }
    initial_parameters <- initial_parameters[
      names(initial_parameters) %in% known
    ]
    if (length(initial_parameters) == 0) {
      return(NULL)
    }
  }
  supplied_names <- names(initial_parameters)
  if (is.null(supplied_names) || !any(nzchar(supplied_names))) {
    if (length(initial_parameters) != n_params) {
      cli::cli_abort(c(
        "An unnamed {.arg initial_parameters} must supply one value per
         coefficient.",
        "x" = "The model has {n_params} coefficient{?s} but
               {.arg initial_parameters} has {length(initial_parameters)}
               value{?s}.",
        "i" = "Name the values to seed only some of them, e.g.
               {.code initial_parameters = c({coef_labels[[1]]} = 0.5)}."
      ))
    }
    return(new_initial_spec(
      seq_len(n_params),
      initial_parameters,
      coef_labels
    ))
  }
  if (!all(nzchar(supplied_names))) {
    cli::cli_abort(c(
      "{.arg initial_parameters} must be either fully named or fully unnamed.",
      "x" = "{sum(!nzchar(supplied_names))} value{?s} {?is/are} unnamed.",
      "i" = "Available coefficient{?s}: {.code {coef_labels}}."
    ))
  }
  idx <- match_coef_labels(
    supplied_names,
    coef_labels,
    "initial_parameters",
    names = names
  )
  new_initial_spec(idx, unname(initial_parameters), coef_labels[idx])
}

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

# Which of the `n_params` coefficients the contract holds, as one logical per
# coefficient -- the form a per-term table wants, as against the positions the
# estimation loops want.
fixed_spec_mask <- function(fixed_spec, n_params) {
  if (is.null(fixed_spec)) {
    return(NULL)
  }
  seq_len(n_params) %in% fixed_spec$idx
}


# Assemble the fixed-coefficient contract at the single point where term names
# and coefficient positions are both known. Three sources fix a coefficient:
# `offset()` terms (fixed at the value the formula carries, `coef =`, or at
# `offset_coef`, aligned by formula order), interaction operand-only terms (kept
# in the design but held out of estimation by fixing at 0 — a 0 coefficient
# contributes 0 * stat, i.e. the column is excluded from the model while
# A formula with no effect term carries nothing to estimate and nothing to
# evaluate, and the preprocessing builders below assume at least one effect
# throughout -- they fail on zero-length dimensions rather than on a stated
# rule, so this runs ahead of them.
#
# A term held at a fixed value still counts: an all-fixed model estimates
# nothing but evaluates its likelihood, which is a supported use.
#
# The reason differs by risk set, and so does the message. Where the likelihood
# is Poisson an intercept alone is a well-defined baseline rate against elapsed
# time, and goldfish is declining a model it could in principle fit; saying it
# is unidentified there would be false and would send the reader looking for a
# statistical error. Everywhere else a constant statistic cancels in the
# normalization, which is a property of the likelihood.
abort_if_no_effect_terms <- function(
  rhs_names,
  parsed_formula,
  spec,
  call = rlang::caller_env()
) {
  n_terms <- length(rhs_names) + length(parsed_formula$interactions)
  if (n_terms > 0L) {
    return(invisible(NULL))
  }

  sub_model <- spec$sub_model
  if (identical(behavior_likelihood(spec), "poisson")) {
    cli::cli_abort(
      c(
        "A model needs at least one effect term.",
        "x" = "This formula carries none.",
        "i" = "On {.val {sub_model}} an intercept alone is a well-defined
               baseline rate, but goldfish does not fit a formula with no
               effect.",
        "i" = "Add an effect term."
      ),
      call = call
    )
  }
  cli::cli_abort(
    c(
      "A model needs at least one effect term.",
      "x" = "This formula carries none.",
      "i" = "An intercept would not serve on {.val {sub_model}}: a statistic
             constant across the alternatives cancels in the risk-set
             normalization, so it identifies nothing.",
      "i" = "Add an effect that varies across the alternatives."
    ),
    call = call
  )
}

# retained for downstream), and the superseded positional `fixed_parameters`
# vector, which is converted here so the wire carries one encoding only. An
# effect at rhs position j maps to coefficient j (+1 when the intercept is
# prepended); interaction columns are estimated.
# Returns `NULL` when no coefficient is fixed. A constant-across-alternatives
# offset in choice cancels in the softmax, so it warns rather than aborts.
assemble_fixed_parameters <- function(
  parsed_formula,
  rhs_names,
  has_intercept,
  model,
  sub_model,
  fixed_parameters,
  offset_coef,
  coef_labels = NULL
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
  # The value each offset term carries in the formula itself, NA where it does
  # not carry one.
  in_formula <- unlist(parsed_formula$offset_coef_parameter)
  if (is.null(in_formula)) {
    in_formula <- rep(NA_real_, length(rhs_names))
  }
  in_formula <- in_formula[is_offset]

  if (length(offset_positions) == 0 && !is.null(offset_coef)) {
    cli::cli_abort(c(
      "{.arg offset_coef} was supplied but the formula has no
       {.fn offset} terms.",
      "i" = "Wrap a term in {.fn offset} to fix its coefficient."
    ))
  }
  # One value per offset term, taken from the formula where it carries one and
  # from `offset_coef` otherwise -- aligned to the offset terms by formula order
  # when unnamed, matched to them by coefficient label when named.
  offset_values <- in_formula
  if (!is.null(offset_coef)) {
    supplied_names <- names(offset_coef)
    if (!is.null(supplied_names) && any(nzchar(supplied_names))) {
      if (!all(nzchar(supplied_names))) {
        cli::cli_abort(c(
          "{.arg offset_coef} must be either fully named or fully unnamed.",
          "x" = "{sum(!nzchar(supplied_names))} value{?s} {?is/are} unnamed.",
          "i" = "Unnamed values align to the {.fn offset} terms in formula
                 order."
        ))
      }
      at <- match(
        match_coef_labels(
          supplied_names,
          coef_labels,
          "offset_coef",
          candidates = offset_positions
        ),
        offset_positions
      )
    } else {
      if (length(offset_coef) != length(offset_positions)) {
        cli::cli_abort(c(
          "{.arg offset_coef} must supply one value per {.fn offset} term.",
          "x" = "The formula has {length(offset_positions)} offset term{?s}
                 ({.code {labels[offset_positions]}}) but {.arg offset_coef}
                 has {length(offset_coef)} value{?s}.",
          "i" = "Set it via {.code set_algorithm_newton(offset_coef = ...)}."
        ))
      }
      at <- seq_along(offset_positions)
    }
    conflicted <- at[!is.na(offset_values[at])]
    if (length(conflicted) > 0) {
      cli::cli_abort(c(
        "A fixed coefficient cannot come from two sources.",
        "x" = "{.code {labels[offset_positions[conflicted]]}} {?has/have} a
               {.arg coef} value in the formula and {?a value/values} in
               {.arg offset_coef}.",
        "i" = "Keep one: the formula's {.code coef = } or
               {.code set_algorithm_newton(offset_coef = ...)}."
      ))
    }
    offset_values[at] <- unname(offset_coef)
  }
  if (anyNA(offset_values)) {
    unvalued <- is.na(offset_values)
    cli::cli_abort(c(
      "Every {.fn offset} term needs a fixed coefficient value.",
      "x" = "No value for {.code {labels[offset_positions[unvalued]]}}.",
      "i" = "Supply it in the formula as {.code offset(term, coef = value)} or
             via {.code set_algorithm_newton(offset_coef = ...)}."
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
  values[offset_positions] <- offset_values

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
