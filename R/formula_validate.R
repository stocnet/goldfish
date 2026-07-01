# Per-(model, sub_model) main-effect validity (design D3).
#
# A single local rule table encoding the softmax-identification rule: an effect
# is a valid *main* effect only if its statistic varies on the axis the
# sub_model estimates over; otherwise it is constant across the compared
# alternatives/units and cancels, so it is not identified. Rejected as *main*
# effects (but permitted inside interaction terms once those land, task 2.5):
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
# This local table mirrors the design D3 identification matrix; it is intended to
# be superseded by `effect-term-registry` metadata (task 7.1). It runs after the
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
# cannot even be computed. Today that is only `global` in the DyNAM choice
# sub-models (there is no `init_DyNAM_choice.global`).
unavailable_variations <- function(model, sub_model) {
  if (
    model %in%
      c("DyNAM", "DyNAMi") &&
      sub_model %in% c("choice", "choice_coordination")
  ) {
    return("global")
  }
  character(0)
}

# Variation axes that ARE computable (available via compute_stats() as design
# columns for interactions / random effects, design D3) but are not identified
# as bare main effects, so they are rejected only when actually estimating.
unidentified_variations <- function(model, sub_model) {
  is_dynam <- model %in% c("DyNAM", "DyNAMi")
  # DyNAM choice: ego-perspective columns are constant across the receiver
  # alternatives (`global` is caught earlier as unavailable).
  if (is_dynam && sub_model %in% c("choice", "choice_coordination")) {
    return("ego")
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

# Validate the parsed main effects against the D3 rule table, aborting once with
# every violation. `effect_names` / `effect_types` are aligned per-term vectors
# (effect name and resolved `type`, `""` when absent), typically
# `vapply(rhs_names, "[[", character(1), 1)` and the parser's `type_parameter`.
# `estimating = FALSE` (preprocessing) checks only the unavailable effects, so
# computable-but-unidentified columns can still be produced (design D3).
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
    "i" = "These effects are computable (e.g. via {.fn compute_stats}) but are
           not identified as bare main effects; they will be usable through
           interaction terms in a future release. Use a rate sub-model (or add
           the time intercept) to estimate a global main effect."
  ))
}
