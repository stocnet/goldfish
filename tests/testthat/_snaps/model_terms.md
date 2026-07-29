# an unrecognized term names the helper

    Code
      estimate_wrapper(depNetwork ~ 1 + indeg + outdeg, model = "DyNAM", sub_model = "rate",
      data = dataTest, control_algo = set_algorithm_newton(initial_parameters = c(
        bogus = 0.5)))
    Condition
      Error in `match_coef_labels()`:
      ! `initial_parameters` names a term this model does not have.
      x Unknown: "bogus".
      i Available: "Intercept", "indeg/networkState", and "outdeg/networkState".
      i Search them with `model_terms(fit, pattern = )`.

# the matcher resolves positions and refuses impossible ones

    Code
      resolve_term_index(9L, fit$names, "effects")
    Condition
      Error:
      ! `effects` must give positions this model has.
      x It must lie in 1:4.
      i See `model_terms()` for the positions and their names.

---

    Code
      resolve_term_index(0L, fit$names, "effects")
    Condition
      Error:
      ! `effects` must give positions this model has.
      x It must lie in 1:4.
      i See `model_terms()` for the positions and their names.

# an ambiguous term is offered a resolution

    Code
      resolve_term_index("indeg/networkState", collided, "effect")
    Condition
      Error:
      ! `effect` matches 2 terms of this model.
      x "indeg/networkState" is carried by more than one term.
      i Name one of "ideg_networ" and "ideg_networ_1", or give its position (2 and 4).
      i See `model_terms()` for every term and every name it answers to.

# model_terms refuses what is not a fit

    Code
      model_terms(depNetwork)
    Condition
      Error in `model_terms()`:
      ! `model_terms()` needs a fitted goldfish model.
      x `x` is a <dependent.goldfish> object.
      i Fit one with `estimate_dynam()` or `estimate_rem()`.

