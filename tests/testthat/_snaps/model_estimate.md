# probabilities guardrail warns with the estimated footprint

    Code
      warn_probabilities_footprint(prep, list(risk_set = list(axis = "dyad")))
    Condition
      Warning:
      ! Storing per-event probabilities for 100 events over a risk set of size 100 will use about 78.1 Kb.
      i For scalable diagnostics request "ranks" or "margins" instead of "probabilities".

---

    Code
      warn_probabilities_footprint(prep, list(risk_set = list(axis = "sender")))
    Condition
      Warning:
      ! Storing per-event probabilities for 100 events over a risk set of size 10 will use about 7.8 Kb.
      i For scalable diagnostics request "ranks" or "margins" instead of "probabilities".

---

    Code
      warn_probabilities_footprint(prep, list(risk_set = list(axis = "receiver_given_sender")))
    Condition
      Warning:
      ! Storing per-event probabilities for 100 events over a risk set of size 10 will use about 7.8 Kb.
      i For scalable diagnostics request "ranks" or "margins" instead of "probabilities".

# backends without an opportunity list redirect to r

    Code
      invisible(estimate_wrapper(depNetwork ~ inertia + recip, model = "DyNAM",
      sub_model = "choice", data = dataTest, control_algo = set_algorithm_newton(
        backend = "cpp"), control_prep = set_preprocessing(opportunities_list = opportunities)))
    Condition
      Warning:
      The `opportunities_list` argument of `set_preprocessing()` is deprecated as of goldfish 1.8.6.
      i Use the `support_constraint` argument of `estimate_dynam()` / `make_specification()` instead.
      Warning:
      `backend = "cpp"` does not support `opportunities_list`.
      i Estimating with `backend = "r"` instead.

