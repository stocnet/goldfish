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

