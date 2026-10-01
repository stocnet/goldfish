# Per-event likelihood pins: one engine pass at a fixed parameter vector, on
# each backend, reduced to the numbers a refactor of the likelihood site must
# not move. Read by test-likelihood_pins.R against the references under
# `_references/likelihood_v1/` (generator and README there).

# Every opt-in component the engines can produce, so a pass exercises every
# reduction the kernels and the r backend carry.
likelihood_pin_needs <- function() {
  list(
    scores = TRUE,
    ranks = TRUE,
    margins = TRUE,
    total_rate = TRUE,
    probabilities = TRUE,
    availability = TRUE,
    conditional_scores = TRUE,
    information_trace = TRUE
  )
}

# The Fisheries build warns about its composition-change events by design; the
# pins are about the likelihood, so the data constructors run quietly.
likelihood_pin_data <- function() {
  suppressWarnings(list(
    social_evolution = baselines_social_evolution_data(),
    fisheries = baselines_fisheries_data()
  ))
}

likelihood_pin_prep <- function(entry, data_list) {
  sub_model <- entry$sub_model
  if (is.null(sub_model)) {
    sub_model <- if (identical(entry$model, "REM")) "rate" else "choice"
  }
  suppressWarnings(suppressMessages(compute_statistics(
    entry$formula,
    model = entry$model,
    sub_model = sub_model,
    data = data_list[[entry$dataset]],
    output = "preprocessed"
  )))
}

# A per-event probability element is a vector over the actors of one axis or a
# whole n1 x n2 grid; at 84 actors and 439 intervals the grids alone are 25 MB
# per backend, too large to freeze. Three fixed projections per interval -- the
# total and two deterministic oscillating weightings -- move with any single
# entry, so a 1e-10 agreement on them is a 1e-10 agreement on the element.
# Values only: the element's shape is asserted where it is specified
# (test-backend_parity.R).
likelihood_pin_project <- function(probabilities) {
  t(vapply(
    probabilities,
    function(p) {
      p <- as.numeric(p)
      k <- seq_along(p)
      c(sum(p), sum(p * sin(k)), sum(p * cos(1.7 * k)))
    },
    numeric(3)
  ))
}

# One pass, reduced: every component flattened to a plain numeric vector under
# its engine name (each backend is pinned against itself, so the two engine
# vocabularies never meet), the probability list projected as above. The empty
# placeholders a backend returns for components its family lacks are kept, so
# a component appearing or vanishing is seen too.
likelihood_pin_pass <- function(prep, at, backend) {
  res <- evaluate_engine_once(
    spec = prep$model_spec,
    prep = prep,
    pars = at,
    backend = backend,
    needs = likelihood_pin_needs()
  )
  probability_field <- if (identical(backend, "r")) {
    "pMatrix"
  } else {
    "event_probabilities"
  }
  out <- lapply(
    res[setdiff(names(res), probability_field)],
    function(x) as.numeric(unlist(x, use.names = FALSE))
  )
  out$probability_projection <- likelihood_pin_project(res[[probability_field]])
  out
}
