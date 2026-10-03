# The ordinal REM on two node sets. Senders and receivers are different
# actors, so the grid has no self-dyads and a cell (i, i) is an ordinary
# dyad. Every backend has to give the per-event log-likelihood of a softmax
# over all the available cells; `twomode_rem_ordered_oracle()` computes it by
# hand on the estimable two-mode fixture, whose only effect, `inertia`,
# starts at zero and counts the dyad's earlier events.

twomode_rem_ordered_data <- function(variant) {
  data <- make_stocnet_fixture_twomode_estimable()
  absent <- switch(
    variant,
    plain = NULL,
    sender_absent = list(node = 1L, window = c(1.5, 3.5)),
    receiver_absent = list(node = 5L, window = c(7.5, 9.5))
  )
  if (!is.null(absent)) {
    leaves <- data.frame(
      time = absent$window,
      node = absent$node,
      var = "active",
      stringsAsFactors = FALSE
    )
    leaves$value <- list(list(FALSE), list(TRUE))
    data$nodes$active <- TRUE
    data$changes <- rbind(data$changes, leaves)
    data$changes <- data$changes[order(data$changes$time), ]
  }
  data
}

twomode_rem_ordered_oracle <- function(data, inertia) {
  n_senders <- 4L
  n_receivers <- 3L
  active <- rep(TRUE, n_senders + n_receivers)
  counts <- matrix(0, n_senders, n_receivers)
  pending <- data$changes[data$changes$var == "active", ]
  ties <- data$ties
  log_likelihood <- numeric(nrow(ties))
  for (e in seq_len(nrow(ties))) {
    arrived <- pending$time < ties$time[e]
    for (k in which(arrived)) {
      active[pending$node[k]] <- pending$value[[k]][[1]]
    }
    pending <- pending[!arrived, , drop = FALSE]
    predictor <- inertia * (counts > 0)
    at_risk <- outer(
      active[seq_len(n_senders)],
      active[n_senders + seq_len(n_receivers)],
      `&`
    )
    sender <- ties$from[e]
    receiver <- ties$to[e] - n_senders
    log_likelihood[e] <- predictor[sender, receiver] -
      log(sum(exp(predictor[at_risk])))
    counts[sender, receiver] <- counts[sender, receiver] + 1
  }
  log_likelihood
}

test_that("the two-mode ordinal REM keeps the (i, i) dyads on every backend", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  inertia <- 0.8
  needs <- list(
    scores = TRUE,
    ranks = FALSE,
    margins = FALSE,
    total_rate = FALSE,
    probabilities = FALSE,
    availability = FALSE,
    conditional_scores = FALSE,
    information_trace = FALSE
  )
  for (variant in c("plain", "sender_absent", "receiver_absent")) {
    data <- twomode_rem_ordered_data(variant)
    prep <- suppressWarnings(suppressMessages(compute_statistics(
      membership ~ inertia,
      model = "REM",
      sub_model = "rate_ordered",
      data = as_goldfish(data),
      output = "preprocessed"
    )))
    oracle <- twomode_rem_ordered_oracle(data, inertia)
    for (backend in c("r", "cpp", "gather")) {
      pass <- evaluate_engine_once(
        prep$model_spec,
        prep,
        inertia,
        backend,
        needs
      )
      expect_equal(
        as.numeric(normalize_engine_pass(pass, backend)$interval_loglik),
        oracle,
        tolerance = 1e-10,
        info = paste(variant, backend)
      )
    }
  }
})
