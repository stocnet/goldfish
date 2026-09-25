# Fixtures for the ABEM loop: a contract-conformant stub augmenter, an analytic
# stub evaluator behind the evaluator contract, and a scripted optimizer.
#
# The analytic evaluator treats each pooled sequence's `data` as a numeric
# vector mu_i and scores it as log L_i(theta) = -sum((theta - mu_i)^2) / 2, so
# the score is mu_i - theta and the Fisher information the identity. Q, its
# standard error, and every accept/grow/stop transition are then closed-form,
# and a loop test runs in milliseconds with no preprocessing.

# A stub augmenter over a fixed candidate list: draw `draw_index` k returns
# candidate ((k - 1) mod n) + 1 with that candidate's log proposal density
# (`log_proposal` is recycled over the candidates), so the pool is
# deterministic and needs no RNG. `init()` records every proposal
# evaluator and theta the loop injects, and `restarted` optionally flags draws
# as reject-and-restart ones.
new_stub_augmenter <- function(
  candidates,
  log_proposal = -log(length(candidates)),
  restarted = FALSE
) {
  state <- new.env(parent = emptyenv())
  state$inits <- list()
  state$draws <- list()
  list(
    init = function(proposal_evaluator, theta) {
      state$inits[[length(state$inits) + 1L]] <- list(
        proposal_evaluator = proposal_evaluator,
        theta = theta
      )
      invisible(NULL)
    },
    draw = function(iteration, draw_index, theta) {
      state$draws[[length(state$draws) + 1L]] <- list(
        iteration = iteration,
        draw_index = draw_index,
        theta = theta
      )
      log_proposal <- rep_len(log_proposal, length(candidates))
      lapply(draw_index, function(k) {
        j <- (k - 1L) %% length(candidates) + 1L
        list(
          data = candidates[[j]],
          log_proposal = log_proposal[[j]],
          restarted = restarted
        )
      })
    },
    state = state
  )
}

analytic_loglik <- function(mu, theta) -sum((theta - mu)^2) / 2

local_analytic_evaluator <- function(env = parent.frame()) {
  local_mocked_bindings(
    build_sequence_pool = function(
      sub_models,
      sequences,
      theta_length,
      ...
    ) {
      structure(
        list(
          sub_models = sub_models,
          sequences = lapply(sequences, function(s) {
            list(data = s$data, reference = s$reference)
          }),
          theta_length = as.integer(theta_length),
          map_fn = lapply
        ),
        class = c("goldfishDynesPool", "list")
      )
    },
    evaluate_sequence_pool = function(
      pool,
      theta,
      what = c("loglik", "score", "fisher"),
      ...
    ) {
      mu <- lapply(pool$sequences, function(s) s$data)
      p <- length(theta)
      out <- list()
      if ("loglik" %in% what) {
        out$loglik <- vapply(mu, analytic_loglik, numeric(1), theta = theta)
      }
      if ("score" %in% what) {
        out$score <- do.call(rbind, lapply(mu, function(m) m - theta))
      }
      if ("fisher" %in% what) {
        out$fisher <- lapply(mu, function(m) diag(p))
      }
      out$degenerate <- rep(FALSE, length(mu))
      out
    },
    make_proposal_evaluator = function(sub_models, theta, spec = NULL, ...) {
      function(candidate_seq, rate_queries = NULL) {
        list(loglik = analytic_loglik(candidate_seq, theta), rates = NULL)
      }
    },
    .env = env
  )
}

# Replace the M-step with a scripted proposal: `step_fn(theta)` is the theta'
# the optimizer returns from the last accepted theta.
local_scripted_optimizer <- function(step_fn, env = parent.frame()) {
  local_mocked_bindings(
    optimize_abem_sgd = function(theta, score_fn, weights, ...) {
      list(
        parameters = step_fn(theta),
        iterations = 1L,
        converged = TRUE,
        gradient_norm = 0
      )
    },
    .env = env
  )
}

# A loop control over the analytic fixtures: small pools, the defaults
# otherwise.
analytic_em_control <- function(...) {
  set_algorithm_em(
    n_sequences = 4L,
    augmenter = set_augmenter_options(routine = "random"),
    ...
  )
}

analytic_sub_models <- function() {
  list(list(theta_index = 1L, fixed_only = FALSE))
}

local_abem_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# Four actors, all present, one directed `calls` layer, and a two-plateau pin
# over the wave periods [0, 10) and [10, 20]: log-hazard -1, then -2. The
# default path's event at t = 10 closes the interval (6, 10], so no interval
# crosses the interior boundary.
abem_two_wave_data <- function(calls_time = c(1, 3, 6, 10, 12, 15)) {
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(calls = "increment"),
      directed = c(calls = TRUE),
      observation = c(calls = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:4),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = data.frame(
      from = c(1L, 2L, 1L, 3L, 4L, 1L),
      to = c(2L, 1L, 2L, 4L, 3L, 2L),
      time = calls_time,
      layer = "calls"
    )
  )
}

abem_pinned_submodel <- function() {
  new_pool_submodel(
    calls ~ 1,
    model = "DyNAM",
    sub_model = "rate",
    theta_index = integer(0),
    pinned_rate = make_intercept_only_rate(
      c(-1, -2),
      model_type = "DyNAM-M-Rate",
      wave_times = c(0, 10, 20)
    )
  )
}

# Three orderings of one 30-event stream among five actors: the base order and
# two with a few adjacent events swapped. A choice model scores the order, so
# the three likelihoods differ.
abem_toy_candidates <- function() {
  ties <- withr::with_seed(1, {
    from <- sample.int(5L, 30L, replace = TRUE)
    to <- vapply(from, function(f) sample(setdiff(1:5, f), 1L), integer(1))
    data.frame(from = from, to = to)
  })
  one_order <- function(order) {
    list(
      info = list(
        name = "toy",
        focal = "calls",
        update = c(calls = "increment"),
        directed = c(calls = TRUE),
        observation = c(calls = "event")
      ),
      nodes = data.frame(
        label = paste0("N", 1:5),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = data.frame(
        from = ties$from[order],
        to = ties$to[order],
        time = seq_len(30L),
        layer = "calls"
      )
    )
  }
  swap <- function(order, at) {
    for (i in at) {
      order[c(i, i + 1L)] <- order[c(i + 1L, i)]
    }
    order
  }
  base <- seq_len(30L)
  list(
    one_order(base),
    one_order(swap(base, c(5L, 12L, 20L))),
    one_order(swap(base, c(8L, 16L, 25L)))
  )
}
