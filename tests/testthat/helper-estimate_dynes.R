# Fixtures for the estimate_dynes() surface: joint specifications to
# decompose, stub augmenters that draw through the injected RNG substream, and
# a mocked parallel backend that runs in-process but returns its results in
# reverse arrival order, so a reduction that trusted arrival order would show.

# A raw (fully specified, so completion adds nothing) joint specification with
# seven free coefficients and one offset. `calls` panel-observed makes it a
# modeled panel process, which estimate_dynes() requires; declared "event",
# friendship is the only panel layer and only a covariate, so nothing is latent.
dynes_surface_spec <- function(calls = "panel") {
  data <- rq_tied_data(calls)
  make_joint_specification(
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ inertia + offset(tie(friendship), coef = -0.5) + tie(emails),
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg,
      choice = ~inertia,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

dynes_multiwave_data <- function(friendship = "panel") {
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(friendship = "increment", calls = "increment"),
      directed = c(friendship = TRUE, calls = TRUE),
      observation = c(friendship = friendship, calls = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(
      data.frame(
        from = c(1L, 2L, 3L, 4L),
        to = c(2L, 3L, 4L, 5L),
        time = c(1, 2, 3, 4),
        layer = "friendship"
      ),
      data.frame(
        from = c(1L, 2L, 3L, 4L, 5L),
        to = c(2L, 3L, 4L, 5L, 1L),
        time = c(1, 2, 3, 4, 5),
        layer = "calls"
      )
    )
  )
}

# A completed joint specification carrying every fid kind the decomposition
# meets: a modeled-panel choice with an offset, an authored timed rate (its
# intercept free), a completed rate pinned on two wave periods, and a completed
# uniform choice. Every friendship event creates a new tie, so the default
# inertia column is always zero and the friendship choice's information matrix
# is singular: a test that evaluates the likelihood at theta passes a
# `friendship_choice` with a varying free effect.
dynes_multiwave_spec <- function(
  friendship_choice = ~ inertia + offset(recip, coef = -0.5)
) {
  suppressWarnings(complete_generative_spec(
    dynes_pinned_raw_spec(friendship_choice),
    wave_times = c(0, 2, 5)
  ))
}

# The raw joint specification inside dynes_multiwave_spec(): estimate_dynes()
# completes it at entry, pinning the friendship rate and supplying a uniform
# calls choice, so three coefficients are free.
dynes_pinned_raw_spec <- function(
  friendship_choice = ~ inertia + offset(recip, coef = -0.5)
) {
  events <- dynes_multiwave_data("event")
  make_joint_specification(
    make_specification(
      choice = friendship_choice,
      layer = "friendship",
      model = "DyNAM",
      data = events
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      layer = "calls",
      model = "DyNAM",
      data = events
    ),
    data = dynes_multiwave_data("panel")
  )
}

# A two-layer sequence for the flavored joint likelihood: `calls` carries the
# creation/dissolution flavors add_flavor() stamps from the update values, and
# `emails` three plain events strictly inside the calls span. An emails event
# then only cuts a calls-rate interval in two: no calls statistic changes there
# and the window still ends at the last calls event, so each calls fid scores
# exactly as it does on the ordinary flavored path. A different `seed` is a
# different draw on the same actors, the shape an augmented sequence has.
# `observation = "panel"` makes calls a modeled panel process.
dynes_flavored_data <- function(seed = 3L, observation = "event") {
  calls <- flavored_event_stream(n_actors = 8L, n_events = 40L, seed = seed)
  emails <- data.frame(
    from = c(1L, 3L, 5L),
    to = c(2L, 4L, 6L),
    time = c(5.5, 12.5, 30.5),
    layer = "emails",
    weight = 1
  )
  add_flavor(
    list(
      info = list(
        name = "toy",
        focal = "calls",
        update = c(calls = "increment", emails = "increment"),
        directed = c(calls = TRUE, emails = TRUE),
        observation = c(calls = observation, emails = "event")
      ),
      nodes = data.frame(
        label = paste0("N", 1:8),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = rbind(calls, emails)
    ),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

# The flavored calls process (both families, both flavors) beside a plain
# emails choice. Every fid is authored, so no completion is needed to evaluate
# it; estimate_dynes() completes it with a pinned emails rate.
dynes_flavored_spec <- function(data = dynes_flavored_data()) {
  make_joint_specification(
    dynes_flavored_calls_spec(data),
    make_specification(
      choice = ~ tie(calls),
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

dynes_flavored_calls_spec <- function(data = dynes_flavored_data()) {
  make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + outdeg),
    choice = list(creation ~ recip + trans, dissolution ~ recip),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
}

# Draws a scalar near 1 (the analytic evaluator's data) and a widely spread
# log proposal density, both from the RNG the seam injects. The spread makes
# the importance weights degenerate, so the ESS guard redraws once a pool
# persists. Every draw is logged by its (iteration, draw_index) key.
new_keyed_stub_augmenter <- function() {
  state <- new.env(parent = emptyenv())
  state$log <- list()
  list(
    draw = function(iteration, draw_index, theta) {
      lapply(draw_index, function(k) {
        value <- 1 + stats::runif(1) / 10
        log_proposal <- stats::rnorm(1, sd = 20)
        state$log[[paste(iteration, k)]] <- c(value, log_proposal)
        list(data = value, log_proposal = log_proposal)
      })
    },
    state = state
  )
}

# dynes_pinned_raw_spec() with a varying friendship effect, which keeps each
# sequence's information invertible. Its formulas close over this function's
# frame, which nothing writes to afterwards, so a fit whose statistics were
# built on a daemon (from a serialized copy of that frame) stays identical to
# one built in this process.
dynes_varying_raw_spec <- function() {
  dynes_pinned_raw_spec(~ indeg + offset(recip, coef = -0.5))
}

# Draws dynes_pinned_raw_spec() sequences whose friendship events fall at
# sorted uniform times inside their own wave period, (0, 2) or (2, 5), so each
# draw still reaches the observed wave states, with a normal log proposal
# density, all from the RNG the seam injects. Nothing is mocked in the draw or
# in the evaluation it feeds, so it runs unchanged on real mirai daemons.
new_multiwave_augmenter <- function() {
  base <- dynes_multiwave_data()
  friendship <- which(base$ties$layer == "friendship")
  first_wave <- base$ties$time[friendship] <= 2
  list(
    draw = function(iteration, draw_index, theta) {
      lapply(draw_index, function(k) {
        data <- base
        data$ties$time[friendship] <- c(
          sort(stats::runif(sum(first_wave), 0, 2)),
          sort(stats::runif(sum(!first_wave), 2, 5))
        )
        list(data = data, log_proposal = stats::rnorm(1))
      })
    }
  )
}

# Draws dynes_pinned_raw_spec() sequences on the wave grid estimate_dynes()
# infers from its data, (1, 2, 3, 4): the friendship tie stamped at the first
# wave is the initial condition (a history row), and each later one falls at a
# uniform time inside its own wave period, so each draw still reaches the
# observed wave states, with a normal log proposal density, all from the RNG
# the seam injects. Nothing is mocked in the draw or in the evaluation it
# feeds, so it runs unchanged on real mirai daemons.
new_wave_grid_augmenter <- function() {
  base <- dynes_multiwave_data()
  friendship <- which(base$ties$layer == "friendship")
  base$ties$time[friendship[[1L]]] <- NA
  latent <- friendship[-1L]
  list(
    draw = function(iteration, draw_index, theta) {
      lapply(draw_index, function(k) {
        data <- base
        data$ties$time[latent] <- stats::runif(length(latent)) +
          seq_along(latent)
        list(data = data, log_proposal = stats::rnorm(1))
      })
    }
  )
}

# The surface's augmenter slot, pointed at a test augmenter.
local_dynes_augmenter <- function(augmenter, env = parent.frame()) {
  local_mocked_bindings(
    resolve_dynes_augmenter = function(...) augmenter,
    .env = env
  )
}

# Pretend mirai is installed, with daemons that run in this process and hand
# back each result tagged by index in reverse order.
local_reversed_parallel_backend <- function(env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$started <- list()
  calls$maps <- 0L
  local_mocked_bindings(
    dynes_mirai_available = function() TRUE,
    dynes_daemons_start = function(n_workers, blas_threads) {
      calls$started[[length(calls$started) + 1L]] <- c(
        n_workers = n_workers,
        blas_threads = blas_threads
      )
      "mocked"
    },
    dynes_daemons_stop = function(compute) invisible(NULL),
    dynes_parallel_map = function(x, f, compute) {
      calls$maps <- calls$maps + 1L
      rev(lapply(seq_along(x), function(i) {
        list(index = i, value = f(x[[i]]))
      }))
    },
    dynes_broadcast = function(f, args, compute, n_workers) {
      rep(list(do.call(f, args)), n_workers)
    },
    .env = env
  )
  calls
}

# Pretend mirai is installed, with in-process daemons that record what crosses
# the seam: every mapped task function with its inputs (`tasks`), and every
# broadcast function with its arguments (`broadcasts`). A broadcast mutates
# the one in-process resident store once and answers for each of the
# `n_workers` daemons, as everywhere() would.
local_recording_parallel_backend <- function(env = parent.frame()) {
  record <- new.env(parent = emptyenv())
  record$tasks <- list()
  record$broadcasts <- list()
  local_mocked_bindings(
    dynes_mirai_available = function() TRUE,
    dynes_daemons_start = function(n_workers, blas_threads) "recorded",
    dynes_daemons_stop = function(compute) invisible(NULL),
    dynes_parallel_map = function(x, f, compute) {
      record$tasks[[length(record$tasks) + 1L]] <- list(f = f, x = x)
      lapply(seq_along(x), function(i) list(index = i, value = f(x[[i]])))
    },
    dynes_broadcast = function(f, args, compute, n_workers) {
      record$broadcasts[[length(record$broadcasts) + 1L]] <- list(
        f = f,
        args = args
      )
      rep(list(do.call(f, args)), n_workers)
    },
    .env = env
  )
  record
}

# A flavored state panel (friendship) modeled choice-only beside an observed
# calls stream, six actors. Friendship changes are stamped at the waves 5 and
# 10 that first record them; the history rows are the starting network.
# Interval 1 creates (3, 4) and (4, 5) and dissolves (1, 2); interval 2
# creates (5, 6) and dissolves (2, 3).
recipe_state_data <- function() {
  friendship <- data.frame(
    from = c(1L, 2L, 3L, 4L, 1L, 5L, 2L),
    to = c(2L, 3L, 4L, 5L, 2L, 6L, 3L),
    time = c(NA, NA, 5, 5, 5, 10, 10),
    weight = c(1, 1, 1, 1, -1, 1, -1),
    layer = "friendship"
  )
  calls <- data.frame(
    from = c(1L, 2L, 3L, 6L),
    to = c(3L, 4L, 5L, 1L),
    time = c(1, 3, 6, 8),
    weight = 1,
    layer = "calls"
  )
  add_flavor(
    list(
      info = list(
        name = "toy",
        focal = "calls",
        update = c(friendship = "increment", calls = "increment"),
        directed = c(friendship = TRUE, calls = TRUE),
        observation = c(friendship = "panel", calls = "event")
      ),
      nodes = data.frame(
        label = paste0("N", 1:6),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = rbind(friendship, calls)
    ),
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

recipe_state_spec <- function(data = recipe_state_data()) {
  make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# An unflavored friendship panel (empty at 0; ties into N2 and N5 by wave 5,
# more by wave 10) beside calls observed only before wave 5, six actors. The
# calls choice reads indeg(friendship), which is zero for every receiver
# unless the drawn path places a friendship event before a call: the
# per-sequence information is singular on some paths and not on others.
early_calls_data <- function() {
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(friendship = "increment", calls = "increment"),
      directed = c(friendship = TRUE, calls = TRUE),
      observation = c(friendship = "panel", calls = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(
      data.frame(
        from = c(1L, 3L, 4L, 2L, 6L),
        to = c(2L, 2L, 5L, 5L, 5L),
        time = c(5, 5, 5, 10, 10),
        weight = 1,
        layer = "friendship"
      ),
      data.frame(
        from = c(6L, 4L, 1L, 5L),
        to = c(2L, 1L, 4L, 3L),
        time = c(0.5, 1, 2, 3),
        weight = 1,
        layer = "calls"
      )
    )
  )
}

early_calls_spec <- function(data = early_calls_data()) {
  make_joint_specification(
    make_specification(
      choice = ~indeg,
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ indeg(friendship),
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# An unflavored count panel (calls, cumulative counts at waves 5 and 10)
# beside an observed emails stream, four actors: (1, 2) rises 0 -> 2 -> 5 and
# (2, 3) 0 -> 0 -> 1.
recipe_count_data <- function() {
  list(
    info = list(
      name = "toy",
      focal = "emails",
      update = c(calls = "increment", emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = "panel", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:4),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(
      data.frame(
        from = c(1L, 1L, 2L),
        to = c(2L, 2L, 3L),
        time = c(5, 10, 10),
        weight = c(2, 3, 1),
        layer = "calls"
      ),
      data.frame(
        from = c(1L, 3L),
        to = c(3L, 4L),
        time = c(2, 7),
        weight = 1,
        layer = "emails"
      )
    )
  )
}

recipe_count_spec <- function(data = recipe_count_data()) {
  make_joint_specification(
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# --- Count-panel closed-form oracle ------------------------------------------
# A count panel `calls` (waves 5 and 10 from t0 = 0) with exogenous effects
# only, rate `~ 1 + ego(x)` and choice `~ alter(y)`, beside an observed
# `emails` stream with the same formulas that reads no modeled panel layer,
# so its estimates leave the calls oracle untouched. Sender rates are constant
# over the window, so every path between the waves has the same likelihood:
# nothing is missing, and the observed-data likelihood is the Poisson
# likelihood of the dyad counts, with a closed-form MLE and standard errors.

oracle_nodes <- function(n = 12L, seed = 7L) {
  withr::with_seed(seed, {
    data.frame(
      label = paste0("N", seq_len(n)),
      mode = "p",
      x = round(stats::rnorm(n), 2),
      y = round(stats::rnorm(n), 2),
      stringsAsFactors = FALSE
    )
  })
}

oracle_data <- function(nodes, ties = NULL, observation = "panel") {
  if (is.null(ties)) {
    # Placeholder rows so both layers exist before anything is simulated.
    ties <- data.frame(
      from = c(1L, 2L),
      to = c(2L, 1L),
      time = c(5, 1),
      weight = 1,
      layer = c("calls", "emails")
    )
  }
  list(
    info = list(
      name = "oracle",
      focal = "calls",
      update = c(calls = "increment", emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = observation, emails = "event")
    ),
    nodes = nodes,
    ties = ties
  )
}

oracle_spec <- function(data) {
  make_joint_specification(
    make_specification(
      rate = ~ 1 + ego(x),
      choice = ~ alter(y),
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + ego(x),
      choice = ~ alter(y),
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# A simulated run projected onto the panel: the modeled panel layer is
# observed only as its per-dyad change over each between-wave interval,
# stamped at the wave that closes it (an increment row of the cumulative
# count). Events after the last wave are dropped; observed layers keep theirs.
project_to_waves <- function(events, panel_layer, waves, start_time = 0) {
  bounds <- c(start_time, waves)
  ev <- events[events$time > start_time & events$time <= max(waves), ]
  pe <- ev[ev$layer == panel_layer, ]
  pe$wave <- waves[findInterval(pe$time, bounds, left.open = TRUE)]
  agg <- stats::aggregate(
    pe$increment,
    by = list(from = pe$sender, to = pe$receiver, time = pe$wave),
    FUN = sum
  )
  names(agg)[4] <- "weight"
  agg <- agg[agg$weight != 0, ]
  agg <- agg[order(agg$time, agg$from, agg$to), ]
  re <- ev[ev$layer != panel_layer, ]
  list(
    panel = data.frame(
      from = agg$from,
      to = agg$to,
      time = agg$time,
      weight = agg$weight,
      layer = panel_layer
    ),
    events = if (nrow(re) > 0L) {
      data.frame(
        from = re$sender,
        to = re$receiver,
        time = re$time,
        weight = re$increment,
        layer = re$layer
      )
    }
  )
}

# The closed-form MLE. Sender counts are Poisson(T exp(b0 + b1 x_i)); the
# receiver choice is a conditional logit in alter(y), fitted as the Poisson
# regression with sender fixed effects that is its exact equivalent. Event
# times enter only through the compensator, which is path-independent here,
# so the counts are all the likelihood sees.
oracle_mle <- function(panel, nodes, horizon) {
  n <- nrow(nodes)
  counts <- matrix(0, n, n)
  for (r in seq_len(nrow(panel))) {
    counts[panel$from[r], panel$to[r]] <-
      counts[panel$from[r], panel$to[r]] + panel$weight[r]
  }
  sender <- data.frame(n = rowSums(counts), x = nodes$x, t = horizon)
  rate <- stats::glm(
    n ~ x + offset(log(t)),
    family = stats::poisson(),
    data = sender
  )
  dyads <- expand.grid(i = seq_len(n), j = seq_len(n))
  dyads <- dyads[dyads$i != dyads$j, ]
  dyads$n <- counts[cbind(dyads$i, dyads$j)]
  dyads$y <- nodes$y[dyads$j]
  # Senders with no events carry no choice information.
  dyads <- dyads[dyads$i %in% which(sender$n > 0), ]
  choice <- stats::glm(
    n ~ factor(i) + y,
    family = stats::poisson(),
    data = dyads
  )
  est <- c(stats::coef(rate), stats::coef(choice)[["y"]])
  se <- c(
    sqrt(diag(stats::vcov(rate))),
    sqrt(stats::vcov(choice)["y", "y"])
  )
  labels <- c(
    "calls › rate: Intercept",
    "calls › rate: ego/x",
    "calls › choice: alter/y"
  )
  names(est) <- names(se) <- labels
  list(estimate = est, se = se)
}

# The oracle panel from a seeded simulate() of the joint specification:
# the specification to estimate on the projected data, its wave grid, and the
# closed-form calls MLE.
oracle_count_panel <- function(seed = 3L) {
  waves <- c(5, 10)
  nodes <- oracle_nodes()
  generative <- oracle_spec(oracle_data(nodes, observation = "event"))
  truth <- set_parameters(
    generative,
    `calls › rate` = c(-1, 0.5),
    `calls › choice` = 0.8,
    `emails › rate` = c(-1, -0.3),
    `emails › choice` = -0.5
  )
  # The placeholder data end at t = 5, so the run is flagged as passing the
  # observation window; the covariates are constant, so holding them there is
  # exact.
  sim <- suppressWarnings(simulate(
    generative,
    nsim = 1,
    seed = seed,
    coef = truth,
    horizon = max(waves),
    control_sim = set_simulation_guard(max_events = 1e4)
  ))
  projected <- project_to_waves(sim$events, "calls", waves)
  list(
    spec = oracle_spec(
      oracle_data(nodes, rbind(projected$panel, projected$events))
    ),
    wave_times = c(0, waves),
    mle = oracle_mle(projected$panel, nodes, max(waves))
  )
}

# --- Seeded recovery fixture -------------------------------------------------
# The parameter-recovery study's state-panel shape at its second flips level:
# 12 actors, a modeled flavored `friendship` panel (rate `~ 1` and choice
# `~ recip` per flavor) observed at waves 5 and 10 from t0 = 0, beside an
# observed `calls` stream that does not read it. Both friendship rate
# intercepts at -3.2 give about four net, and as many latent, flips per
# interval, so the minimal-flip estimand carries no visible bias there.

recovery_waves <- c(5, 10)

recovery_nodes <- function(n = 12L) {
  data.frame(
    label = paste0("N", seq_len(n)),
    mode = "p",
    stringsAsFactors = FALSE
  )
}

# Starting friendship network of density 0.2, fixed across replicates.
recovery_start_ties <- function(n = 12L, density = 0.2, seed = 11L) {
  withr::with_seed(seed, {
    d <- expand.grid(from = seq_len(n), to = seq_len(n))
    d <- d[d$from != d$to, ]
    d <- d[stats::runif(nrow(d)) < density, ]
  })
  data.frame(
    from = d$from,
    to = d$to,
    time = NA_real_,
    weight = 1,
    layer = "friendship"
  )
}

recovery_data <- function(ties, observation = "panel") {
  add_flavor(
    list(
      info = list(
        name = "recovery",
        focal = "calls",
        update = c(friendship = "increment", calls = "increment"),
        directed = c(friendship = TRUE, calls = TRUE),
        observation = c(friendship = observation, calls = "event")
      ),
      nodes = recovery_nodes(),
      ties = ties
    ),
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

recovery_spec <- function(data) {
  make_joint_specification(
    make_specification(
      rate = list(creation ~ 1, dissolution ~ 1),
      choice = list(creation ~ recip, dissolution ~ recip),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# Generating data: the starting network; one creation on an absent dyad and
# one dissolution on a present one at the horizon, because
# make_specification() derives the flavor support constraint only from a
# layer with a timed flavored row; and calls rows at t0 and at the horizon.
# simulate() opens its clock at the earliest data time whatever its
# `control_prep` says, so the row at t0 is what makes the generating window
# the estimation window.
recovery_generating_data <- function() {
  start <- recovery_start_ties()
  n <- nrow(recovery_nodes())
  present <- paste(start$from, start$to)
  absent <- expand.grid(from = seq_len(n), to = seq_len(n))
  absent <- absent[
    absent$from != absent$to & !paste(absent$from, absent$to) %in% present,
  ][1, ]
  horizon <- max(recovery_waves)
  recovery_data(
    rbind(
      start,
      data.frame(
        from = c(absent$from, start$from[1]),
        to = c(absent$to, start$to[1]),
        time = horizon,
        weight = c(1, -1),
        layer = "friendship"
      ),
      data.frame(
        from = 1L,
        to = 2L,
        time = c(0, horizon),
        weight = 1,
        layer = "calls"
      )
    ),
    observation = "event"
  )
}

# One seeded simulate() pool at the known parameters, projected onto the
# waves. Capped runs are dropped through filter_simulation(), and panels with
# no creation or no dissolution flip (that flavor's rate then has no finite
# MLE) are dropped after projection; both are counted. `first_event` is each
# kept run's earliest event time, `specs` the joint specifications to fit.
recovery_panels <- function(nsim, seed) {
  generative <- recovery_spec(recovery_generating_data())
  truth <- set_parameters(
    generative,
    `friendship › creation › rate` = -3.2,
    `friendship › dissolution › rate` = -3.2,
    `friendship › creation › choice` = 1,
    `friendship › dissolution › choice` = -1,
    `calls › rate` = -0.5,
    `calls › choice` = 0.5
  )
  pool <- simulate(
    generative,
    nsim = nsim,
    seed = seed,
    coef = truth,
    horizon = max(recovery_waves),
    control_sim = set_simulation_guard(max_events = 5e4)
  )
  kept <- unclass(filter_simulation(pool, !capped))
  panels <- lapply(kept, function(run) {
    projected <- project_to_waves(run$events, "friendship", recovery_waves)
    rbind(recovery_start_ties(), projected$panel, projected$events)
  })
  identified <- vapply(
    panels,
    function(ties) {
      w <- ties$weight[ties$layer == "friendship" & !is.na(ties$time)]
      any(w > 0) && any(w < 0)
    },
    logical(1)
  )
  layout <- coef_layout(truth)
  layout <- layout[!is.na(layout$index), ]
  list(
    truth = stats::setNames(
      layout$value,
      paste0(layout$process, ": ", layout$name)
    ),
    specs = lapply(panels[identified], function(ties) {
      recovery_spec(recovery_data(ties))
    }),
    first_event = vapply(kept, function(run) min(run$events$time), numeric(1)),
    capped = nsim - length(kept),
    unidentified = sum(!identified)
  )
}
