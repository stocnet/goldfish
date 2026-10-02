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
