# Fixtures for the estimate_dynes() surface: joint specifications to
# decompose, stub augmenters that draw through the injected RNG substream, and
# a mocked parallel backend that runs in-process but returns its results in
# reverse arrival order, so a reduction that trusted arrival order would show.

# A raw (fully specified, so completion adds nothing) joint specification with
# seven free coefficients and one offset.
dynes_surface_spec <- function() {
  data <- rq_tied_data()
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
dynes_flavored_data <- function(seed = 3L) {
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
        observation = c(calls = "event", emails = "event")
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
    .env = env
  )
  calls
}
