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
# uniform choice.
dynes_multiwave_spec <- function() {
  events <- dynes_multiwave_data("event")
  joint <- make_joint_specification(
    make_specification(
      choice = ~ inertia + offset(recip, coef = -0.5),
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
  suppressWarnings(complete_generative_spec(joint, wave_times = c(0, 2, 5)))
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
