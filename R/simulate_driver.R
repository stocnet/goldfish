# =========================================================================== #
# The simulation driver.
#
# One loop over the merged single-clock walk, for every family the package
# models. A replicate opens the walk ONCE and then repeats
#
#   advance -> evaluate every modeled rate -> draw the waiting time and the
#   firing process -> draw the mark -> accept -> inject
#
# so DyNAM, REM and a flavored specification differ in which fids the walk
# carries, never in the shape of the loop. Nothing here re-preprocesses: the
# engines are compiled at open and one injected event updates the shared state
# every engine reads.
#
# The exponential waiting time is exact only while every rate is constant
# between draws, which holds because the driver redraws at every breakpoint the
# handle reports -- an observed covariate row, a node-composition change, a
# window expiry, or a time the parameter provider asked to be called back at.
# An effect that decayed continuously in time would break that condition and
# must bring its own clock.
# =========================================================================== #

# The regimes a process can be in during a run, in the vocabulary the result
# and `regime_of()` report: drawn from its own formula, drawn from an
# auto-supplied zero-parameter default, or replayed from the observed stream.
SIM_REGIMES <- c("modeled", "completed", "anchored-replay")

# --------------------------------------------------------------------------- #
# Running one replicate
# --------------------------------------------------------------------------- #

# `spec` is already generatively complete. Returns a `goldfishSim`.
simulate_replicate <- function(
  spec,
  provider,
  steps,
  replicate,
  times,
  horizon,
  n_events,
  control_sim,
  control_prep,
  call = rlang::caller_env()
) {
  handle <- walk_open(spec, control_prep, call = call)
  n_observed <- sum(handle$schedule$dependent)
  # The window rate estimation integrates exposure over: the last observed
  # event, dependent or exogenous, window expiries excluded, unless the
  # controls set an end. With no target a run covers that same period, so the
  # event count it draws is what the clock produces, not a number fixed in
  # advance. Past it, nothing observed can change the exogenous state.
  window <- resolve_walk_extent(handle$merged, control_prep)
  window_end <- window$end
  if (is.null(n_events) && is.null(horizon)) {
    horizon <- window_end
  }
  max_events <- control_sim$max_events %||% (10L * n_observed)
  window_length <- window$end - window$start
  # A step must move the clock by more than this; NULL leaves the clock
  # unguarded. At zero this is exactly a wait that leaves the clock
  # unchanged.
  clock_floor <- if (!is.null(control_sim$clock_resolution)) {
    control_sim$clock_resolution * window_length
  }
  routing <- simulation_routing(spec, handle)
  map <- routing$map
  rate_fids <- map$fid[map$family == "rate"]
  if (length(rate_fids) == 0L) {
    cli::cli_abort(
      c(
        "Free-running simulation needs a process with a rate.",
        "i" = "A choice-only specification has no clock of its own; simulate
               it with {.code times = \"observed\"}."
      ),
      call = call,
      class = "goldfish_sim_no_clock"
    )
  }

  clock <- steps$clock %||% default_clock(spec)
  mark <- steps$mark %||% default_mark
  accept <- steps$accept %||% default_accept
  evaluate <- steps$evaluate %||% default_evaluate

  dims <- simulation_fid_dims(handle, routing)
  updates <- simulation_mark_updates(spec)
  pinned <- spec$completed_rates %||% list()
  latent <- provider$init(replicate, handle)
  # A run never accepts more events than it proposes, and `max_events` bounds
  # the proposals, so the run's size is known before its first draw.
  collector <- new_event_collector(
    capacity = min(n_events %||% max_events, max_events),
    observed = n_observed
  )
  trajectory <- new_rate_trajectory(
    observed_wait = window_length / n_observed
  )

  t <- handle$current_time
  k <- 0L
  n_drawn <- 0L
  stop_reason <- NULL
  provider_breakpoint <- Inf

  repeat {
    stop_reason <- reached_target(k, t, n_events, horizon)
    if (!is.null(stop_reason)) {
      break
    }
    if (n_drawn >= max_events) {
      stop_reason <- "max_events"
      break
    }

    walk_advance(handle, t, call = call)
    resolved <- provider$at(k, t, handle, latent)
    theta <- provider_parameters(resolved, routing, call)
    latent <- resolved$latent %||% latent
    provider_breakpoint <- resolved$breakpoint %||% Inf

    rates <- rate_values(handle, rate_fids, theta, evaluate, routing, pinned, t)
    total <- sum(vapply(rates, sum, numeric(1)))
    if (!(total > 0)) {
      stop_reason <- "no_rate"
      break
    }

    drawn <- clock(vapply(rates, sum, numeric(1)), t, handle)
    wait <- drawn$wait
    # The next moment at which a rate can change on its own. Drawing past it
    # would extrapolate a constant rate over a state that has already moved, so
    # the draw is discarded and remade there -- the redraw discipline that keeps
    # the competing-exponential draw exact.
    breakpoint <- min(
      walk_next_breakpoint(handle, call = call),
      provider_breakpoint
    )
    if (is.finite(breakpoint) && t + wait > breakpoint) {
      t <- breakpoint
      next
    }
    if (identical(drawn$kind, "breakpoint")) {
      t <- t + wait
      next
    }

    # A wait too small to move a clock this far from zero would stamp every
    # later event with one timestamp, which no continuous-time process draws.
    # The step is measured as the clock records it, not as drawn.
    if (!is.null(clock_floor) && !((t + wait) - t > clock_floor)) {
      stop_reason <- "clock_resolution"
      break
    }
    t <- t + wait
    if (!is.null(horizon) && t > horizon) {
      t <- horizon
      stop_reason <- "horizon"
      break
    }
    record_total_rate(trajectory, t, total, wait)
    if (rate_has_run_away(trajectory, control_sim)) {
      stop_reason <- "rate_trajectory"
      break
    }

    fid <- drawn$fid
    evaluation <- mark_evaluation(
      handle,
      fid,
      theta,
      evaluate,
      dims,
      rates,
      routing,
      updates
    )
    event <- mark(fid, evaluation, handle)
    n_drawn <- n_drawn + 1L
    if (!isTRUE(accept(event, handle))) {
      next
    }
    event$time <- t
    walk_inject(handle, event, call = call)
    k <- k + 1L
    collect_event(collector, event, fid, map, latent)
  }

  new_goldfish_sim(
    events = collector_frame(collector),
    process_map = map,
    times = times,
    capped = stop_reason %in% SIM_GUARD_STOPS,
    stop_reason = stop_reason,
    n_proposals = n_drawn,
    end_time = t,
    window_end = window_end,
    trajectory = rate_trajectory_frame(trajectory),
    latent = collector_latent(collector),
    spec = spec,
    replicate = replicate
  )
}

# --------------------------------------------------------------------------- #
# The default steps
# --------------------------------------------------------------------------- #

# PE. goldfish's own evaluation: the process-state evaluator on a vector theta.
# A caller wanting anything else -- a per-actor deviation, a regime mixture, a
# different link -- supplies its own `evaluate` step rather than teaching this
# one a second case.
default_evaluate <- function(stats, theta, risk, meta) {
  state <- list(
    model_type = meta$model_type,
    stat_mat = stats,
    active_sender = risk$active_sender,
    active_dyad = risk$active_dyad,
    active_dyad_encoding = risk$encoding,
    n_actors1 = meta$n_actors1,
    n_actors2 = meta$n_actors2,
    n_parameters = ncol(stats),
    twomode_or_reflexive = meta$twomode_or_reflexive,
    is_rate = identical(meta$family, "rate"),
    event_sender = meta$sender %||% NA_integer_,
    event_receiver = NA_integer_,
    is_dependent = FALSE,
    timespan = NA_real_
  )
  evaluate_process_state(state, theta)$value
}

# P2. The clock keyed on the specification's behavior. A timed exponential
# process draws the minimum of the competing exponentials, which is one
# exponential at the total rate, and then which process fired in proportion to
# its share.
default_clock <- function(spec) {
  function(rates, t, handle) {
    total <- sum(rates)
    list(
      wait = stats::rexp(1L, total),
      fid = as.integer(names(rates)[sample_index(rates)]),
      kind = "event"
    )
  }
}

# P3. The family's own mark. A DyNAM step has drawn its rate fid, so the sender
# comes from that fid's rate vector and the receiver from the sender's row of
# the paired choice fid -- one row, not the whole matrix. REM draws the dyad
# directly from the fid's own values.
default_mark <- function(fid, evaluation, handle) {
  # A DyNAM rate is a sender block and factorizes: draw the sender, then the
  # receiver from that sender's row alone. A REM rate already ranges over the
  # dyads, so its own values ARE the candidate space and there is no partner to
  # consult -- which is what having no paired choice fid means.
  if (!is.null(evaluation$choice)) {
    sender <- sample_index(evaluation$rate)
    receiver <- sample_index(evaluation$choice(sender))
  } else {
    cell <- sample_index(evaluation$rate)
    n2 <- evaluation$n_actors2
    sender <- ((cell - 1L) %/% n2) + 1L
    receiver <- ((cell - 1L) %% n2) + 1L
  }
  event <- list(
    layer = evaluation$layer,
    sender = sender,
    receiver = receiver,
    flavor = evaluation$flavor
  )
  update <- evaluation$update %||% list(semantics = "increment", value = 1)
  event[[update$semantics]] <- update$value
  event
}

# P4. Accept every drawn event. An endpoint-conditioned augmenter replaces this.
default_accept <- function(event, handle) TRUE

# --------------------------------------------------------------------------- #
# Evaluating a fid through the (possibly supplied) evaluate step
# --------------------------------------------------------------------------- #

# The narrow contract the evaluate step sees: the fid's statistics rows in
# sender-major order, theta as the provider returned it, the live risk set, and
# the small meta a variant needs to index them. `sender` travels in `meta`
# rather than as a pre-sliced matrix: the rows stay the fid's whole candidate
# space in one known order, so a step that needs one ego's block takes it
# (`n_actors2` rows per ego) and a step that couples egos still sees them all.
evaluate_fid <- function(handle, fid, theta, evaluate, sender = NULL) {
  state <- simulation_fid_state(handle, fid)
  map <- handle$process_map
  row <- match(fid, map$fid)
  stats_rows <- state$stat_mat
  meta <- list(
    family = map$family[row],
    model_type = state$model_type,
    n_actors1 = state$n_actors1,
    n_actors2 = state$n_actors2,
    twomode_or_reflexive = state$twomode_or_reflexive,
    sender = sender
  )
  risk <- list(
    active_sender = state$active_sender == 1,
    active_dyad = state$active_dyad,
    encoding = state$active_dyad_encoding
  )
  evaluate(stats_rows, theta, risk, meta)
}

# A fid's state at the live walk. A fid the walk compiled reads its engine. A
# deferred fid has no engine, because its whole family carries no effect, so
# its state is the degenerate one its formula reduces to -- a column of ones
# for an intercept-only rate, no columns at all for an effect-free choice --
# over the risk set of its same-process sibling, which the walk did compile.
#
# A process's rate and choice share one support constraint, and the sibling's
# live mask survives the deferral, so the deferred fid draws inside the same
# support the modeled one does: senders gated by the mask's rows, receivers
# by the drawing sender's row of the mask. Presence alone would let a
# completed choice pick a receiver the constraint excludes.
simulation_fid_state <- function(handle, fid) {
  if (!fid %in% handle$deferred$fid) {
    return(walk_build_state(handle, fid))
  }
  map <- handle$process_map
  row <- match(fid, map$fid)
  sibling <- sibling_fid(map, fid)
  engine <- walk_engine_of_fid(handle, sibling)
  n1 <- engine$n1
  n2 <- engine$n2
  present1 <- engine$presence1
  present2 <- engine$presence2
  support <- walk_live_support(handle, sibling)
  if (identical(map$family[row], "rate")) {
    gate <- if (is.null(support)) {
      TRUE
    } else {
      sender_gate_from_mask(
        support$value,
        support$stored_kind,
        present2,
        n1,
        n2
      )
    }
    return(list(
      model_type = "DyNAM-M-Rate",
      stat_mat = matrix(1, n1, 1L),
      active_sender = present1 & gate,
      active_dyad = rep(TRUE, n1),
      active_dyad_encoding = "alter",
      n_actors1 = n1,
      n_actors2 = 1L,
      twomode_or_reflexive = TRUE
    ))
  }
  receivers <- if (is.null(support)) {
    list(dyad = present2, encoding = "alter")
  } else {
    grid <- support_to_grid(support$value, support$stored_kind, n1, n2)
    list(
      dyad = matrix(present2, n1, n2, byrow = TRUE) & grid,
      encoding = "point"
    )
  }
  list(
    model_type = "DyNAM-M",
    stat_mat = matrix(0, n1 * n2, 0L),
    active_sender = present1,
    active_dyad = receivers$dyad,
    active_dyad_encoding = receivers$encoding,
    n_actors1 = n1,
    n_actors2 = n2,
    # Whether the two sides are different node sets, so a self-loop is a legal
    # alternative rather than the one cell to drop.
    twomode_or_reflexive = isTRUE(engine$model_spec$is_two_mode)
  )
}

# The step and the parameters one fid is evaluated with, by its regime. A
# modeled fid takes the evaluate step -- the caller's if supplied -- at the
# provider's parameters. A completed default is never the caller's to replace:
# a pinned rate is the package's evaluator at the intercept completion fixed
# for the current period, a uniform choice the package's evaluator at no
# parameter at all, and neither reads the provider.
fid_evaluation_inputs <- function(routing, fid, evaluate, theta, pinned, t) {
  key <- as.character(fid)
  switch(
    routing$step[match(fid, routing$map$fid)],
    default = list(step = evaluate, theta = theta[[key]]),
    constant = list(
      step = default_evaluate,
      theta = pinned_intercept(pinned[[key]], t)
    ),
    uniform = list(step = default_evaluate, theta = numeric(0))
  )
}

# The pinned intercept in force at `t`. A completed rate completion could not
# pin has no level to be evaluated at, so it can never fire.
pinned_intercept <- function(rate, t) {
  if (is.null(rate)) {
    return(-Inf)
  }
  rate$intercept[intercept_only_rate_period(rate, t)]
}

# Every rate process's values at the live state, named by fid.
rate_values <- function(
  handle,
  rate_fids,
  theta,
  evaluate,
  routing,
  pinned,
  t
) {
  values <- lapply(rate_fids, function(fid) {
    inputs <- fid_evaluation_inputs(routing, fid, evaluate, theta, pinned, t)
    evaluate_fid(handle, fid, inputs$theta, inputs$step)
  })
  stats::setNames(values, as.character(rate_fids))
}

# What the mark step reads for the fid that fired: its own values, the paired
# choice evaluation as a function of the drawn sender (so only the drawn
# sender's row is ever computed), and the identity it needs to name the event.
mark_evaluation <- function(
  handle,
  fid,
  theta,
  evaluate,
  dims,
  rates,
  routing,
  updates
) {
  map <- routing$map
  row <- match(fid, map$fid)
  choice_fid <- paired_choice_fid(map, fid)
  evaluation <- list(
    fid = fid,
    family = map$family[row],
    layer = map$layer[row],
    flavor = map$flavor[row],
    n_actors1 = dims[[as.character(fid)]]$n_actors1,
    n_actors2 = dims[[as.character(fid)]]$n_actors2,
    rate = rates[[as.character(fid)]],
    choice = NULL,
    update = updates[[as.character(fid)]]
  )
  if (is.na(choice_fid)) {
    return(evaluation)
  }
  # A choice step never reads the clock, so the pinned table and time are not
  # needed to resolve its inputs.
  inputs <- fid_evaluation_inputs(
    routing,
    choice_fid,
    evaluate,
    theta,
    pinned = list(),
    t = NA_real_
  )
  evaluation$choice <- function(sender) {
    evaluate_fid(handle, choice_fid, inputs$theta, inputs$step, sender = sender)
  }
  evaluation
}

# The per-fid shapes, read once at open: they are fixed by the compiled engines
# and a step needs them to decode a dyad cell, so rebuilding a state per step
# to recover two integers would be waste.
simulation_fid_dims <- function(handle, routing) {
  map <- routing$map
  dims <- lapply(map$fid, function(fid) {
    state <- simulation_fid_state(handle, fid)
    list(n_actors1 = state$n_actors1, n_actors2 = state$n_actors2)
  })
  stats::setNames(dims, as.character(map$fid))
}

# The update a drawn event applies, per fid. A flavor's value comes from its
# layer's `values_equivalence`, under the layer's own update semantics, so a
# drawn dissolution removes the tie a creation made instead of adding another.
# An unflavored process adds one on its layer.
simulation_mark_updates <- function(spec) {
  map <- spec$process_map
  info <- spec$data$info %||% list()
  updates <- lapply(seq_len(nrow(map)), function(i) {
    layer <- map$layer[i]
    mapping <- info$values_equivalence[[layer]]
    flavor <- map$flavor[i]
    if (is.na(flavor) || is.null(mapping)) {
      return(list(semantics = "increment", value = 1))
    }
    semantics <- unname(info$update[layer])
    list(semantics = semantics, value = unname(mapping[[flavor]]))
  })
  stats::setNames(updates, as.character(map$fid))
}

# Each process's regime and the evaluate step it resolves to, read from the
# completion record and never from the formula's shape: an authored
# `rate = ~ 1` has a completed rate's shape but is modeled, and reads its
# intercept from the parameters.
simulation_routing <- function(spec, handle) {
  map <- spec$process_map
  completed <- map$completed %||% rep(FALSE, nrow(map))
  map$regime <- ifelse(completed, "completed", "modeled")
  step <- ifelse(
    !completed,
    "default",
    ifelse(map$family == "rate", "constant", "uniform")
  )
  list(map = map, step = step, modeled = map$fid[!completed])
}

# The other sub-model of `fid`'s process: same layer, same flavor.
sibling_fid <- function(map, fid) {
  row <- match(fid, map$fid)
  same <- map$layer == map$layer[row] &
    identical_flavor(map$flavor, map$flavor[row]) &
    map$family != map$family[row]
  map$fid[which(same)[1L]]
}

# The choice fid of the same process as `fid`: same layer, same flavor.
paired_choice_fid <- function(map, fid) {
  row <- match(fid, map$fid)
  same <- map$layer == map$layer[row] &
    identical_flavor(map$flavor, map$flavor[row]) &
    map$family == "choice"
  if (!any(same)) {
    return(NA_integer_)
  }
  map$fid[which(same)[1L]]
}

identical_flavor <- function(flavors, this) {
  if (is.na(this)) is.na(flavors) else !is.na(flavors) & flavors == this
}

# --------------------------------------------------------------------------- #
# Draws and targets
# --------------------------------------------------------------------------- #

# One index drawn with probability proportional to `weights`, which need not be
# normalized. Exact zeros are excluded rather than given a vanishing share.
sample_index <- function(weights) {
  total <- sum(weights)
  if (!(total > 0)) {
    return(NA_integer_)
  }
  which(stats::runif(1L) * total <= cumsum(weights))[1L]
}

# The target that binds, named as `stop_reason` reports it, or NULL.
reached_target <- function(k, t, n_events, horizon) {
  if (!is.null(n_events) && k >= n_events) {
    return("n_events")
  }
  if (!is.null(horizon) && t >= horizon) {
    return("horizon")
  }
  NULL
}

# --------------------------------------------------------------------------- #
# The explosion guard
# --------------------------------------------------------------------------- #

# The stops that end a replicate as a guard rather than a target. Each flags
# the replicate and keeps the events drawn; none ends the call.
SIM_GUARD_STOPS <- c("max_events", "rate_trajectory", "clock_resolution")

# The total rate at each drawn event, kept so a guard stop can show how the
# rate got there. Grown by doubling: a run's length is unknown until it stops.
# `observed_wait` is the scale the waiting times are judged against; it is not
# finite when nothing was observed, and the wait criterion is then off.
new_rate_trajectory <- function(observed_wait) {
  env <- new.env(parent = emptyenv())
  env$time <- numeric(64L)
  env$total_rate <- numeric(64L)
  env$wait <- numeric(64L)
  env$n <- 0L
  env$observed_wait <- observed_wait
  env
}

record_total_rate <- function(trajectory, t, total, wait) {
  n <- trajectory$n + 1L
  if (n > length(trajectory$time)) {
    size <- 2L * length(trajectory$time)
    length(trajectory$time) <- size
    length(trajectory$total_rate) <- size
    length(trajectory$wait) <- size
  }
  trajectory$time[n] <- t
  trajectory$total_rate[n] <- total
  trajectory$wait[n] <- wait
  trajectory$n <- n
  invisible(NULL)
}

rate_trajectory_frame <- function(trajectory) {
  keep <- seq_len(trajectory$n)
  data.frame(
    time = trajectory$time[keep],
    total_rate = trajectory$total_rate[keep]
  )
}

# The rate-trajectory trigger, off unless the guard sets a finite factor. A
# run stops when its total rate exceeds `rate_multiple` times its value at
# the first event, or when the median of its last `wait_window` waiting
# times falls below the observed mean waiting time divided by
# `wait_collapse`.
rate_has_run_away <- function(trajectory, guard) {
  n <- trajectory$n
  if (n == 0L) {
    return(FALSE)
  }
  if (
    is.finite(guard$rate_multiple) &&
      trajectory$total_rate[n] > guard$rate_multiple * trajectory$total_rate[1L]
  ) {
    return(TRUE)
  }
  window <- guard$wait_window
  scale <- trajectory$observed_wait
  if (
    !is.finite(guard$wait_collapse) ||
      n < window ||
      !is.finite(scale) ||
      !(scale > 0)
  ) {
    return(FALSE)
  }
  recent <- trajectory$wait[(n - window + 1L):n]
  stats::median(recent) < scale / guard$wait_collapse
}

# One warning per call, whatever happened across its replicates: how many
# stopped at a guard, and which, and how many ran past the end of the
# observation window, where the exogenous state is held rather than invented.
warn_simulation_conditions <- function(runs, call) {
  n_runs <- length(runs)
  reasons <- vapply(runs, function(run) run$diagnostics$stop_reason, "")
  guarded <- reasons %in% SIM_GUARD_STOPS
  past_end <- vapply(
    runs,
    function(run) run$diagnostics$end_time > run$diagnostics$window_end,
    logical(1)
  )
  n_flagged <- sum(guarded | past_end)
  if (n_flagged == 0L) {
    return(invisible(NULL))
  }
  bullets <- character(0)
  classes <- character(0)
  if (any(guarded)) {
    counts <- table(factor(reasons[guarded], levels = SIM_GUARD_STOPS))
    counts <- counts[counts > 0L]
    by_guard <- vapply(
      names(counts),
      function(guard) {
        n <- counts[[guard]]
        cli::format_inline("{n} at {.val {guard}}")
      },
      character(1)
    )
    n_guarded <- sum(guarded)
    bullets <- c(
      bullets,
      "!" = cli::format_inline(
        "{n_guarded} stopped at a guard ({by_guard}); each keeps the events
         drawn before the stop and is flagged {.field capped}."
      )
    )
    classes <- c(classes, "goldfish_sim_guard_stop")
  }
  if (any(past_end)) {
    n_past <- sum(past_end)
    window_end <- runs[[which(past_end)[1L]]]$diagnostics$window_end
    bullets <- c(
      bullets,
      "!" = cli::format_inline(
        "{n_past} ran past the end of the observation window at
         {.val {window_end}}; covariate and composition state is held at its
         value there."
      )
    )
    classes <- c(classes, "goldfish_sim_frozen_exogenous")
  }
  cli::cli_warn(
    c(
      "Simulation flagged {n_flagged} of {n_runs} replicate{?s}.",
      bullets,
      "i" = "Flagged replicates stay in the result; each one's
             {.field diagnostics} holds its stop reason and total-rate
             trajectory."
    ),
    call = call,
    class = classes
  )
}

# --------------------------------------------------------------------------- #
# Resolving `coef` to a provider
# --------------------------------------------------------------------------- #

# Every accepted `coef` form becomes a provider, so the loop only ever calls
# `at()`. A numeric vector is the single-process convention; a `goldfishParams`
# is the joint surface, gated on completeness and reconciled against the
# completed spec; a provider is itself.
resolve_coef_provider <- function(coef, spec, call) {
  if (is_parameter_provider(coef)) {
    return(coef)
  }
  map <- spec$process_map
  if (is_parameters_goldfish(coef)) {
    reconcile_joint_parameters(coef, spec, arg = "coef", call = call)
    full <- joint_simulation_parameters(coef, arg = "coef", call = call)
    labels <- render_process_label(map, map$fid)
    by_fid <- stats::setNames(
      lapply(labels, function(label) full[[label]] %||% numeric(0)),
      as.character(map$fid)
    )
    return(constant_parameter_provider(by_fid))
  }
  if (is.numeric(coef)) {
    authored <- map$fid[!(map$completed %||% rep(FALSE, nrow(map)))]
    if (length(authored) != 1L) {
      cli::cli_abort(
        c(
          "A numeric {.arg coef} names one process, but the specification has
           {length(authored)}.",
          "i" = "Build the parameters with {.fn set_parameters} over the joint
                 specification."
        ),
        call = call,
        class = "goldfish_sim_bad_coef"
      )
    }
    by_fid <- stats::setNames(list(as.numeric(coef)), as.character(authored))
    return(constant_parameter_provider(by_fid))
  }
  cli::cli_abort(
    c(
      "{.arg coef} must be a numeric vector, a {.cls goldfishParams}, or a
       {.cls goldfishParamProvider}.",
      "x" = "A {.cls {class(coef)[1]}} was supplied."
    ),
    call = call,
    class = "goldfish_sim_bad_coef"
  )
}

# The provider's per-step return, checked against the fids the walk carries.
provider_parameters <- function(resolved, routing, call) {
  map <- routing$map
  if (!is.list(resolved) || is.null(resolved$parameters)) {
    cli::cli_abort(
      c(
        "The parameter provider's {.fn at} must return a list with
         {.field parameters}.",
        "i" = "{.field parameters} is a list of parameter vectors named by
               formula id."
      ),
      call = call,
      class = "goldfish_sim_bad_parameters"
    )
  }
  theta <- resolved$parameters
  # Only the walked processes need parameters: a completed default is pinned or
  # uniform by construction and reads none, so demanding an entry for it would
  # make the caller supply a value that is never used.
  missing <- setdiff(as.character(routing$modeled), names(theta))
  if (length(missing) > 0L) {
    labels <- render_process_label(map, as.integer(missing))
    cli::cli_abort(
      c(
        "The parameter provider left {length(missing)} process{?es}
         unparameterized.",
        "x" = "No parameters for {.val {labels}}."
      ),
      call = call,
      class = "goldfish_sim_bad_parameters"
    )
  }
  theta
}

# --------------------------------------------------------------------------- #
# Collecting the sequence
# --------------------------------------------------------------------------- #

# One typed vector per output column plus the latent path, allocated once at
# the run's capacity and written by index, so an event costs a few element
# writes rather than a data frame. A capacity whose columns would pass the
# byte budget starts at the observed dependent count instead and doubles.
new_event_collector <- function(capacity, observed) {
  if (capacity * SIM_EVENT_BYTES > sim_collector_byte_budget()) {
    capacity <- max(observed, 1L)
  }
  env <- new.env(parent = emptyenv())
  allocate_event_columns(env, capacity)
  env$n <- 0L
  env
}

# Bytes one collected event occupies: three doubles, three integers, and the
# pointers of two strings and a latent entry.
SIM_EVENT_BYTES <- 3L * 8L + 3L * 4L + 3L * 8L

# The largest columns a run allocates up front, 64 MB: about 1.1 million
# events, beyond the default guard of any data set this package fits.
sim_collector_byte_budget <- function() 64 * 1024^2

allocate_event_columns <- function(env, capacity) {
  env$time <- numeric(capacity)
  env$layer <- character(capacity)
  env$flavor <- character(capacity)
  env$sender <- integer(capacity)
  env$receiver <- integer(capacity)
  env$increment <- numeric(capacity)
  env$fid <- integer(capacity)
  env$latent <- vector("list", capacity)
  invisible(NULL)
}

grow_event_columns <- function(collector) {
  size <- max(2L * length(collector$time), 1L)
  for (column in c(SIM_EVENT_COLUMNS, "latent")) {
    length(collector[[column]]) <- size
  }
  invisible(NULL)
}

SIM_EVENT_COLUMNS <- c(
  "time",
  "layer",
  "flavor",
  "sender",
  "receiver",
  "increment",
  "fid"
)

collect_event <- function(collector, event, fid, map, latent) {
  k <- collector$n + 1L
  if (k > length(collector$time)) {
    grow_event_columns(collector)
  }
  collector$time[k] <- event$time
  collector$layer[k] <- event$layer
  collector$flavor[k] <- map$flavor[match(fid, map$fid)]
  collector$sender[k] <- event$sender %||% NA_integer_
  collector$receiver[k] <- event$receiver %||% NA_integer_
  collector$increment[k] <- event$increment %||% NA_real_
  collector$fid[k] <- fid
  # `[<-` with a list, not `[[<-`: assigning NULL through `[[` would delete
  # the slot rather than record a step without latent state.
  collector$latent[k] <- list(latent)
  collector$n <- k
  invisible(NULL)
}

# The columns cut to the events drawn, set as a data frame in place: no
# `data.frame()` call and no bind.
collector_frame <- function(collector) {
  keep <- seq_len(collector$n)
  columns <- lapply(
    stats::setNames(SIM_EVENT_COLUMNS, SIM_EVENT_COLUMNS),
    function(column) collector[[column]][keep]
  )
  structure(
    columns,
    class = "data.frame",
    row.names = .set_row_names(collector$n)
  )
}

collector_latent <- function(collector) {
  collector$latent[seq_len(collector$n)]
}

# --------------------------------------------------------------------------- #
# The result
# --------------------------------------------------------------------------- #

# `times` is the resolved variant from `resolve_simulation_times()`: the
# `times` run, its `source`, and the specification's `default` and `reason`.
new_goldfish_sim <- function(
  events,
  process_map,
  times,
  capped,
  stop_reason,
  n_proposals,
  end_time,
  window_end,
  trajectory,
  latent,
  spec,
  replicate
) {
  # The latent path is written only when a provider supplied one, so a plain
  # run carries no column of NULLs.
  latent_path <- if (any(!vapply(latent, is.null, logical(1)))) latent else NULL
  structure(
    list(
      events = events,
      process_map = process_map,
      times = times$times,
      times_source = times$source,
      times_default = times$default,
      times_reason = times$reason,
      capped = capped,
      diagnostics = list(
        stop_reason = stop_reason,
        n_events = nrow(events),
        n_proposals = n_proposals,
        acceptance_rate = if (n_proposals > 0L) {
          nrow(events) / n_proposals
        } else {
          NA_real_
        },
        end_time = end_time,
        window_end = window_end,
        trajectory = trajectory
      ),
      latent = latent_path,
      replicate = replicate
    ),
    class = "goldfishSim"
  )
}

#' @param x a `goldfishSim` from [simulate()].
#' @param ... ignored.
#' @rdname print-method
#' @export
print.goldfishSim <- function(x, ...) {
  cli::cli_text("{.cls goldfishSim}: {nrow(x$events)} event{?s}")
  # Where the variant came from, so a reader can tell a derived default from
  # an override of it.
  if (identical(x$times_source, "specification")) {
    cli::cli_text(
      "{.field times}: {.val {x$times}} — from the specification
       ({x$times_reason})"
    )
  } else if (identical(x$times, x$times_default)) {
    cli::cli_text("{.field times}: {.val {x$times}} — requested")
  } else {
    cli::cli_text(
      "{.field times}: {.val {x$times}} — requested; the specification's
       default is {.val {x$times_default}}"
    )
  }
  map <- x$process_map
  regime <- map$regime %||% rep("modeled", nrow(map))
  labels <- render_process_label(map, map$fid)
  cli::cli_ul()
  for (i in seq_along(labels)) {
    cli::cli_li("{.field {labels[i]}} ({regime[i]})")
  }
  cli::cli_end()
  if (isTRUE(x$capped)) {
    reason <- x$diagnostics$stop_reason
    end_time <- x$diagnostics$end_time
    cli::cli_alert_warning(
      "Run stopped at the {.val {reason}} guard at time {.val {end_time}};
       flagged as capped."
    )
  }
  invisible(x)
}
