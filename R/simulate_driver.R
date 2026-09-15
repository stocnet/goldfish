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
  max_events,
  control_prep,
  call = rlang::caller_env()
) {
  handle <- walk_open(spec, control_prep, call = call)
  # The defaults count the events the walk itself steps, so a run with no
  # target generates as many events as were observed on the same processes.
  n_observed <- sum(handle$schedule$dependent)
  if (is.null(n_events) && is.null(horizon)) {
    n_events <- n_observed
  }
  max_events <- max_events %||% (10L * n_observed)
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
  collector <- new_event_collector()
  freeze_time <- last_exogenous_time(handle)
  frozen_warned <- FALSE

  t <- handle$current_time
  k <- 0L
  n_drawn <- 0L
  stop_reason <- "target"
  provider_breakpoint <- Inf

  repeat {
    if (reached_target(k, t, n_events, horizon)) {
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

    if (!frozen_warned && t > freeze_time && is.finite(freeze_time)) {
      warn_frozen_exogenous(freeze_time, call)
      frozen_warned <- TRUE
    }

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

    t <- t + wait
    if (!is.null(horizon) && t > horizon) {
      t <- horizon
      stop_reason <- "target"
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
    capped = identical(stop_reason, "max_events"),
    stop_reason = stop_reason,
    n_proposals = n_drawn,
    latent = collector$latent,
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
# over the node sets of its same-process sibling, which the walk did compile.
simulation_fid_state <- function(handle, fid) {
  if (!fid %in% handle$deferred$fid) {
    return(walk_build_state(handle, fid))
  }
  map <- handle$process_map
  row <- match(fid, map$fid)
  engine <- walk_engine_of_fid(handle, sibling_fid(map, fid))
  n1 <- engine$n1
  n2 <- engine$n2
  if (identical(map$family[row], "rate")) {
    return(list(
      model_type = "DyNAM-M-Rate",
      stat_mat = matrix(1, n1, 1L),
      active_sender = engine$presence1,
      active_dyad = rep(TRUE, n1),
      active_dyad_encoding = "alter",
      n_actors1 = n1,
      n_actors2 = 1L,
      twomode_or_reflexive = TRUE
    ))
  }
  list(
    model_type = "DyNAM-M",
    stat_mat = matrix(0, n1 * n2, 0L),
    active_sender = engine$presence1,
    active_dyad = engine$presence2,
    active_dyad_encoding = "alter",
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

reached_target <- function(k, t, n_events, horizon) {
  if (!is.null(n_events) && k >= n_events) {
    return(TRUE)
  }
  if (!is.null(horizon) && t >= horizon) {
    return(TRUE)
  }
  FALSE
}

# The last time the observed data can still change the exogenous state. Past
# it a free-running run holds that state rather than inventing its continuation.
last_exogenous_time <- function(handle) {
  rows <- handle$exo_rows
  if (length(rows) == 0L) {
    return(-Inf)
  }
  max(handle$schedule$time[rows])
}

warn_frozen_exogenous <- function(freeze_time, call) {
  cli::cli_warn(
    c(
      "Simulating past the last observed exogenous change.",
      "i" = "Covariate and composition state is held at its value from
             {.val {freeze_time}} for the rest of the run."
    ),
    call = call,
    class = "goldfish_sim_frozen_exogenous"
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

new_event_collector <- function() {
  env <- new.env(parent = emptyenv())
  env$rows <- list()
  env$latent <- list()
  env
}

collect_event <- function(collector, event, fid, map, latent) {
  row <- match(fid, map$fid)
  collector$rows[[length(collector$rows) + 1L]] <- data.frame(
    time = event$time,
    layer = event$layer,
    flavor = map$flavor[row],
    sender = event$sender %||% NA_integer_,
    receiver = event$receiver %||% NA_integer_,
    increment = event$increment %||% NA_real_,
    fid = fid,
    stringsAsFactors = FALSE
  )
  collector$latent[[length(collector$latent) + 1L]] <- latent
  invisible(NULL)
}

collector_frame <- function(collector) {
  if (length(collector$rows) == 0L) {
    return(data.frame(
      time = numeric(0),
      layer = character(0),
      flavor = character(0),
      sender = integer(0),
      receiver = integer(0),
      increment = numeric(0),
      fid = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, collector$rows)
}

# --------------------------------------------------------------------------- #
# The result
# --------------------------------------------------------------------------- #

new_goldfish_sim <- function(
  events,
  process_map,
  times,
  capped,
  stop_reason,
  n_proposals,
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
      times = times,
      capped = capped,
      diagnostics = list(
        stop_reason = stop_reason,
        n_events = nrow(events),
        n_proposals = n_proposals,
        acceptance_rate = if (n_proposals > 0L) {
          nrow(events) / n_proposals
        } else {
          NA_real_
        }
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
  cli::cli_text(
    "{.cls goldfishSim}: {nrow(x$events)} event{?s},
     {.field times} = {.val {x$times}}"
  )
  map <- x$process_map
  regime <- map$regime %||% rep("modeled", nrow(map))
  labels <- render_process_label(map, map$fid)
  cli::cli_ul()
  for (i in seq_along(labels)) {
    cli::cli_li("{.field {labels[i]}} ({regime[i]})")
  }
  cli::cli_end()
  if (isTRUE(x$capped)) {
    cli::cli_alert_warning(
      "Run stopped at the {.arg max_events} guard; flagged as capped."
    )
  }
  invisible(x)
}
