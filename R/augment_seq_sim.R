# =========================================================================== #
# augment_seq_sim(): the model-driven augmenter.
#
# It draws each between-wave interval forward from the model at theta,
# conditioned on the wave it must reach, and runs as supplied steps on the
# simulation driver (simulate_replicate()) rather than a loop of its own: the
# driver advances the walk, evaluates every rate with the package's own
# evaluator and injects the events; the augmenter supplies
#
#   clock   which event comes next, and when,
#   mark    who acts on whom,
#   accept  the completion check that rejects a stranded interval draw.
#
# It never supplies the evaluation step. Its rates and choices are the
# evaluator's, so the proposal and the target read the same intensity for the
# placed flip -- for a completed timed rate, the per-period pinned plateau of
# `spec$completed_rates`, never theta.
#
# One step, at the walk's state after the previous event:
#
# * The candidates are the (layer, flavor, sender) pairs with a remaining
#   observed flip whose support applies now (the sender's rate and some
#   remaining receiver's choice are nonzero), plus the next observed
#   relational event of the interval. One is selected in proportion to its
#   rate; the selection probability enters the density.
# * A relational event is placed at its observed time.
# * A flip's waiting time follows its pair's rate. Up to a breakpoint of the
#   walk (an exogenous row, a window expiry, a composition change) the rate is
#   constant, so the draw is an exponential censored there: past it the walk
#   advances and the same pair redraws at the rate the walk then holds. The
#   segment ending at the next relational event or the wave end is a
#   truncated exponential, so the flip lands strictly inside the interval and
#   every wave is hit. The receiver is drawn among the pair's remaining
#   observed receivers by the choice probabilities.
#
# The density is thus a product of exact per-segment densities at the rates
# the target's compensator integrates. It normalizes over the observed flips,
# where the target normalizes over the full structural support; that gap is
# what the importance weight corrects.
#
# Under cross-dyad support coupling a greedy step can disable another flip's
# support before it is placed. The interval draw is then rejected and redrawn
# until it completes (the shared restart-ceiling helper), so the delivered
# draw is conditioned on completion and its density is divided by the
# completion probability, estimated by the draw's acceptance rate. That
# plug-in normalizer depends on theta and does not cancel, so the estimation
# loop warns once per run when a restart fires (warn_restart_coupling_once()).
#
# An interval is redrawn alone: each attempt replays the intervals before it,
# as drawn, and the observed events up to its start.
# =========================================================================== #

# The sim augmenter over a completed specification and its recipe.
augment_seq_sim <- function(spec, recipe, control, call = rlang::caller_env()) {
  context <- new_augmenter_context(spec, recipe, call = call)
  coupling <- support_constraint_labels(spec)
  plan <- sim_plan(context, coupled = length(coupling) > 0L)
  n_intervals <- length(recipe$wave_times) - 1L

  draw_one <- function(theta) {
    parameters <- flat_to_fid_theta(spec, theta, call = call)
    times <- rep(NA_real_, nrow(recipe$events))
    attempts <- rep(1L, n_intervals)
    log_q <- 0
    for (k in seq_len(n_intervals)) {
      ids <- recipe$events$event[recipe$events$interval == k]
      if (length(ids) == 0L) {
        next
      }
      accepted <- redraw_until_accepted(
        function() sim_interval_attempt(plan, k, times, parameters, call),
        function(attempt) attempt$completed,
        coupling,
        call = call
      )
      times[ids] <- accepted$value$times[ids]
      log_q <- log_q + accepted$value$log_q
      attempts[[k]] <- accepted$attempts
    }
    completion <- acceptance_rate(rep(1L, n_intervals), attempts, theta)
    completion$interval <- seq_len(n_intervals)
    list(
      data = emit_augmented_data(context, times),
      log_proposal = log_q - sum(log(completion$rate)),
      restarted = any(attempts > 1L),
      log_proposal_raw = log_q,
      completion = completion
    )
  }

  new_dynes_augmenter(
    draw = function(iteration, draw_index, theta) {
      lapply(draw_index, function(k) draw_one(theta))
    },
    routine = "sim",
    coupling = coupling
  )
}

# The theta-free plan every interval attempt reads: the specification the
# driver walks, the observed events it replays, and the flips it draws.
#
# The walk is opened over the base data with every flip stamped at the
# observation start. A modeled layer's rows are never applied by the walk --
# the driver injects them -- so the stamps only set where the walk's clock
# opens, which must not be after the first time a flip can be drawn.
sim_plan <- function(context, coupled) {
  spec <- context$spec
  recipe <- context$recipe
  waves <- recipe$wave_times
  map <- spec$process_map
  labels <- as.data.frame(context$base$nodes)$label
  update <- context$base$info$update

  stamped <- emit_augmented_data(
    context,
    rep(waves[[1L]], nrow(recipe$events))
  )
  events <- recipe$events
  flips <- data.frame(
    event = events$event,
    interval = events$interval,
    chain = events$chain,
    layer = events$layer,
    flavor = events$flavor,
    sender = node_index(events$from, labels),
    receiver = node_index(events$to, labels),
    semantics = sim_semantics(update[events$layer]),
    value = ifelse(
      update[events$layer] == "replace",
      events$to_value,
      events$to_value - events$from_value
    ),
    stringsAsFactors = FALSE
  )
  flips$rate_fid <- sim_rate_fid(map, flips$layer, flips$flavor)

  runner <- new.env(parent = emptyenv())
  list(
    spec = spec,
    walk_spec = bind_candidate_data(spec, stamped),
    runner = runner,
    steps = sim_steps(runner),
    recipe = recipe,
    waves = waves,
    map = map,
    observed = sim_observed_events(context, labels, update),
    flips = flips,
    rate_fids = map$fid[map$family == "rate"],
    coupled = coupled
  )
}

# The observed events of the modeled processes up to the last wave, in the
# order of the observed stream: the initial condition (at or before the
# observation start, interval 0) and the relational events of each interval
# (w_{k-1}, w_k]. A flavor no process models is replayed by the driver itself.
sim_observed_events <- function(context, labels, update) {
  map <- context$spec$process_map
  waves <- context$recipe$wave_times
  ties <- as.data.frame(context$base$ties)
  flavor <- ties[["flavor"]] %||% rep(NA_character_, nrow(ties))
  modeled <- modeled_tie_rows(ties, map)
  keep <- modeled & !is.na(ties$time) & ties$time <= waves[[length(waves)]]
  ties <- ties[keep, , drop = FALSE]
  flavor <- flavor[keep]
  observed <- data.frame(
    time = as.numeric(ties$time),
    interval = findInterval(ties$time, waves, left.open = TRUE),
    layer = ties$layer,
    flavor = flavor,
    sender = node_index(ties$from, labels),
    receiver = node_index(ties$to, labels),
    semantics = sim_semantics(update[ties$layer]),
    value = ties[["weight"]] %||% rep(1, nrow(ties)),
    stringsAsFactors = FALSE
  )
  observed$rate_fid <- sim_rate_fid(map, observed$layer, observed$flavor)
  observed
}

sim_semantics <- function(update) {
  ifelse(update == "replace", "replace", "increment")
}

# The rate fid of each (layer, flavor), NA for a process without one.
sim_rate_fid <- function(map, layer, flavor) {
  vapply(
    seq_along(layer),
    function(i) {
      rows <- map$layer == layer[[i]] &
        identical_flavor(map$flavor, flavor[[i]]) &
        map$family == "rate"
      if (any(rows)) map$fid[which(rows)[1L]] else NA_integer_
    },
    integer(1)
  )
}

# One attempt at interval `k` over the intervals before it drawn at `times`.
# Returns `list(completed, times, log_q)`: whether every flip of the interval
# was placed, the times with the interval's filled in, and the log density
# the attempt accumulated over the interval's steps.
sim_interval_attempt <- function(plan, k, times, parameters, call) {
  state <- new_sim_attempt(plan, k, times, parameters)
  plan$runner$state <- state
  on.exit(plan$runner$state <- NULL)
  completed <- tryCatch(
    {
      run <- simulate_replicate(
        spec = plan$walk_spec,
        provider = constant_parameter_provider(parameters),
        steps = plan$steps,
        replicate = 1L,
        times = list(
          times = "exact",
          source = "augmenter",
          default = "exact",
          reason = "an augmented sequence"
        ),
        horizon = NULL,
        n_events = state$n_events,
        control_sim = set_simulation_guard(
          max_events = state$n_events + 1L,
          clock_resolution = NULL
        ),
        control_prep = set_preprocessing(),
        call = call
      )
      if (!identical(run$diagnostics$stop_reason, "n_events")) {
        cli::cli_abort(
          "The sim draw of interval {k} stopped at
           {.val {run$diagnostics$stop_reason}} before placing every event.",
          .internal = TRUE,
          call = call
        )
      }
      TRUE
    },
    goldfish_dynes_sim_stranded = function(condition) FALSE
  )
  list(completed = completed, times = state$times, log_q = state$log_q)
}

# The mutable state one attempt's steps share: the forced events (the
# observed events and earlier intervals' flips up to the interval start), the
# interval's relational events, the recipe cursor over its flips, the step
# in progress, and the density so far.
new_sim_attempt <- function(plan, k, times, parameters) {
  observed <- plan$observed
  flips <- plan$flips
  earlier <- flips[flips$interval < k, , drop = FALSE]
  earlier$time <- times[earlier$event]
  columns <- c(
    "time",
    "layer",
    "flavor",
    "sender",
    "receiver",
    "semantics",
    "value",
    "rate_fid"
  )
  forced <- rbind(
    observed[observed$interval < k, columns, drop = FALSE],
    earlier[, columns, drop = FALSE]
  )
  # Stable, so observed events sharing a stamp keep the stream's order; a
  # drawn flip never shares one.
  forced <- forced[order(forced$time, method = "radix"), , drop = FALSE]
  relational <- observed[observed$interval == k, columns, drop = FALSE]
  state <- new.env(parent = emptyenv())
  state$plan <- plan
  state$k <- k
  state$wave_start <- plan$waves[[k]]
  state$wave_end <- plan$waves[[k + 1L]]
  state$parameters <- parameters
  state$forced <- forced
  state$relational <- relational
  state$n_forced <- 0L
  state$n_relational <- 0L
  state$cursor <- new_recipe_cursor(plan$recipe)
  state$n_events <- nrow(forced) +
    nrow(relational) +
    sum(flips$interval == k)
  state$times <- times
  state$pending <- NULL
  state$time <- NA_real_
  state$stranded <- FALSE
  state$log_q <- 0
  state$routing <- NULL
  state
}

# --------------------------------------------------------------------------- #
# The steps
# --------------------------------------------------------------------------- #

# The driver's steps, built once per augmenter. Each reads the attempt in
# progress from `runner`; with none, as in set_simulation_steps()'s
# construction-time dry call on a one-actor toy handle, it answers with a
# well-formed placeholder, since the steps read the real walk.
sim_steps <- function(runner) {
  set_simulation_steps(
    clock = function(rates, t, handle) {
      if (is.null(runner$state)) {
        return(list(wait = 1, fid = 1L, kind = "event"))
      }
      sim_clock(runner$state, t, handle)
    },
    mark = function(fid, evaluation, handle) {
      if (is.null(runner$state)) {
        return(list(layer = evaluation$layer %||% "toy"))
      }
      sim_mark(runner$state, evaluation)
    },
    accept = function(event, handle) {
      if (is.null(runner$state)) {
        return(TRUE)
      }
      sim_accept(runner$state, event)
    }
  )
}

# P2. The next event and its waiting time. A step in progress -- a relational
# event the driver clipped at a breakpoint, or a flip whose draw was censored
# at one -- continues; otherwise the next forced event comes, else a new
# candidate is selected.
sim_clock <- function(state, t, handle) {
  if (is.null(state$routing)) {
    state$routing <- simulation_routing(state$plan$spec, handle)
  }
  # The replayed events end before the interval starts: the walk moves on to
  # the wave that opens it, at which the draw begins.
  if (
    is.null(state$pending) &&
      state$n_forced == nrow(state$forced) &&
      t < state$wave_start
  ) {
    return(list(
      wait = state$wave_start - t,
      fid = state$plan$rate_fids[[1L]],
      kind = "breakpoint"
    ))
  }
  if (is.null(state$pending)) {
    state$pending <- sim_next_step(state, t, handle)
  }
  pending <- state$pending
  if (identical(pending$kind, "stranded")) {
    return(list(wait = 0, fid = state$plan$rate_fids[[1L]], kind = "event"))
  }
  if (!identical(pending$kind, "flip")) {
    state$time <- pending$event$time
    return(list(
      wait = pending$event$time - t,
      fid = sim_event_fid(state, pending$event),
      kind = "event"
    ))
  }
  sim_flip_wait(state, pending, t, handle)
}

# The step that follows the last placed event: the next forced event, else a
# candidate selected in proportion to its rate, else a strand.
sim_next_step <- function(state, t, handle) {
  if (state$n_forced < nrow(state$forced)) {
    state$n_forced <- state$n_forced + 1L
    return(list(kind = "forced", event = state$forced[state$n_forced, ]))
  }
  relational <- NULL
  if (state$n_relational < nrow(state$relational)) {
    relational <- state$relational[state$n_relational + 1L, ]
  }
  # A flip lands strictly inside the gap to the next relational event, so
  # none fits between two events at one stamp: the sibling of a placed event
  # comes next, at no wait.
  if (sim_sibling_pending(state)) {
    if (!(sim_relational_weight(state, relational, t, handle) > 0)) {
      return(list(kind = "stranded"))
    }
    return(list(kind = "relational", event = relational))
  }
  options <- sim_flip_options(state, t, handle)
  weights <- options$weight
  if (!is.null(relational)) {
    weights <- c(weights, sim_relational_weight(state, relational, t, handle))
  }
  total <- sum(weights)
  if (!(total > 0)) {
    return(list(kind = "stranded"))
  }
  chosen <- sample_index(weights)
  state$log_q <- state$log_q + log(weights[[chosen]] / total)
  if (chosen > nrow(options)) {
    return(list(kind = "relational", event = relational))
  }
  list(kind = "flip", option = options[chosen, ])
}

# Whether the next relational event shares the stamp of the one placed last.
sim_sibling_pending <- function(state) {
  n <- state$n_relational
  n > 0L &&
    n < nrow(state$relational) &&
    state$relational$time[[n + 1L]] == state$relational$time[[n]]
}

# The (layer, flavor, sender) candidates at `t`: the heads of the interval's
# chains with a flip left, grouped by pair, each weighted by the pair's rate
# and kept only while some remaining receiver has support now.
sim_flip_options <- function(state, t, handle) {
  heads <- sim_chain_heads(state, state$cursor$remaining(state$k))
  pairs <- unique(heads[, c("layer", "flavor", "sender", "rate_fid")])
  pairs$weight <- vapply(
    seq_len(nrow(pairs)),
    function(i) {
      pair <- pairs[i, ]
      rate <- sim_fid_values(state, handle, pair$rate_fid, t)[[pair$sender]]
      if (!(rate > 0)) {
        return(0)
      }
      receivers <- sim_pair_receivers(heads, pair)
      choice <- sim_choice_row(state, handle, pair, t)
      if (!any(choice[receivers] > 0)) 0 else rate
    },
    numeric(1)
  )
  pairs[pairs$weight > 0, , drop = FALSE]
}

# A relational event's selection weight: its sender's rate on its process
# (its dyad's, for a process whose rate ranges over dyads). Under a user
# support constraint an event whose support no longer applies cannot be
# placed, and gets no weight; without one, an observed event's support is
# only its own dyad's, which no flip of another dyad moves.
sim_relational_weight <- function(state, event, t, handle) {
  fid <- event$rate_fid
  if (is.na(fid)) {
    return(1)
  }
  values <- sim_fid_values(state, handle, fid, t)
  map <- state$plan$map
  choice_fid <- paired_choice_fid(map, fid)
  if (is.na(choice_fid)) {
    n2 <- length(values) / n_actors(handle)
    return(values[[(event$sender - 1L) * n2 + event$receiver]])
  }
  rate <- values[[event$sender]]
  if (state$plan$coupled && rate > 0) {
    choice <- sim_fid_values(state, handle, choice_fid, t, event$sender)
    if (!(choice[[event$receiver]] > 0)) {
      return(0)
    }
  }
  rate
}

# The waiting time of the flip in progress. Censored at a breakpoint before
# the anchor (the next relational event or the wave end); truncated at the
# anchor otherwise. The density of the part drawn is added to the attempt's.
sim_flip_wait <- function(state, pending, t, handle) {
  option <- pending$option
  anchor <- state$wave_end
  if (state$n_relational < nrow(state$relational)) {
    anchor <- min(anchor, state$relational$time[[state$n_relational + 1L]])
  }
  rate <- sim_fid_values(state, handle, option$rate_fid, t)[[option$sender]]
  fid <- option$rate_fid
  if (!(rate > 0)) {
    state$pending <- list(kind = "stranded")
    return(list(wait = 0, fid = fid, kind = "event"))
  }
  breakpoint <- walk_next_breakpoint(handle)
  if (breakpoint < anchor) {
    wait <- stats::rexp(1L, rate)
    if (t + wait < breakpoint) {
      state$log_q <- state$log_q + log(rate) - rate * wait
      state$time <- t + wait
      return(list(wait = wait, fid = fid, kind = "event"))
    }
    state$log_q <- state$log_q - rate * (breakpoint - t)
    return(list(wait = breakpoint - t, fid = fid, kind = "breakpoint"))
  }
  width <- anchor - t
  repeat {
    # Inverse CDF of the exponential truncated to (0, width); a draw that
    # rounds onto either end is redrawn, so the flip never meets an anchor.
    wait <- -log1p(stats::runif(1L) * expm1(-rate * width)) / rate
    if (wait > 0 && t + wait < anchor) {
      break
    }
  }
  state$log_q <- state$log_q +
    log(rate) -
    rate * wait -
    log(-expm1(-rate * width))
  state$time <- t + wait
  list(wait = wait, fid = fid, kind = "event")
}

# P3. The event to inject. A flip's receiver is drawn among its pair's
# remaining observed receivers by the choice probabilities the driver
# evaluated; a stranded step hands accept() a marker instead.
sim_mark <- function(state, evaluation) {
  pending <- state$pending
  if (identical(pending$kind, "stranded")) {
    return(list(layer = evaluation$layer, stranded = TRUE))
  }
  if (!identical(pending$kind, "flip")) {
    return(sim_event(pending$event))
  }
  option <- pending$option
  heads <- sim_chain_heads(state, state$cursor$remaining(state$k))
  receivers <- sim_pair_receivers(heads, option)
  choice <- evaluation$choice(option$sender)[receivers]
  total <- sum(choice)
  if (!(total > 0)) {
    state$pending <- list(kind = "stranded")
    return(list(layer = evaluation$layer, stranded = TRUE))
  }
  drawn <- sample_index(choice)
  state$log_q <- state$log_q + log(choice[[drawn]] / total)
  head <- heads[
    heads$layer == option$layer &
      identical_flavor(heads$flavor, option$flavor) &
      heads$sender == option$sender &
      heads$receiver == receivers[[drawn]],
    ,
    drop = FALSE
  ][1L, ]
  state$pending$event_id <- head$event
  state$pending$chain <- head$chain
  sim_event(head)
}

# P4. The completion check. A stranded step rejects the whole interval draw:
# the flips it left cannot all be placed, so the attempt stops and the
# restart helper draws the interval again. Any other event is recorded as
# placed.
sim_accept <- function(state, event) {
  if (isTRUE(event$stranded)) {
    state$stranded <- TRUE
    rlang::abort(
      "The interval draw stranded.",
      class = "goldfish_dynes_sim_stranded"
    )
  }
  pending <- state$pending
  switch(
    pending$kind,
    relational = {
      state$n_relational <- state$n_relational + 1L
    },
    flip = {
      state$cursor$take(pending$chain)
      state$times[[pending$event_id]] <- state$time
    }
  )
  state$pending <- NULL
  TRUE
}

# --------------------------------------------------------------------------- #
# Reading the walk
# --------------------------------------------------------------------------- #

# A fid's values at the live state through the package evaluator, routed as
# the driver routes it: a modeled fid at its theta, a completed rate at the
# pinned plateau of the period holding `t`, a completed choice uniform.
sim_fid_values <- function(state, handle, fid, t, sender = NULL) {
  inputs <- fid_evaluation_inputs(
    state$routing,
    fid,
    default_evaluate,
    state$parameters,
    state$plan$spec$completed_rates %||% list(),
    t
  )
  evaluate_fid(handle, fid, inputs$theta, inputs$step, sender = sender)
}

sim_choice_row <- function(state, handle, pair, t) {
  choice_fid <- paired_choice_fid(state$plan$map, pair$rate_fid)
  if (is.na(choice_fid)) {
    cli::cli_abort(
      "A panel process without a choice sub-model cannot be augmented.",
      .internal = TRUE
    )
  }
  sim_fid_values(state, handle, choice_fid, t, sender = pair$sender)
}

# The next event of every chain of the interval with a flip left.
sim_chain_heads <- function(state, remaining) {
  chains <- state$plan$recipe$chains
  at <- match(remaining$chain, chains$chain)
  ids <- chains$first[at] + chains$length[at] - remaining$left
  flips <- state$plan$flips
  flips[match(ids, flips$event), , drop = FALSE]
}

sim_pair_receivers <- function(heads, pair) {
  unique(heads$receiver[
    heads$layer == pair$layer &
      identical_flavor(heads$flavor, pair$flavor) &
      heads$sender == pair$sender
  ])
}

# The fid the driver is told fired: the event's rate fid, or any rate fid for
# a process without one (the driver only reads it to build the evaluation).
sim_event_fid <- function(state, event) {
  fid <- event$rate_fid
  if (is.na(fid)) state$plan$rate_fids[[1L]] else fid
}

sim_event <- function(row) {
  event <- list(
    layer = row$layer,
    sender = row$sender,
    receiver = row$receiver,
    flavor = row$flavor
  )
  event[[row$semantics]] <- row$value
  event
}
