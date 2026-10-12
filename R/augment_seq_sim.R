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
# Under coupling an interval is redrawn alone: each attempt replays the
# intervals before it, as drawn, and the observed events up to its start. The
# replay is not drawn, so it evaluates nothing: the attempt's first clock step
# applies it to the walk one stamp at a time, as the time-anchored simulation
# applies observed stamps, and moves the walk on to the interval's opening
# wave.
#
# Without a user support constraint no step can strand, provided no actor
# taking part in an interval joins or leaves inside it (estimate_dynes()
# snaps such changes to the waves before completion). Nothing is redrawn, so
# one walk carries every interval of the draw, and a strand means the
# precondition was bypassed: it aborts rather than restarts, so no silent
# completion normalizer enters the density.
# =========================================================================== #

# The sim augmenter over a completed specification and its recipe.
augment_seq_sim <- function(
  spec,
  recipe,
  control,
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  context <- new_augmenter_context(spec, recipe, control_prep, call = call)
  coupling <- support_constraint_labels(spec)
  plan <- sim_plan(context, coupled = length(coupling) > 0L)
  n_intervals <- length(recipe$wave_times) - 1L
  drawn <- which(tabulate(recipe$events$interval, n_intervals) > 0L)

  draw_one <- function(theta) {
    parameters <- flat_to_fid_theta(spec, theta, call = call)
    times <- rep(NA_real_, nrow(recipe$events))
    attempts <- rep(1L, n_intervals)
    log_q <- rep(0, n_intervals)
    if (plan$coupled) {
      for (k in drawn) {
        ids <- recipe$events$event[recipe$events$interval == k]
        accepted <- redraw_until_accepted(
          function() sim_interval_attempt(plan, k, times, parameters, call),
          function(attempt) attempt$completed,
          coupling,
          call = call
        )
        times[ids] <- accepted$value$times[ids]
        log_q[[k]] <- accepted$value$log_q[[k]]
        attempts[[k]] <- accepted$attempts
      }
    } else if (length(drawn) > 0L) {
      run <- sim_interval_attempt(plan, drawn, times, parameters, call)
      if (!run$completed) {
        sim_abort_uncoupled_strand(call)
      }
      times <- run$times
      log_q <- run$log_q
    }
    # Summed interval by interval, in order, as the per-interval draws were.
    total <- 0
    for (k in drawn) {
      total <- total + log_q[[k]]
    }
    completion <- acceptance_rate(rep(1L, n_intervals), attempts, theta)
    completion$interval <- seq_len(n_intervals)
    list(
      data = emit_augmented_data(context, times),
      log_proposal = total - sum(log(completion$rate)),
      restarted = any(attempts > 1L),
      log_proposal_raw = total,
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
  flips <- list(
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
    )
  )
  flips$rate_fid <- sim_rate_fid(map, flips$layer, flips$flavor)
  # The (layer, flavor, sender) pair each flip belongs to, and the row each
  # event id sits at, so a step finds its candidates by integer comparison.
  pair <- paste(flips$layer, flips$flavor, flips$sender, sep = "\r")
  flips$pair <- match(pair, unique(pair))
  flip_row <- rep(NA_integer_, max(c(0L, flips$event)))
  flip_row[flips$event] <- seq_along(flips$event)

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
    flip_row = flip_row,
    rate_fids = map$fid[map$family == "rate"],
    coupled = coupled
  )
}

# The observed events of the modeled processes up to the last wave, in the
# order of the observed stream, as columns: the initial condition (at or
# before the observation start, interval 0) and the relational events of each
# interval (w_{k-1}, w_k]. A flavor no process models is replayed by the
# driver itself.
sim_observed_events <- function(context, labels, update) {
  map <- context$spec$process_map
  waves <- context$recipe$wave_times
  ties <- as.data.frame(context$base$ties)
  flavor <- ties[["flavor"]] %||% rep(NA_character_, nrow(ties))
  modeled <- modeled_tie_rows(ties, map)
  keep <- modeled & !is.na(ties$time) & ties$time <= waves[[length(waves)]]
  ties <- ties[keep, , drop = FALSE]
  flavor <- flavor[keep]
  observed <- list(
    time = as.numeric(ties$time),
    interval = findInterval(ties$time, waves, left.open = TRUE),
    layer = ties$layer,
    flavor = flavor,
    sender = node_index(ties$from, labels),
    receiver = node_index(ties$to, labels),
    semantics = sim_semantics(update[ties$layer]),
    value = ties[["weight"]] %||% rep(1, nrow(ties))
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

# One attempt at the intervals `intervals`, in order, over the intervals
# before the first drawn at `times`: one interval under coupling, every
# interval with flips without. Returns `list(completed, times, log_q)`:
# whether every event of the intervals was placed, the times with theirs
# filled in, and the log density each interval accumulated over its steps
# (one entry per interval of the recipe).
sim_interval_attempt <- function(plan, intervals, times, parameters, call) {
  state <- new_sim_attempt(plan, intervals, times, parameters)
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
          "The sim draw of interval {state$k} stopped at
           {.val {run$diagnostics$stop_reason}} before placing every event.",
          .internal = TRUE,
          call = call
        )
      }
      TRUE
    },
    goldfish_dynes_sim_stranded = function(condition) FALSE
  )
  list(completed = completed, times = state$times, log_q = state$interval_log_q)
}

# The internal error of a strand on the single walk of an uncoupled draw.
sim_abort_uncoupled_strand <- function(call) {
  cli::cli_abort(
    c(
      "A sim draw without a support constraint stranded.",
      "i" = "It cannot strand once every composition change of an actor
             taking part in an interval sits on the interval's waves, as
             {.fn estimate_dynes} arranges before drawing."
    ),
    class = "goldfish_dynes_uncoupled_strand",
    .internal = TRUE,
    call = call
  )
}

# The mutable state one attempt's steps share: the intervals it draws and the
# one in progress, the forced events applied before that interval opens (as
# the columns replay_stamps() reads), the interval's relational events (as
# columns, a step addressing one by its position), the recipe cursor over the
# flips, the step in progress, the values read at the walk's current state,
# and the density of each interval. The driver draws only the intervals' own
# events.
new_sim_attempt <- function(plan, intervals, times, parameters) {
  observed <- plan$observed
  state <- new.env(parent = emptyenv())
  state$plan <- plan
  state$intervals <- intervals
  state$parameters <- parameters
  state$cursor <- new_recipe_cursor(plan$recipe)
  state$n_events <- sum(observed$interval %in% intervals) +
    sum(plan$flips$interval %in% intervals)
  state$times <- times
  state$pending <- NULL
  state$time <- NA_real_
  state$stranded <- FALSE
  state$log_q <- 0
  state$interval_log_q <- rep(0, length(plan$waves) - 1L)
  state$routing <- NULL
  # The first interval is entered past everything before it.
  state$k <- -1L
  sim_enter_interval(state, intervals[[1L]])
  sim_clear_cache(state)
  state
}

# Make interval `k` the one in progress. The events after the interval left
# and before `k` -- for the first interval, everything before it: the
# observed events and earlier intervals' flips, as drawn at `state$times` --
# are forced, and the next clock call applies them. Built once per interval.
sim_enter_interval <- function(state, k) {
  plan <- state$plan
  observed <- plan$observed
  flips <- plan$flips
  left <- state$k
  before <- which(observed$interval > left & observed$interval < k)
  earlier <- which(flips$interval > left & flips$interval < k)
  # Stable, so observed events sharing a stamp keep the stream's order; a
  # drawn flip never shares one.
  time <- c(observed$time[before], state$times[flips$event[earlier]])
  sorted <- order(time, method = "radix")
  forced <- function(column) {
    c(observed[[column]][before], flips[[column]][earlier])[sorted]
  }
  current <- which(observed$interval == k)
  relational <- lapply(
    observed[c("time", "layer", "flavor", "sender", "receiver", "semantics")],
    function(column) column[current]
  )
  relational$value <- observed$value[current]
  relational$rate_fid <- observed$rate_fid[current]
  state$k <- k
  state$wave_start <- plan$waves[[k]]
  state$wave_end <- plan$waves[[k + 1L]]
  state$forced <- if (length(time) > 0L) {
    list(
      time = time[sorted],
      layer = forced("layer"),
      sender = forced("sender"),
      receiver = forced("receiver"),
      field = forced("semantics"),
      value = forced("value"),
      stamp_end = stamp_ends(time[sorted])
    )
  }
  state$entered <- FALSE
  state$relational <- relational
  state$n_relational <- 0L
  state$n_relational_total <- length(current)
  state$flips_left <- sum(flips$interval == k)
}

# Whether the interval in progress has placed every event; if so, its density
# is set aside and the next interval of the attempt, if any, is entered.
sim_close_interval <- function(state) {
  if (state$flips_left > 0L || state$n_relational < state$n_relational_total) {
    return(invisible())
  }
  state$interval_log_q[[state$k]] <- state$log_q
  state$log_q <- 0
  at <- match(state$k, state$intervals)
  if (at < length(state$intervals)) {
    sim_enter_interval(state, state$intervals[[at + 1L]])
  }
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
      sim_mark(runner$state, evaluation, handle)
    },
    accept = function(event, handle) {
      if (is.null(runner$state)) {
        return(TRUE)
      }
      sim_accept(runner$state, event)
    }
  )
}

# P2. The next event and its waiting time. The attempt's first call applies
# the forced events and moves the walk up to the interval's opening wave. A
# step in progress -- a relational event the driver clipped at a breakpoint,
# or a flip whose draw was censored at one -- continues; otherwise a new
# candidate is selected.
sim_clock <- function(state, t, handle) {
  # The driver has moved the walk since the last step.
  sim_clear_cache(state)
  if (is.null(state$routing)) {
    state$routing <- simulation_routing(state$plan$spec, handle)
  }
  if (!state$entered) {
    sim_move(state, handle, state$wave_start, state$forced)
    state$forced <- NULL
    state$entered <- TRUE
  }
  # The walk already stands strictly before the wave that opens the interval;
  # the driver's clock follows it there, and the draw begins.
  if (is.null(state$pending) && t < state$wave_start) {
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
    state$time <- state$relational$time[[pending$event]]
    return(list(
      wait = state$time - t,
      fid = sim_event_fid(state, pending$event),
      kind = "event"
    ))
  }
  sim_flip_wait(state, pending, t, handle)
}

# The step that follows the last placed event: a candidate selected in
# proportion to its rate, else a strand. A relational event is its position
# in the attempt's relational columns.
sim_next_step <- function(state, t, handle) {
  relational <- NULL
  if (state$n_relational < state$n_relational_total) {
    relational <- state$n_relational + 1L
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
  # An event at the closing wave leaves no room after it for a flip of the
  # interval: it is not weighted, and comes, with certainty, once no flip of
  # the interval is left.
  if (
    !is.null(relational) &&
      state$relational$time[[relational]] == state$wave_end
  ) {
    if (state$flips_left == 0L) {
      return(list(kind = "relational", event = relational))
    }
    relational <- NULL
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
  if (chosen > length(options$head)) {
    return(list(kind = "relational", event = relational))
  }
  list(kind = "flip", option = options$head[[chosen]])
}

# Whether the next relational event shares the stamp of the one placed last.
sim_sibling_pending <- function(state) {
  n <- state$n_relational
  n > 0L &&
    n < state$n_relational_total &&
    state$relational$time[[n + 1L]] == state$relational$time[[n]]
}

# The (layer, flavor, sender) candidates at `t`: the heads of the interval's
# chains with a flip left, grouped by pair, each weighted by the pair's rate
# and kept only while some remaining receiver has support now. A pair is
# named by the flip row of its first head, `head`.
sim_flip_options <- function(state, t, handle) {
  flips <- state$plan$flips
  heads <- sim_chain_heads(state)
  head <- heads[!duplicated(flips$pair[heads])]
  rate_fid <- flips$rate_fid[head]
  weight <- vapply(
    seq_along(head),
    function(i) {
      sender <- flips$sender[[head[[i]]]]
      rate <- sim_fid_values(state, handle, rate_fid[[i]], t)[[sender]]
      if (!(rate > 0)) {
        return(0)
      }
      receivers <- sim_pair_receivers(flips, heads, head[[i]])
      choice <- sim_choice_row(state, handle, rate_fid[[i]], sender, t)
      if (!any(choice[receivers] > 0)) 0 else rate
    },
    numeric(1)
  )
  keep <- weight > 0
  list(head = head[keep], rate_fid = rate_fid[keep], weight = weight[keep])
}

# A relational event's selection weight: its sender's rate on its process
# (its dyad's, for a process whose rate ranges over dyads). Under a user
# support constraint an event whose support no longer applies cannot be
# placed, and gets no weight; without one, an observed event's support is
# only its own dyad's, which no flip of another dyad moves.
sim_relational_weight <- function(state, event, t, handle) {
  relational <- state$relational
  fid <- relational$rate_fid[[event]]
  sender <- relational$sender[[event]]
  receiver <- relational$receiver[[event]]
  if (is.na(fid)) {
    return(1)
  }
  values <- sim_fid_values(state, handle, fid, t)
  map <- state$plan$map
  choice_fid <- paired_choice_fid(map, fid)
  if (is.na(choice_fid)) {
    n2 <- length(values) / n_actors(handle)
    return(values[[(sender - 1L) * n2 + receiver]])
  }
  rate <- values[[sender]]
  if (state$plan$coupled && rate > 0) {
    choice <- sim_fid_values(state, handle, choice_fid, t, sender)
    if (!(choice[[receiver]] > 0)) {
      return(0)
    }
  }
  rate
}

# The waiting time of the flip in progress. Censored at a breakpoint before
# the anchor (the next relational event or the wave end); truncated at the
# anchor otherwise. The density of the part drawn is added to the attempt's.
sim_flip_wait <- function(state, pending, t, handle) {
  flips <- state$plan$flips
  fid <- flips$rate_fid[[pending$option]]
  anchor <- state$wave_end
  if (state$n_relational < state$n_relational_total) {
    anchor <- min(anchor, state$relational$time[[state$n_relational + 1L]])
  }
  rate <- sim_fid_values(state, handle, fid, t)[[
    flips$sender[[pending$option]]
  ]]
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
# remaining observed receivers by the choice probabilities at the walk's
# state; a stranded step hands accept() a marker instead. The driver does not
# move the walk between the clock and the mark, so the sender's choice row is
# the one the clock read when it weighted the pair.
sim_mark <- function(state, evaluation, handle) {
  pending <- state$pending
  if (identical(pending$kind, "stranded")) {
    return(list(layer = evaluation$layer, stranded = TRUE))
  }
  if (!identical(pending$kind, "flip")) {
    return(sim_event(state$relational, pending$event))
  }
  flips <- state$plan$flips
  option <- pending$option
  heads <- sim_chain_heads(state)
  receivers <- sim_pair_receivers(flips, heads, option)
  choice <- sim_choice_row(
    state,
    handle,
    flips$rate_fid[[option]],
    flips$sender[[option]],
    state$time
  )[receivers]
  total <- sum(choice)
  if (!(total > 0)) {
    state$pending <- list(kind = "stranded")
    return(list(layer = evaluation$layer, stranded = TRUE))
  }
  drawn <- sample_index(choice)
  state$log_q <- state$log_q + log(choice[[drawn]] / total)
  head <- heads[
    flips$pair[heads] == flips$pair[[option]] &
      flips$receiver[heads] == receivers[[drawn]]
  ][[1L]]
  state$pending$event_id <- flips$event[[head]]
  state$pending$chain <- flips$chain[[head]]
  sim_event(flips, head)
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
      state$flips_left <- state$flips_left - 1L
      state$times[[pending$event_id]] <- state$time
    }
  )
  state$pending <- NULL
  sim_close_interval(state)
  TRUE
}

# --------------------------------------------------------------------------- #
# Moving the walk
# --------------------------------------------------------------------------- #

# The one place the augmenter moves the walk itself rather than leaving it to
# the driver: replay `events` (the columns replay_stamps() reads) one stamp at
# a time, then advance strictly before `to`, the node composition at `to`
# included. Nothing is evaluated on the way. The walk then stands ahead of the
# driver's clock, which is safe because both only move forward and the
# driver's next advance is never earlier than `to`. The values read before the
# move describe a state the walk has left, so they are dropped.
sim_move <- function(state, handle, to, events = NULL) {
  if (!is.null(events)) {
    replay_stamps(handle, events)
  }
  walk_advance(handle, to, inclusive = FALSE)
  sim_clear_cache(state)
}

# --------------------------------------------------------------------------- #
# Reading the walk
# --------------------------------------------------------------------------- #

# A fid's values at the live state through the package evaluator, routed as
# the driver routes it: a modeled fid at its theta, a completed rate at the
# pinned plateau of the period holding `t`, a completed choice uniform.
#
# Each is evaluated once per walk state -- a rate fid's vector once, a choice
# fid's row once per sender -- and every later read at that state reuses it.
# Whatever moves the walk clears the cache: the driver between steps (so each
# clock call starts empty) and sim_move() within one.
sim_fid_values <- function(state, handle, fid, t, sender = NULL) {
  at <- match(fid, state$plan$map$fid)
  cached <- state$cache[[at]]
  if (is.null(sender)) {
    if (!is.null(cached)) {
      return(cached)
    }
  } else if (length(cached) >= sender && !is.null(cached[[sender]])) {
    return(cached[[sender]])
  }
  inputs <- fid_evaluation_inputs(
    state$routing,
    fid,
    default_evaluate,
    state$parameters,
    state$plan$spec$completed_rates %||% list(),
    t
  )
  values <- evaluate_fid(
    handle,
    fid,
    inputs$theta,
    inputs$step,
    sender = sender
  )
  if (is.null(sender)) {
    state$cache[[at]] <- values
  } else {
    # One slot per sender, so a row is stored as it is read.
    rows <- cached %||% list()
    rows[[sender]] <- values
    state$cache[[at]] <- rows
  }
  values
}

# One slot per fid of the process map, empty.
sim_clear_cache <- function(state) {
  state$cache <- vector("list", nrow(state$plan$map))
}

sim_choice_row <- function(state, handle, rate_fid, sender, t) {
  choice_fid <- paired_choice_fid(state$plan$map, rate_fid)
  if (is.na(choice_fid)) {
    cli::cli_abort(
      "A panel process without a choice sub-model cannot be augmented.",
      .internal = TRUE
    )
  }
  sim_fid_values(state, handle, choice_fid, t, sender = sender)
}

# The flip row of the next event of every chain of the interval with a flip
# left.
sim_chain_heads <- function(state) {
  state$plan$flip_row[state$cursor$heads(state$k)]
}

# The receivers of the heads `heads` that share the pair of flip row `head`.
sim_pair_receivers <- function(flips, heads, head) {
  unique(flips$receiver[heads[flips$pair[heads] == flips$pair[[head]]]])
}

# The fid the driver is told fired: the event's rate fid, or any rate fid for
# a process without one (the driver only reads it to build the evaluation).
sim_event_fid <- function(state, event) {
  fid <- state$relational$rate_fid[[event]]
  if (is.na(fid)) state$plan$rate_fids[[1L]] else fid
}

# The event at position `i` of the columns `columns`.
sim_event <- function(columns, i) {
  event <- list(
    layer = columns$layer[[i]],
    sender = columns$sender[[i]],
    receiver = columns$receiver[[i]],
    flavor = columns$flavor[[i]]
  )
  event[[columns$semantics[[i]]]] <- columns$value[[i]]
  event
}
