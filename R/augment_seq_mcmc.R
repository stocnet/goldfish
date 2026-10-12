# =========================================================================== #
# augment_seq_mcmc(): the Metropolis-Hastings augmenter.
#
# One serial chain runs over the whole sequence, its state the times of every
# latent event of the recipe. A move stays inside one between-wave interval:
#
# * permute picks a pair of the interval's events from different chains,
#   uniformly among such pairs, swaps their slots in the interval's time
#   order and redraws both times;
# * shift picks one event uniformly and redraws its time between its
#   neighbors, so relative order among panel events holds and only its
#   position relative to observed relational events changes (this is what
#   moves an interval holding a single event).
#
# Windows follow one rule: with pred/succ the neighbors of a slot among the
# panel events that do not move, padded by the wave boundaries, the event
# moved into the earlier slot h draws on (pred(h), succ(h)) and the one moved
# into the later slot k on (max(new time of the first, pred(k)), succ(k)).
# Each time is a truncated exponential at the moved pair's rate frozen at the
# state after the preceding panel event (the window's lower anchor) of the
# sequence the move starts from: forward rates are read on the current
# sequence, reverse rates on the candidate. The acceptance ratio is
#
#   alpha = [f(candidate) / f(current)] * [q_rev / q_fwd],
#
# the move-type and pair probabilities cancelling because they depend only on
# the recipe (event counts, interval membership, chains), never on the order.
# Two steps of one chain never pair; after a swap each chain's times are put
# back on its steps in sorted order, so chain order holds by construction.
#
# Cross-dyad support is not screened out of the move set, which would make it
# depend on the state and break that cancellation. A candidate whose ordering
# fires a flip without applicable support has zero target density: a support
# lookup rejects it (alpha = 0) before its likelihood is computed, an ordinary
# rejected step with no normalizer, since the target genuinely has no mass
# there.
#
# The target density and the rates come from the proposal evaluator the loop
# injects each EM iteration through init(). A retained draw is the chain's
# current accepted state with the log-likelihood it was accepted at, never a
# rejected candidate's; with the target as the proposal, that log-likelihood
# is also the draw's log proposal density.
# =========================================================================== #

# Sweeps (as many proposals as panel events) run when burn_in / thinning are
# left unset. Placeholders until a tuning study pins them.
mcmc_default_burn_in <- function() 10L
mcmc_default_thinning <- function() 1L

# The MCMC augmenter over a completed specification and its recipe.
augment_seq_mcmc <- function(
  spec,
  recipe,
  control,
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  context <- new_augmenter_context(spec, recipe, control_prep, call = call)
  chain <- new_mcmc_chain(context, control, call = call)

  new_dynes_augmenter(
    draw = function(iteration, draw_index, theta) {
      mcmc_draw(chain, draw_index, theta)
    },
    routine = "mcmc",
    init = function(proposal_evaluator, theta) {
      mcmc_attach(chain, proposal_evaluator)
    },
    diagnostics = function() mcmc_diagnostics(chain),
    coupling = support_constraint_labels(spec)
  )
}

# The chain: the theta-free plan it moves over and its mutable state. `times`
# holds one time per recipe event, `data` the sequence they emit and `loglik`
# its log-likelihood under the current target; the three change together.
new_mcmc_chain <- function(context, control, call = rlang::caller_env()) {
  options <- control$augmenter
  recipe <- context$recipe
  events <- recipe$events
  labels <- as.data.frame(context$base$nodes)$label
  focal <- unique(vapply(
    context$spec$specifications,
    function(spec) spec$focal,
    character(1)
  ))
  ties <- as.data.frame(context$base$ties)
  observed <- ties$time[ties$layer %in% focal & !is.na(ties$time)]

  chain <- new.env(parent = emptyenv())
  chain$context <- context
  chain$control <- control
  chain$call <- call
  chain$burn_in <- options$burn_in %||% mcmc_default_burn_in()
  chain$thinning <- options$thinning %||% mcmc_default_thinning()
  chain$move_probs <- options$move_probs %||% c(permute = 0.5, shift = 0.5)
  chain$initialize <- options$initialize %||% "random"
  chain$catalog <- mcmc_move_catalog(recipe)
  chain$intervals <- mcmc_interval_plan(recipe)
  # Without a user support constraint every ordering is legal, so a move
  # asks the evaluator for no legality (new_legality_checker()).
  chain$constrained <- length(support_constraint_labels(context$spec)) > 0L
  # Each event's pair as a rate query names it.
  chain$pair <- data.frame(
    layer = events$layer,
    flavor = as.character(events$flavor),
    sender = labels[node_index(events$from, labels)],
    receiver = labels[node_index(events$to, labels)]
  )
  # The modeled layers' observed timed rows: with the latent events they are
  # the candidate's dependent events a rate query's `after` counts.
  chain$observed <- sort(observed)
  chain$initial_times <- if (!is.null(options$initial_sequence)) {
    mcmc_times_from_data(context, options$initial_sequence, call = call)
  }
  chain$times <- NULL
  chain$data <- NULL
  chain$loglik <- NA_real_
  chain$start_log_proposal <- NA_real_
  chain$evaluator <- NULL
  chain$burn_pending <- TRUE
  moves <- c(permute = 0L, shift = 0L)
  chain$counts <- list(
    proposals = moves,
    accepted = moves,
    rejected_illegal = 0L,
    sweeps = 0L,
    burn_in_sweeps = 0L
  )
  chain
}

# The pairs a permute may swap, across every interval, and the events a shift
# may move. Both are fixed by the recipe, so a pick's probability does not
# depend on the chain's state.
mcmc_move_catalog <- function(recipe) {
  pairs <- lapply(seq_along(recipe$pairs), function(k) {
    pairs <- recipe$pairs[[k]]
    data.frame(
      interval = rep(k, nrow(pairs)),
      first = unname(pairs[, "first"]),
      second = unname(pairs[, "second"])
    )
  })
  list(
    pairs = do.call(rbind, pairs),
    events = recipe$events$event,
    n_events = nrow(recipe$events)
  )
}

mcmc_interval_plan <- function(recipe) {
  events <- recipe$events
  waves <- recipe$wave_times
  lapply(seq_len(length(waves) - 1L), function(k) {
    list(
      events = events$event[events$interval == k],
      lower = waves[[k]],
      upper = waves[[k + 1L]]
    )
  })
}

# --- Lifecycle ----------------------------------------------------------------

# Place the chain at `times`. Its log-likelihood is unknown until a target
# scores it.
mcmc_start <- function(chain, times) {
  chain$times <- times
  chain$data <- emit_augmented_data(chain$context, times)
  chain$loglik <- NA_real_
  invisible(chain)
}

# A new target (one per EM iteration): the chain keeps its state -- the warm
# start -- rescored under the new target, and burns in again before its next
# retained draw, since that state was drawn for the previous target.
mcmc_attach <- function(chain, evaluator) {
  chain$evaluator <- evaluator
  chain$burn_pending <- TRUE
  if (!is.null(chain$times)) {
    chain$loglik <- evaluator(chain$data)$loglik
  }
  invisible(chain)
}

# The chain's first state: the user's initial sequence, else one draw of the
# `initialize` routine at `theta`. Returns the times and that draw's log
# proposal density (zero for a user sequence, which has none).
mcmc_initial_times <- function(chain, theta) {
  if (!is.null(chain$initial_times)) {
    return(list(times = chain$initial_times, log_proposal = 0))
  }
  constructor <- switch(
    chain$initialize,
    random = augment_seq_random,
    sim = augment_seq_sim
  )
  context <- chain$context
  initializer <- constructor(
    context$spec,
    context$recipe,
    chain$control,
    control_prep = context$control_prep,
    call = chain$call
  )
  draw <- initializer$draw(0L, 1L, theta)[[1L]]
  list(
    times = mcmc_times_from_data(context, draw$data, call = chain$call),
    log_proposal = draw$log_proposal
  )
}

# One retained draw per entry of `draw_index`, each `thinning` sweeps after
# the last, after `burn_in` sweeps whenever the target changed. A draw before
# any target is injected (the warm-start fit's) returns the chain's start.
mcmc_draw <- function(chain, draw_index, theta) {
  if (is.null(chain$times)) {
    start <- mcmc_initial_times(chain, theta)
    mcmc_start(chain, start$times)
    chain$start_log_proposal <- start$log_proposal
  }
  if (is.null(chain$evaluator)) {
    return(lapply(draw_index, function(k) {
      list(
        data = chain$data,
        log_proposal = chain$start_log_proposal,
        restarted = FALSE
      )
    }))
  }
  if (is.na(chain$loglik)) {
    chain$loglik <- chain$evaluator(chain$data)$loglik
  }
  if (chain$burn_pending) {
    mcmc_sweeps(chain, chain$burn_in)
    chain$counts$burn_in_sweeps <- chain$counts$burn_in_sweeps + chain$burn_in
    chain$burn_pending <- FALSE
  }
  lapply(draw_index, function(k) {
    mcmc_sweeps(chain, chain$thinning)
    list(
      data = chain$data,
      log_proposal = chain$loglik,
      restarted = FALSE,
      loglik = chain$loglik
    )
  })
}

mcmc_sweeps <- function(chain, n) {
  for (sweep in seq_len(n)) {
    for (step in seq_len(chain$catalog$n_events)) {
      mcmc_step(chain)
    }
    chain$counts$sweeps <- chain$counts$sweeps + 1L
  }
  invisible(chain)
}

mcmc_diagnostics <- function(chain) {
  counts <- chain$counts
  c(
    counts,
    list(
      acceptance_rate = sum(counts$accepted) / max(1L, sum(counts$proposals))
    )
  )
}

# --- One step -----------------------------------------------------------------

# Propose one move (a picked one unless `move` names it), accept or reject it,
# and return the step's record.
mcmc_step <- function(chain, move = NULL) {
  move <- move %||% mcmc_pick_move(chain)
  if (is.null(move)) {
    return(invisible(NULL))
  }
  current <- chain$times
  placements <- switch(
    move$type,
    permute = mcmc_permute_placements(chain, current, move$events),
    shift = mcmc_shift_placements(chain, current, move$event)
  )
  chain$counts$proposals[[move$type]] <- chain$counts$proposals[[move$type]] +
    1L

  forward <- mcmc_forward(chain, current, placements)
  proposed <- mcmc_relabel(forward$times, chain$context$recipe$events$chain)
  record <- list(
    move = move,
    current = current,
    proposed = proposed,
    forward = forward[c("queries", "rates", "log_q")],
    legal = TRUE,
    accepted = FALSE,
    log_alpha = -Inf
  )

  # The candidate's legality and its reverse rates are read on one walk, and
  # an illegal candidate is rejected unscored.
  data <- emit_augmented_data(chain$context, proposed)
  reverse_queries <- mcmc_queries(chain, proposed, placements$reverse)
  scored <- if (chain$constrained) {
    chain$evaluator(
      data,
      rate_queries = reverse_queries,
      legality = list(
        interval = placements$interval,
        waves = chain$context$recipe$wave_times
      )
    )
  } else {
    chain$evaluator(data, rate_queries = reverse_queries)
  }
  if (isFALSE(scored$legal)) {
    chain$counts$rejected_illegal <- chain$counts$rejected_illegal + 1L
    record$legal <- FALSE
    return(invisible(record))
  }
  reverse <- list(
    queries = reverse_queries,
    rates = scored$rates,
    log_q = mcmc_log_q(placements$reverse, scored$rates, chain$call)
  )
  log_alpha <- mcmc_log_alpha(
    scored$loglik,
    chain$loglik,
    reverse$log_q - forward$log_q
  )
  accepted <- log(stats::runif(1L)) < log_alpha
  if (accepted) {
    chain$times <- proposed
    chain$data <- data
    chain$loglik <- scored$loglik
    chain$counts$accepted[[move$type]] <- chain$counts$accepted[[move$type]] +
      1L
  }
  record$data <- data
  record$loglik <- scored$loglik
  record$reverse <- reverse
  record$log_alpha <- log_alpha
  record$accepted <- accepted
  invisible(record)
}

# The move type by `move_probs`, falling back to the other type when the
# chosen one has no move (a recipe with no pair), then a uniform pick of its
# pair or event. Returns NULL when there is nothing to move.
mcmc_pick_move <- function(chain) {
  catalog <- chain$catalog
  if (catalog$n_events == 0L) {
    return(NULL)
  }
  probs <- chain$move_probs
  p_permute <- if ("permute" %in% names(probs)) probs[["permute"]] else 0
  n_pairs <- nrow(catalog$pairs)
  type <- if (stats::runif(1L) < p_permute) "permute" else "shift"
  if (identical(type, "permute") && n_pairs == 0L) {
    type <- "shift"
  }
  if (identical(type, "permute")) {
    row <- sample.int(n_pairs, 1L)
    return(list(
      type = "permute",
      events = c(catalog$pairs$first[[row]], catalog$pairs$second[[row]])
    ))
  }
  list(
    type = "shift",
    event = catalog$events[[sample.int(catalog$n_events, 1L)]]
  )
}

# The placements of a permute of events `events` on `times`: the event in the
# later slot k moves into the earlier slot h, then the event in slot h into
# slot k, bounded below by the first's new time. The reverse move from the
# candidate puts each back at its current time in the same windows. Every
# window bound and rate anchor is an event that does not move, or a wave, so
# it is the same time in the current sequence and the candidate.
mcmc_permute_placements <- function(chain, times, events) {
  interval <- chain$context$recipe$events$interval[[events[[1L]]]]
  plan <- chain$intervals[[interval]]
  slots <- plan$events[order(times[plan$events])]
  n <- length(slots)
  position <- match(events, slots)
  h <- min(position)
  k <- max(position)
  x <- slots[[k]]
  y <- slots[[h]]
  time_at <- function(slot, wave) {
    if (slot >= 1L && slot <= n) times[[slots[[slot]]]] else wave
  }
  pred_h <- time_at(h - 1L, plan$lower)
  succ_h <- time_at(if (h + 1L == k) k + 1L else h + 1L, plan$upper)
  pred_k <- time_at(if (k - 1L == h) h - 1L else k - 1L, plan$lower)
  succ_k <- time_at(k + 1L, plan$upper)
  list(
    interval = interval,
    forward = list(
      mcmc_placement(x, pred_h, succ_h),
      mcmc_placement(y, pred_k, succ_k, after_previous = TRUE)
    ),
    reverse = list(
      mcmc_placement(y, pred_h, succ_h, target = times[[y]]),
      mcmc_placement(
        x,
        pred_k,
        succ_k,
        after_previous = TRUE,
        target = times[[x]]
      )
    )
  )
}

# The placement of a shift of `event`: a redraw between its neighbors, and
# back to its current time.
mcmc_shift_placements <- function(chain, times, event) {
  interval <- chain$context$recipe$events$interval[[event]]
  plan <- chain$intervals[[interval]]
  slots <- plan$events[order(times[plan$events])]
  i <- match(event, slots)
  pred <- if (i > 1L) times[[slots[[i - 1L]]]] else plan$lower
  succ <- if (i < length(slots)) times[[slots[[i + 1L]]]] else plan$upper
  list(
    interval = interval,
    forward = list(mcmc_placement(event, pred, succ)),
    reverse = list(mcmc_placement(event, pred, succ, target = times[[event]]))
  )
}

# One event placed on (lower, upper) -- or, `after_previous`, on (max(lower,
# the previous placement's time), upper) -- at its pair's rate frozen at the
# state after the events at or before `lower`. `target` is the time a reverse
# placement returns the event to.
mcmc_placement <- function(
  event,
  lower,
  upper,
  after_previous = FALSE,
  target = NA_real_
) {
  list(
    event = event,
    lower = lower,
    upper = upper,
    anchor = lower,
    after_previous = after_previous,
    target = target
  )
}

# Draw the forward placements on the current sequence: their rates read there
# (rates only; its log-likelihood is held), their times drawn in order.
mcmc_forward <- function(chain, times, placements) {
  queries <- mcmc_queries(chain, times, placements$forward)
  rates <- chain$evaluator(
    chain$data,
    rate_queries = queries,
    loglik = FALSE
  )$rates
  check_mcmc_rates(rates, queries, chain$call)
  log_q <- 0
  previous <- -Inf
  for (i in seq_along(placements$forward)) {
    placement <- placements$forward[[i]]
    lower <- mcmc_lower(placement, previous)
    t <- trunc_exp_draw(rates[[i]], lower, placement$upper)
    log_q <- log_q +
      trunc_exp_log_density(t, rates[[i]], lower, placement$upper)
    times[[placement$event]] <- t
    previous <- t
  }
  list(times = times, queries = queries, rates = rates, log_q = log_q)
}

# The reverse density of placements whose rates were read on the candidate.
mcmc_log_q <- function(placements, rates, call) {
  log_q <- 0
  previous <- -Inf
  for (i in seq_along(placements)) {
    placement <- placements[[i]]
    lower <- mcmc_lower(placement, previous)
    log_q <- log_q +
      trunc_exp_log_density(
        placement$target,
        rates[[i]],
        lower,
        placement$upper
      )
    previous <- placement$target
  }
  log_q
}

mcmc_lower <- function(placement, previous) {
  if (placement$after_previous) {
    max(placement$lower, previous)
  } else {
    placement$lower
  }
}

# The rate queries of placements on the sequence at `times`: each moved pair
# after the dependent events stamped at or before its window's lower anchor --
# the preceding panel event's position among the sequence's modeled events,
# or, at a wave boundary, every modeled event up to the wave.
mcmc_queries <- function(chain, times, placements) {
  lapply(placements, function(placement) {
    pair <- chain$pair[placement$event, ]
    list(
      after = mcmc_ordinal(chain, times, placement$anchor),
      layer = pair$layer,
      flavor = pair$flavor,
      sender = pair$sender,
      receiver = pair$receiver
    )
  })
}

# How many of the sequence's modeled events fall at or before `time`. A
# latent event never ties an observed one or a wave (its times are continuous
# draws strictly inside an interval), so the count is the position of the
# event at `time`.
mcmc_ordinal <- function(chain, times, time) {
  as.integer(sum(chain$observed <= time) + sum(times <= time))
}

# Each chain's times back on its steps in order, so a swap that moved a step
# past a later step of its own chain relabels them instead of breaking chain
# order. Events of a chain are contiguous in step order in the recipe.
mcmc_relabel <- function(times, chain) {
  unsplit(lapply(split(times, chain), sort), chain)
}

# log alpha from the target's log-likelihoods and the proposal log-ratio. A
# candidate with no target mass is rejected; a current state with none is left
# for any candidate that has some.
mcmc_log_alpha <- function(loglik_proposed, loglik_current, log_q_ratio) {
  if (!is.finite(loglik_proposed) && loglik_proposed < 0) {
    return(-Inf)
  }
  if (!is.finite(loglik_current) && loglik_current < 0) {
    return(Inf)
  }
  loglik_proposed - loglik_current + log_q_ratio
}

check_mcmc_rates <- function(rates, queries, call) {
  if (
    length(rates) != length(queries) ||
      anyNA(rates) ||
      any(rates < 0)
  ) {
    cli::cli_abort(
      "The proposal evaluator must answer each rate query with one
       non-negative rate.",
      .internal = TRUE,
      call = call
    )
  }
  invisible(rates)
}

# --- Truncated exponential ----------------------------------------------------

# A time on (lower, upper) with density proportional to rate * e^(-rate (t -
# lower)), by inverse CDF; a draw rounding onto either end is redrawn. A rate
# too small to bend the window is uniform, its limit.
trunc_exp_draw <- function(rate, lower, upper) {
  width <- upper - lower
  repeat {
    u <- stats::runif(1L)
    wait <- if (rate * width < 1e-10) {
      u * width
    } else {
      -log1p(u * expm1(-rate * width)) / rate
    }
    t <- lower + wait
    if (t > lower && t < upper) {
      return(t)
    }
  }
}

trunc_exp_log_density <- function(t, rate, lower, upper) {
  width <- upper - lower
  if (rate * width < 1e-10) {
    return(-log(width))
  }
  log(rate) - rate * (t - lower) - log(-expm1(-rate * width))
}

# --- A sequence's latent times ------------------------------------------------

# The time of every recipe event in a sequence: the timed rows of the modeled
# panel layers strictly inside the wave grid, matched to the recipe's per-dyad
# chains by layer, interval and dyad, each chain's times in order onto its
# steps. A sequence that places any change on a wave or outside the grid, or
# that gives a chain the wrong number of events, does not hit the waves.
mcmc_times_from_data <- function(context, data, call = rlang::caller_env()) {
  bind_candidate_data(context$spec, data, call = call)
  recipe <- context$recipe
  waves <- recipe$wave_times
  chains <- recipe$chains
  labels <- as.data.frame(context$base$nodes)$label
  ties <- as.data.frame(data$ties)
  rows <- ties[
    ties$layer %in%
      names(recipe$layers) &
      !is.na(ties$time) &
      ties$time > waves[[1L]] &
      ties$time <= waves[[length(waves)]],
    ,
    drop = FALSE
  ]
  interval <- findInterval(rows$time, waves, left.open = TRUE)
  on_wave <- rows$time %in% waves
  key <- function(layer, interval, from, to) {
    paste(
      layer,
      interval,
      labels[node_index(from, labels)],
      labels[node_index(to, labels)],
      sep = "\r"
    )
  }
  row_key <- key(rows$layer, interval, rows$from, rows$to)
  chain_key <- key(chains$layer, chains$interval, chains$from, chains$to)

  times <- rep(NA_real_, nrow(recipe$events))
  wrong_count <- 0L
  for (i in seq_len(nrow(chains))) {
    placed <- sort(rows$time[!on_wave & row_key == chain_key[[i]]])
    if (length(placed) != chains$length[[i]]) {
      wrong_count <- wrong_count + 1L
      next
    }
    times[chains$first[[i]] + seq_along(placed) - 1L] <- placed
  }
  unmatched <- sum(!on_wave & !row_key %in% chain_key)
  if (any(on_wave) || wrong_count > 0L || unmatched > 0L) {
    cli::cli_abort(
      c(
        "The initial sequence does not hit the panel waves.",
        "x" = if (any(on_wave)) {
          "{sum(on_wave)} change{?s} fall{?s/} on a wave time."
        },
        "x" = if (wrong_count > 0L) {
          "{wrong_count} per-dyad chain{?s} {?carries/carry} the wrong number
           of events."
        },
        "x" = if (unmatched > 0L) {
          "{unmatched} change{?s} {?is/are} not in the wave diff."
        },
        "i" = "An initial sequence must place every change the waves record
               strictly inside its between-wave interval."
      ),
      class = "goldfish_dynes_invalid_initial_sequence",
      call = call
    )
  }
  times
}
