# =========================================================================== #
# augment_seq_random(): the uniform augmenter.
#
# Per interval of length L with n latent events it draws n iid uniform times,
# gives each same-dyad chain its drawn times in sorted order (so chain order
# holds by construction), and emits the interval in time order. A chain of c_j
# events has c_j! assignments collapsing onto one sorted configuration, so the
# log proposal density of the interval is
#
#   sum_j log(c_j!) - n log L - log P(legal),
#
# identical for every draw: the uniform proposal gives every configuration the
# same density.
#
# The draw is support-blind. Under cross-dyad support coupling (a constraint
# reading another latent event of the interval, or an observed event's
# constraint reading the panel) a uniform ordering can fire an event whose
# support is not yet applicable. Such a sequence has zero target density, or
# may make the walk error rather than return -Inf, so it would be dead weight
# in the pool. The ordering is therefore replayed against the support on the
# walk and redrawn until legal, which makes the delivered draw uniform over the
# legal configurations. P(legal) is the legal fraction of uniform orderings: a
# theta-free structural constant per interval, so it cancels in the normalized
# importance weights and random needs no plug-in-bias warning (contrast sim).
# For it to be the same constant in every draw it is estimated once, when the
# augmenter is built, as the acceptance rate of a pilot of redraws on a private
# stream; a per-draw 1/attempts would vary across draws and not cancel.
#
# At theta = 0 every rate is equal and every choice uniform, so the model's
# own endpoint-conditioned path distribution is this proposal: the sanctioned
# cold start. Away from 0 the target depends on the timing and order the
# uniform draws ignore and the importance weights degenerate, which a redraw
# cannot fix; set_augmenter_options() documents the caveat for users.
# =========================================================================== #

# The fewest attempts the pilot spends on a coupled interval.
legal_pilot_attempts <- function() 200L

# The random augmenter over a completed specification and its recipe.
augment_seq_random <- function(
  spec,
  recipe,
  control,
  call = rlang::caller_env()
) {
  context <- new_augmenter_context(spec, recipe, call = call)
  plan <- random_interval_plan(recipe)
  checker <- new_legality_checker(context)
  legal <- random_legal_pilot(plan, checker, control$seed %||% 1L, call)
  log_proposal <- sum(vapply(plan, `[[`, numeric(1), "log_density")) -
    sum(log(legal$rate))

  draw_one <- function() {
    times <- rep(NA_real_, nrow(recipe$events))
    restarted <- FALSE
    for (k in seq_along(plan)) {
      interval <- plan[[k]]
      if (is.null(checker) || length(interval$events) == 0L) {
        times[interval$events] <- random_interval_times(interval)
        next
      }
      accepted <- redraw_until_accepted(
        function() random_interval_times(interval),
        function(candidate) checker$legal(times, k, candidate),
        checker$constraint,
        call = call
      )
      times[interval$events] <- accepted$value
      restarted <- restarted || accepted$attempts > 1L
    }
    list(
      data = emit_augmented_data(context, times),
      log_proposal = log_proposal,
      restarted = restarted
    )
  }

  new_dynes_augmenter(
    draw = function(iteration, draw_index, theta) {
      lapply(draw_index, function(k) draw_one())
    },
    routine = "random",
    diagnostics = function() list(legal = legal)
  )
}

# Per interval: its recipe event ids, bounds, the chain of each event, and the
# closed-form log density of its uniform draw before the legality normalizer.
random_interval_plan <- function(recipe) {
  events <- recipe$events
  chains <- recipe$chains
  waves <- recipe$wave_times
  lapply(seq_len(length(waves) - 1L), function(k) {
    ids <- events$event[events$interval == k]
    lower <- waves[[k]]
    upper <- waves[[k + 1L]]
    list(
      events = ids,
      chain = events$chain[ids],
      lower = lower,
      upper = upper,
      log_density = sum(lfactorial(chains$length[chains$interval == k])) -
        length(ids) * log(upper - lower)
    )
  })
}

# One interval's times: iid uniform strictly inside the interval (a draw that
# rounds onto a wave is redrawn), then each chain's times sorted onto its
# steps. Events of a chain are contiguous in step order in the recipe.
random_interval_times <- function(interval) {
  n <- length(interval$events)
  # A panel unchanged between two waves leaves its interval with no latent
  # event; unsplit() cannot rebuild an empty vector.
  if (n == 0L) {
    return(numeric(0))
  }
  width <- interval$upper - interval$lower
  times <- interval$lower + stats::runif(n) * width
  on_wave <- times <= interval$lower | times >= interval$upper
  while (any(on_wave)) {
    times[on_wave] <- interval$lower + stats::runif(sum(on_wave)) * width
    on_wave <- times <= interval$lower | times >= interval$upper
  }
  chain <- interval$chain
  unsplit(lapply(split(times, chain), sort), chain)
}

# P(legal) per interval: exactly 1 without a coupled support constraint, else
# the acceptance rate of a pilot of at least legal_pilot_attempts() redraws,
# run on a private L'Ecuyer-CMRG stream so that building the augmenter reads
# and leaves the session's random stream as it was, and every draw records the
# same constant. The pilot walks the intervals in order, each over a legal
# draw of the ones before it, since an interval's legality is judged from the
# state its predecessors leave.
random_legal_pilot <- function(plan, checker, seed, call) {
  n_intervals <- length(plan)
  legal <- data.frame(
    interval = seq_len(n_intervals),
    successes = rep(NA_integer_, n_intervals),
    attempts = rep(NA_integer_, n_intervals),
    rate = rep(1, n_intervals)
  )
  if (is.null(checker)) {
    return(legal)
  }
  withr::with_seed(
    seed,
    .rng_kind = "L'Ecuyer-CMRG",
    {
      times <- rep(NA_real_, sum(lengths(lapply(plan, `[[`, "events"))))
      for (k in seq_len(n_intervals)) {
        interval <- plan[[k]]
        if (length(interval$events) == 0L) {
          next
        }
        successes <- 0L
        attempts <- 0L
        while (attempts < legal_pilot_attempts()) {
          accepted <- redraw_until_accepted(
            function() random_interval_times(interval),
            function(candidate) checker$legal(times, k, candidate),
            checker$constraint,
            call = call
          )
          successes <- successes + 1L
          attempts <- attempts + accepted$attempts
          times[interval$events] <- accepted$value
        }
        rate <- acceptance_rate(successes, attempts)
        legal$successes[k] <- rate$successes
        legal$attempts[k] <- rate$attempts
        legal$rate[k] <- rate$rate
      }
    }
  )
  legal
}

# The support-legality replay, or NULL when no modeled layer carries a user
# support constraint: without one, a flavor's derived constraint reads only its
# own dyad, whose chain order the draw keeps, so every ordering is legal.
#
# `legal(times, k, candidate)` rebuilds the sequence with interval `k` at
# `candidate` over the intervals before it at `times`, opens a walk on it, and
# replays every modeled event up to the end of interval `k` in time order,
# checking each event inside the interval against the live support before
# injecting it. Later intervals' events sit at fixed placeholder times: they
# are never reached. The walk is opened on the candidate itself because its
# clock starts at the first scheduled time, which a drawn event may precede.
new_legality_checker <- function(context) {
  constraint <- support_constraint_labels(context$spec)
  if (length(constraint) == 0L) {
    return(NULL)
  }
  recipe <- context$recipe
  waves <- recipe$wave_times
  placeholder <- legality_placeholder_times(recipe)
  update <- context$base$info$update
  labels <- as.data.frame(context$base$nodes)$label

  legal <- function(times, k, candidate) {
    ids <- recipe$events$event[recipe$events$interval == k]
    times[ids] <- candidate
    times[is.na(times)] <- placeholder[is.na(times)]
    data <- emit_augmented_data(context, times)
    handle <- open_augmenter_walk(context, data)
    ties <- as.data.frame(data$ties)
    rows <- which(
      ties$layer %in%
        handle$focal_layers &
        !is.na(ties$time) &
        ties$time <= waves[[k + 1L]]
    )
    for (r in rows) {
      t <- ties$time[[r]]
      layer <- ties$layer[[r]]
      sender <- node_index(ties$from[[r]], labels)
      receiver <- node_index(ties$to[[r]], labels)
      walk_advance(handle, t, inclusive = FALSE)
      if (t > waves[[k]]) {
        flavor <- ties[["flavor"]][r] %||% NA_character_
        if (!walk_event_supported(handle, layer, flavor, sender, receiver)) {
          return(FALSE)
        }
      }
      value <- ties[["weight"]][r] %||% 1
      event <- list(layer = layer, sender = sender, receiver = receiver)
      event[[
        if (identical(update[[layer]], "replace")) "replace" else "increment"
      ]] <-
        value
      event$time <- t
      walk_inject(handle, event)
    }
    TRUE
  }
  list(legal = legal, constraint = constraint)
}

# The user support constraints of a joint specification, labeled by the layer
# they constrain, as the restart abort and the coupling warning name them.
support_constraint_labels <- function(spec) {
  constrained <- Filter(
    function(spec) !is.null(spec$support_constraint),
    spec$specifications
  )
  vapply(
    constrained,
    function(spec) paste0(spec$focal, ": ", deparse1(spec$support_constraint)),
    character(1)
  )
}

# Distinct in-interval times for every recipe event, in step order per chain,
# for the intervals a legality replay has not drawn yet.
legality_placeholder_times <- function(recipe) {
  events <- recipe$events
  waves <- recipe$wave_times
  times <- numeric(nrow(events))
  for (k in seq_len(length(waves) - 1L)) {
    ids <- events$event[events$interval == k]
    width <- waves[[k + 1L]] - waves[[k]]
    times[ids] <- waves[[k]] + width * seq_along(ids) / (length(ids) + 1L)
  }
  times
}

node_index <- function(node, labels) {
  if (is.character(node)) match(node, labels) else as.integer(node)
}

# Whether the live support admits `sender -> receiver` on the processes of
# `layer` (and `flavor`) now: the sender in every such fid's risk set and, for
# a choice, the receiver in the sender's. Fids the walk compiled no engine for
# carry no constraint of their own to read.
walk_event_supported <- function(handle, layer, flavor, sender, receiver) {
  map <- handle$process_map
  fids <- map$fid[map$layer == layer & map$flavor %in% flavor]
  for (fid in setdiff(fids, handle$deferred$fid)) {
    engine <- walk_engine_of_fid(handle, fid)
    risk_set <- walk_risk_set(handle, engine, fid)
    if (!isTRUE(risk_set$active_sender[[sender]])) {
      return(FALSE)
    }
    if (engine$is_sender) {
      next
    }
    dyad <- risk_set$active_dyad
    supported <- if (is.matrix(dyad)) {
      dyad[sender, receiver]
    } else {
      dyad[[receiver]]
    }
    if (!isTRUE(supported)) {
      return(FALSE)
    }
  }
  TRUE
}
