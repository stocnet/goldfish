# A seeded data-generating process for the simulation cross-validation tests,
# with the oracle frames built inside the same loop.
#
# The process is an exact-time DyNAM: the waiting time is an exponential draw
# from the total rate over the senders at risk, the sender is drawn in
# proportion to its rate, and the receiver from the multinomial over the
# receivers it may choose. Before each event is applied, the loop writes the
# rows a classical fit of the same likelihood needs, from its own state and
# with the rule the process itself used:
#
# - the rate frame: one row per sender at risk per interval (`outcome`, `dt`,
#   the statistics), which `glm(poisson, offset(log(dt)))` fits;
# - the choice frame: one row per receiver the sender may choose per
#   dependent event (`chosen`, `event`, the statistics), which a conditional
#   logit stratified by event fits;
# - the dyad frame: one row per dyad at risk per interval, for the REM rate.
#
# The risk-set rule: an absent node has no row on either axis; a sender
# whose every present receiver is excluded has no rate row (the row
# reduction a dyadic mask implies); and every event that is not a dependent
# event of a frame's process -- an exogenous event, a composition change, an
# event of another flavor -- closes the open interval with a censored row
# (`outcome = 0`) and opens a new one.
#
# Nothing here reads goldfish's walk or effect code: the state is a 0/1
# adjacency matrix updated in the loop, and each statistic is written out
# from its definition on that matrix, so an agreement with goldfish is
# evidence about goldfish.

# Statistics of the focal state `tie` (0/1, zero diagonal), one per effect the
# fixtures use. The rate's degree counts are present ties; `trans` is the
# two-path count i -> k -> j.
dgp_rate_stats <- function(tie) {
  cbind(indeg = colSums(tie), outdeg = rowSums(tie))
}

dgp_choice_stats <- function(tie, sender) {
  cbind(
    recip = tie[, sender],
    trans = drop(tie[sender, ] %*% tie)
  )
}

# A flavor's process on a mutually exclusive layer: creation targets an absent
# tie, dissolution a present one. Without flavors the single process may
# target any receiver, a repeated tie leaving the 0/1 state unchanged.
dgp_flavor_mask <- function(tie, flavor) {
  switch(
    flavor %||% "none",
    creation = tie == 0,
    dissolution = tie == 1,
    none = matrix(TRUE, nrow(tie), ncol(tie))
  )
}

# The receivers each sender may choose: present, not itself, allowed by the
# flavor's derived mask and by the user constraint.
dgp_allowed <- function(tie, exogenous_tie, present, flavor, constraint) {
  allowed <- dgp_flavor_mask(tie, flavor)
  if (!is.null(constraint)) {
    allowed <- allowed & constraint(tie, exogenous_tie)
  }
  allowed <- allowed & outer(present, present)
  diag(allowed) <- FALSE
  allowed
}

# Simulate `n_events` dependent events on `n_actors` nodes.
#
# `processes` is a named list with one entry per modeled flavor (or one
# unnamed entry, `list(list(...))`, for an unflavored layer), each holding
# `rate` (intercept, indeg, outdeg) and `choice` (recip, trans) coefficients.
# `initial` (a two-column from/to matrix) seeds the focal state; `exogenous`
# (time, from, to) is a second layer's event stream, which `constraint`
# (`function(tie, exogenous_tie)` returning a logical allowed matrix) may
# read; `composition` (time, node, present) switches nodes in and out.
# `unmodeled` names flavors whose events happen, drawn with their own
# coefficients, but are no process's dependent events in the frames -- the
# single-modeled-flavor case. `rem = TRUE` also writes the dyad frame.
simulate_relational_sequence <- function(
  n_actors,
  n_events,
  processes,
  seed,
  initial = NULL,
  exogenous = NULL,
  constraint = NULL,
  composition = NULL,
  unmodeled = character(0),
  rem = FALSE
) {
  withr::local_seed(seed)
  flavors <- names(processes)
  flavored <- !is.null(flavors)
  keys <- if (flavored) flavors else "none"
  names(processes) <- keys

  tie <- matrix(0, n_actors, n_actors)
  if (!is.null(initial)) {
    tie[initial] <- 1
  }
  exogenous_tie <- matrix(0, n_actors, n_actors)
  present <- rep(TRUE, n_actors)

  exogenous <- exogenous %||% data.frame(time = numeric(0))
  composition <- composition %||% data.frame(time = numeric(0))
  breaks <- sort(c(exogenous$time, composition$time))
  n_intervals <- n_events + length(breaks)

  rate_frames <- lapply(keys, function(k) {
    dgp_new_frame(n_intervals * n_actors, c("indeg", "outdeg"))
  })
  names(rate_frames) <- keys
  choice_frames <- lapply(keys, function(k) {
    dgp_new_frame(n_events * n_actors, c("recip", "trans"))
  })
  names(choice_frames) <- keys
  dyad_frame <- if (rem) {
    dgp_new_frame(n_intervals * n_actors^2, c("indeg", "outdeg"))
  }

  events <- data.frame(
    time = numeric(n_events),
    from = integer(n_events),
    to = integer(n_events),
    flavor = character(n_events),
    stringsAsFactors = FALSE
  )
  n_rate <- stats::setNames(integer(length(keys)), keys)
  n_choice <- n_rate
  n_dyad <- 0L
  n_dependent <- stats::setNames(integer(length(keys)), keys)
  now <- 0
  interval <- 0L
  n_drawn <- 0L
  next_break <- 1L

  while (n_drawn < n_events) {
    interval <- interval + 1L
    allowed <- lapply(keys, function(k) {
      dgp_allowed(tie, exogenous_tie, present, if (flavored) k, constraint)
    })
    names(allowed) <- keys
    rate_stats <- dgp_rate_stats(tie)
    at_risk <- lapply(allowed, function(a) which(rowSums(a) > 0))
    rates <- lapply(keys, function(k) {
      theta <- processes[[k]]$rate
      s <- rate_stats[at_risk[[k]], , drop = FALSE]
      exp(theta[1] + drop(s %*% theta[-1]))
    })
    names(rates) <- keys
    total <- sum(vapply(rates, sum, numeric(1)))
    wait <- stats::rexp(1, total)
    break_time <- if (next_break <= length(breaks)) breaks[next_break] else Inf

    exogenous_now <- now + wait >= break_time
    end <- if (exogenous_now) break_time else now + wait
    dt <- end - now

    # Who moved, if anyone: the drawn flavor and sender of this interval.
    mover <- NULL
    if (!exogenous_now) {
      flat <- unlist(rates, use.names = FALSE)
      pick <- sample.int(length(flat), 1L, prob = flat / sum(flat))
      owner <- rep(keys, lengths(rates))[pick]
      sender <- unlist(at_risk, use.names = FALSE)[pick]
      mover <- list(flavor = owner, sender = sender)
    }

    for (k in keys) {
      if (k %in% unmodeled) {
        next
      }
      senders <- at_risk[[k]]
      moved <- !is.null(mover) && identical(mover$flavor, k)
      rows <- n_rate[[k]] + seq_along(senders)
      rate_frames[[k]] <- dgp_fill(
        rate_frames[[k]],
        rows,
        interval = interval,
        sender = senders,
        receiver = NA_integer_,
        outcome = as.integer(moved & senders == (mover$sender %||% 0L)),
        dt = dt,
        dependent = moved,
        stats = rate_stats[senders, , drop = FALSE]
      )
      n_rate[[k]] <- n_rate[[k]] + length(senders)
    }

    if (rem) {
      dyads <- which(allowed[[1]], arr.ind = TRUE)
      dyads <- dyads[order(dyads[, 1], dyads[, 2]), , drop = FALSE]
      rows <- n_dyad + seq_len(nrow(dyads))
      moved_dyad <- integer(nrow(dyads))
      dyad_frame <- dgp_fill(
        dyad_frame,
        rows,
        interval = interval,
        sender = dyads[, 1],
        receiver = dyads[, 2],
        outcome = moved_dyad,
        dt = dt,
        dependent = !is.null(mover),
        # A REM degree effect reads the receiver by default
        # (`type = "alter"`), the out-degree included.
        stats = cbind(
          indeg = colSums(tie)[dyads[, 2]],
          outdeg = rowSums(tie)[dyads[, 2]]
        )
      )
      n_dyad <- n_dyad + nrow(dyads)
    }

    now <- end
    if (exogenous_now) {
      applied <- dgp_apply_break(
        break_time,
        exogenous,
        composition,
        exogenous_tie,
        present
      )
      exogenous_tie <- applied$exogenous_tie
      present <- applied$present
      next_break <- next_break + applied$n
      interval <- interval + applied$n - 1L
      next
    }

    k <- mover$flavor
    sender <- mover$sender
    receivers <- which(allowed[[k]][sender, ])
    stats <- dgp_choice_stats(tie, sender)[receivers, , drop = FALSE]
    eta <- drop(stats %*% processes[[k]]$choice)
    weights <- exp(eta - max(eta))
    receiver <- receivers[
      sample.int(length(receivers), 1L, prob = weights / sum(weights))
    ]
    if (rem) {
      hit <- n_dyad -
        nrow(dyads) +
        which(dyads[, 1] == sender & dyads[, 2] == receiver)
      dyad_frame$outcome[hit] <- 1L
    }
    if (!k %in% unmodeled) {
      n_dependent[[k]] <- n_dependent[[k]] + 1L
      rows <- n_choice[[k]] + seq_along(receivers)
      choice_frames[[k]] <- dgp_fill(
        choice_frames[[k]],
        rows,
        interval = n_dependent[[k]],
        sender = sender,
        receiver = receivers,
        outcome = as.integer(receivers == receiver),
        dt = NA_real_,
        dependent = TRUE,
        stats = stats
      )
      n_choice[[k]] <- n_choice[[k]] + length(receivers)
    }

    n_drawn <- n_drawn + 1L
    events[n_drawn, ] <- list(now, sender, receiver, k)
    tie[sender, receiver] <- if (identical(k, "dissolution")) 0 else 1
  }

  trim <- function(frame, n) frame[seq_len(n), , drop = FALSE]
  list(
    events = events,
    initial = initial,
    exogenous = if (nrow(exogenous) > 0) exogenous,
    composition = if (nrow(composition) > 0) composition,
    rate = Map(trim, rate_frames, n_rate),
    choice = Map(trim, choice_frames, n_choice),
    dyad = if (rem) trim(dyad_frame, n_dyad)
  )
}

# A preallocated frame: one column per bookkeeping field and per statistic.
dgp_new_frame <- function(n_rows, stat_names) {
  frame <- data.frame(
    interval = integer(n_rows),
    sender = integer(n_rows),
    receiver = integer(n_rows),
    outcome = integer(n_rows),
    dt = numeric(n_rows),
    dependent = logical(n_rows)
  )
  for (s in stat_names) {
    frame[[s]] <- numeric(n_rows)
  }
  frame
}

dgp_fill <- function(
  frame,
  rows,
  interval,
  sender,
  receiver,
  outcome,
  dt,
  dependent,
  stats
) {
  if (length(rows) == 0L) {
    return(frame)
  }
  frame$interval[rows] <- interval
  frame$sender[rows] <- sender
  frame$receiver[rows] <- receiver
  frame$outcome[rows] <- outcome
  frame$dt[rows] <- dt
  frame$dependent[rows] <- dependent
  for (s in colnames(stats)) {
    frame[[s]][rows] <- stats[, s]
  }
  frame
}

# Apply every exogenous and composition row stamped at `time`, returning the
# new states and how many rows were consumed (each one is its own boundary
# in goldfish's schedule, so each closes an interval of length zero after
# the first).
dgp_apply_break <- function(
  time,
  exogenous,
  composition,
  exogenous_tie,
  present
) {
  on_exogenous <- which(exogenous$time == time)
  for (r in on_exogenous) {
    exogenous_tie[exogenous$from[r], exogenous$to[r]] <- 1
  }
  on_composition <- which(composition$time == time)
  for (r in on_composition) {
    present[composition$node[r]] <- composition$present[r]
  }
  list(
    exogenous_tie = exogenous_tie,
    present = present,
    n = length(on_exogenous) + length(on_composition)
  )
}

# The stocnet the simulated sequence was generated on: the focal `calls`
# layer (initial ties at time NA, then the events in order), an `emails`
# layer carrying the exogenous stream, and a node `present` stream.
dgp_stocnet <- function(sim, n_actors) {
  events <- sim$events
  focal <- data.frame(
    from = events$from,
    to = events$to,
    time = events$time,
    layer = "calls",
    weight = ifelse(events$flavor == "dissolution", -1, 1),
    stringsAsFactors = FALSE
  )
  if (!is.null(sim$initial)) {
    focal <- rbind(
      data.frame(
        from = sim$initial[, 1],
        to = sim$initial[, 2],
        time = NA_real_,
        layer = "calls",
        weight = 1,
        stringsAsFactors = FALSE
      ),
      focal
    )
  }
  update <- c(calls = "increment")
  directed <- c(calls = TRUE)
  observation <- c(calls = "event")
  if (!is.null(sim$exogenous)) {
    focal <- rbind(
      focal,
      data.frame(
        from = sim$exogenous$from,
        to = sim$exogenous$to,
        time = sim$exogenous$time,
        layer = "emails",
        weight = 1,
        stringsAsFactors = FALSE
      )
    )
    update <- c(update, emails = "increment")
    directed <- c(directed, emails = TRUE)
    observation <- c(observation, emails = "event")
  }
  data <- list(
    info = list(
      name = "simulated",
      focal = "calls",
      update = update,
      directed = directed,
      observation = observation
    ),
    nodes = data.frame(
      label = sprintf("A%02d", seq_len(n_actors)),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = focal
  )
  if (!is.null(sim$composition)) {
    # `active`, not its former name `present`: manynet renames `present` on
    # a node frame only, and a `changes` row saying `present` would be an
    # ordinary attribute that switches no node in or out.
    changes <- data.frame(
      time = sim$composition$time,
      node = sim$composition$node,
      var = "active"
    )
    changes$value <- lapply(sim$composition$present, list)
    data$changes <- changes
  }
  flavors <- unique(events$flavor)
  if (!identical(flavors, "none")) {
    data$ties$flavor <- NULL
    data <- add_flavor(
      data,
      layer = "calls",
      values_equivalence = c(creation = 1, dissolution = -1),
      flavor_style = "mutually_exclusive"
    )
  }
  data
}
