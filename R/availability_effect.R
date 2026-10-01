# Availability maintained as an effect ----
#
# A fid's risk set, kept the way an effect's statistic is kept: one value
# in memory at its own encoding, updated in place where its inputs moved,
# and its changes written as an initial value plus a flat update buffer and
# a per-row pointer. Its inputs are the two flip streams the walk already
# has, the fid's live mask flips and the composition changes of both node
# sides.
# The batch walk steps it at every row the fid stores and keeps the stream;
# the walk handle steps it when it reads the risk set and keeps nothing.
#
# Variants, by what the fid's likelihood reads:
#   gate     rate with a constraint: sender i is at risk iff present and
#            allowed at least one present receiver other than itself. The
#            per-sender count of such receivers is the cache.
#   presence rate without a constraint: presence alone. Nothing is stored
#            (the writer keeps its own stream); the count at risk is kept.
#   presence_dyad
#            REM without a constraint: the present senders times the
#            present receivers, less the self-dyads on one node set. As
#            above, only the count at risk is kept.
#   alter    choice with a column-broadcast mask: one receiver vector.
#   outer    choice with a sender-only mask: receiver presence alone.
#   point    choice with a dyadic mask or an opportunity list: the grid of
#            present receivers each sender is allowed.
#   dyad     REM and coordination with a point-kind constraint: both
#            presences and the mask on the grid, coordination symmetrised.
#   factors  REM and coordination with an alter-, ego- or global-kind
#            constraint: two factor vectors, cell (i, j) = f1[i] & f2[j],
#            each a presence with the mask folded in at its own axis.
#
# The stored encodings are the ones the post-walk folds produced, so the
# engines read them unchanged. The point grid is the true availability of
# every sender at every row; a choice likelihood reads only the event
# sender's row of it.

# `changes1` / `changes2` are the composition changes of each side as
# parallel `time`, `node`, `replace` vectors, applied in their given order
# up to the target a caller names (`availability_step()`). `mask` is a live
# mask maintainer (`build_mask_maintainer()`) read through `value()`, or
# NULL. `opportunities` is the deprecated per-event opportunity list: at the
# row of event `e` the event sender's row of an opportunity mask is set to
# the receivers it lists (all of them when the entry is NULL), and every
# sender's row keeps its last setting, so it is a dyadic mask like any
# other. `record = FALSE` keeps no stream. `count = TRUE` keeps the
# per-sender count of allowed present receivers for a variant that does not
# need it to store its value, so the observed-event check can read it.
# `drop_diagonal` says the self-dyad is not a dyad (one node set): a sender
# is not its own receiver in the counts, and the self-dyads are not counted
# at risk.
new_availability_effect <- function(
  variant,
  n1,
  n2,
  presence1,
  presence2,
  changes1 = NULL,
  changes2 = NULL,
  mask = NULL,
  stored_kind = NULL,
  drop_diagonal = FALSE,
  symmetric = FALSE,
  opportunities = NULL,
  record = TRUE,
  count = identical(variant, "gate")
) {
  av <- new.env(parent = emptyenv())
  av$variant <- variant
  av$n1 <- n1
  av$n2 <- n2
  # Fresh copies: both vectors are written in place from here on.
  av$p1 <- as.logical(presence1) & TRUE
  av$p2 <- as.logical(presence2) & TRUE
  av$n_present2 <- sum(av$p2)
  av$changes1 <- changes1
  av$changes2 <- changes2
  av$cursor1 <- 0L
  av$cursor2 <- 0L
  av$mask <- mask
  av$kind <- stored_kind
  av$drop_diagonal <- drop_diagonal
  av$symmetric <- symmetric
  av$opportunities <- opportunities
  av$opportunity <- NULL
  if (!is.null(opportunities)) {
    av$opportunity <- matrix(TRUE, n1, n2)
  }
  av$pending <- list()
  av$value <- NULL
  av$init <- NULL
  av$rows <- 0L
  av$n_at_risk <- 0
  av$exposure <- 0
  av$check <- NULL
  av$count <- NULL
  if (count && !is.null(mask)) {
    av$count <- if (symmetric_vector_mask(av)) {
      symmetric_receiver_count(av)
    } else {
      as.double(initial_receiver_count(
        mask$value(),
        stored_kind,
        av$p2,
        n1,
        n2,
        drop_diagonal
      ))
    }
  }
  if (identical(variant, "presence")) {
    av$n_at_risk <- sum(av$p1)
  }
  if (identical(variant, "factors")) {
    av$value1 <- NULL
    av$sum1 <- 0
    av$sum2 <- 0
    av$sum_self <- 0
  }
  if (identical(variant, "presence_dyad")) {
    av$n_senders <- sum(av$p1)
    av$n_both <- if (drop_diagonal) sum(av$p1 & av$p2[seq_len(n1)]) else 0
    av$n_at_risk <- av$n_senders * av$n_present2 - av$n_both
  }
  av$record <- record
  if (record) {
    av$n_changes <- integer(64L)
    av$node1 <- double(256L)
    av$node2 <- double(256L)
    av$repl <- double(256L)
    av$n_emitted <- 0L
    if (identical(variant, "factors")) {
      # The sender factor's own stream, kept the way the receiver one is.
      side1 <- new.env(parent = emptyenv())
      side1$variant <- "alter"
      side1$n1 <- n1
      side1$record <- TRUE
      side1$rows <- 0L
      side1$init <- NULL
      side1$n_changes <- integer(64L)
      side1$node1 <- double(256L)
      side1$node2 <- NULL
      side1$repl <- double(256L)
      side1$n_emitted <- 0L
      av$side1 <- side1
    }
  }
  av
}

# Queue one batch of the fid's mask flips, as the maintainer reported them.
# Batches are applied in order at the next step, so a cell that flips and
# flips back between two rows moves the counts twice, not once.
availability_push_flips <- function(av, flips) {
  if (length(flips$entries) > 0L) {
    pending <- av$pending
    av$pending <- NULL
    pending[[length(pending) + 1L]] <- flips
    av$pending <- pending
  }
  invisible(NULL)
}

# How far a side's composition changes reach at `t`: every change stamped
# at or before it, as the writer maps a change onto the first row not
# before it.
availability_presence_target <- function(changes, cursor, t) {
  n <- length(changes$time)
  while (cursor < n && changes$time[[cursor + 1L]] <= t) {
    cursor <- cursor + 1L
  }
  cursor
}

# Step the effect over one row: the queued mask flips, then the receiver
# side's changes up to `target2`, then the sender side's up to `target1`,
# the order the folds applied them in. `sender` is the row's event sender
# (an opportunity row is set there). `interval` is the row's exposure
# time: the count at risk after the row's changes, times the interval, is
# the row's share of the exposure, as the rate likelihood reads the row.
#
# The exposure is that sum over the stored rows, with each row's presence
# and mask changes applied first, as the likelihood engines apply them
# before the row. Only then is `n_dep_events / exposure` exactly the
# intercept-only maximum-likelihood rate; the continuous integral over the
# change stream is not, because the engine assigns a change to the whole
# interval ending at the row it maps to. The entity counted is the one the
# rate is defined over: active senders for the actor-oriented rate, active
# dyads for the tie-oriented (REM) rate.
availability_step <- function(
  av,
  target1,
  target2,
  sender = NA_integer_,
  interval = 0
) {
  first <- is.null(av$value)
  moved1 <- availability_take_changes(av, 1L, target1)
  moved2 <- availability_take_changes(av, 2L, target2)
  if (
    !is.null(av$check$epoch) &&
      (length(av$pending) > 0L || length(moved2$node) > 0L)
  ) {
    av$check$epoch <- av$check$epoch + 1
  }
  if (av$variant %in% c("presence", "presence_dyad")) {
    availability_step_presence(av, moved1, moved2)
  } else {
    touched <- integer(0)
    if (is.null(av$count)) {
      receivers <- availability_write_presence(av, 2L, moved2)
    } else {
      for (flips in av$pending) {
        touched <- c(touched, availability_gate_flips(av, flips))
      }
      for (k in seq_along(moved2$node)) {
        touched <- c(
          touched,
          availability_gate_receiver(av, moved2$node[[k]], moved2$replace[[k]])
        )
      }
      receivers <- unique(moved2$node)
      availability_track_positive(av, touched)
    }
    senders <- availability_write_presence(av, 1L, moved1)
    switch(
      av$variant,
      gate = availability_step_gate(av, c(touched, senders), first),
      alter = ,
      outer = availability_step_receivers(av, receivers, first),
      point = availability_step_point(av, receivers, sender, first),
      dyad = availability_step_dyad(av, senders, receivers, first),
      factors = availability_step_factors(av, senders, receivers, first)
    )
  }
  av$exposure <- av$exposure + interval * av$n_at_risk
  av$pending <- list()
  av$rows <- av$rows + 1L
  invisible(NULL)
}

# The changes of one side between its cursor and `target`, advancing it.
availability_take_changes <- function(av, side, target) {
  cursor_name <- if (side == 1L) "cursor1" else "cursor2"
  changes <- if (side == 1L) av$changes1 else av$changes2
  cursor <- av[[cursor_name]]
  if (target <= cursor) {
    return(NULL)
  }
  at <- (cursor + 1L):target
  av[[cursor_name]] <- target
  list(node = changes$node[at], replace = changes$replace[at])
}

# Apply one side's changes to its presence vector in place, in order, and
# name the nodes that moved.
availability_write_presence <- function(av, side, moved) {
  if (is.null(moved)) {
    return(integer(0))
  }
  name <- if (side == 1L) "p1" else "p2"
  buffer <- av[[name]]
  av[[name]] <- NULL
  buffer <- write_entries(buffer, moved$node, moved$replace)
  av[[name]] <- buffer
  if (side == 2L) {
    av$n_present2 <- sum(buffer)
  }
  unique(moved$node)
}

## Rate gate ----

# A mask flip moves the counts of the senders it reaches, read at the mask's
# kind: a point flip one sender, a column flip every sender but the one it
# names, a row flip one sender outright, a scalar flip everyone. A receiver
# arriving or leaving moves the count of every sender allowed to reach it.
# The counts are moved before this runs (`availability_step()`); `affected`
# names the senders whose count or presence moved on the row.
availability_step_gate <- function(av, affected, first) {
  if (first) {
    availability_open(av, av$p1 & (av$count > 0))
    return(invisible(NULL))
  }
  if (length(affected) == 0L) {
    availability_emit(av, integer(0), logical(0))
    return(invisible(NULL))
  }
  affected <- sort(unique(affected))
  desired <- av$p1[affected] & (av$count[affected] > 0)
  flips <- changed_entries(av$value, affected, desired)
  availability_write_value(av, flips)
  availability_emit(av, flips$entries, flips$values)
  invisible(NULL)
}

# One batch of mask flips on the counts; returns the senders it touched.
availability_gate_flips <- function(av, flips) {
  if (symmetric_vector_mask(av)) {
    # A flip of m[k] moves every sender's count by receiver k and sender k's
    # own gate m[k] at once, so the counts are read afresh off the vector.
    count <- av$count
    av$count <- NULL
    av$count <- write_entries(
      count,
      seq_len(av$n1),
      symmetric_receiver_count(av)
    )
    return(seq_len(av$n1))
  }
  entries <- flips$entries
  values <- flips$values
  n1 <- av$n1
  p2 <- av$p2
  kind <- av$kind
  dd <- av$drop_diagonal
  count <- av$count
  av$count <- NULL
  touched <- integer(0)
  if (kind == 0L) {
    senders <- ((entries - 1L) %% n1) + 1L
    receivers <- ((entries - 1L) %/% n1) + 1L
    live <- p2[receivers] & !(dd & senders == receivers)
    if (any(live)) {
      touched <- unique(senders[live])
      slot <- match(senders[live], touched)
      step <- tabulate(slot[values[live]], length(touched)) -
        tabulate(slot[!values[live]], length(touched))
      count <- write_entries(count, touched, count[touched] + step)
    }
  } else if (kind == 1L) {
    live <- p2[entries]
    delta <- sum(live & values) - sum(live & !values)
    if (delta != 0) {
      count <- count + delta
      touched <- seq_len(n1)
    }
    # A column's flip reaches every sender but the one it names.
    if (dd) {
      own <- entries <= n1 & live
      own[own] <- p2[entries[own]]
      if (any(own)) {
        j <- entries[own]
        count <- write_entries(
          count,
          j,
          count[j] - ifelse(values[own], 1, -1)
        )
        touched <- c(touched, j)
      }
    }
  } else if (kind == 2L) {
    own <- if (dd) as.double(p2[entries]) else 0
    count <- write_entries(
      count,
      entries,
      ifelse(values, av$n_present2 - own, 0)
    )
    touched <- entries
  } else {
    own <- if (dd) as.double(p2[seq_len(n1)]) else 0
    count <- if (values[[length(values)]]) {
      av$n_present2 - own + rep(0, n1)
    } else {
      double(n1)
    }
    touched <- seq_len(n1)
  }
  av$count <- count
  touched
}

# One receiver's arrival or departure on the counts; returns the senders it
# touched. The node that moves is not its own receiver.
availability_gate_receiver <- function(av, node, value) {
  p2 <- av$p2
  delta <- as.integer(value) - as.integer(p2[[node]])
  if (delta == 0L) {
    return(integer(0))
  }
  n1 <- av$n1
  mask <- av$mask$value()
  reached <- if (symmetric_vector_mask(av)) {
    if (isTRUE(as.logical(mask[[node]]))) {
      which(as.logical(mask))
    } else {
      integer(0)
    }
  } else {
    switch(
      as.character(av$kind),
      "0" = which(as.logical(mask[(node - 1L) * n1 + seq_len(n1)])),
      "1" = if (isTRUE(as.logical(mask[[node]]))) seq_len(n1) else integer(0),
      "2" = which(as.logical(mask)),
      "3" = if (isTRUE(as.logical(mask))) seq_len(n1) else integer(0)
    )
  }
  if (av$drop_diagonal && node <= n1) {
    reached <- reached[reached != node]
  }
  if (length(reached) > 0L) {
    count <- av$count
    av$count <- NULL
    count <- write_entries(count, reached, count[reached] + delta)
    av$count <- count
  }
  p2 <- NULL
  buffer <- av$p2
  av$p2 <- NULL
  buffer <- write_entries(buffer, node, value)
  av$p2 <- buffer
  av$n_present2 <- av$n_present2 + delta
  availability_track_receiver(av, node, value)
  reached
}

# Whether the mask is a vector stored at its own kind whose cell is
# symmetrised: coordination's m[i] & m[j], which no stored grid carries.
symmetric_vector_mask <- function(av) {
  isTRUE(av$symmetric) && isTRUE(av$kind %in% c(1L, 2L))
}

# Each sender's count of allowed present receivers under a symmetric vector
# mask: m[i] times the present receivers m allows, less its own cell.
symmetric_receiver_count <- function(av) {
  allowed <- as.logical(av$mask$value())
  reachable <- sum(allowed & av$p2)
  own <- if (av$drop_diagonal) allowed[seq_len(av$n1)] & av$p2[seq_len(av$n1)]
  as.double(allowed[seq_len(av$n1)]) *
    (reachable - if (is.null(own)) 0 else own)
}

## Rate presence ----

# No constraint: the stored stream is the writer's; only the count at risk
# is kept, moved one change at a time, since a change may restate a value
# already held.
availability_step_presence <- function(av, moved1, moved2) {
  dyad <- identical(av$variant, "presence_dyad")
  for (side in c(2L, 1L)) {
    moved <- if (side == 1L) moved1 else moved2
    if (is.null(moved) || (side == 2L && !dyad)) {
      next
    }
    name <- if (side == 1L) "p1" else "p2"
    for (k in seq_along(moved$node)) {
      node <- moved$node[[k]]
      value <- moved$replace[[k]]
      buffer <- av[[name]]
      delta <- as.integer(value) - as.integer(buffer[[node]])
      if (delta == 0L) {
        next
      }
      if (dyad && av$drop_diagonal && node <= av$n1) {
        other <- if (side == 1L) av$p2 else av$p1
        if (other[[node]]) {
          av$n_both <- av$n_both + delta
        }
      }
      av[[name]] <- NULL
      buffer <- write_entries(buffer, node, value)
      av[[name]] <- buffer
      if (side == 1L) {
        av$n_senders <- av$n_senders + delta
      } else {
        av$n_present2 <- av$n_present2 + delta
      }
    }
  }
  av$n_at_risk <- if (dyad) {
    av$n_senders * av$n_present2 - av$n_both
  } else {
    sum(av$p1)
  }
  invisible(NULL)
}

## Choice, receiver axis ----

# The alter encoding reads presence and the column-broadcast mask; the outer
# encoding reads presence alone (its mask gates senders, which the choice
# likelihood never reads off its own observed sender).
availability_step_receivers <- function(av, receivers, first) {
  affected <- receivers
  alter <- identical(av$variant, "alter")
  if (alter) {
    for (flips in av$pending) {
      affected <- c(
        affected,
        if (av$kind == 1L) flips$entries else seq_len(av$n2)
      )
    }
  }
  receiver_value <- function(at) {
    if (!alter) {
      return(av$p2[at])
    }
    mask <- av$mask$value()
    allowed <- if (av$kind == 1L) mask[at] else rep(mask, length(at))
    av$p2[at] & as.logical(allowed)
  }
  if (first) {
    availability_open(av, receiver_value(seq_len(av$n2)))
    return(invisible(NULL))
  }
  if (length(affected) == 0L) {
    availability_emit(av, integer(0), logical(0))
    return(invisible(NULL))
  }
  affected <- sort(unique(affected))
  flips <- changed_entries(av$value, affected, receiver_value(affected))
  availability_write_value(av, flips)
  availability_emit(av, flips$entries, flips$values)
  invisible(NULL)
}

## Choice, point grid ----

# Cell (i, j) is receiver j present, allowed for i by the mask, and listed
# for i by its last opportunity set. A receiver arriving or leaving moves
# its column, a mask flip its cells, an opportunity row its row.
availability_step_point <- function(av, receivers, sender, first) {
  n1 <- av$n1
  n2 <- av$n2
  cells <- integer(0)
  for (j in receivers) {
    cells <- c(cells, (j - 1L) * n1 + seq_len(n1))
  }
  for (flips in av$pending) {
    cells <- c(cells, map_entries(flips$entries, av$kind, 0L, n1, n2))
  }
  if (!is.null(av$opportunities)) {
    cells <- c(cells, availability_set_opportunity(av, sender))
  }
  if (first) {
    # Column j of the grid is receiver j's presence, which is `p2` with
    # each entry repeated down its column.
    grid <- rep(av$p2, each = n1)
    if (!is.null(av$mask)) {
      grid <- grid & support_to_grid(av$mask$value(), av$kind, n1, n2)
    }
    if (!is.null(av$opportunity)) {
      grid <- grid & av$opportunity
    }
    dim(grid) <- c(n1, n2)
    availability_open(av, grid)
    return(invisible(NULL))
  }
  if (length(cells) == 0L) {
    availability_emit(av, integer(0), logical(0))
    return(invisible(NULL))
  }
  cells <- sort(unique(cells))
  flips <- changed_entries(av$value, cells, availability_point_cells(av, cells))
  availability_write_value(av, flips)
  availability_emit(av, flips$entries, flips$values)
  invisible(NULL)
}

availability_point_cells <- function(av, cells) {
  n1 <- av$n1
  allowed <- av$p2[((cells - 1L) %/% n1) + 1L]
  if (!is.null(av$mask)) {
    allowed <- allowed &
      as.logical(read_value_at_entries(
        av$mask$value(),
        av$kind,
        cells,
        0L,
        n1,
        av$n2
      ))
  }
  if (!is.null(av$opportunity)) {
    allowed <- allowed & av$opportunity[cells]
  }
  allowed
}

# Set the event sender's opportunity row from the list entry of this row;
# returns the cells whose opportunity changed.
availability_set_opportunity <- function(av, sender) {
  e <- av$rows + 1L
  if (is.na(sender) || e > length(av$opportunities)) {
    return(integer(0))
  }
  n1 <- av$n1
  n2 <- av$n2
  listed <- av$opportunities[[e]]
  row <- if (is.null(listed)) rep(TRUE, n2) else seq_len(n2) %in% listed
  cells <- sender + n1 * (seq_len(n2) - 1L)
  flips <- changed_entries(av$opportunity, cells, row)
  buffer <- av$opportunity
  av$opportunity <- NULL
  buffer <- write_entries(buffer, flips$entries, flips$values)
  av$opportunity <- buffer
  flips$entries
}

## REM and coordination ----

# Both presences and the mask on the grid. A sender moving reaches its row,
# a receiver its column, a mask flip the cells its kind projects onto;
# coordination's symmetrised grid is closed under transposition.
availability_step_dyad <- function(av, senders, receivers, first) {
  n1 <- av$n1
  n2 <- av$n2
  # Each flipped cell once, at its last flip: the order the cells are
  # visited in is the order the row's changes are stored in.
  mask_entries <- unlist(
    lapply(av$pending, `[[`, "entries"),
    use.names = FALSE
  )
  mask_entries <- mask_entries[!duplicated(mask_entries, fromLast = TRUE)]
  if (first) {
    grid <- outer(av$p1, av$p2) &
      (support_to_grid(av$mask$value(), av$kind, n1, n2) == 1)
    if (av$symmetric) {
      grid <- symmetrize_mask(grid)
    }
    availability_open(av, grid)
    return(invisible(NULL))
  }
  reached <- unique(c(
    if (length(senders) > 0L) map_entries(senders, 2L, 0L, n1, n2),
    if (length(receivers) > 0L) map_entries(receivers, 1L, 0L, n1, n2),
    if (length(mask_entries) > 0L) {
      map_entries(mask_entries, av$kind, 0L, n1, n2)
    }
  ))
  if (length(reached) == 0L) {
    availability_emit(av, integer(0), logical(0))
    return(invisible(NULL))
  }
  if (av$symmetric) {
    reached <- unique(c(reached, transposed_entries(reached, n1)))
    desired <- availability_dyad_raw(av, reached) &
      availability_dyad_raw(av, transposed_entries(reached, n1))
  } else {
    desired <- availability_dyad_raw(av, reached)
  }
  flips <- changed_entries(av$value, reached, desired)
  availability_write_value(av, flips)
  availability_emit(av, flips$entries, flips$values)
  invisible(NULL)
}

availability_dyad_raw <- function(av, entries) {
  n1 <- av$n1
  rows <- ((entries - 1L) %% n1) + 1L
  cols <- ((entries - 1L) %/% n1) + 1L
  av$p1[rows] &
    av$p2[cols] &
    read_value_at_entries(av$mask$value(), av$kind, entries, 0L, n1, av$n2)
}

# The transposes of linear point entries of an n1 x n1 grid.
transposed_entries <- function(entries, n1) {
  rows <- ((entries - 1L) %% n1) + 1L
  cols <- ((entries - 1L) %/% n1) + 1L
  (rows - 1L) * n1 + cols
}

## REM and coordination, factor form ----

# A vector-kind mask on a dyad loop factors: cell (i, j) is f1[i] & f2[j].
# The mask folds into the factor of its own axis -- an alter-kind or global
# mask into the receiver factor, an ego-kind one into the sender factor --
# and, symmetrised for coordination, a vector mask folds into both, since the
# symmetric cell m[i] & m[j] is the outer product of the vector with itself.
# A global mask is a scalar, so its symmetric fold is itself. A sender moving
# reaches its entry of f1, a receiver its entry of f2, a mask flip the entries
# of the factor it folds into (all of them for a global flip).
availability_step_factors <- function(av, senders, receivers, first) {
  if (first) {
    av$value1 <- availability_factor(av, 1L, seq_len(av$n1)) & TRUE
    availability_open(av, availability_factor(av, 2L, seq_len(av$n2)))
    av$sum1 <- sum(av$value1)
    av$sum2 <- sum(av$value)
    av$sum_self <- availability_factor_self(av, seq_len(min(av$n1, av$n2)))
    av$n_at_risk <- av$sum1 * av$sum2 - av$sum_self
    if (av$record) {
      av$side1$init <- av$value1 & TRUE
      av$side1$rows <- av$rows
      availability_emit(av$side1, integer(0), logical(0))
    }
    return(invisible(NULL))
  }
  reached1 <- senders
  reached2 <- receivers
  for (flips in av$pending) {
    if (availability_factor_folds(av, 1L)) {
      reached1 <- c(reached1, flips$entries)
    }
    if (availability_factor_folds(av, 2L)) {
      reached2 <- c(
        reached2,
        if (av$kind == 3L) seq_len(av$n2) else flips$entries
      )
    }
  }
  flips1 <- availability_factor_flips(av, 1L, reached1)
  flips2 <- availability_factor_flips(av, 2L, reached2)
  availability_write_factor(av, 1L, flips1)
  availability_write_factor(av, 2L, flips2)
  av$n_at_risk <- av$sum1 * av$sum2 - av$sum_self
  availability_emit(av, flips2$entries, flips2$values)
  if (av$record) {
    av$side1$rows <- av$rows
    availability_emit(av$side1, flips1$entries, flips1$values)
  }
  invisible(NULL)
}

# Whether the mask folds into factor `side` (1 sender, 2 receiver).
availability_factor_folds <- function(av, side) {
  kind <- av$kind
  vector_mask <- kind %in% c(1L, 2L)
  if (side == 1L) {
    kind == 2L || (av$symmetric && vector_mask)
  } else {
    kind %in% c(1L, 3L) || (av$symmetric && vector_mask)
  }
}

# Factor `side` at entries `at`, from presence and the mask.
availability_factor <- function(av, side, at) {
  base <- if (side == 1L) av$p1[at] else av$p2[at]
  if (!availability_factor_folds(av, side)) {
    return(base)
  }
  mask <- av$mask$value()
  allowed <- if (av$kind == 3L) {
    rep(as.logical(mask), length(at))
  } else {
    as.logical(mask[at])
  }
  base & allowed
}

# How many of the self-dyads at entries `at` both factors hold; zero unless
# the self-dyad is no dyad (one node set).
availability_factor_self <- function(av, at) {
  if (!av$drop_diagonal || length(at) == 0L) {
    return(0)
  }
  sum(av$value1[at] & av$value[at])
}

availability_factor_flips <- function(av, side, reached) {
  if (length(reached) == 0L) {
    return(list(entries = integer(0), values = logical(0)))
  }
  reached <- sort(unique(reached))
  current <- if (side == 1L) av$value1 else av$value
  changed_entries(current, reached, availability_factor(av, side, reached))
}

# Write one factor's flips in place and move the sums the count at risk is
# made of: the factor's own sum, and the self-dyads its flipped entries are
# half of.
availability_write_factor <- function(av, side, flips) {
  entries <- flips$entries
  if (length(entries) == 0L) {
    return(invisible(NULL))
  }
  values <- flips$values
  step <- sum(values) - sum(!values)
  self <- entries[entries <= min(av$n1, av$n2)]
  self_before <- availability_factor_self(av, self)
  if (side == 1L) {
    value <- av$value1
    av$value1 <- NULL
    av$value1 <- write_entries(value, entries, values)
    av$sum1 <- av$sum1 + step
  } else {
    value <- av$value
    av$value <- NULL
    av$value <- write_entries(value, entries, values)
    av$sum2 <- av$sum2 + step
  }
  av$sum_self <- av$sum_self - self_before + availability_factor_self(av, self)
  invisible(NULL)
}

## Shared bookkeeping ----

# The first row: the value is computed whole, once, and the stream's initial
# value is its own object, since the value is written in place from here on.
availability_open <- function(av, value) {
  av$value <- value & TRUE
  av$n_at_risk <- sum(av$value)
  if (availability_drops_self(av)) {
    av$n_at_risk <- av$n_at_risk - sum(diag(av$value))
  }
  if (av$record) {
    av$init <- value
    availability_emit(av, integer(0), logical(0))
  }
  invisible(NULL)
}

# Whether the self-dyads of the grid are left out of the count at risk: a
# REM or coordination grid on one node set, where the self-dyad is not a
# dyad whatever the mask holds there.
availability_drops_self <- function(av) {
  identical(av$variant, "dyad") && av$drop_diagonal
}

availability_write_value <- function(av, flips) {
  if (length(flips$entries) == 0L) {
    return(invisible(NULL))
  }
  counted <- if (availability_drops_self(av)) {
    ((flips$entries - 1L) %% av$n1) != ((flips$entries - 1L) %/% av$n1)
  } else {
    TRUE
  }
  av$n_at_risk <- av$n_at_risk +
    sum(flips$values & counted) -
    sum(!flips$values & counted)
  value <- av$value
  av$value <- NULL
  value <- write_entries(value, flips$entries, flips$values)
  av$value <- value
  invisible(NULL)
}

# Close one stored row with the entries that changed on it. The buffers
# double when full and are taken out of the effect before they are written,
# so a row copies nothing.
availability_emit <- function(av, entries, values) {
  if (!av$record) {
    return(invisible(NULL))
  }
  k <- av$rows + 1L
  n_changes <- av$n_changes
  av$n_changes <- NULL
  if (k > length(n_changes)) {
    length(n_changes) <- 2L * length(n_changes)
  }
  n <- length(entries)
  n_changes[k] <- n
  av$n_changes <- n_changes
  if (n == 0L) {
    return(invisible(NULL))
  }
  from <- av$n_emitted
  need <- from + n
  point <- av$variant %in% c("point", "dyad")
  node1 <- av$node1
  av$node1 <- NULL
  repl <- av$repl
  av$repl <- NULL
  node2 <- if (point) av$node2
  av$node2 <- NULL
  if (need > length(node1)) {
    capacity <- max(2L * length(node1), need)
    length(node1) <- capacity
    length(repl) <- capacity
    if (point) length(node2) <- capacity
  }
  at <- (from + 1L):need
  if (point) {
    node1[at] <- ((entries - 1L) %% av$n1) + 1L
    node2[at] <- ((entries - 1L) %/% av$n1) + 1L
  } else {
    node1[at] <- entries
  }
  repl[at] <- as.numeric(values)
  av$node1 <- node1
  av$node2 <- node2
  av$repl <- repl
  av$n_emitted <- need
  invisible(NULL)
}

# The stream as the folds stored it: the initial value, the update buffer
# (`(node, replace)` rows, or `(node1, node2, replace)` on the grid) and the
# cumulative per-row pointer.
availability_stream <- function(av) {
  rows <- seq_len(av$rows)
  at <- seq_len(av$n_emitted)
  update <- if (av$variant %in% c("point", "dyad")) {
    n1v <- av$node1[at]
    n2v <- av$node2[at]
    rv <- av$repl[at]
    if (length(at) > 0L) rbind(n1v, n2v, rv) else matrix(0, 3L, 0L)
  } else if (identical(av$variant, "gate")) {
    node_vec <- av$node1[at]
    repl_vec <- av$repl[at]
    if (length(at) > 0L) rbind(node_vec, repl_vec) else matrix(0, 2L, 0L)
  } else {
    nv <- av$node1[at]
    rv <- av$repl[at]
    if (length(at) > 0L) rbind(nv, rv) else matrix(0, 2L, 0L)
  }
  list(
    init = av$init,
    update = update,
    pointer = cumsum(av$n_changes[rows])
  )
}

# Write a fid's availability onto its finalized output: the folded stream and
# its encoding, and the walk's verdict on the observed events. The raw
# presences the fold consumed are not kept; the verdict was reached in the
# walk, so nothing downstream needs them.
availability_finish <- function(av, out) {
  if (!is.null(av$check) && !av$check$immediate) {
    out$support_verdict <- availability_verdict(av)
  }
  n_stored <- length(out$event_time)
  if (av$variant %in% c("presence", "presence_dyad") || n_stored == 0L) {
    return(out)
  }
  if (av$rows != n_stored) {
    cli::cli_abort(
      "The availability of this output has {av$rows} row{?s}, the output
       {n_stored}.",
      .internal = TRUE
    )
  }
  stream <- availability_stream(av)
  if (identical(av$variant, "gate")) {
    out$active_sender_init <- stream$init
    out$active_sender_update <- stream$update
    out$active_sender_update_pointer <- stream$pointer
    return(out)
  }
  if (identical(av$variant, "factors")) {
    # The outer encoding's sender factor rides as the object's own second
    # factor, in the sender availability fields.
    av$side1$rows <- av$rows
    sender <- availability_stream(av$side1)
    out$active_sender_init <- sender$init
    out$active_sender_update <- sender$update
    out$active_sender_update_pointer <- sender$pointer
  }
  out$active_dyad_init <- stream$init
  out$active_dyad_update <- stream$update
  out$active_dyad_update_pointer <- stream$pointer
  out$active_dyad_encoding <- switch(
    av$variant,
    dyad = ,
    point = "point",
    factors = "outer",
    av$variant
  )
  out
}

# One side's composition changes as parallel vectors, in the order the
# writer applies them.
availability_changes <- function(changes) {
  if (length(changes) == 0L) {
    return(NULL)
  }
  list(
    time = vapply(changes, `[[`, double(1), "time"),
    node = vapply(changes, `[[`, integer(1), "node"),
    replace = vapply(changes, `[[`, logical(1), "replace")
  )
}

## The observed-event check ----
#
# Each dependent event of a constrained output is checked against the
# availability as the walk keeps it, at the event:
#   rate    the observed sender is gated out (its count is zero)  -> error
#   choice  the sender has no allowed present receiver            -> error
#           the observed receiver is not among them               -> error
#   both    exactly one candidate (forced)                        -> warn
# and what describes the whole sequence is accumulated for the end:
#   rate    a sender present at the start never at risk           -> warn
#   choice  a receiver present at some event, never a candidate   -> warn
#           a sender present at the start allowed no receiver     -> warn
# "At risk" and "allowed a receiver" are the sender's count being positive
# at a dependent event. The count moves only where a mask cell flips or a
# receiver enters or leaves, so the senders that were ever positive at a
# dependent event are followed through those crossings: a sender turning
# positive notes how many events were checked; turning back, it was
# positive at an event iff more have been checked since. Receivers present
# at some event are followed the same way. A rate's check reads one gate
# entry and one count; a choice's reads its sender's row, as its likelihood
# does, to know which receivers were candidates.

# Start checking an effect's dependent events. `family` is "rate" (the
# sender-gate check) or "choice"; `label` names the process in the
# messages; `immediate` raises an error at its event, otherwise the verdict
# is kept for estimation to raise. Called before the first step, so the
# sender presence it keeps is the one at the start.
availability_enable_check <- function(
  av,
  family,
  label = NULL,
  immediate = TRUE
) {
  ck <- new.env(parent = emptyenv())
  ck$family <- family
  ck$label <- label
  ck$immediate <- immediate
  ck$init1 <- av$p1 & TRUE
  ck$n_dep <- 0
  positive <- av$count > 0
  ck$positive <- positive
  ck$since1 <- ifelse(positive, 0, NA_real_)
  ck$ever1 <- logical(av$n1)
  ck$n_init_positive <- sum(ck$init1 & positive)
  if (identical(family, "choice")) {
    # Row `i` of a point mask is `i + n1 * (j - 1)` over the receivers `j`;
    # the column offsets are the same for every row, so they are built once.
    ck$row_offsets <- as.integer(av$n1) * (seq_len(av$n2) - 1L)
    ck$since2 <- ifelse(av$p2, 0, NA_real_)
    ck$ever2 <- logical(av$n2)
    ck$candidate <- logical(av$n2)
    # A sender's allowed receivers move only with a mask flip or a receiver
    # entering or leaving; each such row starts a new epoch, and a sender's
    # row is read for candidates once per epoch it checks an event in.
    ck$epoch <- 0
    ck$marked <- rep(-1, av$n1)
    ck$shared_epoch <- -1
    ck$first_sender <- NA_integer_
    ck$complete <- FALSE
  }
  ck$forced <- integer(64L)
  ck$n_forced <- 0L
  ck$failure <- NULL
  av$check <- ck
  invisible(NULL)
}

# Follow the senders whose count moved across zero on this row.
availability_track_positive <- function(av, touched) {
  ck <- av$check
  if (is.null(ck) || length(touched) == 0L) {
    return(invisible(NULL))
  }
  touched <- unique(touched)
  now <- av$count[touched] > 0
  crossed <- now != ck$positive[touched]
  if (!any(crossed)) {
    return(invisible(NULL))
  }
  up <- touched[crossed & now]
  down <- touched[crossed & !now]
  since <- ck$since1
  ck$since1 <- NULL
  if (length(down) > 0L) {
    seen <- down[ck$n_dep > since[down]]
    if (length(seen) > 0L) {
      ever <- ck$ever1
      ck$ever1 <- NULL
      ck$ever1 <- write_entries(ever, seen, rep(TRUE, length(seen)))
    }
    since <- write_entries(since, down, rep(NA_real_, length(down)))
  }
  since <- write_entries(since, up, rep(ck$n_dep, length(up)))
  ck$since1 <- since
  positive <- ck$positive
  ck$positive <- NULL
  ck$positive <- write_entries(
    positive,
    c(up, down),
    c(
      rep(TRUE, length(up)),
      rep(FALSE, length(down))
    )
  )
  ck$n_init_positive <- ck$n_init_positive +
    sum(ck$init1[up]) -
    sum(ck$init1[down])
  invisible(NULL)
}

# Follow one receiver entering or leaving (a choice's check only).
availability_track_receiver <- function(av, node, value) {
  ck <- av$check
  if (is.null(ck) || is.null(ck$since2)) {
    return(invisible(NULL))
  }
  since <- ck$since2
  ck$since2 <- NULL
  if (value) {
    since <- write_entries(since, node, as.double(ck$n_dep))
  } else {
    if (ck$n_dep > since[[node]]) {
      ever <- ck$ever2
      ck$ever2 <- NULL
      ck$ever2 <- write_entries(ever, node, TRUE)
    }
    since <- write_entries(since, node, NA_real_)
  }
  ck$since2 <- since
  invisible(NULL)
}

# Check the dependent event just stored on row `row`. The first failure is
# kept, and raised here when the check is immediate. A kept verdict checks
# no later event, since that failure is what estimation reports.
availability_check_event <- function(av, row, sender, receiver) {
  ck <- av$check
  if (is.null(ck) || (!ck$immediate && !is.null(ck$failure))) {
    return(invisible(NULL))
  }
  failure <- NULL
  single <- FALSE
  if (identical(ck$family, "rate")) {
    if (!ck$positive[[sender]]) {
      failure <- list(type = "gated", event = row, sender = sender)
    } else {
      single <- ck$n_init_positive == 1
    }
  } else {
    # The sender's allowed, present receivers are counted by the cache the
    # walk keeps, and the observed one is a single cell, so an event reads no
    # row. The row is read only to name new candidates, and only when the
    # sender's allowed receivers may have changed since it was last read.
    n_allowed <- av$count[[sender]]
    availability_mark_candidates(av, ck, sender)
    if (n_allowed == 0) {
      failure <- list(type = "empty", event = row, sender = sender)
    } else if (!availability_check_cell(av, sender, receiver)) {
      failure <- list(
        type = "excluded",
        event = row,
        sender = sender,
        receiver = receiver
      )
    } else {
      single <- n_allowed == 1
    }
  }
  ck$n_dep <- ck$n_dep + 1
  if (single) {
    forced <- ck$forced
    ck$forced <- NULL
    if (ck$n_forced == length(forced)) {
      length(forced) <- 2L * length(forced)
    }
    ck$n_forced <- ck$n_forced + 1L
    forced[ck$n_forced] <- row
    ck$forced <- forced
  }
  if (!is.null(failure) && is.null(ck$failure)) {
    ck$failure <- failure
    if (ck$immediate) {
      validate_support_constraint(
        list(failure = failure),
        process_label = ck$label
      )
    }
  }
  invisible(NULL)
}

# The receivers sender `i` is allowed and finds present, as one row read at
# the mask's kind. The sender is not among its own candidates on one node
# set, whatever the mask allows on the diagonal.
availability_allowed_row <- function(av, sender) {
  n2 <- av$n2
  mask <- av$mask$value()
  allowed_row <- if (av$kind == 0L) {
    as.logical(mask[sender + av$check$row_offsets])
  } else if (symmetric_vector_mask(av)) {
    as.logical(mask) & isTRUE(as.logical(mask[[sender]]))
  } else {
    support_row(mask, av$kind, sender, av$n1, n2)
  }
  allowed_row <- allowed_row & av$p2
  if (av$drop_diagonal && sender <= n2) {
    allowed_row[[sender]] <- FALSE
  }
  allowed_row
}

# Name the sender's allowed receivers as candidates. A point mask gives
# each sender its own row, read once per epoch the sender checks an event
# in. Any other mask gives every sender the same row R, gated at most by the
# sender's own entry, so the candidates of an epoch are R less the first
# sender's own cell while one sender has checked, and all of R from the
# second distinct sender on: R is read once per epoch, the cell once.
availability_mark_candidates <- function(av, ck, sender) {
  if (av$kind == 0L) {
    if (ck$marked[[sender]] < ck$epoch) {
      availability_name_candidates(ck, availability_allowed_row(av, sender))
      marked <- ck$marked
      ck$marked <- NULL
      ck$marked <- write_entries(marked, sender, ck$epoch)
    }
    return(invisible(NULL))
  }
  if (!availability_sender_gate(av, sender)) {
    return(invisible(NULL))
  }
  if (ck$shared_epoch < ck$epoch) {
    availability_name_candidates(ck, availability_allowed_row(av, sender))
    ck$shared_epoch <- ck$epoch
    ck$first_sender <- sender
    ck$complete <- !av$drop_diagonal
  } else if (!ck$complete && sender != ck$first_sender) {
    first <- ck$first_sender
    if (first <= av$n2 && availability_check_cell(av, sender, first)) {
      availability_name_candidates(ck, first)
    }
    ck$complete <- TRUE
  }
  invisible(NULL)
}

# Name receivers as candidates: a logical row, or receiver indices. Only the
# ones not yet named are written.
availability_name_candidates <- function(ck, receivers) {
  fresh <- if (is.logical(receivers)) {
    which(receivers & !ck$candidate)
  } else {
    receivers[!ck$candidate[receivers]]
  }
  if (length(fresh) > 0L) {
    candidate <- ck$candidate
    ck$candidate <- NULL
    ck$candidate <- write_entries(candidate, fresh, rep(TRUE, length(fresh)))
  }
  invisible(NULL)
}

# Whether a sender may have any allowed receiver under a mask that is not a
# point grid: its own entry for an ego or a symmetric vector mask, the
# scalar for a global one, always for an alter one.
availability_sender_gate <- function(av, sender) {
  mask <- av$mask$value()
  if (av$kind == 2L || symmetric_vector_mask(av)) {
    return(isTRUE(as.logical(mask[[sender]])))
  }
  if (av$kind == 3L) {
    return(isTRUE(as.logical(mask)))
  }
  TRUE
}

# Whether the observed receiver is allowed and present for the sender: one
# cell, read at the mask's kind.
availability_check_cell <- function(av, sender, receiver) {
  if (!av$p2[[receiver]] || (av$drop_diagonal && sender == receiver)) {
    return(FALSE)
  }
  mask <- av$mask$value()
  if (symmetric_vector_mask(av)) {
    return(
      isTRUE(as.logical(mask[[sender]])) &&
        isTRUE(as.logical(mask[[receiver]]))
    )
  }
  isTRUE(as.logical(read_value_at_entries(
    mask,
    av$kind,
    sender + av$n1 * (receiver - 1L),
    0L,
    av$n1,
    av$n2
  )))
}

# What the check found over the whole sequence, in the form estimation
# raises it from (`validate_support_constraint()`).
availability_verdict <- function(av) {
  ck <- av$check
  seen <- function(ever, since) {
    ever | (!is.na(since) & ck$n_dep > since)
  }
  at_risk <- seen(ck$ever1, ck$since1)
  verdict <- list(
    family = ck$family,
    failure = ck$failure,
    forced = ck$forced[seq_len(ck$n_forced)],
    never_sender = which(ck$init1 & !at_risk)
  )
  if (identical(ck$family, "choice")) {
    present <- seen(ck$ever2, ck$since2)
    verdict$never_receiver <- which(present & !ck$candidate)
  }
  verdict
}

# Raise a support-constraint verdict: the error of its first failing event,
# or the warnings describing the whole sequence. `process_label` names the
# process the verdict belongs to when several are preprocessed together; it
# is rendered for display only, never parsed back.
validate_support_constraint <- function(verdict, process_label = NULL) {
  in_process <- if (is.null(process_label)) {
    NULL
  } else {
    c("i" = "Process {.field {process_label}}.")
  }
  failure <- verdict$failure
  if (!is.null(failure)) {
    e <- failure$event
    sender <- failure$sender
    receiver <- failure$receiver
    switch(
      failure$type,
      empty = cli::cli_abort(c(
        "{.arg support_constraint}: empty risk set at event {e}.",
        "x" = "Sender {sender} has no allowed, present receiver.",
        in_process
      )),
      excluded = cli::cli_abort(c(
        "{.arg support_constraint}: the observed dyad is excluded.",
        "x" = "Event {e}: receiver {receiver} is not allowed for
               sender {sender}.",
        in_process
      )),
      gated = cli::cli_abort(c(
        "{.arg support_constraint}: the observed sender is gated out.",
        "x" = "Event {e}: sender {sender} has no allowed, present
               receiver.",
        in_process
      ))
    )
  }

  if (identical(verdict$family, "choice")) {
    # "Present" here means present at some event across the sequence, not
    # at time zero: a receiver that joins mid-sequence and is never allowed
    # is named, and one absent throughout is not (it is not part of any
    # risk set).
    never <- verdict$never_receiver
    if (length(never) > 0) {
      cli::cli_warn(c(
        "!" = "{.arg support_constraint}: {length(never)} receiver{?s} present
               at some event, never an allowed candidate.",
        "i" = "{cli::qty(length(never))}Node{?s}: {.val {never}}."
      ))
    }
    # A sender allowed no receiver at any event is a legitimate model -- the
    # constraint may describe a wider population than the observed events --
    # so this is a warning, not an error, and mirrors the rate family's
    # "never at risk". An observed sender in this set errors above at its
    # own event; this names the pattern over the whole sequence for the
    # rest.
    never_sender <- verdict$never_sender
    if (length(never_sender) > 0) {
      cli::cli_warn(c(
        "!" = "{.arg support_constraint}: {length(never_sender)} present
               sender{?s} allowed no receiver at any event.",
        "i" = "{cli::qty(length(never_sender))}Node{?s}: {.val {never_sender}}."
      ))
    }
  } else {
    never <- verdict$never_sender
    if (length(never) > 0) {
      cli::cli_warn(c(
        "!" = "{.arg support_constraint}: {length(never)} present sender{?s}
               never at risk (always gated out).",
        "i" = "{cli::qty(length(never))}Node{?s}: {.val {never}}."
      ))
    }
  }

  forced <- verdict$forced
  if (length(forced) > 0) {
    cli::cli_warn(c(
      "!" = "{.arg support_constraint}: {length(forced)} event{?s} with a
             single candidate (forced choice; contributes 0 to the
             log-likelihood).",
      "i" = "{cli::qty(length(forced))}Event{?s}: {.val {forced}}."
    ))
  }
  invisible(NULL)
}
