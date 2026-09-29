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
#   alter    choice with a column-broadcast mask: one receiver vector.
#   outer    choice with a sender-only mask: receiver presence alone.
#   point    choice with a dyadic mask or an opportunity list: the grid of
#            present receivers each sender is allowed.
#   dyad     REM and coordination with a constraint: both presences and the
#            mask on the grid, coordination symmetrised.
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
# other. `record = FALSE` keeps no stream.
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
  record = TRUE
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
  if (identical(variant, "gate")) {
    av$count <- as.double(initial_receiver_count(
      mask$value(),
      stored_kind,
      av$p2,
      n1,
      n2,
      drop_diagonal
    ))
  }
  if (identical(variant, "presence")) {
    av$n_at_risk <- sum(av$p1)
  }
  av$record <- record
  if (record) {
    av$n_changes <- integer(64L)
    av$node1 <- double(256L)
    av$node2 <- double(256L)
    av$repl <- double(256L)
    av$n_emitted <- 0L
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
# (an opportunity row is set there).
availability_step <- function(av, target1, target2, sender = NA_integer_) {
  first <- is.null(av$value)
  moved1 <- availability_take_changes(av, 1L, target1)
  switch(
    av$variant,
    gate = availability_step_gate(av, moved1, target2, first),
    presence = availability_step_presence(av, moved1, target2),
    alter = ,
    outer = availability_step_receivers(av, moved1, target2, first),
    point = availability_step_point(av, moved1, target2, sender, first),
    dyad = availability_step_dyad(av, moved1, target2, first)
  )
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
availability_step_gate <- function(av, moved1, target2, first) {
  n1 <- av$n1
  affected <- integer(0)
  for (flips in av$pending) {
    affected <- c(affected, availability_gate_flips(av, flips))
  }
  moved2 <- availability_take_changes(av, 2L, target2)
  if (!is.null(moved2)) {
    for (k in seq_along(moved2$node)) {
      affected <- c(
        affected,
        availability_gate_receiver(av, moved2$node[[k]], moved2$replace[[k]])
      )
    }
  }
  affected <- c(affected, availability_write_presence(av, 1L, moved1))
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
  reached <- switch(
    as.character(av$kind),
    "0" = which(as.logical(mask[(node - 1L) * n1 + seq_len(n1)])),
    "1" = if (isTRUE(as.logical(mask[[node]]))) seq_len(n1) else integer(0),
    "2" = which(as.logical(mask)),
    "3" = if (isTRUE(as.logical(mask))) seq_len(n1) else integer(0)
  )
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
  reached
}

## Rate presence ----

# No constraint: the stored stream is the writer's; only the count at risk
# is kept.
availability_step_presence <- function(av, moved1, target2) {
  availability_take_changes(av, 2L, target2)
  if (!is.null(moved1)) {
    availability_write_presence(av, 1L, moved1)
    av$n_at_risk <- sum(av$p1)
  }
  invisible(NULL)
}

## Choice, receiver axis ----

# The alter encoding reads presence and the column-broadcast mask; the outer
# encoding reads presence alone (its mask gates senders, which the choice
# likelihood never reads off its own observed sender).
availability_step_receivers <- function(av, moved1, target2, first) {
  availability_write_presence(av, 1L, moved1)
  affected <- availability_write_presence(
    av,
    2L,
    availability_take_changes(av, 2L, target2)
  )
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
availability_step_point <- function(av, moved1, target2, sender, first) {
  n1 <- av$n1
  n2 <- av$n2
  availability_write_presence(av, 1L, moved1)
  moved2 <- availability_write_presence(
    av,
    2L,
    availability_take_changes(av, 2L, target2)
  )
  cells <- integer(0)
  for (j in moved2) {
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
availability_step_dyad <- function(av, moved1, target2, first) {
  n1 <- av$n1
  n2 <- av$n2
  senders <- availability_write_presence(av, 1L, moved1)
  receivers <- availability_write_presence(
    av,
    2L,
    availability_take_changes(av, 2L, target2)
  )
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

## Shared bookkeeping ----

# The first row: the value is computed whole, once, and the stream's initial
# value is its own object, since the value is written in place from here on.
availability_open <- function(av, value) {
  av$value <- value & TRUE
  av$n_at_risk <- sum(av$value)
  if (av$record) {
    av$init <- value
    availability_emit(av, integer(0), logical(0))
  }
  invisible(NULL)
}

availability_write_value <- function(av, flips) {
  if (length(flips$entries) == 0L) {
    return(invisible(NULL))
  }
  av$n_at_risk <- av$n_at_risk + sum(flips$values) - sum(!flips$values)
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

# Write a fid's availability onto its finalized output, with the fields the
# fail-fast validation reads (the raw presences the stored value no longer
# shows) stashed on its support mask.
availability_finish <- function(av, out) {
  n_stored <- length(out$event_time)
  if (identical(av$variant, "presence") || n_stored == 0L) {
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
    out$support_mask$sender_presence_init <- out$active_sender_init
    out$active_sender_init <- stream$init
    out$active_sender_update <- stream$update
    out$active_sender_update_pointer <- stream$pointer
    out$active_sender_changes <- list()
    out$active_sender_folded <- TRUE
    return(out)
  }
  if (identical(av$variant, "dyad")) {
    out$support_mask$sender_presence_init <- out$active_sender_init
    out$support_mask$receiver_presence_init <- out$active_dyad_init
  } else if (!is.null(out$support_mask)) {
    out$support_mask$receiver_presence_init <- out$active_dyad_init
    out$support_mask$receiver_presence_update <- out$active_dyad_update
    out$support_mask$receiver_presence_update_pointer <-
      out$active_dyad_update_pointer
  }
  out$active_dyad_init <- stream$init
  out$active_dyad_update <- stream$update
  out$active_dyad_update_pointer <- stream$pointer
  out$active_dyad_encoding <- if (av$variant %in% c("dyad", "point")) {
    "point"
  } else {
    av$variant
  }
  out$active_dyad_folded <- TRUE
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
