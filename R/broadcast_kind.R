####################### #
#
# Goldfish package
# The shared vocabulary for values held at a broadcast kind
#
####################### #

# Four things in the preprocessing walk are the same kind of object: a value
# that varies on some subset of the dyad grid's axes, changes at events, and is
# consumed as a stream of changed entries. Statistics, interaction operands,
# constraint atoms, and the support mask. Each already declares a broadcast
# kind; each used to carry its own projection, expansion and diff.
#
# Every function here is an existing function with one hard-coded argument
# freed. `support_to_grid()` and `expand_operand_update()` were the same
# function with the target kind pinned to point; making the target a parameter
# is the whole extraction. The support mask's own reduction back to its kind
# was already `reduce_value()` with the source pinned to point.
#
# Broadcast kinds name the axes a value varies on:
#   3 global  neither axis   a scalar
#   2 ego     row / sender   a length-n1 vector
#   1 alter   col / receiver a length-n2 vector
#   0 point   both axes      a dense n1 x n2 matrix
#
# They form a lattice under `axis_union_kind()` with global at the bottom and
# point at the top, so a value can always be read AT a wider kind and never at a
# narrower one. Projection widens; reduction narrows and is defined only when
# the value really is constant on the axis being dropped.

#' Number of stored entries of a value held at `kind`
#' @noRd
kind_length <- function(kind, n1, n2) {
  switch(
    as.character(kind),
    "3" = 1L,
    "2" = as.integer(n1),
    "1" = as.integer(n2),
    "0" = as.integer(n1) * as.integer(n2),
    cli::cli_abort("Unknown broadcast kind {.val {kind}}.", .internal = TRUE)
  )
}

#' Read a value held at kind `from` as a value at the wider kind `to`
#'
#' `to` must be at or above `from` in the lattice, which
#' `axis_union_kind(c(from, to)) == to` states exactly. Widening to point is the
#' old `support_to_grid()`; widening a scalar to one axis is what lets a
#' separable constraint stay separable instead of materialising a grid.
#'
#' @param value the stored value.
#' @param from,to broadcast kinds.
#' @param n1,n2 sender and receiver counts.
#' @noRd
project_value <- function(value, from, to, n1, n2) {
  if (from == to) {
    return(value)
  }
  if (axis_union_kind(c(from, to)) != to) {
    cli::cli_abort(
      "Cannot project a value from kind {.val {from}} to {.val {to}}.",
      .internal = TRUE
    )
  }
  if (to == 0L) {
    # A scalar and an ego vector both recycle down columns; an alter vector
    # fills across rows.
    return(matrix(value, n1, n2, byrow = identical(as.integer(from), 1L)))
  }
  # `from` is global here: the only remaining widenings are scalar -> ego and
  # scalar -> alter, since ego and alter are incomparable.
  rep(value, kind_length(to, n1, n2))
}

#' Read a value held at kind `from` as a value at the narrower kind `to`
#'
#' The inverse of [project_value()]. This reads one slice rather than checking
#' the value really is constant on the dropped axis: the kind is declared, so a
#' value that reaches here at kind 1 IS column-constant by construction.
#'
#' **Away from the diagonal.** A dyad statistic is a broadcast everywhere except
#' on its own diagonal, which is zeroed because a node has no tie to itself. So
#' `ego(a)` with `a = (1, 2, 3)` is not row-constant: its `[1, 1]` entry is 0,
#' not `a[1]`. Reading the value for node `k` therefore has to come from a cell
#' whose OTHER index is not `k`, which is what the donor indices below pick.
#' Reading row or column 1 outright returns the diagonal entry for node 1 and
#' nothing else, which is why the defect it caused was one wrong node rather
#' than a visibly broken result. A two-mode grid has no diagonal, and an
#' off-diagonal read is equally correct there, so no branch on mode is needed.
#'
#' @param value the stored value.
#' @param from,to broadcast kinds.
#' @noRd
reduce_value <- function(value, from, to) {
  if (from == to) {
    return(value)
  }
  if (axis_union_kind(c(from, to)) != from) {
    cli::cli_abort(
      "Cannot reduce a value from kind {.val {from}} to {.val {to}}.",
      .internal = TRUE
    )
  }
  if (from == 0L) {
    n1 <- nrow(value)
    n2 <- ncol(value)
    return(switch(
      as.character(to),
      "3" = value[[off_diagonal_cell(n1, n2)]],
      "2" = value[cbind(seq_len(n1), donor_index(seq_len(n1), n2))],
      "1" = value[cbind(donor_index(seq_len(n2), n1), seq_len(n2))]
    ))
  }
  # ego or alter down to global: the value is already kind-shaped, so it carries
  # no diagonal and any entry will do.
  value[[1L]]
}

#' The index to read a broadcast value from, on the axis being dropped
#'
#' Anything but `node`, so the read never lands on the diagonal — index 1 for
#' every node but the first, and 2 for the first. A one-column or one-row grid
#' has no off-diagonal cell to offer, so it falls back to the only index there
#' is.
#'
#' @param node the node whose value is being read.
#' @param size the length of the axis being dropped.
#' @noRd
donor_index <- function(node, size) {
  if (size < 2L) {
    return(rep(1L, length(node)))
  }
  ifelse(node == 1L, 2L, 1L)
}

#' A cell of the grid that is not on its diagonal, as a linear index
#'
#' A global value varies on neither axis, so any off-diagonal cell carries it.
#' `donor_index()` cannot serve here: applied to both indices it picks `(2, 2)`,
#' which is back on the diagonal.
#' @noRd
off_diagonal_cell <- function(n1, n2) {
  if (n2 >= 2L) {
    return(1L + n1)
  }
  if (n1 >= 2L) {
    return(2L)
  }
  1L
}

#' Project a set of changed entries from kind `from` into kind `to`
#'
#' The entry addresses a value at kind `k` is indexed by: a single entry at
#' global, the sender index at ego, the receiver index at alter, and the
#' `(sender, receiver)` cell at point. Widening a delta means naming every entry
#' of the wider value the narrow one covers -- one alter entry is a whole column
#' of the grid -- which is what `expand_operand_update()` did for `to = point`
#' alone.
#'
#' @param node1,node2 the delta's sender / receiver indices (as the effect
#'   update closures emit them).
#' @param values the delta's replacement values, aligned with `node1`/`node2`.
#' @param from,to broadcast kinds.
#' @param n1,n2 sender and receiver counts.
#' @return `list(entries, values)`. `entries` is a two-column `(sender,
#'   receiver)` cell matrix when `to` is point, and an index vector otherwise.
#' @noRd
project_entries <- function(node1, node2, values, from, to, n1, n2) {
  if (axis_union_kind(c(from, to)) != to) {
    cli::cli_abort(
      "Cannot project entries from kind {.val {from}} to {.val {to}}.",
      .internal = TRUE
    )
  }
  # A global delta names the whole value whatever the target kind, and the last
  # write of the event wins.
  if (from == 3L) {
    last <- values[length(values)]
    if (to == 0L) {
      cells <- cbind(
        rep(seq_len(n1), times = n2),
        rep(seq_len(n2), each = n1)
      )
      return(list(entries = cells, values = rep(last, n1 * n2)))
    }
    len <- kind_length(to, n1, n2)
    return(list(entries = seq_len(len), values = rep(last, len)))
  }
  if (to != 0L) {
    # `from == to` is the only remaining case: ego and alter are incomparable,
    # and neither widens into the other without reaching point.
    return(list(
      entries = if (from == 2L) node1 else node2,
      values = values
    ))
  }
  if (from == 0L) {
    return(list(entries = cbind(node1, node2), values = values))
  }
  if (from == 1L) {
    # One receiver entry is a whole column of the grid.
    return(list(
      entries = cbind(
        rep(seq_len(n1), times = length(node2)),
        rep(node2, each = n1)
      ),
      values = rep(values, each = n1)
    ))
  }
  # ego: one sender entry is a whole row.
  list(
    entries = cbind(
      rep(node1, each = n2),
      rep(seq_len(n2), times = length(node1))
    ),
    values = rep(values, each = n2)
  )
}

#' Write entries into a buffer held at a broadcast kind
#'
#' `entries` is addressed as [project_entries()] returns it: a two-column cell
#' matrix for a point buffer, an index vector otherwise. A double or logical
#' buffer of any shape takes the C++ in-place write, under the aliasing
#' precondition every kind-shaped buffer meets by construction — each is
#' materialized fresh at seeding and lives in exactly one environment binding.
#' Anything else, or a value whose type does not already match the buffer's,
#' takes ordinary subassignment, so the buffer is returned either way and the
#' caller's code reads the same whether or not the write was in place.
#'
#' @param buffer the stored value.
#' @param entries,values as returned by [project_entries()].
#' @noRd
write_entries <- function(buffer, entries, values) {
  if (length(values) == 0L) {
    return(buffer)
  }
  if (
    typeof(buffer) == typeof(values) &&
      typeof(buffer) %in% c("double", "logical")
  ) {
    set_entries(buffer, linear_entries(buffer, entries), values)
    return(buffer)
  }
  buffer[entries] <- values
  buffer
}

#' Address entries of a buffer by 1-based linear index
#'
#' A cell matrix indexes a two-dimensional buffer by `(row, col)`; the in-place
#' writer takes one index, so the pair is folded down the buffer's own first
#' dimension. An index vector already addresses the buffer linearly.
#' @noRd
linear_entries <- function(buffer, entries) {
  if (!is.matrix(entries)) {
    return(as.integer(entries))
  }
  as.integer(entries[, 1L] + (entries[, 2L] - 1L) * nrow(buffer))
}

#' The entries at which two values of the same kind differ
#'
#' The per-step body of `crossings_from_vectors()`, freed from the timeline it
#' walked. `entries` restricts the comparison to the entries that could have
#' moved -- the locality every incremental consumer relies on -- and defaults to
#' comparing the whole value.
#'
#' @param previous,current two values at the same broadcast kind.
#' @param entries the entries to compare, or `NULL` for all of them.
#' @return `list(entries, values)` naming only the entries that changed.
#' @noRd
emit_crossings <- function(previous, current, entries = NULL) {
  if (is.null(entries)) {
    entries <- seq_along(current)
  }
  changed <- which(previous[entries] != current[entries])
  at <- if (is.matrix(entries)) {
    entries[changed, , drop = FALSE]
  } else {
    entries[changed]
  }
  list(entries = at, values = current[at])
}

#' Read a value held at `kind` at a set of point cells
#'
#' The counterpart of [project_entries()] on the read side, and what lets an
#' interaction product be recomputed at the cells its operands touched without
#' any operand being stored dense. A row-constant operand answers a cell by its
#' sender index, a column-constant one by its receiver index, and a scalar by
#' itself.
#'
#' @param value the stored value.
#' @param kind its broadcast kind.
#' @param cells a two-column `(sender, receiver)` cell matrix.
#' @param drop_diagonal answer a self-dyad with zero, as [project_value()] does.
#' @noRd
read_value_at_cells <- function(value, kind, cells, drop_diagonal = FALSE) {
  at_cells <- switch(
    as.character(kind),
    "3" = rep(value, nrow(cells)),
    "2" = value[cells[, 1L]],
    "1" = value[cells[, 2L]],
    "0" = value[cells],
    cli::cli_abort("Unknown broadcast kind {.val {kind}}.", .internal = TRUE)
  )
  if (drop_diagonal) {
    at_cells[cells[, 1L] == cells[, 2L]] <- 0
  }
  at_cells
}

#' Read a value held at `kind` at linear entries of a wider kind
#'
#' The same read as [read_value_at_cells()], addressed linearly rather than by
#' cell, which is what an incremental recompute works in: the entries that could
#' have moved are a set of positions in the target value, not a set of dyads.
#'
#' @param value the stored value.
#' @param kind its broadcast kind.
#' @param entries linear positions within a value at `target_kind`.
#' @param target_kind the kind those entries index.
#' @param n1,n2 sender and receiver counts.
#' @noRd
read_value_at_entries <- function(value, kind, entries, target_kind, n1, n2) {
  if (kind == target_kind) {
    return(value[entries])
  }
  if (kind == 3L) {
    return(rep(value, length(entries)))
  }
  # `target_kind` is point here: ego and alter are incomparable, and global was
  # handled above, so nothing else can widen.
  if (kind == 2L) {
    return(value[((entries - 1L) %% n1) + 1L])
  }
  value[((entries - 1L) %/% n1) + 1L]
}

#' The entries of a wider value that entries of a narrower one can affect
#'
#' The index-only half of [project_entries()], and the licence the incremental
#' recompute rests on: mask entry `e` depends on entry `e` of each atom, so a
#' change to an atom entry can move only the entries that entry projects onto.
#' One ego entry is a whole row of the grid, one alter entry a whole column, and
#' a global entry is everything.
#'
#' @param entries linear positions within a value at `from`.
#' @param from,to broadcast kinds, `to` at or above `from`.
#' @param n1,n2 sender and receiver counts.
#' @noRd
map_entries <- function(entries, from, to, n1, n2) {
  if (from == to) {
    return(entries)
  }
  if (from == 3L) {
    return(seq_len(kind_length(to, n1, n2)))
  }
  if (to != 0L) {
    cli::cli_abort(
      "Cannot map entries from kind {.val {from}} to {.val {to}}.",
      .internal = TRUE
    )
  }
  if (from == 2L) {
    return(as.integer(outer(entries, (seq_len(n2) - 1L) * n1, "+")))
  }
  as.integer(outer(seq_len(n1), (entries - 1L) * n1, "+"))
}

#' Keep one write per entry, the last
#'
#' A broadcast effect reports its delta on the dyad grid's terms: an alter-kind
#' effect emits one row per sender for the one receiver whose value moved, so a
#' delta that is one entry wide at the effect's own kind arrives n1 rows long.
#' Only the last write of an entry is observable, so collapsing them is exact,
#' and it is the difference between writing one entry and writing the same entry
#' n1 times.
#'
#' @param entries,values a delta at one broadcast kind.
#' @noRd
collapse_entries <- function(entries, values) {
  keep <- !duplicated(entries, fromLast = TRUE)
  list(entries = entries[keep], values = values[keep])
}

#' An effect's own delta as one write per entry at its kind
#'
#' The shared step every path that stores a value at its own kind performs: an
#' interaction operand, a constraint atom, and a broadcast column each project
#' the effect's `(node1, node2, replace)` block into their kind and then keep
#' one write per entry. Projecting with `from == to` names the entries the block
#' touches, `linear_entries()` folds a point delta's cells to addresses so the
#' collapse compares addresses rather than rows, and [collapse_entries()] keeps
#' the last write of each -- exact, since only the last write of an entry is
#' observable, and the difference between writing one entry and writing the same
#' entry n1 times.
#'
#' `buffer` is consulted only to fold a point delta by its first dimension; a
#' delta at any other kind carries a vector of addresses and ignores it, so a
#' caller with no buffer (a value that never varies on both axes) may pass
#' `NULL`.
#'
#' @param buffer the stored value the delta writes into, or `NULL` when the kind
#'   is never point.
#' @param node1,node2 the effect's sender / receiver indices.
#' @param values the delta's replacement values, aligned with `node1`/`node2`.
#' @param kind the value's broadcast kind.
#' @param n1,n2 sender and receiver counts, as [project_entries()] takes them.
#' @return `list(entries, values)` at linear addresses of a value at `kind`,
#'   one write per address.
#' @noRd
collapse_operand_delta <- function(buffer, node1, node2, values, kind, n1, n2) {
  delta <- project_entries(node1, node2, values, kind, kind, n1, n2)
  collapse_entries(linear_entries(buffer, delta$entries), delta$values)
}

#' Of a set of candidate writes, the ones that actually change the buffer
#'
#' The locality primitive: an incremental recompute produces a value for every
#' entry that COULD have moved, and only the ones that did belong in an update
#' stream. [emit_crossings()] is the same question asked of two whole values.
#'
#' @param buffer the current value.
#' @param entries linear positions within it.
#' @param values what those entries would become.
#' @return `list(entries, values)` naming only the entries that differ.
#' @noRd
changed_entries <- function(buffer, entries, values) {
  moved <- which(buffer[entries] != values)
  list(entries = entries[moved], values = values[moved])
}
