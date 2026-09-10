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
# is the whole extraction. `support_from_grid()` was already the reduction.
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
#' The inverse of [project_value()], and the reason a mask timeline fits in
#' memory. Like the `support_from_grid()` it generalizes, this reads one slice
#' rather than checking the value really is constant on the dropped axis: the
#' kind comes from the atoms' declared kinds, so a value that reaches here at
#' kind 1 IS row-constant by construction.
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
    return(switch(
      as.character(to),
      "3" = value[[1L]],
      "2" = value[, 1L],
      "1" = value[1L, ]
    ))
  }
  # ego or alter down to global: every entry is equal, so any of them will do.
  value[[1L]]
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
#' @noRd
read_value_at_cells <- function(value, kind, cells) {
  switch(
    as.character(kind),
    "3" = rep(value, nrow(cells)),
    "2" = value[cells[, 1L]],
    "1" = value[cells[, 2L]],
    "0" = value[cells],
    cli::cli_abort("Unknown broadcast kind {.val {kind}}.", .internal = TRUE)
  )
}
