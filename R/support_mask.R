####################### #
#
# Goldfish package
# Assembling the support_constraint active mask from its atoms
#
####################### #

#' Evaluate the constraint boolean tree into the support mask
#'
#' Binds each atom's current statistic value to its `.a{k}` placeholder and
#' evaluates the parsed mask expression. The result's shape (scalar / length-n1
#' / length-n2 / n1xn2) follows from the atoms' shapes, i.e. it is stored at the
#' constraint's `mask_kind` (the axis-union of its atoms' broadcast kinds).
#'
#' @param atom_values a named list mapping `.a1, .a2, ...` to the atoms' current
#'   statistic values (aligned with the parser's atom order).
#' @param expr the evaluable mask expression from `parse_support_constraint()`.
#' @return the support mask at its natural broadcast kind.
#' @noRd
assemble_support_mask <- function(atom_values, expr) {
  env <- list2env(atom_values, parent = baseenv())
  eval(expr, envir = env)
}

#' Broadcast a support mask stored at `mask_kind` onto the n1 x n2 dyad grid
#'
#' The mask is kept at its axis-union kind to avoid materialising a dense matrix
#' for separable constraints; this expands it only where a full grid is needed
#' (mask assembly / from-scratch checks). `NULL` support (no constraint) is the
#' all-allowed grid.
#'
#' @param support the support mask (scalar, length-n1, length-n2, or n1xn2), or
#'   `NULL` for no constraint.
#' @param mask_kind `3` global (scalar), `2` ego (row/sender), `1` alter
#'   (col/receiver), `0` point (dense).
#' @param n1,n2 sender and receiver counts.
#' @return an n1 x n2 logical matrix.
#' @noRd
support_to_grid <- function(support, mask_kind, n1, n2) {
  if (is.null(support)) {
    return(matrix(TRUE, n1, n2))
  }
  matrix(
    as.logical(project_value(support, mask_kind, 0L, n1, n2)),
    n1,
    n2
  )
}

#' One sender's row of a support mask, without building the grid
#'
#' Every dyad-side consumer reads the mask one row at a time -- the choice risk
#' set is the event sender's row, and an alter fold reads any row because the
#' mask is column-broadcast. Going through [support_to_grid()] to take one row
#' materializes n1 x n2 to use n2 of it, which at 1899 actors and 1500 events is
#' 41 GB of grids built and discarded.
#'
#' @param support the mask at `stored_kind`, or `NULL` for no constraint.
#' @param stored_kind its broadcast kind.
#' @param sender the row to read; ignored for a mask that does not vary by
#'   sender.
#' @param n1,n2 sender and receiver counts.
#' @return a length-n2 logical vector.
#' @noRd
support_row <- function(support, stored_kind, sender, n1, n2) {
  if (is.null(support)) {
    return(rep(TRUE, n2))
  }
  switch(
    as.character(stored_kind),
    "3" = rep(as.logical(support), n2),
    "2" = rep(as.logical(support[[sender]]), n2),
    "1" = as.logical(support),
    # The stored vector is the grid in column-major order, so row `sender`
    # is every n1-th entry from `sender` on; reshaping it into a matrix
    # first would copy all n1 x n2 cells to read n2 of them.
    "0" = as.logical(support[sender + n1 * (seq_len(n2) - 1L)]),
    cli::cli_abort("Unknown mask kind {.val {stored_kind}}.")
  )
}

#' Which senders have at least one allowed, present receiver
#'
#' The row reduction the DyNAM-rate gate is defined by, read at the mask's own
#' kind. A separable mask answers it without a grid: an alter mask gives every
#' sender the same answer, an ego mask gives each sender its own, and a global
#' mask gives everyone the same. Only a genuinely dyadic mask is reduced row by
#' row, and even then over its stored vector, without building the grid.
#'
#' The preprocessing folds MAINTAIN this as a count rather than recomputing it;
#' this is the from-scratch form, for a consumer that reads one event at a time
#' and has no count to carry.
#'
#' A sender is not a receiver of its own event on a one-mode layer, so
#' `drop_diagonal` takes each sender's own cell back out of its row. The mask
#' itself is untouched: it answers "is this dyad allowed" everywhere, and a
#' creation flavor's mask allows the self-dyad forever, since no actor holds a
#' tie to itself.
#'
#' @param support the mask at `stored_kind`.
#' @param stored_kind its broadcast kind.
#' @param active_2 receiver presence.
#' @param n1,n2 sender and receiver counts.
#' @param drop_diagonal exclude each sender's own cell, as a one-mode risk set
#'   does. Required: the two node sets decide it, and a silent default is how
#'   the gate came to count self-dyads in the first place.
#' @return a length-n1 logical vector.
#' @noRd
sender_gate_from_mask <- function(
  support,
  stored_kind,
  active_2,
  n1,
  n2,
  drop_diagonal
) {
  if (is.null(support)) {
    return(rep(any(active_2), n1))
  }
  abort_if_diagonal_off_grid(drop_diagonal, n1, n2)
  # Each sender's own cell, allowed and present, or no correction at all.
  # Computed by branch and never as a term multiplied by zero: on a two-mode
  # grid the diagonal index does not exist, and `0 * NA` is `NA`.
  own <- if (drop_diagonal) {
    switch(
      as.character(stored_kind),
      "3" = rep(isTRUE(as.logical(support)), n1),
      "2" = as.logical(support),
      "1" = as.logical(support)[seq_len(n1)],
      "0" = as.logical(support)[(seq_len(n1) - 1L) * n1 + seq_len(n1)]
    ) &
      active_2[seq_len(n1)]
  } else {
    rep(FALSE, n1)
  }
  count <- switch(
    as.character(stored_kind),
    "3" = if (isTRUE(as.logical(support))) rep(sum(active_2), n1) else 0L,
    "2" = ifelse(as.logical(support), sum(active_2), 0L),
    "1" = rep(sum(as.logical(support) & active_2), n1),
    "0" = point_receiver_count(support, active_2, n1, n2),
    cli::cli_abort("Unknown mask kind {.val {stored_kind}}.")
  )
  count - own > 0
}

# Each sender's allowed, present receivers under a dyadic (kind 0) mask, the
# self-dyad included. The count is taken on the stored column-major vector:
# with every receiver present that is a pass over it, and otherwise only the
# present receivers' columns are copied, never the whole grid twice as a
# reshape and a column subset would.
point_receiver_count <- function(support, active_2, n1, n2) {
  if (!is.logical(support)) {
    support <- as.logical(support)
  }
  if (all(active_2)) {
    return(.rowSums(support, n1, n2))
  }
  columns <- which(as.logical(active_2))
  cells <- rep((columns - 1L) * n1, each = n1) + seq_len(n1)
  .rowSums(support[cells], n1, length(columns))
}

# A self-dyad exists only where both axes name one node set. A caller asking
# to drop it on a two-mode grid has the wrong flag, and reading a diagonal
# that is not there would hand back NA rather than fail.
abort_if_diagonal_off_grid <- function(drop_diagonal, n1, n2) {
  if (drop_diagonal && n1 != n2) {
    cli::cli_abort(
      "A one-mode gate needs a square grid, not {n1} x {n2}.",
      .internal = TRUE
    )
  }
  invisible(NULL)
}

#' Symmetrise a point-kind mask (`mask & t(mask)`)
#'
#' For DyNAM `choice_coordination` and REM on an undirected network a mutual /
#' undirected dyad is active only when both directions are allowed.
#' @noRd
symmetrize_mask <- function(mask) {
  mask & t(mask)
}

#' Assemble the effective per-model risk-set mask
#'
#' Conjoins sender presence (`active_1`, always), the support constraint
#' (`support`, at `mask_kind`), and receiver presence (`active_2`, always):
#'
#' \preformatted{
#'   rate   (sender gate) : active[i]  = active_1[i] & (any allowed receiver j)
#'   choice (row filter)  : cand[i, ]  = active_1[i] & active_2 & support[i, ]
#'   REM    (full matrix) : mask[i, j] = active_1[i] & support[i, j] & active_2[j]
#' }
#'
#' Presence factors are never bypassed, so an absent node is excluded regardless
#' of the constraint. When `support` is `NULL` the mask degenerates to the
#' separable presence product.
#'
#' The self-dyad is the CALLER's to exclude: this assembler reduces the grid it
#' is handed, so a one-mode caller hands it a grid whose diagonal is already
#' off. The maintained gate ([sender_gate_from_mask()], the preprocessing fold)
#' takes `drop_diagonal` instead, since it never builds a grid.
#'
#' @param support the support mask at `mask_kind`, or `NULL` for no constraint.
#' @param active_1,active_2 logical sender / receiver presence vectors.
#' @param mask_kind the support's broadcast kind (see `support_to_grid()`).
#' @param model one of `"rate"`, `"choice"`, `"REM"`.
#' @param symmetric symmetrise the dyad grid (coordination / undirected REM).
#' @return a length-n1 logical sender gate for `"rate"`, else an n1 x n2 logical
#'   candidate/risk matrix.
#' @noRd
assemble_model_mask <- function(
  support,
  active_1,
  active_2,
  mask_kind = 0L,
  model = c("rate", "choice", "REM"),
  symmetric = FALSE
) {
  model <- match.arg(model)
  n1 <- length(active_1)
  n2 <- length(active_2)
  grid <- support_to_grid(support, mask_kind, n1, n2)
  full <- outer(as.logical(active_1), as.logical(active_2), "&") & grid
  if (symmetric) {
    full <- symmetrize_mask(full)
  }
  switch(
    model,
    rate = rowSums(full) > 0,
    choice = full,
    REM = full
  )
}
