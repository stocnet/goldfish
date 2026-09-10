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

#' Reduce a dense support grid to its axis-union kind
#'
#' The inverse of [support_to_grid()], and the reason the mask timeline fits in
#' memory. A constraint separable on one axis carries the same information in a
#' length-n1 or length-n2 vector, or in one scalar, as it does in an n1 x n2
#' matrix — and there is one stored mask per snapshot time, so on 1899 actors
#' and 59,835 snapshots the dense form is about 860 GB while the alter form is
#' 114 MB.
#'
#' The reduction reads one slice rather than checking the grid is really
#' separable: the kind comes from `active_dyad_encoding_decide()`, which derives
#' it from the atoms' broadcast kinds, so a mask that reaches here at kind 1 IS
#' row-constant by construction.
#'
#' @param grid an n1 x n2 logical matrix.
#' @param mask_kind `3` global (scalar), `2` ego (row/sender), `1` alter
#'   (col/receiver), `0` point (dense, returned unchanged).
#' @return the support at `mask_kind`.
#' @noRd
support_from_grid <- function(grid, mask_kind) {
  reduce_value(grid, 0L, mask_kind)
}

#' A cursor over a support mask's flat update stream
#'
#' The mask reaches its consumers as an initial value plus a stream of the
#' entries that flipped, keyed by a cumulative per-event pointer, instead of one
#' stored value per event. Every consumer walks the stored events in order, so a
#' cursor that advances with them reconstructs the value at each without the
#' timeline ever existing at once. That difference is the whole point: on 1899
#' actors and 1500 events the stored timeline was 10.95 MB carrying one changed
#' entry.
#'
#' The cursor returns a fresh value rather than mutating one in place, so a
#' consumer may hold on to what it was given. That costs one kind-sized copy per
#' event, which for a separable mask is a vector and for a point mask is the
#' grid the consumer was going to build anyway.
#'
#' @param support_mask the `support_mask` list a preprocessing produced, or
#'   `NULL` for an unconstrained model.
#' @return a function of the stored event index returning the mask at its stored
#'   kind, to be called with ascending indices.
#' @noRd
mask_cursor <- function(support_mask) {
  if (is.null(support_mask)) {
    return(function(e) NULL)
  }
  value <- support_mask$initial
  update <- support_mask$update
  pointer <- support_mask$update_pointer
  seen <- 0L
  function(e) {
    hi <- if (is.null(pointer) || length(pointer) < e) 0L else pointer[[e]]
    if (hi > seen) {
      at <- update[1L, (seen + 1L):hi]
      value[at] <<- as.logical(update[2L, (seen + 1L):hi])
      seen <<- hi
    }
    value
  }
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
