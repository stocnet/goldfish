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
  switch(
    as.character(mask_kind),
    # scalar recycles everywhere; ego recycles down columns (row/sender axis);
    # alter fills across rows (col/receiver axis); point is already the grid.
    "3" = matrix(as.logical(support), n1, n2),
    "2" = matrix(as.logical(support), n1, n2),
    "1" = matrix(as.logical(support), n1, n2, byrow = TRUE),
    "0" = matrix(as.logical(support), n1, n2),
    cli::cli_abort("Unknown mask kind {.val {mask_kind}}.")
  )
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
