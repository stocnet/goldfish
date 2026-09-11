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
  abort_if_stale_support_mask(support_mask)
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

#' A reader of a support mask's per-event flips
#'
#' [mask_cursor()] answers "what is the mask now"; this answers "what just
#' changed", which is what an incrementally maintained consumer needs. A
#' consumer that keeps a derived quantity -- a per-sender count of allowed
#' receivers, say -- adjusts it from the flips and never reconstructs the mask
#' at all.
#'
#' @param support_mask the `support_mask` list a preprocessing produced.
#' @return a function of the stored event index returning
#'   `list(entries, values)` for that event, to be called with ascending
#'   indices.
#' @noRd
mask_flips <- function(support_mask) {
  abort_if_stale_support_mask(support_mask)
  flip_reader(support_mask$update, support_mask$update_pointer)
}

#' Refuse a support mask that predates the update stream
#'
#' A preprocessed object built before the mask became a stream carries its
#' timeline as `support`, one stored value per event, and no update buffer.
#' Reading it through a cursor would not fail: it would find no flips and hand
#' back the initial mask at every event, so the model would be estimated on a
#' constraint frozen at time zero.
#'
#' The epoch stamp cannot catch this. `prep_version` moves once per RELEASE
#' whose layout differs from the previous release's, and both layouts are epoch
#' 2 within this development line, so the object is current in stamp and stale
#' in content -- the case `format_version.R` names explicitly and assigns to the
#' consumer. This is that consumer check: it names the component it needs rather
#' than guessing at the object's age.
#'
#' @param support_mask a preprocessed object's `support_mask`.
#' @noRd
abort_if_stale_support_mask <- function(
  support_mask,
  call = rlang::caller_env()
) {
  if (!is.null(support_mask$update)) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "This preprocessed object's {.field support_mask} predates the mask
       update stream.",
      "x" = "It carries a stored mask per event ({.field support}) and no
             {.field update} buffer, so its constraint cannot be read over
             time.",
      "i" = "Preprocess the specification again with this version of
             {.pkg goldfish}."
    ),
    call = call
  )
}

#' The per-event slice of any flat `(entry, replace)` buffer
#'
#' The support mask's update stream and the two presence crossings buffers are
#' the same shape -- entries in the first row, replacements in the second, keyed
#' by a cumulative per-event pointer -- so one reader serves all three.
#'
#' @param update a 2-row `(entry, replace)` matrix.
#' @param pointer the cumulative per-event pointer into its columns.
#' @return a function of the stored event index, to be called with ascending
#'   indices.
#' @noRd
flip_reader <- function(update, pointer) {
  seen <- 0L
  function(e) {
    hi <- if (is.null(pointer) || length(pointer) < e) 0L else pointer[[e]]
    if (hi <= seen) {
      return(list(entries = integer(0), values = logical(0)))
    }
    cols <- (seen + 1L):hi
    seen <<- hi
    list(
      entries = as.integer(update[1L, cols]),
      values = as.logical(update[2L, cols])
    )
  }
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
    "0" = as.logical(matrix(support, n1, n2)[sender, ]),
    cli::cli_abort("Unknown mask kind {.val {stored_kind}}.")
  )
}

#' Which senders have at least one allowed, present receiver
#'
#' The row reduction the DyNAM-rate gate is defined by, read at the mask's own
#' kind. A separable mask answers it without a grid: an alter mask gives every
#' sender the same answer, an ego mask gives each sender its own, and a global
#' mask gives everyone the same. Only a genuinely dyadic mask is reduced row by
#' row, and even then the grid is built once for the event rather than as a
#' `rep()`-broadcast intermediate.
#'
#' The preprocessing folds MAINTAIN this as a count rather than recomputing it;
#' this is the from-scratch form, for a consumer that reads one event at a time
#' and has no count to carry.
#'
#' @param support the mask at `stored_kind`.
#' @param stored_kind its broadcast kind.
#' @param active_2 receiver presence.
#' @param n1,n2 sender and receiver counts.
#' @return a length-n1 logical vector.
#' @noRd
sender_gate_from_mask <- function(support, stored_kind, active_2, n1, n2) {
  if (is.null(support)) {
    return(rep(any(active_2), n1))
  }
  switch(
    as.character(stored_kind),
    "3" = rep(isTRUE(as.logical(support)) && any(active_2), n1),
    "2" = as.logical(support) & any(active_2),
    "1" = rep(any(as.logical(support) & active_2), n1),
    "0" = rowSums(matrix(as.logical(support), n1, n2)[,
      active_2,
      drop = FALSE
    ]) >
      0,
    cli::cli_abort("Unknown mask kind {.val {stored_kind}}.")
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
