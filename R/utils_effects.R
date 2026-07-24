#' Expand 2-column rate changes to 3-column ego or alter perspective
#'
#' Helper functions used internally to promote actor-level change matrices
#' (2 columns: `node1`, `replace`) produced by DyNAM-rate effects into the
#' dyad-level format (3 columns: `node1`, `node2`, `replace`) expected by
#' choice and REM effect wrappers.
#'
#' `to_ego()` repeats each affected actor across all alters (ego perspective).
#' `to_alter()` repeats each affected actor across all egos (alter perspective).
#'
#' @param changes a matrix with columns `node1` and `replace`, or `NULL`.
#' @param n2 integer. Number of alters / receivers (used by `to_ego()`).
#' @param n1 integer. Number of senders / egos (used by `to_alter()`).
#' @param is_two_mode logical. Whether the network is two-mode; if `FALSE`
#'   (default) the focal actor is excluded from the expanded rows.
#' @return A matrix with columns `node1`, `node2`, `replace`, or `NULL` when
#'   `changes` is `NULL`.
#' @keywords internal
#' @name utils-effects
NULL

#' @rdname utils-effects
to_ego <- function(changes, n2, is_two_mode = FALSE) {
  if (is.null(changes)) {
    return(NULL)
  }
  if (!is.matrix(changes)) {
    changes <- matrix(changes, nrow = 1, dimnames = list(NULL, names(changes)))
  }
  do.call(
    rbind,
    lapply(seq_len(nrow(changes)), function(i) {
      node_val <- changes[i, "node1"]
      rep_val <- changes[i, "replace"]
      others <- if (is_two_mode) seq_len(n2) else setdiff(seq_len(n2), node_val)
      cbind(node1 = node_val, node2 = others, replace = rep_val)
    })
  )
}

#' @rdname utils-effects
to_alter <- function(changes, n1, is_two_mode = FALSE) {
  if (is.null(changes)) {
    return(NULL)
  }
  if (!is.matrix(changes)) {
    changes <- matrix(changes, nrow = 1, dimnames = list(NULL, names(changes)))
  }
  do.call(
    rbind,
    lapply(seq_len(nrow(changes)), function(i) {
      node_val <- changes[i, "node1"]
      rep_val <- changes[i, "replace"]
      others <- if (is_two_mode) seq_len(n1) else setdiff(seq_len(n1), node_val)
      cbind(node1 = others, node2 = node_val, replace = rep_val)
    })
  )
}
