#' Convert 2-column rate changes to 3-column ego perspective
#'
#' Expands actor-level changes from the rate model format to the ego-perspective
#' format used by choice and REM models. Each affected actor (node1) is expanded
#' to rows covering all alters (node2).
#'
#' @param changes matrix with columns node1 and replace, or NULL
#' @param n2 integer number of alters / receivers
#' @param is_two_mode logical whether the network is two-mode
#' @return matrix with columns node1, node2, replace, or NULL
#' @export
to_ego <- function(changes, n2, is_two_mode = FALSE) {
  if (is.null(changes)) return(NULL)
  if (!is.matrix(changes)) {
    changes <- matrix(changes, nrow = 1, dimnames = list(NULL, names(changes)))
  }
  do.call(rbind, lapply(seq_len(nrow(changes)), function(i) {
    node_val <- changes[i, "node1"]
    rep_val <- changes[i, "replace"]
    others <- if (is_two_mode) seq_len(n2) else setdiff(seq_len(n2), node_val)
    cbind(node1 = node_val, node2 = others, replace = rep_val)
  }))
}

#' Convert 2-column rate changes to 3-column alter perspective
#'
#' Expands actor-level changes from the rate model format to the alter-perspective
#' format used by choice and REM models. Each affected actor (node1 in the 2-col
#' format, acting as the alter) is expanded to rows covering all egos (node1 in
#' the 3-col format).
#'
#' @param changes matrix with columns node1 and replace, or NULL
#' @param n1 integer number of senders / egos
#' @param is_two_mode logical whether the network is two-mode
#' @return matrix with columns node1, node2, replace, or NULL
#' @export
to_alter <- function(changes, n1, is_two_mode = FALSE) {
  if (is.null(changes)) return(NULL)
  if (!is.matrix(changes)) {
    changes <- matrix(changes, nrow = 1, dimnames = list(NULL, names(changes)))
  }
  do.call(rbind, lapply(seq_len(nrow(changes)), function(i) {
    node_val <- changes[i, "node1"]
    rep_val <- changes[i, "replace"]
    others <- if (is_two_mode) seq_len(n1) else setdiff(seq_len(n1), node_val)
    cbind(node1 = others, node2 = node_val, replace = rep_val)
  }))
}
