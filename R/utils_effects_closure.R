#' Apply two-path cache update with pmax clamping
#'
#' @param res list with `cache` (square matrix) and `changes` (NULL or array)
#' @param ids integer matrix with two columns (node1 indices, node2 indices);
#'   if zero-length, res is returned unchanged
#' @param replace numeric. New value of the tie (sign-normalised)
#' @param old_value numeric. Previous value of the tie (sign-normalised)
#' @param transformer_fn function applied to the updated counts before storing
#'   in `res$changes`
#'
#' @return Updated `res` list
#' @noRd
apply_two_path_update <- function(res, ids, replace, old_value, transformer_fn) {
  if (length(ids) > 0) {
    replaceValues <- pmax(
      0L,
      replace - old_value + res$cache[cbind(ids[, 1], ids[, 2])]
    )
    res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
    res$changes <- cbind(
      node1 = ids[, 1],
      node2 = ids[, 2],
      replace = forceAndCall(1, transformer_fn, replaceValues)
    )
  }
  res
}
