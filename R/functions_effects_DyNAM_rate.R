# define methods ----------------------------------------------------------
# init the statistical matrix
init_DyNAM_rate <- function(effectFun, ...) {
  UseMethod("init_DyNAM_rate", effectFun)
}

# default -----------------------------------------------------------------
#' @export
init_DyNAM_rate.default <- function(
    effectFun,
    network = NULL, attribute = NULL,
    window,
    n1, n2, ...) {
  init_DyNAM_choice.default(
    effectFun = effectFun,
    network = network, attribute = attribute,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

# Structural effects ------------------------------------------------------
# indeg -------------------------------------------------------------------
#' init stat matrix indegree using cache
#'
#' @param effectFun function with additional parameters weighted,
#'  is_two_mode, transformer_fn
#' @param network matrix n1*n2
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list with named components: cache numeric vector size n2,
#'  stat matrix numeric n1*n2
#' @noRd
#' @export
#'
#' @examples
#' \donttest{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0, 3,
#'     1, 0, 0, 0, 0, 4,
#'     1, 2, 0, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 6, byrow = TRUE
#' )
#' effectFUN <- function(
#'     weighted = TRUE, is_two_mode = TRUE, transformer_fn = identity) {
#'   NULL
#' }
#' init_REM_choice.indeg(effectFUN, network, 5, 6)
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 3,
#'     1, 0, 0, 0, 4,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' effectFUN <- function(
#'     weighted = TRUE, is_two_mode = FALSE, transformer_fn = identity) {
#'   NULL
#' }
#' init_DyNAM_rate.indeg(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'     weighted = TRUE, is_two_mode = FALSE, transformer_fn = identity,
#'     type = "alter") {
#'   NULL
#' }
#' init_DyNAM_rate.indeg(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_rate.indeg <- function(effectFun, network, window, n1, n2, ...) {
  formals(effectFun) <- c(formals(effectFun), list(type = "ego"))
  init_REM_choice.indeg(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

#' update stat indegree using cache ego
#'
#' @param network matrix n1*n2
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache numeric vector size n2
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#' @param is_two_mode logical
#' @param weighted logical
#' @param transformer_fn function to apply to the stat
#'
#' @return list:
#'   cache numeric vector size n2,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#'
#' @examples
#' \donttest{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 3,
#'     1, 0, 0, 0, 4,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' cache <- c(2, 7, 0, 1, 7)
#' update_DyNAM_rate_indeg(
#'   network,
#'   1, 2, 3,
#'   cache, 5, 5,
#'   is_two_mode = TRUE, weighted = TRUE, transformer_fn = sqrt
#' )
#' }
update_DyNAM_rate_indeg <- function(
    network,
    sender, receiver, replace, cache,
    n1, n2, is_two_mode = FALSE,
    weighted = FALSE, transformer_fn = identity) {
  update_REM_choice_indeg(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    n1 = n1, n2 = n2, is_two_mode = is_two_mode,
    weighted = weighted, transformer_fn = transformer_fn, type = "ego"
  )
}

# outdeg ---------------------------------------------------------------
#' @export
init_DyNAM_rate.outdeg <- function(effectFun, network, window, n1, n2, ...) {
  formals(effectFun) <- c(formals(effectFun), list(type = "ego"))
  init_REM_choice.outdeg(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}


update_DyNAM_rate_outdeg <- function(
    network,
    sender, receiver, replace, cache,
    n1, n2, is_two_mode = FALSE,
    weighted = FALSE, transformer_fn = identity) {
  update_REM_choice_outdeg(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    n1 = n1, n2 = n2, is_two_mode = is_two_mode,
    weighted = weighted, transformer_fn = transformer_fn, type = "ego"
  )
}

# nodeTrans ------------------------------------------------------------------
#' @export
init_DyNAM_rate.node_trans <- function(effectFun, network, window, n1, n2, ...) {
  formals(effectFun) <- c(formals(effectFun), list(type = "ego"))
  init_REM_choice.node_trans(
    effectFun = effectFun, network = network,
    window = window, n1 = n1, n2 = n2, ...
  )
}

update_DyNAM_rate_node_trans <- function(
    network,
    sender,
    receiver,
    replace,
    cache,
    n1, n2,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_REM_choice_node_trans(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    n1 = n1, n2 = n2, is_two_mode = is_two_mode,
    transformer_fn = transformer_fn, type = "ego"
  )
}
# tertius ----------------------------------------------------------------
#' @export
init_DyNAM_rate.tertius <- function(
    effectFun, network, attribute, window, n1, n2, ...) {
  formals(effectFun) <- c(formals(effectFun), list(type = "ego"))
  init_REM_choice.tertius(
    effectFun = effectFun, network = network, attribute = attribute,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

update_DyNAM_rate_tertius <- function(
    network,
    attribute,
    sender = NULL,
    receiver = NULL,
    node = NULL,
    replace,
    cache,
    is_two_mode = FALSE,
    n1 = n1, n2 = n2,
    transformer_fn = identity,
    summarizer_fn = function(x) mean(x, na.rm = TRUE)) {
  update_REM_choice_tertius(
    network = network,
    attribute = attribute,
    sender = sender,
    receiver = receiver,
    node = node,
    replace = replace,
    cache = cache,
    is_two_mode = is_two_mode,
    n1 = n1, n2 = n2,
    transformer_fn = transformer_fn,
    summarizer_fn = summarizer_fn, type = "ego"
  )
}

# Covariate effects -------------------------------------------------------
# ego ---------------------------------------------------------------------
#' @export
init_DyNAM_rate.ego <- function(effectFun, attribute, n1, n2, ...) {
  init_REM_choice.ego(
    effectFun = effectFun, attribute = attribute,
    n1 = n1, n2 = n2, ...
  )
}

update_DyNAM_rate_ego <- function(
    attribute,
    node, replace,
    n1, n2,
    is_two_mode = FALSE) {
  update_REM_choice_ego(
    attribute = attribute,
    node = node, replace = replace,
    n1 = n1, n2 = n2,
    is_two_mode = is_two_mode
  )
}
