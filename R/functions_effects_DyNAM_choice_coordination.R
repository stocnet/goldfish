# define methods ----------------------------------------------------------
# init the statistical matrix
init_DyNAM_choice_coordination <- function(effectFun, ...) {
  UseMethod("init_DyNAM_choice", effectFun)
}

# Structural effects ------------------------------------------------------
# tie ---------------------------------------------------------------------

update_DyNAM_choice_coordination_tie <- function(
    network,
    sender, receiver, replace,
    weighted = FALSE, transformer_fn = identity) {
  update_DyNAM_choice_tie(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    weighted = weighted, transformer_fn = transformer_fn
  )
}

# inertia -----------------------------------------------------------------
update_DyNAM_choice_coordination_inertia <- function(
    network,
    sender, receiver, replace,
    weighted = FALSE, transformer_fn = identity) {
  update_DyNAM_choice_coordination_tie(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    weighted = weighted, transformer_fn = transformer_fn
  )
}

# indeg -------------------------------------------------------------------
update_DyNAM_choice_coordination_indeg <- function(
    network,
    sender, receiver, replace, cache,
    n1, n2, is_two_mode = FALSE,
    weighted = FALSE, transformer_fn = identity) {
  update_DyNAM_choice_indeg(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    n1 = n1, n2 = n2, is_two_mode = is_two_mode,
    weighted = weighted, transformer_fn = transformer_fn
  )
}

# outdeg -------------------------------------------------------------------
# update_DyNAM_choice_coordination_outdeg <- function(
#   network,
#   sender, receiver, replace,
#   cache, n1, n2,
#   is_two_mode = FALSE,
#   weighted = FALSE, transformer_fn = identity)
#   update_DyNAM_choice_outdeg(
#     network = network,
#     sender = sender, receiver = receiver, replace = replace, cache = cache,
#     n1 = n1, n2 = n2, is_two_mode = is_two_mode,
#     weighted = weighted, transformer_fn = transformer_fn
#   )

# trans -------------------------------------------------------------------
update_DyNAM_choice_coordination_trans <- function(
    network,
    sender,
    receiver,
    replace, cache,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice_trans(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    is_two_mode = is_two_mode, transformer_fn = transformer_fn
  )
}

# mixedTrans --------------------------------------------------------------
update_DyNAM_choice_coordination_mixedTrans <- function(
    network,
    sender,
    receiver,
    replace, netUpdate, cache,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice_mixedTrans(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    netUpdate = netUpdate, cache = cache,
    is_two_mode = is_two_mode, transformer_fn = transformer_fn
  )
}

# four --------------------------------------------------------------------
update_DyNAM_choice_coordination_four <- function(
    network,
    sender, receiver, replace,
    cache,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice_four(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    cache = cache,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}

# tertius ----------------------------------------------------------------
update_DyNAM_choice_coordination_tertius <- function(
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
  update_DyNAM_choice_tertius(
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
    summarizer_fn = summarizer_fn
  )
}

# tertiusDiff ----------------------------------------------------------------
#' update stat transitivity using cache
#'
#' @param network matrix n1*n1
#' @param attribute numeric vector n1
#' @param sender integer||NULL if node is not NULL
#' @param receiver integer||NULL if node is not NULL
#' @param node integer||NULL if sender and receiver are not NULL
#' @param replace numeric
#' @param cache numeric vector n1
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#' @param transformer_fn function to apply to the difference
#' @param summarizer_fn function usa to aggregate in-neighbors attributes
#'
#' @return list:
#'   cache numeric vector size n1
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
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
#' attribute <- c(1, 0, 1, 3, 1)
#' cache <- c(2, 1, 0, 1, 0, 2)
#'
#' update_DyNAM_choice_coordination_tertiusDiff(
#'   network, attribute,
#'   sender = 2, receiver = 3,
#'   node = NULL,
#'   3,
#'   cache,
#'   n1 = 5, n2 = 6,
#'   transformer_fn = function(x) x^2,
#'   summarizer_fn = function(x) median(x, na.rm = TRUE)
#' )
#'
#' update_DyNAM_choice_coordination_tertiusDiff(
#'   network, attribute,
#'   sender = NULL, receiver = NULL,
#'   node = 3,
#'   3,
#'   cache,
#'   n1 = 5, n2 = 6,
#'   transformer_fn = function(x) x^2,
#'   summarizer_fn = function(x) median(x, na.rm = TRUE)
#' )
#' }
update_DyNAM_choice_coordination_tertiusDiff <- function(
    network,
    attribute,
    sender = NULL,
    receiver = NULL,
    node = NULL,
    replace,
    cache,
    is_two_mode = FALSE,
    n1 = n1, n2 = n2,
    transformer_fn = abs,
    summarizer_fn = function(x) mean(x, na.rm = TRUE)) {
  update_DyNAM_choice_tertiusDiff(
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
    summarizer_fn = summarizer_fn
  )
}

# nodeTrans ------------------------------------------------------------------
update_DyNAM_choice_coordination_nodeTrans <- function(
    network,
    sender,
    receiver,
    replace,
    cache,
    n1, n2,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice_nodeTrans(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    n1 = n1, n2 = n2, is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}

# Covariate effects -------------------------------------------------------
# alter -------------------------------------------------------------------
update_DyNAM_choice_coordination_alter <- function(
    attribute,
    node, replace,
    n1, n2,
    is_two_mode = FALSE) {
  update_DyNAM_choice_alter(
    attribute = attribute,
    node = node, replace = replace,
    n1 = n1, n2 = n2,
    is_two_mode = is_two_mode
  )
}

# same --------------------------------------------------------------------
update_DyNAM_choice_coordination_same <- function(
    attribute,
    node, replace,
    is_two_mode = FALSE) {
  update_DyNAM_choice_same(
    attribute = attribute,
    node = node, replace = replace,
    is_two_mode = is_two_mode
  )
}

# diff --------------------------------------------------------------------
update_DyNAM_choice_coordination_diff <- function(
    attribute, node, replace,
    n1, n2,
    is_two_mode = FALSE,
    transformer_fn = abs) {
  update_DyNAM_choice_diff(
    attribute = attribute,
    node = node, replace = replace,
    n1 = n1, n2 = n2,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}

# sim ---------------------------------------------------------------------
update_DyNAM_choice_coordination_sim <- function(
    attribute, node, replace,
    n1, n2,
    is_two_mode = FALSE,
    transformer_fn = abs) {
  update_DyNAM_choice_sim(
    attribute = attribute,
    node = node, replace = replace,
    n1 = n1, n2 = n2,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}

# ego alter interaction ---------------------------------------------------
update_DyNAM_choice_coordination_egoAlterInt <- function(
    attribute, node, replace,
    attUpdate,
    n1, n2,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice_egoAlterInt(
    attribute = attribute,
    node = node, replace = replace,
    attUpdate = attUpdate,
    n1 = n1, n2 = n2,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}
