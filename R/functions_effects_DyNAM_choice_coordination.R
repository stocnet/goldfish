# define methods ----------------------------------------------------------
# init the statistical matrix
init_DyNAM_choice_coordination <- function(
  effectFun, network, attribute, n1, n2, cache = NULL)
  UseMethod("init_DyNAM_choice", effectFun)

# Structural effects ------------------------------------------------------
# tie ---------------------------------------------------------------------

update_DyNAM_choice_coordination_tie <- function(
  network,
  sender, receiver, replace,
  weighted = FALSE, transformFun = identity)
  update_DyNAM_choice_tie(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    weighted = weighted, transformFun = transformFun
  )


# inertia -----------------------------------------------------------------
update_DyNAM_choice_coordination_inertia <- function(
  network,
  sender, receiver, replace,
  weighted = FALSE, transformFun = identity)
  update_DyNAM_choice_coordination_tie(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    weighted = weighted, transformFun = transformFun
  )


# indeg -------------------------------------------------------------------
update_DyNAM_choice_coordination_indeg <- function(
  network,
  sender, receiver, replace, cache,
  n1, n2, isTwoMode = FALSE,
  weighted = FALSE, transformFun = identity)
  update_DyNAM_choice_indeg(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    n1 = n1, n2 = n2, isTwoMode = isTwoMode,
    weighted = weighted, transformFun = transformFun
  )

# outdeg -------------------------------------------------------------------
# update_DyNAM_choice_coordination_outdeg <- function(
#   network,
#   sender, receiver, replace,
#   cache, n1, n2,
#   isTwoMode = FALSE,
#   weighted = FALSE, transformFun = identity)
#   update_DyNAM_choice_outdeg(
#     network = network,
#     sender = sender, receiver = receiver, replace = replace, cache = cache,
#     n1 = n1, n2 = n2, isTwoMode = isTwoMode,
#     weighted = weighted, transformFun = transformFun
#   )

# trans -------------------------------------------------------------------
update_DyNAM_choice_coordination_trans <- function(
  network,
  sender,
  receiver,
  replace, cache,
  isTwoMode = FALSE,
  transformFun = identity)
  update_DyNAM_choice_trans(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    isTwoMode = isTwoMode, transformFun = transformFun
  )


# mixedTrans --------------------------------------------------------------
update_DyNAM_choice_coordination_mixedTrans <- function(
  network,
  sender,
  receiver,
  replace, netUpdate, cache,
  isTwoMode = FALSE,
  transformFun = identity)
  update_DyNAM_choice_mixedTrans(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    netUpdate = netUpdate, cache = cache,
    isTwoMode = isTwoMode, transformFun = transformFun
  )

# four --------------------------------------------------------------------
update_DyNAM_choice_coordination_four <- function(
  network,
  sender, receiver, replace,
  cache,
  isTwoMode = FALSE,
  transformFun = identity)
  update_DyNAM_choice_four(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    cache = cache,
    isTwoMode = isTwoMode,
    transformFun = transformFun
  )

# tertius ----------------------------------------------------------------
update_DyNAM_choice_coordination_tertius <- function(
  network,
  attribute,
  sender = NULL,
  receiver = NULL,
  node = NULL,
  replace,
  cache,
  isTwoMode = FALSE,
  n1 = n1, n2 = n2,
  transformFun = identity,
  aggregateFun = function(x) mean(x, na.rm = TRUE))
update_DyNAM_choice_tertius(
  network = network,
  attribute = attribute,
  sender = sender,
  receiver = receiver,
  node = node,
  replace = replace,
  cache = cache,
  isTwoMode = isTwoMode,
  n1 = n1, n2 = n2,
  transformFun = transformFun,
  aggregateFun = aggregateFun
)

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
#' @param transformFun function to apply to the difference
#' @param aggregateFun function usa to aggregate in-neighbors attributes
#'
#' @return list:
#'   cache numeric vector size n1
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#'
#' @examples
#' \dontrun{
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
#'   transformFun = function(x) x ^ 2,
#'   aggregateFun = function(x) median(x, na.rm = TRUE))
#'
#' update_DyNAM_choice_coordination_tertiusDiff(
#'   network, attribute,
#'   sender = NULL, receiver = NULL,
#'   node = 3,
#'   3,
#'   cache,
#'   n1 = 5, n2 = 6,
#'   transformFun = function(x) x ^ 2,
#'   aggregateFun = function(x) median(x, na.rm = TRUE))
#' }
update_DyNAM_choice_coordination_tertiusDiff <- function(
  network,
  attribute,
  sender = NULL,
  receiver = NULL,
  node = NULL,
  replace,
  cache,
  isTwoMode = FALSE,
  n1 = n1, n2 = n2,
  transformFun = abs,
  aggregateFun = function(x) mean(x, na.rm = TRUE))
update_DyNAM_choice_tertiusDiff(network = network,
                                 attribute = attribute,
                                 sender = sender,
                                 receiver = receiver,
                                 node = node,
                                 replace = replace,
                                 cache = cache,
                                 isTwoMode = isTwoMode,
                                 n1 = n1, n2 = n2,
                                 transformFun = transformFun,
                                 aggregateFun = aggregateFun)

# nodeTrans ------------------------------------------------------------------
update_DyNAM_choice_coordination_nodeTrans <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  n1, n2,
  isTwoMode = FALSE,
  transformFun = identity)
  update_DyNAM_choice_nodeTrans(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    n1 = n1, n2 = n2, isTwoMode = isTwoMode,
    transformFun = transformFun)


# Covariate effects -------------------------------------------------------
# alter -------------------------------------------------------------------
update_DyNAM_choice_coordination_alter <- function(
  attribute,
  node, replace,
  n1, n2,
  isTwoMode = FALSE)
  update_DyNAM_choice_alter(
    attribute = attribute,
    node = node, replace = replace,
    n1 = n1, n2 = n2,
    isTwoMode = isTwoMode
  )


# same --------------------------------------------------------------------
update_DyNAM_choice_coordination_same <- function(
  attribute,
  node, replace,
  isTwoMode = FALSE)
  update_DyNAM_choice_same(
    attribute = attribute,
    node = node, replace = replace,
    isTwoMode = isTwoMode
  )


# diff --------------------------------------------------------------------
update_DyNAM_choice_coordination_diff <- function(
  attribute, node, replace,
  n1, n2,
  isTwoMode = FALSE,
  transformFun = abs)
  update_DyNAM_choice_diff(
    attribute = attribute,
    node = node, replace = replace,
    n1 = n1, n2 = n2,
    isTwoMode = isTwoMode,
    transformFun = transformFun
  )



# sim ---------------------------------------------------------------------
update_DyNAM_choice_coordination_sim <- function(
  attribute, node, replace,
  n1, n2,
  isTwoMode = FALSE,
  transformFun = abs)
  update_DyNAM_choice_sim(
    attribute = attribute,
    node = node, replace = replace,
    n1 = n1, n2 = n2,
    isTwoMode = isTwoMode,
    transformFun = transformFun
  )
