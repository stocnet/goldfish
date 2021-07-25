# define methods ----------------------------------------------------------
# init the statistical matrix
init_REM_choice <- function(effectFun, network, attribute, n1, n2, cache = NULL)
  UseMethod("init_REM_choice", effectFun)


# default -----------------------------------------------------------------
init_REM_choice.default <- function(effectFun,
                                    network = NULL, attribute = NULL,
                                    window,
                                    n1, n2) {
  init_DyNAM_choice.default(effectFun = effectFun,
                            network = network, attribute = attribute,
                            window = window,
                            n1 = n1, n2 = n2)
}

# Structural effects ------------------------------------------------------
# tie ---------------------------------------------------------------------
init_REM_choice.tie <- function(effectFun, network, window, n1, n2)
  init_DyNAM_choice.tie(effectFun = effectFun, network = network,
                        window = window, n1 = n1, n2 = n2)

update_REM_choice_tie <- function(network,
                                  sender, receiver, replace,
                                  weighted = FALSE, transformFun = identity)
  update_DyNAM_choice_tie(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    weighted = weighted, transformFun = transformFun
  )


# inertia -----------------------------------------------------------------
init_REM_choice.inertia <- function(effectFun, network, window, n1, n2)
  init_REM_choice.tie(effectFun = effectFun, network = network,
                      window = window, n1 = n1, n2 = n2)


update_REM_choice_inertia <- function(network,
                                      sender, receiver, replace,
                                      weighted = FALSE, transformFun = identity)
  update_REM_choice_tie(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    weighted = weighted, transformFun = transformFun
  )

# recip -------------------------------------------------------------------
init_REM_choice.recip <- function(effectFun, network, window, n1, n2)
  init_DyNAM_choice.recip(effectFun = effectFun, network = network,
                          window = window,
                          n1 = n1, n2 = n2)

update_REM_choice_recip <- function(network,
                                    sender, receiver, replace,
                                    isTwoMode = FALSE,
                                    weighted = FALSE,
                                    transformFun = identity)
  update_DyNAM_choice_recip(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    isTwoMode = isTwoMode,
    weighted = weighted,
    transformFun = transformFun
  )

# indeg -------------------------------------------------------------------
#' init stat matrix indegree using cache
#'
#' @param effectFun function with additional parameters weighted, isTwoMode,
#'   transformFun, type
#' @param network matrix n1*n2
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list with named components: cache numeric vector size n2,
#'   stat matrix numeric n1*n2
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
#' effectFUN <- function(
#'   weighted = TRUE, isTwoMode = TRUE, transformFun = identity)
#'   NULL
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
#'   weighted = TRUE, isTwoMode = FALSE, transformFun = identity,
#'   type = "ego")
#'   NULL
#' init_REM_choice.indeg(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'   weighted = TRUE, isTwoMode = FALSE, transformFun = identity,
#'   type = "alter")
#'   NULL
#' init_REM_choice.indeg(effectFUN, network, NULL, 5, 5)
#' }
init_REM_choice.indeg <- function(effectFun, network, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  weighted <- eval(params[["weighted"]])
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])
  type <- eval(params[["type"]])

  if (isTwoMode && type == "ego") {
    stop("'indeg' effect must not use for type 'ego' (type = 'ego') when is ",
         "a two-mode network (isTwoMode = TRUE) ", call. = FALSE)
  }
  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = numeric(n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
      ))
  }

  # indeg as colsums
  cache <- .colSums(if (weighted) network else network > 0, n1, n2,
                    na.rm = TRUE)

  # applied transformFun instead
  stat <- forceAndCall(1, funApply, cache)

  # return expected n1 * n2 matrix
  # update check that type should be one of "alter" or "ego"
  byRow <- type == "alter"
  stat <- matrix(stat, nrow = n1, ncol = n2, byrow = byRow)
  #
  if (!isTwoMode) diag(stat) <- 0

  return(list(cache = cache, stat = stat))
}

#' update stat indegree using cache
#'
#' @param network matrix n1*n2
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache numeric vector size n2
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#' @param isTwoMode logical
#' @param weighted logical
#' @param transformFun function to apply to the stat
#' @param type character should be 'alter' or 'ego' type
#'
#' @return list:
#'   cache numeric vector size n2,
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
#' cache <- c(2, 7, 0, 1, 0, 7)
#' update_REM_choice_indeg(
#'   network,
#'   1, 2, 3,
#'   cache, 5, 6,
#'   isTwoMode = TRUE, weighted = TRUE, transformFun = sqrt, type = "ego")
#'
#' update_REM_choice_indeg(
#'   network,
#'   1, 2, 3,
#'   cache, 5, 6,
#'   isTwoMode = TRUE, weighted = TRUE, transformFun = sqrt, type = "al")
#' }
update_REM_choice_indeg <- function(network,
                                    sender, receiver, replace, cache,
                                    n1, n2, isTwoMode = FALSE,
                                    weighted = FALSE, transformFun = identity,
                                    type = c("alter", "ego")) {
  type <- match.arg(type)
  # init res: changes NULL if not changes
  res <- list(cache = NULL, changes = NULL)

  # Get old value
  oldValue <- network[sender, receiver]

  # change weighted
  if (!weighted) {
    oldValue <- sign(oldValue)
    replace <- sign(replace)
  }

  # Check if old value has changed
  if (is.na(oldValue) & is.na(replace)) {
    return(res)
  } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
    return(res)
  }

  if (is.na(oldValue)) oldValue <- 0
  if (is.na(replace)) replace <- 0
  # update cache for receiver, event/tie
  cache[receiver] <- cache[receiver] + replace - oldValue

  if (type == "alter") {
    # when the cache had change
    if (isTwoMode) other <- seq_len(n1) else
       other <- setdiff(seq_len(n1), receiver)

    changes <- cbind(
      node1 = other,
      node2 = receiver,
      replace = forceAndCall(1, transformFun, cache[receiver])
    )
  } else if (type == "ego") {
    # when the cache had change
    if (isTwoMode) other <- seq_len(n2) else
       other <- setdiff(seq_len(n2), receiver)

    changes <- cbind(
      node1 = receiver,
      node2 = other,
      replace = forceAndCall(1, transformFun, cache[receiver])
    )
  }
  return(list(cache = cache, changes = changes))
}

# outdeg -------------------------------------------------------------------
#' init stat matrix outdegree using cache
#'
#' @param effectFun function with additional parameters weighted, isTwoMode,
#'   transformFun, type
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window,
#'   if not null and not Inf return empty stat and cache
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list with named components: cache numeric vector size n2,
#'   stat matrix numeric n1*n2
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
#' effectFUN <- function(
#'   weighted = TRUE, isTwoMode = TRUE, transformFun = identity)
#'   NULL
#' init_REM_choice.outdeg(effectFUN, network, NULL, 5, 6)
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
#'   weighted = TRUE, isTwoMode = FALSE, transformFun = identity,
#'   type = "ego")
#'   NULL
#' init_REM_choice.outdeg(effectFUN, network, 1, 5, 5)
#'
#' effectFUN <- function(
#'   weighted = TRUE, isTwoMode = FALSE, transformFun = identity,
#'   type = "alter")
#'   NULL
#' init_REM_choice.outdeg(effectFUN, network, NULL, 5, 5)
#' }
init_REM_choice.outdeg <- function(effectFun, network, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  weighted <- eval(params[["weighted"]])
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])
  type <- eval(params[["type"]])

  if (isTwoMode && type == "alter") {
    stop(
      "'indeg' effect must not use for type 'alter' (type = 'alter') when is ",
      "a two-mode network (isTwoMode = TRUE) ", call. = FALSE)
  }
  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = numeric(n1),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # indeg as colsums
  cache <- .rowSums(if (weighted) network else network > 0, n1, n2,
                    na.rm = TRUE)

  # applied transformFun instead
  stat <- forceAndCall(1, funApply, cache)

  # return expected n1 * n2 matrix
  # update check that type should be one of "alter" or "ego"
  byRow <- type == "alter"
  stat <- matrix(stat, nrow = n1, ncol = n2, byrow = byRow)
  #
  if (!isTwoMode) diag(stat) <- 0

  return(list(cache = cache, stat = stat))
}

#' update stat outdegree using cache
#'
#' @param network matrix n1*n2
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache numeric vector size n2
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#' @param isTwoMode logical
#' @param weighted logical
#' @param transformFun function to apply to the stat
#' @param type character should be 'alter' or 'ego' type
#'
#' @return list:
#'   cache numeric vector size n1,
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
#' cache <- c(2, 7, 0, 1, 0, 7)
#' update_REM_choice_outdeg(
#'   network,
#'   1, 2, 3,
#'   cache, 5, 6,
#'   isTwoMode = TRUE, weighted = TRUE, transformFun = sqrt, type = "ego")
#'
#' update_REM_choice_outdeg(
#'   network,
#'   1, 2, 3,
#'   cache, 5, 6,
#'   isTwoMode = TRUE, weighted = TRUE, transformFun = sqrt, type = "al")
#' }
update_REM_choice_outdeg <- function(network,
                                     sender, receiver, replace, cache,
                                     n1, n2, isTwoMode = FALSE,
                                     weighted = FALSE, transformFun = identity,
                                     type = c("alter", "ego")) {
  type <- match.arg(type)
  # init res: changes NULL if not changes
  res <- list(cache = NULL, changes = NULL)

  # Get old value
  oldValue <- network[sender, receiver]

  # change weighted
  if (!weighted) {
    oldValue <- sign(oldValue)
    replace <- sign(replace)
  }

  # Check if old value has changed
  if (is.na(oldValue) & is.na(replace)) {
    return(res)
  } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
    return(res)
  }

  if (is.na(oldValue)) oldValue <- 0
  if (is.na(replace)) replace <- 0

  # update cache for sender, event/tie
  cache[sender] <- cache[sender] + replace - oldValue

  if (type == "alter") {
    # when the cache had change
    if (isTwoMode) other <- seq_len(n1) else
      other <- setdiff(seq_len(n1), sender)

    changes <- cbind(
      node1 = other,
      node2 = sender,
      replace = forceAndCall(1, transformFun, cache[sender])
    )
  } else if (type == "ego") {
    # when the cache had change
    if (isTwoMode) other <- seq_len(n2) else
      other <- setdiff(seq_len(n2), sender)

    changes <- cbind(
      node1 = sender,
      node2 = other,
      replace = forceAndCall(1, transformFun, cache[sender])
    )
  }
  return(list(cache = cache, changes = changes))
}

# trans -------------------------------------------------------------------
init_REM_choice.trans <- function(effectFun, network, window, n1, n2)
  init_DyNAM_choice.trans(effectFun = effectFun, network = network,
                          window = window,
                          n1 = n1, n2 = n2)

update_REM_choice_trans <- function(network,
                                    sender,
                                    receiver,
                                    replace, cache,
                                    isTwoMode = FALSE,
                                    transformFun = identity)
  update_DyNAM_choice_trans(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    cache = cache,
    isTwoMode = isTwoMode, transformFun = transformFun
  )


# mixedTrans --------------------------------------------------------------
init_REM_choice.mixedTrans <- function(effectFun, network, window, n1, n2)
  init_DyNAM_choice.mixedTrans(effectFun = effectFun, network = network,
                          window = window,
                          n1 = n1, n2 = n2)

update_REM_choice_mixedTrans <- function(network,
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
init_REM_choice.four <- function(effectFun, network, window, n1, n2)
  init_DyNAM_choice.four(effectFun = effectFun, network = network,
                         window = window,
                         n1 = n1, n2 = n2)

update_REM_choice_four <- function(network,
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
#' init stat matrix tertius using cache
#'
#' @param effectFun function with additional parameters transformFun,
#'   aggregateFun
#' @param network matrix n1*n2
#' @param attribute numeric vector n1
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache numeric vector n1
#'   stat matrix numeric n1*n2
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
#' effectFUN <- function(
#'   type = "alter", isTwoMode = TRUE,
#'   transformFun = abs, aggregateFun = function(x) median(x, na.rm = TRUE))
#'   NULL
#' init_REM_choice.tertius(effectFUN, network, attribute)
#' }
init_REM_choice.tertius <- function(
  effectFun, network, attribute, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  aggFun <- eval(params[["aggregateFun"]])
  funApply <- eval(params[["transformFun"]]) # applied FUN instead
  isTwoMode <- eval(params[["isTwoMode"]])
  type <- eval(params[["type"]])

  if (isTwoMode && type == "ego") {
    stop("'tertius' effect must not use for type 'ego' (type = 'ego') when is ",
         "a two-mode network (isTwoMode = TRUE) ", call. = FALSE)
  }
  # if (anyNA(network)) network[is.na(network)] <- 0
  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = numeric(n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # always weighted
  network <- sign(unname(network))
  # compute cache[j]: agg_{k \in N^-(j)}(z_k) || NA if N^-(j) == \empty
  cache <- apply(X = network, MARGIN = 2,
                FUN = function(x) {
                  # # inNeighbor of j
                  inReceiver <- which(x == 1)
                  # # not aggregated if not inNeighbor(j)
                  if (length(inReceiver) == 0) return(NA_real_)
                  # # apply aggFun to inNeighbor(j)
                  forceAndCall(1, aggFun, attribute[inReceiver])
                })
  # applied transformFun
  stat <- forceAndCall(1, funApply, cache)
  # impute missing values by the average
  if (anyNA(stat)) {
    imputeVal <- mean(stat, na.rm = TRUE)
    stat[is.na(stat)] <- imputeVal
  }
  # return expected n1 * n2 matrix
  # update check that type should be one of "alter" or "ego"
  byRow <- type == "alter"
  stat <- matrix(stat, nrow = n1, ncol = n2, byrow = byRow)
  #
  if (!isTwoMode) diag(stat) <- 0

  return(list(cache = cache, stat = stat))
}

#' update stat tertius using cache
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
#' update_REM_choice_tertius(network, attribute,
#'                           sender = 2, receiver = 3,
#'                           node = NULL,
#'                           3,
#'                           cache,
#'                           n1 = 5, n2 = 6,
#'                           transformFun = function(x) x ^ 2,
#'                           aggregateFun = function(x) median(x, na.rm = TRUE))
#'
#' update_REM_choice_tertius(network, attribute,
#'                           sender = NULL, receiver = NULL,
#'                           node = 3,
#'                           3,
#'                           cache,
#'                           n1 = 5, n2 = 6,
#'                           transformFun = function(x) x ^ 2,
#'                           aggregateFun = function(x) median(x, na.rm = TRUE))
#' }
update_REM_choice_tertius <- function(
  network,
  attribute,
  sender = NULL,
  receiver = NULL,
  node = NULL,
  replace,
  cache,
  n1 = n1, n2 = n2,
  isTwoMode = FALSE,
  type = c("alter", "ego"),
  transformFun = identity,
  aggregateFun = function(x) mean(x, na.rm = TRUE)) {
  type <- match.arg(type)
  # utility functions to return third nodes
  third <- function(n, diff = c(node)) {
    setdiff(seq_len(n), diff)
  }
  # init with empty network
  isEmpty <- all(cache == 0)
  isImpute <- anyNA(cache)
  # init res
  res <- list(cache = NULL, changes = NULL)
  # case 1: an update in the network[sende, receiver] <- replace
  if (is.null(node) && !is.null(sender) && !is.null(receiver)) {
    # get old value, always weighted
    replace <- sign(replace)
    oldValue <- sign(network[sender, receiver])

    # Check if old value has changed
    if (is.na(oldValue) & is.na(replace)) {
      return(res)
    } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
      return(res)
    }

    if (is.na(oldValue)) oldValue <- 0
    if (is.na(replace)) replace <- 0

    newValue <- replace - oldValue

    if (newValue == 1) {
      # get all in-neighbors of receiver k->j, consider also sender
      inReceiver <- c(which(network[, receiver] > 0), sender)
    } else {
      # delete the k -> j tie, not consider sender
      inReceiver <- setdiff(which(network[, receiver] > 0), sender)
    }

    # change stat
    valChangeCache <- forceAndCall(
      1, aggregateFun,
      if (length(inReceiver) > 0) attribute[inReceiver] else NA)
    # changes case 1: all nodes needs to be update the att[i] - cache[j] values
    # if (isTwoMode) seq_len(n2) else third(n1, receiver)
    nodesChange <- if (!is.na(valChangeCache)) receiver else numeric()
    isImpute <- ifelse(!isImpute & is.na(valChangeCache), TRUE, isImpute)
    cache[receiver] <- valChangeCache
  }

  # case 2: an update in the attribute[node] <- replace
  if (!is.null(node) && is.null(sender) && is.null(receiver)) {
    # Get old value
    oldValue <- attribute[node]

    # Check if old value has changed
    if (is.na(oldValue) & is.na(replace)) {
      return(res)
    } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
      return(res)
    }

    if (is.na(replace)) replace <- mean(attribute[-node], na.rm = TRUE)

    # get all out-neighbors of node k->j
    outNode <- which(network[node, ] > 0)

    cache[outNode] <-
      vapply(X = outNode,
             FUN =  function(x) {
               # # inNeighbor of outNode, excluding Node because has a new value
               inReceiver <- setdiff(which(network[, x] > 0), node)
               # # apply aggFun to inNeighbor(outNode)
               forceAndCall(1, aggregateFun, c(attribute[inReceiver], replace))
             },
             FUN.VALUE = double(1))

    # changes case 2: is an update value for node,
    #   then its update is done separately
    nodesChange <- outNode
  }
  # changes depending isTwoMode
  if (type == "alter") {
    # when the cache had change
    changes <- Reduce(
      rbind,
      lapply(
        nodesChange,
        function(x)
          cbind(
            node1 = if (isTwoMode) seq_len(n1) else third(n1, x),
            node2 = x,
            replace = forceAndCall(1, transformFun, cache[x])
          )
      )
    )
  } else if (type == "ego") {
    # when the cache had change
    changes <- Reduce(
      rbind,
      lapply(
        nodesChange,
        function(x)
          cbind(
            node1 = x,
            node2 = if (isTwoMode) seq_len(n1) else third(n1, x),
            replace = forceAndCall(1, transformFun, cache[x])
          )
      )
    )
  }
  # when is just initialize it need to change all values to the average
  if (isEmpty || isImpute) {
    toImpute <- which(is.na(cache))
    imputeVal <- mean(cache, na.rm = TRUE)
    # changes depending isTwoMode
    if (type == "alter") {
      #
      changes <- rbind(
        changes,
        Reduce(
          rbind,
          lapply(
            toImpute,
            function(x)
              cbind(
                node1 = if (isTwoMode) seq_len(n1) else third(n1, x),
                node2 = x,
                replace = forceAndCall(1, transformFun, imputeVal)
              )
          )
        )
      )
    } else if (type == "ego") {
      #
      changes <- rbind(
        changes,
        Reduce(
          rbind,
          lapply(
            toImpute,
            function(x)
              cbind(
                node1 = x,
                node2 = if (isTwoMode) seq_len(n1) else third(n1, x),
                replace = forceAndCall(1, transformFun, imputeVal)
              )
          )
        )
      )
    }
  }
  return(list(cache = cache, changes = changes))
}
# tertiusDiff ----------------------------------------------------------------
#' init stat matrix tertius-diff using cache
#'
#' @param effectFun function with additional parameters transformFun,
#'   aggregateFun
#' @param network matrix n1*n2
#' @param attribute numeric vector n1
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache numeric vector n1
#'   stat matrix numeric n1*n2
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
#' effectFUN <- function(transformFun = abs,
#'                       aggregateFun = function(x) median(x, na.rm = TRUE))
#'   NULL
#' init_REM_choice.tertiusDiff(effectFUN, network, attribute)
#' }
init_REM_choice.tertiusDiff <- function(
  effectFun, network, attribute, window, n1, n2)
  init_DyNAM_choice.tertiusDiff(effectFun = effectFun,
                                 network = network, attribute = attribute,
                                 window = window,
                                 n1 = n1, n2 = n2)

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
#' update_REM_choice_tertiusDiff(network, attribute,
#'                           sender = 2, receiver = 3,
#'                           node = NULL,
#'                           3,
#'                           cache,
#'                           n1 = 5, n2 = 6,
#'                           transformFun = function(x) x ^ 2,
#'                           aggregateFun = function(x) median(x, na.rm = TRUE))
#'
#' update_REM_choice_tertiusDiff(network, attribute,
#'                           sender = NULL, receiver = NULL,
#'                           node = 3,
#'                           3,
#'                           cache,
#'                           n1 = 5, n2 = 6,
#'                           transformFun = function(x) x ^ 2,
#'                           aggregateFun = function(x) median(x, na.rm = TRUE))
#' }
update_REM_choice_tertiusDiff <- function(
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
  update_DyNAM_choice_tertiusDiff(
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
    aggregateFun = aggregateFun)


# nodeTrans ------------------------------------------------------------------

#' node trans init
#' number of transitive triangles i->j->k;i->k where node i is embedded. Source node
#' @param effectFun function with additional parameters isTwoMode, transformFun, type
#' @param network matrix n1*n2
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list with named components: cache numeric vector size n2, stat matrix numeric n1*n2
#' @noRd
#'
#' @examples
#' \dontrun{
#'
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
#'   isTwoMode = TRUE, transformFun = identity, type = "ego")
#'   NULL
#' init_REM_choice.nodeTrans(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'   isTwoMode = FALSE, transformFun = identity, type = "ego")
#'   NULL
#' init_REM_choice.nodeTrans(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'   isTwoMode = FALSE, transformFun = identity, type = "alter")
#'   NULL
#' init_REM_choice.nodeTrans(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'   isTwoMode = FALSE, transformFun = identity, type = "alter")
#'   NULL
#' init_REM_choice.nodeTrans(effectFUN, network, 9, 5, 5)
#' }
init_REM_choice.nodeTrans <- function(effectFun, network, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])
  type <- eval(params[["type"]])

  if (isTwoMode) {
    stop("'nodeTrans' effect must not use ",
         "when is a two-mode network (isTwoMode = TRUE)", call. = FALSE)
  }

  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = numeric(n1),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # always weighted
  network <- sign(network)

  # compute stat: number of triangles i->j->k, i->k from i perspective
  # stat <- diag(tcrossprod(network %*% network, network))
  cache <- .rowSums((network %*% network) * network, m = n1, n = n2,
                    na.rm = TRUE)

  stat <- matrix(forceAndCall(1, funApply, cache),
                 nrow = n1, ncol = n2, byrow = (type == "alter")
  )
  # if (!isTwoMode)
  diag(stat) <- 0

  return(list(cache = cache, stat = stat))
}

#' update node trans
#' number of transitive triangles i->j->k;i->k where node i is embedded as
#'  a source node
#' @param network matrix n1*n2
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache numeric vector size n1
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#' @param isTwoMode logical
#' @param transformFun function to apply to the stat
#' @param type character should be 'alter' or 'ego' type
#'
#' @return list:
#'   cache numeric vector size n2,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#'
#' @examples
#' \dontrun{
#' cache <- c(0, 0, 1, 1, 0)
#' update_REM_choice_nodeTrans(network, 1, 5, 1, cache, 5, 5, type = "alter")
#' update_REM_choice_nodeTrans(network, 1, 5, 1, cache, 5, 5, type = "ego")
#' update_REM_choice_nodeTrans(network, 3, 2, 0, cache, 5, 5, type = "ego")
#' }
update_REM_choice_nodeTrans <- function(network,
                                         sender,
                                         receiver,
                                         replace,
                                         cache,
                                         n1, n2,
                                         isTwoMode = FALSE,
                                         transformFun = identity,
                                         type = c("alter", "ego")) {
  type <- match.arg(type)
  res <- list(cache = NULL, changes = NULL)

  # only relevant for one-mode networks
  if (sender == receiver) {
    return(res)
  }

  # get old value, always weighted
  replace <- sign(replace)
  oldValue <- sign(network[sender, receiver])

  # Check if old value has changed
  if (is.na(oldValue) & is.na(replace)) {
    return(res)
  } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
    return(res)
  }

  if (is.na(oldValue)) oldValue <- 0
  if (is.na(replace)) replace <- 0

  third <- function(n, diff) {
    setdiff(seq_len(n), diff)
  }

  # compute neighbourhoods for sender and receivers, (i = sender, j = receiver)
  outNeighSender <- which(network[sender, ] > 0) # N^+(i)
  inNeighReceiver <- which(network[, receiver] > 0) # N^-(j)

  # commonReceivers = k: i->j & i->k & j->k => trans(i) += 1
  commonReceivers <- intersect(
    outNeighSender,
    which(network[receiver, ] > 0)
  ) # N^+(i) \cap N^+(j)
  # commonSenders = l: l->i & l->j & i->j => trans(l) += 1
  commonSenders <- intersect(
    which(network[, sender] > 0),
    inNeighReceiver
  ) # N^-(i) \cap N^-(j)
  # brokers = b: i->b & b->j & i->j => trans(i) += 1
  brokers <- intersect(outNeighSender, inNeighReceiver) # N^+(i) \cap N^-(j)

  senderChanges <- length(commonReceivers) + length(brokers) # trans(i) += 1

  changes <- NULL
  if (senderChanges > 0) {
    replaceValues <- (replace - oldValue) * senderChanges + cache[sender]
    if (type == "ego") {
      changes <- cbind(
        node1 = sender, node2 = third(n1, sender), replace = replaceValues)
    } else {
      changes <- cbind(
        node1 = third(n1, sender), node2 = sender, replace = replaceValues)
    }
    cache[sender] <- replaceValues
  }

  if (length(commonSenders) > 0) {
    replaceValues <- (replace - oldValue) + cache[commonSenders]
    changes <- rbind(
      changes,
      Reduce(
        rbind,
        lapply(
          seq_along(commonSenders),
          function(x)
            if (type == "ego") {
              cbind(
                node1 = commonSenders[x],
                node2 = third(n1, commonSenders[x]),
                replace = replaceValues[x]
              )
            } else {
              cbind(
                node1 = third(n1, commonSenders[x]),
                node2 = commonSenders[x],
                replace = replaceValues[x]
              )
            }
        )
      )
    )
    cache[commonSenders] <- replaceValues
  }

  if (!is.null(changes)) {
    changes[, "replace"] <- forceAndCall(1, transformFun, changes[, "replace"])
    # res$changes <- changes
  }
  return(list(cache = cache, changes = changes))
}

# Covariate effects -------------------------------------------------------
# ego ---------------------------------------------------------------------
init_REM_choice.ego <- function(effectFun, attribute, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])

  # compute stat
  stats <- matrix(attribute, nrow = n1, ncol = n2, byrow = FALSE)
  if (!isTwoMode) diag(stats) <- 0

  return(list(stat = stats))
}

update_REM_choice_ego <- function(attribute,
                                  node, replace,
                                  n1, n2,
                                  isTwoMode = FALSE) {
  res <- list(changes = NULL)
  # Get old value
  oldValue <- attribute[node]

  # Check if old value has changed
  if (is.na(oldValue) & is.na(replace)) {
    return(res)
  } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
    return(res)
  }

  if (is.na(replace)) replace <- mean(attribute[-node], na.rm = TRUE)
  # utility functions to return third nodes
  third <- function(n, diff = c(node)) {
    setdiff(seq_len(n), diff)
  }

  if (!isTwoMode) nodesChange <- third(n1, node) else nodesChange <- seq_len(n2)

  # change stat
  res$changes <- cbind(node1 = node, node2 = nodesChange, replace = replace)
  return(res)
}

# alter -------------------------------------------------------------------
init_REM_choice.alter <- function(effectFun, attribute, n1, n2)
  init_DyNAM_choice.alter(
    effectFun = effectFun, attribute = attribute, n1 = n2, n2 = n2)

update_REM_choice_alter <- function(
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
init_REM_choice.same <- function(effectFun, attribute)
  init_DyNAM_choice.same(effectFun = effectFun, attribute = attribute)

update_REM_choice_same <- function(attribute,
                                   node, replace,
                                   isTwoMode = FALSE)
  update_DyNAM_choice_same(
    attribute = attribute,
    node = node, replace = replace,
    isTwoMode = isTwoMode
  )

# diff --------------------------------------------------------------------
init_REM_choice.diff <- function(effectFun, attribute)
  init_DyNAM_choice.diff(effectFun = effectFun, attribute = attribute)

update_REM_choice_diff <- function(attribute, node, replace,
                                   n1, n2,
                                   isTwoMode = FALSE,
                                   transformFun = abs)
  update_DyNAM_choice_diff(
    attribute = attribute,
    node = node, replace = replace,
    isTwoMode = isTwoMode,
    n1 = n1, n2 = n2,
    transformFun = transformFun
  )

# sim ---------------------------------------------------------------------
init_REM_choice.sim <- function(effectFun, attribute)
  init_DyNAM_choice.sim(effectFun = effectFun, attribute = attribute)

update_REM_choice_sim <- function(attribute, node, replace,
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
