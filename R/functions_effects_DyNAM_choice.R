# define methods ----------------------------------------------------------
# init the statistical matrix: list(cache = NULL||list, stat = matrix)
init_DyNAM_choice <- function(effectFun, network, attribute, n1, n2, cache = NULL)
  UseMethod("init_DyNAM_choice")

# default -----------------------------------------------------------------
init_DyNAM_choice.default <- function(effectFun,
                                      network = NULL, attribute = NULL,
                                      window,
                                      n1, n2) {

  # print(match.call())
  if (is.null(network) && is.null(attribute)) {
    # this check could be unnecessary
    stop("the effect function doesn't specify neither a network nor an attribute as argument")
  }

  # if multiple networks, attributes or combination of both are specified.
  # The initialization is done over the fist network
  # lenNetwork <- length(network)
  hasNetwork  <- length(network)   >= 1
  hasMultNets <- length(network)   >= 1 & is.list(network)
  hasMultAtt  <- length(attribute) >= 1 & is.list(attribute)

  .argsNames <- names(formals(effectFun))
  # if network inputs, just the first network is empty.
  stats <- matrix(0, nrow = n1, ncol = n2) # check for poss

  # init a generic cache object
  if ("cache" %in% .argsNames) {
    cache <- stats
  } else cache <- NULL

  if (hasNetwork) {
    # check if not empty network to initialize the statistical matrix
    # create a copy of the network to iterate over
    if (hasMultNets) {
      areEmpty <- vapply(network, function(x) all(x[!is.na(x)] == 0), logical(1))
      if ((!is.null(window) && !is.infinite(window)) || any(areEmpty)) {
        if (is.null(cache)) return(list(stat =  stats))
        return(list(cache = cache, stat =  stats))
      }
      netIter <- network[[1]]
    } else {
      if ((!is.null(window) && !is.infinite(window)) || all(network[!is.na(network)] == 0)) {
        if (is.null(cache)) return(list(stat =  stats))
        return(list(cache = cache, stat =  stats))
      }
      netIter <- network
    }

    emptyObject <- array(0, dim = dim(netIter))
  } else {
    if (hasMultAtt) {
      areEmpty <- vapply(attribute, function(x) all(x[!is.na(x)] == 0), logical(1))
      if (any(areEmpty)) {
        if (is.null(cache)) return(list(stat =  stats))
        return(list(cache = cache, stat =  stats))
      }
      attIter <- attribute[[1]]
    } else {
      if (all(attribute[!is.na(attribute)] == 0)) {
        if (is.null(cache)) return(list(stat =  stats))
        return(list(cache = cache, stat =  stats))
      }
      attIter <- attribute
    }

    emptyObject <- vector(mode = "numeric", length = length(attIter))
  }
  # iterate over not empty entries and compute updates
  if (hasNetwork) {
    # it has define network(s) as argument(s)
    # not empty rows
    rowsIter <- which(rowSums(netIter != 0, na.rm = TRUE) > 0)
    for (i in rowsIter) {
      colsIter <- which(!is.na(netIter[i, ]) & netIter[i, ] != 0)
      for (j in colsIter) {
        # feed empty object to the effect function
        if (hasMultNets) {
          netArg <- network
          netArg[[1]] <- emptyObject
        } else {
          netArg <- emptyObject
        }
        # set arguments values and only keep the ones in formals(effectFun)
        .argsFUN <- list(
          network = netArg,
          attribute = attribute,
          sender = i,
          receiver = j,
          replace = netIter[i, j],
          n1 = if ("n1" %in% .argsNames) n1 else NULL,
          n2 = if ("n2" %in% .argsNames) n2 else NULL,
          cache = cache
        )
        .argsKeep <- pmatch(.argsNames, names(.argsFUN))
        # construct network objects step by step from empty objects
        res <- do.call(effectFun, .argsFUN[na.omit(.argsKeep)])
        if (!is.null(res$changes) && nrow(res$changes) > 0) {
          stats[cbind(res$changes[, 1], res$changes[, 2])] <- res$changes[, 3]
        }
        # update cache if any
        if (!is.null(cache) && !is.null(res$cache)) {
          cache <- res$cache
        }
        # update networks
        emptyObject[i, j] <- netIter[i, j]
      }
    }
  } else {
    # just attribute(s)
    nodesIter <- which(!is.na(attIter) & attIter != 0)
    for (i in nodesIter) {
      # feed empty object to the effect function
      if (hasMultAtt) {
        attArg <- attribute
        attArg[[1]] <- emptyObject
      } else {
        attArg <- emptyObject
      }
      # set arguments values and only keep the ones in formals(effectFun)
      .argsFUN <- list(
        attribute = attArg,
        node = i,
        replace = attIter[i],
        n1 = if ("n1" %in% .argsNames) n1 else NULL,
        n2 = if ("n2" %in% .argsNames) n2 else NULL,
        cache = cache
      )
      .argsKeep <- pmatch(.argsNames, names(.argsFUN))
      # construct network objects step by step from empty objects
      res <- do.call(effectFun, .argsFUN[na.omit(.argsKeep)])
      if (!is.null(res$changes) && nrow(res$changes) > 0) {
        stats[cbind(res$changes[, 1], res$changes[, 2])] <- res$changes[, 3]
      }
      # update cache if any
      if (!is.null(cache) && !is.null(res$cache)) {
        cache <- res$cache
      }
      # update networks
      emptyObject[i] <- attIter[i]
    }
  }
  if (is.null(cache)) return(list(stat = stats))
  return(list(cache = cache, stat = stats))
}

# Structural effects ------------------------------------------------------
# tie ---------------------------------------------------------------------
#' init stat matrix tie
#'
#' @param effectFun function with additional parameters weighted, transformFun
#' @param network matrix n1*n2
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list: stat matrix numeric n1*n2
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
#' effectFUN <- function(weighted = TRUE, transformFun = identity)
#'   NULL
#' init_DyNAM_choice.tie(effectFUN, network)
#' }
init_DyNAM_choice.tie <- function(effectFun, network, window, n1, n2) {
  # get arguments
  params <- formals(effectFun)
  weighted <- eval(params[["weighted"]])
  funApply <- eval(params[["transformFun"]])

  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    value <- if (weighted) forceAndCall(1, funApply, 0) else 0
    return(list(stat = matrix(value, nrow = n1, ncol = n2)))
  }

  if (weighted) {
    stat <- forceAndCall(1, funApply, network)
  } else {
    # if (params[["transformFun"]] != "identity")
    #   warning("Effect tie/inertia with 'weighted = FALSE'",
    #           "doesn't apply 'trasnformFun =", params[["transformFun"]], "'\n",
    #           call. = FALSE)
    stat <- 1 * (network > 0)
  }
  return(list(stat = unname(stat)))
}

#' update stat indegree using cache
#'
#' @param network matrix n1*n2
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param weighted logical
#' @param transformFun function to apply to the stat
#'
#' @return list:
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases tie
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
#' update_DyNAM_choice_tie(network,
#'                         1, 2, 3,
#'                         weighted = TRUE, transformFun = sqrt)
#' }
update_DyNAM_choice_tie <- function(network,
                                    sender, receiver, replace,
                                    weighted = FALSE, transformFun = identity) {

  # No change check, irrelevant for two-mode network
  # if(sender == receiver) return(NULL)

  # init res
  res <- list(changes = NULL)

  # Get old value
  oldValue <- network[sender, receiver]

  # change for weighted effect
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

  # change stat
  res$changes <- cbind(
    node1 = sender,
    node2 = receiver,
    replace = if (!weighted)  1 * (replace > 0) else forceAndCall(1, transformFun, replace)
  )

  return(res)
}

# inertia -----------------------------------------------------------------
init_DyNAM_choice.inertia <- function(effectFun, network, window, n1, n2)
  init_DyNAM_choice.tie(effectFun = effectFun, network = network, window = window, n1 = n1, n2 = n2)

#' @aliases inertia
update_DyNAM_choice_inertia <- function(network,
                                        sender, receiver, replace,
                                        weighted = FALSE, transformFun = identity)
  update_DyNAM_choice_tie(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    weighted = weighted, transformFun = transformFun
  )

# indeg -------------------------------------------------------------------
#' init stat matrix indegree using cache alter
#'
#' @param effectFun function with additional parameters weighted, isTwoMode, transformFun
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
#' effectFUN <- function(weighted = TRUE, isTwoMode = FALSE, transformFun = identity)
#'   NULL
#' init_DyNAM_choice.indeg(effectFUN, network, NULL, 5, 6)
#' }
init_DyNAM_choice.indeg <- function(effectFun, network, window, n1, n2) {
  formals(effectFun) <- c(formals(effectFun), list(type = "alter"))
  init_REM_choice.indeg(effectFun = effectFun, network = network,
                        window = window,
                        n1 = n1, n2 = n2)
}


#' update stat indegree using cache alter
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
#' update_DyNAM_choice_indeg(network,
#'                           1, 2, 3,
#'                           cache, 5, 6,
#'                           isTwoMode = TRUE, weighted = TRUE, transformFun = sqrt)
#'
#' }
update_DyNAM_choice_indeg <- function(network,
                                      sender, receiver, replace,
                                      cache, n1, n2,
                                      isTwoMode = FALSE,
                                      weighted = FALSE, transformFun = identity)
  update_REM_choice_indeg(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    n1 = n1, n2 = n2, isTwoMode = isTwoMode,
    weighted = weighted, transformFun = transformFun, type = "alter"
  )

# outdeg -------------------------------------------------------------------
#' init stat matrix outdegree using cache alter
#'
#' @param effectFun function with additional parameters weighted, isTwoMode, transformFun
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window, if not null and not Inf return empty stat and cache
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list with named components: cache numeric vector size n1, stat matrix numeric n1*n2
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
#' effectFUN <- function(weighted = TRUE, isTwoMode = FALSE, transformFun = identity)
#'   NULL
#' init_DyNAM_choice.outdeg(effectFUN, network, NULL, 5, 6)
#' init_DyNAM_choice.outdeg(effectFUN, network, 1, 5, 6)
#' }
init_DyNAM_choice.outdeg <- function(effectFun, network, window, n1, n2) {
  formals(effectFun) <- c(formals(effectFun), list(type = "alter"))
  init_REM_choice.outdeg(effectFun = effectFun, network = network,
                        window = window, n1 = n1, n2 = n2)
}


#' update stat outdegree using cache alter
#'
#' @param network matrix n1*n2
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache numeric vector size n1
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#' @param isTwoMode logical
#' @param weighted logical
#' @param transformFun function to apply to the stat
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
#' update_DyNAM_choice_indeg(network,
#'                           1, 2, 3,
#'                           cache, 5, 6,
#'                           isTwoMode = TRUE, weighted = TRUE, transformFun = sqrt)
#'
#' }
update_DyNAM_choice_outdeg <- function(network,
                                       sender, receiver, replace,
                                       cache, n1, n2,
                                       isTwoMode = FALSE,
                                       weighted = FALSE, transformFun = identity)
  update_REM_choice_outdeg(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    n1 = n1, n2 = n2, isTwoMode = isTwoMode,
    weighted = weighted, transformFun = transformFun, type = "alter"
  )


# recip -------------------------------------------------------------------
#' init stat matrix reciprocity
#'
#' @param effectFun function with additional parameters weighted, isTwoMode, transformFun
#' @param network matrix n1*n2
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list: stat matrix numeric n1*n2
#' @noRd
#'
#' @examples
#' \dontrun{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' effectFUN <- function(weighted = FALSE, isTwoMode = FALSE, transformFun = sqrt)
#'   NULL
#'
#' init_DyNAM_choice.recip(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.recip <- function(effectFun, network, window, n1, n2) {
  params <- formals(effectFun)
  weighted <- eval(params[["weighted"]])
  funApply <- eval(params[["transformFun"]])
  isTwoMode <- eval(params[["isTwoMode"]])

  if (isTwoMode) {
    stop("'recip' effect must not use when is a two-mode network (isTwoMode = TRUE)", call. = FALSE)
  }

  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    value <- if (weighted) forceAndCall(1, funApply, 0) else 0
    return(list(stat = matrix(value, nrow = n1, ncol = n2)))
  }

  if (weighted) {
    stats <- forceAndCall(1, funApply, t(network))
  } else {
    # network <- sign(network)
    stats <- t(network > 0) * 1
  }

  # if (!isTwoMode) diag(stats) <- 0 # # I think is not needed!!!
  return(list(stat = unname(stats)))
}

#' update stat reciprocity
#'
#' @param network matrix n1*n2
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param weighted logical
#' @param isTwoMode logical
#' @param transformFun function to apply to the stat
#'
#' @return list:
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases recip
#'
#' @examples
#' \dontrun{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' update_DyNAM_choice_recip(network,
#'                           1, 2, 9,
#'                           weighted = TRUE, isTwoMode = FALSE, transformFun = sqrt)
#' }
update_DyNAM_choice_recip <- function(network,
                                      sender, receiver, replace,
                                      weighted = FALSE,
                                      isTwoMode = FALSE,
                                      transformFun = identity) {

  # init res
  res <- list(changes = NULL)

  # No change check, only relevant for one-mode networks
  if (sender == receiver) {
    return(res)
  }

  # Get old value
  oldValue <- network[sender, receiver]

  # change for weighted effect
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

  # change stat
  res$changes <- cbind(
    node1 = receiver,
    node2 = sender,
    replace = if (!weighted) 1 * (replace > 0) else forceAndCall(1, transformFun, replace)
  )

  return(res)
}

# node_trans ------------------------------------------------------------------
init_DyNAM_choice.nodeTrans <- function(effectFun, network, window, n1, n2) {
  formals(effectFun) <- c(formals(effectFun), list(type = "alter"))
  init_REM_choice.nodeTrans(
    effectFun = effectFun, network = network,
    window = window, n1 = n1, n2 = n2)
}

update_DyNAM_choice_nodeTrans <- function(network,
                                           sender,
                                           receiver,
                                           replace,
                                           cache,
                                           n1, n2,
                                           isTwoMode = FALSE,
                                           transformFun = identity)
  update_REM_choice_nodeTrans(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    n1 = n1, n2 = n2, isTwoMode = isTwoMode,
    transformFun = transformFun, type = "alter")

# Closure effects --------------------------------------------------------------
# trans -------------------------------------------------------------------
#' init stat matrix transitivity using cache: Closure of two-paths (i->k->j)
#'
#' @param effectFun function with additional parameters transformFun, isTwoMode
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#'
#' @examples
#' \dontrun{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' effectFUN <- function(isTwoMode = FALSE, transformFun = sqrt)
#'   NULL
#' init_DyNAM_choice.trans(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.trans <- function(effectFun, network, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])

  if (isTwoMode) {
    stop("'trans' effect must not use when is a two-mode network (isTwoMode = TRUE)", call. = FALSE)
  }

  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
      ))
  }
  # always weighted
  network <- sign(network)
  # compute stat
  cache <- unname(network %*% network)
  # diag(cache) <- 0

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat transitivity using cache
#'
#' @param network matrix n1*n1
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache stat matrix numeric n1 * n1
#' @param isTwoMode logical
#' @param transformFun function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases trans
#'
#' @examples
#' \dontrun{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' cache <- matrix(
#'   c(
#'     1, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 1, 1,
#'     0, 0, 0, 0, 0),
#'   nrow = 5, ncol = 5)
#'
#' update_DyNAM_choice_trans(network, 4, 3, 5, cache, transformFun = sqrt)
#' update_DyNAM_choice_trans(network, 1, 4, 0, cache, transformFun = sqrt)
#' update_DyNAM_choice_trans(network, 5, 1, 8, cache, transformFun = sqrt)
#' }
update_DyNAM_choice_trans <- function(network,
                                      sender,
                                      receiver,
                                      replace, cache,
                                      isTwoMode = FALSE,
                                      transformFun = identity) {
  # only relevant for one-mode networks
  res <- list(cache = cache, changes = NULL)
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
  # get all in-neighbors of sender and out-neighbors of receiver
  # consider i -> k -> j,
  # when sender = k and receiver = j, the constraint that k != j has been satisfied.
  temp <- network[, sender]
  # temp[c(sender, receiver)] <- 0 # don't consider the cases with i = k
  temp[sender] <- 0 # don't consider the cases with i = k
  inSender <- which(temp > 0)
  # when sender = i and receiver = k,  the constraint that i != k has been satisfied.
  temp <- network[receiver, ]
  #temp[c(sender, receiver)] <- 0 # don't consider the cases with  k = j
  temp[receiver] <- 0 # don't consider the cases with  k = j
  outReceiver <- which(temp > 0)
  ids <- rbind(
    if (length(outReceiver) > 0) cbind(sender, outReceiver),
    if (length(inSender) > 0) cbind(inSender, receiver)
  )
  # update cache
  if (length(outReceiver) + length(inSender) > 0) {
    # changes in two-paths (i->k->j)
    replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
    res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
    res$changes <- cbind(
      node1 = ids[, 1],
      node2 = ids[, 2],
      replace = forceAndCall(1, transformFun, replaceValues)
    )
  }
  return(res)
}

# cycle ------------------------------------------------------------------------
#' init stat matrix cyclying using cache: Closure of two-paths (j->k->i)
#'
#' @param effectFun function with additional parameters transformFun, isTwoMode
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#'
#' @examples
#' \dontrun{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' effectFUN <- function(isTwoMode = FALSE, transformFun = sqrt)
#'   NULL
#' init_DyNAM_choice.cycle(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.cycle <- function(effectFun, network, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])

  if (isTwoMode) {
    stop("'trans' effect must not use when is a two-mode network (isTwoMode = TRUE)", call. = FALSE)
  }

  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # always weighted
  network <- sign(network)
  # compute stat
  cache <- unname(t(network %*% network))
  diag(cache) <- 0

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat cyclying using cache
#'
#' @param network matrix n1*n1
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache stat matrix numeric n1 * n1
#' @param isTwoMode logical
#' @param transformFun function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases cycle
#'
#' @examples
#' \dontrun{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' cache <- matrix(
#'   c(
#'     1, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 1, 1,
#'     0, 0, 0, 0, 0),
#'   nrow = 5, ncol = 5)
#'
#' update_DyNAM_choice_cycle(network, 4, 3, 5, cache, transformFun = sqrt)
#' update_DyNAM_choice_cycle(network, 1, 4, 0, cache, transformFun = sqrt)
#' update_DyNAM_choice_cycle(network, 5, 1, 8, cache, transformFun = sqrt)
#' }
update_DyNAM_choice_cycle <- function(network,
                                      sender,
                                      receiver,
                                      replace, cache,
                                      isTwoMode = FALSE,
                                      transformFun = identity) {
  # only relevant for one-mode networks
  res <- list(cache = cache, changes = NULL)
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
  # get all in-neighbors of sender and out-neighbors of receiver
  # consider j -> k -> i,
  # when sender = k and receiver = j, the constraint that k != j has been satisfied.
  temp <- network[, sender]
  temp[c(sender, receiver)] <- 0 # don't consider the cases with i = k
  inSender <- which(temp > 0)
  # when sender = i and receiver = k,  the constraint that i != k has been satisfied.
  temp <- network[receiver, ]
  temp[c(sender, receiver)] <- 0 # don't consider the cases with  k = j
  outReceiver <- which(temp > 0)
  ids <- rbind(
    if (length(outReceiver) > 0) cbind(outReceiver, sender),
    if (length(inSender) > 0) cbind(receiver, inSender)
  )
  # update cache
  if (length(outReceiver) + length(inSender) > 0) {
    # changes in two-paths (i->k->j)
    replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
    res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
    res$changes <- cbind(
      node1 = ids[, 1],
      node2 = ids[, 2],
      replace = forceAndCall(1, transformFun, replaceValues)
    )
  }
  return(res)
}

# sender closure ---------------------------------------------------------------
#' init stat matrix using cache: Closure of two-paths (i -> k <- j)
#'
#' two out start closure effect in Rsiena manual, but it's two shared popularity
#' a version that consider the values is balance
#'
#' @param effectFun function with additional parameters transformFun, isTwoMode
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#'
#' @examples
#' \dontrun{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' effectFUN <- function(isTwoMode = FALSE, transformFun = sqrt)
#'   NULL
#' init_DyNAM_choice.clSender(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.clSender <- function(effectFun, network, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])

  if (isTwoMode) {
    stop("'trans' effect must not use when is a two-mode network (isTwoMode = TRUE)", call. = FALSE)
  }

  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # always weighted
  network <- sign(network)
  # compute stat
  cache <- unname(tcrossprod(network, network))

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat using cache
#'
#' @param network matrix n1*n1
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache stat matrix numeric n1 * n1
#' @param isTwoMode logical
#' @param transformFun function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases clSender
#'
#' @examples
#' \dontrun{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' cache <- matrix(
#'   c(
#'     1, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 1, 0, 1,
#'     0, 0, 0, 1, 1,
#'     0, 0, 1, 1, 2),
#'   nrow = 5, ncol = 5)
#'
#' update_DyNAM_choice_clSender(network, 2, 1, 5, cache, transformFun = sqrt)
#' update_DyNAM_choice_clSender(network, 3, 2, 0, cache, transformFun = sqrt)
#' update_DyNAM_choice_clSender(network, 2, 5, 2, cache, transformFun = sqrt)
#' }
update_DyNAM_choice_clSender <- function(
  network,
  sender,
  receiver,
  replace, cache,
  isTwoMode = FALSE,
  transformFun = identity) {
  # only relevant for one-mode networks
  res <- list(cache = cache, changes = NULL)
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
  # get in-neighbors of receiver
  # consider i -> k <- j,
  # when sender = i and receiver = k
  temp <- network[, receiver]
  temp[c(sender, receiver)] <- 0 # don't consider the cases with  k = j
  inReceiver <- which(temp > 0)
  # update cache
  if (length(inReceiver) > 0) {
    ids <- rbind(
      cbind(sender, inReceiver),
      cbind(inReceiver, sender)
    )
    # changes in two-paths (i -> k <- j)
    replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
    res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
    res$changes <- cbind(
      node1 = ids[, 1],
      node2 = ids[, 2],
      replace = forceAndCall(1, transformFun, replaceValues)
    )
  }
  return(res)
}

# receiver closure -------------------------------------------------------------
#' init stat matrix using cache: Closure of two-paths (i <- k ->j)
#'
#' two out start closure effect in Rsiena manual
#' an weighted version could be inStructEq structural equivalence effect with respect to incoming ties
#'
#' @param effectFun function with additional parameters transformFun, isTwoMode
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#'
#' @examples
#' \dontrun{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' effectFUN <- function(isTwoMode = FALSE, transformFun = sqrt)
#'   NULL
#' init_DyNAM_choice.clReceiver(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.clReceiver <- function(effectFun, network, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])

  if (isTwoMode) {
    stop("'trans' effect must not use when is a two-mode network (isTwoMode = TRUE)", call. = FALSE)
  }

  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # always weighted
  network <- sign(network)
  # compute stat
  cache <- unname(crossprod(network, network))

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat transitivity using cache
#'
#' @param network matrix n1*n1
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache stat matrix numeric n1 * n1
#' @param isTwoMode logical
#' @param transformFun function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases clReceiver
#'
#' @examples
#' \dontrun{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' cache <- matrix(
#'   c(
#'     1, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 1, 0, 1,
#'     0, 0, 0, 1, 1,
#'     0, 0, 1, 1, 2),
#'   nrow = 5, ncol = 5)
#'
#' update_DyNAM_choice_clReceiver(network, 1, 2, 5, cache, transformFun = sqrt)
#' update_DyNAM_choice_clReceiver(network, 5, 1, 0, cache, transformFun = sqrt)
#' update_DyNAM_choice_clReceiver(network, 2, 4, 5, cache, transformFun = sqrt)
#' }
update_DyNAM_choice_clReceiver <- function(
  network,
  sender,
  receiver,
  replace, cache,
  isTwoMode = FALSE,
  transformFun = identity) {
  # only relevant for one-mode networks
  res <- list(cache = cache, changes = NULL)
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
  # get in-neighbors of receiver
  # consider i <- k -> j,
  # when sender = k and receiver = j
  temp <- network[sender, ]
  temp[c(sender, receiver)] <- 0 # don't consider the cases with  k = j
  outSender <- which(temp > 0)
  # update cache
  if (length(outSender) > 0) {
    ids <- rbind(
      cbind(outSender, receiver),
      cbind(receiver, outSender)
    )
    # changes in two-paths (i -> k <- j)
    replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
    res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
    res$changes <- cbind(
      node1 = ids[, 1],
      node2 = ids[, 2],
      replace = forceAndCall(1, transformFun, replaceValues)
    )
  }
  return(res)
}

# mixedTrans --------------------------------------------------------------
#' init stat matrix transitivity using cache: Closure of two-paths (i->k->j)
#'
#' @param effectFun function with additional parameters transformFun, isTwoMode
#' @param network list of matrices n1*n2; they should be one-mode over the same set of nodes
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#'
#' @examples
#' \dontrun{
#' net1 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     0, 0, 1, 0, 0,
#'     0, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' net2 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' networks <- list(net1, net2)
#' effectFUN <- function(isTwoMode = FALSE, transformFun = sqrt)
#'   NULL
#' init_DyNAM_choice.mixedTrans(effectFUN, networks, NULL, 5, 5)
#' init_DyNAM_choice.mixedTrans(effectFUN, networks, 1, 5, 5)
#' }

init_DyNAM_choice.mixedTrans <- function(effectFun, network, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])
  # if (isTwoMode) {
  #   stop("'trans' effect must not use when is a two-mode network (isTwoMode = TRUE)", call. = FALSE)
  # }
  # always weighted, detach networks
  network2 <- sign(network[[2]])
  network1 <- sign(network[[1]])
  if (ncol(network1) != nrow(network2) || nrow(network1) != n1 || ncol(network2) != n2)
    stop("Non conformable dimensions.\n\tnetwork 1: ", paste(dim(network1), collapse = ", "),
         "\n\tnetwork 2: ", paste(dim(network1), collapse = ", "),
         "\n\tdependet network: ", n1, ", ", n2,
         "\n\trows of network 1 and cols of network 2 must be the same as the correspondent in dependent network,",
         "cols of network 1 must be the same as rows of network2")
  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network1 == 0) || all(network2 == 0)) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # compute stat
  cache <- unname(network1 %*% network2)

  # # It do no harm if we consider chain i->k->j with i = j
  # if (!isTwoMode) diag(stats) <- 0

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat transitivity using cache
#'
#' @param network list of matrices n1*n2; they should be one-mode over the same set of nodes
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param netUpdate integer, indicates if the first or second network is being updated
#' @param cache stat matrix numeric n1 * n1
#' @param isTwoMode logical
#' @param transformFun function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases mixedTrans
#'
#' @examples
#' \dontrun{
#' net1 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     0, 0, 1, 0, 0,
#'     0, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' net2 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' networks <- list(net1, net2)
#' cache <- matrix(
#'       c(
#'         1, 0, 0, 0, 0,
#'         0, 0, 0, 1, 0,
#'         1, 0, 0, 0, 0,
#'         0, 1, 0, 0, 0,
#'         1, 0, 0, 0, 0),
#'       nrow = 5, ncol = 5, byrow = TRUE)
#' update_DyNAM_choice_mixedTrans(networks, 4, 3, 5, 1, cache, transformFun = sqrt)
#' update_DyNAM_choice_mixedTrans(networks, 4, 3, 5, 2, cache, transformFun = sqrt)
#' update_DyNAM_choice_mixedTrans(networks, 2, 1, 0, 1, cache, transformFun = sqrt)
#' }
update_DyNAM_choice_mixedTrans <- function(network, sender, receiver, replace,
                                           netUpdate,
                                           cache, isTwoMode = FALSE,
                                           transformFun = identity) {
  if (length(netUpdate) > 1 || !netUpdate %in% c(1, 2))
    stop(dQuote("mixedTransTrip2"), "receive a wrong ", dQuote("netUpdate")," argument. ",
         "Check you declare only two networks in network argument", call. = FALSE)

  network2 <- network[[2]]
  network1 <- network[[1]]

  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  replace <- sign(replace)

  if (netUpdate == 1) {
    oldValue <- sign(network1[sender, receiver])
    if (is.na(oldValue) & is.na(replace)) {
      return(res)
    } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
      return(res)
    }
    if (is.na(oldValue)) {
      oldValue <- 0
    }
    if (is.na(replace)) {
      replace <- 0
    }
    # receiver's outNeighbors in network2 create new two paths with sender
    temp <- network2[receiver, ]
    temp[c(sender, receiver)] <- 0
    outReceiver <- which(temp > 0)
    if (length(outReceiver) > 0) {
      ids <- cbind(sender, outReceiver)
      replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
      res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
      res$changes <- cbind(
        node1 = ids[, 1], node2 = ids[, 2],
        replace = forceAndCall(
          1, transformFun,
          replaceValues
        )
      )
    }
    return(res)
  } else {
    oldValue <- sign(network2[sender, receiver])
    if (is.na(oldValue) & is.na(replace)) {
      return(res)
    } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
      return(res)
    }
    if (is.na(oldValue)) {
      oldValue <- 0
    }
    if (is.na(replace)) {
      replace <- 0
    }
    # sender's inNeighbors in network1 create new two paths with receiver
    temp <- network1[, sender]
    temp[c(sender, receiver)] <- 0
    inSender <- which(temp > 0)
    if (length(inSender) > 0) {
      ids <- cbind(inSender, receiver)
      replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
      res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
      res$changes <- cbind(
        node1 = ids[, 1], node2 = ids[, 2],
        replace = forceAndCall(
          1, transformFun,
          replaceValues
        )
      )
    }
    return(res)
  }
}

# mixedCycle --------------------------------------------------------------
#' init stat matrix transitivity using cache: Closure of two-paths (j->k->i)
#'
#' @param effectFun function with additional parameters transformFun, isTwoMode
#' @param network list of matrices n1*n2; they should be one-mode over the same set of nodes
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#'
#' @examples
#' \dontrun{
#' net1 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     0, 0, 1, 0, 0,
#'     0, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' net2 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' networks <- list(net1, net2)
#' effectFUN <- function(isTwoMode = FALSE, transformFun = sqrt)
#'   NULL
#' init_DyNAM_choice.mixedCycle(effectFUN, networks, NULL, 5, 5)
#' init_DyNAM_choice.mixedCycle(effectFUN, networks, 1, 5, 5)
#' }

init_DyNAM_choice.mixedCycle <- function(effectFun, network, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])
  # if (isTwoMode) {
  #   stop("'trans' effect must not use when is a two-mode network (isTwoMode = TRUE)", call. = FALSE)
  # }
  # always weighted, detach networks
  network2 <- sign(network[[2]])
  network1 <- sign(network[[1]])
  if (ncol(network1) != nrow(network2) || nrow(network1) != n1 || ncol(network2) != n2)
    stop("Non conformable dimensions.\n\tnetwork 1: ", paste(dim(network1), collapse = ", "),
         "\n\tnetwork 2: ", paste(dim(network1), collapse = ", "),
         "\n\tdependet network: ", n1, ", ", n2,
         "\n\trows of network 1 and cols of network 2 must be the same as the correspondent in dependent network,",
         "cols of network 1 nust be the same as rows of network2")
  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network1 == 0) || all(network2 == 0)) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # compute stat
  cache <- unname(t(network1 %*% network2))

  # # It do no harm if we consider chain i->k->j with i = j
  # if (!isTwoMode) diag(stats) <- 0

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat transitivity using cache
#'
#' @param network list of matrices n1*n2; they should be one-mode over the same set of nodes
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param netUpdate integer, indicates if the first or second network is being updated
#' @param cache stat matrix numeric n1 * n1
#' @param isTwoMode logical
#' @param transformFun function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases mixedCycle
#'
#' @examples
#' \dontrun{
#' net1 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     0, 0, 1, 0, 0,
#'     0, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' net2 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' networks <- list(net1, net2)
#' cache <- matrix(
#'       c(
#'         1, 0, 0, 0, 0,
#'         0, 0, 0, 1, 0,
#'         1, 0, 0, 0, 0,
#'         0, 1, 0, 0, 0,
#'         1, 0, 0, 0, 0),
#'       nrow = 5, ncol = 5, byrow = TRUE)
#' update_DyNAM_choice_mixedCycle(networks, 4, 3, 5, 1, cache, transformFun = sqrt)
#' update_DyNAM_choice_mixedCycle(networks, 4, 3, 5, 2, cache, transformFun = sqrt)
#' update_DyNAM_choice_mixedCycle(networks, 2, 1, 0, 1, cache, transformFun = sqrt)
#' }
update_DyNAM_choice_mixedCycle <- function(network, sender, receiver, replace,
                                           netUpdate,
                                           cache, isTwoMode = FALSE,
                                           transformFun = identity) {
  if (length(netUpdate) > 1 || !netUpdate %in% c(1, 2))
    stop(dQuote("mixedTransTrip2"), "receive a wrong ", dQuote("netUpdate")," argument. ",
         "Check you declare only two networks in network argument", call. = FALSE)

  network2 <- network[[2]]
  network1 <- network[[1]]

  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  replace <- sign(replace)

  if (netUpdate == 1) {
    oldValue <- sign(network1[sender, receiver])
    if (is.na(oldValue) & is.na(replace)) {
      return(res)
    } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
      return(res)
    }
    if (is.na(oldValue)) {
      oldValue <- 0
    }
    if (is.na(replace)) {
      replace <- 0
    }
    # receiver's outNeighbors in network2 create new two paths with sender
    temp <- network2[receiver, ]
    temp[c(sender, receiver)] <- 0
    outReceiver <- which(temp > 0)
    if (length(outReceiver) > 0) {
      ids <- cbind(outReceiver, sender)
      replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
      res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
      res$changes <- cbind(
        node1 = ids[, 1], node2 = ids[, 2],
        replace = forceAndCall(
          1, transformFun,
          replaceValues
        )
      )
    }
    return(res)
  } else {
    oldValue <- sign(network2[sender, receiver])
    if (is.na(oldValue) & is.na(replace)) {
      return(res)
    } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
      return(res)
    }
    if (is.na(oldValue)) {
      oldValue <- 0
    }
    if (is.na(replace)) {
      replace <- 0
    }
    # sender's inNeighbors in network1 create new two paths with receiver
    temp <- network1[, sender]
    temp[c(sender, receiver)] <- 0
    inSender <- which(temp > 0)
    if (length(inSender) > 0) {
      ids <- cbind(receiver, inSender)
      replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
      res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
      res$changes <- cbind(
        node1 = ids[, 1], node2 = ids[, 2],
        replace = forceAndCall(
          1, transformFun,
          replaceValues
        )
      )
    }
    return(res)
  }
}

# mixed sender closure --------------------------------------------------------
#' init stat matrix using cache: two-paths (i->k<-j)
#'
#' @param effectFun function with additional parameters transformFun, isTwoMode
#' @param network list of two matrices
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#'
#' @examples
#' \dontrun{
#' net1 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     0, 0, 1, 0, 0,
#'     0, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' net2 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' networks <- list(net1, net2)
#' effectFUN <- function(isTwoMode = FALSE, transformFun = sqrt)
#'   NULL
#' init_DyNAM_choice.mixedClSender(effectFUN, networks, NULL, 5, 5)
#' init_DyNAM_choice.mixedClSender(effectFUN, networks, 1, 5, 5)
#' }

init_DyNAM_choice.mixedClSender <- function(effectFun, network, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])
  if (isTwoMode) {
    stop(dQuote("mixedTransTrip2"),
         " effect must not use when is a two-mode network (isTwoMode = TRUE)", call. = FALSE)
  }
  # always weighted, detach networks
  network2 <- sign(network[[2]])
  network1 <- sign(network[[1]])
  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network1 == 0) || all(network2 == 0)) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # compute stat
  cache <- unname(tcrossprod(network1, network2))

  # # It do no harm if we consider chain i->k->j with i = j
  # if (!isTwoMode) diag(stats) <- 0

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat transitivity using cache
#'
#' @param network list of matrices n1*n2; they should be one-mode over the same set of nodes
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param netUpdate integer, indicates if the first or second network is being updated
#' @param cache stat matrix numeric n1 * n1
#' @param isTwoMode logical
#' @param transformFun function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases mixedClSender
#'
#' @examples
#' \dontrun{
#' net1 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     0, 0, 1, 0, 0,
#'     0, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' net2 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' networks <- list(net1, net2)
#' cache <- matrix(
#'   c(1, 0, 0, 0, 0,
#'     0, 1, 0, 0, 0,
#'     0, 0, 1, 0, 1,
#'     0, 1, 0, 0, 0,
#'     0, 1, 1, 0, 1),
#'   nrow = 5, ncol = 5),
#'       nrow = 5, ncol = 5, byrow = TRUE)
#' update_DyNAM_choice_mixedClSender(networks, 5, 1, 2, 1, cache,
#'                                   transformFun = sqrt)
#' update_DyNAM_choice_mixedClSender(networks, 5, 2, 0, 2, cache,
#'                                   transformFun = sqrt)
#' update_DyNAM_choice_mixedClSender(networks, 2, 3, 6, 2, cache,
#'                                   transformFun = sqrt)
#' update_DyNAM_choice_mixedClSender(networks, 4, 3, 6, 2, cache,
#'                                   transformFun = sqrt)
#' }
update_DyNAM_choice_mixedClSender <- function(
  network, sender, receiver, replace,
  netUpdate,
  cache, isTwoMode = FALSE,
  transformFun = identity) {

  if (length(netUpdate) > 1 || !netUpdate %in% c(1, 2))
    stop(dQuote("mixedTransTrip2"), "receive a wrong ", dQuote("netUpdate")," argument. ",
         "Check that only two networks are declared in the 'network' argument", call. = FALSE)
  network2 <- network[[2]]
  network1 <- network[[1]]

  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  replace <- sign(replace)

  if (netUpdate == 1) {
    oldValue <- sign(network1[sender, receiver])
    if (is.na(oldValue) & is.na(replace)) {
      return(res)
    } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
      return(res)
    }
    if (is.na(oldValue)) {
      oldValue <- 0
    }
    if (is.na(replace)) {
      replace <- 0
    }
    # receiver's inNeighbors in network2 create new two in star with sender
    temp <- network2[, receiver]
    temp[c(sender, receiver)] <- 0
    inReceiver <- which(temp > 0)
    if (length(inReceiver) > 0) {
      ids <- cbind(sender, inReceiver)
      replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
      res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
      res$changes <- cbind(
        node1 = ids[, 1], node2 = ids[, 2],
        replace = forceAndCall(
          1, transformFun,
          replaceValues
        )
      )
    }
    return(res)
  } else {
    oldValue <- sign(network2[sender, receiver])
    if (is.na(oldValue) & is.na(replace)) {
      return(res)
    } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
      return(res)
    }
    if (is.na(oldValue)) {
      oldValue <- 0
    }
    if (is.na(replace)) {
      replace <- 0
    }
    # sender's inNeighbors in network1 create new two paths with receiver
    temp <- network1[, receiver]
    temp[c(sender, receiver)] <- 0
    inReceiver <- which(temp > 0)
    if (length(inReceiver) > 0) {
      ids <- cbind(inReceiver, sender)
      replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
      res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
      res$changes <- cbind(
        node1 = ids[, 1], node2 = ids[, 2],
        replace = forceAndCall(
          1, transformFun,
          replaceValues
        )
      )
    }
    return(res)
  }
}

# mixed receiver closure -------------------------------------------------------
#' init stat matrix using cache: two-paths (i<-k->j)
#'
#' @param effectFun function with additional parameters transformFun, isTwoMode
#' @param network list of two matrices
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#'
#' @examples
#' \dontrun{
#' net1 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     0, 0, 1, 0, 0,
#'     0, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' net2 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' networks <- list(net1, net2)
#' effectFUN <- function(isTwoMode = FALSE, transformFun = sqrt)
#'   NULL
#' init_DyNAM_choice.mixedClReceiver(effectFUN, networks, NULL, 5, 5)
#' init_DyNAM_choice.mixedClReceiver(effectFUN, networks, 1, 5, 5)
#' }

init_DyNAM_choice.mixedClReceiver <- function(effectFun, network, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])
  if (isTwoMode) {
    stop(dQuote("mixedTransTrip2"),
         " effect must not use when is a two-mode network (isTwoMode = TRUE)", call. = FALSE)
  }
  # always weighted, detach networks
  network2 <- sign(network[[2]])
  network1 <- sign(network[[1]])
  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network1 == 0) || all(network2 == 0)) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # compute stat
  cache <- unname(crossprod(network1, network2))

  # # It do no harm if we consider chain i->k->j with i = j
  # if (!isTwoMode) diag(stats) <- 0

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat transitivity using cache
#'
#' @param network list of matrices n1*n2; they should be one-mode over the same set of nodes
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param netUpdate integer, indicates if the first or second network is being updated
#' @param cache stat matrix numeric n1 * n1
#' @param isTwoMode logical
#' @param transformFun function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases mixedClReceiver
#'
#' @examples
#' \dontrun{
#' net1 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     0, 0, 1, 0, 0,
#'     0, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' net2 <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     5, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' networks <- list(net1, net2)
#' cache <- matrix(
#'   c(1, 1, 1, 0, 0,
#'     0, 2, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0),
#'   nrow = 5, ncol = 5)
#' update_DyNAM_choice_mixedClReceiver(networks, 3, 4, 2, 1, cache,
#'                                     transformFun = sqrt)
#' update_DyNAM_choice_mixedClReceiver(networks, 5, 3, 2, 2, cache,
#'                                     transformFun = sqrt)
#' update_DyNAM_choice_mixedClReceiver(networks, 4, 3, 0, 1, cache,
#'                                     transformFun = sqrt)
#' update_DyNAM_choice_mixedClReceiver(networks, 1, 4, 0, 1, cache,
#'                                     transformFun = sqrt)
#' }
update_DyNAM_choice_mixedClReceiver <- function(
  network, sender, receiver, replace,
  netUpdate,
  cache, isTwoMode = FALSE,
  transformFun = identity) {

  if (length(netUpdate) > 1 || !netUpdate %in% c(1, 2))
    stop(dQuote("mixedTransTrip2"), "receive a wrong ", dQuote("netUpdate")," argument. ",
         "Check that only two networks are declared in the 'network' argument", call. = FALSE)
  network2 <- network[[2]]
  network1 <- network[[1]]

  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  replace <- sign(replace)

  if (netUpdate == 1) {
    oldValue <- sign(network1[sender, receiver])
    if (is.na(oldValue) & is.na(replace)) {
      return(res)
    } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
      return(res)
    }
    if (is.na(oldValue)) {
      oldValue <- 0
    }
    if (is.na(replace)) {
      replace <- 0
    }
    # receiver's inNeighbors in network2 create new two in star with sender
    temp <- network2[sender, ]
    temp[c(sender, receiver)] <- 0
    outSender <- which(temp > 0)
    if (length(outSender) > 0) {
      ids <- cbind(receiver, outSender)
      replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
      res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
      res$changes <- cbind(
        node1 = ids[, 1], node2 = ids[, 2],
        replace = forceAndCall(
          1, transformFun,
          replaceValues
        )
      )
    }
    return(res)
  } else {
    oldValue <- sign(network2[sender, receiver])
    if (is.na(oldValue) & is.na(replace)) {
      return(res)
    } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
      return(res)
    }
    if (is.na(oldValue)) {
      oldValue <- 0
    }
    if (is.na(replace)) {
      replace <- 0
    }
    # sender's inNeighbors in network1 create new two paths with receiver
    temp <- network1[sender, ]
    temp[c(sender, receiver)] <- 0
    outSender <- which(temp > 0)
    if (length(outSender) > 0) {
      ids <- cbind(outSender, receiver)
      replaceValues <- replace - oldValue + res$cache[cbind(ids[, 1], ids[, 2])]
      res$cache[cbind(ids[, 1], ids[, 2])] <- replaceValues
      res$changes <- cbind(
        node1 = ids[, 1], node2 = ids[, 2],
        replace = forceAndCall(
          1, transformFun,
          replaceValues
        )
      )
    }
    return(res)
  }
}

# four --------------------------------------------------------------------
#' init stat matrix four using cache: Closure of three-paths (i->k<-j->l)
#'
#' @param effectFun function with additional parameters transformFun, isTwoMode
#' @param network matrix n1*n2
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n2
#' @noRd
#'
#' @examples
#' \dontrun{
#' network <- matrix(
#'   c(
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0,
#'     1, 0, 0, 0, 0,
#'     1, 2, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' effectFUN <- function(isTwoMode = FALSE, transformFun = sqrt)
#'   NULL
#' init_DyNAM_choice.four(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.four <- function(effectFun, network, window, n1, n2) {
  # We save the following quantities in the cache: 1. stats 2. network_old 3. two type

  # return zero-matrix if network is without edges
  if (all(network == 0)) {
    return(list(cache = network, stat = network))
  }
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]])

  # if (anyNA(network)) network[is.na(network)] <- 0
  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # always weighted
  network <- sign(network)
  # we don't consider self-connecting edges which may appears in one-mode models.
  if (!isTwoMode) diag(network) <- 0

  # compute stat
  # Consider a chain i->k<-j->l

  # stat <- crossprod(tcrossprod(network, network), network)
  stat <- tcrossprod(network, network) %*% network
  temp <- network * network
  ## i ==j
  # temp <- sapply(1:n1, function(x) drop(network[x, ] %*% network[x, ]))
  stat_1 <- diag(rowSums(temp)) %*% network
  ## l == k
  # temp <- sapply(1:n2, function(x) drop(network[, x] %*% network[, x]))
  stat_2 <- network %*% diag(colSums(temp))
  ## i==j&& l==k which is essentially i -> j
  stat_3 <- network
  stat <- unname(stat - stat_1 - stat_2 + stat_3)

  # cache = list(stat = stat, network_old = network)
  return(list(
    cache = stat,
    stat = forceAndCall(1, funApply, stat)
  ))
}

#' update stat transitivity using cache
#'
#' @param network matrix n1*n2
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache stat matrix numeric n1 * n2
#' @param isTwoMode logical
#' @param transformFun function to apply to the stat
#'
#' @return list:
#'   cache matrix numeric size n1 * n2,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @importFrom stats aggregate
#' @aliases four
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
#' cache <- matrix(
#'   c(
#'     0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0,
#'     2, 0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 2
#'   ),
#'   nrow = 5, ncol = 6, byrow = TRUE
#' )
#'
#' update_DyNAM_choice_four(network,
#'                              3, 5, 2,
#'                              cache,
#'                              isTwoMode = TRUE,
#'                              transformFun = identity)
#' }
update_DyNAM_choice_four <- function(network,
                                     sender, receiver, replace,
                                     cache,
                                     isTwoMode = FALSE,
                                     transformFun = identity) {
  # init res
  res <- list(cache = NULL, changes = NULL)

  # get old value, always weighted
  replace2 <- sign(replace)
  oldValue <- sign(network[sender, receiver])

  # Check if old value has changed
  if (is.na(oldValue) & is.na(replace2)) {
    return(res)
  } else if (!is.na(oldValue) & !is.na(replace2) & oldValue == replace2) {
    return(res)
  }
  if (is.na(oldValue)) oldValue <- 0
  if (is.na(replace)) replace <- 0
  if (!isTwoMode & sender == receiver) return(res)
  # CALCULATE CHANGE

  # If isIncrease is 1, then the number of edges just from zero to nonzero, otherwise it is the other way around.
  isIncrease <- sign(replace)
  # Use the new network
  if (anyNA(network)) network[is.na(network)] <- 0
  network[sender, receiver] <- replace
  network <- 1 * (network > 0)
  if (!isTwoMode) diag(network) <- 0

  # consider all chain i->k<-j->l
  # consider the chains in which the new tie is in position j->l. we consider all possible two-path i->k<-j
  # Here sender = j and receiver = l
  temp <- network[sender, ]
  temp[receiver] <- 0 # don't consider the cases with l = k
  twoPathFromSenderValues <- drop(temp %*% t(network))
  twoPathFromSenderValues[sender] <- 0 # do not consider the cases with j = i
  twoPathFromSender <- which(twoPathFromSenderValues > 0)
  twoPathFromSenderValues <- twoPathFromSenderValues[twoPathFromSender]
  # consider the chains in which the new tie is in position i->k. we consider all possible two-path k<-j->l
  # here sender = i and receiver = k
  temp <- t(network)[receiver, ]
  temp[sender] <- 0 # don't consider the cases with j = i
  twoPathFromReceiverValues <- drop(temp %*% network)
  twoPathFromReceiverValues[receiver] <- 0 # don't consider the cases with k = l
  twoPathFromReceiver <- which(twoPathFromReceiverValues > 0)
  twoPathFromReceiverValues <- twoPathFromReceiverValues[twoPathFromReceiver]
  # consider the chains in which the new tie is in position k<-j. we consider all possible i and l
  # Here sender = j and receiver = k
  temp <- network[sender, ]
  temp[receiver] <- 0 # don't consider the case with k = l
  neighborSender <- which(temp > 0)
  temp <- network[, receiver]
  temp[sender] <- 0 # don't consider the case with k = l
  neighborReceiver <- which(temp > 0)
  inOutCombinations <- expand.grid(neighborReceiver, neighborSender)

  # Calculate the increment
  changes <- rbind(
    if (length(twoPathFromSender) > 0) {
      cbind(
        node1 = twoPathFromSender, node2 = receiver,
        replace = (2 * isIncrease - 1) * twoPathFromSenderValues
      )
    },
    if (length(twoPathFromReceiver) > 0) {
      cbind(
        node1 = sender, node2 = twoPathFromReceiver,
        replace = (2 * isIncrease - 1) * twoPathFromReceiverValues
      )
    },
    if (nrow(inOutCombinations) > 0) {
      cbind(
        node1 = inOutCombinations[, 1], node2 = inOutCombinations[, 2],
        replace = 2 * isIncrease - 1
      )
    }
  )

  if (!is.null(changes)) {
    if (nrow(changes) > 1) {
      changes <- as.data.frame(changes)
      changes <- stats::aggregate(replace ~ ., changes, sum)
      changes <- as.matrix(changes)
    }

    # Calculate the replace
    changes[, "replace"] <- cache[cbind(changes[, "node1"], changes[, "node2"])] + changes[, "replace"]
    cache[cbind(changes[, "node1"], changes[, "node2"])] <- changes[, "replace"]
    # res$changes <- changes
    changes[, "replace"] <- forceAndCall(1, transformFun,
                                         ifelse(changes[, "replace"] >= 0, changes[, "replace"], 0))
  }

  return(list(cache = cache, changes = changes))
}


# Structural and attribute effects ---------------------------------------------
# tertius ----------------------------------------------------------------
init_DyNAM_choice.tertius <- function(effectFun, network, attribute, window, n1, n2) {
  formals(effectFun) <- c(formals(effectFun), list(type = "alter"))
  init_REM_choice.tertius(effectFun = effectFun, network = network, attribute = attribute,
                          window = window,
                          n1 = n1, n2 = n2)
}

update_DyNAM_choice_tertius <- function(network,
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
  update_REM_choice_tertius(
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
    aggregateFun = aggregateFun, type = "alter"
  )
# tertiusDiff ----------------------------------------------------------------
#' init stat matrix tertius-diff using cache
#'
#' @param effectFun function with additional parameters transformFun, aggregateFun
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
#' init_DyNAM_choice.tertiusDiff(effectFUN, network, attribute)
#' }
init_DyNAM_choice.tertiusDiff <- function(effectFun, network, attribute, window, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  aggFun <- eval(params[["aggregateFun"]])
  funApply <- eval(params[["transformFun"]]) # applied FUN instead
  isTwoMode <- eval(params[["isTwoMode"]])
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
  stat <- apply(X = network, MARGIN = 2,
                FUN = function(x) {
                  # # inNeighbor of j
                  inReceiver <- which(x == 1)
                  # # not aggregated if not inNeighbor(j)
                  if (length(inReceiver) == 0) return(NA_real_)
                  # # apply aggFun to inNeighbor(j)
                  forceAndCall(1, aggFun, attribute[inReceiver])
                })

  stat2 <- forceAndCall(1, funApply, outer(attribute, stat, "-"))
  # impute missing entries: nodes without inNeighbor, transformFun(differences)
  if (isTwoMode) {
    stat2[is.na(stat2)] <- mean(stat2, na.rm = TRUE)
  } else {
    diag(stat2) <- NA
    stat2[is.na(stat2)] <- mean(stat2, na.rm = TRUE)
    diag(stat2) <- 0
  }
  # # applied transformFun to z_i - agg_{k \in N^-(j)}(z_k)
  return(list(
    cache = stat,
    stat = stat2
  ))
}

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
#' @aliases tertiusDiff
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
#' update_DyNAM_choice_tertiusDiff(network, attribute,
#'                                 sender = 2, receiver = 3,
#'                                 node = NULL,
#'                                 3,
#'                                 cache,
#'                                 n1 = 5, n2 = 6,
#'                                 transformFun = function(x) x ^ 2,
#'                                 aggregateFun = function(x) median(x, na.rm = TRUE))
#'
#' update_DyNAM_choice_tertiusDiff(network, attribute,
#'                                 sender = NULL, receiver = NULL,
#'                                 node = 3,
#'                                 3,
#'                                 cache,
#'                                 n1 = 5, n2 = 6,
#'                                 transformFun = function(x) x ^ 2,
#'                                 aggregateFun = function(x) median(x, na.rm = TRUE))
#' }
update_DyNAM_choice_tertiusDiff <- function(network,
                                             attribute,
                                             sender = NULL,
                                             receiver = NULL,
                                             node = NULL,
                                             replace,
                                             cache,
                                             n1 = n1, n2 = n2,
                                             isTwoMode = FALSE,
                                             transformFun = abs,
                                             aggregateFun = function(x) mean(x, na.rm = TRUE)) {
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
    valChangeCache <- forceAndCall(1, aggregateFun,
                                    if (length(inReceiver) > 0) attribute[inReceiver] else NA)
    # changes case 1: all nodes needs to be update the att[i] - cache[j] values
    # if (isTwoMode) seq_len(n2) else third(n1, receiver)
    nodesChange <- if (!is.na(valChangeCache)) receiver else numeric()
    isImpute <- ifelse(!isImpute & is.na(valChangeCache), TRUE, isImpute)
    cache[receiver] <- valChangeCache
    changes <- NULL
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

    # changes case 2: is an update value for node, then its update is done separately
    nodesChange <- outNode
    isNotMissCache <- which(!is.na(cache))
    if (!isTwoMode) isNotMissCache <- setdiff(isNotMissCache, node)
    changes <- cbind(
      node1 = node,
      node2 = isNotMissCache,
      replace = forceAndCall(1, transformFun,
                             (if (isTwoMode) replace else replace[-node]) - cache[isNotMissCache])
    )
  }
  changes <- rbind(
    changes,
    Reduce(
      rbind,
      lapply(
        nodesChange,
        function(x)
          cbind(
            node1 = if (isTwoMode) seq_len(n1) else third(n1, x),
            node2 = x,
            replace = forceAndCall(1, transformFun,
                                   (if (isTwoMode) attribute else attribute[-x]) - cache[x])
          )
      )
    )
  )
  # when is just initialize it need to change all values to the average
  if (isEmpty) {
    toImpute <- matrix(TRUE, nrow = n1, ncol = n2)
    toImpute[cbind(changes[, "node1"], changes[, "node2"])] <- FALSE
    if (!isTwoMode) {
      diag(toImpute) <- FALSE
    }
    imputeVal <- mean(changes[, "replace"], na.rm = TRUE)
    changes <- rbind(changes,
                     cbind(which(toImpute, arr.ind = TRUE), imputeVal))
  } else if (isImpute) {
    stat <- forceAndCall(1, transformFun, outer(attribute, cache, "-"))
    toImpute <- is.na(stat)
    toImpute[cbind(changes[, "node1"], changes[, "node2"])] <- FALSE
    if (!isTwoMode) {
      diag(stat) <- NA
      diag(toImpute) <- FALSE
    }
    imputeVal <- mean(stat, na.rm = TRUE)
    if (any(toImpute)) {
      changes <- rbind(changes,
                       cbind(which(toImpute, arr.ind = TRUE), imputeVal))
    }
  }
  return(list(cache = cache, changes = changes))
}

# Covariate effects -------------------------------------------------------
# alter -------------------------------------------------------------------
init_DyNAM_choice.alter <- function(effectFun, attribute, n1, n2) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])

  # compute stat
  stats <- matrix(attribute, nrow = n1, ncol = n2, byrow = TRUE)
  if (!isTwoMode) diag(stats) <- 0

  return(list(stat = stats))
}

#' @aliases alter
update_DyNAM_choice_alter <- function(attribute,
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

  if (!isTwoMode) nodesChange <- third(n1, node) else nodesChange <- seq_len(n1)

  # change stat
  res$changes <- cbind(node1 = nodesChange, node2 = node, replace = replace)
  return(res)
}

# same --------------------------------------------------------------------
init_DyNAM_choice.same <- function(effectFun, attribute) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  if (isTwoMode) stop("effect 'same' doesn't work in two mode networks ('isTwoMode = TRUE')")
  stat <- 1 * outer(attribute, attribute, "==")
  diag(stat) <- 0
  return(list(stat = stat))
}

#' @aliases same
update_DyNAM_choice_same <- function(attribute,
                                     node, replace, isTwoMode = FALSE) {
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

  # compute change stat
  changes <- NULL

  # if replace is missing impute by the average of not missing values, as initial imputation
  if (is.na(replace)) replace <- mean(attribute[-node], na.rm = TRUE)

  oldSameNodes <- setdiff(which(attribute == oldValue), node)
  if (length(oldSameNodes) != 0) {
    changes <- rbind(
      changes,
      cbind(node1 = node, node2 = oldSameNodes, replace = 0),
      cbind(node1 = oldSameNodes, node2 = node, replace = 0)
    )
  }

  sameNodes <- setdiff(which(attribute == replace), node)
  if (length(sameNodes) != 0) {
    changes <- rbind(
      changes,
      cbind(node1 = node, node2 = sameNodes, replace = 1),
      cbind(node1 = sameNodes, node2 = node, replace = 1)
    )
  }

  if (!is.null(changes)) res$changes <- changes

  return(res)
}

# diff --------------------------------------------------------------------
init_DyNAM_choice.diff <- function(effectFun, attribute) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]]) # applied FUN instead
  if (isTwoMode) stop("effect 'diff' doesn't work in two mode networks ('isTwoMode = TRUE')")
  return(list(stat = forceAndCall(1, funApply, outer(attribute, attribute, "-"))))
}

#' @aliases diff
update_DyNAM_choice_diff <- function(attribute, node, replace,
                                     n1, n2,
                                     isTwoMode = FALSE,
                                     transformFun = abs) {
  res <- list(changes = NULL)
  # utility functions to return third nodes
  third <- function(n, diff = c(node)) {
    setdiff(seq_len(n), diff)
  }

  # Get old value
  oldValue <- attribute[node]

  # Check if old value has changed
  if (is.na(oldValue) & is.na(replace)) {
    return(res)
  } else if (!is.na(oldValue) & !is.na(replace) & oldValue == replace) {
    return(res)
  }

  if (is.na(replace)) replace <- mean(attribute[-node], na.rm = TRUE)

  # compute change stat
  newDiff <- forceAndCall(1, transformFun, replace - attribute[-node])

  res$changes <- rbind(
    cbind(node1 = node, node2 = third(n1), replace = newDiff),
    cbind(node1 = third(n1), node2 = node, replace = newDiff)
  )
  return(res)
}

# sim ---------------------------------------------------------------------
init_DyNAM_choice.sim <- function(effectFun, attribute) {
  # Get arguments
  params <- formals(effectFun)
  isTwoMode <- eval(params[["isTwoMode"]])
  funApply <- eval(params[["transformFun"]]) # applied FUN instead
  if (isTwoMode) stop("effect 'sim' doesn't work in two mode networks ('isTwoMode = TRUE')")
  return(list(stat = (-1) * forceAndCall(1, funApply, outer(attribute, attribute, "-"))))
}

#' @aliases sim
update_DyNAM_choice_sim <- function(attribute, node, replace,
                                    n1, n2,
                                    isTwoMode = FALSE,
                                    transformFun = abs) {
  update_DyNAM_choice_diff(attribute = attribute,
                           node = node, replace = replace,
                           n1 = n1, n2 = n2,
                           isTwoMode = isTwoMode,
                           transformFun = function(x) (-1) * transformFun(x)
  )
}
