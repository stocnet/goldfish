# define methods ----------------------------------------------------------
# init the statistical matrix: list(cache = NULL||list, stat = matrix)
init_DyNAM_choice <- function(effect_fun, ...) {
  UseMethod("init_DyNAM_choice")
}

# default -----------------------------------------------------------------
#' @export
init_DyNAM_choice.default <- function(
  effect_fun,
  network = NULL,
  attribute = NULL,
  window,
  n1,
  n2,
  ...
) {
  # print(match.call())
  if (is.null(network) && is.null(attribute)) {
    # this check could be unnecessary
    stop(
      "the effect function doesn't specify neither a network",
      " nor an attribute as argument"
    )
  }

  # if multiple networks, attributes or combination of both are specified.
  # The initialization is done over the fist network
  # lenNetwork <- length(network)
  hasNetwork <- length(network) >= 1
  hasMultNets <- length(network) >= 1 & is.list(network)
  hasMultAtt <- length(attribute) >= 1 & is.list(attribute)

  .args_names <- names(formals(effect_fun))
  # if network inputs, just the first network is empty.
  stats <- matrix(0, nrow = n1, ncol = n2) # check for poss

  # init a generic cache object
  if ("cache" %in% .args_names) {
    cache <- stats
  } else {
    cache <- NULL
  }

  if (hasNetwork) {
    # check if not empty network to initialize the statistical matrix
    # create a copy of the network to iterate over
    if (hasMultNets) {
      areEmpty <- vapply(
        network,
        function(x) all(x[!is.na(x)] == 0),
        logical(1)
      )
      if ((!is.null(window) && !is.infinite(window)) || any(areEmpty)) {
        if (is.null(cache)) {
          return(list(stat = stats))
        }
        return(list(cache = cache, stat = stats))
      }
      netIter <- network[[1]]
    } else {
      if (
        (!is.null(window) && !is.infinite(window)) ||
          all(network[!is.na(network)] == 0)
      ) {
        if (is.null(cache)) {
          return(list(stat = stats))
        }
        return(list(cache = cache, stat = stats))
      }
      netIter <- network
    }

    emptyObject <- array(0, dim = dim(netIter))
  } else {
    if (hasMultAtt) {
      areEmpty <- vapply(
        attribute,
        function(x) all(x[!is.na(x)] == 0),
        logical(1)
      )
      if (any(areEmpty)) {
        if (is.null(cache)) {
          return(list(stat = stats))
        }
        return(list(cache = cache, stat = stats))
      }
      attIter <- attribute[[1]]
    } else {
      if (all(attribute[!is.na(attribute)] == 0)) {
        if (is.null(cache)) {
          return(list(stat = stats))
        }
        return(list(cache = cache, stat = stats))
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
        # set arguments values and only keep the ones in formals(effect_fun)
        .args_fun <- list(
          network = netArg,
          attribute = attribute,
          sender = i,
          receiver = j,
          replace = netIter[i, j],
          n1 = if ("n1" %in% .args_names) n1 else NULL,
          n2 = if ("n2" %in% .args_names) n2 else NULL,
          cache = cache,
          ...
        )
        .args_keep <- pmatch(.args_names, names(.args_fun))
        # construct network objects step by step from empty objects
        res <- do.call(effect_fun, .args_fun[na.omit(.args_keep)])
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
      # set arguments values and only keep the ones in formals(effect_fun)
      .args_fun <- list(
        attribute = attArg,
        node = i,
        replace = attIter[i],
        n1 = if ("n1" %in% .args_names) n1 else NULL,
        n2 = if ("n2" %in% .args_names) n2 else NULL,
        cache = cache
      )
      .args_keep <- pmatch(.args_names, names(.args_fun))
      # construct network objects step by step from empty objects
      res <- do.call(effect_fun, .args_fun[na.omit(.args_keep)])
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
  if (is.null(cache)) {
    return(list(stat = stats))
  }
  return(list(cache = cache, stat = stats))
}

# Structural effects ------------------------------------------------------
# tie ---------------------------------------------------------------------
#' init stat matrix tie
#'
#' @param effect_fun function with additional parameters weighted, transformer_fn
#' @param network matrix n1*n2
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list: stat matrix numeric n1*n2
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
#' effectFUN <- function(weighted = TRUE, transformer_fn = identity) {
#'   NULL
#' }
#' init_DyNAM_choice.tie(effectFUN, network)
#' }
init_DyNAM_choice.tie <- function(effect_fun, network, window, n1, n2, ...) {
  # get arguments
  params <- formals(effect_fun)
  weighted <- eval(params[["weighted"]])
  funApply <- eval(params[["transformer_fn"]])

  # has window or is empty initialize empty
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    value <- if (weighted) forceAndCall(1, funApply, 0) else 0
    return(list(stat = matrix(value, nrow = n1, ncol = n2)))
  }

  if (weighted) {
    stat <- forceAndCall(1, funApply, network)
  } else {
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
#' @param transformer_fn function to apply to the stat
#'
#' @return list:
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases tie
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
#' update_DyNAM_choice_tie(network,
#'   1, 2, 3,
#'   weighted = TRUE, transformer_fn = sqrt
#' )
#' }
update_DyNAM_choice_tie <- function(
  network,
  sender,
  receiver,
  replace,
  weighted = FALSE,
  transformer_fn = identity
) {
  # No change check, irrelevant for two-mode network
  # if(sender == receiver) return(NULL)

  # init res
  res <- list(changes = NULL)

  # Get old value
  old_value <- network[sender, receiver]

  # change for weighted effect
  if (!weighted) {
    old_value <- sign(old_value)
    replace <- sign(replace)
  }

  # If the old value of the tie is the same as the replace value
  if (old_value == replace) {
    return(res)
  }

  # change stat
  res$changes <- cbind(
    node1 = sender,
    node2 = receiver,
    replace = if (!weighted) {
      1 * (replace > 0)
    } else {
      forceAndCall(1, transformer_fn, replace)
    }
  )

  return(res)
}

# inertia -----------------------------------------------------------------
#' @export
init_DyNAM_choice.inertia <- function(
  effect_fun,
  network,
  window,
  n1,
  n2,
  ...
) {
  init_DyNAM_choice.tie(
    effect_fun = effect_fun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

#' @aliases inertia
update_DyNAM_choice_inertia <- function(
  network,
  sender,
  receiver,
  replace,
  weighted = FALSE,
  transformer_fn = identity
) {
  update_DyNAM_choice_tie(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    weighted = weighted,
    transformer_fn = transformer_fn
  )
}

# indeg -------------------------------------------------------------------
#' init stat matrix indegree using cache alter
#'
#' @param effect_fun function with additional parameters
#'   weighted, is_two_mode, transformer_fn
#' @param network matrix n1*n2
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list with named components: cache numeric vector size n2,
#'   stat matrix numeric n1*n2
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
#' effectFUN <- function(weighted = TRUE, is_two_mode = FALSE,
#'                       transformer_fn = identity) {
#'   NULL
#' }
#' init_DyNAM_choice.indeg(effectFUN, network, NULL, 5, 6)
#' }
init_DyNAM_choice.indeg <- function(effect_fun, network, window, n1, n2, ...) {
  # A formula-parsed effect already carries the resolved `type` (native ego/alter
  # in choice); only inject the alter default for a direct init call
  # whose closure has no `type` formal. The shared REM init keeps the two-mode
  # ego guard.
  if (!"type" %in% names(formals(effect_fun))) {
    formals(effect_fun) <- c(formals(effect_fun), list(type = "alter"))
  }
  init_REM_choice.indeg(
    effect_fun = effect_fun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
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
#'     0, 0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 0, 0,
#'     0, 2, 0, 0, 0, 3,
#'     1, 0, 0, 0, 0, 4,
#'     1, 2, 0, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 6, byrow = TRUE
#' )
#' cache <- c(2, 7, 0, 1, 0, 7)
#' update_DyNAM_choice_indeg(
#'   network,
#'   1, 2, 3,
#'   cache, 5, 6,
#'   is_two_mode = TRUE, weighted = TRUE, transformer_fn = sqrt
#' )
#' }
update_DyNAM_choice_indeg <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  n1,
  n2,
  is_two_mode = FALSE,
  weighted = FALSE,
  transformer_fn = identity,
  type = c("alter", "ego")
) {
  update_REM_choice_indeg(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    cache = cache,
    n1 = n1,
    n2 = n2,
    is_two_mode = is_two_mode,
    weighted = weighted,
    transformer_fn = transformer_fn,
    type = type
  )
}

# outdeg -------------------------------------------------------------------
#' init stat matrix outdegree using cache alter
#'
#' @param effect_fun function with additional parameters
#'   weighted, is_two_mode, transformer_fn
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window,
#'   if not null and not Inf return empty stat and cache
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list with named components: cache numeric vector size n1,
#'   stat matrix numeric n1*n2
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
#' effectFUN <- function(weighted = TRUE, is_two_mode = FALSE,
#'                       transformer_fn = identity) {
#'   NULL
#' }
#' init_DyNAM_choice.outdeg(effectFUN, network, NULL, 5, 6)
#' init_DyNAM_choice.outdeg(effectFUN, network, 1, 5, 6)
#' }
init_DyNAM_choice.outdeg <- function(effect_fun, network, window, n1, n2, ...) {
  # See init_DyNAM_choice.indeg: pass a formula-parsed `type` through, inject the
  # alter default only for a type-less direct init call.
  if (!"type" %in% names(formals(effect_fun))) {
    formals(effect_fun) <- c(formals(effect_fun), list(type = "alter"))
  }
  init_REM_choice.outdeg(
    effect_fun = effect_fun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
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
#' @param is_two_mode logical
#' @param weighted logical
#' @param transformer_fn function to apply to the stat
#'
#' @return list:
#'   cache numeric vector size n1,
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
#' cache <- c(2, 7, 0, 1, 0, 7)
#' update_DyNAM_choice_indeg(
#'   network,
#'   1, 2, 3,
#'   cache, 5, 6,
#'   is_two_mode = TRUE, weighted = TRUE, transformer_fn = sqrt
#' )
#' }
update_DyNAM_choice_outdeg <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  n1,
  n2,
  is_two_mode = FALSE,
  weighted = FALSE,
  transformer_fn = identity,
  type = c("alter", "ego")
) {
  update_REM_choice_outdeg(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    cache = cache,
    n1 = n1,
    n2 = n2,
    is_two_mode = is_two_mode,
    weighted = weighted,
    transformer_fn = transformer_fn,
    type = type
  )
}

# recip -------------------------------------------------------------------
#' init stat matrix reciprocity
#'
#' @param effect_fun function with additional parameters
#'   weighted, is_two_mode, transformer_fn
#' @param network matrix n1*n2
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list: stat matrix numeric n1*n2
#' @noRd
#' @export
#'
#' @examples
#' \donttest{
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
#' effectFUN <- function(weighted = FALSE, is_two_mode = FALSE,
#'                       transformer_fn = sqrt) {
#'   NULL
#' }
#'
#' init_DyNAM_choice.recip(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.recip <- function(effect_fun, network, window, n1, n2, ...) {
  params <- formals(effect_fun)
  weighted <- eval(params[["weighted"]])
  funApply <- eval(params[["transformer_fn"]])
  is_two_mode <- eval(params[["is_two_mode"]])

  # Reciprocity reads w[j, i], the transpose of the dyad; on two modes that
  # entry is in a different node space than the model's dyad.
  if (is_two_mode) {
    cli::cli_abort(c(
      "{.fn recip} cannot be computed on a two-mode network.",
      "x" = "It reads the reverse tie {.code w[j, i]}, which a two-mode network
             has no room for.",
      "i" = "Use {.code four()} for the two-mode closure, or {.code inertia()}
             for the tie itself."
    ))
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

  # if (!is_two_mode) diag(stats) <- 0 # # I think is not needed!!!
  return(list(stat = unname(stats)))
}

#' update stat reciprocity
#'
#' @param network matrix n1*n2
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param weighted logical
#' @param is_two_mode logical
#' @param transformer_fn function to apply to the stat
#'
#' @return list:
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases recip
#'
#' @examples
#' \donttest{
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
#' update_DyNAM_choice_recip(
#'   network,
#'   1, 2, 9,
#'   weighted = TRUE, is_two_mode = FALSE, transformer_fn = sqrt
#' )
#' }
update_DyNAM_choice_recip <- function(
  network,
  sender,
  receiver,
  replace,
  weighted = FALSE,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  # init res
  res <- list(changes = NULL)

  # No change check, only relevant for one-mode networks
  if (sender == receiver) {
    return(res)
  }

  # Get old value
  old_value <- network[sender, receiver]

  # change for weighted effect
  if (!weighted) {
    old_value <- sign(old_value)
    replace <- sign(replace)
  }

  # If the old value of the tie is the same as the replace value
  if (old_value == replace) {
    return(res)
  }

  # change stat
  res$changes <- cbind(
    node1 = receiver,
    node2 = sender,
    replace = if (!weighted) {
      1 * (replace > 0)
    } else {
      forceAndCall(1, transformer_fn, replace)
    }
  )

  return(res)
}

# node_trans ------------------------------------------------------------------
#' @export
init_DyNAM_choice.node_trans <- function(
  effect_fun,
  network,
  window,
  n1,
  n2,
  ...
) {
  # See init_DyNAM_choice.indeg: pass a formula-parsed `type` through, inject the
  # alter default only for a type-less direct init call.
  if (!"type" %in% names(formals(effect_fun))) {
    formals(effect_fun) <- c(formals(effect_fun), list(type = "alter"))
  }
  init_REM_choice.node_trans(
    effect_fun = effect_fun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_DyNAM_choice_node_trans <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  n1,
  n2,
  is_two_mode = FALSE,
  transformer_fn = identity,
  type = c("alter", "ego")
) {
  update_REM_choice_node_trans(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    cache = cache,
    n1 = n1,
    n2 = n2,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn,
    type = type
  )
}

# Closure effects --------------------------------------------------------------

#' get in-neighbors of sender
#'
#' @param network square matrix. Positive values indicate ties
#' @param sender integer. Sanitized index of the sender
#' @param receiver integer. Sanitized index of the receiver
#'
#' @returns integer vector. Indices of the in-neighbors of sender
#' @noRd
#'
#' @examples get_two_path_in_neigh(networkState, 1, 2)
get_two_path_in_neigh <- function(network, sender, receiver) {
  temp <- network[, sender]
  temp[sender] <- 0 # don't consider the cases with i = k
  inSender <- which(temp > 0)
  return(inSender)
}

#' get out-neighbors of receiver
#'
#' @param network square matrix. Positive values indicate ties
#' @param sender integer. Sanitized index of the sender
#' @param receiver integer. Sanitized index of the receiver
#'
#' @returns integer vector. Indices of the out-neighbors of receiver
#' @noRd
#'
#' @examples get_two_path_out_neigh(networkState, 1, 2)
get_two_path_out_neigh <- function(network, sender, receiver) {
  temp <- network[receiver, ]
  temp[receiver] <- 0 # don't consider the cases with  k = j
  outReceiver <- which(temp > 0)
  return(outReceiver)
}

#' compute ids of nodes to update the number of two-paths
#'
#' The pooled version doesn't consider the sequence in which the two-path is
#' created. The two-path considered is `source` -> `broker` -> `sink`.
#'
#' @param network square matrix. Positive values indicate ties
#' @param sender integer. Sanitized index of the sender
#' @param receiver integer. Sanitized index of the receiver
#' @param replace numeric. New value of the tie between sender and receiver
#' @param cache square matrix. Number of two-paths between nodes.
#' @param event_order integer vector. Last event updated in
#'   the network argument
#'
#' @returns integer array. Each row contains the source and sink of the two-path
#'   to be updated.
#' @noRd
#'
#' @examples
#'   compute_update_two_path_pooled(
#'     networkState, 1, 2, 1,
#'     cache = networkState %*% networkState,
#'     event_order = c(sender = 0, receiver = 0, event_order = 0)
#'   )
compute_update_two_path_pooled <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  event_order,
  last_update = NULL
) {
  # get all in-neighbors of sender and out-neighbors of receiver
  inSender <- get_two_path_in_neigh(network, sender, receiver)
  outReceiver <- get_two_path_out_neigh(network, sender, receiver)
  ids <- rbind(
    if (length(outReceiver) > 0) cbind(sender, outReceiver),
    if (length(inSender) > 0) cbind(inSender, receiver)
  )
  if (length(ids) > 0) {
    colnames(ids) <- c("source", "sink")
  }
  return(ids)
}

#' compute ids of nodes to update the number of two-paths
#'
#' The sequential version considers the sequence in which the two-path is
#' created. The two-path considered is `source` -> `broker` -> `sink`.
#'
#' @param network square matrix. Positive values indicate ties
#' @param sender integer. Sanitized index of the sender
#' @param receiver integer. Sanitized index of the receiver
#' @param replace numeric. New value of the tie between sender and receiver
#' @param cache square matrix. Number of two-paths between nodes.
#' @param event_order integer vector. Last event updated in
#'  the network argument
#'
#' @returns integer array. Each row contains the source and sink of the two-path
#' @noRd
#'
#' @examples
#'  compute_update_two_path_sequential(
#'    networkState, 1, 2, 1,
#'    cache = networkState %*% networkState,
#'    event_order = c(sender = 0, receiver = 0, event_order = 0)
#'  )
compute_update_two_path_sequential <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  event_order,
  last_update = NULL
) {
  inSender <- get_two_path_in_neigh(network, sender, receiver)

  # when sender = i and receiver = k, we only look for two paths to delete
  if (replace < 1) {
    # when sender = i and receiver = k, constraint i != k is satisfied.
    outReceiver <- get_two_path_out_neigh(network, sender, receiver)
  } else {
    outReceiver <- NULL
  }

  ids <- rbind(
    if (length(outReceiver) > 0) cbind(sender, outReceiver),
    if (length(inSender) > 0) cbind(inSender, receiver)
  )
  if (length(ids) > 0) {
    colnames(ids) <- c("source", "sink")
  }
  return(ids)
}

#' compute ids of nodes to update the number of two-paths
#'
#' The consecutive version force an strict sequence in which the two-path is
#' created. The two-path considered is `source` -> `broker` -> `sink`.
#'
#' @param network square matrix. Positive values indicate ties
#' @param sender integer. Sanitized index of the sender
#' @param receiver integer. Sanitized index of the receiver
#' @param replace numeric. New value of the tie between sender and receiver
#' @param cache square matrix. Number of two-paths between nodes.
#' @param event_order integer vector. Last event updated in
#'  the network argument
#'
#' @returns integer array. Each row contains the source and sink of the two-path
#' @noRd
#'
#' @examples
#' compute_update_two_path_consecutive(
#'  networkState, 1, 2, 1,
#'  cache = networkState %*% networkState,
#'  event_order = c(sender = 0, receiver = 0, event_order = 0)
#' )
compute_update_two_path_consecutive <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  event_order,
  last_update = NULL
) {
  lastSender <- last_update["sender"]
  lastReceiver <- last_update["receiver"]
  lastEventOrder <- last_update["event_order"]

  if (replace == 1) {
    if (
      !is.null(last_update) &&
        (lastEventOrder == (event_order - 1L)) &&
        (sender == lastReceiver)
    ) {
      inSender <- lastSender
    } else {
      inSender <- NULL
    }
    outReceiver <- NULL
  } else if (replace < 1) {
    inSender <- get_two_path_in_neigh(network, sender, receiver)
    outReceiver <- get_two_path_out_neigh(network, sender, receiver)
  } else {
    inSender <- NULL
    outReceiver <- NULL
  }
  ids <- rbind(
    if (length(outReceiver) > 0) cbind(sender, outReceiver),
    if (length(inSender) > 0) cbind(inSender, receiver)
  )
  if (length(ids) > 0) {
    colnames(ids) <- c("source", "sink")
  }
  return(ids)
}
# trans -------------------------------------------------------------------
#' init stat matrix transitivity using cache: Closure of two-paths (i->k->j)
#'
#' @param effect_fun function with additional parameters transformer_fn, is_two_mode
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#' @export
#'
#' @examples
#' \donttest{
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
#' effectFUN <- function(is_two_mode = FALSE, transformer_fn = sqrt) {
#'   NULL
#' }
#' init_DyNAM_choice.trans(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.trans <- function(effect_fun, network, window, n1, n2, ...) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])
  history <- eval(params[["history"]])
  history <- match.arg(history, c('pooled', 'sequential', 'consecutive'))

  # A transitive path i -> k -> j needs the receiver of the first tie to be the
  # sender of the second, so the network must be square over a single mode.
  if (is_two_mode) {
    cli::cli_abort(c(
      "{.fn trans} cannot be computed on a two-mode network.",
      "x" = "It counts paths {.code i -> k -> j}, which need one node set on
             both ends of a tie.",
      "i" = "Use {.code four()} for the two-mode closure, or
             {.code mixed_trans()} to chain a two-mode network with one that
             closes the path."
    ))
  }

  # has window or is empty initialize empty
  if (
    (!is.null(window) && !is.infinite(window)) ||
      (all(network == 0)) ||
      (history != 'pooled')
  ) {
    cache <- matrix(0, nrow = n1, ncol = n2)
    if (history == "consecutive") {
      attr(cache, "last_update") <- c(sender = 0, receiver = 0, event_order = 0)
    }
    return(list(
      cache = cache,
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
#' @param is_two_mode logical
#' @param transformer_fn function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases trans
#'
#' @examples
#' \donttest{
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
#'     0, 0, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5
#' )
#'
#' update_DyNAM_choice_trans(network, 4, 3, 5, cache, transformer_fn = sqrt)
#' update_DyNAM_choice_trans(network, 1, 4, 0, cache, transformer_fn = sqrt)
#' update_DyNAM_choice_trans(network, 5, 1, 8, cache, transformer_fn = sqrt)
#' }

update_DyNAM_choice_trans <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity,
  history = c('pooled', 'sequential', 'consecutive'),
  event_order = NULL
) {
  history <- match.arg(history, c('pooled', 'sequential', 'consecutive'))
  # only relevant for one-mode networks
  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }

  # get old value, always unweighted
  replace <- sign(replace)
  old_value <- sign(network[sender, receiver])

  # If the old value of the tie is the same as the replace value
  if (old_value == replace) {
    return(res)
  }

  last_update <- if (history == "consecutive") {
    attr(res$cache, "last_update")
  } else {
    NULL
  }

  ids <- do.call(
    what = paste0("compute_update_two_path_", history),
    args = list(
      network = network,
      sender = sender,
      receiver = receiver,
      replace = replace,
      cache = cache,
      event_order = event_order,
      last_update = last_update
    )
  )

  if (history == "consecutive" && replace >= 1) {
    attr(res$cache, "last_update") <- c(
      sender = sender,
      receiver = receiver,
      event_order = event_order
    )
  }

  res <- apply_two_path_update(res, ids, replace, old_value, transformer_fn)
  return(res)
}

# cycle ------------------------------------------------------------------------
#' init stat matrix cyclying using cache: Closure of two-paths (j->k->i)
#'
#' @param effect_fun function with additional parameters transformer_fn, is_two_mode
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#' @export
#'
#' @examples
#' \donttest{
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
#' effectFUN <- function(is_two_mode = FALSE, transformer_fn = sqrt) {
#'   NULL
#' }
#' init_DyNAM_choice.cycle(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.cycle <- function(effect_fun, network, window, n1, n2, ...) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])
  history <- eval(params[["history"]])
  history <- match.arg(history, c('pooled', 'sequential', 'consecutive'))

  # A cycle i -> k -> j -> i closes back onto the sender, so the network must be
  # square over a single mode.
  if (is_two_mode) {
    cli::cli_abort(c(
      "{.fn cycle} cannot be computed on a two-mode network.",
      "x" = "It counts paths that close back onto the sender, which need one
             node set on both ends of a tie.",
      "i" = "Use {.code four()} for the two-mode closure, or
             {.code mixed_cycle()} to chain a two-mode network with one that
             closes the path."
    ))
  }

  # has window or is empty initialize empty
  if (
    (!is.null(window) && !is.infinite(window)) ||
      (all(network == 0)) ||
      (history != 'pooled')
  ) {
    cache <- matrix(0, nrow = n1, ncol = n2)
    if (history == "consecutive") {
      attr(cache, "last_update") <- c(sender = 0, receiver = 0, event_order = 0)
    }
    return(list(
      cache = cache,
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

#' update stat cycle of length two using cache
#'
#' @param network matrix n1*n1
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache stat matrix numeric n1 * n1
#' @param is_two_mode logical
#' @param transformer_fn function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases cycle
#'
#' @examples
#' \donttest{
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
#'     0, 0, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5
#' )
#'
#' update_DyNAM_choice_cycle(network, 4, 3, 5, cache, transformer_fn = sqrt)
#' update_DyNAM_choice_cycle(network, 1, 4, 0, cache, transformer_fn = sqrt)
#' update_DyNAM_choice_cycle(network, 5, 1, 8, cache, transformer_fn = sqrt)
#' }
update_DyNAM_choice_cycle <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity,
  history = c('pooled', 'sequential', 'consecutive'),
  event_order = 0
) {
  history <- match.arg(history, c('pooled', 'sequential', 'consecutive'))
  # only relevant for one-mode networks
  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  # get old value, always weighted
  replace <- sign(replace)
  old_value <- sign(network[sender, receiver])

  # If the old value of the tie is the same as the replace value
  if (old_value == replace) {
    return(res)
  }

  last_update <- if (history == "consecutive") {
    attr(res$cache, "last_update")
  } else {
    NULL
  }

  ids <- do.call(
    what = paste0("compute_update_two_path_", history),
    args = list(
      network = network,
      sender = sender,
      receiver = receiver,
      replace = replace,
      cache = cache,
      event_order = event_order,
      last_update = last_update
    )
  )

  if (history == "consecutive" && replace >= 1) {
    attr(res$cache, "last_update") <- c(
      sender = sender,
      receiver = receiver,
      event_order = event_order
    )
  }

  if (length(ids) > 0) {
    colnames(ids) <- c("source", "sink")
    ids_cycle <- ids[, c("sink", "source"), drop = FALSE]
    res <- apply_two_path_update(
      res,
      ids_cycle,
      replace,
      old_value,
      transformer_fn
    )
  }
  return(res)
}


# closure common receiver  ------------------------------------------------
#' init stat matrix using cache: Closure of two-paths (i -> k <- j)
#'
#' two out start closure effect in Rsiena manual (transTrip2),
#' but it's two shared popularity (sharedPop)
#' a version that consider the values is balance
#'
#' @param effect_fun function with additional parameters transformer_fn, is_two_mode
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#' @export
#'
#' @examples
#' \donttest{
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
#' effectFUN <- function(is_two_mode = FALSE, transformer_fn = sqrt) {
#'   NULL
#' }
#' init_DyNAM_choice.common_receiver(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.common_receiver <- function(
  effect_fun,
  network,
  window,
  n1,
  n2,
  ...
) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])

  # Both i and j index the *sender* side of the covariate (the statistic sums
  # w[i, k] * w[j, k]), so the dependent network must be one-mode over that
  # side. A two-mode covariate is the valid case: it projects shared
  # affiliations onto the one-mode dependent network. Where the mode map is
  # available the parser has already compared the node sets themselves; this
  # dimension check is the backstop for a direct call.
  if (n1 != n2 || nrow(network) != n1) {
    cli::cli_abort(c(
      "{.fn common_receiver} needs a one-mode dependent network.",
      "x" = "Its two indices both read the sender side of the covariate
             ({nrow(network)} node{?s}), while the dependent network is
             {n1} x {n2}.",
      "i" = "The covariate may be two-mode, but its sender side must be the
             dependent network's node set."
    ))
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
  cache <- unname(tcrossprod(network))

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
#' @param is_two_mode logical
#' @param transformer_fn function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#'
#' @examples
#' \donttest{
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
#'     0, 0, 1, 1, 2
#'   ),
#'   nrow = 5, ncol = 5
#' )
#'
#' update_DyNAM_choice_common_receiver(network, 2, 1, 5, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice_common_receiver(network, 3, 2, 0, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice_common_receiver(network, 2, 5, 2, cache,
#'   transformer_fn = sqrt
#' )
#' }
update_DyNAM_choice_common_receiver <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  # only relevant for one-mode networks
  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  # get old value, always weighted
  replace <- sign(replace)
  old_value <- sign(network[sender, receiver])

  # If the old value of the tie is the same as the replace value
  if (old_value == replace) {
    return(res)
  }
  # get in-neighbors of receiver
  # consider i -> k <- j,
  # when sender = i and receiver = k
  temp <- network[, receiver]
  temp[c(sender, receiver)] <- 0 # don't consider the cases with  k = j
  inReceiver <- which(temp > 0)
  if (length(inReceiver) > 0) {
    ids <- rbind(
      cbind(sender, inReceiver),
      cbind(inReceiver, sender)
    )
    res <- apply_two_path_update(res, ids, replace, old_value, transformer_fn)
  }
  return(res)
}

# closure common sender ---------------------------------------------------
#' init stat matrix using cache: Closure of two-paths (i <- k ->j)
#'
#' two out start closure effect in Rsiena manual
#' an weighted version could be inStructEq structural equivalence effect
#' with respect to incoming ties
#'
#' @param effect_fun function with additional parameters transformer_fn, is_two_mode
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#' @export
#'
#' @examples
#' \donttest{
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
#' effectFUN <- function(is_two_mode = FALSE, transformer_fn = sqrt) {
#'   NULL
#' }
#' init_DyNAM_choice.common_sender(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.common_sender <- function(
  effect_fun,
  network,
  window,
  n1,
  n2,
  ...
) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])

  # Both i and j index the *receiver* side of the covariate (the statistic sums
  # w[k, i] * w[k, j]), so the dependent network must be one-mode over that
  # side. A two-mode covariate is the valid case: it projects shared
  # affiliations onto the one-mode dependent network. Where the mode map is
  # available the parser has already compared the node sets themselves; this
  # dimension check is the backstop for a direct call.
  if (n1 != n2 || ncol(network) != n1) {
    cli::cli_abort(c(
      "{.fn common_sender} needs a one-mode dependent network.",
      "x" = "Its two indices both read the receiver side of the covariate
             ({ncol(network)} node{?s}), while the dependent network is
             {n1} x {n2}.",
      "i" = "The covariate may be two-mode, but its receiver side must be the
             dependent network's node set."
    ))
  }

  #n1 <- nrow(network)
  #n2 <- ncol(network)
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
  cache <- unname(crossprod(network))

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat common sender using cache
#'
#' @param network matrix n1*n1
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param cache stat matrix numeric n1 * n1
#' @param is_two_mode logical
#' @param transformer_fn function to apply to the stat
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#'
#' @examples
#' \donttest{
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
#'     0, 0, 1, 1, 2
#'   ),
#'   nrow = 5, ncol = 5
#' )
#'
#' update_DyNAM_choice.common_sender(
#'   network, 1, 2, 5, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice.common_sender(
#'   network, 5, 1, 0, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice.common_sender(
#'   network, 2, 4, 5, cache,
#'   transformer_fn = sqrt
#' )
#' }
update_DyNAM_choice_common_sender <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  # only relevant for one-mode networks
  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  # get old value, always weighted
  replace <- sign(replace)
  old_value <- sign(network[sender, receiver])

  # If the old value of the tie is the same as the replace value
  if (old_value == replace) {
    return(res)
  }

  # get out-neighbors of sender
  # consider i <- k -> j,
  # when sender = k and receiver = j
  temp <- network[sender, ]
  temp[c(sender, receiver)] <- 0 # don't consider the cases with  k = j
  outSender <- which(temp > 0)
  if (length(outSender) > 0) {
    ids <- rbind(
      cbind(outSender, receiver),
      cbind(receiver, outSender)
    )
    res <- apply_two_path_update(res, ids, replace, old_value, transformer_fn)
  }
  return(res)
}

# mixed_trans --------------------------------------------------------------
#' init stat matrix transitivity using cache: Closure of two-paths (i->k->j)
#'
#' @param effect_fun function with additional parameters transformer_fn, is_two_mode
#' @param network list of matrices n1*n2;
#'   they should be one-mode over the same set of nodes
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#' @export
#'
#' @examples
#' \donttest{
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
#' effectFUN <- function(is_two_mode = FALSE, transformer_fn = sqrt) {
#'   NULL
#' }
#' init_DyNAM_choice.mixed_trans(effectFUN, networks, NULL, 5, 5)
#' init_DyNAM_choice.mixed_trans(effectFUN, networks, 1, 5, 5)
#' }
init_DyNAM_choice.mixed_trans <- function(
  effect_fun,
  network,
  window,
  n1,
  n2,
  ...
) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])
  history <- eval(params[["history"]])
  history <- match.arg(history, c("pooled", "sequential"))
  if (identical(history, "consecutive")) {
    cli::cli_abort(
      "history = \"consecutive\" is not supported for mixed-network effects."
    )
  }

  # always weighted, detach networks
  network2 <- sign(network[[2]])
  network1 <- sign(network[[1]])
  if (
    ncol(network1) != nrow(network2) ||
      nrow(network1) != n1 ||
      ncol(network2) != n2
  ) {
    abort_mixed_chain("mixed_trans", network1, network2, n1, n2)
  }
  # has window or is empty or non-pooled history: initialize empty
  if (
    (!is.null(window) && !is.infinite(window)) ||
      all(network1 == 0) ||
      all(network2 == 0) ||
      history != "pooled"
  ) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # compute stat
  cache <- unname(network1 %*% network2)

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat mixed transitivity using cache
#'
#' @param network list of matrices n1*n2;
#'   they should be one-mode over the same set of nodes
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param net_update integer, indicates if the first or second network
#'   is being updated
#' @param cache stat matrix numeric n1 * n1
#' @param is_two_mode logical
#' @param transformer_fn function to apply to the stat
#' @param history character, one of \code{"pooled"} (default) or
#'   \code{"sequential"}. When \code{"sequential"}, only additions to the
#'   second network (\code{net_update = 2}) create new two-path counts;
#'   additions to the first network (\code{net_update = 1}) are skipped.
#'   Removals always update the cache regardless of \code{history}.
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases mixed_trans
#'
#' @examples
#' \donttest{
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
#'   c(
#'     1, 0, 0, 0, 0,
#'     0, 0, 0, 1, 0,
#'     1, 0, 0, 0, 0,
#'     0, 1, 0, 0, 0,
#'     1, 0, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' update_DyNAM_choice_mixed_trans(networks, 4, 3, 5, 1, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice_mixed_trans(networks, 4, 3, 5, 2, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice_mixed_trans(networks, 2, 1, 0, 1, cache,
#'   transformer_fn = sqrt
#' )
#' }
update_DyNAM_choice_mixed_trans <- function(
  network,
  sender,
  receiver,
  replace,
  net_update,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity,
  history = c("pooled", "sequential")
) {
  history <- match.arg(history)
  if (length(net_update) > 1 || !net_update %in% c(1, 2)) {
    stop(
      dQuote("mixed_trans"),
      "receive a wrong ",
      dQuote("net_update"),
      " argument. ",
      "Check you declare only two networks in network argument",
      call. = FALSE
    )
  }

  network2 <- network[[2]]
  network1 <- network[[1]]

  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  replace <- sign(replace)

  if (net_update == 1) {
    old_value <- sign(network1[sender, receiver])
    if (old_value == replace) {
      return(res)
    }
    if (history == "sequential" && replace >= 1) {
      return(res)
    }
    temp <- network2[receiver, ]
    temp[c(sender, receiver)] <- 0
    outReceiver <- which(temp > 0)
    if (length(outReceiver) > 0) {
      ids <- cbind(sender, outReceiver)
      res <- apply_two_path_update(res, ids, replace, old_value, transformer_fn)
    }
    return(res)
  } else {
    old_value <- sign(network2[sender, receiver])
    if (old_value == replace) {
      return(res)
    }
    temp <- network1[, sender]
    temp[c(sender, receiver)] <- 0
    inSender <- which(temp > 0)
    if (length(inSender) > 0) {
      ids <- cbind(inSender, receiver)
      res <- apply_two_path_update(res, ids, replace, old_value, transformer_fn)
    }
    return(res)
  }
}

# mixed_cycle --------------------------------------------------------------
#' init stat matrix transitivity using cache: Closure of two-paths (j->k->i)
#'
#' @param effect_fun function with additional parameters transformer_fn, is_two_mode
#' @param network list of matrices n1*n2;
#'   they should be one-mode over the same set of nodes
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#' @export
#'
#' @examples
#' \donttest{
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
#' effectFUN <- function(is_two_mode = FALSE, transformer_fn = sqrt) {
#'   NULL
#' }
#' init_DyNAM_choice.mixed_cycle(effectFUN, networks, NULL, 5, 5)
#' init_DyNAM_choice.mixed_cycle(effectFUN, networks, 1, 5, 5)
#' }
init_DyNAM_choice.mixed_cycle <- function(
  effect_fun,
  network,
  window,
  n1,
  n2,
  ...
) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])
  history <- eval(params[["history"]])
  history <- match.arg(history, c("pooled", "sequential"))
  if (identical(history, "consecutive")) {
    cli::cli_abort(
      "history = \"consecutive\" is not supported for mixed-network effects."
    )
  }

  # always weighted, detach networks
  network2 <- sign(network[[2]])
  network1 <- sign(network[[1]])

  if (
    ncol(network1) != nrow(network2) ||
      nrow(network1) != n1 ||
      ncol(network2) != n2
  ) {
    abort_mixed_chain("mixed_cycle", network1, network2, n1, n2)
  }
  # has window or is empty or non-pooled history: initialize empty
  if (
    (!is.null(window) && !is.infinite(window)) ||
      all(network1 == 0) ||
      all(network2 == 0) ||
      history != "pooled"
  ) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # compute stat
  cache <- unname(t(network1 %*% network2))

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat mixed cycle using cache
#'
#' @param network list of matrices n1*n2
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param net_update integer, indicates if the first or second network
#'   is being updated
#' @param cache stat matrix numeric n1 * n1
#' @param is_two_mode logical
#' @param transformer_fn function to apply to the stat
#' @param history character, one of \code{"pooled"} (default) or
#'   \code{"sequential"}. When \code{"sequential"}, only additions to the
#'   second network (\code{net_update = 2}) create new two-path counts;
#'   additions to the first network (\code{net_update = 1}) are skipped.
#'   Removals always update the cache regardless of \code{history}.
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases mixed_cycle
#'
#' @examples
#' \donttest{
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
#'   c(
#'     1, 0, 0, 0, 0,
#'     0, 0, 0, 1, 0,
#'     1, 0, 0, 0, 0,
#'     0, 1, 0, 0, 0,
#'     1, 0, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' update_DyNAM_choice_mixed_cycle(networks, 4, 3, 5, 1, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice_mixed_cycle(networks, 4, 3, 5, 2, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice_mixed_cycle(networks, 2, 1, 0, 1, cache,
#'   transformer_fn = sqrt
#' )
#' }
update_DyNAM_choice_mixed_cycle <- function(
  network,
  sender,
  receiver,
  replace,
  net_update,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity,
  history = c("pooled", "sequential")
) {
  history <- match.arg(history)
  if (length(net_update) > 1 || !net_update %in% c(1, 2)) {
    stop(
      dQuote("mixed_cycle"),
      " receive a wrong ",
      dQuote("net_update"),
      " argument. ",
      "Check that you only declare two networks as argument.",
      call. = FALSE
    )
  }

  network2 <- network[[2]]
  network1 <- network[[1]]

  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  replace <- sign(replace)

  if (net_update == 1) {
    old_value <- sign(network1[sender, receiver])
    if (old_value == replace) {
      return(res)
    }
    if (history == "sequential" && replace >= 1) {
      return(res)
    }
    outReceiver <- get_two_path_out_neigh(network2, sender, receiver)
    if (length(outReceiver) > 0) {
      ids <- cbind(outReceiver, sender)
      res <- apply_two_path_update(res, ids, replace, old_value, transformer_fn)
    }
    return(res)
  } else {
    old_value <- sign(network2[sender, receiver])
    if (old_value == replace) {
      return(res)
    }
    inSender <- get_two_path_in_neigh(network1, sender, receiver)
    if (length(inSender) > 0) {
      ids <- cbind(receiver, inSender)
      res <- apply_two_path_update(res, ids, replace, old_value, transformer_fn)
    }
    return(res)
  }
}

# mixed common receiver ---------------------------------------------------
#' init stat matrix using cache: two-paths (i->k<-j)
#'
#' @param effect_fun function with additional parameters transformer_fn, is_two_mode
#' @param network list of two matrices
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#' @export
#'
#' @examples
#' \donttest{
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
#' effectFUN <- function(is_two_mode = FALSE, transformer_fn = sqrt) {
#'   NULL
#' }
#' init_DyNAM_choice.mixed_common_receiver(effectFUN, networks, NULL, 5, 5)
#' init_DyNAM_choice.mixed_common_receiver(effectFUN, networks, 1, 5, 5)
#' }
init_DyNAM_choice.mixed_common_receiver <- function(
  effect_fun,
  network,
  window,
  n1,
  n2,
  ...
) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])
  history <- eval(params[["history"]])
  history <- match.arg(history, c("pooled", "sequential"))
  if (identical(history, "consecutive")) {
    cli::cli_abort(
      "history = \"consecutive\" is not supported for mixed-network effects."
    )
  }
  # The statistic is symmetrized over the dyad, so the dependent network must be
  # one-mode; the two covariates may each be two-mode as long as they share the
  # side they are compared on.
  if (is_two_mode) {
    cli::cli_abort(c(
      "{.fn mixed_common_receiver} needs a one-mode dependent network.",
      "x" = "Its two indices both read a sender side, which a two-mode
             dependent network splits over different node sets."
    ))
  }
  # always weighted, detach networks
  network2 <- sign(network[[2]])
  network1 <- sign(network[[1]])
  # has window or is empty or non-pooled history: initialize empty
  if (
    (!is.null(window) && !is.infinite(window)) ||
      all(network1 == 0) ||
      all(network2 == 0) ||
      history != "pooled"
  ) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # compute stat
  cache <- unname(tcrossprod(network1, network2))
  cache <- cache + t(cache)

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat mixed common receiver using cache
#'
#' @param network list of matrices n1*n2;
#'   they should be one-mode over the same set of nodes
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param net_update integer, indicates if the first or second network
#'   is being updated
#' @param cache stat matrix numeric n1 * n1
#' @param is_two_mode logical
#' @param transformer_fn function to apply to the stat
#' @param history character, one of \code{"pooled"} (default) or
#'   \code{"sequential"}. When \code{"sequential"}, only additions to the
#'   second network (\code{net_update = 2}) create new two-path counts;
#'   additions to the first network (\code{net_update = 1}) are skipped.
#'   Removals always update the cache regardless of \code{history}.
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#'
#' @examples
#' \donttest{
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
#'   c(
#'     1, 0, 0, 0, 0,
#'     0, 1, 0, 0, 0,
#'     0, 0, 1, 0, 1,
#'     0, 1, 0, 0, 0,
#'     0, 1, 1, 0, 1
#'   ),
#'   nrow = 5, ncol = 5, byrow = TRUE
#' )
#' update_DyNAM_choice_mixed_common_receiver(networks, 5, 1, 2, 1, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice_mixed_common_receiver(networks, 5, 2, 0, 2, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice_mixed_common_receiver(networks, 2, 3, 6, 2, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice_mixed_common_receiver(networks, 4, 3, 6, 2, cache,
#'   transformer_fn = sqrt
#' )
#' }
update_DyNAM_choice_mixed_common_receiver <- function(
  network,
  sender,
  receiver,
  replace,
  net_update,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity,
  history = c("pooled", "sequential")
) {
  history <- match.arg(history)
  if (length(net_update) > 1 || !net_update %in% c(1, 2)) {
    stop(
      dQuote("mixed_common_receiver"),
      "receive a wrong ",
      dQuote("net_update"),
      " argument. ",
      "Check that you only declare two networks as argument",
      call. = FALSE
    )
  }
  network2 <- network[[2]]
  network1 <- network[[1]]

  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  replace <- sign(replace)

  if (net_update == 1) {
    old_value <- sign(network1[sender, receiver])
    if (old_value == replace) {
      return(res)
    }
    if (history == "sequential" && replace >= 1) {
      return(res)
    }
    temp <- network2[, receiver]
    temp[c(sender, receiver)] <- 0
    inReceiver <- which(temp > 0)
    if (length(inReceiver) > 0) {
      ids <- rbind(cbind(sender, inReceiver), cbind(inReceiver, sender))
      res <- apply_two_path_update(res, ids, replace, old_value, transformer_fn)
    }
    return(res)
  } else {
    old_value <- sign(network2[sender, receiver])
    if (old_value == replace) {
      return(res)
    }
    temp <- network1[, receiver]
    temp[c(sender, receiver)] <- 0
    inReceiver <- which(temp > 0)
    if (length(inReceiver) > 0) {
      ids <- rbind(cbind(inReceiver, sender), cbind(sender, inReceiver))
      res <- apply_two_path_update(res, ids, replace, old_value, transformer_fn)
    }
    return(res)
  }
}

# mixed common sender -------------------------------------------------------
#' init stat matrix using cache: two-paths (i<-k->j)
#'
#' @param effect_fun function with additional parameters transformer_fn, is_two_mode
#' @param network list of two matrices
#' @param window NULL||numeric(1) size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n1
#' @noRd
#' @export
#'
#' @examples
#' \donttest{
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
#' effectFUN <- function(is_two_mode = FALSE, transformer_fn = sqrt) {
#'   NULL
#' }
#' init_DyNAM_choice.mixed_common_sender(effectFUN, networks, NULL, 5, 5)
#' init_DyNAM_choice.mixed_common_sender(effectFUN, networks, 1, 5, 5)
#' }
init_DyNAM_choice.mixed_common_sender <- function(
  effect_fun,
  network,
  window,
  n1,
  n2,
  ...
) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])
  history <- eval(params[["history"]])
  history <- match.arg(history, c("pooled", "sequential"))
  if (identical(history, "consecutive")) {
    cli::cli_abort(
      "history = \"consecutive\" is not supported for mixed-network effects."
    )
  }
  # The statistic is symmetrized over the dyad, so the dependent network must be
  # one-mode; the two covariates may each be two-mode as long as they share the
  # side they are compared on.
  if (is_two_mode) {
    cli::cli_abort(c(
      "{.fn mixed_common_sender} needs a one-mode dependent network.",
      "x" = "Its two indices both read a receiver side, which a two-mode
             dependent network splits over different node sets."
    ))
  }
  # always weighted, detach networks
  network2 <- sign(network[[2]])
  network1 <- sign(network[[1]])
  # has window or is empty or non-pooled history: initialize empty
  if (
    (!is.null(window) && !is.infinite(window)) ||
      all(network1 == 0) ||
      all(network2 == 0) ||
      history != "pooled"
  ) {
    return(list(
      cache = matrix(0, nrow = n1, ncol = n2),
      stat = matrix(forceAndCall(1, funApply, 0), nrow = n1, ncol = n2)
    ))
  }
  # compute stat
  cache <- unname(crossprod(network1, network2))
  cache <- cache + t(cache)

  return(list(
    cache = cache,
    stat = forceAndCall(1, funApply, cache)
  ))
}

#' update stat mixed common sender using cache
#'
#' @param network list of matrices n1*n2; they should be one-mode over
#'   the same set of nodes
#' @param sender integer
#' @param receiver integer
#' @param replace numeric
#' @param net_update integer, indicates if the first or second network
#'   is being updated
#' @param cache stat matrix numeric n1 * n1
#' @param is_two_mode logical
#' @param transformer_fn function to apply to the stat
#' @param history character, one of \code{"pooled"} (default) or
#'   \code{"sequential"}. When \code{"sequential"}, only additions to the
#'   second network (\code{net_update = 2}) create new two-path counts;
#'   additions to the first network (\code{net_update = 1}) are skipped.
#'   Removals always update the cache regardless of \code{history}.
#'
#' @return list:
#'   cache matrix size n1 * n1,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#'
#' @examples
#' \donttest{
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
#'   c(
#'     1, 1, 1, 0, 0,
#'     0, 2, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0
#'   ),
#'   nrow = 5, ncol = 5
#' )
#' update_DyNAM_choice_mixed_common_sender(networks, 3, 4, 2, 1, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice_mixed_common_sender(networks, 5, 3, 2, 2, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice.mixed_common_sender(networks, 4, 3, 0, 1, cache,
#'   transformer_fn = sqrt
#' )
#' update_DyNAM_choice.mixed_common_sender(networks, 1, 4, 0, 1, cache,
#'   transformer_fn = sqrt
#' )
#' }
update_DyNAM_choice_mixed_common_sender <- function(
  network,
  sender,
  receiver,
  replace,
  net_update,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity,
  history = c("pooled", "sequential")
) {
  history <- match.arg(history)
  if (length(net_update) > 1 || !net_update %in% c(1, 2)) {
    stop(
      dQuote("mixed_common_sender"),
      "receive a wrong ",
      dQuote("net_update"),
      " argument. ",
      "Check that only two networks are declared in the 'network' argument",
      call. = FALSE
    )
  }
  network2 <- network[[2]]
  network1 <- network[[1]]

  res <- list(cache = cache, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  replace <- sign(replace)

  if (net_update == 1) {
    old_value <- sign(network1[sender, receiver])
    if (old_value == replace) {
      return(res)
    }
    if (history == "sequential" && replace >= 1) {
      return(res)
    }
    temp <- network2[sender, ]
    temp[c(sender, receiver)] <- 0
    outSender <- which(temp > 0)
    if (length(outSender) > 0) {
      ids <- rbind(cbind(receiver, outSender), cbind(outSender, receiver))
      res <- apply_two_path_update(res, ids, replace, old_value, transformer_fn)
    }
    return(res)
  } else {
    old_value <- sign(network2[sender, receiver])
    if (old_value == replace) {
      return(res)
    }
    temp <- network1[sender, ]
    temp[c(sender, receiver)] <- 0
    outSender <- which(temp > 0)
    if (length(outSender) > 0) {
      ids <- rbind(cbind(outSender, receiver), cbind(receiver, outSender))
      res <- apply_two_path_update(res, ids, replace, old_value, transformer_fn)
    }
    return(res)
  }
}

# four --------------------------------------------------------------------
#' init stat matrix four using cache: Closure of three-paths (i->k<-j->l)
#'
#' @param effect_fun function with additional parameters transformer_fn, is_two_mode
#' @param network matrix n1*n2
#' @param window NULL|numeric size of the window
#' @param n1 integer nrow(network)
#' @param n2 integer ncol(network)
#'
#' @return list
#'   cache matrix numeric n1*n1
#'   stat matrix numeric n1*n2
#' @noRd
#' @export
#'
#' @examples
#' \donttest{
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
#' effectFUN <- function(is_two_mode = FALSE, transformer_fn = sqrt) {
#'   NULL
#' }
#' init_DyNAM_choice.four(effectFUN, network, NULL, 5, 5)
#' }
init_DyNAM_choice.four <- function(
  effect_fun,
  network,
  window,
  n1,
  n2,
  ...
) {
  # return zero-matrix if network is without edges
  if (all(network == 0)) {
    return(list(cache = network, stat = network))
  }
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])

  # if (anyNA(network)) network[is.na(network)] <- 0
  # has window or is empty initialize empty
  if (!is.null(window) && !is.infinite(window)) {
    return(list(
      cache = matrix(
        0,
        nrow = n1,
        ncol = n2,
        dimnames = list(
          sprintf("Actor %d", 1:n1),
          sprintf("Actor %d", 1:n2)
        )
      ),
      stat = matrix(
        forceAndCall(1, funApply, 0),
        nrow = n1,
        ncol = n2,
        dimnames = list(
          sprintf("Actor %d", 1:n1),
          sprintf("Actor %d", 1:n2)
        )
      )
    ))
  }
  # always weighted
  network <- sign(network)
  # we don't consider self-connecting edges which may appears in one-mode models
  if (!is_two_mode) {
    diag(network) <- 0
  }

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
  dimnames(stat) <- list(
    sprintf("Actor %d", 1:n1),
    sprintf("Actor %d", 1:n2)
  )
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
#' @param is_two_mode logical
#' @param transformer_fn function to apply to the stat
#'
#' @return list:
#'   cache matrix numeric size n1 * n2,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases four
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
#'   3, 5, 2,
#'   cache,
#'   is_two_mode = TRUE,
#'   transformer_fn = identity
#' )
#' }
update_DyNAM_choice_four <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  # init res
  res <- list(cache = NULL, changes = NULL)

  # get old value, always weighted
  replace2 <- sign(replace)
  old_value <- sign(network[sender, receiver])

  # Check if old value has changed
  if (is.na(old_value) && is.na(replace2)) {
    return(res)
  } else if (!is.na(old_value) && !is.na(replace2) && old_value == replace2) {
    return(res)
  }
  if (is.na(old_value)) {
    old_value <- 0
  }
  if (is.na(replace)) {
    replace <- 0
  }
  if (!is_two_mode && sender == receiver) {
    return(res)
  }
  # CALCULATE CHANGE

  # If isIncrease is 1, then the number of edges just from zero to nonzero,
  #  otherwise it is the other way around.
  isIncrease <- sign(replace)
  # Use the new network
  if (anyNA(network)) {
    network[is.na(network)] <- 0
  }
  network[sender, receiver] <- replace
  network <- 1 * (network > 0)
  if (!is_two_mode) {
    diag(network) <- 0
  }

  # consider all chain i->k<-j->l
  # consider the chains in which the new tie is in position j->l.
  # we consider all possible two-path i->k<-j
  # Here sender = j and receiver = l
  temp <- network[sender, ]
  temp[receiver] <- 0 # don't consider the cases with l = k
  twoPathFromSenderValues <- drop(temp %*% t(network))
  twoPathFromSenderValues[sender] <- 0 # do not consider the cases with j = i
  twoPathFromSender <- which(twoPathFromSenderValues > 0)
  twoPathFromSenderValues <- twoPathFromSenderValues[twoPathFromSender]
  # consider the chains in which the new tie is in position i->k.
  # we consider all possible two-path k<-j->l
  # here sender = i and receiver = k
  temp <- t(network)[receiver, ]
  temp[sender] <- 0 # don't consider the cases with j = i
  twoPathFromReceiverValues <- drop(temp %*% network)
  twoPathFromReceiverValues[receiver] <- 0 # don't consider the cases with k = l
  twoPathFromReceiver <- which(twoPathFromReceiverValues > 0)
  twoPathFromReceiverValues <- twoPathFromReceiverValues[twoPathFromReceiver]
  # consider the chains in which the new tie is in position k<-j.
  # we consider all possible i and l
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
        node1 = twoPathFromSender,
        node2 = receiver,
        replace = (2 * isIncrease - 1) * twoPathFromSenderValues
      )
    },
    if (length(twoPathFromReceiver) > 0) {
      cbind(
        node1 = sender,
        node2 = twoPathFromReceiver,
        replace = (2 * isIncrease - 1) * twoPathFromReceiverValues
      )
    },
    if (nrow(inOutCombinations) > 0) {
      cbind(
        node1 = inOutCombinations[, 1],
        node2 = inOutCombinations[, 2],
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
    changes[, "replace"] <-
      cache[cbind(changes[, "node1"], changes[, "node2"])] +
      changes[, "replace"]

    cache[cbind(changes[, "node1"], changes[, "node2"])] <- changes[, "replace"]
    # res$changes <- changes
    changes[, "replace"] <- forceAndCall(
      1,
      transformer_fn,
      ifelse(changes[, "replace"] >= 0, changes[, "replace"], 0)
    )
  }

  return(list(cache = cache, changes = changes))
}


# Structural and attribute effects ---------------------------------------------
# tertius ----------------------------------------------------------------
#' @export
init_DyNAM_choice.tertius <- function(
  effect_fun,
  network,
  attribute,
  window,
  n1,
  n2,
  ...
) {
  # See init_DyNAM_choice.indeg: pass a formula-parsed `type` through, inject the
  # alter default only for a type-less direct init call.
  if (!"type" %in% names(formals(effect_fun))) {
    formals(effect_fun) <- c(formals(effect_fun), list(type = "alter"))
  }
  init_REM_choice.tertius(
    effect_fun = effect_fun,
    network = network,
    attribute = attribute,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_DyNAM_choice_tertius <- function(
  network,
  attribute,
  sender = NULL,
  receiver = NULL,
  node = NULL,
  replace,
  cache,
  is_two_mode = FALSE,
  n1 = n1,
  n2 = n2,
  transformer_fn = identity,
  summarizer_fn = function(x) mean(x, na.rm = TRUE),
  type = c("alter", "ego")
) {
  update_REM_choice_tertius(
    network = network,
    attribute = attribute,
    sender = sender,
    receiver = receiver,
    node = node,
    replace = replace,
    cache = cache,
    is_two_mode = is_two_mode,
    n1 = n1,
    n2 = n2,
    transformer_fn = transformer_fn,
    summarizer_fn = summarizer_fn,
    type = type
  )
}

# tertius_diff ----------------------------------------------------------------
#' init stat matrix tertius-diff using cache
#'
#' @param effect_fun function with additional parameters transformer_fn,
#'   summarizer_fn
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
#' attribute <- c(1, 0, 1, 3, 1)
#' effectFUN <- function(transformer_fn = abs,
#'                       summarizer_fn = function(x) median(x, na.rm = TRUE)) {
#'   NULL
#' }
#' init_DyNAM_choice.tertius_diff(effectFUN, network, attribute)
#' }
init_DyNAM_choice.tertius_diff <- function(
  effect_fun,
  network,
  attribute,
  window,
  n1,
  n2,
  ...
) {
  # Get arguments
  params <- formals(effect_fun)
  aggFun <- eval(params[["summarizer_fn"]])
  funApply <- eval(params[["transformer_fn"]]) # applied FUN instead
  is_two_mode <- eval(params[["is_two_mode"]])
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
  stat <- apply(
    X = network,
    MARGIN = 2,
    FUN = function(x) {
      # # inNeighbor of j
      inReceiver <- which(x == 1)
      # # not aggregated if not inNeighbor(j)
      if (length(inReceiver) == 0) {
        return(NA_real_)
      }
      # # apply aggFun to inNeighbor(j)
      forceAndCall(1, aggFun, attribute[inReceiver])
    }
  )

  stat2 <- forceAndCall(1, funApply, outer(attribute, stat, "-"))
  # empty-neighborhood default: a node with no in-neighbor has an undefined
  # aggregate, so it takes the mean of the defined entries (part of the
  # statistic's definition, not attribute imputation).
  if (is_two_mode) {
    stat2[is.na(stat2)] <- mean(stat2, na.rm = TRUE)
  } else {
    diag(stat2) <- NA
    stat2[is.na(stat2)] <- mean(stat2, na.rm = TRUE)
    diag(stat2) <- 0
  }
  # # applied transformer_fn to z_i - agg_{k \in N^-(j)}(z_k)
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
#' @param transformer_fn function to apply to the difference
#' @param summarizer_fn function usa to aggregate in-neighbors attributes
#'
#' @return list:
#'   cache numeric vector size n1
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#' @aliases tertius_diff
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
#' update_DyNAM_choice_tertius_diff(
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
#' update_DyNAM_choice_tertius_diff(
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
update_DyNAM_choice_tertius_diff <- function(
  network,
  attribute,
  sender = NULL,
  receiver = NULL,
  node = NULL,
  replace,
  cache,
  n1 = n1,
  n2 = n2,
  is_two_mode = FALSE,
  transformer_fn = abs,
  summarizer_fn = function(x) mean(x, na.rm = TRUE)
) {
  # utility functions to return third nodes
  third <- function(n, diff = c(node)) {
    setdiff(seq_len(n), diff)
  }
  # init with empty network
  isEmpty <- all(cache == 0)
  needs_default <- anyNA(cache)
  # init res
  res <- list(cache = NULL, changes = NULL)
  # case 1: an update in the network[sende, receiver] <- replace
  if (is.null(node) && !is.null(sender) && !is.null(receiver)) {
    # get old value, always weighted
    replace <- sign(replace)
    old_value <- sign(network[sender, receiver])

    # If the old value of the tie is the same as the replace value
    if (old_value == replace) {
      return(res)
    }

    newValue <- replace - old_value

    if (newValue == 1) {
      # get all in-neighbors of receiver k->j, consider also sender
      inReceiver <- c(which(network[, receiver] > 0), sender)
    } else {
      # delete the k -> j tie, not consider sender
      inReceiver <- setdiff(which(network[, receiver] > 0), sender)
    }

    # change stat
    valChangeCache <- forceAndCall(
      1,
      summarizer_fn,
      if (length(inReceiver) > 0) attribute[inReceiver] else NA
    )

    # changes case 1: all nodes needs to be update the att[i] - cache[j] values
    # if (is_two_mode) seq_len(n2) else third(n1, receiver)
    nodesChange <- if (!is.na(valChangeCache)) receiver else numeric()
    needs_default <- ifelse(
      !needs_default && is.na(valChangeCache),
      TRUE,
      needs_default
    )
    cache[receiver] <- valChangeCache
    changes <- NULL
  }

  # case 2: an update in the attribute[node] <- replace
  if (!is.null(node) && is.null(sender) && is.null(receiver)) {
    # Get old value
    old_value <- attribute[node]

    # If the old value of the tie is the same as the replace value
    if (old_value == replace) {
      return(res)
    }

    # get all out-neighbors of node k->j
    outNode <- which(network[node, ] > 0)

    cache[outNode] <-
      vapply(
        X = outNode,
        FUN = function(x) {
          # # inNeighbor of outNode, excluding Node because has a new value
          inReceiver <- setdiff(which(network[, x] > 0), node)
          # # apply aggFun to inNeighbor(outNode)
          forceAndCall(1, summarizer_fn, c(attribute[inReceiver], replace))
        },
        FUN.VALUE = double(1)
      )

    # changes case 2: it's an update value for node,
    #   then its update is done separately
    nodesChange <- outNode
    isNotMissCache <- which(!is.na(cache))
    if (!is_two_mode) {
      isNotMissCache <- setdiff(isNotMissCache, node)
    }
    changes <- cbind(
      node1 = node,
      node2 = isNotMissCache,
      replace = forceAndCall(
        1,
        transformer_fn,
        (if (is_two_mode) replace else replace[-node]) - cache[isNotMissCache]
      )
    )
  }
  changes <- rbind(
    changes,
    Reduce(
      rbind,
      lapply(
        nodesChange,
        \(x) {
          cbind(
            node1 = if (is_two_mode) seq_len(n1) else third(n1, x),
            node2 = x,
            replace = forceAndCall(
              1,
              transformer_fn,
              (if (is_two_mode) attribute else attribute[-x]) - cache[x]
            )
          )
        }
      )
    )
  )
  # on initialization every entry takes the empty-neighborhood default (the mean
  # of the defined statistic values)
  if (isEmpty) {
    default_cells <- matrix(TRUE, nrow = n1, ncol = n2)
    default_cells[cbind(changes[, "node1"], changes[, "node2"])] <- FALSE
    if (!is_two_mode) {
      diag(default_cells) <- FALSE
    }
    default_val <- mean(changes[, "replace"], na.rm = TRUE)
    changes <- rbind(
      changes,
      cbind(which(default_cells, arr.ind = TRUE), default_val)
    )
  } else if (needs_default) {
    stat <- forceAndCall(1, transformer_fn, outer(attribute, cache, "-"))
    default_cells <- is.na(stat)
    default_cells[cbind(changes[, "node1"], changes[, "node2"])] <- FALSE
    if (!is_two_mode) {
      diag(stat) <- NA
      diag(default_cells) <- FALSE
    }
    default_val <- mean(stat, na.rm = TRUE)
    if (any(default_cells)) {
      changes <- rbind(
        changes,
        cbind(which(default_cells, arr.ind = TRUE), default_val)
      )
    }
  }
  return(list(cache = cache, changes = changes))
}

# Covariate effects -------------------------------------------------------
# alter -------------------------------------------------------------------
#' @export
init_DyNAM_choice.alter <- function(effect_fun, attribute, n1, n2, ...) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])

  # compute stat
  stats <- matrix(attribute, nrow = n1, ncol = n2, byrow = TRUE)
  if (!is_two_mode) {
    diag(stats) <- 0
  }

  return(list(stat = stats))
}

#' @aliases alter
update_DyNAM_choice_alter <- function(
  attribute,
  node,
  replace,
  n1,
  n2,
  is_two_mode = FALSE
) {
  res <- list(changes = NULL)
  # Get old value
  old_value <- attribute[node]

  # If the old value of the tie is the same as the replace value
  if (old_value == replace) {
    return(res)
  }

  # utility functions to return third nodes
  third <- function(n, diff = c(node)) {
    setdiff(seq_len(n), diff)
  }

  if (!is_two_mode) {
    nodesChange <- third(n1, node)
  } else {
    nodesChange <- seq_len(n1)
  }

  # change stat
  res$changes <- cbind(node1 = nodesChange, node2 = node, replace = replace)
  return(res)
}

# The two sides a comparison effect reads. On a two-mode focal the parser
# resolves one written operand into one attribute position per side, so the
# init receives a list; on a one-mode focal both positions name the same
# reference, the object table collapses them, and the init receives the single
# vector it always did -- read here as both sides.
cross_side_attribute <- function(attribute, is_two_mode) {
  if (is.list(attribute)) {
    return(list(ego = attribute[[1]], alter = attribute[[2]]))
  }
  list(ego = attribute, alter = attribute)
}

# A comparison effect's update on a two-mode focal. `att_update` names the
# position whose vector changed -- 1 the sender side, 2 the receiver side -- so
# a sender change rewrites that node's row against every alter, and a receiver
# change that node's column against every ego. The one-mode update cannot make
# this distinction (it emits both orientations for one change) and does not
# need to: there both positions are the same vector.
update_cross_side_same <- function(
  attribute,
  node,
  replace,
  att_update,
  n1,
  n2
) {
  res <- list(changes = NULL)
  ego <- attribute[[1]]
  alter <- attribute[[2]]
  if (identical(att_update, 1L) || identical(att_update, 1)) {
    if (identical(ego[node], replace)) {
      return(res)
    }
    res$changes <- cbind(
      node1 = node,
      node2 = seq_len(n2),
      replace = 1 * (replace == alter)
    )
    return(res)
  }
  if (identical(alter[node], replace)) {
    return(res)
  }
  res$changes <- cbind(
    node1 = seq_len(n1),
    node2 = node,
    replace = 1 * (ego == replace)
  )
  res
}

# The same split for the difference family. Orientation matters here in a way
# it does not for `same`: the statistic is `ego - alter`, so a sender change
# varies the minuend and a receiver change the subtrahend.
update_cross_side_diff <- function(
  attribute,
  node,
  replace,
  att_update,
  n1,
  n2,
  transformer_fn
) {
  res <- list(changes = NULL)
  ego <- attribute[[1]]
  alter <- attribute[[2]]
  if (identical(att_update, 1L) || identical(att_update, 1)) {
    if (identical(ego[node], replace)) {
      return(res)
    }
    res$changes <- cbind(
      node1 = node,
      node2 = seq_len(n2),
      replace = forceAndCall(1, transformer_fn, replace - alter)
    )
    return(res)
  }
  if (identical(alter[node], replace)) {
    return(res)
  }
  res$changes <- cbind(
    node1 = seq_len(n1),
    node2 = node,
    replace = forceAndCall(1, transformer_fn, ego - replace)
  )
  res
}

# same --------------------------------------------------------------------
#' @export
init_DyNAM_choice.same <- function(effect_fun, attribute, ...) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  sides <- cross_side_attribute(attribute, is_two_mode)
  stat <- 1 * outer(sides$ego, sides$alter, "==")
  # A two-mode statistic has no diagonal to exclude: rows and columns index
  # different node sets, so [i, i] is an ordinary dyad.
  if (!is_two_mode) {
    diag(stat) <- 0
  }
  return(list(stat = stat))
}

#' @aliases same
update_DyNAM_choice_same <- function(
  attribute,
  node,
  replace,
  att_update,
  n1,
  n2,
  is_two_mode = FALSE
) {
  if (is_two_mode) {
    return(update_cross_side_same(attribute, node, replace, att_update, n1, n2))
  }
  res <- list(changes = NULL)
  # Get old value
  old_value <- attribute[node]

  # If the old value of the tie is the same as the replace value
  if (old_value == replace) {
    return(res)
  }

  # compute change stat
  changes <- NULL

  oldSameNodes <- setdiff(which(attribute == old_value), node)
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

  if (!is.null(changes)) {
    res$changes <- changes
  }

  return(res)
}

# diff --------------------------------------------------------------------
#' @export
init_DyNAM_choice.diff <- function(effect_fun, attribute, ...) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]]) # applied FUN instead
  sides <- cross_side_attribute(attribute, is_two_mode)
  return(list(
    stat = forceAndCall(
      1,
      funApply,
      outer(sides$ego, sides$alter, "-")
    )
  ))
}

#' @aliases diff
update_DyNAM_choice_diff <- function(
  attribute,
  node,
  replace,
  att_update,
  n1,
  n2,
  is_two_mode = FALSE,
  transformer_fn = abs
) {
  if (is_two_mode) {
    return(update_cross_side_diff(
      attribute,
      node,
      replace,
      att_update,
      n1,
      n2,
      transformer_fn
    ))
  }
  res <- list(changes = NULL)
  # utility functions to return third nodes
  third <- function(n, diff = c(node)) {
    setdiff(seq_len(n), diff)
  }

  # Get old value
  old_value <- attribute[node]

  # If the old value of the tie is the same as the replace value
  if (old_value == replace) {
    return(res)
  }

  # compute change stat
  newDiff <- forceAndCall(1, transformer_fn, replace - attribute[-node])

  res$changes <- rbind(
    cbind(node1 = node, node2 = third(n1), replace = newDiff),
    cbind(node1 = third(n1), node2 = node, replace = newDiff)
  )
  return(res)
}

# sim ---------------------------------------------------------------------
#' @export
init_DyNAM_choice.sim <- function(effect_fun, attribute, ...) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]]) # applied FUN instead
  sides <- cross_side_attribute(attribute, is_two_mode)
  return(list(
    stat = (-1) *
      forceAndCall(1, funApply, outer(sides$ego, sides$alter, "-"))
  ))
}

#' @aliases sim
update_DyNAM_choice_sim <- function(
  attribute,
  node,
  replace,
  att_update,
  n1,
  n2,
  is_two_mode = FALSE,
  transformer_fn = abs
) {
  update_DyNAM_choice_diff(
    attribute = attribute,
    node = node,
    replace = replace,
    att_update = att_update,
    n1 = n1,
    n2 = n2,
    is_two_mode = is_two_mode,
    transformer_fn = function(x) (-1) * transformer_fn(x)
  )
}

# ego alter interaction ---------------------------------------------------

#' @export
init_DyNAM_choice.ego_alter_interaction <- function(
  effect_fun,
  attribute,
  ...
) {
  # Get arguments
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]]) # applied FUN instead
  if (length(attribute) != 2) {
    stop("Interaction ego alter is just define for two attributes")
  }

  attr1 <- attribute[[1]]
  attr2 <- attribute[[2]]
  return(list(stat = forceAndCall(1, funApply, outer(attr1, attr2, "*"))))
}

#' ego alter interaction
#' attribute = list(attr1, attr2) attr1 is ego and attr2 is alter
#' @noRd
update_DyNAM_choice_ego_alter_interaction <- function(
  attribute,
  node,
  replace,
  att_update,
  n1,
  n2,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  if (length(attribute) != 2) {
    stop("Interaction ego alter is just define for two attributes")
  }

  attr1 <- attribute[[1]]
  attr2 <- attribute[[2]]

  res <- list(changes = NULL)
  # utility functions to return third nodes
  third <- function(n, diff = c(node)) {
    setdiff(seq_len(n), diff)
  }

  if (att_update == 1) {
    # Get old value
    old_value <- attr1[node]

    # If the old value of the tie is the same as the replace value
    if (old_value == replace) {
      return(res)
    }

    # compute change stat
    newDiff <- forceAndCall(1, transformer_fn, replace * attr2[-node])

    res$changes <- rbind(
      cbind(node1 = node, node2 = third(n1), replace = newDiff)
    )
    return(res)
  } else if (att_update == 2) {
    # Get old value
    old_value <- attr2[node]

    # If the old value of the tie is the same as the replace value
    if (old_value == replace) {
      return(res)
    }

    # compute change stat
    newDiff <- forceAndCall(1, transformer_fn, attr1[-node] * replace)

    res$changes <- rbind(
      cbind(node1 = third(n1), node2 = node, replace = newDiff)
    )
    return(res)
  }
}

# global ------------------------------------------------------------------
# Make `global` computable in DyNAM choice / choice_coordination by aliasing the
# shared REM-choice global, exactly as the degree
# family aliases REM. The statistic is a global covariate broadcast to the dyad
# (via `to_ego`); it is not identified as a bare main effect in choice, so
# `validate_effects()` rejects it at estimation while `compute_statistics()` still
# produces the column (a design column for interactions / random effects).
#' @export
init_DyNAM_choice.global <- function(effect_fun, attribute, n1, n2, ...) {
  init_REM_choice.global(effect_fun, attribute, n1, n2, ...)
}

update_DyNAM_choice_global <- function(
  attribute,
  replace,
  n1,
  n2,
  is_two_mode = FALSE,
  ...
) {
  update_REM_choice_global(
    attribute = attribute,
    replace = replace,
    n1 = n1,
    n2 = n2,
    is_two_mode = is_two_mode,
    ...
  )
}

# ego ---------------------------------------------------------------------
# Make `ego` computable in DyNAM choice / choice_coordination by aliasing the
# shared REM-choice ego binding, exactly as `global` aliases the REM-choice
# global. The statistic broadcasts the sender's attribute across that sender's
# receivers (the REM `to_ego` expansion); like `global` it is not identified as
# a bare main effect in choice (constant across the receiver alternatives, so it
# cancels in the softmax), so `validate_effects()` rejects it at estimation
# while it stays dispatchable as an interaction operand and a support_constraint
# atom.
#' @export
init_DyNAM_choice.ego <- function(effect_fun, attribute, n1, n2, ...) {
  init_REM_choice.ego(effect_fun, attribute, n1, n2, ...)
}

update_DyNAM_choice_ego <- function(
  attribute,
  node,
  replace,
  n1,
  n2,
  is_two_mode = FALSE
) {
  update_REM_choice_ego(
    attribute = attribute,
    node = node,
    replace = replace,
    n1 = n1,
    n2 = n2,
    is_two_mode = is_two_mode
  )
}
