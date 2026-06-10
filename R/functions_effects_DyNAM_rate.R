# define methods ----------------------------------------------------------
init_DyNAM_rate <- function(effectFun, ...) {
  UseMethod("init_DyNAM_rate", effectFun)
}

# default -----------------------------------------------------------------
#' @export
init_DyNAM_rate.default <- function(
  effectFun,
  network = NULL,
  attribute = NULL,
  window,
  n1,
  n2,
  ...
) {
  init_DyNAM_choice.default(
    effectFun = effectFun,
    network = network,
    attribute = attribute,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

# Structural effects ------------------------------------------------------
# indeg -------------------------------------------------------------------
#' @export
init_DyNAM_rate.indeg <- function(effectFun, network, window, n1, n2, ...) {
  params <- formals(effectFun)
  weighted <- eval(params[["weighted"]])
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])

  if (is_two_mode) {
    stop(
      dQuote("indeg"),
      " effect must not be used with type 'ego' when is a two-mode network",
      call. = FALSE
    )
  }
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = numeric(n2),
      stat = rep(forceAndCall(1, funApply, 0), n1)
    ))
  }
  cache <- .colSums(
    if (weighted) network else network > 0,
    n1,
    n2,
    na.rm = TRUE
  )
  stat <- forceAndCall(1, funApply, cache)
  list(cache = cache, stat = stat)
}

update_DyNAM_rate_indeg <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  n1,
  n2,
  is_two_mode = FALSE,
  weighted = FALSE,
  transformer_fn = identity
) {
  res <- list(cache = NULL, changes = NULL)
  oldValue <- network[sender, receiver]
  if (!weighted) {
    oldValue <- sign(oldValue)
    replace <- sign(replace)
  }
  if (oldValue == replace) {
    return(res)
  }
  cache[receiver] <- cache[receiver] + replace - oldValue
  changes <- cbind(
    node1 = receiver,
    replace = forceAndCall(1, transformer_fn, cache[receiver])
  )
  list(cache = cache, changes = changes)
}

# outdeg ------------------------------------------------------------------
#' @export
init_DyNAM_rate.outdeg <- function(effectFun, network, window, n1, n2, ...) {
  params <- formals(effectFun)
  weighted <- eval(params[["weighted"]])
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])

  if (is_two_mode) {
    stop(
      dQuote("outdeg"),
      " effect must not be used with type 'ego' when is a two-mode network",
      call. = FALSE
    )
  }
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = numeric(n1),
      stat = rep(forceAndCall(1, funApply, 0), n1)
    ))
  }
  cache <- .rowSums(
    if (weighted) network else network > 0,
    n1,
    n2,
    na.rm = TRUE
  )
  stat <- forceAndCall(1, funApply, cache)
  list(cache = cache, stat = stat)
}

update_DyNAM_rate_outdeg <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  n1,
  n2,
  is_two_mode = FALSE,
  weighted = FALSE,
  transformer_fn = identity
) {
  res <- list(cache = NULL, changes = NULL)
  oldValue <- network[sender, receiver]
  if (!weighted) {
    oldValue <- sign(oldValue)
    replace <- sign(replace)
  }
  if (oldValue == replace) {
    return(res)
  }
  cache[sender] <- cache[sender] + replace - oldValue
  changes <- cbind(
    node1 = sender,
    replace = forceAndCall(1, transformer_fn, cache[sender])
  )
  list(cache = cache, changes = changes)
}

# node_trans --------------------------------------------------------------
#' @export
init_DyNAM_rate.node_trans <- function(
  effectFun,
  network,
  window,
  n1,
  n2,
  ...
) {
  params <- formals(effectFun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])

  if (is_two_mode) {
    stop(
      "'node_trans' effect must not be used when is a two-mode network",
      call. = FALSE
    )
  }
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = numeric(n1),
      stat = rep(forceAndCall(1, funApply, 0), n1)
    ))
  }
  network <- sign(network)
  cache <- .rowSums(
    (network %*% network) * network,
    m = n1,
    n = n2,
    na.rm = TRUE
  )
  stat <- forceAndCall(1, funApply, cache)
  list(cache = cache, stat = stat)
}

update_DyNAM_rate_node_trans <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  n1,
  n2,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  res <- list(cache = NULL, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  replace <- sign(replace)
  oldValue <- sign(network[sender, receiver])
  if (oldValue == replace) {
    return(res)
  }

  third <- function(n, diff) setdiff(seq_len(n), diff)

  outNeighSender <- which(network[sender, ] > 0)
  inNeighReceiver <- which(network[, receiver] > 0)
  common_receivers <- intersect(outNeighSender, which(network[receiver, ] > 0))
  common_senders <- intersect(which(network[, sender] > 0), inNeighReceiver)
  brokers <- intersect(outNeighSender, inNeighReceiver)

  senderChanges <- length(common_receivers) + length(brokers)
  changes <- NULL

  if (senderChanges > 0) {
    replaceValues <- (replace - oldValue) * senderChanges + cache[sender]
    changes <- cbind(node1 = sender, replace = replaceValues)
    cache[sender] <- replaceValues
  }

  if (length(common_senders) > 0) {
    replaceValues <- (replace - oldValue) + cache[common_senders]
    changes <- rbind(
      changes,
      cbind(node1 = common_senders, replace = replaceValues)
    )
    cache[common_senders] <- replaceValues
  }

  if (!is.null(changes)) {
    changes[, "replace"] <- forceAndCall(
      1,
      transformer_fn,
      changes[, "replace"]
    )
  }
  list(cache = cache, changes = changes)
}

# tertius -----------------------------------------------------------------
#' @export
init_DyNAM_rate.tertius <- function(
  effectFun,
  network,
  attribute,
  window,
  n1,
  n2,
  ...
) {
  params <- formals(effectFun)
  aggFun <- eval(params[["summarizer_fn"]])
  funApply <- eval(params[["transformer_fn"]])
  is_two_mode <- eval(params[["is_two_mode"]])

  if (is_two_mode) {
    stop(
      "'tertius' effect must not be used with type 'ego' when is a two-mode network",
      call. = FALSE
    )
  }
  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = numeric(n2),
      stat = rep(forceAndCall(1, funApply, 0), n1)
    ))
  }
  network <- sign(unname(network))
  cache <- apply(X = network, MARGIN = 2, FUN = function(x) {
    inReceiver <- which(x == 1)
    if (length(inReceiver) == 0) {
      return(NA_real_)
    }
    forceAndCall(1, aggFun, attribute[inReceiver])
  })
  stat <- forceAndCall(1, funApply, cache)
  if (anyNA(stat)) {
    imputeVal <- mean(stat, na.rm = TRUE)
    stat[is.na(stat)] <- imputeVal
  }
  list(cache = cache, stat = stat)
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
  n1 = n1,
  n2 = n2,
  transformer_fn = identity,
  summarizer_fn = function(x) mean(x, na.rm = TRUE)
) {
  isEmpty <- all(cache == 0)
  isImpute <- anyNA(cache)
  res <- list(cache = NULL, changes = NULL)
  nodesChange <- numeric()

  if (is.null(node) && !is.null(sender) && !is.null(receiver)) {
    replace <- sign(replace)
    oldValue <- sign(network[sender, receiver])
    if (oldValue == replace) {
      return(res)
    }
    newValue <- replace - oldValue
    if (newValue == 1) {
      inReceiver <- c(which(network[, receiver] > 0), sender)
    } else {
      inReceiver <- setdiff(which(network[, receiver] > 0), sender)
    }
    valChangeCache <- forceAndCall(
      1,
      summarizer_fn,
      if (length(inReceiver) > 0) attribute[inReceiver] else NA
    )
    nodesChange <- if (!is.na(valChangeCache)) receiver else numeric()
    isImpute <- ifelse(!isImpute && is.na(valChangeCache), TRUE, isImpute)
    cache[receiver] <- valChangeCache
  }

  if (!is.null(node) && is.null(sender) && is.null(receiver)) {
    oldValue <- attribute[node]
    if (oldValue == replace) {
      return(res)
    }
    outNode <- which(network[node, ] > 0)
    cache[outNode] <- vapply(
      X = outNode,
      FUN = function(x) {
        inReceiver <- setdiff(which(network[, x] > 0), node)
        forceAndCall(1, summarizer_fn, c(attribute[inReceiver], replace))
      },
      FUN.VALUE = double(1)
    )
    nodesChange <- outNode
  }

  changes <- if (length(nodesChange) > 0) {
    cbind(
      node1 = nodesChange,
      replace = forceAndCall(1, transformer_fn, cache[nodesChange])
    )
  } else {
    NULL
  }

  if (isEmpty || isImpute) {
    toImpute <- which(is.na(cache))
    imputeVal <- mean(cache, na.rm = TRUE)
    impute_changes <- if (length(toImpute) > 0) {
      cbind(
        node1 = toImpute,
        replace = forceAndCall(1, transformer_fn, imputeVal)
      )
    } else {
      NULL
    }
    changes <- rbind(changes, impute_changes)
  }
  list(cache = cache, changes = changes)
}

# Covariate effects -------------------------------------------------------
# ego ---------------------------------------------------------------------
#' @export
init_DyNAM_rate.ego <- function(effectFun, attribute, n1, n2, ...) {
  params <- formals(effectFun)
  is_two_mode <- eval(params[["is_two_mode"]])
  if (is_two_mode) {
    stop(
      "'ego' effect must not be used with type 'ego' in a two-mode network",
      call. = FALSE
    )
  }
  list(stat = attribute)
}

update_DyNAM_rate_ego <- function(
  attribute,
  node,
  replace,
  n1,
  n2,
  is_two_mode = FALSE
) {
  res <- list(changes = NULL)
  if (attribute[node] == replace) {
    return(res)
  }
  res$changes <- cbind(node1 = node, replace = replace)
  res
}

# degree (undirected) -----------------------------------------------------
#' @export
init_DyNAM_rate.degree <- function(effectFun, network, window, n1, n2, ...) {
  params <- formals(effectFun)
  weighted <- eval(params[["weighted"]])
  funApply <- eval(params[["transformer_fn"]])

  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = numeric(n1),
      stat = rep(forceAndCall(1, funApply, 0), n1)
    ))
  }
  net <- if (weighted) network else network > 0
  sym_net <- net + t(net)
  cache <- .rowSums(sym_net, n1, n2, na.rm = TRUE)
  stat <- forceAndCall(1, funApply, cache)
  list(cache = cache, stat = stat)
}

update_DyNAM_rate_degree <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  n1,
  n2,
  weighted = FALSE,
  transformer_fn = identity
) {
  res <- list(cache = NULL, changes = NULL)
  oldValue <- network[sender, receiver]
  if (!weighted) {
    oldValue <- sign(oldValue)
    replace <- sign(replace)
  }
  if (oldValue == replace) {
    return(res)
  }
  delta <- replace - oldValue
  cache[sender] <- cache[sender] + delta
  cache[receiver] <- cache[receiver] + delta
  changes <- cbind(
    node1 = c(sender, receiver),
    replace = forceAndCall(1, transformer_fn, cache[c(sender, receiver)])
  )
  list(cache = cache, changes = changes)
}

# triangle (undirected) ---------------------------------------------------
#' @export
init_DyNAM_rate.triangle <- function(effectFun, network, window, n1, n2, ...) {
  params <- formals(effectFun)
  funApply <- eval(params[["transformer_fn"]])

  if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
    return(list(
      cache = numeric(n1),
      stat = rep(forceAndCall(1, funApply, 0), n1)
    ))
  }
  net <- sign(network + t(network))
  cache <- .rowSums((net %*% net) * net, m = n1, n = n2, na.rm = TRUE) / 2L
  stat <- forceAndCall(1, funApply, cache)
  list(cache = cache, stat = stat)
}

update_DyNAM_rate_triangle <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  n1,
  n2,
  transformer_fn = identity
) {
  res <- list(cache = NULL, changes = NULL)
  if (sender == receiver) {
    return(res)
  }
  replace <- sign(replace)
  oldValue <- sign(network[sender, receiver] + network[receiver, sender])
  if (oldValue == replace) {
    return(res)
  }

  common <- intersect(
    which(network[sender, ] > 0 | network[, sender] > 0),
    which(network[receiver, ] > 0 | network[, receiver] > 0)
  )
  affected <- unique(c(sender, receiver, common))
  net_updated <- network
  net_updated[sender, receiver] <- replace
  net_updated[receiver, sender] <- replace
  sym_updated <- sign(net_updated + t(net_updated))
  new_tri <- .rowSums(
    (sym_updated %*% sym_updated) * sym_updated,
    m = n1,
    n = n2,
    na.rm = TRUE
  ) /
    2L
  cache[affected] <- new_tri[affected]
  changes <- cbind(
    node1 = affected,
    replace = forceAndCall(1, transformer_fn, cache[affected])
  )
  list(cache = cache, changes = changes)
}

# global ------------------------------------------------------------------
#' @export
init_DyNAM_rate.global <- function(effectFun, attribute, n1, n2, ...) {
  list(stat = rep(attribute, n1))
}

update_DyNAM_rate_global <- function(
  attribute,
  replace,
  n1,
  n2,
  node = NULL,
  ...
) {
  res <- list(changes = NULL)
  if (isTRUE(attribute == replace)) {
    return(res)
  }
  res$changes <- cbind(node1 = seq_len(n1), replace = replace)
  res
}
