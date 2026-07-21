# define methods ----------------------------------------------------------
init_DyNAM_rate <- function(effect_fun, ...) {
  UseMethod("init_DyNAM_rate", effect_fun)
}

# Does this closure read the sender's own side? A rate closure has no `type`
# formal: the statistic is indexed by the sender, so the read is always the ego
# one. The choice and REM inits delegate here carrying a `type`, and only their
# ego variant reads the sender.
reads_ego_side <- function(params) {
  type <- resolved_type(params)
  is.null(type) || identical(type, "ego")
}

# The `type` variant an effect will run with. A formal left at its default still
# holds the whole choice vector (`c("alter", "ego")`), which match.arg resolves
# to the first: comparing it as written gives a length-2 condition, which is an
# error rather than a silent first-element read.
resolved_type <- function(params) {
  eval(params[["type"]])[1]
}

# default -----------------------------------------------------------------
#' @export
init_DyNAM_rate.default <- function(
  effect_fun,
  network = NULL,
  attribute = NULL,
  window,
  n1,
  n2,
  ...
) {
  init_DyNAM_choice.default(
    effect_fun = effect_fun,
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
init_DyNAM_rate.indeg <- function(effect_fun, network, window, n1, n2, ...) {
  params <- formals(effect_fun)
  weighted <- eval(params[["weighted"]])
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])

  # The rate statistic is indexed by the sender, and in-degree counts the ties
  # a sender receives -- which on a two-mode network it never does. The choice
  # and REM inits reuse this computation for both type variants, and their
  # `type = "alter"` read (receiver popularity) is well defined over two modes,
  # so the guard follows the side actually read rather than two-modeness alone.
  if (is_two_mode && reads_ego_side(params)) {
    cli::cli_abort(c(
      "{.fn indeg} cannot be computed on a two-mode network in a rate model.",
      "x" = "It counts the ties the sender receives, and senders of a two-mode
             network are never receivers.",
      "i" = "Use {.code outdeg()} for sender activity, or an {.fn indeg} over a
             one-mode network among the senders."
    ))
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
  old_value <- network[sender, receiver]
  if (!weighted) {
    old_value <- sign(old_value)
    replace <- sign(replace)
  }
  if (old_value == replace) {
    return(res)
  }
  cache[receiver] <- cache[receiver] + replace - old_value
  changes <- cbind(
    node1 = receiver,
    replace = forceAndCall(1, transformer_fn, cache[receiver])
  )
  list(cache = cache, changes = changes)
}

# outdeg ------------------------------------------------------------------
#' @export
init_DyNAM_rate.outdeg <- function(effect_fun, network, window, n1, n2, ...) {
  params <- formals(effect_fun)
  weighted <- eval(params[["weighted"]])
  funApply <- eval(params[["transformer_fn"]])

  # Out-degree reads the sender's own row, which a two-mode network has: sender
  # activity is as well defined over two modes as over one.
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
  old_value <- network[sender, receiver]
  if (!weighted) {
    old_value <- sign(old_value)
    replace <- sign(replace)
  }
  if (old_value == replace) {
    return(res)
  }
  cache[sender] <- cache[sender] + replace - old_value
  changes <- cbind(
    node1 = sender,
    replace = forceAndCall(1, transformer_fn, cache[sender])
  )
  list(cache = cache, changes = changes)
}

# node_trans --------------------------------------------------------------
#' @export
init_DyNAM_rate.node_trans <- function(
  effect_fun,
  network,
  window,
  n1,
  n2,
  ...
) {
  params <- formals(effect_fun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])

  # A transitive path i -> k -> j needs the receivers of a tie to be senders of
  # the next one, so the network must be square over a single mode.
  if (is_two_mode) {
    cli::cli_abort(c(
      "{.fn node_trans} cannot be computed on a two-mode network.",
      "x" = "It counts paths {.code i -> k -> j}, which need one node set on
             both ends of a tie.",
      "i" = "Use {.code four()} for the two-mode closure, or {.fn node_trans}
             over a one-mode network."
    ))
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
  old_value <- sign(network[sender, receiver])
  if (old_value == replace) {
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
    replaceValues <- (replace - old_value) * senderChanges + cache[sender]
    changes <- cbind(node1 = sender, replace = replaceValues)
    cache[sender] <- replaceValues
  }

  if (length(common_senders) > 0) {
    replaceValues <- (replace - old_value) + cache[common_senders]
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
  effect_fun,
  network,
  attribute,
  window,
  n1,
  n2,
  ...
) {
  params <- formals(effect_fun)
  aggFun <- eval(params[["summarizer_fn"]])
  funApply <- eval(params[["transformer_fn"]])
  is_two_mode <- eval(params[["is_two_mode"]])

  # The summary is taken over a node's in-neighbors; the rate statistic needs it
  # for the sender, who on a two-mode network has none. As with `indeg`, the
  # choice and REM inits reuse this for both variants and their
  # `type = "alter"` read is well defined over two modes.
  if (is_two_mode && reads_ego_side(params)) {
    cli::cli_abort(c(
      "{.fn tertius} cannot be computed on a two-mode network in a rate model.",
      "x" = "It summarizes the attribute over the sender's in-neighbors, and
             senders of a two-mode network never receive.",
      "i" = "Use {.fn tertius} over a one-mode network among the senders, or
             {.code tertius_diff()}."
    ))
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
    old_value <- sign(network[sender, receiver])
    if (old_value == replace) {
      return(res)
    }
    newValue <- replace - old_value
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
    old_value <- attribute[node]
    if (old_value == replace) {
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
init_DyNAM_rate.ego <- function(effect_fun, attribute, n1, n2, ...) {
  # The attribute arrives already sliced to the side this position reads, so a
  # two-mode network needs no special case here: `ego` is the sender's own
  # value either way. The stop this replaced guarded the ambiguity of two
  # separate node-set objects, which one nodes table with a mode column
  # dissolves.
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
init_DyNAM_rate.degree <- function(effect_fun, network, window, n1, n2, ...) {
  params <- formals(effect_fun)
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
  old_value <- network[sender, receiver]
  if (!weighted) {
    old_value <- sign(old_value)
    replace <- sign(replace)
  }
  if (old_value == replace) {
    return(res)
  }
  delta <- replace - old_value
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
init_DyNAM_rate.triangle <- function(effect_fun, network, window, n1, n2, ...) {
  params <- formals(effect_fun)
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
  old_value <- sign(network[sender, receiver] + network[receiver, sender])
  if (old_value == replace) {
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
init_DyNAM_rate.global <- function(effect_fun, attribute, n1, n2, ...) {
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
