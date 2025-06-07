# define methods ----------------------------------------------------------
# init the statistical matrix
init_REM_choice <- function(effectFun, ...) {
  UseMethod("init_REM_choice", effectFun)
}

# default -----------------------------------------------------------------
#' @export
init_REM_choice.default <- function(
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
# tie ---------------------------------------------------------------------
#' @export
init_REM_choice.tie <- function(effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.tie(
    effectFun = effectFun, network = network,
    window = window, n1 = n1, n2 = n2, ...
  )
}

update_REM_choice_tie <- function(
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
#' @export
init_REM_choice.inertia <- function(effectFun, network, window, n1, n2, ...) {
  init_REM_choice.tie(
    effectFun = effectFun, network = network,
    window = window, n1 = n1, n2 = n2, ...
  )
}

update_REM_choice_inertia <- function(
    network,
    sender, receiver, replace,
    weighted = FALSE, transformer_fn = identity) {
  update_REM_choice_tie(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    weighted = weighted, transformer_fn = transformer_fn
  )
}

# recip -------------------------------------------------------------------
#' @export
init_REM_choice.recip <- function(effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.recip(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

update_REM_choice_recip <- function(
    network,
    sender, receiver, replace,
    is_two_mode = FALSE,
    weighted = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice_recip(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    is_two_mode = is_two_mode,
    weighted = weighted,
    transformer_fn = transformer_fn
  )
}

# indeg -------------------------------------------------------------------
#' init stat matrix indegree using cache
#'
#' @param effectFun function with additional parameters weighted, is_two_mode,
#'   transformer_fn, type
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
#' effectFUN <- function(
#'     weighted = TRUE, is_two_mode = TRUE, transformer_fn = identity) {
#'   NULL
#' }
#' init_REM_choice.indeg(effectFUN, network, NULL, 5, 6)
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
#'     weighted = TRUE, is_two_mode = FALSE, transformer_fn = identity,
#'     type = "ego") {
#'   NULL
#' }
#' init_REM_choice.indeg(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'     weighted = TRUE, is_two_mode = FALSE, transformer_fn = identity,
#'     type = "alter") {
#'   NULL
#' }
#' init_REM_choice.indeg(effectFUN, network, NULL, 5, 5)
#' }
init_REM_choice.indeg <- function(effectFun, network, window, n1, n2, ...) {
  # Get arguments
  params <- formals(effectFun)
  weighted <- eval(params[["weighted"]])
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])
  type <- eval(params[["type"]])

  if (is_two_mode && type == "ego") {
    stop(dQuote("indeg"),
      "effect must not use for type 'ego' (type = 'ego') when is ",
      "a two-mode network (is_two_mode = TRUE) ",
      call. = FALSE
    )
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
    na.rm = TRUE
  )

  # applied transformer_fn instead
  stat <- forceAndCall(1, funApply, cache)

  # return expected n1 * n2 matrix
  # update check that type should be one of "alter" or "ego"
  byRow <- type == "alter"
  stat <- matrix(stat, nrow = n1, ncol = n2, byrow = byRow)
  #
  if (!is_two_mode) diag(stat) <- 0

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
#' @param is_two_mode logical
#' @param weighted logical
#' @param transformer_fn function to apply to the stat
#' @param type character should be 'alter' or 'ego' type
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
#' update_REM_choice_indeg(
#'   network,
#'   1, 2, 3,
#'   cache, 5, 6,
#'   is_two_mode = TRUE, weighted = TRUE, transformer_fn = sqrt, type = "ego"
#' )
#'
#' update_REM_choice_indeg(
#'   network,
#'   1, 2, 3,
#'   cache, 5, 6,
#'   is_two_mode = TRUE, weighted = TRUE, transformer_fn = sqrt, type = "al"
#' )
#' }
update_REM_choice_indeg <- function(
    network,
    sender, receiver, replace, cache,
    n1, n2, is_two_mode = FALSE,
    weighted = FALSE, transformer_fn = identity,
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
  
  # If the old value of the tie is the same as the replace value
  if (oldValue == replace) {
    return(res)
  }
  # update cache for receiver, event/tie
  cache[receiver] <- cache[receiver] + replace - oldValue

  if (type == "alter") {
    # when the cache had change
    if (is_two_mode) {
      other <- seq_len(n1)
    } else {
      other <- setdiff(seq_len(n1), receiver)
    }

    changes <- cbind(
      node1 = other,
      node2 = receiver,
      replace = forceAndCall(1, transformer_fn, cache[receiver])
    )
  } else if (type == "ego") {
    # when the cache had change
    if (is_two_mode) {
      other <- seq_len(n2)
    } else {
      other <- setdiff(seq_len(n2), receiver)
    }

    changes <- cbind(
      node1 = receiver,
      node2 = other,
      replace = forceAndCall(1, transformer_fn, cache[receiver])
    )
  }
  return(list(cache = cache, changes = changes))
}

# outdeg -------------------------------------------------------------------
#' init stat matrix outdegree using cache
#'
#' @param effectFun function with additional parameters weighted, is_two_mode,
#'   transformer_fn, type
#' @param network matrix n1*n2
#' @param window NULL||numeric(1) size of the window,
#'   if not null and not Inf return empty stat and cache
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
#' effectFUN <- function(
#'     weighted = TRUE, is_two_mode = TRUE, transformer_fn = identity) {
#'   NULL
#' }
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
#'     weighted = TRUE, is_two_mode = FALSE, transformer_fn = identity,
#'     type = "ego") {
#'   NULL
#' }
#' init_REM_choice.outdeg(effectFUN, network, 1, 5, 5)
#'
#' effectFUN <- function(
#'     weighted = TRUE, is_two_mode = FALSE, transformer_fn = identity,
#'     type = "alter") {
#'   NULL
#' }
#' init_REM_choice.outdeg(effectFUN, network, NULL, 5, 5)
#' }
init_REM_choice.outdeg <- function(effectFun, network, window, n1, n2, ...) {
  # Get arguments
  params <- formals(effectFun)
  weighted <- eval(params[["weighted"]])
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])
  type <- eval(params[["type"]])

  if (is_two_mode && type == "alter") {
    stop(
      dQuote("outdeg"),
      "effect must not use for type 'alter' (type = 'alter') when is ",
      "a two-mode network (is_two_mode = TRUE) ",
      call. = FALSE
    )
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
    na.rm = TRUE
  )

  # applied transformer_fn instead
  stat <- forceAndCall(1, funApply, cache)

  # return expected n1 * n2 matrix
  # update check that type should be one of "alter" or "ego"
  byRow <- type == "alter"
  stat <- matrix(stat, nrow = n1, ncol = n2, byrow = byRow)
  #
  if (!is_two_mode) diag(stat) <- 0

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
#' @param is_two_mode logical
#' @param weighted logical
#' @param transformer_fn function to apply to the stat
#' @param type character should be 'alter' or 'ego' type
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
#' update_REM_choice_outdeg(
#'   network,
#'   1, 2, 3,
#'   cache, 5, 6,
#'   is_two_mode = TRUE, weighted = TRUE, transformer_fn = sqrt, type = "ego"
#' )
#'
#' update_REM_choice_outdeg(
#'   network,
#'   1, 2, 3,
#'   cache, 5, 6,
#'   is_two_mode = TRUE, weighted = TRUE, transformer_fn = sqrt, type = "al"
#' )
#' }
update_REM_choice_outdeg <- function(
    network,
    sender, receiver, replace, cache,
    n1, n2, is_two_mode = FALSE,
    weighted = FALSE, transformer_fn = identity,
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
  
  # If the old value of the tie is the same as the replace value
  if (oldValue == replace) {
    return(res)
  }

  # update cache for sender, event/tie
  cache[sender] <- cache[sender] + replace - oldValue

  if (type == "alter") {
    # when the cache had change
    if (is_two_mode) {
      other <- seq_len(n1)
    } else {
      other <- setdiff(seq_len(n1), sender)
    }

    changes <- cbind(
      node1 = other,
      node2 = sender,
      replace = forceAndCall(1, transformer_fn, cache[sender])
    )
  } else if (type == "ego") {
    # when the cache had change
    if (is_two_mode) {
      other <- seq_len(n2)
    } else {
      other <- setdiff(seq_len(n2), sender)
    }

    changes <- cbind(
      node1 = sender,
      node2 = other,
      replace = forceAndCall(1, transformer_fn, cache[sender])
    )
  }
  return(list(cache = cache, changes = changes))
}

# trans -------------------------------------------------------------------
#' @export
init_REM_choice.trans <- function(effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.trans(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

update_REM_choice_trans <- function(
    network,
    sender,
    receiver,
    replace, cache,
    is_two_mode = FALSE,
    transformer_fn = identity,
    history = c('pooled','sequential','consecutive'),
    eventOrder = 0) {
  update_DyNAM_choice_trans(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    cache = cache,
    is_two_mode = is_two_mode, transformer_fn = transformer_fn,
    history = history, eventOrder = 0
  )
}

# cycle -------------------------------------------------------------------
#' @export
init_REM_choice.cycle <- function(effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.cycle(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

update_REM_choice_cycle <- function(
    network,
    sender,
    receiver,
    replace, cache,
    is_two_mode = FALSE,
    transformer_fn = identity,
    history = c('pooled','sequential','consecutive'),
    eventOrder = 0) {
  update_DyNAM_choice_cycle(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    cache = cache,
    is_two_mode = is_two_mode, transformer_fn = transformer_fn, history = history, eventOrder = 0
  )
}

# common receiver ---------------------------------------------------------
#' @export
init_REM_choice.common_receiver <- function(
    effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.common_receiver(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

update_REM_choice.common_receiver <- function(
    network,
    sender,
    receiver,
    replace, cache,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice.common_receiver(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    cache = cache,
    is_two_mode = is_two_mode, transformer_fn = transformer_fn
  )
}

# common sender -----------------------------------------------------------
#' @export
init_REM_choice.common_sender <- function(
    effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.common_sender(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

update_REM_choice.common_sender <- function(
    network,
    sender,
    receiver,
    replace, cache,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice.common_sender(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    cache = cache,
    is_two_mode = is_two_mode, transformer_fn = transformer_fn
  )
}

# mixed_trans --------------------------------------------------------------
#' @export
init_REM_choice.mixed_trans <- function(
    effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.mixed_trans(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

update_REM_choice_mixed_trans <- function(
    network,
    sender,
    receiver,
    replace, netUpdate, cache,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice_mixed_trans(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    netUpdate = netUpdate, cache = cache,
    is_two_mode = is_two_mode, transformer_fn = transformer_fn
  )
}

# mixed_cycle --------------------------------------------------------------
#' @export
init_REM_choice.mixed_cycle <- function(
    effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.mixed_cycle(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

update_REM_choice_mixed_cycle <- function(
    network,
    sender,
    receiver,
    replace, netUpdate, cache,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice_mixed_cycle(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    netUpdate = netUpdate, cache = cache,
    is_two_mode = is_two_mode, transformer_fn = transformer_fn
  )
}

# mixed common receiver ---------------------------------------------------
#' @export
init_REM_choice.mixed_common_receiver <- function(
    effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.mixed_common_receiver(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

update_REM_choice_mixed_common_receiver <- function(
    network,
    sender,
    receiver,
    replace, netUpdate, cache,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice_mixed_common_receiver(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    netUpdate = netUpdate, cache = cache,
    is_two_mode = is_two_mode, transformer_fn = transformer_fn
  )
}

# mixed common sender -----------------------------------------------------
#' @export
init_REM_choice.mixed_common_sender <- function(
    effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.mixed_common_sender(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

update_REM_choice_mixed_common_sender <- function(
    network,
    sender,
    receiver,
    replace, netUpdate, cache,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice_mixed_common_sender(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    netUpdate = netUpdate, cache = cache,
    is_two_mode = is_two_mode, transformer_fn = transformer_fn
  )
}

# four --------------------------------------------------------------------
#' @export
init_REM_choice.four <- function(effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.four(
    effectFun = effectFun, network = network,
    window = window,
    n1 = n1, n2 = n2, ...
  )
}

update_REM_choice_four <- function(
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
#' init stat matrix tertius using cache
#'
#' @param effectFun function with additional parameters transformer_fn,
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
#' effectFUN <- function(
#'     type = "alter", is_two_mode = TRUE,
#'     transformer_fn = abs, summarizer_fn = function(x) median(x, na.rm = TRUE)) {
#'   NULL
#' }
#' init_REM_choice.tertius(effectFUN, network, attribute)
#' }
init_REM_choice.tertius <- function(
    effectFun, network, attribute, window, n1, n2, ...) {
  # Get arguments
  params <- formals(effectFun)
  aggFun <- eval(params[["summarizer_fn"]])
  funApply <- eval(params[["transformer_fn"]]) # applied FUN instead
  is_two_mode <- eval(params[["is_two_mode"]])
  type <- eval(params[["type"]])

  if (is_two_mode && type == "ego") {
    stop("'tertius' effect must not use for type 'ego' (type = 'ego') when is ",
      "a two-mode network (is_two_mode = TRUE) ",
      call. = FALSE
    )
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
  cache <- apply(
    X = network, MARGIN = 2,
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
  # applied transformer_fn
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
  if (!is_two_mode) diag(stat) <- 0

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
#' update_REM_choice_tertius(network, attribute,
#'   sender = 2, receiver = 3,
#'   node = NULL,
#'   3,
#'   cache,
#'   n1 = 5, n2 = 6,
#'   transformer_fn = function(x) x^2,
#'   summarizer_fn = function(x) median(x, na.rm = TRUE)
#' )
#'
#' update_REM_choice_tertius(network, attribute,
#'   sender = NULL, receiver = NULL,
#'   node = 3,
#'   3,
#'   cache,
#'   n1 = 5, n2 = 6,
#'   transformer_fn = function(x) x^2,
#'   summarizer_fn = function(x) median(x, na.rm = TRUE)
#' )
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
    is_two_mode = FALSE,
    type = c("alter", "ego"),
    transformer_fn = identity,
    summarizer_fn = function(x) mean(x, na.rm = TRUE)) {
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
    
    # If the old value of the tie is the same as the replace value
    if (oldValue == replace) {
      return(res)
    }

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
      1, summarizer_fn,
      if (length(inReceiver) > 0) attribute[inReceiver] else NA
    )
    # changes case 1: all nodes needs to be update the att[i] - cache[j] values
    # if (is_two_mode) seq_len(n2) else third(n1, receiver)
    nodesChange <- if (!is.na(valChangeCache)) receiver else numeric()
    isImpute <- ifelse(!isImpute && is.na(valChangeCache), TRUE, isImpute)
    cache[receiver] <- valChangeCache
  }

  # case 2: an update in the attribute[node] <- replace
  if (!is.null(node) && is.null(sender) && is.null(receiver)) {
    # Get old value
    oldValue <- attribute[node]
    
    # If the old value of the tie is the same as the replace value
    if (oldValue == replace) {
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

    # changes case 2: is an update value for node,
    #   then its update is done separately
    nodesChange <- outNode
  }
  # changes depending is_two_mode
  if (type == "alter") {
    # when the cache had change
    changes <- Reduce(
      rbind,
      lapply(
        nodesChange,
        \(x) {
          cbind(
            node1 = if (is_two_mode) seq_len(n1) else third(n1, x),
            node2 = x,
            replace = forceAndCall(1, transformer_fn, cache[x])
          )
        }
      )
    )
  } else if (type == "ego") {
    # when the cache had change
    changes <- Reduce(
      rbind,
      lapply(
        nodesChange,
        \(x) {
          cbind(
            node1 = x,
            node2 = if (is_two_mode) seq_len(n1) else third(n1, x),
            replace = forceAndCall(1, transformer_fn, cache[x])
          )
        }
      )
    )
  }
  # when is just initialize it need to change all values to the average
  if (isEmpty || isImpute) {
    toImpute <- which(is.na(cache))
    imputeVal <- mean(cache, na.rm = TRUE)
    # changes depending is_two_mode
    if (type == "alter") {
      #
      changes <- rbind(
        changes,
        Reduce(
          rbind,
          lapply(
            toImpute,
            \(x) {
              cbind(
                node1 = if (is_two_mode) seq_len(n1) else third(n1, x),
                node2 = x,
                replace = forceAndCall(1, transformer_fn, imputeVal)
              )
            }
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
            \(x) {
              cbind(
                node1 = x,
                node2 = if (is_two_mode) seq_len(n1) else third(n1, x),
                replace = forceAndCall(1, transformer_fn, imputeVal)
              )
            }
          )
        )
      )
    }
  }
  return(list(cache = cache, changes = changes))
}
# tertius_diff ----------------------------------------------------------------
#' init stat matrix tertius-diff using cache
#'
#' @param effectFun function with additional parameters transformer_fn,
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
#' init_REM_choice.tertius_diff(effectFUN, network, attribute)
#' }
init_REM_choice.tertius_diff <- function(
    effectFun, network, attribute, window, n1, n2, ...) {
  init_DyNAM_choice.tertius_diff(
    effectFun = effectFun,
    network = network, attribute = attribute,
    window = window,
    n1 = n1, n2 = n2
  )
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
#' update_REM_choice_tertius_diff(
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
#' update_REM_choice_tertius_diff(
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
update_REM_choice_tertius_diff <- function(
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
  update_DyNAM_choice_tertius_diff(
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

#' node trans init
#' number of transitive triangles i->j->k;i->k where node i is embedded.
#' Source node
#' @param effectFun function with additional parameters is_two_mode, transformer_fn,
#'  type, etc.
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
#'     is_two_mode = TRUE, transformer_fn = identity, type = "ego") {
#'   NULL
#' }
#' init_REM_choice.nodeTrans(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'     is_two_mode = FALSE, transformer_fn = identity, type = "ego") {
#'   NULL
#' }
#' init_REM_choice.nodeTrans(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'     is_two_mode = FALSE, transformer_fn = identity, type = "alter") {
#'   NULL
#' }
#' init_REM_choice.nodeTrans(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'     is_two_mode = FALSE, transformer_fn = identity, type = "alter") {
#'   NULL
#' }
#' init_REM_choice.nodeTrans(effectFUN, network, 9, 5, 5)
#' }
init_REM_choice.nodeTrans <- function(effectFun, network, window, n1, n2, ...) {
  # Get arguments
  params <- formals(effectFun)
  is_two_mode <- eval(params[["is_two_mode"]])
  funApply <- eval(params[["transformer_fn"]])
  type <- eval(params[["type"]])

  if (is_two_mode) {
    stop("'nodeTrans' effect must not use ",
      "when is a two-mode network (is_two_mode = TRUE)",
      call. = FALSE
    )
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
  cache <- .rowSums((network %*% network) * network,
    m = n1, n = n2,
    na.rm = TRUE
  )

  stat <- matrix(forceAndCall(1, funApply, cache),
    nrow = n1, ncol = n2, byrow = (type == "alter")
  )
  # if (!is_two_mode)
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
#' @param is_two_mode logical
#' @param transformer_fn function to apply to the stat
#' @param type character should be 'alter' or 'ego' type
#'
#' @return list:
#'   cache numeric vector size n2,
#'   changes NULL || array cbind(node1 = x, node2 = y, replace = z) stat updates
#' @noRd
#'
#' @examples
#' \donttest{
#' cache <- c(0, 0, 1, 1, 0)
#' update_REM_choice_nodeTrans(network, 1, 5, 1, cache, 5, 5, type = "alter")
#' update_REM_choice_nodeTrans(network, 1, 5, 1, cache, 5, 5, type = "ego")
#' update_REM_choice_nodeTrans(network, 3, 2, 0, cache, 5, 5, type = "ego")
#' }
update_REM_choice_nodeTrans <- function(
    network,
    sender,
    receiver,
    replace,
    cache,
    n1, n2,
    is_two_mode = FALSE,
    transformer_fn = identity,
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
  
  # If the old value of the tie is the same as the replace value
  if (oldValue == replace) {
    return(res)
  }

  third <- function(n, diff) {
    setdiff(seq_len(n), diff)
  }

  # compute neighbourhoods for sender and receivers,
  #   (i = sender, j = receiver)
  outNeighSender <- which(network[sender, ] > 0) # N^+(i)
  inNeighReceiver <- which(network[, receiver] > 0) # N^-(j)

  # common_receivers = k: i->j & i->k & j->k => trans(i) += 1
  common_receivers <- intersect(
    outNeighSender,
    which(network[receiver, ] > 0)
  ) # N^+(i) \cap N^+(j)
  # common_senders = l: l->i & l->j & i->j => trans(l) += 1
  common_senders <- intersect(
    which(network[, sender] > 0),
    inNeighReceiver
  ) # N^-(i) \cap N^-(j)
  # brokers = b: i->b & b->j & i->j => trans(i) += 1
  brokers <- intersect(outNeighSender, inNeighReceiver) # N^+(i) \cap N^-(j)

  senderChanges <- length(common_receivers) + length(brokers) # trans(i) += 1

  changes <- NULL
  if (senderChanges > 0) {
    replaceValues <- (replace - oldValue) * senderChanges + cache[sender]
    if (type == "ego") {
      changes <- cbind(
        node1 = sender, node2 = third(n1, sender), replace = replaceValues
      )
    } else {
      changes <- cbind(
        node1 = third(n1, sender), node2 = sender, replace = replaceValues
      )
    }
    cache[sender] <- replaceValues
  }

  if (length(common_senders) > 0) {
    replaceValues <- (replace - oldValue) + cache[common_senders]
    changes <- rbind(
      changes,
      Reduce(
        rbind,
        lapply(
          seq_along(common_senders),
          \(x) {
            if (type == "ego") {
              cbind(
                node1 = common_senders[x],
                node2 = third(n1, common_senders[x]),
                replace = replaceValues[x]
              )
            } else {
              cbind(
                node1 = third(n1, common_senders[x]),
                node2 = common_senders[x],
                replace = replaceValues[x]
              )
            }
          }
        )
      )
    )
    cache[common_senders] <- replaceValues
  }

  if (!is.null(changes)) {
    changes[, "replace"] <- forceAndCall(1, transformer_fn, changes[, "replace"])
    # res$changes <- changes
  }
  return(list(cache = cache, changes = changes))
}

# Covariate effects -------------------------------------------------------
# ego ---------------------------------------------------------------------
#' @export
init_REM_choice.ego <- function(effectFun, attribute, n1, n2, ...) {
  # Get arguments
  params <- formals(effectFun)
  is_two_mode <- eval(params[["is_two_mode"]])

  # compute stat
  stats <- matrix(attribute, nrow = n1, ncol = n2, byrow = FALSE)
  if (!is_two_mode) diag(stats) <- 0

  return(list(stat = stats))
}

update_REM_choice_ego <- function(
    attribute,
    node, replace,
    n1, n2,
    is_two_mode = FALSE) {
  res <- list(changes = NULL)
  # Get old value
  oldValue <- attribute[node]
  
  # If the old value of the tie is the same as the replace value
  if (oldValue == replace) {
    return(res)
  }
  # utility functions to return third nodes
  third <- function(n, diff = c(node)) {
    setdiff(seq_len(n), diff)
  }

  if (!is_two_mode) nodesChange <- third(n1, node) else nodesChange <- seq_len(n2)

  # change stat
  res$changes <- cbind(node1 = node, node2 = nodesChange, replace = replace)
  return(res)
}

# alter -------------------------------------------------------------------
#' @export
init_REM_choice.alter <- function(effectFun, attribute, n1, n2, ...) {
  init_DyNAM_choice.alter(
    effectFun = effectFun, attribute = attribute, n1 = n2, n2 = n2, ...
  )
}

update_REM_choice_alter <- function(
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
#' @export
init_REM_choice.same <- function(effectFun, attribute, ...) {
  init_DyNAM_choice.same(effectFun = effectFun, attribute = attribute, ...)
}

update_REM_choice_same <- function(
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
#' @export
init_REM_choice.diff <- function(effectFun, attribute, ...) {
  init_DyNAM_choice.diff(effectFun = effectFun, attribute = attribute, ...)
}

update_REM_choice_diff <- function(
    attribute, node, replace,
    n1, n2,
    is_two_mode = FALSE,
    transformer_fn = abs) {
  update_DyNAM_choice_diff(
    attribute = attribute,
    node = node, replace = replace,
    is_two_mode = is_two_mode,
    n1 = n1, n2 = n2,
    transformer_fn = transformer_fn
  )
}

# sim ---------------------------------------------------------------------
#' @export
init_REM_choice.sim <- function(effectFun, attribute, ...) {
  init_DyNAM_choice.sim(effectFun = effectFun, attribute = attribute, ...)
}

update_REM_choice_sim <- function(
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
#' @export
init_REM_choice.ego_alter_interaction <- function(effectFun, attribute, ...) {
  init_DyNAM_choice.sim(effectFun = effectFun, attribute = attribute, ...)
}

update_REM_choice_ego_alter_interaction <- function(
    attribute, node, replace,
    attUpdate,
    n1, n2,
    is_two_mode = FALSE,
    transformer_fn = identity) {
  update_DyNAM_choice_ego_alter_interaction(
    attribute = attribute,
    node = node, replace = replace,
    attUpdate = attUpdate,
    n1 = n1, n2 = n2,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}
