# define methods ----------------------------------------------------------
# init the statistical matrix
init_REM_choice <- function(effectFun, ...) {
  UseMethod("init_REM_choice", effectFun)
}

# default -----------------------------------------------------------------
#' @export
init_REM_choice.default <- function(
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
# tie ---------------------------------------------------------------------
#' @export
init_REM_choice.tie <- function(effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.tie(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_tie <- function(
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

# inertia -----------------------------------------------------------------
#' @export
init_REM_choice.inertia <- function(effectFun, network, window, n1, n2, ...) {
  init_REM_choice.tie(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_inertia <- function(
  network,
  sender,
  receiver,
  replace,
  weighted = FALSE,
  transformer_fn = identity
) {
  update_REM_choice_tie(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    weighted = weighted,
    transformer_fn = transformer_fn
  )
}

# recip -------------------------------------------------------------------
#' @export
init_REM_choice.recip <- function(effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.recip(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_recip <- function(
  network,
  sender,
  receiver,
  replace,
  is_two_mode = FALSE,
  weighted = FALSE,
  transformer_fn = identity
) {
  update_DyNAM_choice_recip(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
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
  params <- formals(effectFun)
  is_two_mode <- eval(params[["is_two_mode"]])
  type <- eval(params[["type"]])
  if (is_two_mode && type == "ego") {
    stop(
      dQuote("indeg"),
      "effect must not use for type 'ego' (type = 'ego') when is ",
      "a two-mode network (is_two_mode = TRUE) ",
      call. = FALSE
    )
  }
  rate_init <- init_DyNAM_rate.indeg(effectFun, network, window, n1, n2, ...)
  stat <- matrix(
    rate_init$stat,
    nrow = n1,
    ncol = n2,
    byrow = (type == "alter")
  )
  if (!is_two_mode) {
    diag(stat) <- 0
  }
  list(cache = rate_init$cache, stat = stat)
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
  type <- match.arg(type)
  rate_result <- update_DyNAM_rate_indeg(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    cache = cache,
    n1 = n1,
    n2 = n2,
    is_two_mode = is_two_mode,
    weighted = weighted,
    transformer_fn = transformer_fn
  )
  if (is.null(rate_result$changes)) {
    return(list(cache = rate_result$cache, changes = NULL))
  }
  changes <- if (type == "ego") {
    to_ego(rate_result$changes, n2, is_two_mode)
  } else {
    to_alter(rate_result$changes, n1, is_two_mode)
  }
  list(cache = rate_result$cache, changes = changes)
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
  params <- formals(effectFun)
  is_two_mode <- eval(params[["is_two_mode"]])
  type <- eval(params[["type"]])
  if (is_two_mode && type == "alter") {
    stop(
      dQuote("outdeg"),
      "effect must not use for type 'alter' (type = 'alter') when is ",
      "a two-mode network (is_two_mode = TRUE) ",
      call. = FALSE
    )
  }
  rate_init <- init_DyNAM_rate.outdeg(effectFun, network, window, n1, n2, ...)
  stat <- matrix(
    rate_init$stat,
    nrow = n1,
    ncol = n2,
    byrow = (type == "alter")
  )
  if (!is_two_mode) {
    diag(stat) <- 0
  }
  list(cache = rate_init$cache, stat = stat)
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
  type <- match.arg(type)
  rate_result <- update_DyNAM_rate_outdeg(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    cache = cache,
    n1 = n1,
    n2 = n2,
    is_two_mode = is_two_mode,
    weighted = weighted,
    transformer_fn = transformer_fn
  )
  if (is.null(rate_result$changes)) {
    return(list(cache = rate_result$cache, changes = NULL))
  }
  changes <- if (type == "ego") {
    to_ego(rate_result$changes, n2, is_two_mode)
  } else {
    to_alter(rate_result$changes, n1, is_two_mode)
  }
  list(cache = rate_result$cache, changes = changes)
}

# trans -------------------------------------------------------------------
#' @export
init_REM_choice.trans <- function(effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.trans(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_trans <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity,
  history = c('pooled', 'sequential', 'consecutive'),
  eventOrder = 0
) {
  update_DyNAM_choice_trans(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    cache = cache,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn,
    history = history,
    eventOrder = 0
  )
}

# cycle -------------------------------------------------------------------
#' @export
init_REM_choice.cycle <- function(effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.cycle(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_cycle <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity,
  history = c('pooled', 'sequential', 'consecutive'),
  eventOrder = 0
) {
  update_DyNAM_choice_cycle(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    cache = cache,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn,
    history = history,
    eventOrder = 0
  )
}

# common receiver ---------------------------------------------------------
#' @export
init_REM_choice.common_receiver <- function(
  effectFun,
  network,
  window,
  n1,
  n2,
  ...
) {
  init_DyNAM_choice.common_receiver(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_common_receiver <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  update_DyNAM_choice_common_receiver(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    cache = cache,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}

# common sender -----------------------------------------------------------
#' @export
init_REM_choice.common_sender <- function(
  effectFun,
  network,
  window,
  n1,
  n2,
  ...
) {
  init_DyNAM_choice.common_sender(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_common_sender <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  update_DyNAM_choice_common_sender(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    cache = cache,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}

# mixed_trans --------------------------------------------------------------
#' @export
init_REM_choice.mixed_trans <- function(
  effectFun,
  network,
  window,
  n1,
  n2,
  ...
) {
  init_DyNAM_choice.mixed_trans(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_mixed_trans <- function(
  network,
  sender,
  receiver,
  replace,
  netUpdate,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  update_DyNAM_choice_mixed_trans(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    netUpdate = netUpdate,
    cache = cache,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}

# mixed_cycle --------------------------------------------------------------
#' @export
init_REM_choice.mixed_cycle <- function(
  effectFun,
  network,
  window,
  n1,
  n2,
  ...
) {
  init_DyNAM_choice.mixed_cycle(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_mixed_cycle <- function(
  network,
  sender,
  receiver,
  replace,
  netUpdate,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  update_DyNAM_choice_mixed_cycle(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    netUpdate = netUpdate,
    cache = cache,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}

# mixed common receiver ---------------------------------------------------
#' @export
init_REM_choice.mixed_common_receiver <- function(
  effectFun,
  network,
  window,
  n1,
  n2,
  ...
) {
  init_DyNAM_choice.mixed_common_receiver(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_mixed_common_receiver <- function(
  network,
  sender,
  receiver,
  replace,
  netUpdate,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  update_DyNAM_choice_mixed_common_receiver(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    netUpdate = netUpdate,
    cache = cache,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}

# mixed common sender -----------------------------------------------------
#' @export
init_REM_choice.mixed_common_sender <- function(
  effectFun,
  network,
  window,
  n1,
  n2,
  ...
) {
  init_DyNAM_choice.mixed_common_sender(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_mixed_common_sender <- function(
  network,
  sender,
  receiver,
  replace,
  netUpdate,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  update_DyNAM_choice_mixed_common_sender(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    netUpdate = netUpdate,
    cache = cache,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}

# four --------------------------------------------------------------------
#' @export
init_REM_choice.four <- function(effectFun, network, window, n1, n2, ...) {
  init_DyNAM_choice.four(
    effectFun = effectFun,
    network = network,
    window = window,
    n1 = n1,
    n2 = n2,
    ...
  )
}

update_REM_choice_four <- function(
  network,
  sender,
  receiver,
  replace,
  cache,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  update_DyNAM_choice_four(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
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
  effectFun,
  network,
  attribute,
  window,
  n1,
  n2,
  ...
) {
  params <- formals(effectFun)
  is_two_mode <- eval(params[["is_two_mode"]])
  type <- eval(params[["type"]])
  if (is_two_mode && type == "ego") {
    stop(
      "'tertius' effect must not use for type 'ego' (type = 'ego') when is ",
      "a two-mode network (is_two_mode = TRUE) ",
      call. = FALSE
    )
  }
  rate_init <- init_DyNAM_rate.tertius(
    effectFun,
    network,
    attribute,
    window,
    n1,
    n2,
    ...
  )
  stat <- matrix(
    rate_init$stat,
    nrow = n1,
    ncol = n2,
    byrow = (type == "alter")
  )
  if (!is_two_mode) {
    diag(stat) <- 0
  }
  list(cache = rate_init$cache, stat = stat)
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
  n1 = n1,
  n2 = n2,
  is_two_mode = FALSE,
  type = c("alter", "ego"),
  transformer_fn = identity,
  summarizer_fn = function(x) mean(x, na.rm = TRUE)
) {
  type <- match.arg(type)
  rate_result <- update_DyNAM_rate_tertius(
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
    summarizer_fn = summarizer_fn
  )
  if (is.null(rate_result$changes)) {
    return(list(cache = rate_result$cache, changes = NULL))
  }
  changes <- if (type == "ego") {
    to_ego(rate_result$changes, n2, is_two_mode)
  } else {
    to_alter(rate_result$changes, n1, is_two_mode)
  }
  list(cache = rate_result$cache, changes = changes)
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
  effectFun,
  network,
  attribute,
  window,
  n1,
  n2,
  ...
) {
  init_DyNAM_choice.tertius_diff(
    effectFun = effectFun,
    network = network,
    attribute = attribute,
    window = window,
    n1 = n1,
    n2 = n2
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
  n1 = n1,
  n2 = n2,
  transformer_fn = abs,
  summarizer_fn = function(x) mean(x, na.rm = TRUE)
) {
  update_DyNAM_choice_tertius_diff(
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
#' init_REM_choice.node_trans(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'     is_two_mode = FALSE, transformer_fn = identity, type = "ego") {
#'   NULL
#' }
#' init_REM_choice.node_trans(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'     is_two_mode = FALSE, transformer_fn = identity, type = "alter") {
#'   NULL
#' }
#' init_REM_choice.node_trans(effectFUN, network, NULL, 5, 5)
#'
#' effectFUN <- function(
#'     is_two_mode = FALSE, transformer_fn = identity, type = "alter") {
#'   NULL
#' }
#' init_REM_choice.node_trans(effectFUN, network, 9, 5, 5)
#' }
init_REM_choice.node_trans <- function(
  effectFun,
  network,
  window,
  n1,
  n2,
  ...
) {
  params <- formals(effectFun)
  is_two_mode <- eval(params[["is_two_mode"]])
  type <- eval(params[["type"]])
  if (is_two_mode) {
    stop(
      "'node_trans' effect must not use ",
      "when is a two-mode network (is_two_mode = TRUE)",
      call. = FALSE
    )
  }
  rate_init <- init_DyNAM_rate.node_trans(
    effectFun,
    network,
    window,
    n1,
    n2,
    ...
  )
  stat <- matrix(
    rate_init$stat,
    nrow = n1,
    ncol = n2,
    byrow = (type == "alter")
  )
  diag(stat) <- 0
  list(cache = rate_init$cache, stat = stat)
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
#' update_REM_choice_node_trans(network, 1, 5, 1, cache, 5, 5, type = "alter")
#' update_REM_choice_node_trans(network, 1, 5, 1, cache, 5, 5, type = "ego")
#' update_REM_choice_node_trans(network, 3, 2, 0, cache, 5, 5, type = "ego")
#' }
update_REM_choice_node_trans <- function(
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
  type <- match.arg(type)
  rate_result <- update_DyNAM_rate_node_trans(
    network = network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    cache = cache,
    n1 = n1,
    n2 = n2,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
  if (is.null(rate_result$changes)) {
    return(list(cache = rate_result$cache, changes = NULL))
  }
  changes <- if (type == "ego") {
    to_ego(rate_result$changes, n2, is_two_mode)
  } else {
    to_alter(rate_result$changes, n1, is_two_mode)
  }
  list(cache = rate_result$cache, changes = changes)
}

# Covariate effects -------------------------------------------------------
# ego ---------------------------------------------------------------------
#' @export
init_REM_choice.ego <- function(effectFun, attribute, n1, n2, ...) {
  params <- formals(effectFun)
  is_two_mode <- eval(params[["is_two_mode"]])
  rate_init <- init_DyNAM_rate.ego(effectFun, attribute, n1, n2, ...)
  stats <- matrix(rate_init$stat, nrow = n1, ncol = n2, byrow = FALSE)
  if (!is_two_mode) {
    diag(stats) <- 0
  }
  list(stat = stats)
}

update_REM_choice_ego <- function(
  attribute,
  node,
  replace,
  n1,
  n2,
  is_two_mode = FALSE
) {
  rate_result <- update_DyNAM_rate_ego(
    attribute = attribute,
    node = node,
    replace = replace,
    n1 = n1,
    n2 = n2,
    is_two_mode = is_two_mode
  )
  if (is.null(rate_result$changes)) {
    return(list(changes = NULL))
  }
  list(changes = to_ego(rate_result$changes, n2, is_two_mode))
}

# alter -------------------------------------------------------------------
#' @export
init_REM_choice.alter <- function(effectFun, attribute, n1, n2, ...) {
  init_DyNAM_choice.alter(
    effectFun = effectFun,
    attribute = attribute,
    n1 = n2,
    n2 = n2,
    ...
  )
}

update_REM_choice_alter <- function(
  attribute,
  node,
  replace,
  n1,
  n2,
  is_two_mode = FALSE
) {
  update_DyNAM_choice_alter(
    attribute = attribute,
    node = node,
    replace = replace,
    n1 = n1,
    n2 = n2,
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
  node,
  replace,
  is_two_mode = FALSE
) {
  update_DyNAM_choice_same(
    attribute = attribute,
    node = node,
    replace = replace,
    is_two_mode = is_two_mode
  )
}

# diff --------------------------------------------------------------------
#' @export
init_REM_choice.diff <- function(effectFun, attribute, ...) {
  init_DyNAM_choice.diff(effectFun = effectFun, attribute = attribute, ...)
}

update_REM_choice_diff <- function(
  attribute,
  node,
  replace,
  n1,
  n2,
  is_two_mode = FALSE,
  transformer_fn = abs
) {
  update_DyNAM_choice_diff(
    attribute = attribute,
    node = node,
    replace = replace,
    is_two_mode = is_two_mode,
    n1 = n1,
    n2 = n2,
    transformer_fn = transformer_fn
  )
}

# sim ---------------------------------------------------------------------
#' @export
init_REM_choice.sim <- function(effectFun, attribute, ...) {
  init_DyNAM_choice.sim(effectFun = effectFun, attribute = attribute, ...)
}

update_REM_choice_sim <- function(
  attribute,
  node,
  replace,
  n1,
  n2,
  is_two_mode = FALSE,
  transformer_fn = abs
) {
  update_DyNAM_choice_sim(
    attribute = attribute,
    node = node,
    replace = replace,
    n1 = n1,
    n2 = n2,
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
  attribute,
  node,
  replace,
  attUpdate,
  n1,
  n2,
  is_two_mode = FALSE,
  transformer_fn = identity
) {
  update_DyNAM_choice_ego_alter_interaction(
    attribute = attribute,
    node = node,
    replace = replace,
    attUpdate = attUpdate,
    n1 = n1,
    n2 = n2,
    is_two_mode = is_two_mode,
    transformer_fn = transformer_fn
  )
}

# global ------------------------------------------------------------------
#' @export
init_REM_choice.global <- function(effectFun, attribute, n1, n2, ...) {
  rate_init <- init_DyNAM_rate.global(effectFun, attribute, n1, n2, ...)
  stat <- matrix(rate_init$stat, nrow = n1, ncol = n2)
  if (n1 == n2) {
    diag(stat) <- 0
  }
  list(stat = stat)
}

update_REM_choice_global <- function(
  attribute,
  replace,
  n1,
  n2,
  is_two_mode = FALSE,
  ...
) {
  rate_result <- update_DyNAM_rate_global(
    attribute = attribute,
    replace = replace,
    n1 = n1,
    n2 = n2,
    ...
  )
  if (is.null(rate_result$changes)) {
    return(list(changes = NULL))
  }
  list(changes = to_ego(rate_result$changes, n2, is_two_mode))
}
