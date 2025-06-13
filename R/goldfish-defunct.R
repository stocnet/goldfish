#' Functions renamed in goldfish 1.7.0
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' goldfish 1.7.0 renamed a number of functions that aims to: align function
#' naming to match tidyverse conventions (using `snake_case`),
#' and foster a common and intuitive interface across the 
#' \href{https://github.com/stocnet}{stocnet}
#' ecosystem, promoting consistency and easier transitions between
#' related packages.
#'
#' * `defineNodes()` -> `make_nodes()`
#' * `defineNetwork()` -> `make_network()`
#' * `defineDependentEvents()` -> `make_dependent_events()`
#' * `defineGlobalAttribute()` -> `make_global_attribute()`
#' * `defineGroups_interaction()` -> `make_groups_interaction()`
#' * `linkEvents(x)` -> `link_events(x)`
#' * `estimate()` -> `estimate_dynam()`, `estimate_rem()` & `estimate_dynami()` 
#' * `examineOutliers()` -> `examine_outliers()`
#' * `examineChangepoints()` -> `examine_changepoints()`
#' * `egoAlterInt()` -> `ego_alter_interaction()`
#' * `nodeTrans()` -> `node_trans()`
#' * `commonSender()` & `commonReceiver()` -> `common_sender()` &
#'   `common_receiver()`
#' * `mixedTrans()`, `mixedCycle()`, `mixedCommonSender()` &
#'   `mixedCommonReceiver()` -> `mixed_trans()`, `mixed_cycle()`,
#'   `mixed_common_sender()` & `mixed_common_receiver()`
#' * `tertiusDiff()` -> `tertius_diff()`
#'
#' \strong{We strongly encourage all users to update their code to use
#' the new function names as soon as possible.} When an old function name is
#' used, it will now issue a **deprecation warning**, guiding you to the
#' recommended new function name.
#'
#' These deprecated functions (old names) are scheduled for removal in a
#' future major release of the package. Updating your code now will ensure
#' future compatibility and allow you to benefit from the improved clarity and
#' consistency.
#'
#' Please consult the individual function documentation for details on their new
#' names and updated usage.
#' @keywords internal
#' @name defunct
#' @aliases NULL
NULL

#' @rdname defunct
#' @export
defineNodes <- function(nodes) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "defineNodes()",
    with = "make_nodes()"
  )
  make_nodes(nodes = nodes)
}

#' @rdname defunct
#' @export
defineNetwork <- function(
    matrix = NULL,
    nodes, nodes2 = NULL, directed = TRUE, envir = environment()
  ) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "defineNetwork()",
    with = "make_network()"
  )
  make_network(
    matrix = matrix, nodes = nodes, nodes2 = nodes2,
    directed = directed, envir = envir
  )
}

#' @rdname defunct
#' @export
defineDependentEvents <- function(
    events, nodes, nodes2 = NULL, default_network = NULL,
    envir = environment()
  ) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "defineDependentEvents()",
    with = "make_dependent_events()"
  )
  make_dependent_events(
    events = events, nodes = nodes, nodes2 = nodes2,
    default_network = default_network,
    envir = envir
  )
}

#' @rdname defunct
#' @export
defineGlobalAttribute <- function(global) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "defineGlobalAttribute()",
    with = "make_global_attribute()"
  )
  make_global_attribute(global = global)
}

#' @rdname defunct
#' @export
defineGroups_interaction <- function(
    records, actors, seed_randomization = NULL,
    progress = getOption("progress")
  ) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "defineGroups_interaction()",
    with = "make_groups_interaction()"
  )
  make_groups_interaction(
    records = records, actors = actors,
    seed_randomization = seed_randomization, progress = progress
  )
}

#' @rdname defunct
#' @export
linkEvents <- function(x, ...) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "linkEvents()",
    with = "link_events()"
  )
  link_events(x, ...)
}

#' @rdname defunct
#' @export
examineOutliers <- function(
    x, method = c("Hampel", "IQR", "Top"), parameter = 3, window = NULL
  ) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "examineOutliers()",
    with = "examine_outliers()"
  )
  examine_outliers(
    x = x, method = method, parameter = parameter, window = window
  )
}

#' @rdname defunct
#' @export
examineChangepoints <- function(
    x, moment = c("mean", "variance"), method = c("PELT", "AMOC", "BinSeg"),
    window = NULL, ...
  ) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "examineChangepoints()",
    with = "examine_changepoints()"
  )
  examine_changepoints(
    x = x, moment = moment, method = method, window = window, ...
  )
}

# Deprecated effect functions ----

#' @rdname defunct
#' @export
egoAlterInt <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "egoAlterInt()",
    with = "ego_alter_interaction()"
  )
  update_DyNAM_choice_ego_alter_interaction(...)
}

#' @rdname defunct
#' @export
nodeTrans <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "nodeTrans()",
    with = "node_trans()"
  )
  update_DyNAM_choice_node_trans(...)
}

#' @rdname defunct
#' @export
commonSender <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "commonSender()",
    with = "common_sender()"
  )
  update_DyNAM_choice_common_sender(...)
}

#' @rdname defunct
#' @export
commonReceiver <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "commonReceiver()",
    with = "common_receiver()"
  )
  update_DyNAM_choice_common_receiver(...)
}

#' @rdname defunct
#' @export
mixedTrans <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "mixedTrans()",
    with = "mixed_trans()"
  )
  update_DyNAM_choice_mixed_trans(...)
}

#' @rdname defunct
#' @export
mixedCycle <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "mixedCycle()",
    with = "mixed_cycle()"
  )
  update_DyNAM_choice_mixed_cycle(...)
}

#' @rdname defunct
#' @export
mixedCommonSender <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "mixedCommonSender()",
    with = "mixed_common_sender()"
  )
  update_DyNAM_choice_mixed_common_sender(...)
}

#' @rdname defunct
#' @export
mixedCommonReceiver <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "mixedCommonReceiver()",
    with = "mixed_common_receiver()"
  )
  update_DyNAM_choice_mixed_common_receiver(...)
}

#' @rdname defunct
#' @export
tertiusDiff <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.7.0",
    what = "tertiusDiff()",
    with = "tertius_diff()"
  )
  update_DyNAM_choice_tertius_diff(...)
}


