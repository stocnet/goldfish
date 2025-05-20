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
#' * `defineGroups_interaction()` -> `make_group_interaction()`
#' * `linkEvents(x)` -> `link_events(x)`
#' * `estimate()` -> `estimate_dynam()`, `estimate_rem()` & `estimate_dynami()` 
#' * `examineOutliers()` -> `examine_outliers()`
#' * `examineChangepoints()` -> `examine_changepoints()`
#' * `egoAlterInt()` -> `ego_alter_interaction()`
#' * `nodeTrans()` -> `node_trans()`
#' * `commonSender()` & `commonReceiver()` -> `common_sender()` & `common_receiver()`
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