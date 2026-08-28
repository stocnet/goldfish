######################### ###
#
# Print and summary functions
# for classes in Goldfish
#
######################### ###

#' Methods for `goldfish` objects.
#'
#' Printing functions for `goldfish` objects.
#'
#' @param x an object of class `result.goldfish`, `summary.result.goldfish`,
#' `nodes.goldfish`, `network.goldfish`, `dependent.goldfish`, or
#' `preprocessed.goldfish`.
#' @param digits minimal number of significant digits, see [print.default()].
#' @param width controls the maximum number of columns on a line used in
#' printing `summary.result.goldfish` and `preprocessed.goldfish`,
#' see  [print.default()].
#' @param complete logical. Indicates whether the parameter coefficients
#' of effects held fixed during estimation (via `offset()`) should be printed.
#' The default value is `FALSE`. _Note:_ applies for objects of class
#' `result.goldfish` and `summary.result.goldfish`.
#' @param compact logical. For objects of class `summary.result.goldfish`,
#' when `TRUE` (the default) a single coefficients table is printed whose row
#' labels are compact term strings and the separate "Effects details" table is
#' omitted; when `FALSE` the "Effects details" table is printed before the
#' coefficients table. The compact labels read `effect/obj·obj2 [args]`:
#' `/` separates the effect from its object(s), the middle dot `·` joins
#' multiple objects, and a single trailing `[ ]` block collects the remaining
#' (comma-separated) arguments. A short legend after the coefficients table
#' keys any opaque argument codes that appear.
#' @param full logical. Indicates whether the complete `matrix`/`data.frame`
#' should be printed. The default value `FALSE`.
#' @param ... further arguments to be passed to the respective `default`
#' method.
#' @name print-method
#' @return Not value, called for printing side effect.
NULL

# Print Goldfish results
# @return prints just the coefficients of the estimated model.
#   See \code{\link{print.summary.result.goldfish}} for a more
#   comprehensible output.
#' @importFrom stats coef
#' @export
#' @rdname print-method
#' @method print result.goldfish
print.result.goldfish <- function(
  x,
  ...,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  complete = FALSE
) {
  inform_if_stale_result(x)
  cat("\nCall:\n")
  print(x$call)
  cat("\n\n")
  if (length(coef(x, complete = complete))) {
    cat("Coefficients:\n")
    print.default(
      format(coef(x, complete = complete), digits = digits),
      print.gap = 2,
      quote = FALSE,
      width = width,
      ...
    )
  } else {
    cat("No coefficients\n")
  }
  cat("\n")
  invisible(x)
}

#' @method summary result.goldfish
#' @export
#' @noRd
summary.result.goldfish <- function(object, ...) {
  abort_if_stale_result(object, "a summary")
  nParams <- object$n_params

  if (is.null(object$names)) {
    object$names <- seq_len(nParams)
  }
  # names <- object$names

  est <- object$parameters
  std.err <- object$standard_errors
  z <- est / std.err
  p <- 2 * (1 - stats::pnorm(abs(z)))

  isFixed <- GetFixed(object)

  if (any(isFixed)) {
    std.err[isFixed] <- NA_real_
    z[isFixed] <- NA_real_
    p[isFixed] <- NA_real_
  }
  # sig <- rep("", nparams)
  # sig[abs(z) > stats::qnorm(1 - 0.05 / 2)] <- "*"
  # sig[abs(z) > stats::qnorm(1 - 0.01 / 2)] <- "**"
  # sig[abs(z) > stats::qnorm(1 - 0.001 / 2)] <- "***"

  # signif <- symnum(pv, corr = false, na = false,
  #                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  #                  symbols = c("***", "**", "*",
  #                              ".", " "))
  #
  coefmat <- cbind(est, std.err, z, p)
  dimnames(coefmat) <- list(
    rownames(object$names),
    c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  )

  object$coef_mat <- coefmat
  object$AIC <- stats::AIC(object)
  object$BIC <- stats::BIC(object)
  class(object) <- "summary.result.goldfish"
  return(object)
  # format.pval()
}

#' @export
#' @rdname print-method
#' @return For objects of class `result.goldfish` and `summary.result.goldfish`
#'  print the estimated coefficients when `complete = FALSE`, otherwise it
#'  includes also the fixed coefficients.
#' For `summary.result.goldfish` print:
#' \item{Effect details:}{a table with additional information of the effects.
#' The information corresponds to the  values of the effects arguments when
#' they are modified and if they where fixed during estimation, see
#' `vignette("goldfish_effects")` for the complete list of arguments, and
#' [estimate] on how to fix coefficients during estimation.}
#' \item{Coefficients:}{a table with the estimated coefficients, their
#'   approximate standard error obtain from the inverse of the negative Fisher
#'   information matrix, z-value and the p-value of the univariate two-tailed
#'   Wald test to test the hypothesis that the parameter is 0.}
#' \item{Convergence and Information Criteria:}{Information about the
#'   convergence of the iterative Newton-Raphson procedure and the score value
#'   in the last iteration. Information criteria as the AIC, BIC and the AIC
#'   corrected for small sample size AICc are reported.}
#' \item{Model and sub_model:}{the values set during estimation.}
print.summary.result.goldfish <- function(
  x,
  ...,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  compact = TRUE,
  complete = FALSE
) {
  nParams <- x$n_params
  aicc <- x$AIC + 2 * nParams * (nParams + 1) / (x$n_events - nParams - 1)
  cat("\nCall:\n")
  print(x$call, width = width, ...)
  cat("\n")
  # cat("Frequencies of alternatives:")
  # print(prop.table(x$freq), digits = digits)
  # cat("\n")
  # print(x$est.stat)

  isFixed <- GetFixed(x)

  if (!complete && any(isFixed)) {
    names <- detail_display_table(x$names[!isFixed, , drop = FALSE])
    coefMat <- x$coef_mat[!isFixed, ]
  } else {
    names <- detail_display_table(x$names)
    coefMat <- x$coef_mat
  }
  # Nothing to detail when every term names the same one object and no flag
  # applies to any of them.
  isDetPrint <- !((ncol(names) == 1) &&
    (length(unique(names[, "Object"])) == 1))

  legendLines <- character(0)
  if (compact) {
    termsFull <- compact_term_strings(names, "console", width = 10000L)
    tmp <- coefMat
    rownames(tmp) <- termsFull
    # An all-fixed fit renders a coefficient table with no rows, so there is no
    # term to measure; `max()` of nothing warns its way to -Inf.
    fullWidth <- if (length(termsFull) > 0) max(nchar(termsFull)) else 0L
    numericWidth <- max(nchar(utils::capture.output(
      stats::printCoefmat(tmp, digits = digits, ...)
    ))) -
      fullWidth
    avail <- max(12L, width - numericWidth)
    terms <- if (fullWidth <= avail) {
      termsFull
    } else {
      compact_term_strings(names, "console", width = avail)
    }
    rownames(coefMat) <- terms
    legendLines <- .compactLegend(terms, termsFull)
  } else if (isDetPrint) {
    cat("\nEffects details:\n")
    # As a matrix: the table is a data.frame, and print.default() would render
    # one as its underlying list of columns.
    print.default(as.matrix(names), quote = FALSE, width = width, ...)
  }

  cat("\nCoefficients:\n")
  stats::printCoefmat(coefMat, digits = digits, width = width, ...)
  if (length(legendLines)) {
    cat("\n")
    writeLines(strwrap(legendLines, width = width, exdent = 2))
  }
  cat("\n")
  rc <- x$convergence$return_code
  if (is.null(rc) || rc == 0L) {
    cat("  Not converged (return code 0)\n")
  } else if (rc == 1L) {
    cat("  Return code 1: gradient close to zero\n")
  } else if (rc == 2L) {
    cat("  Return code 2: step size close to zero (damped)\n")
  }
  scoreRel <- x$convergence$score_rel_norm
  if (is.null(scoreRel) && !is.null(x$convergence$max_abs_score)) {
    scoreRel <- x$convergence$max_abs_score / max(1, abs(x$log_likelihood))
  }
  stepAbs <- x$convergence$max_abs_update
  if (!is.null(scoreRel) && !is.null(stepAbs)) {
    cat(sprintf(
      "    score (rel. norm): %s    step (max|update|): %s\n",
      formatC(scoreRel, format = "e", digits = 2),
      formatC(stepAbs, format = "e", digits = 2)
    ))
  }
  nFree <- x$n_params
  nTotal <- length(x$parameters)
  nFixed <- nTotal - nFree
  cat(
    " ",
    nFree,
    if (nFree != 1) "parameters" else "parameter",
    "estimated",
    if (nFixed > 0) paste0("(", nFixed, " fixed)") else "",
    "\n"
  )
  cat(
    " ",
    paste("Log-Likelihood: ", signif(x$log_likelihood, digits), "\n", sep = "")
  )
  cat(
    " ",
    "AIC: ",
    signif(x$AIC, digits),
    "\n  AICc:",
    signif(aicc, digits),
    "\n  BIC: ",
    signif(x$BIC, digits),
    "\n"
  )
  cat("  model:", dQuote(x$model), "sub_model:", dQuote(x$sub_model), "\n")
  invisible(x)
}

# print nodes.goldfish object
# @param x a nodes.goldfish object
#' @export
#' @method print nodes.goldfish
#' @importFrom utils head
#' @rdname print-method
#' @return For objects of class `nodes.goldfish` print information of the total
#' number of nodes in the object, the number of nodes present at the beginning
#' of preprocessing, a table with the linked attributes with their respective
#' events data frame and a printing of the first rows in the nodes data frame.
#' See [make_nodes()].
# @examples print(structure(data.frame(label = 1:5),
#                 class = c("nodes.goldfish", "data.frame")))
print.nodes.goldfish <- function(x, ..., full = FALSE, n = 6) {
  events <- attr(x, "events")
  dynamicAttr <- attr(x, "dynamic_attributes")
  cat("Number of nodes:", nrow(x), "\n")
  if ("present" %in% names(x)) {
    cat("Number of present nodes:", sum(x$present), "\n")
  }
  if (!is.null(events) && any(events != "")) {
    title <- c("Dynamic attribute(s):", "Linked events")
    mxName <- max(nchar(dynamicAttr), nchar(title[1])) + 4
    cat(
      title[1],
      strrep(" ", mxName - nchar(title[1])),
      title[2],
      "\n",
      sep = ""
    )
    lapply(
      seq_along(events),
      function(x) {
        cat(
          strrep(" ", 2),
          dynamicAttr[x],
          strrep(" ", mxName - nchar(dynamicAttr[x]) - 2),
          events[x],
          "\n"
        )
      }
    )
  }

  cat("\n")
  attributes(x)[c("events", "dynamic_attributes")] <- NULL
  class(x) <- "data.frame"
  # x <- as.data.frame(x)
  if (full) {
    print((x), ...)
  } else {
    cat("First", min(nrow(x), n), "rows\n")
    print(head(x, min(nrow(x), n)), ...)
  }
  invisible(NULL)
}


# stylize print network.goldfish object
#
# @param x a network.goldfish object to print
#' @param n number of rows for \code{data.frame}, and rows and columns for
#' \code{matrix} to be printed.
#' @export
#' @rdname print-method
#' @return For objects of class `network.goldfish` print information of the
#' dimensions of the network, number of ties presented at the beginning of the
#' preprocessing, the nodes data frames linked to it, information about their
#' definition as a one-mode and directed network, linked events data frame to it
#' and a printing of the first rows and columns in the array.
#' See [make_network()].
# @examples print(structure(rep(0, 100), dim = c(10, 10),
#                 class = "network.goldfish"))
print.network.goldfish <- function(x, ..., full = FALSE, n = 6L) {
  nodes <- attr(x, "nodes")
  directed <- attr(x, "directed")
  is_two_mode <- attr(x, "is_two_mode")
  ties <- sum(x > 0, na.rm = TRUE) / ifelse(directed, 1, 2)
  events <- attr(x, "events")
  cat(
    "Dimensions:",
    paste(dim(x), collapse = " "),
    "\nNumber of ties (no weighted):",
    ties,
    "\nNodes set(s):",
    paste(nodes, collapse = " "),
    "\nIt is a",
    ifelse(is_two_mode, "two-mode", "one-mode"),
    "and",
    ifelse(directed, "directed", "undirected"),
    "network\n"
  )

  if (!is.null(events) && any(events != "")) {
    cat("Linked events:", paste(events, collapse = ", "), "\n")
  }

  cat("\n")
  attributes(x)[c("class", "events", "nodes", "directed")] <- NULL
  if (full) {
    print(x)
  } else {
    cat("First", min(c(dim(x), n)), "rows and columns\n")
    if (R.version$major >= "4") {
      print(head(x, c(min(c(nrow(x), n)), min(c(ncol(x), n)))), ...)
    } else {
      print(head(x[, seq(min(c(nrow(x), n)))], min(c(ncol(x), n))), ...)
    }
  }
  invisible(NULL)
}


#' @export
#' @rdname print-method
#' @return For objects of class `dependent.goldfish` print information of the
#'  total number of events in the object, linked nodes set(s),
#'  linked default network
#' and a printing of the first rows in the events data frame.
#' See [make_dependent_events()].
#
# @examples
# print(
#  structure(
#    data.frame(sender = 1:5, receiver = 2:6, time = 1:5, replace = rep(1, 5)),
#    class = c("nodes.goldfish", "data.frame"),
#    nodes = "nodes", default_network = "network"
#  )
# )
print.dependent.goldfish <- function(x, ..., full = FALSE, n = 6) {
  nodes <- attr(x, "nodes")
  default_network <- attr(x, "default_network")
  cat(
    "Number of events:",
    nrow(x),
    "\nNodes set(s):",
    paste(nodes, collapse = " "),
    "\n"
  )
  if (!is.null(default_network) && default_network != "") {
    cat("Default network:", default_network, "\n")
  }

  cat("\n")
  attributes(x)[c("nodes", "default_network", "type")] <- NULL
  class(x) <- "data.frame"
  # x <- as.data.frame(x)
  if (full) {
    print((x), ...)
  } else {
    cat("First", min(nrow(x), n), "rows\n")
    print(head(x, min(nrow(x), n)), ...)
  }
  invisible(NULL)
}

#' @export
#' @rdname print-method
#' @return For objects of class `specification.goldfish` print a single-glance
#'   overview of the model, dependent process, and formulas.
print.specification.goldfish <- function(x, ...) {
  submodels <- names(x$submodels)
  cli::cli_rule(left = "{.cls specification.goldfish}")
  cli::cli_text("Model {.val {x$model}} · sub-model{?s} {.field {submodels}}")

  dep <- x$dependent
  time_span <- if (!is.null(dep$time_span)) {
    paste(format(dep$time_span), collapse = " – ")
  } else {
    "unknown"
  }
  # Prefer the data's mode names over the synthetic side keys; the legacy path
  # carries no mode pair, so its real node-set names stand in.
  nodes_line <- if (dep$is_two_mode) {
    if (!is.null(dep$mode_pair)) {
      sprintf("%s → %s", dep$mode_pair$sender, dep$mode_pair$receiver)
    } else {
      sprintf("%s → %s", dep$nodes, dep$nodes2)
    }
  } else {
    if (!is.null(dep$mode_pair)) dep$mode_pair$sender else dep$nodes
  }
  network_line <- if (!is.null(dep$network) && nzchar(dep$network)) {
    dep$network
  } else {
    NA_character_
  }
  # Flavors nested under the layer: the modeled flavor(s) labeled, the rest
  # listed as state-only. Shown only when the layer carries several distinct
  # flavors -- a lone flavor (e.g. the synthetic key a make_dependent_events()
  # wrapper stamps) is an implementation detail and stays hidden.
  flavors <- dep$flavors
  modeled <- dep$modeled_flavors %||% dep$modeled_flavor
  flavor_bullets <- character(0)
  if (length(flavors) > 1) {
    if (length(modeled) > 0) {
      state_only <- setdiff(flavors, modeled)
      modeled_line <- if (length(modeled) > 1) {
        "Modeled flavors: {.val {modeled}}"
      } else {
        "Modeled flavor: {.val {modeled}}"
      }
      flavor_bullets <- c(" " = modeled_line)
      if (length(state_only) > 0) {
        flavor_bullets <- c(
          flavor_bullets,
          " " = "State-only flavor{?s}: {.val {state_only}}"
        )
      }
    } else {
      flavor_bullets <- c(" " = "Flavors (all modeled): {.val {flavors}}")
    }
  }

  cli::cli_text("")
  cli::cli_text("{.strong Dependent}")
  dep_bullets <- c(
    "*" = "Layer: {.val {dep$layer}}",
    flavor_bullets,
    "*" = "Events: {.val {dep$n_events}}",
    "*" = "Time span: {.val {time_span}}",
    "*" = "Nodes: {.field {nodes_line}}"
  )
  if (!is.na(network_line)) {
    dep_bullets <- c(dep_bullets, "*" = "Network: {.val {network_line}}")
  }
  cli::cli_bullets(dep_bullets)

  cli::cli_text("")
  if (!is.null(x$processes)) {
    print_flavor_processes(x)
  } else {
    formula_dl <- character(0)
    if (!is.null(x$submodels$rate)) {
      rate_str <- deparse1(x$submodels$rate$input_formula)
      formula_dl <- c(formula_dl, Rate = "{.code {rate_str}}")
    }
    if (!is.null(x$submodels$choice)) {
      choice_str <- deparse1(x$submodels$choice$input_formula)
      formula_dl <- c(formula_dl, Choice = "{.code {choice_str}}")
    }
    if (!is.null(x$derived_constraint)) {
      derived_str <- deparse1(x$derived_constraint)
      formula_dl <- c(formula_dl, Derived = "{.code {derived_str}}")
    }
    if (!is.null(x$support_constraint)) {
      support_str <- deparse1(x$support_constraint)
      formula_dl <- c(formula_dl, Support = "{.code {support_str}}")
    }
    cli::cli_dl(formula_dl)
  }

  cli::cli_text("")
  if (isTRUE(x$valid)) {
    cli::cli_alert_success("Specification is valid.")
  } else {
    cli::cli_alert_danger("Specification is not valid.")
  }
  invisible(x)
}

#' @export
#' @rdname print-method
#' @return For objects of class `goldfishJointSpec` print one section
#'   per modeled layer (flavors nested), each formula's fid and separable/coupled
#'   status, and the derived and combined support constraints.
print.goldfishJointSpec <- function(x, ...) {
  map <- x$process_map
  # A base (uncompleted) joint spec has no `completed` column; treat every fid as
  # authored so the print works before and after the completion transform runs.
  if (is.null(map$completed)) {
    map$completed <- rep(FALSE, nrow(map))
  }
  modeled_panel <- x$modeled_panel %||% character(0)
  cli::cli_rule(left = "{.cls goldfishJointSpec}")

  n_proc <- length(x$specifications)
  n_fid <- nrow(map)
  # Separability is the D4 rule (NOT coupled AND layer NOT a modeled panel
  # process), not the negation of `coupled`: a modeled-panel fid completed with a
  # reads-nothing default has `coupled = FALSE` yet stays non-separable, so the
  # two counts are independent facts rather than complements.
  n_coupled <- sum(map$coupled)
  n_sep <- sum(joint_separable(x))
  n_completed <- sum(map$completed)
  cli::cli_text(
    "{n_proc} process{?es} over one shared data object ·
     {n_fid} formula{?s} · {n_coupled} coupled · {n_sep} separable ·
     {n_completed} auto-supplied"
  )

  # Sections are driven by the process_map (labels rendered from it, never
  # parsed back), and the matching specification supplies each fid's formula and
  # constraint text.
  specs_by_focal <- stats::setNames(
    x$specifications,
    vapply(x$specifications, `[[`, character(1), "focal")
  )

  for (lyr in unique(map$layer)) {
    spec <- specs_by_focal[[lyr]]
    lyr_rows <- map[map$layer == lyr, , drop = FALSE]
    cli::cli_text("")
    cli::cli_text("{.strong Layer} {.val {lyr}} — Model {.val {spec$model}}")
    for (fl in unique(lyr_rows$flavor)) {
      keep <- if (is.na(fl)) is.na(lyr_rows$flavor) else lyr_rows$flavor == fl
      if (!is.na(fl)) {
        cli::cli_text("{.strong Flavor} {.val {fl}}")
      }
      print_joint_flavor(
        spec,
        fl,
        lyr_rows[keep, , drop = FALSE],
        modeled_panel = modeled_panel
      )
    }
  }

  cli::cli_text("")
  cli::cli_alert_info(
    "Estimate a multivariate specification with
                       {.fn estimate_dynes}."
  )
  invisible(x)
}

# One flavor's formulas and constraints. Each family line carries its fid and
# whether the fid is separable (reads no modeled panel layer) or coupled; the
# derived flavor constraint and its AND-composition with any user support
# constraint are shown so a reader sees the mask each fid estimates under.
print_joint_flavor <- function(
  spec,
  flavor,
  rows,
  modeled_panel = character(0)
) {
  flavored <- !is.null(spec$processes) && !is.na(flavor)
  submodels <- if (flavored) {
    spec$processes[[flavor]]$submodels
  } else {
    spec$submodels
  }
  completed <- rows$completed %||% rep(FALSE, nrow(rows))
  for (i in seq_len(nrow(rows))) {
    bundle <- submodels[[rows$family[i]]]
    formula_str <- deparse1(bundle$input_formula)
    family_label <- sub("^(.)", "\\U\\1", rows$family[i], perl = TRUE)
    # D4: a fid on a modeled panel layer is never separable, even reading
    # nothing; `coupled` (reads ANOTHER modeled panel layer) is a strict subset.
    status <- if (rows$coupled[i]) {
      "coupled"
    } else if (rows$layer[i] %in% modeled_panel) {
      "non-separable"
    } else {
      "separable"
    }
    if (isTRUE(completed[i])) {
      status <- paste0(status, ", auto-supplied")
    }
    cli::cli_bullets(c(
      "*" = "{.field {family_label}} [fid {rows$fid[i]}, {status}]:
             {.code {formula_str}}"
    ))
  }

  derived <- if (flavored) {
    spec$processes[[flavor]]$derived_constraint
  } else {
    spec$derived_constraint
  }
  user <- spec$support_constraint
  if (!is.null(derived)) {
    cli::cli_bullets(c(" " = "Derived: {.code {deparse1(derived)}}"))
    if (!is.null(user)) {
      combined <- and_compose_constraint(derived, user)
      cli::cli_bullets(c(" " = "Combined: {.code {deparse1(combined)}}"))
    }
  } else if (!is.null(user)) {
    cli::cli_bullets(c(" " = "Support: {.code {deparse1(user)}}"))
  }
  invisible(NULL)
}

# One section per modeled flavor: its rate/choice formulas and its derived
# (and, when combined with a user constraint, combined) support constraint.
print_flavor_processes <- function(x) {
  for (fl in names(x$processes)) {
    proc <- x$processes[[fl]]
    cli::cli_text("{.strong Flavor} {.val {fl}}")
    formula_dl <- character(0)
    if (!is.null(proc$submodels$rate)) {
      rate_str <- deparse1(proc$submodels$rate$input_formula)
      formula_dl <- c(formula_dl, Rate = "{.code {rate_str}}")
    }
    if (!is.null(proc$submodels$choice)) {
      choice_str <- deparse1(proc$submodels$choice$input_formula)
      formula_dl <- c(formula_dl, Choice = "{.code {choice_str}}")
    }
    if (!is.null(proc$derived_constraint)) {
      derived_str <- deparse1(proc$derived_constraint)
      label <- if (is.null(x$support_constraint)) "Constraint" else "Derived"
      formula_dl <- c(
        formula_dl,
        stats::setNames(
          "{.code {derived_str}}",
          label
        )
      )
    }
    cli::cli_dl(formula_dl)
  }
  if (!is.null(x$support_constraint)) {
    support_str <- deparse1(x$support_constraint)
    cli::cli_dl(c(Support = "{.code {support_str}}"))
  }
  invisible(NULL)
}

# A multi-process fit prints one section per flavor, with that flavor's
# sub-models nested inside it: the flavor is the process a reader reasons about,
# and its rate and choice parts are two halves of one decision. Section labels
# are rendered from the process_map rather than pasted from list keys, so a
# layer or flavor name containing a dot or colliding with another cannot be
# mistaken for structure.
#' @export
#' @method print flavored_result.goldfish
#' @noRd
print.flavored_result.goldfish <- function(
  x,
  ...,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  complete = FALSE
) {
  map <- x$process_map
  cli::cli_rule(left = "{.cls flavored_result.goldfish}")
  cli::cli_text(
    "Model {.val {x$model}} · layer {.val {x$layer}} ·
     {length(x$flavors)} flavor{?s}"
  )

  ordered_rows <- flavored_row_order(x)
  for (fl in x$flavors) {
    cli::cli_text("")
    cli::cli_text("{.strong Flavor} {.val {fl}}")
    for (i in ordered_rows[map$flavor[ordered_rows] == fl]) {
      fit <- x$results[[as.character(map$fid[i])]]
      family_label <- sub("^(.)", "\\U\\1", map$family[i], perl = TRUE)
      cli::cli_text("{.field {family_label}}")
      estimates <- stats::coef(fit, complete = complete)
      if (length(estimates) == 0) {
        cli::cli_text("No coefficients")
        next
      }
      # Numeric tables stay on print.default: cli formats prose, not columns.
      print.default(
        format(estimates, digits = digits),
        print.gap = 2,
        quote = FALSE,
        width = width,
        ...
      )
    }
  }

  cli::cli_text("")
  total <- stats::logLik(x)
  cli::cli_text(
    "Total log-likelihood {.val {round(as.numeric(total), digits)}}
     on {attr(total, 'df')} parameter{?s}"
  )
  invisible(x)
}

#' @export
#' @rdname print-method
print.data.goldfish <- function(x, ...) {
  if (!is.environment(x)) {
    return(print_data_goldfish_list(x, ...))
  }
  cat("Goldfish Data Environment\n")
  cat("=========================\n\n")

  all_obj_names <- ls(envir = x, all.names = TRUE)
  obj_names <- all_obj_names[!grepl("^\\.", all_obj_names)]

  if (length(obj_names) == 0) {
    cat("Environment is empty.\n")
    invisible(x)
    return()
  }

  nodeset_names <- mget(
    ".nodeset_names",
    envir = x,
    ifnotfound = list(character(0))
  )$.nodeset_names
  events_names <- mget(
    ".events_names",
    envir = x,
    ifnotfound = list(character(0))
  )$.events_names

  nets_obj <- character(0)
  deps_obj <- character(0)
  global_obj <- character(0)

  categorized <- c(events_names)

  for (name in setdiff(obj_names, categorized)) {
    obj <- get(name, envir = x)
    if (inherits(obj, "nodes.goldfish")) {
      nodeset_names <- c(nodeset_names, name)
    } else if (inherits(obj, "network.goldfish")) {
      nets_obj <- c(nets_obj, name)
    } else if (inherits(obj, "dependent.goldfish")) {
      deps_obj <- c(deps_obj, name)
    } else if (inherits(obj, "global.goldfish")) {
      global_obj <- c(global_obj, name)
    }
  }

  nodeset_names <- unique(nodeset_names)
  categorized <- c(categorized, nodeset_names, nets_obj, deps_obj, global_obj)
  other_obj <- setdiff(obj_names, categorized)

  if (length(nodeset_names) > 0) {
    cat("--- Nodesets ---\n")
    nodes_summary <- data.frame(
      n = vapply(nodeset_names, \(n) nrow(get(n, envir = x)), integer(1)),
      attributes = vapply(
        nodeset_names,
        \(n) {
          obj <- get(n, envir = x)
          ncol(obj) - 1L
        },
        integer(1)
      ),
      linked_events = vapply(
        nodeset_names,
        \(n) {
          obj <- get(n, envir = x)
          ev <- if (inherits(obj, "nodes.goldfish")) {
            attr(obj, "events")
          } else {
            NULL
          }
          if (is.null(ev) || length(ev) == 0) "" else paste(ev, collapse = ", ")
        },
        character(1)
      ),
      row.names = nodeset_names,
      check.names = FALSE
    )
    print(nodes_summary)
    cat("\n")
  }

  if (length(nets_obj) > 0) {
    cat("--- Networks ---\n")
    nets_summary <- data.frame(
      dimensions = vapply(
        nets_obj,
        \(n) paste(dim(get(n, envir = x)), collapse = "x"),
        character(1)
      ),
      nodeset = vapply(
        nets_obj,
        \(n) paste(attr(get(n, envir = x), "nodes"), collapse = ", "),
        character(1)
      ),
      linked_events = vapply(
        nets_obj,
        function(n) {
          ev <- attr(get(n, envir = x), "events")
          if (is.null(ev) || length(ev) == 0) "" else paste(ev, collapse = ", ")
        },
        character(1)
      ),
      directed = vapply(
        nets_obj,
        \(n) attr(get(n, envir = x), "directed"),
        logical(1)
      ),
      two_mode = vapply(
        nets_obj,
        function(n) {
          is_two_mode <- attr(get(n, envir = x), "is_two_mode")
          if (is.null(is_two_mode)) FALSE else is_two_mode
        },
        logical(1)
      ),
      row.names = nets_obj,
      check.names = FALSE
    )
    print(nets_summary)
    cat("\n")
  }

  if (length(deps_obj) > 0) {
    cat("--- Dependent Events ---\n")
    deps_summary <- data.frame(
      n_events = vapply(deps_obj, \(n) nrow(get(n, envir = x)), integer(1)),
      default_network = vapply(
        deps_obj,
        function(n) {
          net <- attr(get(n, envir = x), "default_network")
          if (is.null(net)) "" else net
        },
        character(1)
      ),
      nodeset = vapply(
        deps_obj,
        \(n) paste(attr(get(n, envir = x), "nodes"), collapse = ", "),
        character(1)
      ),
      row.names = deps_obj,
      check.names = FALSE
    )
    print(deps_summary)
    cat("\n")
  }

  if (length(events_names) > 0) {
    cat("--- Events Data Frames ---\n")
    events_summary <- data.frame(
      n_events = vapply(events_names, \(n) nrow(get(n, envir = x)), integer(1)),
      object_type = vapply(
        events_names,
        function(n) {
          cols <- colnames(get(n, envir = x))
          if (all(c("sender", "receiver") %in% cols)) {
            return("dyadic")
          }
          if ("node" %in% cols) {
            return("nodal")
          }
          return("unknown")
        },
        character(1)
      ),
      update_mode = vapply(
        events_names,
        function(n) {
          cols <- colnames(get(n, envir = x))
          if ("increment" %in% cols) {
            return("increment")
          }
          if ("replace" %in% cols) {
            return("replace")
          }
          return("unknown")
        },
        character(1)
      ),
      row.names = events_names,
      check.names = FALSE
    )
    print(events_summary)
    cat("\n")
  }

  if (length(global_obj) > 0) {
    cat("--- Global Attributes ---\n")
    global_summary <- data.frame(
      n_events = vapply(global_obj, \(n) nrow(get(n, envir = x)), integer(1)),
      row.names = global_obj,
      check.names = FALSE
    )
    print(global_summary)
    cat("\n")
  }

  if (length(other_obj) > 0) {
    cat("--- Other Objects ---\n")
    other_summary <- data.frame(
      class = vapply(other_obj, \(n) class(get(n, envir = x))[1], character(1)),
      length = vapply(other_obj, \(n) length(get(n, envir = x)), integer(1)),
      row.names = other_obj,
      check.names = FALSE
    )
    print(other_summary)
    cat("\n")
  }

  invisible(x)
}

# print preprocessed.goldfish
#
# @param x a preprocessed.goldfish object
#' @export
#' @rdname print-method
#
# @examples print(
#   structure(
#     list(formula = dep ~ inertia, dependentStatistics = numeric(20)),
#     class = "preprocessed.goldfish"
#   )
# )
print.preprocessed.goldfish <- function(x, ..., width = getOption("width")) {
  cat("**Preprocess object for the model:**\n")
  print(x$formula)
  cat(" dependent events processed: ", sum(x$is_dependent == 1L), "\n")
  # cat(" Model type:", result$model.type, "\n")
  cat("*The results are available in the following objects:*\n\n")

  textNodes <- paste(
    "A character identifier for the modeled side: a synthetic side key on the",
    "stocnet path, a nodes.goldfish object name on the legacy path"
  )

  description <- data.frame(
    name = c(
      "initial_stats",
      "stat_mat_update",
      "stat_mat_pointer",
      "stat_mat_broadcast",
      "stat_mat_broadcast_pointer",
      "intervals",
      "is_dependent",
      "event_time",
      "event_sender",
      "event_receiver",
      "event_pos",
      "active_sender_init",
      "active_sender_changes",
      "active_dyad_init",
      "active_dyad_changes",
      "active_sender_update",
      "active_dyad_update",
      "active_dyad_encoding",
      "n_dep_events",
      "total_time",
      "avg_active_entity",
      "start_time",
      "end_time",
      "formula",
      "nodes",
      "nodes2"
    ),
    description = c(
      "Initial statistical matrices/vectors for the effects.",
      paste(
        "Numeric matrix (4 x n): flat buffer of statistic updates across all",
        "stored events; rows are node1, node2, effect, replace (0-indexed)."
      ),
      "Integer vector: cumulative column count in stat_mat_update per event.",
      paste(
        "Numeric matrix (4 x m): compact broadcast (constant-value fan-out)",
        "updates; rows are kind, fixed, effect, replace (0-indexed). kind 1",
        "broadcasts over senders (fixed alter), 2 over alters (fixed ego), 3",
        "over all actors."
      ),
      "Integer vector: column count in stat_mat_broadcast per event.",
      "Numeric vector: elapsed time before each event (dep + RC merged).",
      "Integer vector: 1 for dependent events, 0 for right-censored.",
      "Time of the event.",
      "Integer index of the event sender (NA for global RC events).",
      "Integer index of the event receiver (NA for non-dyadic events).",
      "Consecutive integer identifying each event position.",
      "Initial presence vector for mode-1 nodes.",
      "List of composition changes for mode-1 nodes.",
      "Initial presence vector for mode-2 nodes.",
      "List of composition changes for mode-2 nodes.",
      "Presence updates (C-format) for mode-1 nodes; paired pointer suffix.",
      "Presence updates (C-format) for mode-2 nodes; paired pointer suffix.",
      "Encoding of active_dyad: 'alter' (receiver vector) or 'outer' (factors).",
      "Intercept scalar: number of dependent events (rate / REM models).",
      "Intercept scalar: total elapsed time (rate / REM models).",
      "Intercept scalar: average number of active actors (rate / REM models).",
      "Numeric time value of the initial time considered during estimation.",
      "Numeric time value of the final time considered during estimation.",
      "Formula of the model to estimate.",
      paste(textNodes, "(rows/first-mode)"),
      paste(textNodes, "(cols/second-mode)")
    ),
    stringsAsFactors = FALSE
  )

  description$name <- paste0("$", description$name)
  mxName <- max(nchar(description$name)) + 3

  lapply(
    seq_len(nrow(description)),
    function(x) {
      cat(description[x, 1])
      wrap <- strwrap(description[x, 2], width = width)
      wrap[1] <- paste(strrep(" ", mxName - nchar(description[x, 1])), wrap[1])
      wrap <- paste(wrap, collapse = paste0("\n", strrep(" ", mxName + 1)))
      cat(wrap)
      cat("\n")
    }
  )

  invisible(NULL)
}

# Print algorithm_newton.goldfish object
#' @export
#' @rdname print-method
#' @return For objects of class `algorithm_newton.goldfish`, print a summary
#'   of the estimation algorithm options.
print.algorithm_newton.goldfish <- function(x, ...) {
  cat("Estimation Algorithm Options (algorithm_newton.goldfish):\n")
  for (name in names(x)) {
    value <- x[[name]]
    if (is.null(value)) {
      cat(sprintf("  %-25s: NULL\n", name))
    } else if (is.atomic(value) && length(value) == 1) {
      cat(sprintf("  %-25s: %s\n", name, as.character(value)))
    } else if (is.atomic(value) && length(value) > 1 && length(value) <= 5) {
      cat(sprintf(
        "  %-25s: %s\n",
        name,
        paste(as.character(value), collapse = ", ")
      ))
    } else if (is.list(value) || (is.atomic(value) && length(value) > 5)) {
      cat(sprintf(
        "  %-25s: <%s of length %d>\n",
        name,
        class(value)[1],
        length(value)
      ))
    } else {
      cat(sprintf("  %-25s: <%s>\n", name, class(value)[1]))
    }
  }
  invisible(x)
}

# Print preprocessing.goldfish object
#' @export
#' @rdname print-method
#' @return For objects of class `preprocessing.goldfish`, print a summary
#'   of the preprocessing control options.
print.preprocessing.goldfish <- function(x, ...) {
  cat("Preprocessing Control Options (preprocessing.goldfish):\n")
  for (name in names(x)) {
    value <- x[[name]]
    if (is.null(value)) {
      cat(sprintf("  %-20s: NULL\n", name))
    } else if (name == "opportunities_list" && is.list(value)) {
      cat(sprintf("  %-20s: List of %d elements\n", name, length(value)))
    } else if (is.atomic(value) && length(value) == 1) {
      cat(sprintf("  %-20s: %s\n", name, as.character(value)))
    } else if (is.atomic(value) && length(value) > 1 && length(value) <= 5) {
      cat(sprintf(
        "  %-20s: %s\n",
        name,
        paste(as.character(value), collapse = ", ")
      ))
    } else {
      cat(sprintf("  %-20s: <%s>\n", name, class(value)[1]))
    }
  }
  invisible(x)
}

#' @importFrom generics tidy
#' @export
generics::tidy
# tidy <- function(x) UseMethod("tidy")
# # just for testing, don't use because overwrites use in other packages

#' @method tidy result.goldfish
#' @export
tidy.result.goldfish <- function(
  x,
  conf.int = FALSE,
  conf.level = 0.95,
  compact = TRUE,
  complete = FALSE,
  ...
) {
  isFixed <- GetFixed(x)
  coefMat <- summary.result.goldfish(x)$coef_mat
  colnames(coefMat) <- c("estimate", "std.error", "statistic", "p.value")

  if (conf.int) {
    confInterval <- stats::confint(x, level = conf.level)
    colnames(confInterval) <- c("conf.low", "conf.high")
  }

  dispNames <- detail_display_table(x$names)

  if (compact) {
    terms <- term_label(x$names, ".term_export", "export")

    if (!complete) {
      terms <- terms[!isFixed]
    }

    terms <- cbind(term = terms)
  } else {
    # The term names become a column, so the row names are dropped rather than
    # carried: two rows may legitimately share a name (one effect used twice),
    # which a data.frame will not hold.
    terms <- data.frame(
      term = rownames(dispNames),
      dispNames,
      row.names = NULL,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    terms[, "Object"] <- gsub("\\$", " ", terms[, "Object"])

    if (!complete) terms <- terms[!isFixed, ]
  }

  if (complete) {
    result <- cbind(tibble::as_tibble(terms), tibble::as_tibble(coefMat))

    if (conf.int) {
      confIntervalComplete <- matrix(
        NA_real_,
        nrow = length(isFixed),
        ncol = ncol(confInterval),
        dimnames = list(NULL, colnames(confInterval))
      )
      confIntervalComplete[!isFixed, ] <- confInterval

      result <- cbind(result, tibble::as_tibble(confIntervalComplete))
    }
  } else {
    coefMat <- coefMat[!isFixed, ]
    result <- cbind(tibble::as_tibble(terms), tibble::as_tibble(coefMat))

    if (conf.int) result <- cbind(result, tibble::as_tibble(confInterval))
  }

  return(tibble::as_tibble(result))
}

#' @importFrom generics glance
#' @export
generics::glance
# glance <- function(x) UseMethod("glance")
# just for testing, don't use because overwrites use in other packages

#' @method glance result.goldfish
#' @export
glance.result.goldfish <- function(x, ...) {
  with(
    summary(x),
    tibble::tibble(
      # r.squared = r.squared,
      # adj.r.squared = adj.r.squared,
      # sigma = sigma,
      # statistic = fstatistic["value"],
      # p.value = pf(
      #   fstatistic["value"],
      #   fstatistic["numdf"],
      #   fstatistic["dendf"],
      #   lower.tail = FALSE
      # ),
      logLik = as.numeric(logLik(x)),
      AIC = stats::AIC(x),
      BIC = stats::BIC(x),
      # deviance = stats::deviance(x),
      # df.residual = df.residual(x),
      df = x$n_params,
      nobs = x$n_events
    )
  )
}

#' @importFrom generics augment
#' @export
generics::augment

#' Augment the modeled events with per-interval model quantities
#'
#' @description
#' One row per interval of the likelihood — the dependent events and, on a
#' sub-model that has them, the right-censored intervals — with the model's
#' own per-interval quantities beside the event columns.
#'
#' @details
#' Rows are in **interval order**, the order the likelihood ran in, which is
#' what makes the per-interval columns line up with the event they belong to.
#' A right-censored interval carries its time and its log-likelihood
#' contribution, but no sender, receiver or increment, and no `.fitted` or
#' `.resid`: it realizes no outcome, so a fitted outcome probability and its
#' deviance are not defined there.
#'
#' @param x a fitted model of class `"result.goldfish"`.
#' @param ... Additional arguments passed to or from other methods
#'   (currently unused).
#'
#' @return A [tibble::tibble()] with the modeled event columns plus
#'   `censored`, `n_intervals` (how many likelihood intervals were accumulated
#'   into that event's waiting time), `event_log_lik`, and the broom-convention
#'   `.fitted` (the fitted outcome probability, `exp(event_log_lik)`) and
#'   `.resid` (its deviance residual, `-2 * event_log_lik`). A row is
#'   `censored` only where the observation window outlives the last event: it
#'   closes no waiting time, so it has neither a fitted outcome nor a deviance.
#'
#' @examples
#' data("social_evolution")
#' fit <- estimate_dynam(
#'   calls ~ inertia + recip,
#'   sub_model = "choice",
#'   data = social_evolution
#' )
#' augment(fit)
#'
#' @seealso [residuals.result.goldfish()] and [fitted.result.goldfish()] for
#'   the same quantities on their own, and the other types they come in.
#' @method augment result.goldfish
#' @export
augment.result.goldfish <- function(x, ...) {
  # Aborts: the per-event column it appends comes from `interval_log_lik`, so on
  # an old object it would hand back a tibble with a column of NULL-turned-NA
  # rather than the per-event log-likelihood it promises.
  abort_if_stale_result(x, "an augmented event table")
  # The stocnet path carries the modeled dependent events on the result; the
  # legacy path resolves the dependent-events object from the formula LHS name.
  data <- x$dependent_events %||% get(as.character(x$formula[2]))
  class(data) <- "data.frame"
  tib <- tibble::as_tibble(data)

  # One row per dependent event, not per likelihood interval. The censored
  # intervals are not rows of their own: each belongs to the waiting time of
  # the event that follows it, and is accumulated into that event's span. The
  # interleaving this replaces existed because the table carried one row per
  # interval and had to keep them in time order.
  index <- accumulation_index(x)
  span <- if (is.null(index)) {
    rep(1L, nrow(tib))
  } else {
    unname(tabulate(index))
  }

  # Intervals after the last dependent event close no waiting time. They are a
  # censored remainder, and the table carries it as one final row so it stays
  # aligned with `residuals()`, which returns the same shape. Letting the two
  # disagree would reproduce the mis-pairing this change exists to remove.
  remainder <- length(span) > nrow(tib)
  if (remainder) {
    tail_row <- tib[NA_integer_, , drop = FALSE]
    tail_row$time <- utils::tail(x$event_time, 1)
    tib <- rbind(tib, tail_row)
  }
  # `censored`, not `right_censored_event`: it marks the one row that closes no
  # waiting time, which is precisely the row that is NOT an event.
  tib$censored <- c(rep(FALSE, nrow(tib) - remainder), rep(TRUE, remainder))

  if (!is.numeric(tib$time)) {
    tib$time <- as.POSIXct(tib$time)
  }
  # How many likelihood intervals were accumulated into this event's span. It
  # is the only place the interval structure stays visible once every other
  # surface is per event, and it is what lets the two counts a fit reports be
  # reconstructed from this table alone.
  tib$n_intervals <- span
  tib$event_log_lik <- accumulate_over_events(x$interval_log_lik, x)
  # broom's conventions. `.fitted` is the span's density contribution and
  # `.resid` its deviance -- exactly the literature's `f_k` and `D_k`, which
  # are defined over the waiting time rather than over one stored interval. The
  # censored remainder realizes no outcome, so it has neither.
  fitted <- exp(tib$event_log_lik)
  resid <- -2 * tib$event_log_lik
  tib$.fitted <- ifelse(tib$censored, NA_real_, fitted)
  tib$.resid <- ifelse(tib$censored, NA_real_, resid)
  tib
}

#' @export
#' @method augment flavored_result.goldfish
#' @noRd
augment.flavored_result.goldfish <- function(x, ...) {
  # A tidy return, so the identity travels as columns rather than as a list:
  # the per-process tables row-bind and gain `flavor` and `family` appended
  # after the existing columns. That keeps the event columns positionally
  # stable between a single-process fit and a multi-process one, and it is what
  # lets a plot facet on the identity instead of needing a flavored method of
  # its own.
  #
  # The non-tidy methods take the other route -- a list named by process label
  # -- because a vector cannot carry a column. See `?diagnostic-requirements`.
  tables <- lapply(flavored_processes(x), function(process) {
    append_process_identity(augment(process$fit, ...), process)
  })
  do.call(rbind, tables)
}

#' @return The object, invisibly.
#' @rdname diagnose
#' @method print diagnose_outliers
#' @export
print.diagnose_outliers <- function(x, ...) {
  print_diagnose_table(x, x$outlier, "outlier")
}

#' @rdname diagnose
#' @method print diagnose_changepoints
#' @export
print.diagnose_changepoints <- function(x, ...) {
  print_diagnose_table(x, x$cpt, "changepoint")
}

# The header both diagnostic tables share. It reads its counts from the
# `context` metadata rather than sniffing which columns are present, which is
# what let one print method serve two different objects by guessing.
print_diagnose_table <- function(x, flagged, noun) {
  context <- attr(x, "context")
  params <- attr(x, "params")
  n_flagged <- sum(flagged, na.rm = TRUE)
  # `qty()` sits between the noun and the plural marker: cli takes the
  # quantity from the interpolation immediately before the marker, and the
  # noun is a length-1 string, so without it every count reads as singular.
  cli::cli_text(
    "{.strong {n_flagged}} {noun}{cli::qty(n_flagged)}{?s} identified by the
     {.val {params$method}} method."
  )
  # A container's table is several series stacked, each flagged against its own
  # scale, so the count above is a total over them rather than one series'. The
  # same branch `print.test_time` and `print.test_gof` make, told apart by the
  # column the flavored form appends.
  if ("flavor" %in% names(x)) {
    cli::cli_text(
      "Across {length(context$flavor)} flavor{?s} over {length(context$fid)}
       process{?es}, each {noun} flagged within its own process."
    )
  }
  # Which intervals took part is the one thing a reader cannot recover from
  # the table, since the censored rows are present either way.
  if (!is.null(context$n_analyzed) && !is.null(context$n_intervals)) {
    scope <- if (isTRUE(params$include_censored)) {
      "all intervals, right-censored included"
    } else {
      "the dependent intervals"
    }
    cli::cli_text(
      "Computed over {scope}: {context$n_analyzed} of
       {context$n_intervals} interval{?s}."
    )
  }
  # The flagged rows, not the head of the series: the first ten intervals are
  # almost never the interesting ones, and taking both the count and the
  # listing from one column is what keeps the header from disagreeing with
  # what is shown. Nothing flagged prints the header alone -- the full series
  # is in the object either way.
  if (n_flagged > 0) {
    print(tibble::as_tibble(x)[which(flagged), ])
  }
  invisible(x)
}
