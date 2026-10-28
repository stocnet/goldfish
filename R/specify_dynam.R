# A formula written for any model -> a goldfish specification ------------------
#
# `specify_dynam()` is the goldfish writer of the stocnet "one formula, many
# models" convention (infernet's `specify_mrqap()`, `specify_dynam()`, and
# `specify_ergm()`). It reads a formula written for goldfish, for an MRQAP in
# infernet, or for ergm, rewrites each term as its goldfish effect, drops and
# names a term goldfish cannot fit, and builds the specification with
# `make_specification()`. Without a formula it lists every effect goldfish can
# build for a network, as `RSiena::getEffects()` does. It is proposed as the
# successor of `make_specification()`'s formula arguments; see
# `openspec/changes/specify-dynam/`.

#' Specify a goldfish model from a formula written for any model
#'
#' `r lifecycle::badge("experimental")`
#'
#' `specify_dynam()` reads a formula written for goldfish, for an MRQAP in
#' `{infernet}`, or for `{ergm}`, and returns the goldfish model it describes.
#' Its left-hand side names the dependent layer, as in [estimate_dynam()], so
#' one formula can move between models. It is proposed as the successor of
#' [make_specification()]: with `.data`, it returns the same `goldfishSpec`.
#'
#' - A term that goldfish has under another name is rewritten as it, as
#'   ergm's `nodematch("dept")` becomes `same(dept)`, and infernet's
#'   `exchange(friends)` becomes `recip(friends)`.
#' - A term that goldfish defines differently, such as infernet's `sim()`, is
#'   kept and named.
#' - A term that goldfish cannot fit is dropped and named, so that the rest
#'   still fits: an ergm term without a goldfish effect, such as `gwesp()`,
#'   infernet's `mix()` or random intercepts, and, with `.data`, a main effect
#'   that the sub-model cannot identify, such as `ego()` in a choice model.
#'
#' Rewritten and differently read terms are reported with [cli::cli_inform()],
#' and dropped terms in one warning. A name that no model knows is left for
#' [make_specification()], which aborts.
#'
#' @section A table of every effect:
#'   Without a formula, or with `.` as its whole right-hand side, as in
#'   `calls ~ .`, `specify_dynam()` returns a table of every effect that
#'   goldfish can build for `.data` in `model` and `sub_model`, as
#'   `RSiena::getEffects()` does for a Siena data object. Each effect is
#'   written out for the network's layers and node attributes:
#'
#'   - an effect of a network, such as `inertia` or `indeg()`, on its own,
#'     where it reads the dependent layer, and with each other layer, and
#'     with `type = "ego"` where it has a `type`;
#'   - an effect of an attribute, `ego()`, `alter()`, and `same()`, for each
#'     node attribute, and `diff()` and `sim()` for each numeric one;
#'   - `tertius()` and `tertius_diff()` for each layer and numeric attribute;
#'   - `global()` for each global variable.
#'
#'   A label, a name, `active`, or an attribute with a different category for
#'   every node gets no effect. The closures through two layers, such as
#'   `mixed_trans()`, are not listed.
#'   Each row's status is [make_specification()] of that effect alone:
#'   `"fits"`, or `"drops"` with goldfish's reason.
#'
#'   The table is a tibble of class `"goldfishTerms"`, with the columns `term`,
#'   `effect`, `layer`, `attribute`, `status`, and `reason`. Choose rows with
#'   `[` or `dplyr::filter()`, and `formula()` of the rows gives the
#'   specification of those effects.
#'
#' @param formula A two-sided formula, or a list of them named by sub-model,
#'   such as `list(rate = calls ~ 1 + indeg, choice = calls ~ inertia)`.
#'   The left-hand side names the dependent layer, or `.` for `info$focal`.
#'   Without it, or with `.` as its right-hand side, the table of every effect.
#' @param .data A `stocnet` object. Without it, the formula is read and
#'   written for goldfish, but no specification is built.
#' @param input The model the formula was written for: `"dynam"`, `"mrqap"`,
#'   or `"ergm"`. `NULL`, the default, reads it from the class of the formula,
#'   such as `"ergm_formula"`, or else from the names in it.
#' @param model `"DyNAM"` or `"REM"`.
#' @param sub_model The sub-model of a single formula: `"choice"`,
#'   `"choice_coordination"`, `"rate"`, or `"rate_ordered"`. A REM takes
#'   `"rate"` or `"rate_ordered"`.
#' @param support_constraint As in [make_specification()].
#' @return With `.data`, a `goldfishSpec`, as [make_specification()] returns,
#'   whose `record` holds what goldfish makes of each term; `tidy()` returns
#'   it. Without `.data`, a formula of class `c("goldfishFormula", "formula")`,
#'   whose `record` attribute holds the same. Without a formula, a tibble of
#'   class `"goldfishTerms"`, one row for each effect.
#' @seealso [make_specification()], [estimate_dynam()]
#' @export
#' @examples
#' data("social_evolution")
#' # A goldfish formula
#' spec <- specify_dynam(calls ~ inertia + recip + trans, social_evolution)
#' tidy(spec)
#' # An ergm formula, read for goldfish
#' specify_dynam(calls ~ edges + mutual + absdiff("floor") + gwesp(0.5))
#' # Every effect of a choice model, and a specification of some of them
#' effects <- specify_dynam(calls ~ ., social_evolution)
#' effects
#' formula(effects[effects$effect %in% c("inertia", "recip", "same"), ])
specify_dynam <- function(
  formula,
  .data = NULL,
  input = NULL,
  model = c("DyNAM", "REM"),
  sub_model = c("choice", "choice_coordination", "rate", "rate_ordered"),
  support_constraint = NULL
) {
  model <- match.arg(model)
  # A REM is tie-oriented, so its one formula is a rate formula by default.
  if (model == "REM" && missing(sub_model)) {
    sub_model <- "rate"
  }
  sub_model <- match.arg(sub_model)
  if (model == "REM" && sub_model %in% c("choice", "choice_coordination")) {
    cli::cli_abort(c(
      "A REM has no {.val {sub_model}} sub-model.",
      "i" = "Use {.code sub_model = \"rate\"} or {.val rate_ordered}."
    ))
  }
  if (missing(formula)) {
    if (is.null(.data)) {
      cli::cli_abort("{.arg formula} or {.arg .data} must be supplied.")
    }
    return(sd_term_table(.data, quote(.), model, sub_model))
  }
  if (inherits(formula, "formula") && sd_every_term(formula)) {
    if (is.null(.data)) {
      cli::cli_abort(c(
        "A formula of {.code .} asks for every effect, which needs
         {.arg .data}.",
        "i" = "Give the network, e.g. {.code specify_dynam(calls ~ ., data)}."
      ))
    }
    return(sd_term_table(.data, formula[[2]], model, sub_model))
  }
  parts <- if (inherits(formula, "formula")) {
    stats::setNames(list(formula), sub_model)
  } else {
    formula
  }
  sd_check_parts(parts, model)
  layers <- sd_layers(.data)
  read <- lapply(names(parts), function(k) {
    sd_read(parts[[k]], k, input = input, layers = layers)
  })
  names(read) <- names(parts)
  for (k in names(read)) {
    read[[k]]$record <- sd_check_validity(read[[k]], model, k, .data)
  }
  record <- do.call(rbind, lapply(names(read), function(k) {
    cbind(part = k, read[[k]]$record)
  }))
  sd_report(record)
  kept <- lapply(read, function(r) {
    r$record$as[r$record$status %in% c("fits", "adapted", "interpreted") &
                  nzchar(r$record$as)]
  })
  kept <- lapply(kept, function(k) {
    setdiff(unique(unlist(strsplit(k, " + ", fixed = TRUE))), "1")
  })
  empty <- vapply(names(kept), function(k) {
    length(kept[[k]]) == 0 && !read[[k]]$intercept
  }, logical(1))
  if (all(empty)) {
    lost <- record[record$status == "drops", , drop = FALSE]
    cli::cli_abort(c(
      "No term of the formula can be fitted by goldfish.",
      stats::setNames(sprintf("{.code %s} %s", sd_escape(lost$term),
                              sd_escape(lost$reason)),
                      rep("x", nrow(lost))),
      "i" = "Add a term that goldfish fits, such as {.code inertia}."
    ))
  }
  rhs <- lapply(names(read), function(k) {
    terms <- c(if (read[[k]]$intercept) "1", kept[[k]])
    sd_one_sided(terms, environment(parts[[k]]))
  })
  names(rhs) <- names(read)
  lhs <- read[[1]]$lhs
  if (is.null(.data)) {
    out <- stats::as.formula(call("~", lhs, rhs[[1]][[2]]),
                             env = environment(parts[[1]]))
    return(structure(out, class = c("goldfishFormula", "formula"),
                     input = read[[1]]$input, record = record))
  }
  spec <- make_specification(
    rate = rhs$rate %||% rhs$rate_ordered,
    choice = rhs$choice %||% rhs$choice_coordination,
    model = model,
    rate_sub_model = if (is.null(rhs$rate_ordered)) "rate" else "rate_ordered",
    choice_sub_model = if (is.null(rhs$choice_coordination)) {
      "choice"
    } else {
      "choice_coordination"
    },
    layer = sd_layer_name(lhs, .data),
    support_constraint = support_constraint,
    data = .data
  )
  spec$record <- record
  spec$input <- read[[1]]$input
  spec
}

#' @rdname specify_dynam
#' @param x A `goldfishSpec`, or the table of every effect.
#' @param ... Not used.
#' @export
tidy.goldfishSpec <- function(x, ...) {
  record <- x$record
  if (is.null(record)) {
    record <- do.call(rbind, lapply(names(x$submodels), function(k) {
      labels <- attr(stats::terms(x$submodels[[k]]$input_formula),
                     "term.labels")
      data.frame(part = rep(k, length(labels)), term = labels, as = labels,
                 status = rep("fits", length(labels)),
                 reason = rep("", length(labels)))
    }))
  }
  blank <- function(v) ifelse(nzchar(v), v, NA_character_)
  tibble::tibble(part = record$part, term = record$term,
                 status = record$status, as = blank(record$as),
                 reason = blank(record$reason))
}

#' @rdname specify_dynam
#' @param lhs The dependent layer of the formula, as a name or a string.
#'   `NULL`, the default, is the layer the table was made for.
#' @export
formula.goldfishTerms <- function(x, lhs = NULL, ...) {
  lhs <- substitute(lhs)
  if (is.null(lhs)) {
    lhs <- attr(x, "layer")
  }
  if (is.character(lhs)) {
    lhs <- as.name(lhs)
  }
  if (nrow(x) == 0) {
    cli::cli_abort(c(
      "The table has no rows, so there is no effect for the formula.",
      "i" = "Choose at least one row, e.g. {.code x[x$status == \"fits\", ]}."
    ))
  }
  sub_model <- attr(x, "sub_model")
  terms <- c(if (sub_model == "rate") "1", x$term)
  f <- stats::as.formula(call("~", lhs, str2lang(paste(terms,
                                                       collapse = " + "))),
                         env = globalenv())
  specify_dynam(f, attr(x, "data"), input = "dynam", model = attr(x, "model"),
                sub_model = sub_model)
}

#' @rdname specify_dynam
#' @export
tidy.goldfishTerms <- function(x, ...) {
  out <- as.data.frame(x)
  for (a in c("layer", "model", "sub_model", "data")) {
    attr(out, a) <- NULL
  }
  tibble::as_tibble(out)
}

#' @export
print.goldfishTerms <- function(x, ...) {
  status <- x[["status"]]
  counts <- table(factor(status, levels = c("fits", "drops")))
  counts <- counts[counts > 0]
  summary <- if (length(counts)) {
    paste(counts, names(counts), collapse = ", ")
  } else {
    "none"
  }
  cli::cli_text(
    "Effects of a {.val {attr(x, 'model')}} {.val {attr(x, 'sub_model')}} ",
    "model of {.field {deparse(attr(x, 'layer'))}}: {summary}"
  )
  out <- tidy(x)
  shown <- intersect(c("term", "status", "reason"), names(out))
  print(if (length(shown)) out[shown] else out, n = Inf, ...)
  cli::cli_text(
    "Choose rows, and {.fn formula} of them gives their specification."
  )
  invisible(x)
}

# ---- reading a formula -------------------------------------------------------

#' @keywords internal
#' @noRd
sd_every_term <- function(formula) {
  length(formula) == 3 && identical(formula[[3]], as.name("."))
}

#' @keywords internal
#' @noRd
sd_check_parts <- function(parts, model) {
  allowed <- if (model == "REM") {
    c("rate", "rate_ordered")
  } else {
    c("choice", "choice_coordination", "rate", "rate_ordered")
  }
  ok <- is.list(parts) && length(parts) > 0 && !is.null(names(parts)) &&
    all(names(parts) %in% allowed) &&
    all(vapply(parts, function(f) inherits(f, "formula") && length(f) == 3,
               logical(1)))
  if (!ok) {
    cli::cli_abort(c(
      "{.arg formula} must be a two-sided formula, or a list of them named by
       sub-model.",
      "i" = "The sub-models of a {.val {model}} are {.val {allowed}}."
    ))
  }
  if (sum(names(parts) %in% c("rate", "rate_ordered")) > 1 ||
        sum(names(parts) %in% c("choice", "choice_coordination")) > 1) {
    cli::cli_abort("{.arg formula} has more than one rate or choice formula.")
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
sd_layers <- function(.data) {
  if (is.null(.data)) {
    return(character(0))
  }
  layers <- .data$info$layers
  if (is.null(layers)) {
    layers <- unique(.data$ties$layer)
  }
  as.character(layers)
}

# The dependent layer that the left-hand side names: `.` is `info$focal`, and
# a name that is no layer, as the network on an ergm left-hand side, is too.
#' @keywords internal
#' @noRd
sd_layer_name <- function(lhs, .data) {
  name <- deparse(lhs)
  if (name == "." || !(name %in% c(sd_layers(.data),
                                   names(.data$info$dependents)))) {
    return(.data$info$focal)
  }
  name
}

# The names of goldfish's effects, from the functions that update them.
#' @keywords internal
#' @noRd
sd_effect_names <- function() {
  fns <- ls(asNamespace("goldfish"), pattern = "^update_(DyNAM|REM)_")
  unique(sub("^update_(DyNAM|REM)_(choice_coordination|choice|rate)_", "", fns))
}

# The model a formula was written for: as stated, from its class, or from a
# name that only one model has. A name that is also a layer is the layer.
#' @keywords internal
#' @noRd
sd_read_input <- function(formula, input, layers) {
  inputs <- c("dynam", "mrqap", "ergm")
  if (!is.null(input)) {
    if (identical(input, "rem")) {
      return("dynam")
    }
    if (!(length(input) == 1 && input %in% inputs)) {
      cli::cli_abort("{.arg input} must be one of {.val {inputs}}.")
    }
    return(input)
  }
  classed <- intersect(sub("_formula$", "", class(formula)), inputs)
  if (inherits(formula, "goldfishFormula")) {
    classed <- "dynam"
  }
  if (length(classed) > 0) {
    return(classed[1])
  }
  used <- setdiff(sd_formula_names(formula[[3]]), layers)
  ergm_only <- intersect(used, setdiff(names(sd_ergm_terms()),
                                       sd_effect_names()))
  mrqap_only <- intersect(used, setdiff(names(sd_mrqap_terms()),
                                        sd_effect_names()))
  bars <- grepl("|", deparse(formula[[3]]), fixed = TRUE)
  input <- if (length(ergm_only)) {
    "ergm"
  } else if (length(mrqap_only) || bars) {
    "mrqap"
  } else {
    "dynam"
  }
  if (input != "dynam") {
    clue <- c(ergm_only, mrqap_only, "|")[1]
    cli::cli_inform(
      "Reading the formula as one written for {.val {input}},
       from {.code {clue}}."
    )
  }
  input
}

#' @keywords internal
#' @noRd
sd_formula_names <- function(expr) {
  if (is.name(expr)) {
    return(as.character(expr))
  }
  if (!is.call(expr)) {
    return(character(0))
  }
  fn <- as.character(expr[[1]])
  if (fn %in% c("+", ":", "*", "(", "-")) {
    return(unlist(lapply(as.list(expr)[-1], sd_formula_names)))
  }
  if (fn == "|") {
    return(character(0))
  }
  fn
}

# One formula: its terms written for goldfish, and the record of each.
#' @keywords internal
#' @noRd
sd_read <- function(formula, part, input, layers) {
  # Fixed effects after a bare bar, as in infernet and fixest.
  fixed <- character(0)
  rhs <- formula[[3]]
  if (is.call(rhs) && identical(rhs[[1]], as.name("|"))) {
    fixed <- paste("|", deparse(rhs[[3]]))
    formula[[3]] <- rhs[[2]]
  }
  input <- sd_read_input(formula, input, layers)
  tt <- stats::terms(formula)
  labels <- attr(tt, "term.labels")
  offsets <- vapply(as.list(attr(tt, "variables"))[-1][attr(tt, "offset")],
                    function(e) paste(deparse(e), collapse = ""), "")
  record <- sd_no_record()
  intercept <- sd_has_one(formula[[3]])
  for (lab in labels) {
    if (grepl("|", lab, fixed = TRUE)) {
      record <- rbind(record, sd_row(
        paste0("(", lab, ")"), "", "drops",
        "is a random intercept, which goldfish does not fit"
      ))
      next
    }
    one <- sd_read_term(lab, input, layers)
    # ergm's `edges` is the intercept: a rate model's time intercept, and
    # nothing in a choice model, which compares the receivers of one event.
    if (identical(one$as, "1")) {
      if (part %in% c("rate", "rate_ordered")) {
        intercept <- TRUE
      } else {
        one <- list(
          as = "", status = "drops",
          reason = "is the intercept, which a choice model does not have"
        )
      }
    }
    record <- rbind(record, sd_row(lab, one$as, one$status, one$reason))
  }
  for (f in fixed) {
    record <- rbind(record, sd_row(
      f, "", "drops", "is a fixed effect, which goldfish does not fit"
    ))
  }
  for (o in offsets) {
    keep <- input == "dynam"
    record <- rbind(record, sd_row(
      o, if (keep) o else "", if (keep) "fits" else "drops",
      if (keep) "" else sprintf("is an offset for %s, not yet read here", input)
    ))
  }
  list(lhs = formula[[2]], input = input, record = record,
       intercept = intercept)
}

#' @keywords internal
#' @noRd
sd_has_one <- function(expr) {
  if (identical(expr, 1) || identical(expr, 1L)) {
    return(TRUE)
  }
  is.call(expr) && identical(expr[[1]], as.name("+")) &&
    any(vapply(as.list(expr)[-1], sd_has_one, logical(1)))
}

#' @keywords internal
#' @noRd
sd_no_record <- function() {
  data.frame(term = character(0), as = character(0), status = character(0),
             reason = character(0))
}

#' @keywords internal
#' @noRd
sd_row <- function(term, as, status, reason) {
  data.frame(term = term, as = as, status = status, reason = reason)
}

# One term, operand by operand. An interaction with an operand that goldfish
# cannot fit is dropped whole, and so is one with an ergm term that becomes two.
#' @keywords internal
#' @noRd
sd_read_term <- function(label, input, layers) {
  ops <- sd_flatten(str2lang(label))
  read <- lapply(ops, sd_read_operand, input = input, layers = layers)
  reason <- unlist(lapply(read, `[[`, "drop"))
  if (length(reason) > 0) {
    return(list(as = "", status = "drops", reason = reason[1]))
  }
  exprs <- lapply(read, `[[`, "expr")
  if (length(ops) > 1 && any(lengths(exprs) != 1)) {
    return(list(as = "", status = "drops",
                reason = "becomes two effects, which cannot be interacted"))
  }
  new <- if (length(ops) == 1) {
    paste(vapply(exprs[[1]], sd_deparse, ""), collapse = " + ")
  } else {
    sd_deparse(Reduce(function(a, b) call(":", a, b),
                      lapply(exprs, `[[`, 1)))
  }
  note <- c(unlist(lapply(read, `[[`, "note")), "")[1]
  interpreted <- c(unlist(lapply(read, `[[`, "interpreted")), "")[1]
  if (identical(new, "1")) {
    return(list(as = "1", status = "adapted",
                reason = "is the time intercept of a rate model"))
  }
  if (nzchar(interpreted)) {
    return(list(as = new, status = "interpreted", reason = interpreted))
  }
  if (!identical(new, label)) {
    return(list(as = new, status = "adapted", reason = note))
  }
  list(as = new, status = "fits", reason = "")
}

#' @keywords internal
#' @noRd
sd_flatten <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], as.name(":"))) {
    return(c(sd_flatten(expr[[2]]), sd_flatten(expr[[3]])))
  }
  list(expr)
}

#' @keywords internal
#' @noRd
sd_deparse <- function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = "")
}

# One operand: a list of the goldfish expressions it becomes, or the reason
# to drop it, with a note where it was rewritten and a reason where goldfish
# defines it differently.
#' @keywords internal
#' @noRd
sd_read_operand <- function(expr, input, layers) {
  name <- if (is.call(expr)) as.character(expr[[1]]) else as.character(expr)
  if (input == "ergm") {
    reader <- sd_ergm_terms()[[name]]
    if (is.function(reader)) {
      return(reader(expr))
    }
    if (is.character(reader)) {
      return(list(drop = reader))
    }
  }
  if (input == "mrqap") {
    if (is.name(expr) && (name %in% layers ||
                            !(name %in% sd_effect_names()))) {
      return(list(expr = list(call("tie", expr, weighted = TRUE)),
                  note = "since a layer named on its own reads its values"))
    }
    reader <- sd_mrqap_terms()[[name]]
    if (!is.null(reader) && is.call(expr)) {
      return(reader(expr))
    }
  }
  list(expr = list(expr))
}

# Each ergm term that has a goldfish effect, as a function of its call, or the
# reason that it has none. ergm names an attribute with a string, and goldfish
# with a bare name.
#' @keywords internal
#' @noRd
sd_ergm_terms <- function() {
  attr_of <- function(expr) {
    as.name(if (is.call(expr) && length(expr) > 1) as.character(expr[[2]])
            else NA_character_)
  }
  no_levels <- function(fn) {
    function(expr) {
      if (is.call(expr) && !is.null(expr[["levels"]])) {
        return(list(drop = "has no goldfish effect with `levels =` yet"))
      }
      list(expr = lapply(fn, function(f) call(f, attr_of(expr))))
    }
  }
  as_name <- function(new) function(expr) list(expr = list(as.name(new)))
  structural <- c(
    "asymmetric", "balance", "b1degree", "b2degree", "b1star", "b2star",
    "concurrent", "cyclicalties", "ddsp", "desp", "dgwdsp", "dgwesp",
    "dgwnsp", "dnsp", "dsp", "esp", "gwb1degree", "gwb2degree", "gwdegree",
    "gwdsp", "gwesp", "gwidegree", "gwnsp", "gwodegree", "idegree",
    "intransitive", "isolates", "istar", "kstar", "localtriangle", "m2star",
    "meandeg", "nsp", "odegree", "ostar", "transitiveties", "triadcensus",
    "twopath", "sender", "receiver", "sociality"
  )
  read <- list(
    edges = function(expr) list(expr = list(1)),
    density = function(expr) list(expr = list(1)),
    mutual = as_name("recip"),
    ttriple = as_name("trans"),
    transitive = as_name("trans"),
    ctriple = as_name("cycle"),
    triangle = as_name("triangle"),
    cycle = function(expr) {
      if (is.call(expr) && length(expr) == 2 && isTRUE(expr[[2]] == 4)) {
        return(list(expr = list(as.name("four"))))
      }
      list(drop = "has no goldfish effect but for cycles of four")
    },
    nodeocov = no_levels("ego"),
    nodeofactor = no_levels("ego"),
    b1cov = no_levels("ego"),
    b1factor = no_levels("ego"),
    nodeicov = no_levels("alter"),
    nodeifactor = no_levels("alter"),
    b2cov = no_levels("alter"),
    b2factor = no_levels("alter"),
    nodecov = no_levels(c("ego", "alter")),
    nodefactor = no_levels(c("ego", "alter")),
    nodematch = function(expr) {
      if (is.call(expr) && !is.null(expr$diff) && isTRUE(eval(expr$diff))) {
        return(list(drop = "has no goldfish effect with `diff = TRUE`"))
      }
      no_levels("same")(expr)
    },
    nodemix = function(expr) list(drop = "has no goldfish effect"),
    absdiff = no_levels("diff"),
    diff = function(expr) {
      list(expr = list(call("diff", attr_of(expr),
                            transformer_fn = quote(identity))))
    },
    edgecov = function(expr) list(expr = list(call("tie", attr_of(expr))))
  )
  c(read, stats::setNames(
    rep(list("has no goldfish effect"), length(structural)), structural
  ))
}

# Each term of infernet's MRQAP formula that goldfish has under another name,
# defines differently, or does not have.
#' @keywords internal
#' @noRd
sd_mrqap_terms <- function() {
  rename <- function(new, note = "") {
    function(expr) {
      expr[[1]] <- as.name(new)
      list(expr = list(expr), note = note)
    }
  }
  no_levels <- function(expr) {
    if (!is.null(expr[["levels"]])) {
      return(list(drop = "has no goldfish effect with `levels =` yet"))
    }
    list(expr = list(expr))
  }
  differs <- function(reason) {
    function(expr) list(expr = list(expr), interpreted = reason)
  }
  drop <- function(reason) {
    function(expr) list(drop = reason)
  }
  ego_differs <- paste(
    "leaves out the sender in infernet,",
    "and includes it in goldfish"
  )
  reporter <- "describes the reporter of a cognitive social structure"
  # infernet names the side of a degree with `node =`, and its `outdeg()`
  # reads the sender by default, where goldfish's reads the receiver.
  degree <- function(expr) {
    args <- as.list(expr)[-1]
    side <- args$node %||% args$type
    args$node <- NULL
    args$type <- NULL
    if (is.null(side) && identical(expr[[1]], as.name("outdeg"))) {
      side <- "ego"
    }
    if (identical(side, "ego")) {
      args$type <- "ego"
    }
    new <- as.call(c(expr[[1]], args))
    list(expr = list(new),
         note = if (!identical(new, expr)) "as goldfish names the side" else "")
  }
  list(
    exchange = rename("recip"),
    dist = rename("diff"),
    ego = no_levels,
    alter = no_levels,
    same = function(expr) {
      if (!is.null(expr$diff) && isTRUE(eval(expr$diff))) {
        return(list(drop = "has no goldfish effect with `diff = TRUE`"))
      }
      no_levels(expr)
    },
    match = drop("has no goldfish effect with `diff = TRUE`"),
    mix = drop("has no goldfish effect"),
    indeg = degree,
    outdeg = degree,
    sim = differs(paste(
      "is 1 - |d| / range in infernet, and -|d| in goldfish,",
      "so its coefficient is on another scale"
    )),
    tertius = differs(ego_differs),
    tertius_diff = differs(ego_differs),
    reporter = drop(reporter),
    reporter_ego = drop(reporter),
    reporter_alter = drop(reporter),
    reporter_same = drop(reporter)
  )
}

# With the network, a main effect that the sub-model cannot identify, such as
# `ego()` in a choice model, is dropped with goldfish's reason, so that the
# rest still fits.
#' @keywords internal
#' @noRd
sd_check_validity <- function(read, model, part, .data) {
  record <- read$record
  if (is.null(.data)) {
    return(record)
  }
  sub_model <- part
  if (part == "rate" && !read$intercept) {
    sub_model <- "rate_ordered"
  }
  disallowed <- unidentified_variations(model, sub_model)
  for (k in seq_len(nrow(record))) {
    if (!record$status[k] %in% c("fits", "adapted", "interpreted") ||
          !nzchar(record$as[k]) || record$as[k] == "1") {
      next
    }
    for (one in strsplit(record$as[k], " + ", fixed = TRUE)[[1]]) {
      expr <- str2lang(one)
      if (length(sd_flatten(expr)) > 1) {
        next
      }
      name <- if (is.call(expr)) as.character(expr[[1]]) else as.character(expr)
      type <- if (is.call(expr) && !is.null(expr$type)) expr$type else ""
      variation <- effect_variation(name, as.character(type))
      if (variation %in% disallowed) {
        record$status[k] <- "drops"
        record$reason[k] <- switch(
          variation,
          global = "is constant across the alternatives",
          ego = paste(
            "is an ego-perspective statistic,",
            "constant across the alternatives"
          ),
          alter = paste(
            "is an alter-perspective statistic,",
            "with no receiver axis in a rate model"
          )
        )
      }
    }
  }
  record
}

#' @keywords internal
#' @noRd
sd_one_sided <- function(terms, env) {
  rhs <- if (length(terms) == 0) "1" else paste(terms, collapse = " + ")
  stats::as.formula(paste("~", rhs), env = env %||% globalenv())
}

# Rewritten and differently read terms are information, one line each; a
# dropped term is a warning, since the model is then not the one the formula
# describes.
#' @keywords internal
#' @noRd
sd_report <- function(record) {
  for (k in seq_len(nrow(record))) {
    term <- sd_escape(record$term[k])
    as <- sd_escape(record$as[k])
    why <- if (nzchar(record$reason[k])) {
      paste0(", ", sd_escape(record$reason[k]))
    } else {
      ""
    }
    if (record$status[k] == "adapted" && nzchar(as)) {
      cli::cli_inform(paste0("Writing {.code ", term, "} as {.code ", as,
                             "} for goldfish", why, "."))
    } else if (record$status[k] == "interpreted") {
      cli::cli_inform(paste0("{.code ", term, "} ", sd_escape(record$reason[k]),
                             "."))
    }
  }
  dropped <- record[record$status == "drops", , drop = FALSE]
  if (nrow(dropped) > 0) {
    cli::cli_warn(c(
      "Dropped {nrow(dropped)} term{?s} that goldfish cannot fit:",
      stats::setNames(sprintf("{.code %s} %s", sd_escape(dropped$term),
                              sd_escape(dropped$reason)),
                      rep("*", nrow(dropped)))
    ))
  }
  invisible(record)
}

#' @keywords internal
#' @noRd
sd_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

# ---- the table of every effect -----------------------------------------------

#' @keywords internal
#' @noRd
sd_term_table <- function(.data, lhs, model, sub_model) {
  layer <- sd_layer_name(lhs, .data)
  rows <- sd_candidates(.data, layer, model, sub_model)
  slot <- if (sub_model %in% c("rate", "rate_ordered")) "rate" else "choice"
  status <- lapply(rows$term, function(term) {
    rhs <- c(if (sub_model == "rate") "1", term)
    args <- list(
      stats::as.formula(paste("~", paste(rhs, collapse = " + ")),
                        env = globalenv()),
      model = model,
      layer = layer,
      data = .data
    )
    names(args)[1] <- slot
    if (slot == "rate") {
      args$rate_sub_model <- sub_model
    } else {
      args$choice_sub_model <- sub_model
    }
    tryCatch(
      suppressWarnings(suppressMessages({
        do.call(make_specification, args)
        list(status = "fits", reason = NA_character_)
      })),
      error = function(e) {
        list(status = "drops", reason = sd_condition_reason(e))
      }
    )
  })
  rows$status <- vapply(status, `[[`, "", "status")
  rows$reason <- vapply(status, `[[`, "", "reason")
  out <- tibble::as_tibble(rows)
  structure(out, class = c("goldfishTerms", class(out)),
            layer = as.name(layer), model = model, sub_model = sub_model,
            data = .data)
}

# The line of a condition that says why: its first bullet, or else its first
# line, without cli's markup.
#' @keywords internal
#' @noRd
sd_condition_reason <- function(e) {
  msg <- cli::ansi_strip(conditionMessage(e))
  lines <- strsplit(msg, "\n", fixed = TRUE)[[1]]
  bullet <- grep("^\\s*(✖|x) ", lines, value = TRUE)
  out <- if (length(bullet)) bullet[1] else lines[1]
  trimws(sub("^\\s*(✖|x) ", "", out))
}

# Every goldfish effect of `model` and `sub_model`, written out for the
# network's layers and node attributes, by the first argument of the function
# that updates it.
#' @keywords internal
#' @noRd
sd_candidates <- function(.data, layer, model, sub_model) {
  prefix <- switch(
    paste(model, sub_model),
    "DyNAM choice" = "DyNAM_choice",
    "DyNAM choice_coordination" = "DyNAM_choice_coordination",
    "DyNAM rate" = ,
    "DyNAM rate_ordered" = "DyNAM_rate",
    "REM_choice"
  )
  ns <- asNamespace("goldfish")
  fns <- ls(ns, pattern = paste0("^update_", prefix, "_"))
  if (prefix == "DyNAM_choice") {
    fns <- fns[!grepl("^update_DyNAM_choice_coordination_", fns)]
  }
  effects <- sub(paste0("^update_", prefix, "_"), "", fns)
  formals_of <- lapply(fns, function(f) names(formals(get(f, envir = ns))))
  names(formals_of) <- effects
  layers <- sd_layers(.data)
  others <- setdiff(layers, layer)
  attrs <- sd_attributes(.data)
  numeric <- attrs$name[attrs$numeric]
  found <- new.env()
  found$rows <- list()
  add <- function(term, effect, on = NA_character_, attribute = NA_character_) {
    found$rows[[length(found$rows) + 1]] <- data.frame(
      term = term, effect = effect, layer = on, attribute = attribute
    )
  }
  call_of <- function(fn, ...) {
    paste0(fn, "(", paste(c(...), collapse = ", "), ")")
  }
  for (fn in effects) {
    args <- formals_of[[fn]]
    if (startsWith(fn, "mixed_") || fn == "ego_alter_interaction") {
      next
    }
    if (args[1] == "network" && "attribute" %in% args) {
      for (l in layers) {
        for (x in numeric) {
          add(call_of(fn, l, x), fn, l, x)
        }
      }
    } else if (args[1] == "network") {
      add(fn, fn, layer)
      for (l in others) {
        add(call_of(fn, l), fn, l)
      }
      if ("type" %in% args) {
        for (l in layers) {
          add(call_of(fn, l, "type = \"ego\""), fn, l)
        }
      }
    } else if (fn == "global") {
      for (v in sd_globals(.data)) {
        add(call_of(fn, v), fn, attribute = v)
      }
    } else if (args[1] == "attribute") {
      use <- if (fn %in% c("diff", "sim")) numeric else attrs$name
      for (x in use) {
        add(call_of(fn, x), fn, attribute = x)
      }
    }
  }
  if (length(found$rows) == 0) {
    return(data.frame(term = character(0), effect = character(0),
                      layer = character(0), attribute = character(0)))
  }
  do.call(rbind, found$rows)
}

# The node attributes an effect can read. A label, a name, `active`, a marker
# of missing data, or a categorical attribute with a different value for every
# node describes no node in a way an effect can use.
#' @keywords internal
#' @noRd
sd_attributes <- function(.data) {
  nodes <- .data$nodes
  if (is.null(nodes)) {
    return(data.frame(name = character(0), numeric = logical(0)))
  }
  names <- setdiff(names(nodes), c("label", "name", "type", "na", "active",
                                   "present"))
  keep <- vapply(names, function(x) {
    v <- nodes[[x]]
    if (is.list(v)) {
      return(FALSE)
    }
    v <- v[!is.na(v)]
    n <- length(unique(v))
    n >= 2 && !((is.character(v) || is.factor(v)) && n == length(v))
  }, logical(1))
  names <- names[keep]
  data.frame(name = names,
             numeric = vapply(names, function(x) is.numeric(nodes[[x]]),
                              logical(1)),
             row.names = NULL)
}

#' @keywords internal
#' @noRd
sd_globals <- function(.data) {
  g <- .data$global
  if (is.null(g)) {
    return(character(0))
  }
  if (is.data.frame(g) && "var" %in% names(g)) {
    return(unique(as.character(g$var)))
  }
  names(g)
}
