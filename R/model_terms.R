##################### ###
#
# Goldfish package
# The terms of a fitted model, and the one matcher that resolves them
#
##################### ###

#' The terms of a fitted model, and what to call them
#'
#' @description
#' Every term of a fitted model as one row, carrying each of the names the
#' package will answer to for it. Use it when a term argument rejects what you
#' typed, when a model has too many effects for an error to list them, or when
#' two terms differ only in an argument.
#'
#' @details
#' goldfish renders a term differently depending on where it appears: the
#' printed summary shows a compact string, the export surfaces a
#' file-safe form, and `coef()` a minimal-unique abbreviation. Every argument
#' that takes a term — [set_algorithm_newton()]'s `initial_parameters`, the
#' `effect =` / `effects =` arguments of the `diagnose_*` and `test_*`
#' families — accepts **any** of them, plus the coefficient's position. This
#' table is where you read them off.
#'
#' Two of the spellings are unique by construction and two are not, which is
#' why more than one is accepted:
#' \describe{
#'   \item{`term`}{the compact string the printed summary shows, e.g.
#'     `inertia/calls [15m]`. The most readable, but the summary **abbreviates
#'     it to the console width** — on a narrow console two terms can print
#'     identically — and two terms differing only in an anonymous function
#'     argument render alike. This column always holds the full, unabbreviated
#'     string.}
#'   \item{`coefficient`}{the `coef()` label, e.g. `inrt_cal_15m`. Unique by
#'     construction: the disambiguating suffix is what makes it so.}
#'   \item{`export`}{the file-safe form [gather_model_data()] and
#'     `tidy(compact = TRUE)` use.}
#'   \item{`index`}{the coefficient's position. Unique, but **fragile**: adding
#'     a term to the formula renumbers every term after it.}
#' }
#'
#' The remaining columns are the effect details the non-compact summary prints
#' — the object each effect reads, and the arguments it was given — so a
#' `pattern` search reaches them too.
#'
#' On a flavored (multi-process) fit the per-process tables are row-bound with
#' `flavor` and `family` columns appended, so a term carried by two processes
#' appears once per process.
#'
#' This is a *fit-scoped* helper: it answers which terms this model has. It is
#' not the effect catalogue, which answers which effects goldfish provides.
#'
#' @param x a fitted model of class `"goldfishFit"`, or a flavored
#'   multi-process fit.
#' @param pattern an optional regular expression; only terms matching it in any
#'   of their names or in their effect details are returned. Matching is
#'   case-insensitive.
#' @param ... additional arguments passed to or from other methods (currently
#'   unused).
#'
#' @return A [tibble::tibble()] with one row per term, carrying `index`,
#'   `term`, `coefficient`, `export`, the effect-detail columns, and — on a
#'   flavored fit — `flavor` and `family`.
#'
#' @examples
#' data("social_evolution")
#' fit <- estimate_dynam(
#'   calls ~ inertia + recip + trans,
#'   sub_model = "choice",
#'   data = social_evolution
#' )
#' model_terms(fit)
#'
#' # Which of these is the one on the friendship network?
#' model_terms(fit, pattern = "recip")
#'
#' @seealso [set_algorithm_newton()] for `initial_parameters`, and
#'   [estimate_dynam()] for the fitted object these describe.
#' @export
model_terms <- function(x, pattern = NULL, ...) {
  UseMethod("model_terms")
}

#' @export
model_terms.default <- function(x, pattern = NULL, ...) {
  cli::cli_abort(c(
    "{.fn model_terms} needs a fitted goldfish model.",
    "x" = "{.arg x} is {.obj_type_friendly {x}}.",
    "i" = "Fit one with {.fn estimate_dynam} or {.fn estimate_rem}."
  ))
}

#' @rdname model_terms
#' @export
model_terms.goldfishFit <- function(x, pattern = NULL, ...) {
  abort_if_stale_result(x, "a term table")
  filter_model_terms(term_table(x$names), pattern)
}

#' @rdname model_terms
#' @export
model_terms.goldfishFlavFit <- function(x, pattern = NULL, ...) {
  tables <- lapply(flavored_processes(x), function(process) {
    append_process_identity(term_table(process$fit$names), process)
  })
  filter_model_terms(do.call(rbind, tables), pattern)
}

# One row per coefficient, every spelling side by side. `width = Inf` is the
# point of the `term` column: the printed summary abbreviates to the console,
# and an abbreviated string may match nothing or match two terms, so the table
# always reports the full one.
term_table <- function(names) {
  details <- tibble::as_tibble(as.data.frame(
    names,
    stringsAsFactors = FALSE
  ))
  # The generated label columns are the spellings, reported under their own
  # names rather than the internal dotted ones.
  details <- details[, !startsWith(colnames(names), "."), drop = FALSE]
  out <- tibble::tibble(
    index = seq_len(nrow(names)),
    term = unname(compact_term_strings(names, "console", width = Inf)),
    coefficient = term_label(names, ".coef_name", "coef"),
    export = term_label(names, ".term_export", "export")
  )
  out <- cbind(out, details)
  # Effect-detail columns are blank rather than NA where an argument was not
  # given, which is what the summary prints; keep that, so the table reads the
  # same as the page it mirrors.
  tibble::as_tibble(out)
}

filter_model_terms <- function(terms, pattern) {
  if (is.null(pattern)) {
    return(terms)
  }
  if (!rlang::is_string(pattern)) {
    cli::cli_abort("{.arg pattern} must be a single string.")
  }
  searchable <- vapply(
    seq_len(nrow(terms)),
    function(i) paste(as.character(unlist(terms[i, ])), collapse = " "),
    character(1)
  )
  terms[grepl(pattern, searchable, ignore.case = TRUE), , drop = FALSE]
}

# The one matcher every user-facing term argument routes through -------------

# Resolve what a user typed into coefficient positions, accepting any spelling
# the model answers to.
#
# More than one spelling is accepted for CORRECTNESS, not convenience. The
# compact string is the readable one and the one the summary shows, but it is
# neither guaranteed unique -- two terms differing only in an anonymous
# function argument render alike -- nor guaranteed to be what the user saw,
# since the summary abbreviates to the console width. The `coef()` label and
# the index are unique by construction, so they are the spellings that can
# always resolve a term; without them an ambiguous compact string would be a
# dead end, the caller told their name is ambiguous with no way to say which
# one they meant.
#
# `wanted` may be character (any spelling) or numeric (positions).
#
# `expand_family` turns on one further spelling: the bare effect name, which
# selects EVERY term of that effect. The diagnostic tests want it because a
# model may carry an effect several times over different layers or arguments
# -- `inertia`, `inertia(friendship)`, `inertia(calls, weighted = TRUE)` --
# and "test inertia" is then a question about the effect, not about one of its
# variants. It is off elsewhere: an argument that selects a single coefficient
# has nothing to do with a set, and silently returning three positions where
# one was expected would be worse than the unknown-term error.
resolve_term_index <- function(
  wanted,
  names,
  arg,
  expand_family = FALSE,
  call = rlang::caller_env()
) {
  n_terms <- nrow(names)
  if (is.numeric(wanted)) {
    return(resolve_term_position(wanted, n_terms, arg, call = call))
  }
  if (!is.character(wanted)) {
    cli::cli_abort(
      "{.arg {arg}} must name terms, or give their positions.",
      call = call
    )
  }
  spellings <- term_spellings(names)
  matched <- lapply(
    wanted,
    function(one) {
      match_one_term(
        one,
        spellings,
        names,
        arg,
        expand_family = expand_family,
        call = call
      )
    }
  )
  # Request order is preserved -- a selection reports in the order it was
  # asked for -- and an expanded family contributes its terms in model order at
  # the position where it was named. Deduplicated so that naming an effect and
  # one of its own variants still selects each position once.
  as.integer(unique(unlist(matched)))
}

# The three name columns as a position-indexed lookup, long form: one row per
# (spelling, coefficient) pair, so a match on any of them is one comparison.
term_spellings <- function(names) {
  list(
    term = unname(compact_term_strings(names, "console", width = Inf)),
    coefficient = term_label(names, ".coef_name", "coef"),
    export = term_label(names, ".term_export", "export")
  )
}

match_one_term <- function(
  one,
  spellings,
  names,
  arg,
  expand_family = FALSE,
  call
) {
  hits <- sort(unique(unlist(lapply(spellings, function(s) which(s == one)))))
  if (length(hits) == 1L) {
    return(as.integer(hits))
  }
  # The effect family, which the row names of the name matrix carry. Tried only
  # after the per-term spellings, so a term that happens to spell like an
  # effect name still resolves to itself rather than to the family.
  if (expand_family && length(hits) == 0L) {
    family <- which(rownames(names) == one)
    if (length(family) > 0L) {
      return(as.integer(family))
    }
  }
  if (length(hits) > 1L) {
    # Ambiguity is only reachable through the compact string, so listing the
    # colliding strings would repeat one text three times. Offer the spellings
    # that separate them instead -- otherwise the abort has no exit.
    unique_names <- spellings$coefficient[hits]
    cli::cli_abort(
      c(
        "{.arg {arg}} matches {length(hits)} terms of this model.",
        "x" = "{.val {one}} is carried by more than one term.",
        "i" = "Name one of {.val {unique_names}}, or give its position
               ({.val {hits}}).",
        "i" = "See {.fn model_terms} for every term and every name it
               answers to."
      ),
      call = call
    )
  }
  message <- c(
    "{.arg {arg}} names a term this model does not have.",
    "x" = "Unknown: {.val {one}}.",
    "i" = "Available: {.val {spellings$term}}."
  )
  if (expand_family) {
    families <- unique(rownames(names))
    message <- c(
      message,
      "i" = "An effect name selects all of its terms: {.val {families}}."
    )
  }
  cli::cli_abort(
    c(message, "i" = "Search them with {.code model_terms(fit, pattern = )}."),
    call = call
  )
}

# Positions are accepted but fragile: adding a term to the formula renumbers
# every term after it, so an out-of-range position is a counting mistake worth
# naming rather than recycling.
resolve_term_position <- function(wanted, n_terms, arg, call) {
  wanted <- as.integer(wanted)
  if (anyNA(wanted) || any(wanted < 1L) || any(wanted > n_terms)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must give positions this model has.",
        "x" = "It must lie in {.val {1L}}:{.val {n_terms}}.",
        "i" = "See {.fn model_terms} for the positions and their names."
      ),
      call = call
    )
  }
  wanted
}
