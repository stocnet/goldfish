# =========================================================================== #
# as_goldfish(): the optional early validate-and-stamp boundary.
#
# estimate_dynam()/estimate_rem()/make_specification() validate a stocnet on the
# fly; as_goldfish() is the optional early gate that validates once and stamps
# the object for provenance and print dispatch. The stamp NEVER bypasses
# re-validation downstream (manynet verbs and plain list assignment mutate the
# object while preserving the class vector, so a stamp is not evidence of
# validity) -- it is a marker only.
# =========================================================================== #

#' Validate and stamp a stocnet object for goldfish
#'
#' `r lifecycle::badge("experimental")`
#'
#' `as_goldfish()` is the optional early gate for the goldfish data input. It
#' runs the same validation that `estimate_dynam()`, `estimate_rem()`, and
#' `make_specification()` apply to a `stocnet` object, failing fast with an
#' informative error when the data does not meet goldfish's contract, and stamps
#' the object with the `data.goldfish` class marker.
#'
#' Passing the stamped object to estimation is equivalent to passing the raw
#' stocnet: validation runs again unconditionally at specification time. The
#' stamp is used for provenance and print dispatch only; it does not certify the
#' object as still valid, because manynet manipulation verbs and plain list
#' assignment mutate the object while preserving its class vector.
#'
#' @details
#' # Event-ordering contract
#'
#' stocnet coercions arrange ties by `from`/`to`, discarding the incoming row
#' order, so goldfish imposes a deterministic event schedule at conversion time
#' (in preprocessing, not in `as_goldfish()`). Events are ordered by:
#'
#' 1. `time`;
#' 2. dependent (focal-layer) events before exogenous events;
#' 3. component order: `ties`, then `changes`, then `global`;
#' 4. `layer`;
#' 5. `from`/`to`.
#'
#' When `ties` or `changes` carry an integer `order` column, it replaces
#' `from`/`to` as the final tie-break. Multiple `replace` events targeting the
#' same cell (or the same node-variable) at the same time with no `order` column
#' are genuinely ambiguous and abort with an error naming the colliding rows;
#' same-time increments commute and pass silently.
#'
#' @param x a `stocnet` object (a list of `info`, `nodes`, `ties`, and optional
#'   `changes` / `global` components, as produced by
#'   [manynet::make_stocnet()]). A hand-built list with the same structure
#'   (plain data frames) is also accepted -- the boundary reads structure, not
#'   class provenance.
#' @param focal optional character string naming the focal (dependent) layer,
#'   overriding `info$focal`.
#' @param ... reserved for future use.
#'
#' @return `x`, unchanged in structure, with `"data.goldfish"` prepended to its
#'   class vector.
#'
#' @seealso [goldfish_data] for how to assemble the `stocnet` object this gates.
#' @export
#' @examples
#' nodes <- data.frame(label = c("A", "B", "C"), floor = c(1, 2, 1))
#' ties <- data.frame(
#'   from = c(1L, 2L, 3L),
#'   to = c(2L, 3L, 1L),
#'   time = c(NA, 1, 2),
#'   layer = "calls"
#' )
#' # `focal` is optional -- the modeling formula's LHS names the dependent, and
#' # the modeled layer drives side resolution. Set it only for a default.
#' info <- list(
#'   name = "toy",
#'   update = c(calls = "increment"),
#'   directed = c(calls = TRUE),
#'   observation = c(calls = "event")
#' )
#' toy <- list(info = info, nodes = nodes, ties = ties)
#' as_goldfish(toy)
as_goldfish <- function(x, focal = NULL, ...) {
  if (is.environment(x)) {
    cli::cli_abort(c(
      "Converting a legacy {.cls data.goldfish} environment is not yet \\
       available.",
      "i" = "It arrives with the constructor deprecation flip; until then, \\
             rebuild the data as a {.cls stocnet} object with \\
             {.fn manynet::make_stocnet}."
    ))
  }
  validate_goldfish_data(x, focal = focal)
  stamp_data_goldfish(x)
}

# Prepend the data.goldfish marker while preserving the underlying class vector
# (c("stocnet", "list") for a real stocnet; "list" for a hand-built list).
stamp_data_goldfish <- function(x) {
  structure(x, class = unique(c("data.goldfish", class(x))))
}

# Render the list (stocnet) shape of a stamped object. Legacy environment
# objects keep the environment print path in print.data.goldfish().
print_data_goldfish_list <- function(x, ...) {
  info <- x$info %||% list()
  name <- info$name %||% "goldfish data"
  cli::cli_h1("{name}")

  layers <- if (!is.null(x$ties$layer)) unique(x$ties$layer) else character(0)
  focal <- info$focal
  cli::cli_text(
    "{length(layers)} layer{?s}, {nrow(x$nodes)} node{?s}, \\
     {nrow(x$ties)} tie{?s}."
  )

  if (length(layers) > 0) {
    cli::cli_h3("Layers")
    items <- vapply(
      layers,
      function(l) {
        directed <- info$directed[l]
        update <- info$update[l]
        observation <- info$observation[l]
        tag <- if (identical(unname(l), focal)) " (focal)" else ""
        paste0(
          "{.field ",
          l,
          "}",
          tag,
          ": ",
          if (!is.na(observation)) unname(observation) else "?",
          ", ",
          if (isTRUE(unname(directed))) "directed" else "undirected",
          ", ",
          if (!is.na(update)) unname(update) else "?"
        )
      },
      character(1)
    )
    cli::cli_ul(items)
  }

  extras <- c(
    if (!is.null(x$changes) && nrow(x$changes) > 0) {
      "{nrow(x$changes)} attribute change{?s}"
    },
    if (!is.null(x$global) && nrow(x$global) > 0) {
      "{nrow(x$global)} global event{?s}"
    }
  )
  if (length(extras) > 0) {
    cli::cli_text(paste(extras, collapse = ", "), ".")
  }
  invisible(x)
}
