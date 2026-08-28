# =========================================================================== #
# parameters.goldfish: a self-validating parameter object over a joint spec's
# fid-indexed coefficient vectors.
#
# `set_parameters()` resolves the user-facing composite labels and the
# offset-prevails NA disambiguation, then calls the constructor here with the
# per-fid layout already fully resolved: every fixed slot (an `offset()` term
# or an operand-only interaction) carries the specification's value, and every
# free slot is either the user's pin or `NA` (still to be estimated). Two
# consumers read different projections of the same object:
#   * the free-parameter vector (fixed slots skipped) is what `estimate_dynes()`
#     estimates, or warm-starts from when a free slot is pinned;
#   * the full per-fid coefficient vectors are what `simulate()` walks, once
#     every free slot is pinned (the `complete` flag).
# Both projections, and `coef()` on a fitted result, concatenate in the same
# canonical order: `process_map` fid order, then coefficient order within
# each fid (the intercept when present, then effects, then interactions).
# =========================================================================== #

# Build a `parameters.goldfish` from an already-resolved per-fid layout.
#
# `fids` is a named list keyed by `as.character(process_map$fid)`; each entry
# is `list(values, fixed, names)`, one coefficient-space slot per position
# (`n_params` long): `values` the resolved value (fixed slots already carry
# the specification's value; free slots are the user's pin or `NA`), `fixed`
# the logical fixed mask, `names` the coefficient's `coef()` name. This
# constructor does no label resolution or NA classification -- that is
# `set_parameters()`'s job; it only assembles the two projections, the
# complete flag, and the print layout from a layout its caller has already
# validated.
new_parameters_goldfish <- function(process_map, fids) {
  keys <- as.character(process_map$fid)
  if (!identical(sort(names(fids)), sort(keys))) {
    cli::cli_abort(
      "A parameter object needs exactly one layout entry per process_map fid.",
      .internal = TRUE
    )
  }
  fids <- fids[keys] # canonical process_map order, regardless of input order

  full <- vector("list", length(keys))
  free_values <- vector("list", length(keys))
  free_names <- vector("list", length(keys))
  for (i in seq_along(keys)) {
    entry <- fids[[i]]
    values <- as.numeric(entry$values)
    fixed <- as.logical(entry$fixed)
    slot_names <- as.character(entry$names)
    if (
      length(values) != length(fixed) || length(values) != length(slot_names)
    ) {
      cli::cli_abort(
        "A parameter layout entry needs one value, one fixed flag, and one
         name per coefficient position.",
        .internal = TRUE
      )
    }
    if (any(fixed & is.na(values))) {
      cli::cli_abort(
        "A fixed coefficient must already carry the specification's value.",
        .internal = TRUE
      )
    }
    label <- render_process_label(process_map, process_map$fid[i])
    full[[i]] <- stats::setNames(values, slot_names)
    names(full)[i] <- label
    if (any(!fixed)) {
      free_values[[i]] <- values[!fixed]
      free_names[[i]] <- paste0(label, ": ", slot_names[!fixed])
    }
  }
  free <- unlist(free_values, use.names = FALSE) %||% numeric(0)
  names(free) <- unlist(free_names, use.names = FALSE)

  structure(
    list(
      process_map = process_map,
      fids = fids,
      full = full,
      free = free,
      complete = !anyNA(free)
    ),
    class = "parameters.goldfish"
  )
}

is_parameters_goldfish <- function(x) inherits(x, "parameters.goldfish")

#' @export
#' @rdname print-method
#' @return For objects of class `parameters.goldfish` print a per-process
#'   breakdown of pinned, free, and fixed coefficients, and whether the object
#'   is complete.
print.parameters.goldfish <- function(x, ...) {
  cli::cli_rule(left = "{.cls parameters.goldfish}")
  n_proc <- nrow(x$process_map)
  n_free <- length(x$free)
  n_pinned <- sum(!is.na(x$free))
  status <- if (x$complete) "complete" else "partial"
  cli::cli_text(
    "{n_proc} process{?es} · {n_free} free parameter{?s}
     ({n_pinned} pinned) · {status}"
  )
  for (i in seq_len(nrow(x$process_map))) {
    entry <- x$fids[[as.character(x$process_map$fid[i])]]
    label <- names(x$full)[i]
    cli::cli_text("")
    cli::cli_text("{.strong {label}}")
    for (j in seq_along(entry$names)) {
      name <- entry$names[j]
      value <- entry$values[j]
      if (entry$fixed[j]) {
        cli::cli_bullets(c(
          "*" = "{.field {name}}: {.val {value}} (fixed)"
        ))
      } else if (is.na(value)) {
        cli::cli_bullets(c("*" = "{.field {name}}: free"))
      } else {
        cli::cli_bullets(c(
          "*" = "{.field {name}}: {.val {value}} (free, pinned)"
        ))
      }
    }
  }
  invisible(x)
}
