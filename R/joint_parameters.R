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

# The coefficient-space layout of one fid: the `coef()` names, the fixed mask,
# and the fixed values, all length `n_params` and in coefficient order
# (`[intercept?, effects, interactions]`). The fixed mask and values reuse the
# same intercept-shifted projection the estimator assembles -- an `offset()`
# term carries the formula's inline value, an operand-only interaction column is
# held at 0 -- so authoring and estimation classify a slot the same way rather
# than from the raw rhs-aligned parser slots. `assemble_fixed_parameters()` can
# emit an estimate-time diagnostic (a choice offset constant across the
# alternatives); it is not actionable while merely reading the layout, and the
# estimator emits it in its own right, so it is silenced here to avoid a
# duplicate at authoring time.
fid_coefficient_layout <- function(bundle_entry) {
  bundle <- bundle_entry$bundle
  parsed <- bundle$parsed
  rhs_names <- parsed$rhs_names
  has_intercept <- isTRUE(bundle$has_intercept)
  coef_names <- coefficient_term_labels(parsed, rhs_names, has_intercept)
  n_params <- length(coef_names)
  fixed_spec <- suppressWarnings(assemble_fixed_parameters(
    parsed_formula = parsed,
    rhs_names = rhs_names,
    has_intercept = has_intercept,
    model = bundle_entry$model,
    sub_model = bundle_entry$sub_model,
    fixed_parameters = NULL,
    offset_coef = NULL
  ))
  fixed <- logical(n_params)
  values <- rep(NA_real_, n_params)
  if (!is.null(fixed_spec)) {
    fixed[fixed_spec$idx] <- TRUE
    values[fixed_spec$idx] <- fixed_spec$values
  }
  list(
    names = coef_names,
    n_params = n_params,
    fixed = fixed,
    fixed_values = values
  )
}

# Resolve one supplied per-fid vector against a fid's coefficient layout,
# returning a full-length coefficient-ordered numeric vector (`NA` at every slot
# the user left free). A vector is validated three ways: it must have exactly
# `n_params` entries (a mismatch aborts naming the fid and the expected count);
# its per-slot names are optional but all-or-nothing (fully named or fully
# positional -- a partially named vector aborts); and when named, the names must
# be exactly the fid's coefficient labels, which fixes the slot order regardless
# of the order supplied.
resolve_fid_vector <- function(supplied, layout, label) {
  if (!is.numeric(supplied) && !all(is.na(supplied))) {
    cli::cli_abort(c(
      "The values for {.val {label}} must be numeric.",
      "x" = "A {.cls {class(supplied)[1]}} was supplied."
    ))
  }
  if (length(supplied) != layout$n_params) {
    cli::cli_abort(c(
      "The values for {.val {label}} must have one entry per coefficient.",
      "x" = "It has {length(supplied)} entr{?y/ies} but the process has
             {layout$n_params} coefficient{?s}
             ({.code {layout$names}})."
    ))
  }
  nm <- names(supplied)
  named <- if (is.null(nm)) logical(length(supplied)) else nzchar(nm)
  if (any(named) && !all(named)) {
    cli::cli_abort(c(
      "The values for {.val {label}} are only partly named.",
      "x" = "{sum(!named)} of {length(named)} entr{?y is/ies are} unnamed.",
      "i" = "Name every coefficient or none -- a partly named vector cannot be
             aligned unambiguously."
    ))
  }
  values <- rep(NA_real_, layout$n_params)
  if (all(named)) {
    unknown <- setdiff(nm, layout$names)
    if (length(unknown) > 0) {
      cli::cli_abort(c(
        "The values for {.val {label}} name coefficients the process does not
         have.",
        "x" = "Unknown name{?s}: {.val {unknown}}.",
        "i" = "The coefficients are {.code {layout$names}}."
      ))
    }
    if (anyDuplicated(nm) || length(unique(nm)) != layout$n_params) {
      cli::cli_abort(c(
        "The values for {.val {label}} must name every coefficient exactly
         once.",
        "i" = "The coefficients are {.code {layout$names}}."
      ))
    }
    values[match(nm, layout$names)] <- as.numeric(supplied)
  } else {
    values[] <- as.numeric(supplied)
  }
  values
}

#' Set parameter values over a joint specification
#'
#' `r lifecycle::badge("experimental")`
#'
#' `set_parameters()` builds a self-validating `parameters.goldfish`
#' object over a `joint_specification.goldfish`, the parameter surface both
#' `estimate_dynes()` (initial parameters) and `simulate()` (generative
#' coefficients) accept. Each `...` argument is a **full-length per-fid
#' vector** -- one entry per coefficient of that process, in coefficient order
#' (the intercept when the sub-model carries one, then the effects in formula
#' order, then the interaction columns) -- keyed by the process's **rendered
#' label**, the `layer > flavor > family` form that `coef()` / `print()` surface
#' for a fitted result (the flavor segment is present only when the process
#' carries a flavor).
#'
#' @details
#' A key is resolved by **membership** against the set of rendered labels, never
#' by splitting it back into components, so a key matching no label -- or
#' matching more than one -- aborts naming the valid labels. A per-fid vector
#' must have exactly the process's coefficient count; its entries may be named
#' or positional, but **all-or-nothing** -- a partly named vector is rejected,
#' and a fully named one must name every coefficient exactly once. A process
#' whose key is **omitted** is read as all-free (`NA` at every non-fixed slot).
#' Fixed
#' coefficients (`offset()` terms and operand-only interaction columns) always
#' take the specification's value.
#'
#' @param spec a `joint_specification.goldfish` (from
#'   [make_joint_specification()]).
#' @param ... one full-length per-fid vector per process, keyed by the process's
#'   rendered label.
#'
#' @return a `parameters.goldfish` object.
#'
#' @seealso [make_joint_specification()]
#' @export
set_parameters <- function(spec, ...) {
  if (!inherits(spec, "joint_specification.goldfish")) {
    cli::cli_abort(c(
      "{.fn set_parameters} requires a {.cls joint_specification.goldfish}.",
      "i" = "Compose processes with {.fn make_joint_specification}."
    ))
  }
  process_map <- spec$process_map
  bundles <- joint_fid_bundles(spec)
  fids <- process_map$fid
  labels <- render_process_label(process_map, fids)

  dots <- list(...)
  keys <- names(dots)
  if (length(dots) > 0 && (is.null(keys) || !all(nzchar(keys)))) {
    cli::cli_abort(c(
      "Every value passed to {.fn set_parameters} must be named by its
       process label.",
      "i" = "Valid labels: {.val {labels}}."
    ))
  }

  # Resolve each key to a fid by set membership against the rendered labels -- a
  # label is rendered for reading and never parsed back into (layer, flavor,
  # family), so a key that matches no label or more than one (a name carrying
  # the separator, or a genuine collision) is ambiguous and aborts.
  resolved <- integer(length(keys))
  for (k in seq_along(keys)) {
    hits <- which(labels == keys[k])
    if (length(hits) != 1) {
      matched <- if (length(hits) == 0) {
        "matches no process"
      } else {
        "matches more than one process"
      }
      cli::cli_abort(c(
        "The label {.val {keys[k]}} {matched}.",
        "i" = "Valid labels: {.val {labels}}."
      ))
    }
    resolved[k] <- hits
  }
  if (anyDuplicated(resolved)) {
    dup <- labels[unique(resolved[duplicated(resolved)])]
    cli::cli_abort(c(
      "Each process takes at most one value vector.",
      "x" = "Process{?es} {.val {dup}} {?was/were} given more than once."
    ))
  }

  entries <- vector("list", length(fids))
  for (i in seq_along(fids)) {
    layout <- fid_coefficient_layout(bundles[[as.character(fids[i])]])
    hit <- which(resolved == i)
    if (length(hit) == 0) {
      values <- rep(NA_real_, layout$n_params)
    } else {
      values <- resolve_fid_vector(dots[[hit]], layout, labels[i])
    }
    # The specification's fixed value always prevails at a fixed slot.
    values[layout$fixed] <- layout$fixed_values[layout$fixed]
    entries[[i]] <- list(
      values = values,
      fixed = layout$fixed,
      names = layout$names
    )
  }
  names(entries) <- as.character(fids)

  new_parameters_goldfish(process_map, entries)
}

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
