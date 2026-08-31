# =========================================================================== #
# goldfishParams: a self-validating parameter object over a joint spec's
# fid-indexed coefficient vectors.
#
# `set_init_param()` resolves the user-facing composite labels and the
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

# Build a `goldfishParams` from an already-resolved per-fid layout.
#
# `fids` is a named list keyed by `as.character(process_map$fid)`; each entry
# is `list(values, fixed, names)`, one coefficient-space slot per position
# (`n_params` long): `values` the resolved value (fixed slots already carry
# the specification's value; free slots are the user's pin or `NA`), `fixed`
# the logical fixed mask, `names` the coefficient's `coef()` name. This
# constructor does no label resolution or NA classification -- that is
# `set_init_param()`'s job; it only assembles the two projections, the
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
    class = "goldfishParams"
  )
}

is_parameters_goldfish <- function(x) inherits(x, "goldfishParams")

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
#' `set_init_param()` builds a self-validating `goldfishParams`
#' object over a `goldfishJointSpec`, the parameter surface both
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
#' Alternatively, `set_init_param(spec, result)` accepts a **fitted joint
#' result** in place of the per-fid vectors -- the fit -> re-simulate
#' round-trip.
#' The per-fid values are reconstructed from the result's own [coef_layout()]
#' (which keeps the `fid` grouping the flat [coef()] vector discards) after
#' asserting the result was fit against **that same `spec`**; a mismatch aborts.
#' A flat, free-only [coef()] vector is **not** accepted directly, because its
#' per-parameter names collide across fids.
#'
#' @param spec a `goldfishJointSpec` (from
#'   [make_joint_specification()]).
#' @param ... one full-length per-fid vector per process, keyed by the process's
#'   rendered label; or a single fitted joint result (the from-result form).
#'
#' @return a `goldfishParams` object.
#'
#' @seealso [make_joint_specification()]
#' @export
set_init_param <- function(spec, ...) {
  if (!inherits(spec, "goldfishJointSpec")) {
    cli::cli_abort(c(
      "{.fn set_init_param} requires a {.cls goldfishJointSpec}.",
      "i" = "Compose processes with {.fn make_joint_specification}."
    ))
  }
  dots <- list(...)
  # From-result form: `set_init_param(spec, result)` reconstructs the per-fid
  # vectors from a fitted joint result's own `coef_layout()` (the fit ->
  # re-simulate round-trip), which keeps the fid grouping the flat `coef()`
  # vector discards. A flat, free-only `coef()` vector is not accepted directly
  # because its per-parameter names collide across fids.
  if (length(dots) == 1L && inherits(dots[[1]], "flavored_result.goldfish")) {
    return(set_init_param_from_result(spec, dots[[1]]))
  }

  process_map <- spec$process_map
  bundles <- joint_fid_bundles(spec)
  fids <- process_map$fid
  labels <- render_process_label(process_map, fids)

  keys <- names(dots)
  if (length(dots) > 0 && (is.null(keys) || !all(nzchar(keys)))) {
    cli::cli_abort(c(
      "Every value passed to {.fn set_init_param} must be named by its
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
  # A fixed slot (an `offset()` term or an operand-only interaction) is resolved
  # by the specification, never by the user: a value supplied there is dropped
  # and the specification's value prevails. `NA` there is silent; a non-`NA`
  # value is collected so one warning can name every ignored coefficient.
  ignored <- character(0)
  for (i in seq_along(fids)) {
    layout <- fid_coefficient_layout(bundles[[as.character(fids[i])]])
    hit <- which(resolved == i)
    if (length(hit) == 0) {
      values <- rep(NA_real_, layout$n_params)
    } else {
      values <- resolve_fid_vector(dots[[hit]], layout, labels[i])
    }
    overridden <- layout$fixed & !is.na(values)
    if (any(overridden)) {
      ignored <- c(ignored, paste0(labels[i], ": ", layout$names[overridden]))
    }
    values[layout$fixed] <- layout$fixed_values[layout$fixed]
    entries[[i]] <- list(
      values = values,
      fixed = layout$fixed,
      names = layout$names
    )
  }
  names(entries) <- as.character(fids)

  if (length(ignored) > 0) {
    cli::cli_warn(c(
      "!" = "Value{?s} supplied for fixed coefficient{?s} {.val {ignored}}
             {?was/were} ignored.",
      "i" = "A fixed coefficient (an {.fn offset} term or an operand-only
             interaction) keeps the specification's value."
    ))
  }

  new_parameters_goldfish(process_map, entries)
}

# The from-result reconstruction. A fitted joint result and the `spec` it was
# fit against speak one coefficient vocabulary -- the same `coef_layout()`
# skeleton (fid grouping, process labels, coefficient names, fixed mask). This
# first asserts that skeleton matches so a result cannot be spliced onto a
# mismatched spec, then rebuilds one positional per-fid vector per process from
# the result's layout: the estimate at each free slot, `NA` at each fixed slot
# (the spec resolves the fixed value, so re-supplying it here would trip the
# offset-prevails warning). Feeding those back through `set_init_param()` yields
# a complete object a `simulate()` can drive.
set_init_param_from_result <- function(
  spec,
  result,
  call = rlang::caller_env()
) {
  spec_layout <- coef_layout(spec)
  result_layout <- coef_layout(result)
  skeleton <- c("fid", "process", "name", "fixed")
  mismatch <- nrow(spec_layout) != nrow(result_layout) ||
    !identical(spec_layout[skeleton], result_layout[skeleton])
  if (mismatch) {
    cli::cli_abort(
      c(
        "The result was not fit against this specification.",
        "x" = "Its coefficient layout does not match the specification's.",
        "i" = "Pass the same {.cls goldfishJointSpec} the result was
               estimated from."
      ),
      call = call
    )
  }

  # One positional per-fid vector, keyed by the rendered label, in coefficient
  # order (the layout's own order). A fixed slot is left `NA` so the spec's
  # value prevails without a warning.
  by_fid <- split(seq_len(nrow(result_layout)), result_layout$fid)
  by_fid <- by_fid[as.character(unique(result_layout$fid))] # canonical order
  vectors <- lapply(by_fid, function(idx) {
    v <- result_layout$value[idx]
    v[result_layout$fixed[idx]] <- NA_real_
    v
  })
  labels <- vapply(by_fid, function(idx) result_layout$process[idx][1L], "")
  do.call(set_init_param, c(list(spec), stats::setNames(vectors, labels)))
}

#' @export
#' @rdname print-method
#' @return For objects of class `goldfishParams` print a per-process
#'   breakdown of pinned, free, and fixed coefficients, and whether the object
#'   is complete.
print.goldfishParams <- function(x, ...) {
  cli::cli_rule(left = "{.cls goldfishParams}")
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

# =========================================================================== #
# Consumer acceptance surface. Both joint consumers -- `estimate_dynes()`
# (`initial_parameters=`, in abmcem) and `simulate()` (`coef=`, in
# process-simulation) -- route the user's parameter argument through these
# helpers, which live with the specification that owns the fid vocabulary rather
# than being reinvented in each consuming change. v1 accepts ONLY a
# `goldfishParams` (a bare list or flat numeric vector is a recorded
# non-goal), so the object's single construction-time validation is the one both
# consumers trust without re-validating.
# =========================================================================== #

# Accept only a `goldfishParams`, naming the consumer's own argument in the
# abort so the message reads as `initial_parameters` / `coef` rather than an
# internal name.
accept_joint_parameters <- function(x, arg, call = rlang::caller_env()) {
  if (!is_parameters_goldfish(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a {.cls goldfishParams}.",
        "x" = "A {.cls {class(x)[1]}} was supplied.",
        "i" = "Build one with {.fn set_init_param} over the joint
               specification."
      ),
      call = call
    )
  }
  x
}

# The free-parameter projection `estimate_dynes()` estimates (or warm-starts
# from). A partial object -- free `NA` slots -- is the normal warm-start case,
# so this asserts nothing about completeness; the fixed values live in the joint
# spec, not this vector.
joint_initial_parameters <- function(
  x,
  arg = "initial_parameters",
  call = rlang::caller_env()
) {
  accept_joint_parameters(x, arg = arg, call = call)
  x$free
}

# The full per-fid coefficient projection `simulate()` walks, gated on
# completeness: a free slot the user never pinned is a value-gate abort naming
# the effect. This gate is complementary to `walk_open()`'s structural gates
# (spec shape, not parameter values) and never overlaps them. Autocompleted
# defaults are absent from the object and resolved trivially at the consumer, so
# they never enter this gate.
joint_simulation_parameters <- function(
  x,
  arg = "coef",
  call = rlang::caller_env()
) {
  accept_joint_parameters(x, arg = arg, call = call)
  assert_joint_parameters_complete(x, arg = arg, call = call)
  x$full
}

# The value gate: abort unless every free coefficient of the authored fids is
# pinned, naming the unpinned effects. Autocompleted defaults never reach here
# (they are not in the object), so an object over authored fids with every free
# slot pinned is complete even when the completed spec carries more fids.
assert_joint_parameters_complete <- function(
  x,
  arg = "coef",
  call = rlang::caller_env()
) {
  unpinned <- names(x$free)[is.na(x$free)]
  if (length(unpinned) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} leaves {length(unpinned)} free coefficient{?s}
         unpinned.",
        "x" = "Unpinned: {.val {unpinned}}.",
        "i" = "Pin every free coefficient with {.fn set_init_param} before
               simulating."
      ),
      call = call
    )
  }
  invisible(x)
}

# Consumer-entry reconciliation (design D16). The object is built over the
# *authored* fid set; the consumer completes the spec, synthesizing extra
# autocompleted (zero-free-parameter) fids. Every fid of the *completed* spec
# absent from the object must be one of those autocompleted defaults --
# trivially resolved, needing no user value, not rendering the object
# incomplete. An object process the completed spec does not carry, or an absent
# process that is not autocompleted, means the object was built for a different
# specification.
reconcile_joint_parameters <- function(
  x,
  completed_spec,
  arg,
  call = rlang::caller_env()
) {
  accept_joint_parameters(x, arg = arg, call = call)
  # Completion reassigns fids (it rebuilds the process_map), so the object's raw
  # fid integers do not line up with the completed spec's. The rendered process
  # label is the stable identity across the raw/completed boundary, so
  # reconciliation is keyed on it.
  object_labels <- render_process_label(x$process_map, x$process_map$fid)
  map <- completed_spec$process_map
  spec_labels <- render_process_label(map, map$fid)
  completed <- map$completed %||% rep(FALSE, nrow(map))

  stray <- setdiff(object_labels, spec_labels)
  if (length(stray) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} was built for a different specification.",
        "x" = "It carries process{?es} {.val {stray}} the specification does
               not have."
      ),
      call = call
    )
  }
  absent <- setdiff(spec_labels, object_labels)
  not_autocompleted <- absent[!completed[match(absent, spec_labels)]]
  if (length(not_autocompleted) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} is missing an authored process.",
        "x" = "No values for {.val {not_autocompleted}}, which {?is/are} not an
               autocompleted default."
      ),
      call = call
    )
  }
  invisible(x)
}

# =========================================================================== #
# coef_layout(): the coefficient-space layout of a joint parameter surface as a
# tidy table -- one row per coefficient slot (`n_params`: an intercept when the
# sub-model carries one, the effects, the interaction columns), not one per
# effect. It serves three surfaces off one vocabulary: a `joint_specification`
# (the authoring layout, or -- on a completed spec -- the full pre-fit walked
# layout), a `goldfishParams` (the supplied values with the free/fixed
# classification), and a multi-process fitted result (the estimates and
# standard errors grouped back into the per-process blocks the flat `coef()`
# vector discards).
# Rows run in the canonical order -- `process_map` fid order, then coefficient
# order within each fid -- so the `index` column is the slot's position in the
# flat free-parameter vector.
# =========================================================================== #

# The descriptive `sub_model` per process_map row, recovered from the internal
# `stat_block` (`model:sub_model`). This is structured metadata, not a rendered
# label, so splitting it is safe -- unlike a process label, which is rendered
# for reading and never parsed back.
process_sub_models <- function(process_map) {
  sub("^[^:]+:", "", process_map$stat_block)
}

# Assemble a coef_layout table from per-fid blocks (a list in canonical fid
# order). Each block carries `fid`, the rendered `label`, `sub_model`, `flavor`,
# and per-slot `names` / `fixed` / `value` (and, for a fitted result, `se`). The
# flat-theta `index` is filled last, numbering every free slot in canonical
# order; fixed slots are not in theta and stay `NA`.
coef_layout_frame <- function(blocks) {
  parts <- lapply(blocks, function(b) {
    n <- length(b$names)
    df <- data.frame(
      fid = rep(b$fid, n),
      process = rep(b$label, n),
      sub_model = rep(b$sub_model, n),
      flavor = rep(b$flavor, n),
      name = b$names,
      fixed = b$fixed,
      value = b$value,
      stringsAsFactors = FALSE
    )
    if (!is.null(b$se)) {
      df$se <- b$se
    }
    df
  })
  out <- do.call(rbind, parts)
  if (is.null(out) || nrow(out) == 0L) {
    return(data.frame(
      fid = integer(0),
      process = character(0),
      sub_model = character(0),
      flavor = character(0),
      name = character(0),
      fixed = logical(0),
      value = numeric(0),
      index = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  out$index <- NA_integer_
  free <- !out$fixed
  if (any(free)) {
    out$index[free] <- seq_len(sum(free))
  }
  cols <- c(
    "fid",
    "process",
    "sub_model",
    "flavor",
    "name",
    "fixed",
    "value",
    if ("se" %in% names(out)) "se",
    "index"
  )
  out <- out[, cols]
  rownames(out) <- NULL
  out
}

# The layout block for one autocompleted-default fid: a zero-free-parameter
# sub-model `complete_generative_spec()` synthesized at consumer entry (a pinned
# intercept-only rate, or a uniform choice / coordination / ordered draw). It
# contributes no `coef()`-surfaced name, so it renders as a single fixed `"1"`
# placeholder row carrying the frozen value -- a completed timed rate's pinned
# log-hazard (from `$completed_rates`), or `NA` for a uniform draw that has no
# numeric coefficient.
autocompleted_layout_block <- function(spec, fid, label, sub_model, flavor) {
  frozen <- NA_real_
  rate <- spec$completed_rates[[as.character(fid)]]
  if (!is.null(rate)) {
    frozen <- as.numeric(rate$intercept)[[1L]]
  }
  list(
    fid = fid,
    label = label,
    sub_model = sub_model,
    flavor = flavor,
    names = "1",
    fixed = TRUE,
    value = frozen
  )
}

#' Tabulate the coefficient layout of a joint parameter surface
#'
#' `r lifecycle::badge("experimental")`
#'
#' `coef_layout()` returns the coefficient-space layout of a joint parameter
#' surface as a tidy data frame -- **one row per coefficient slot** (an
#' intercept when the sub-model carries one, then the effects in formula order,
#' then the interaction columns; not one row per effect). It serves three
#' surfaces off one label vocabulary: a `goldfishJointSpec` (the
#' empty authoring layout, so a user can discover the labels, names, and order
#' to author [set_init_param()]), a [goldfishParams][set_init_param] object
#' (the supplied values with the free/fixed classification), and a multi-process
#' fitted result (the estimates and standard errors grouped back into the
#' per-process blocks the flat [coef()] vector discards).
#'
#' @details
#' The `joint_specification` method is **completion-aware**. On a **raw**
#' (authored) specification it spans the authored fids only. On a **completed**
#' specification -- the output of `complete_generative_spec()`, still a
#' `goldfishJointSpec`, distinguished by its populated `completed`
#' column -- it additionally renders each autocompleted-default fid's rows as
#' `fixed = TRUE` with the `"1"` placeholder name and the frozen value, giving
#' the full pre-fit walked layout. The `goldfishParams` method spans the
#' **authored** fids only, so autocompleted-default rows appear on the completed
#' spec and fitted-result layouts, never on the authoring or parameter-object
#' layouts.
#'
#' Rows run in the canonical order (`process_map` fid order, then coefficient
#' order within each fid), so `index` is the slot's position in the flat
#' free-parameter vector.
#'
#' @param x a `goldfishJointSpec` (from
#'   [make_joint_specification()]), a `goldfishParams` (from
#'   [set_init_param()]), or a multi-process fitted result.
#' @param ... currently unused.
#'
#' @return a data frame with one row per coefficient slot and columns `fid`, the
#'   process `sub_model`, `flavor`, the effect `name`, a `fixed` logical, the
#'   fixed `value` (the offset value, `0` for an operand-only term, or the
#'   frozen value for an autocompleted default; `NA` for a free slot -- or, on a
#'   parameter object, the pinned value), and `index` (the slot's position in
#'   the flat free-parameter vector, `NA` for fixed rows); the process label is
#'   in the `process` column. The fitted-result method adds a `se` column.
#'
#' @seealso [set_init_param()], [make_joint_specification()]
#' @export
coef_layout <- function(x, ...) {
  UseMethod("coef_layout")
}

#' @export
#' @method coef_layout goldfishJointSpec
#' @rdname coef_layout
coef_layout.goldfishJointSpec <- function(x, ...) {
  process_map <- x$process_map
  bundles <- joint_fid_bundles(x)
  sub_models <- process_sub_models(process_map)
  completed <- process_map$completed %||% rep(FALSE, nrow(process_map))
  blocks <- vector("list", nrow(process_map))
  for (i in seq_len(nrow(process_map))) {
    fid <- process_map$fid[i]
    label <- render_process_label(process_map, fid)
    if (isTRUE(completed[i])) {
      blocks[[i]] <- autocompleted_layout_block(
        x,
        fid,
        label,
        sub_models[i],
        process_map$flavor[i]
      )
    } else {
      layout <- fid_coefficient_layout(bundles[[as.character(fid)]])
      blocks[[i]] <- list(
        fid = fid,
        label = label,
        sub_model = sub_models[i],
        flavor = process_map$flavor[i],
        names = layout$names,
        fixed = layout$fixed,
        value = layout$fixed_values
      )
    }
  }
  coef_layout_frame(blocks)
}

#' @export
#' @method coef_layout goldfishParams
#' @rdname coef_layout
coef_layout.goldfishParams <- function(x, ...) {
  process_map <- x$process_map
  sub_models <- process_sub_models(process_map)
  blocks <- vector("list", nrow(process_map))
  for (i in seq_len(nrow(process_map))) {
    fid <- process_map$fid[i]
    entry <- x$fids[[as.character(fid)]]
    blocks[[i]] <- list(
      fid = fid,
      label = render_process_label(process_map, fid),
      sub_model = sub_models[i],
      flavor = process_map$flavor[i],
      names = entry$names,
      fixed = entry$fixed,
      value = entry$values
    )
  }
  coef_layout_frame(blocks)
}

#' @export
#' @method coef_layout flavored_result.goldfish
#' @rdname coef_layout
coef_layout.flavored_result.goldfish <- function(x, ...) {
  process_map <- x$process_map
  sub_models <- process_sub_models(process_map)
  blocks <- vector("list", nrow(process_map))
  for (i in seq_len(nrow(process_map))) {
    fid <- process_map$fid[i]
    sub <- x$results[[as.character(fid)]]
    est <- sub$parameters
    fixed <- unname(GetFixed(sub))
    se <- rep(NA_real_, length(est))
    sev <- sub$standard_errors
    if (length(sev) == length(est)) {
      se <- sev
    } else if (length(sev) == sum(!fixed)) {
      se[!fixed] <- sev
    }
    blocks[[i]] <- list(
      fid = fid,
      label = render_process_label(process_map, fid),
      sub_model = sub_models[i],
      flavor = process_map$flavor[i],
      names = term_label(sub$names, ".coef_name", "coef"),
      fixed = fixed,
      value = est,
      se = se
    )
  }
  coef_layout_frame(blocks)
}
