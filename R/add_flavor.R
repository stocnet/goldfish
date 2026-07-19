# =========================================================================== #
# add_flavor(): stamp competing-process flavors on a layer's ties.
#
# A relational-state layer (friendship, treaties) evolves through competing
# sub-processes -- creation and dissolution. add_flavor() records that reading
# on the object: it stamps the reserved ties$flavor column from each event's
# update value via a value->flavor mapping, and records the mapping
# (values_equivalence) and the style (flavor_style) in the layer info. It is
# thin -- it derives NO support constraints (specification time owns that) and
# precomputes nothing.
#
# The flavor metadata vocabulary (flavor_style, values_equivalence) and its
# validation live here too, shared between the verb and the stocnet input
# validator (check_flavor_metadata(), called from validate_goldfish_data()).
# =========================================================================== #

# Allowed flavor_style values. mutually_exclusive: the flavors are competing
# directions on one state (creation/dissolution) that cannot both apply to a
# dyad, so a specification derives complementary support masks. redundant:
# repeated same-direction events are meaningful, so no constraint is derived.
flavor_styles <- c("mutually_exclusive", "redundant")

# The update-value set a dichotomous mapping must cover, per update semantics:
# increment layers toggle a state by +1 / -1, replace layers set 1 / 0. Any
# other update semantics carries no dichotomous state, so returns NULL.
flavor_update_values <- function(update) {
  switch(
    update %||% NA_character_,
    increment = c(-1, 1),
    replace = c(0, 1),
    NULL
  )
}

# Validate one flavor mapping (a named value->flavor vector) against a layer's
# update semantics: a named numeric vector, syntactic names, dichotomous, values
# matching the update's +-1 / 1-0 encoding. Shared by add_flavor() and the
# stocnet validator, so every entry path rejects the same bad mappings.
validate_flavor_mapping <- function(mapping, layer, update, call) {
  if (
    is.null(mapping) ||
      !is.numeric(mapping) ||
      is.null(names(mapping)) ||
      any(names(mapping) == "")
  ) {
    cli::cli_abort(
      c(
        "{.arg values_equivalence} for layer {.val {layer}} must be a named \\
         numeric vector mapping flavor names to update values.",
        "i" = "For example {.code c(creation = 1, dissolution = -1)}."
      ),
      call = call
    )
  }
  flavor_names <- names(mapping)
  bad <- flavor_names[make.names(flavor_names) != flavor_names]
  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "Flavor names in {.arg values_equivalence} must be syntactic R names.",
        "x" = "Non-syntactic name{?s}: {.val {bad}}.",
        "i" = "Flavor names key {.fn make_specification} formula lists."
      ),
      call = call
    )
  }
  if (length(mapping) != 2L || anyNA(mapping)) {
    cli::cli_abort(
      c(
        "{.arg values_equivalence} must map exactly two states \\
         (a dichotomous layer).",
        "x" = "Got {length(mapping)} value{?s} for layer {.val {layer}}.",
        "i" = "Only dichotomous state mappings are supported; a multistate \\
               encoding would accumulate (increment) or replace (replace) raw \\
               values -- consider {.code weighted = FALSE} in effect terms."
      ),
      call = call
    )
  }
  expected <- flavor_update_values(update)
  if (is.null(expected)) {
    cli::cli_abort(
      c(
        "Layer {.val {layer}} needs an {.field info$update} of \\
         {.val increment} or {.val replace} to carry flavors.",
        "x" = "Its update is {.val {update}}."
      ),
      call = call
    )
  }
  if (!setequal(as.numeric(mapping), expected)) {
    cli::cli_abort(
      c(
        "{.arg values_equivalence} values must match the layer's update \\
         encoding.",
        "x" = "Layer {.val {layer}} is {.val {update}}; expected values \\
               {.val {expected}} but got {.val {unname(mapping)}}.",
        "i" = if (identical(update, "increment")) {
          "Increment layers toggle a state by +1 / -1; weighted updates are \\
           not dichotomous states."
        } else {
          "Replace layers set 1 / 0."
        }
      ),
      call = call
    )
  }
  invisible(TRUE)
}

# Validate a single flavor_style value.
validate_flavor_style <- function(style, call) {
  if (
    !is.character(style) ||
      length(style) != 1L ||
      is.na(style) ||
      !style %in% flavor_styles
  ) {
    cli::cli_abort(
      c(
        "{.arg flavor_style} must be one of {.val {flavor_styles}}.",
        "x" = "Got {.val {style}}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

# Validate flavor metadata carried on a stocnet's info (info$flavor_style,
# info$values_equivalence). Both are optional and keyed per layer: flavor_style
# is a named character vector, values_equivalence a named list of per-layer
# mappings. add_info() does not type-check these, so a hand-built or
# add_flavor()-stamped object is re-checked here on every entry path.
check_flavor_metadata <- function(info, ties, layers, call) {
  style <- info$flavor_style
  veq <- info$values_equivalence
  if (is.null(style) && is.null(veq)) {
    return(invisible(TRUE))
  }
  if (!is.null(style)) {
    if (!is.character(style) || is.null(names(style))) {
      cli::cli_abort(
        "{.field info$flavor_style} must be a named character vector \\
         (one style per layer).",
        call = call
      )
    }
    unknown <- setdiff(names(style), layers)
    if (length(unknown) > 0) {
      cli::cli_abort(
        c(
          "{.field info$flavor_style} names must be layers present in \\
           {.field ties}.",
          "x" = "Unknown layer{?s}: {.val {unknown}}."
        ),
        call = call
      )
    }
    for (layer in names(style)) {
      validate_flavor_style(unname(style[layer]), call = call)
    }
  }
  if (!is.null(veq)) {
    if (!is.list(veq) || is.null(names(veq))) {
      cli::cli_abort(
        "{.field info$values_equivalence} must be a named list \\
         (one mapping per layer).",
        call = call
      )
    }
    unknown <- setdiff(names(veq), layers)
    if (length(unknown) > 0) {
      cli::cli_abort(
        c(
          "{.field info$values_equivalence} names must be layers present in \\
           {.field ties}.",
          "x" = "Unknown layer{?s}: {.val {unknown}}."
        ),
        call = call
      )
    }
    for (layer in names(veq)) {
      validate_flavor_mapping(
        veq[[layer]],
        layer,
        unname(info$update[layer]),
        call = call
      )
    }
  }
  invisible(TRUE)
}

#' Mark a layer's ties with competing-process flavors
#'
#' `r lifecycle::badge("experimental")`
#'
#' A relational-state layer -- friendship, alliances, treaties -- typically
#' evolves through competing sub-processes: ties are **created** and later
#' **dissolved**. `add_flavor()` records that reading on a `stocnet` object so a
#' [make_specification()] flavor-keyed `rate` / `choice` list can model each
#' sub-process as its own process. It is deliberately thin: it stamps the
#' reserved `ties$flavor` column from each event's update value and records the
#' mapping and style in the layer `info`. It derives **no** support constraints
#' -- specification time owns that (a `mutually_exclusive` layer's
#' create-only-where-absent / dissolve-only-where-present masks are derived by
#' [make_specification()]).
#'
#' Only **dichotomous** states are supported: increment layers whose events are
#' `+1` / `-1`, and replace layers whose events set `1` / `0`. A weighted layer
#' (increments other than `+-1`), a multistate mapping, or a mapping that does
#' not match the layer's update encoding aborts, because those updates
#' accumulate or replace raw values rather than toggling a state; see the
#' `weighted` argument of effect terms for the usual compromise.
#'
#' Only timed event rows are flavored; `time = NA` history rows seed the initial
#' state and stay unflavored.
#'
#' @param x a `stocnet` object (see [goldfish_data]).
#' @param layer character(1) naming the layer whose ties to flavor.
#' @param values_equivalence a named numeric vector mapping flavor names to the
#'   layer's update values, e.g. `c(creation = 1, dissolution = -1)` for an
#'   increment layer or `c(creation = 1, dissolution = 0)` for a replace layer.
#'   Names must be syntactic R names -- they become [make_specification()]
#'   formula-list keys.
#' @param flavor_style one of `"mutually_exclusive"` (competing directions on
#'   one state, the default -- creation and dissolution cannot both apply to a
#'   dyad, so a specification derives complementary support masks) or
#'   `"redundant"` (repeated same-direction events are meaningful, so no
#'   constraint is derived).
#'
#' @return `x` with `ties$flavor` stamped for `layer`, and the mapping and style
#'   recorded in `info$values_equivalence` and `info$flavor_style`.
#'
#' @seealso [make_specification()] for modeling flavors; [goldfish_data] for the
#'   data object and the reserved `flavor` column.
#' @export
#' @examplesIf rlang::is_installed("manynet")
#' data("fisheries_treaties")
#' fish <- add_flavor(
#'   fisheries_treaties,
#'   layer = "treaties",
#'   values_equivalence = c(creation = 1, dissolution = -1)
#' )
add_flavor <- function(
  x,
  layer,
  values_equivalence,
  flavor_style = "mutually_exclusive"
) {
  call <- rlang::current_env()
  if (!is.list(x) || is.null(x$ties) || is.null(x$ties$layer)) {
    cli::cli_abort(
      c(
        "{.arg x} must be a stocnet object with a {.field ties} table.",
        "i" = "Build one with {.fn manynet::make_stocnet} or the goldfish \\
               constructors (see {.help goldfish_data})."
      ),
      call = call
    )
  }
  layers <- unique(as.character(x$ties$layer))
  if (!is.character(layer) || length(layer) != 1L || !layer %in% layers) {
    cli::cli_abort(
      c(
        "{.arg layer} must name a single layer present in {.field ties}.",
        "x" = "{.val {layer}} is not among {.val {layers}}."
      ),
      call = call
    )
  }

  validate_flavor_style(flavor_style, call = call)
  update <- unname((x$info %||% list())$update[layer])
  validate_flavor_mapping(values_equivalence, layer, update, call = call)

  ties <- x$ties
  weight <- if ("weight" %in% names(ties)) ties$weight else rep(1, nrow(ties))
  timed_rows <- ties$layer == layer & !is.na(ties$time)
  uncovered <- setdiff(unique(weight[timed_rows]), unname(values_equivalence))
  if (length(uncovered) > 0) {
    cli::cli_abort(
      c(
        "Layer {.val {layer}} carries update value{?s} outside \\
         {.arg values_equivalence}.",
        "x" = "Uncovered value{?s}: {.val {uncovered}}.",
        "i" = "Only dichotomous {.val {unname(values_equivalence)}} states are \\
               supported; a weighted layer is not a creation/dissolution \\
               toggle -- consider {.code weighted = FALSE} in effect terms."
      ),
      call = call
    )
  }

  flavor <- if ("flavor" %in% names(ties)) {
    ties$flavor
  } else {
    rep(NA_character_, nrow(ties))
  }
  flavor[timed_rows] <- names(values_equivalence)[
    match(weight[timed_rows], unname(values_equivalence))
  ]
  x$ties$flavor <- flavor

  x$info <- x$info %||% list()
  style_vec <- x$info$flavor_style %||% character(0)
  style_vec[[layer]] <- flavor_style
  x$info$flavor_style <- style_vec
  veq <- x$info$values_equivalence %||% list()
  veq[[layer]] <- values_equivalence
  x$info$values_equivalence <- veq

  x
}
