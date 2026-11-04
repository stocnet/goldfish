# =========================================================================== #
# prepare_dynes_data(): turn panel layers given as wave states, wave amounts or
# change-lists into the change-list form the walk reads.
#
# The caller declares two facts per layer, never detected: the style (the
# process the layer records) and the form (what a value at a wave means). This
# file holds the argument validation and the pass-through; the checks on the
# rows and the rewrite build on the resolved declaration returned by
# resolve_prepare_layers().
# =========================================================================== #

prepare_styles <- c("mutually_exclusive", "count", "none")
prepare_forms <- c("states", "per_wave", "changes")
flavor_roles <- c("creation", "dissolution")

#' Prepare panel layers for DyNES estimation
#'
#' `r lifecycle::badge("experimental")`
#'
#' Survey data reports the state at each wave, while the walk reads a panel
#' layer as a list of updates applied in time order. `prepare_dynes_data()`
#' rewrites the named panel layers into that change-list form, so the state the
#' walk builds at every wave equals the survey.
#'
#' Two facts are declared per layer and never detected, because
#' survey-recorded interactions repeated at each wave are a legitimate shape
#' that a heuristic would misread:
#'
#' * the **style** (`layers`): the process the layer records.
#'   `"mutually_exclusive"` is a binary tie that is created and dissolved,
#'   `"count"` is one monotone process of repeated events, and `"none"` is a
#'   valued layer with no process, typically a covariate.
#' * the **form** (`form`): what a value at a wave means. `"states"` (the
#'   default) is the level at the wave, `"per_wave"` is the amount since the
#'   previous wave, and `"changes"` means the rows already are a change-list
#'   in the layer's `update` semantics.
#'
#' A tie that is present or absent has no amount since the previous wave, so
#' `"mutually_exclusive"` cannot be combined with `"per_wave"`. A `"count"`
#' layer needs no flavor: it is estimated as a count panel. A layer that counts
#' two event types together should be split into one `"count"` layer per type.
#'
#' @param x a `stocnet` object (see [goldfish_data]).
#' @param layers a named character vector. Each name is a panel layer of `x`
#'   and each value its style: `"mutually_exclusive"`, `"count"` or `"none"`.
#' @param form a named character vector giving the form of a layer: `"states"`,
#'   `"per_wave"` or `"changes"`. A layer in `layers` without an entry is
#'   `"states"`.
#' @param flavor_names a list keyed by layer. Each entry is a character vector
#'   named by the roles `creation` and `dissolution`, whose values relabel the
#'   two flavors of a `"mutually_exclusive"` layer, for example
#'   `list(friendship = c(creation = "formed", dissolution = "ended"))`.
#' @param wave_times either one vector of times applied to every layer, or a
#'   named list with a vector per layer.
#'
#' @return `x` with the named layers rewritten.
#' @seealso [add_flavor()] for flavoring an event layer.
#' @export
prepare_dynes_data <- function(
  x,
  layers,
  form = NULL,
  flavor_names = NULL,
  wave_times = NULL
) {
  call <- rlang::current_env()
  spec <- resolve_prepare_layers(x, layers, form, flavor_names, call = call)

  for (i in which(spec$form != "changes")) {
    clean_wave_rows(x, spec$layer[i], spec$form[i], call = call)
  }

  needs_rewrite <- spec$form != "changes" | spec$style == "mutually_exclusive"
  if (any(needs_rewrite)) {
    cli::cli_abort(
      "Rewriting {.val {spec$layer[needs_rewrite]}} is not implemented yet.",
      call = call
    )
  }
  x
}

# Check the declaration and resolve it to one row per layer: the layer, its
# style and its form (`"states"` when the layer has no `form` entry). Every
# abort here comes before any row is touched.
resolve_prepare_layers <- function(x, layers, form, flavor_names, call) {
  check_prepare_object(x, call)
  check_declared_vector(
    layers,
    "layers",
    prepare_styles,
    call,
    required = TRUE
  )
  check_declared_vector(form, "form", prepare_forms, call, required = FALSE)

  layer_names <- names(layers)
  style <- unname(layers)
  form_by_layer <- rep("states", length(layers))
  if (length(form) > 0L) {
    unknown <- setdiff(names(form), layer_names)
    if (length(unknown) > 0L) {
      cli::cli_abort(
        c(
          "{.arg form} names a layer that is not in {.arg layers}.",
          "x" = "Layer{?s} not in {.arg layers}: {.val {unknown}}."
        ),
        call = call
      )
    }
    form_by_layer[match(names(form), layer_names)] <- unname(form)
  }

  available <- unique(as.character(x$ties$layer))
  absent <- setdiff(layer_names, available)
  if (length(absent) > 0L) {
    cli::cli_abort(
      c(
        "{.arg layers} names a layer that is not in {.field ties}.",
        "x" = "Absent layer{?s}: {.val {absent}}.",
        "i" = "The layers of {.arg x} are {.val {available}}."
      ),
      call = call
    )
  }
  observation <- (x$info %||% list())$observation
  not_panel <- layer_names[
    vapply(
      layer_names,
      function(layer) !identical(unname(observation[layer]), "panel"),
      logical(1)
    )
  ]
  if (length(not_panel) > 0L) {
    cli::cli_abort(
      c(
        "Only panel layers can be prepared.",
        "x" = "Layer{?s} not observed as {.val panel}: {.val {not_panel}}.",
        "i" = "Event layers keep their times; use {.fn add_flavor} to flavor \\
               one."
      ),
      call = call
    )
  }

  no_amount <- style == "mutually_exclusive" & form_by_layer == "per_wave"
  if (any(no_amount)) {
    cli::cli_abort(
      c(
        "A binary tie has no amount per wave.",
        "x" = "Layer{?s} {.val {layer_names[no_amount]}} {?is/are} declared \\
               {.val mutually_exclusive} with form {.val per_wave}.",
        "i" = "Declare the form {.val states} or {.val changes}, or the style \\
               {.val count}."
      ),
      call = call
    )
  }

  check_flavor_names(flavor_names, layer_names, style, call)

  data.frame(
    layer = layer_names,
    style = style,
    form = form_by_layer,
    stringsAsFactors = FALSE
  )
}

check_prepare_object <- function(x, call) {
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
  invisible(TRUE)
}

# A named character vector whose values come from `vocabulary`. `required`
# makes an empty or missing vector an error; every entry must be named and no
# name may repeat.
check_declared_vector <- function(
  value,
  arg,
  vocabulary,
  call,
  required
) {
  if (is.null(value) || length(value) == 0L) {
    if (required) {
      cli::cli_abort(
        c(
          "{.arg {arg}} must name at least one layer.",
          "i" = "For example {.code {arg} = c(friendship = \\
                 \"mutually_exclusive\")}."
        ),
        call = call
      )
    }
    return(invisible(TRUE))
  }
  if (!is.character(value) || anyNA(value)) {
    cli::cli_abort(
      "{.arg {arg}} must be a named character vector.",
      call = call
    )
  }
  nms <- names(value)
  if (is.null(nms) || any(is.na(nms) | nms == "")) {
    cli::cli_abort(
      c(
        "Every entry of {.arg {arg}} must be named by its layer.",
        "x" = if (identical(arg, "layers")) {
          "Every layer needs a declared style, one of {.val {vocabulary}}."
        } else {
          "Every layer needs a declared form, one of {.val {vocabulary}}."
        }
      ),
      call = call
    )
  }
  repeated <- unique(nms[duplicated(nms)])
  if (length(repeated) > 0L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} names a layer more than once.",
        "x" = "Repeated layer{?s}: {.val {repeated}}."
      ),
      call = call
    )
  }
  outside <- unique(value[!value %in% vocabulary])
  if (length(outside) > 0L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must use one of {.val {vocabulary}}.",
        "x" = "Not in the vocabulary: {.val {outside}}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

# `flavor_names` relabels the two flavors of a "mutually_exclusive" layer. The
# labels key make_specification()'s formula lists, so they must be syntactic
# and distinct; the fixed roles tie each label to its update value.
check_flavor_names <- function(flavor_names, layer_names, style, call) {
  if (is.null(flavor_names) || length(flavor_names) == 0L) {
    return(invisible(TRUE))
  }
  nms <- names(flavor_names)
  if (
    !is.list(flavor_names) ||
      is.null(nms) ||
      any(is.na(nms) | nms == "")
  ) {
    cli::cli_abort(
      "{.arg flavor_names} must be a list named by layer.",
      call = call
    )
  }
  repeated <- unique(nms[duplicated(nms)])
  if (length(repeated) > 0L) {
    cli::cli_abort(
      c(
        "{.arg flavor_names} names a layer more than once.",
        "x" = "Repeated layer{?s}: {.val {repeated}}."
      ),
      call = call
    )
  }
  unknown <- setdiff(nms, layer_names)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "{.arg flavor_names} names a layer that is not in {.arg layers}.",
        "x" = "Layer{?s} not in {.arg layers}: {.val {unknown}}."
      ),
      call = call
    )
  }
  for (layer in nms) {
    layer_style <- style[match(layer, layer_names)]
    if (identical(layer_style, "count")) {
      cli::cli_abort(
        c(
          "A count layer needs no flavor.",
          "x" = "{.arg flavor_names} has an entry for the count layer \\
                 {.val {layer}}.",
          "i" = "A count records repeated events of one process.",
          "i" = "If {.val {layer}} counts two event types together, such as \\
                 signings and endings, split it into one {.val count} layer \\
                 per type."
        ),
        call = call
      )
    }
    if (!identical(layer_style, "mutually_exclusive")) {
      cli::cli_abort(
        c(
          "Only a {.val mutually_exclusive} layer has flavors.",
          "x" = "Layer {.val {layer}} is declared {.val {layer_style}}."
        ),
        call = call
      )
    }
    labels <- flavor_names[[layer]]
    if (
      !is.character(labels) ||
        anyNA(labels) ||
        !setequal(names(labels), flavor_roles) ||
        length(labels) != length(flavor_roles)
    ) {
      cli::cli_abort(
        c(
          "The {.arg flavor_names} entry for {.val {layer}} must be named by \\
           the roles {.val {flavor_roles}}.",
          "i" = "For example {.code c(creation = \"formed\", dissolution = \\
                 \"ended\")}."
        ),
        call = call
      )
    }
    bad <- labels[make.names(labels) != labels]
    if (length(bad) > 0L) {
      cli::cli_abort(
        c(
          "Flavor labels must be syntactic R names.",
          "x" = "Layer {.val {layer}} has non-syntactic label{?s}: \\
                 {.val {unname(bad)}}.",
          "i" = "Flavor labels key {.fn make_specification} formula lists."
        ),
        call = call
      )
    }
    if (anyDuplicated(labels) > 0L) {
      cli::cli_abort(
        c(
          "The two flavor labels must differ.",
          "x" = "Layer {.val {layer}} labels both flavors \\
                 {.val {unname(labels[1L])}}."
        ),
        call = call
      )
    }
  }
  invisible(TRUE)
}

# The timed rows of one "states" or "per_wave" layer, checked and normalized:
# one row per dyad per wave, a value of 0 read as absence (no tie, or no
# events) and dropped, keys in the order the diff compares them. The
# `time = NA` rows are history and are neither checked nor returned: the walk
# folds them, so repeated history rows stay legal. Returns a data frame with
# `from`, `to` (global node ids), `time` and `value`.
#
# On an undirected one-mode layer (i, j) and (j, i) are the same dyad, keyed
# (min, max). Reported both ways at one wave with one value they warn and count
# once, as two respondents reporting the same tie or the same count; with two
# values they are a repeated dyad. A two-mode layer indexes different node sides
# with `from` and `to`, so it is never reordered and has no diagonal.
clean_wave_rows <- function(x, layer, form, call) {
  ties <- as.data.frame(x$ties)
  rows <- ties[ties$layer == layer & !is.na(ties$time), , drop = FALSE]
  value <- if ("weight" %in% names(rows)) rows$weight else rep(1, nrow(rows))
  out <- data.frame(
    from = to_global_id(rows$from, x$nodes),
    to = to_global_id(rows$to, x$nodes),
    time = rows$time,
    value = as.numeric(value)
  )
  labels <- x$nodes$label %||% as.character(seq_len(nrow(x$nodes)))
  describe <- function(i) {
    list(
      wave = format(out$time[i]),
      dyad = paste0(labels[out$from[i]], "-", labels[out$to[i]])
    )
  }

  if (anyNA(out$value)) {
    at <- describe(which(is.na(out$value))[1L])
    cli::cli_abort(
      c(
        "Layer {.val {layer}} has a missing tie value.",
        "x" = "Dyad {.val {at$dyad}} at wave {.val {at$wave}} is {.val {NA}}.",
        "i" = "A wave must report every tie it holds; partially observed \
               waves are not supported."
      ),
      call = call
    )
  }

  mode_info <- build_mode_map(x$info %||% list(), x$nodes, layer)$layers[[
    layer
  ]]
  one_mode <- !isTRUE(mode_info$is_two_mode)
  directed <- !isFALSE(unname((x$info %||% list())$directed[layer]))

  if (one_mode) {
    self <- which(out$from == out$to)
    if (length(self) > 0L) {
      at <- describe(self[1L])
      cli::cli_abort(
        c(
          "Layer {.val {layer}} has a tie from a node to itself.",
          "x" = "Node {.val {labels[out$from[self[1L]]]}} at wave \
                 {.val {at$wave}}.",
          "i" = "A one-mode layer has no diagonal."
        ),
        call = call
      )
    }
  }

  swapped <- !directed & one_mode & out$from > out$to
  lo <- ifelse(swapped, out$to, out$from)
  hi <- ifelse(swapped, out$from, out$to)
  key <- paste(coerce_time(out$time), lo, hi)
  repeated <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (any(repeated)) {
    groups <- split(which(repeated), key[repeated])
    for (g in groups) {
      reversed <- length(g) == 2L && !directed && one_mode && any(swapped[g])
      same_value <- length(unique(out$value[g])) == 1L
      if (!(reversed && same_value && !all(swapped[g]))) {
        at <- describe(g[1L])
        cli::cli_abort(
          c(
            "A wave must hold one value per dyad.",
            "x" = "Layer {.val {layer}} reports dyad {.val {at$dyad}} \
                   {length(g)} times at wave {.val {at$wave}}.",
            "i" = if (reversed && !same_value) {
              "The two reports of this undirected tie disagree: \
               {.val {out$value[g]}}."
            }
          ),
          call = call
        )
      }
    }
    doubled <- vapply(groups, `[`, integer(1), 1L)
    at <- describe(doubled[1L])
    cli::cli_warn(
      c(
        "Layer {.val {layer}} reports {length(doubled)} undirected \
         dyad{?s} in both orders with the same value.",
        "i" = "First: {.val {at$dyad}} at wave {.val {at$wave}}.",
        "i" = "Each is counted once, not twice."
      ),
      call = call
    )
  }

  out$from <- lo
  out$to <- hi
  out <- out[!(duplicated(key) | out$value == 0), , drop = FALSE]
  rownames(out) <- NULL
  out
}
