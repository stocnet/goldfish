# =========================================================================== #
# prepare_dynes_data(): turn panel layers given as wave states, wave amounts or
# change-lists into the change-list form the walk reads.
#
# The caller declares two facts per layer, never detected: the style (the
# process the layer records) and the form (what a value at a wave means). This
# file holds the argument validation (resolve_prepare_layers()), the checks on
# each layer's rows and starting state (check_prepare_layer()), the rewrite by
# form (diff_wave_states(), sum_wave_amounts(), or the pass-through), and the
# flavoring of a "mutually_exclusive" layer (flavor_prepared_layer()); every
# check runs before any row is rewritten.
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
#' @return `x` with the named layers rewritten. Their declared styles are
#'   recorded in `info$dynes_prepared`, a character vector named by layer, and a
#'   recorded layer cannot be prepared again. A layer whose flavor map is
#'   already recorded (by [add_flavor()]) aborts: the function writes the map
#'   itself, so pass such a layer in unflavored.
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

  checked <- lapply(seq_len(nrow(spec)), function(i) {
    check_prepare_layer(
      x,
      spec$layer[i],
      spec$style[i],
      spec$form[i],
      call = call
    )
  })

  diffed <- which(spec$form != "changes")
  written <- lapply(diffed, function(i) {
    shape <- prepare_layer_shape(x, spec$layer[i])
    rewrite <- if (spec$form[i] == "states") {
      diff_wave_states
    } else {
      sum_wave_amounts
    }
    rewrite(
      checked[[i]]$rows,
      checked[[i]]$start,
      checked[[i]]$waves,
      update = shape$update %||% "replace",
      template = x$ties$time
    )
  })
  names(written) <- spec$layer[diffed]
  if (length(written) > 0L) {
    x$ties <- write_prepared_layers(x, written)
  }
  for (layer in spec$layer[spec$style == "mutually_exclusive"]) {
    x <- flavor_prepared_layer(x, layer, flavor_names[[layer]], call = call)
  }

  x$info$dynes_prepared <- c(
    x$info$dynes_prepared,
    stats::setNames(spec$style, spec$layer)
  )
  x
}

# A layer recorded in info$dynes_prepared is already a change-list: diffing it
# again turns every stable tie missing from a later wave into a dissolution,
# and summing its amounts again double-counts. The record is a declared fact,
# so this is the one misuse caught without a heuristic.
check_not_prepared <- function(x, layer_names, call) {
  again <- intersect(layer_names, names(x$info$dynes_prepared))
  if (length(again) == 0L) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    c(
      "Layer{?s} {.val {again}} {?is/are} already prepared.",
      "x" = "{.fn prepare_dynes_data} records the layers it rewrote in \\
             {.field info$dynes_prepared}.",
      "i" = "Preparing a change-list again would read every stable tie \\
             missing from a later wave as a dissolution."
    ),
    call = call
  )
}

# prepare_dynes_data() writes the flavor map itself, so a layer whose map is
# already recorded aborts rather than passing through. The check reads the
# recorded map (info$values_equivalence), a declared fact, not the flavor
# column. A flavored count layer most likely came from add_flavor() run on a
# calls layer, so its error adds the count guidance.
check_not_flavored <- function(x, layer_names, style, call) {
  mapped <- names(Filter(
    Negate(is.null),
    x$info$values_equivalence %||% list()
  ))
  flavored <- intersect(layer_names, mapped)
  if (length(flavored) == 0L) {
    return(invisible(TRUE))
  }
  counts <- flavored[style[match(flavored, layer_names)] == "count"]
  message <- c(
    "Layer{?s} {.val {flavored}} {?is/are} already flavored.",
    "x" = "{.fn prepare_dynes_data} writes the flavor map itself.",
    "i" = "Pass the {cli::qty(flavored)}layer{?s} in unflavored: drop the \\
           {.field info$values_equivalence} entr{?y/ies}."
  )
  if (length(counts) > 0L) {
    message <- c(
      message,
      "i" = "A count layer needs no flavor: {.val {counts}} \\
             record{?s/} repeated events of one process.",
      "i" = "If a count layer counts two event types together, such as \\
             signings and endings, split it into one {.val count} layer per \\
             type."
    )
  }
  cli::cli_abort(
    message,
    class = "goldfish_layer_already_flavored",
    call = call
  )
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

  check_not_prepared(x, layer_names, call)
  check_not_flavored(x, layer_names, style, call)

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

  shape <- prepare_layer_shape(x, layer)
  one_mode <- shape$one_mode
  directed <- shape$directed

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

# What the checks need to know about one layer, resolved once: its mode map
# entry, whether it is one-mode and directed (an undeclared `directed` reads as
# directed), its update semantics, and the node labels used in messages.
prepare_layer_shape <- function(x, layer) {
  info <- x$info %||% list()
  mode_map <- build_mode_map(info, x$nodes, layer)
  list(
    mode_map = mode_map,
    mode = mode_map$layers[[layer]],
    one_mode = !isTRUE(mode_map$layers[[layer]]$is_two_mode),
    directed = !isFALSE(unname(info$directed[layer])),
    update = unname(info$update[layer]),
    labels = x$nodes$label %||% as.character(seq_len(nrow(x$nodes)))
  )
}

dyad_label <- function(shape, from, to) {
  paste0(shape$labels[from], "-", shape$labels[to])
}

is_whole_count <- function(value) {
  !is.na(value) & is.finite(value) & value >= 0 & value == round(value)
}

# Check one declared layer before anything is rewritten: the state before the
# first wave against the style, then either the wave rows (shared checks, the
# style's value checks, and for a count given as totals the rule that a total
# never falls) or the change-list in its own update semantics. Returns the
# starting levels, the cleaned wave rows and the layer's waves, for the rewrite.
check_prepare_layer <- function(x, layer, style, form, call) {
  shape <- prepare_layer_shape(x, layer)
  start <- starting_levels(x, layer, shape)
  check_starting_levels(start, layer, style, shape, call)

  if (identical(form, "changes")) {
    check_change_list(x, layer, style, start, shape, call)
    return(invisible(list(start = start, rows = NULL, waves = NULL)))
  }

  rows <- clean_wave_rows(x, layer, form, call = call)
  ties <- as.data.frame(x$ties)
  timed <- ties$time[ties$layer == layer & !is.na(ties$time)]
  waves <- sort(unique(timed))
  check_wave_values(rows, layer, style, shape, call)
  if (identical(style, "count") && identical(form, "states")) {
    check_totals_never_fall(rows, start, waves, layer, shape, call)
  }
  invisible(list(start = start, rows = rows, waves = waves))
}

# The level of every dyad before the first wave, folded from the layer's
# `time = NA` rows exactly as the walk folds them (materialize_network_state():
# summed under increment, last row winning under replace), so this function
# and the estimator cannot disagree on the starting state. Repeated history
# rows stay legal. Returns the non-zero levels as global `from`, `to`, `value`;
# an undirected one-mode layer is read on its upper triangle, the canonical key.
starting_levels <- function(x, layer, shape) {
  ties <- as.data.frame(x$ties)
  history <- ties[ties$layer == layer & is.na(ties$time), , drop = FALSE]
  if (nrow(history) == 0L) {
    return(data.frame(from = integer(), to = integer(), value = numeric()))
  }
  local <- remap_layer_refs(
    shape$mode_map,
    layer,
    history$from,
    history$to,
    x$nodes
  )
  weight <- if ("weight" %in% names(history)) {
    history$weight
  } else {
    rep(1, nrow(history))
  }
  stream <- data.frame(
    time = NA_real_,
    from = local$from,
    to = local$to,
    value = as.numeric(weight),
    update = shape$update %||% "replace",
    stringsAsFactors = FALSE
  )
  state <- materialize_network_state(
    stream,
    n1 = shape$mode$n1,
    n2 = shape$mode$n2,
    directed = shape$directed
  )
  cells <- which(state != 0, arr.ind = TRUE)
  if (shape$one_mode && !shape$directed) {
    cells <- cells[cells[, 1L] <= cells[, 2L], , drop = FALSE]
  }
  out <- data.frame(
    from = shape$mode$side1[cells[, 1L]],
    to = shape$mode$side2[cells[, 2L]],
    value = state[cells]
  )
  out <- out[order(out$from, out$to), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The starting state must suit the style: without this, two increment history
# rows (3, 7) = 1 fold to a level of 2, the diff to a wave-1 level of 1 writes
# -1, and the flavor map would label a 2 -> 1 move a dissolution.
check_starting_levels <- function(start, layer, style, shape, call) {
  bad <- switch(
    style,
    mutually_exclusive = start$value != 1,
    count = !is_whole_count(start$value),
    rep(FALSE, nrow(start))
  )
  if (!any(bad)) {
    return(invisible(TRUE))
  }
  i <- which(bad)[1L]
  dyad <- dyad_label(shape, start$from[i], start$to[i])
  cli::cli_abort(
    c(
      "The state of layer {.val {layer}} before its first wave does not suit \\
       its style {.val {style}}.",
      "x" = "The {.code time = NA} rows fold dyad {.val {dyad}} to a level \\
             of {.val {start$value[i]}}.",
      "i" = if (identical(style, "count")) {
        "A count level is a whole non-negative number."
      } else {
        "A binary tie is present (1) or absent (0)."
      }
    ),
    call = call
  )
}

# The style's checks on the cleaned wave rows, from which zeros are already
# dropped: a binary level is 1, a count a whole non-negative number. A "none"
# layer takes any value (NA is already refused by clean_wave_rows()).
check_wave_values <- function(rows, layer, style, shape, call) {
  bad <- switch(
    style,
    mutually_exclusive = rows$value != 1,
    count = !is_whole_count(rows$value),
    rep(FALSE, nrow(rows))
  )
  if (!any(bad)) {
    return(invisible(TRUE))
  }
  i <- which(bad)[1L]
  dyad <- dyad_label(shape, rows$from[i], rows$to[i])
  wave <- format(rows$time[i])
  cli::cli_abort(
    c(
      "Layer {.val {layer}} reports a value its style {.val {style}} cannot \\
       hold.",
      "x" = "Dyad {.val {dyad}} at wave {.val {wave}} is \\
             {.val {rows$value[i]}}.",
      "i" = if (identical(style, "count")) {
        "A count is a whole non-negative number."
      } else {
        "A binary tie is present (1) or absent (0); declare the style \\
         {.val count} or {.val none} for a valued layer."
      }
    ),
    call = call
  )
}

# On a count given as totals, a dyad's total never falls from one wave to the
# next. A dyad not reported at a wave has a total of 0 there. The starting state
# acts as a wave 0 for this check only: it is not a wave of the grid and writes
# nothing. Each dyad's waves are read in order once; the earliest fall is
# reported.
check_totals_never_fall <- function(rows, start, waves, layer, shape, call) {
  wave_axis <- coerce_time(waves)
  levels <- rbind(
    data.frame(
      from = start$from,
      to = start$to,
      k = rep(0L, nrow(start)),
      value = start$value
    ),
    data.frame(
      from = rows$from,
      to = rows$to,
      k = match(coerce_time(rows$time), wave_axis),
      value = rows$value
    )
  )
  n <- nrow(levels)
  if (n == 0L) {
    return(invisible(TRUE))
  }
  levels <- levels[order(levels$from, levels$to, levels$k), , drop = FALSE]
  same <- c(
    FALSE,
    levels$from[-1L] == levels$from[-n] & levels$to[-1L] == levels$to[-n]
  )
  prev_k <- c(NA_integer_, levels$k[-n])
  prev_value <- c(NA_real_, levels$value[-n])
  last <- c(!same[-1L], TRUE)

  # Three ways to fall: a reported total below the previous wave's; a wave
  # skipped after a positive total (unreported, so 0); a positive total
  # followed by no report at a later wave.
  dropped <- same & levels$k == prev_k + 1L & levels$value < prev_value
  skipped <- same & levels$k > prev_k + 1L
  ended <- last & levels$k < length(waves)
  falls <- data.frame(
    k = c(levels$k[dropped], prev_k[skipped] + 1L, levels$k[ended] + 1L),
    from = c(levels$from[dropped], levels$from[skipped], levels$from[ended]),
    to = c(levels$to[dropped], levels$to[skipped], levels$to[ended]),
    before = c(prev_value[dropped], prev_value[skipped], levels$value[ended]),
    after = c(
      levels$value[dropped],
      rep(0, sum(skipped)),
      rep(0, sum(ended))
    )
  )
  if (nrow(falls) == 0L) {
    return(invisible(TRUE))
  }
  first <- falls[order(falls$k, falls$from, falls$to)[1L], ]
  dyad <- dyad_label(shape, first$from, first$to)
  wave <- format(waves[first$k])
  cli::cli_abort(
    c(
      "A count total cannot fall.",
      "x" = "Layer {.val {layer}}: dyad {.val {dyad}} falls from \\
             {.val {first$before}} to {.val {first$after}} at wave \\
             {.val {wave}}.",
      "i" = if (first$k == 1L) {
        "The total before the first wave is folded from the \\
         {.code time = NA} rows."
      },
      "i" = "A dyad missing from a wave has a total of 0 there. If the \\
             values are amounts since the previous wave, declare \\
             {.code form = \"per_wave\"}."
    ),
    call = call
  )
}

# A change-list is checked in its own update semantics, reading the layer's
# timed rows once, in time order within each dyad, from the starting state.
#
# "mutually_exclusive": every value is in the update encoding add_flavor()
# requires, and no event contradicts the style (a creation where the tie is
# present, a dissolution where it is absent). add_flavor() only warns on this;
# here the output is meant to be estimator-ready, and the estimator would fail
# later on the same event ("observed dyad is excluded").
# "count": under increment every value is a whole number >= 0; under replace
# every value is a whole number and no dyad's level falls.
check_change_list <- function(x, layer, style, start, shape, call) {
  if (identical(style, "none")) {
    return(invisible(TRUE))
  }
  ties <- as.data.frame(x$ties)
  rows <- ties[ties$layer == layer & !is.na(ties$time), , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(invisible(TRUE))
  }
  from <- to_global_id(rows$from, x$nodes)
  to <- to_global_id(rows$to, x$nodes)
  if (shape$one_mode && !shape$directed) {
    lo <- pmin(from, to)
    to <- pmax(from, to)
    from <- lo
  }
  value <- if ("weight" %in% names(rows)) rows$weight else rep(1, nrow(rows))
  value <- as.numeric(value)
  time <- rows$time
  is_replace <- identical(shape$update, "replace")
  # `problem` and `hint` arrive formatted (cli::format_inline()), so the
  # values they name are interpolated where they are known.
  report <- function(i, problem, hint) {
    dyad <- dyad_label(shape, from[i], to[i])
    wave <- format(time[i])
    cli::cli_abort(
      c(
        "Layer {.val {layer}} is a change-list that does not suit its style \\
         {.val {style}}.",
        "x" = "Dyad {.val {dyad}} at wave {.val {wave}}: {problem}",
        "i" = "{hint}"
      ),
      call = call
    )
  }
  earliest <- function(candidates) {
    candidates[order(coerce_time(time[candidates]))[1L]]
  }

  if (identical(style, "mutually_exclusive")) {
    encoding <- flavor_update_values(shape$update)
    bad <- which(!value %in% encoding)
    if (length(bad) > 0L) {
      i <- earliest(bad)
      report(
        i,
        cli::format_inline(
          "the value {.val {value[i]}} is not an update of a binary tie."
        ),
        cli::format_inline(
          "Under {.val {shape$update}} a binary change is {.val {encoding}}."
        )
      )
    }
  } else {
    whole <- !is.na(value) & is.finite(value) & value == round(value)
    bad <- which(!whole | (!is_replace & value < 0))
    if (length(bad) > 0L) {
      i <- earliest(bad)
      report(
        i,
        cli::format_inline(
          "the value {.val {value[i]}} is not a whole number",
          if (is_replace) "." else " of at least 0."
        ),
        "A count rises by whole events and never falls."
      )
    }
  }

  # The level before each row, dyad by dyad in time order: the starting level
  # for a dyad's first row, then the level the previous row left.
  ord <- order(from, to, coerce_time(time), seq_along(time))
  n <- length(ord)
  f <- from[ord]
  t <- to[ord]
  v <- value[ord]
  first_of_dyad <- c(TRUE, f[-1L] != f[-n] | t[-1L] != t[-n])
  start_level <- start$value[match(paste(f, t), paste(start$from, start$to))]
  start_level[is.na(start_level)] <- 0
  after <- if (is_replace) {
    v
  } else {
    start_level + stats::ave(v, cumsum(first_of_dyad), FUN = cumsum)
  }
  before <- ifelse(first_of_dyad, start_level, c(NA_real_, after[-n]))

  if (identical(style, "mutually_exclusive")) {
    creates <- v == max(encoding)
    bad <- which((creates & before != 0) | (!creates & before == 0))
  } else {
    bad <- which(is_replace & v < before)
  }
  if (length(bad) == 0L) {
    return(invisible(TRUE))
  }
  j <- bad[order(coerce_time(time[ord][bad]))[1L]]
  if (!identical(style, "mutually_exclusive")) {
    report(
      ord[j],
      cli::format_inline(
        "the level falls from {.val {before[j]}} to {.val {v[j]}}."
      ),
      cli::format_inline(
        "A count level never falls; the level before the first row is ",
        "folded from the {.code time = NA} rows."
      )
    )
  }
  report(
    ord[j],
    if (creates[j]) {
      "a creation where the tie is already present."
    } else {
      "a dissolution where the tie is absent."
    },
    cli::format_inline(
      "Each event of a {.val mutually_exclusive} layer changes the tie's ",
      "state; declare {.code form = \"states\"} if the rows are the ties ",
      "reported at each wave."
    )
  )
}

# A grid time on the numeric scale of the ties `time` column. coerce_time()
# counts days for a Date and seconds for a POSIXct, so a Date grid against a
# POSIXct column is first read as midnight UTC of that day, and a POSIXct grid
# against a Date column as its day; both sides of a match then count the same
# unit.
time_on_column_scale <- function(time, template) {
  if (inherits(time, "Date") && inherits(template, "POSIXct")) {
    time <- as.POSIXct(time)
  } else if (inherits(time, "POSIXct") && inherits(template, "Date")) {
    time <- as.Date(time, tz = attr(time, "tzone") %||% "")
  }
  coerce_time(time)
}

# The change-list of one "states" layer: each wave compared with the level
# before it, by dyad key, never through an n1 x n2 matrix. The level before the
# first wave is `start`, folded from the `time = NA` rows as the walk folds
# them. A dyad absent from a wave is at 0 there, so a tie present before and
# missing now is a dissolution. A changed cell (exact `!=`, no tolerance: two
# doubles that print alike but differ still write a row that sets the second)
# gets one row in the layer's own semantics: the new level under replace, the
# difference under increment. `rows` are cleaned wave rows (zeros dropped,
# canonical keys); the written times are in the class of `template`, the ties
# `time` column. Returns `from`, `to` (global ids), `time` and `value`, sorted
# by time, then sender, then receiver.
diff_wave_states <- function(rows, start, waves, update, template) {
  wave_axis <- time_on_column_scale(waves, template)
  wave_of_row <- match(coerce_time(rows$time), wave_axis)
  is_replace <- identical(update, "replace")

  prev_from <- start$from
  prev_to <- start$to
  prev_value <- start$value
  per_wave <- vector("list", length(wave_axis))
  for (k in seq_along(wave_axis)) {
    at <- which(wave_of_row == k)
    from <- rows$from[at]
    to <- rows$to[at]
    level <- rows$value[at]
    key <- paste(from, to)
    prev_key <- paste(prev_from, prev_to)
    before <- prev_value[match(key, prev_key)]
    before[is.na(before)] <- 0
    changed <- level != before
    gone <- !prev_key %in% key
    new_level <- c(level[changed], rep(0, sum(gone)))
    old_level <- c(before[changed], prev_value[gone])
    per_wave[[k]] <- data.frame(
      from = c(from[changed], prev_from[gone]),
      to = c(to[changed], prev_to[gone]),
      k = rep(k, length(new_level)),
      value = if (is_replace) new_level else new_level - old_level
    )
    prev_from <- from
    prev_to <- to
    prev_value <- level
  }

  out <- do.call(
    rbind,
    c(
      list(data.frame(
        from = integer(),
        to = integer(),
        k = integer(),
        value = numeric()
      )),
      per_wave
    )
  )
  out <- out[order(out$k, out$from, out$to), , drop = FALSE]
  data.frame(
    from = out$from,
    to = out$to,
    time = restore_time_class(wave_axis[out$k], template),
    value = out$value
  )
}

# The ties table with each prepared layer's timed rows replaced by its written
# rows. Rows of other layers, and a prepared layer's `time = NA` rows, keep
# their position and every column. The written rows are appended per layer, in
# the order given, already sorted by time, sender and receiver. Columns the
# diff does not read (`order` among them) are left missing on them, since a
# snapshot holds one row per dyad per wave and has nothing to order. A missing
# value column is created, with the default weight 1 on the other rows. Node
# references keep the column's form: labels when the input used labels.
write_prepared_layers <- function(x, written) {
  ties <- as.data.frame(x$ties)
  if (!"weight" %in% names(ties)) {
    ties$weight <- rep(1, nrow(ties))
  }
  labels <- x$nodes$label
  as_refs <- function(id, column) {
    if (is.character(column)) labels[id] else id
  }
  rewritten <- ties$layer %in% names(written) & !is.na(ties$time)
  appended <- lapply(names(written), function(layer) {
    w <- written[[layer]]
    rows <- ties[rep(NA_integer_, nrow(w)), , drop = FALSE]
    rows$layer[] <- rep(layer, nrow(w))
    rows$from <- as_refs(w$from, ties$from)
    rows$to <- as_refs(w$to, ties$to)
    rows$time <- w$time
    rows$weight <- w$value
    rows
  })
  out <- do.call(rbind, c(list(ties[!rewritten, , drop = FALSE]), appended))
  rownames(out) <- NULL
  class(out) <- class(x$ties)
  out
}

# Flavor one prepared "mutually_exclusive" layer with the map add_flavor()
# records under flavor_style = "mutually_exclusive": the creation label at 1,
# the dissolution label at the lower value of the update encoding (-1 under
# increment, 0 under replace). It goes through add_flavor()'s own validation,
# record and contradiction check, so the two paths cannot write different maps.
# The labels only key the map and the flavor column; the support masks and the
# estimator read the mapped values. Only timed rows are stamped: `time = NA`
# rows seed the state and stay unflavored, as add_flavor() leaves them.
flavor_prepared_layer <- function(x, layer, labels, call) {
  labels <- labels %||% stats::setNames(flavor_roles, flavor_roles)
  update <- unname((x$info %||% list())$update[layer])
  encoding <- flavor_update_values(update)
  mapping <- stats::setNames(
    c(max(encoding), min(encoding)),
    unname(labels[flavor_roles])
  )
  validate_flavor_mapping(mapping, layer, update, call = call)

  ties <- x$ties
  weight <- if ("weight" %in% names(ties)) ties$weight else rep(1, nrow(ties))
  timed <- ties$layer == layer & !is.na(ties$time)
  flavor <- if ("flavor" %in% names(ties)) {
    ties$flavor
  } else {
    rep(NA_character_, nrow(ties))
  }
  flavor[timed] <- names(mapping)[match(weight[timed], unname(mapping))]
  x$ties$flavor <- flavor

  x$info <- x$info %||% list()
  style <- x$info$flavor_style %||% character(0)
  style[[layer]] <- "mutually_exclusive"
  x$info$flavor_style <- style
  map <- x$info$values_equivalence %||% list()
  map[[layer]] <- mapping
  x$info$values_equivalence <- map

  warn_style_contradicted(x, layer, mapping, call = call)
  x
}

# The change-list of one "per_wave" layer, whose value at a wave is the amount
# since the previous wave. A dyad's level at a wave is its level before the
# first wave (`start`) plus its amounts up to and including that wave, summed
# per dyad key in wave order. Each dyad reported at a wave gets one row, never
# one row per unit: the amount under increment, the running total under
# replace. A dyad with no row at a wave had no events there and writes nothing.
# Arguments and result as for diff_wave_states().
sum_wave_amounts <- function(rows, start, waves, update, template) {
  wave_axis <- time_on_column_scale(waves, template)
  k <- match(coerce_time(rows$time), wave_axis)
  ord <- order(rows$from, rows$to, k)
  from <- rows$from[ord]
  to <- rows$to[ord]
  k <- k[ord]
  amount <- rows$value[ord]
  dyad <- paste(from, to)
  start_level <- start$value[match(dyad, paste(start$from, start$to))]
  start_level[is.na(start_level)] <- 0
  level <- start_level + stats::ave(amount, dyad, FUN = cumsum)
  value <- if (identical(update, "replace")) level else amount
  out <- order(k, from, to)
  data.frame(
    from = from[out],
    to = to[out],
    time = restore_time_class(wave_axis[k[out]], template),
    value = value[out]
  )
}
