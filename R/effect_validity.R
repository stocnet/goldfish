# Effect side signatures --------------------------------------------------
#
# A model over a focal layer S1 -> S2 computes a statistic indexed by the
# sender (rate) or by the (sender, receiver) dyad (choice, REM). Every index
# into a network argument must land on a side that the argument's OWN layer can
# hold: `w[a, b]` requires a to be in the argument's sender side R1 and b in its
# receiver side R2. Each effect below records that requirement as equations
# between node sets, read off the effect's own formula -- `common_sender` sums
# w[k, i] * w[k, j], so both i and j index R2 -- rather than as a per-effect
# "works on two-mode" verdict, which cannot separate the focal pair from an
# argument's pair or express a type variant.
#
# Sets are compared by identity of their node ids (the mode sets the mode map
# resolved), never by cardinality: two distinct modes of equal size must not
# conform.

# Tokens: S1/S2 are the focal layer's sides; R1/R2 the sides of a single
# network argument; A1/A2 and B1/B2 the sides of the first and second layer of
# a mixed-family `list(net1, net2)` argument.
effect_side_requirements <- function(effect, type = NULL, sub_model = NULL) {
  # In the rate submodel the statistic is indexed by the sender alone, so every
  # ego/alter variant collapses onto the sender side.
  is_ego_read <- identical(sub_model, "rate") || identical(type, "ego")
  dyad <- c("R1=S1", "R2=S2")
  square <- c("R1=R2", "R1=S1", "R2=S2")
  # `cache + t(cache)` in the mixed shared-partner effects symmetrizes the
  # statistic, which a two-mode focal layer cannot hold.
  switch(
    effect,
    inertia = dyad,
    tie = dyad,
    four = dyad,
    tertius_diff = dyad,
    recip = c("R1=S2", "R2=S1"),
    trans = square,
    cycle = square,
    triangle = square,
    node_trans = if (identical(sub_model, "rate")) {
      c("R1=R2", "R1=S1")
    } else {
      square
    },
    degree = c("R1=R2", "R1=S1"),
    common_sender = c("R2=S1", "R2=S2"),
    common_receiver = c("R1=S1", "R1=S2"),
    indeg = if (is_ego_read) "R2=S1" else "R2=S2",
    outdeg = if (is_ego_read) "R1=S1" else "R1=S2",
    tertius = if (is_ego_read) "R2=S1" else "R2=S2",
    mixed_trans = c("A1=S1", "A2=B1", "B2=S2"),
    mixed_cycle = c("A1=S1", "A2=B1", "B2=S2"),
    mixed_common_receiver = c("A1=S1", "B1=S2", "A2=B2", "S1=S2"),
    mixed_common_sender = c("A2=S1", "B2=S2", "A1=B1", "S1=S2"),
    NULL
  )
}

# Effects whose type variant selects which side it reads. On an argument that
# spans the focal layer's own mode pair, the wrong variant is not merely
# non-conformable: it counts ties that cannot exist (senders never receive), so
# the statistic is a structural zero and the coefficient is not identified.
is_degenerate_read <- function(effect) {
  effect %in% c("indeg", "outdeg", "tertius")
}

is_same_pair <- function(layer_map, focal_map) {
  setequal(layer_map$side1, focal_map$side1) &&
    setequal(layer_map$side2, focal_map$side2)
}

# What a two-mode focal layer does admit, for the "use one of these instead"
# bullet. Ordered as the taxonomy reads: dyadic, degree, closure, covariate.
two_mode_alternatives <- function() {
  c(
    "inertia()",
    "tie()",
    "indeg(type = \"alter\")",
    "outdeg(type = \"ego\")",
    "four()",
    "ego()",
    "alter()",
    "same()",
    "diff()",
    "sim()",
    "ego_alter_interaction()",
    "tertius(type = \"alter\")",
    "tertius_diff()",
    "global()"
  )
}

# The layer names a term's network argument refers to. A mixed-family argument
# is written `list(net1, net2)` and carries two.
term_layer_names <- function(arg_name) {
  if (is.null(arg_name) || is.na(arg_name) || !nzchar(arg_name)) {
    return(character())
  }
  if (!grepl("^list\\(", arg_name)) {
    return(arg_name)
  }
  inner <- sub("^list\\(\\s*", "", sub("\\s*\\)$", "", arg_name))
  trimws(strsplit(inner, ",")[[1]])
}

# Resolve the `type` an effect will run with from its parsed signature, so the
# gate checks the variant the user actually asked for. A formal left at its
# default `c("alter", "ego")` resolves to the first value, as `match.arg` would.
resolve_effect_type <- function(signature, envir) {
  if (!"type" %in% names(signature)) {
    return(NULL)
  }
  value <- tryCatch(
    eval(signature[["type"]], envir = envir),
    error = function(e) NULL
  )
  if (is.null(value) || !is.character(value)) {
    return(NULL)
  }
  value[1]
}

#' Check an effect's side signature against the data
#'
#' Validates one parsed term: each index position of the effect reads a side of
#' the focal layer or of its own network argument, and the node sets those
#' positions require must coincide. Silent when the source carries no mode map
#' (nothing to resolve against) or the effect has no network argument -- an
#' attribute read is checked where the attribute is sliced, not here.
#'
#' @param effect effect name as written in the formula.
#' @param arg_name the network argument as written, or `NULL`.
#' @param type the resolved `type` variant, or `NULL`.
#' @param src a data source.
#' @param model,sub_model the model and submodel being specified.
#' @param call the calling environment, for the error's context. `NULL` (the
#'   default) keeps the parser's internal frames out of the message.
#' @noRd
#' Check that an effect's attribute reads are defined on the mode they read
#'
#' Rule R3 of the side contract. An attribute is a column of one nodes table
#' spanning every mode, so a variable that only makes sense for one mode is NA
#' on the others -- reading it on a mode where it is wholly undefined yields an
#' all-NA statistic, which imputation then silently fills with a mean of
#' nothing. Definedness is therefore a property of the (attribute, mode) pair
#' and is reported that way, per mode rather than pooled.
#'
#' Partial missingness is left alone: that is ordinary missing data, and
#' imputation is its contract.
#'
#' @param effect effect name as written in the formula.
#' @param refs the term's resolved object references.
#' @param src a data source.
#' @param call the calling environment, for the error's context.
#' @noRd
check_effect_attributes <- function(effect, refs, src, call = NULL) {
  if (!inherits(src, "data_source_stocnet")) {
    return(invisible(NULL))
  }
  for (ref in unlist(strsplit(
    gsub("^list\\((.*)\\)$", "\\1", refs),
    ",\\s*"
  ))) {
    parts <- strsplit(trimws(ref), "$", fixed = TRUE)[[1]]
    if (length(parts) != 2 || identical(parts[1], GLOBAL_NODESET)) {
      next
    }
    nodeset <- parts[1]
    attribute <- parts[2]
    if (!attribute %in% names(src$nodes)) {
      next
    }
    value <- ds_attribute(src, nodeset, attribute)
    if (length(value) == 0) {
      next
    }
    # Definedness is per mode category, not per view: imputation pools within a
    # category, so a view spanning two modes with the attribute observed for one
    # and wholly missing for the other still yields nothing to impute the second
    # from. Checking the whole slice would pass that case and fail later.
    modes <- ds_side_modes(src, nodeset)
    categories <- if (is.null(modes)) {
      list(seq_along(value))
    } else {
      split(seq_along(value), modes)
    }
    for (idx in categories) {
      if (!all(is.na(value[idx]))) {
        next
      }
      mode <- if (is.null(modes)) {
        sub("^nodal:", "", ds_nodal_view(src, nodeset))
      } else {
        as.character(modes[idx][1])
      }
      cli::cli_abort(
        c(
          "{.fn {effect}} reads {.val {attribute}} on mode {.val {mode}}, where
           it is undefined.",
          "x" = "Every node of mode {.val {mode}} has {.val NA} for
                 {.val {attribute}}.",
          "i" = "Use an attribute defined on that mode, or restrict the effect
                 to the side where {.val {attribute}} is measured."
        ),
        call = call
      )
    }
  }
  invisible(NULL)
}

check_effect_sides <- function(
  effect,
  arg_name,
  type,
  src,
  model,
  sub_model,
  call = NULL
) {
  if (!inherits(src, "data_source_stocnet") || identical(model, "DyNAMi")) {
    return(invisible(NULL))
  }
  requirements <- effect_side_requirements(effect, type, sub_model)
  if (is.null(requirements)) {
    return(invisible(NULL))
  }
  focal_map <- ds_layer_map(src, src$focal)
  layers <- term_layer_names(arg_name)
  layer_maps <- lapply(layers, function(name) ds_layer_map(src, name))
  # An argument that is not a layer of this object (a legacy object, an
  # attribute where a network was expected) is left to the effect's own init.
  if (is.null(focal_map) || !length(layer_maps)) {
    return(invisible(NULL))
  }
  if (any(vapply(layer_maps, is.null, logical(1)))) {
    return(invisible(NULL))
  }
  sides <- list(S1 = focal_map$side1, S2 = focal_map$side2)
  if (length(layer_maps) == 1L) {
    sides$R1 <- layer_maps[[1]]$side1
    sides$R2 <- layer_maps[[1]]$side2
  } else {
    sides$A1 <- layer_maps[[1]]$side1
    sides$A2 <- layer_maps[[1]]$side2
    sides$B1 <- layer_maps[[2]]$side1
    sides$B2 <- layer_maps[[2]]$side2
  }
  for (requirement in requirements) {
    tokens <- strsplit(requirement, "=", fixed = TRUE)[[1]]
    # A requirement over an arity the term does not supply (a mixed effect
    # given one layer) is a signature error the effect's own init reports.
    if (!all(tokens %in% names(sides))) {
      next
    }
    if (!setequal(sides[[tokens[1]]], sides[[tokens[2]]])) {
      abort_effect_sides(
        effect = effect,
        type = type,
        tokens = tokens,
        layers = layers,
        src = src,
        focal_is_two_mode = isTRUE(focal_map$is_two_mode),
        same_pair = is_same_pair(layer_maps[[1]], focal_map),
        call = call
      )
    }
  }
  invisible(NULL)
}

# Name a side in the user's terms: which layer it belongs to, which end of it,
# and -- where the object carries modes -- which mode(s) it spans.
describe_side <- function(token, layers, src) {
  layer <- switch(
    substr(token, 1, 1),
    S = src$focal,
    R = layers[1],
    A = layers[1],
    B = layers[2]
  )
  end <- if (endsWith(token, "1")) "sender" else "receiver"
  modes <- ds_layer_mode_pair(src, layer)[[end]]
  if (is.null(modes)) {
    return(cli::format_inline("the {end} side of layer {.val {layer}}"))
  }
  cli::format_inline(
    "the {end} side of layer {.val {layer}} (mode{?s} {.val {modes}})"
  )
}

# The mixed-family chain i -> k -> j: the first network's receivers must be the
# second's senders, and the outer ends must be the dependent network's sides.
# The parser compares the node sets themselves where the mode map is available;
# this is the backstop for a direct call, so it reports dimensions.
abort_mixed_chain <- function(effect, network1, network2, n1, n2) {
  cli::cli_abort(c(
    "{.fn {effect}} received networks that do not chain.",
    "x" = "network 1 is {nrow(network1)} x {ncol(network1)}, network 2 is
           {nrow(network2)} x {ncol(network2)}, and the dependent network is
           {n1} x {n2}.",
    "i" = "The columns of network 1 must match the rows of network 2 (the
           node set the path passes through).",
    "i" = "The rows of network 1 and the columns of network 2 must match the
           dependent network's sender and receiver sides."
  ))
}

abort_effect_sides <- function(
  effect,
  type,
  tokens,
  layers,
  src,
  focal_is_two_mode,
  same_pair,
  call
) {
  # Only name the type where it selects the side read: `node_trans` carries a
  # `type` formal too, but it is rejected whatever the variant.
  headline <- if (is.null(type) || !is_degenerate_read(effect)) {
    "{.fn {effect}} cannot be computed on {.val {layers}}."
  } else {
    "{.fn {effect}} with {.arg type} = {.val {type}} cannot be computed on \\
     {.val {layers}}."
  }
  mismatch <- cli::format_inline(
    "{describe_side(tokens[1], layers, src)} and \\
     {describe_side(tokens[2], layers, src)} must be the same node set."
  )
  degenerate <- if (is_degenerate_read(effect) && same_pair) {
    c(
      "x" = "Every entry of the statistic would be {.val {0}}, so the effect
             is not identified."
    )
  } else {
    NULL
  }
  alternatives <- if (focal_is_two_mode) {
    c(
      "i" = "On a two-mode focal layer use one of: \\
             {.code {two_mode_alternatives()}}."
    )
  } else {
    c("i" = "Check the mode sets declared for these layers.")
  }
  cli::cli_abort(
    c(headline, "x" = mismatch, degenerate, alternatives),
    call = call
  )
}
