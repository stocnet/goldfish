# =========================================================================== #
# stocnet input validator (goldfish boundary)
#
# manynet's validate_stocnet() follows a reserved-not-required contract: beyond
# ties$from/to almost nothing goldfish consumes is guaranteed (info$update /
# observation carry no per-layer coverage check, info$directed is unvalidated,
# nodes$label uniqueness is unchecked, time may be character/mdate, and mode
# purity of ties is never tested). goldfish therefore re-checks everything it
# consumes on every entry path, assuming nothing from upstream. The checks reuse
# the class_checks.R machinery (check_classes(), check_columns()) generalized
# over stocnet components, which are accepted as plain data.frames (tibbles
# pass, being data.frames) so hand-built fixtures stay dependency-free.
# =========================================================================== #

# Node columns that are structural, not user attributes; every other nodes
# column is an attribute referenced by bare name in formulas and so must be a
# syntactic R name.
reserved_node_cols <- c("label", "mode", "active", "present")

# Time classes goldfish converts to a numeric axis internally. character/mdate
# are rejected with conversion guidance.
allowed_time_classes <- c(
  "numeric",
  "integer",
  "POSIXct",
  "POSIXlt",
  "POSIXt",
  "Date"
)

#' Validate a stocnet object against the goldfish input contract
#'
#' Narrows manynet's reserved-but-optional stocnet contract into the required,
#' class-checked, name-resolvable shape goldfish preprocessing consumes. Reads
#' components structurally (plain data.frames accepted) and aborts with a cli
#' error naming the offending layer/entry on the first violation. Returns the
#' object invisibly and unchanged: validating never restructures, so a stamped
#' object is the caller's object; conversion happens later, at the builder
#' layer.
#'
#' @param x a stocnet-shaped list (`info`, `nodes`, `ties`, optional `changes`,
#'   `global`), or an object carrying those components.
#' @param focal optional character(1) overriding `info$focal` as the dependent
#'   layer.
#' @param call the calling environment for error reporting.
#'
#' @return `x`, invisibly.
#' @noRd
validate_goldfish_data <- function(
  x,
  focal = NULL,
  call = rlang::caller_env()
) {
  check_stocnet_shape(x, call = call)

  nodes <- as.data.frame(x$nodes)
  ties <- as.data.frame(x$ties)
  info <- x$info %||% list()

  layers <- check_layers_present(ties, call = call)

  check_nodes_labels(nodes, call = call)
  check_syntactic_names(nodes, ties, x$changes, x$global, layers, call = call)
  check_layer_coverage(info, layers, call = call)
  focal_layer <- check_focal(info, focal, layers, call = call)
  check_focal_not_panel(info, focal_layer, call = call)
  check_time_contract(ties, x$changes, x$global, call = call)
  check_pool_membership(nodes, ties, x$changes, call = call)
  check_flavor(ties, call = call)
  check_mode_sets(info, nodes, ties, layers, call = call)
  check_list_column_values(x$changes, "changes", call = call)
  check_list_column_values(x$global, "global", call = call)

  invisible(x)
}

check_stocnet_shape <- function(x, call) {
  if (!is.list(x) || is.data.frame(x)) {
    cli::cli_abort(
      c(
        "{.arg data} must be a stocnet object (a list of components).",
        "x" = "Got {.obj_type_friendly {x}}.",
        "i" = "Build one with {.fn manynet::make_stocnet} or the goldfish \\
               constructors."
      ),
      call = call
    )
  }
  missing <- setdiff(c("nodes", "ties"), names(x))
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "A stocnet object must contain {.field nodes} and {.field ties}.",
        "x" = "Missing component{?s}: {.field {missing}}."
      ),
      call = call
    )
  }
  if (!is.data.frame(x$nodes) || !is.data.frame(x$ties)) {
    cli::cli_abort(
      "{.field nodes} and {.field ties} must be data frames.",
      call = call
    )
  }
  invisible(TRUE)
}

check_layers_present <- function(ties, call) {
  if (!"layer" %in% names(ties)) {
    cli::cli_abort(
      c(
        "{.field ties} must carry a {.field layer} column.",
        "i" = "Each tie row names the network layer it belongs to."
      ),
      call = call
    )
  }
  if (!is.character(ties$layer)) {
    cli::cli_abort(
      "{.field ties$layer} must be a character vector, not \\
       {.cls {class(ties$layer)}}.",
      call = call
    )
  }
  if (anyNA(ties$layer)) {
    cli::cli_abort(
      "{.field ties$layer} must not contain missing values.",
      call = call
    )
  }
  unique(ties$layer)
}

check_nodes_labels <- function(nodes, call) {
  if (!"label" %in% names(nodes)) {
    cli::cli_abort(
      "{.field nodes} must carry a {.field label} column.",
      call = call
    )
  }
  if (!is.character(nodes$label)) {
    cli::cli_abort(
      "{.field nodes$label} must be a character vector, not \\
       {.cls {class(nodes$label)}}.",
      call = call
    )
  }
  if (anyNA(nodes$label)) {
    cli::cli_abort(
      "{.field nodes$label} must not contain missing values.",
      call = call
    )
  }
  dup <- unique(nodes$label[duplicated(nodes$label)])
  if (length(dup) > 0) {
    cli::cli_abort(
      c(
        "{.field nodes$label} must be unique.",
        "x" = "Duplicated label{?s}: {.val {dup}}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

# Layer and attribute-variable names appear in effect/support_constraint
# formulas without backticks, so a validated object must use only syntactic R
# names.
check_syntactic_names <- function(nodes, ties, changes, global, layers, call) {
  attr_cols <- setdiff(names(nodes), reserved_node_cols)
  change_vars <- if (!is.null(changes)) unique(changes$var) else character(0)
  global_vars <- if (!is.null(global)) unique(global$var) else character(0)

  named <- list(
    layer = layers,
    "node attribute" = attr_cols,
    "changes variable" = change_vars,
    "global variable" = global_vars
  )
  for (kind in names(named)) {
    values <- named[[kind]]
    bad <- values[make.names(values) != values]
    if (length(bad) > 0) {
      cli::cli_abort(
        c(
          "Every {kind} name must be a syntactic R name.",
          "x" = "Non-syntactic name{?s}: {.val {bad}}.",
          "i" = "Rename, e.g. {.val {bad[1]}} to {.val {make.names(bad[1])}}, \\
                 so it can be used in formulas without backticks."
        ),
        call = call
      )
    }
  }
  invisible(TRUE)
}

# info$update / directed / observation must each be named vectors covering
# exactly the distinct layers present in ties$layer (manynet enforces none of
# this). observation is limited to the layer kinds goldfish models.
check_layer_coverage <- function(info, layers, call) {
  check_named_coverage(info$update, "update", layers, call = call)
  check_named_coverage(info$directed, "directed", layers, call = call)
  check_named_coverage(info$observation, "observation", layers, call = call)

  bad_update <- info$update[!info$update %in% c("increment", "replace")]
  if (length(bad_update) > 0) {
    cli::cli_abort(
      c(
        "{.field info$update} must be {.val increment} or {.val replace}.",
        "x" = "Layer {.val {names(bad_update)[1]}} has \\
               {.val {unname(bad_update)[1]}}."
      ),
      call = call
    )
  }
  if (!is.logical(info$directed)) {
    cli::cli_abort(
      "{.field info$directed} must be a logical vector.",
      call = call
    )
  }
  bad_obs <- info$observation[!info$observation %in% c("event", "panel")]
  if (length(bad_obs) > 0) {
    cli::cli_abort(
      c(
        "goldfish models {.val event} and {.val panel} layers only.",
        "x" = "Layer {.val {names(bad_obs)[1]}} declares \\
               {.field observation} = {.val {unname(bad_obs)[1]}}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

check_named_coverage <- function(value, field, layers, call) {
  if (is.null(value)) {
    cli::cli_abort(
      c(
        "{.field info${field}} is required for every layer.",
        "x" = "It is missing.",
        "i" = "Provide a named vector covering layer{?s} {.val {layers}}."
      ),
      call = call
    )
  }
  nms <- names(value)
  if (is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort(
      c(
        "{.field info${field}} must be named by layer.",
        "i" = "manynet accepts an unnamed vector; goldfish requires one entry \\
               per layer, named."
      ),
      call = call
    )
  }
  missing <- setdiff(layers, nms)
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "{.field info${field}} must cover every layer in {.field ties}.",
        "x" = "No {field} entry for layer{?s} {.val {missing}}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

check_focal <- function(info, focal, layers, call) {
  focal_layer <- focal %||% info$focal
  if (is.null(focal_layer)) {
    return(NULL)
  }
  if (!is.character(focal_layer) || length(focal_layer) != 1) {
    cli::cli_abort(
      "The focal layer must be a single layer name.",
      call = call
    )
  }
  if (!focal_layer %in% layers) {
    cli::cli_abort(
      c(
        "The focal layer must name a layer present in {.field ties}.",
        "x" = "{.val {focal_layer}} is not among {.val {layers}}."
      ),
      call = call
    )
  }
  focal_layer
}

# Panel layers enter DyNAM/REM as change-list exogenous covariates: their tie
# rows are updates applied at their wave times (per info$update), dissolutions
# must be explicit value-0 rows, and windowing such an effect aborts (see
# ds_realize_derivations()). A richer snapshot interpretation -- diffing waves
# and augmenting with latent formation/dissolution events between them -- is the
# future DyNES model's, and it is keyed off a RESERVED per-layer info flag
# (`info$panel_semantics`, named per layer) that this validator neither requires
# nor consults: the slot is defined for that change to own, with no machinery
# built here.
check_focal_not_panel <- function(info, focal_layer, call) {
  if (is.null(focal_layer)) {
    return(invisible(TRUE))
  }
  if (identical(unname(info$observation[focal_layer]), "panel")) {
    cli::cli_abort(
      c(
        "A {.val panel} layer cannot be the focal (dependent) process.",
        "x" = "Layer {.val {focal_layer}} is declared {.field observation} = \\
               {.val panel}.",
        "i" = "goldfish models event-stream dependents; panel-dependent \\
               processes are SAOM/RSiena territory."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

# Time is required on event streams; classes limited to numeric/POSIXct/Date;
# character/mdate abort; all layers must share a comparable axis (integer waves
# only when every stream is numeric). NA time is allowed on ties and on global
# rows -- the pre-observation initial value, the only place a global variable's
# starting value is carried (nodal attributes carry theirs on the nodes table,
# so changes still forbid NA time).
check_time_contract <- function(ties, changes, global, call) {
  streams <- list(ties = ties$time)
  if (!is.null(changes)) {
    streams$changes <- changes$time
  }
  if (!is.null(global)) {
    streams$global <- global$time
  }

  for (nm in names(streams)) {
    time <- streams[[nm]]
    if (is.null(time)) {
      cli::cli_abort(
        c(
          "{.field {nm}} must carry a {.field time} column.",
          "i" = if (nm == "ties") {
            "Use {.code time = NA} rows for pre-observation history."
          }
        ),
        call = call
      )
    }
    if (!any(check_classes(time, allowed_time_classes))) {
      cli::cli_abort(
        c(
          "{.field {nm}$time} has an unsupported class \\
           {.cls {class(time)}}.",
          "i" = "Convert {.field time} to numeric, POSIXct, or Date."
        ),
        call = call
      )
    }
  }
  if (!is.null(changes) && anyNA(changes$time)) {
    cli::cli_abort(
      "{.field changes$time} must not contain missing values.",
      call = call
    )
  }

  axes <- vapply(streams, time_axis, character(1))
  if (length(unique(axes)) > 1) {
    cli::cli_abort(
      c(
        "All streams must share a comparable time axis.",
        "x" = "Mixed axes: \\
               {.field {names(axes)}} on {.val {unname(axes)}}.",
        "i" = "Integer/numeric wave times are only comparable when every \\
               stream uses them; convert to a common POSIXct/Date axis."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

time_axis <- function(time) {
  if (inherits(time, c("POSIXct", "POSIXlt", "POSIXt", "Date"))) {
    "temporal"
  } else {
    "numeric"
  }
}

# from/to (ties) and node (changes) index nodes either by integer position or by
# character label; either way every reference must resolve to a node.
check_pool_membership <- function(nodes, ties, changes, call) {
  n <- nrow(nodes)
  check_node_refs(ties$from, n, nodes$label, "ties$from", call = call)
  check_node_refs(ties$to, n, nodes$label, "ties$to", call = call)
  if (!is.null(changes) && "node" %in% names(changes)) {
    check_node_refs(changes$node, n, nodes$label, "changes$node", call = call)
  }
  invisible(TRUE)
}

check_node_refs <- function(refs, n, labels, field, call) {
  if (is.null(refs)) {
    cli::cli_abort("{.field {field}} is required.", call = call)
  }
  if (is.numeric(refs)) {
    bad <- refs[!is.na(refs) & (refs < 1 | refs > n | refs != as.integer(refs))]
    if (length(bad) > 0) {
      cli::cli_abort(
        c(
          "{.field {field}} must index rows of {.field nodes} (1..{n}).",
          "x" = "Out-of-range value{?s}: {.val {unique(bad)}}."
        ),
        call = call
      )
    }
  } else if (is.character(refs)) {
    bad <- unique(refs[!is.na(refs) & !refs %in% labels])
    if (length(bad) > 0) {
      cli::cli_abort(
        c(
          "{.field {field}} must reference {.field nodes$label}.",
          "x" = "Unknown label{?s}: {.val {bad}}."
        ),
        call = call
      )
    }
  } else {
    cli::cli_abort(
      "{.field {field}} must be integer indices or character labels.",
      call = call
    )
  }
  invisible(TRUE)
}

# Optional reserved flavor column keying which focal rows a specification
# models. Character, syntactic values (they appear as formula-list keys), NA
# allowed (state-only rows under a keyed specification).
check_flavor <- function(ties, call) {
  if (!"flavor" %in% names(ties)) {
    return(invisible(TRUE))
  }
  if (!is.character(ties$flavor)) {
    cli::cli_abort(
      "{.field ties$flavor} must be a character vector, not \\
       {.cls {class(ties$flavor)}}.",
      call = call
    )
  }
  values <- unique(ties$flavor[!is.na(ties$flavor)])
  bad <- values[make.names(values) != values]
  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "{.field ties$flavor} values must be syntactic R names.",
        "x" = "Non-syntactic value{?s}: {.val {bad}}.",
        "i" = "Flavor values appear as formula-list keys."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

# info$sender/receiver declare per-layer sets of nodes$mode values.
# Identical sets -> one-mode over that subset; disjoint sets -> two-mode;
# partial overlap aborts. For a declared layer, ties must be side-pure
# (from-node mode in that layer's sender set, to-node mode in its receiver set).
# Layers declare independently, so one object may mix one-mode and two-mode
# layers.
check_mode_sets <- function(info, nodes, ties, layers, call) {
  if (is.null(info$sender) && is.null(info$receiver)) {
    return(invisible(TRUE))
  }
  if (is.null(info$sender) || is.null(info$receiver)) {
    cli::cli_abort(
      "Declare both {.field info$sender} and {.field info$receiver}, or \\
       neither.",
      call = call
    )
  }
  if (!"mode" %in% names(nodes)) {
    cli::cli_abort(
      c(
        "Declared {.field sender}/{.field receiver} require a {.field mode} \\
         column in {.field nodes}.",
        "x" = "{.field nodes} has no {.field mode} column."
      ),
      call = call
    )
  }
  check_mode_set_encoding(info, call = call)
  sender_sets <- normalize_mode_sets(info$sender, layers)
  receiver_sets <- normalize_mode_sets(info$receiver, layers)

  modes <- unique(nodes$mode)
  unknown <- setdiff(unlist(c(sender_sets, receiver_sets)), modes)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "{.field sender}/{.field receiver} must name values of \\
         {.field nodes$mode}.",
        "x" = "Unknown mode{?s}: {.val {unknown}}."
      ),
      call = call
    )
  }
  undeclared <- setdiff(names(sender_sets), layers)
  if (length(undeclared) > 0) {
    cli::cli_abort(
      c(
        "{.field sender}/{.field receiver} must name layers present in \\
         {.field ties$layer}.",
        "x" = "Unknown layer{?s}: {.val {undeclared}}."
      ),
      call = call
    )
  }

  for (layer in intersect(layers, names(sender_sets))) {
    check_layer_mode_sets(
      layer,
      sender_sets[[layer]],
      receiver_sets[[layer]],
      nodes,
      ties[ties$layer == layer, , drop = FALSE],
      call = call
    )
  }
  invisible(TRUE)
}

# A list of per-layer sets reads naturally but manynet type-checks these entries
# as character, and add_info() does not validate -- so a list is accepted where
# it is written and only aborts later, inside whichever verb re-validates
# (bind_changes()), blaming manynet internals. Reject it here, where the fix is
# obvious.
check_mode_set_encoding <- function(info, call) {
  listed <- c("info$sender", "info$receiver")[
    c(is.list(info$sender), is.list(info$receiver))
  ]
  if (length(listed) == 0) {
    return(invisible(TRUE))
  }
  example <- paste(
    'c(survey = "employees", survey = "supervisor",',
    'report = "employees")'
  )
  cli::cli_abort(
    c(
      "{.field {listed}} must be a character vector, not a list.",
      "i" = "Name each mode with its layer, repeating the layer name to give \\
             it several modes:",
      " " = "{.code {example}}",
      "i" = "A list is rejected by {.fn manynet::make_stocnet} and \\
             {.fn manynet::bind_changes}, so it would fail later."
    ),
    call = call
  )
}

check_layer_mode_sets <- function(
  layer,
  sender_set,
  receiver_set,
  nodes,
  ties,
  call
) {
  if (is.null(sender_set) || is.null(receiver_set)) {
    cli::cli_abort(
      c(
        "Layer {.val {layer}} must declare both {.field sender} and \\
         {.field receiver}, or neither.",
        "i" = "Declaring one side alone leaves the other undefined."
      ),
      call = call
    )
  }
  identical_sets <- setequal(sender_set, receiver_set)
  disjoint_sets <- length(intersect(sender_set, receiver_set)) == 0
  if (!identical_sets && !disjoint_sets) {
    cli::cli_abort(
      c(
        "{.field sender}/{.field receiver} mode sets must be identical \\
         (one-mode subset) or disjoint (two-mode).",
        "x" = "On layer {.val {layer}} the {.field sender} set \\
               ({.val {sender_set}}) and {.field receiver} set \\
               ({.val {receiver_set}}) partially overlap.",
        "i" = "Express such a design as an identical-set one-mode layer plus \\
               a {.fn support_constraint}."
      ),
      call = call
    )
  }

  from_mode <- ref_to_mode(ties$from, nodes)
  to_mode <- ref_to_mode(ties$to, nodes)
  impure_from <- unique(ties$from[!from_mode %in% sender_set])
  impure_to <- unique(ties$to[!to_mode %in% receiver_set])
  if (length(impure_from) > 0 || length(impure_to) > 0) {
    # Character, not the raw integer ids: cli's pluralization asserts a
    # length-1 quantity when the value it counts is a multi-element integer
    # vector (cli 3.6.6), so integer ids here abort the abort itself.
    offending <- as.character(c(impure_from, impure_to))
    cli::cli_abort(
      c(
        "Ties must be side-pure for a declared two-mode/subset layer.",
        # qty() pins the count to the offending nodes; without it the nearest
        # preceding substitution (the layer, always one) drives the plural.
        "x" = "On layer {.val {layer}}, {cli::qty(offending)}node{?s} \\
               {.val {offending}} fall{?s/} outside the declared \\
               {.field sender}/{.field receiver} mode sets."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

ref_to_mode <- function(refs, nodes) {
  if (is.numeric(refs)) {
    nodes$mode[refs]
  } else {
    nodes$mode[match(refs, nodes$label)]
  }
}

# changes$value / global$value are list-columns (each element wrapped). Unwrap
# and require per-var type consistency; var == "active" values must be logical.
check_list_column_values <- function(component, name, call) {
  if (is.null(component) || !"value" %in% names(component)) {
    return(invisible(TRUE))
  }
  value <- component$value
  var <- component$var
  if (is.null(var)) {
    cli::cli_abort(
      "{.field {name}} must carry a {.field var} column.",
      call = call
    )
  }
  scalars <- unwrap_values(value)
  for (v in unique(var)) {
    idx <- which(var == v)
    types <- vapply(scalars[idx], typeof, character(1))
    types <- types[types != "NULL"]
    if (length(unique(types)) > 1) {
      cli::cli_abort(
        c(
          "{.field {name}} values for {.field {v}} must share one type.",
          "x" = "Found type{?s}: {.val {unique(types)}}."
        ),
        call = call
      )
    }
    if (
      identical(v, "active") && length(types) > 0 && !all(types == "logical")
    ) {
      cli::cli_abort(
        c(
          "{.field {name}} {.field active} values must be logical.",
          "x" = "Found type{?s}: {.val {unique(types)}}."
        ),
        call = call
      )
    }
  }
  invisible(TRUE)
}

unwrap_values <- function(value) {
  if (is.list(value)) {
    lapply(value, function(v) {
      while (is.list(v) && length(v) == 1) {
        v <- v[[1]]
      }
      v
    })
  } else {
    as.list(value)
  }
}
