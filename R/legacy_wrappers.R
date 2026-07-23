# =========================================================================== #
# Legacy-constructor assembly into a single stocnet object.
#
# The deprecated constructors (make_nodes/make_network/link_events/
# make_dependent_events/make_global_attributes) keep their signatures and build
# the same tagged fragments as before; make_data() gathers those fragments and
# hands them here to assemble ONE stocnet -- the representation the direct path
# consumes -- so a fresh session never mints a legacy environment. Assembly is
# delegated to manynet::make_stocnet(); goldfish only shapes the components.
#
# Both one-mode event networks (the shipped datasets and every frozen baseline)
# and two-mode (nodes2) ones. Two-mode assembly fuses the legacy node sets into
# one nodes tibble whose `mode` column names the source set, and declares the
# per-layer sender/receiver mode sets that recover each layer's side pair -- the
# translation that makes the mode map the single downstream representation.
#
# A bundle with one node set keeps the pre-two-mode output byte for byte (no
# `mode` column, no mode sets), so the frozen baselines see an unchanged object.
# =========================================================================== #

# A plain numeric matrix from a network.goldfish (strips class + attrs).
legacy_matrix <- function(net) {
  matrix(as.numeric(net), nrow = nrow(net), ncol = ncol(net))
}

# The node set's rows as a plain data frame, with `present` -> `active` (the
# stocnet composition column). Static attribute columns pass through as the
# nodes' initial values.
legacy_nodes_table <- function(node_obj) {
  # Drop the nodes.goldfish class BEFORE coercing: as.data.frame() on a
  # nodes.goldfish dispatches to the state materializer, not a plain coercion.
  tbl <- node_obj
  attr(tbl, "events") <- NULL
  attr(tbl, "dynamic_attributes") <- NULL
  class(tbl) <- "data.frame"
  rownames(tbl) <- NULL
  if ("present" %in% names(tbl) && !"active" %in% names(tbl)) {
    names(tbl)[names(tbl) == "present"] <- "active"
  }
  tbl
}

# A layer's (sender set, receiver set) node-set names. The legacy constructors
# record one name for a one-mode layer and two when built with `nodes2`; naming
# both sides even when they coincide lets one code path serve either.
legacy_layer_sides <- function(obj) {
  sets <- attr(obj, "nodes")
  sets <- sets[nzchar(sets)]
  if (length(sets) < 2) c(sets[1], sets[1]) else sets[seq_len(2)]
}

# The distinct node sets the layers reference, in first-seen order -- which
# fixes the fused id space, so it must be derived once and reused everywhere.
legacy_node_set_names <- function(
  objs,
  layer_names,
  call = rlang::caller_env()
) {
  sets <- unique(unlist(lapply(objs[layer_names], legacy_layer_sides)))
  sets <- sets[!is.na(sets) & nzchar(sets)]
  missing_sets <- sets[
    !vapply(sets, function(nm) !is.null(objs[[nm]]), logical(1))
  ]
  if (length(missing_sets) > 0) {
    cli::cli_abort(
      c(
        "Node set{?s} {.val {missing_sets}} {?is/are} referenced by a layer \\
         but {?was/were} not passed to {.fn make_data}.",
        "i" = "Pass every node set the layers name."
      ),
      call = call
    )
  }
  sets
}

# Where each node set's rows land in the fused id space, with its labels.
# Every from/to/node reference resolves through its own set's frame: a legacy
# label is unique only *within* its node set, so matching against a fused label
# vector would silently cross the sets on a shared label.
legacy_node_frames <- function(objs, node_set_names) {
  sizes <- vapply(node_set_names, function(nm) nrow(objs[[nm]]), integer(1))
  offsets <- cumsum(c(0L, sizes))[seq_along(sizes)]
  frames <- Map(
    function(nm, offset, size) {
      list(labels = objs[[nm]]$label, ids = offset + seq_len(size))
    },
    node_set_names,
    offsets,
    sizes
  )
  names(frames) <- node_set_names
  frames
}

# The fused nodes tibble: each source set contributes its rows and becomes a
# distinct `mode` value, which is what the per-layer mode sets then name. A
# single node set keeps the pre-two-mode shape (no `mode` column) so existing
# one-mode objects are unchanged.
#
# Attribute columns are unioned across sets; a column a set does not carry is NA
# on that mode's rows. That is the honest reading -- the attribute is undefined
# for that mode, not missing at random -- and the reason per-mode attribute
# handling matters downstream.
legacy_fuse_nodes <- function(objs, node_set_names) {
  tables <- lapply(node_set_names, function(nm) legacy_nodes_table(objs[[nm]]))
  if (length(tables) == 1) {
    return(tables[[1]])
  }
  all_cols <- unique(unlist(lapply(tables, names)))
  tables <- Map(
    function(tbl, nm) {
      for (col in setdiff(all_cols, names(tbl))) {
        tbl[[col]] <- NA
      }
      tbl <- tbl[all_cols]
      tbl$mode <- nm
      tbl
    },
    tables,
    node_set_names
  )
  out <- do.call(rbind, tables)
  rownames(out) <- NULL
  out
}

# One layer's ties: history from the initial matrix (`time = NA`) plus the timed
# linked events. Undirected layers take the upper triangle only so mirroring in
# the engine does not double the history. `weight` carries the tie value (matrix
# entry for history, increment/replace value for events).
# The dataset's common time class as a single NA value: the first timed network
# event's time, or numeric NA if every layer is static. Used to type NA history
# rows so rbind across layers keeps one coherent time axis (POSIXct vs numeric).
legacy_time_proto <- function(objs, net_names) {
  for (ln in net_names) {
    ev_name <- attr(objs[[ln]], "events")
    if (length(ev_name) == 0 || !nzchar(ev_name[1])) {
      next
    }
    ev <- objs[[ev_name[1]]]
    if (!is.null(ev) && "time" %in% names(ev) && nrow(ev) > 0) {
      return(ev$time[NA_integer_])
    }
  }
  NA_real_
}

legacy_layer_ties <- function(
  net,
  layer,
  from_frame,
  to_frame,
  events,
  update,
  directed,
  time_proto = NA_real_
) {
  timed <- NULL
  if (!is.null(events) && nrow(events) > 0) {
    value <- if (update == "increment") events$increment else events$replace
    timed <- data.frame(
      from = from_frame$ids[match(events$sender, from_frame$labels)],
      to = to_frame$ids[match(events$receiver, to_frame$labels)],
      time = events$time,
      layer = layer,
      weight = as.numeric(value),
      stringsAsFactors = FALSE
    )
  }

  history <- NULL
  mat <- legacy_matrix(net)
  if (any(mat != 0)) {
    # upper.tri() only means "one copy of each dyad" on a square matrix; on a
    # two-mode layer it would drop history for no reason, so undirected folding
    # applies to the one-mode case alone (`directed` is vacuous on two-mode).
    fold_undirected <- !directed && nrow(mat) == ncol(mat)
    idx <- if (fold_undirected) {
      which(mat != 0 & upper.tri(mat), arr.ind = TRUE)
    } else {
      which(mat != 0, arr.ind = TRUE)
    }
    # NA time of the dataset's common class (a static layer has no timed events
    # to borrow the class from, so a shared prototype keeps the rbind coherent).
    na_time <- time_proto[rep(NA_integer_, nrow(idx))]
    history <- data.frame(
      from = from_frame$ids[idx[, 1]],
      to = to_frame$ids[idx[, 2]],
      time = na_time,
      layer = layer,
      weight = mat[idx],
      stringsAsFactors = FALSE
    )
  }

  rbind(history, timed)
}

# Match a dependent object's events to the focal layer's timed ties and stamp
# the flavor on the matches. Full coverage (every focal event is a dependent)
# leaves no flavor -- the layer models all its rows. A subset stamps `flavor` on
# the matched rows only. Dependent rows matching no layer tie abort (the legacy
# replace-safe encoding has no equivalent); increment layers could carry them as
# flavored increment-0 rows, but the shipped subset patterns never hit this.
legacy_stamp_flavor <- function(
  ties,
  dep,
  dep_name,
  from_frame,
  to_frame,
  update
) {
  focal_rows <- which(
    ties$layer == attr(dep, "default_network") &
      !is.na(ties$time)
  )
  value <- if (update == "increment") dep$increment else dep$replace
  dep_key <- paste(
    from_frame$ids[match(dep$sender, from_frame$labels)],
    to_frame$ids[match(dep$receiver, to_frame$labels)],
    as.numeric(dep$time),
    as.numeric(value),
    sep = "\r"
  )
  focal_key <- paste(
    ties$from[focal_rows],
    ties$to[focal_rows],
    as.numeric(ties$time[focal_rows]),
    as.numeric(ties$weight[focal_rows]),
    sep = "\r"
  )
  matched <- focal_key %in% dep_key
  unmatched <- !dep_key %in% focal_key
  if (any(unmatched)) {
    cli::cli_abort(c(
      "Dependent events in {.val {dep_name}} have no matching tie in layer \\
       {.val {attr(dep, 'default_network')}}.",
      "i" = "Every dependent event must update the default network; add the \\
             missing ties with {.fn link_events} first."
    ))
  }
  if (all(matched)) {
    # Full coverage: the layer models all its rows, no flavor needed.
    return(list(ties = ties, flavor = NA_character_))
  }
  if (!"flavor" %in% names(ties)) {
    ties$flavor <- NA_character_
  }
  ties$flavor[focal_rows[matched]] <- dep_name
  list(ties = ties, flavor = dep_name)
}

# Nodal dynamic-attribute events -> the `changes` component. `present` events
# route to `var = "active"` (composition); every other linked attribute keeps
# its name. `value` is a list-column, matching manynet's changes contract.
legacy_changes_table <- function(node_obj, objs, frame) {
  event_names <- attr(node_obj, "events")
  attributes_v <- attr(node_obj, "dynamic_attributes")
  if (length(event_names) == 0) {
    return(NULL)
  }
  parts <- vector("list", length(event_names))
  for (i in seq_along(event_names)) {
    ev <- objs[[event_names[i]]]
    if (is.null(ev)) {
      next
    }
    var <- attributes_v[i]
    if (identical(var, "present")) {
      var <- "active"
    }
    node_col <- if ("node" %in% names(ev)) ev$node else ev$label
    parts[[i]] <- data.frame(
      time = ev$time,
      node = frame$ids[match(node_col, frame$labels)],
      var = var,
      stringsAsFactors = FALSE
    )
    parts[[i]]$value <- as.list(ev$replace)
  }
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) {
    return(NULL)
  }
  do.call(rbind, parts)
}

# Global attribute object -> the `global` component: the initial one-row values
# as `time = NA` rows, plus each linked replace event. Single global variable
# (the legacy change frames carry no `var` column to disambiguate several).
legacy_global_table <- function(glob_obj, objs) {
  vars <- setdiff(names(glob_obj), character(0))
  init <- lapply(vars, function(v) {
    data.frame(time = NA_real_, var = v, stringsAsFactors = FALSE)
  })
  init <- do.call(rbind, init)
  init$value <- as.list(unlist(glob_obj[vars], use.names = FALSE))

  event_names <- attr(glob_obj, "events")
  events <- NULL
  if (length(event_names) > 0 && length(vars) == 1) {
    ev_parts <- lapply(event_names, function(nm) {
      ev <- objs[[nm]]
      if (is.null(ev)) {
        return(NULL)
      }
      col <- attr(ev, "replace") %||% "replace"
      part <- data.frame(time = ev$time, var = vars, stringsAsFactors = FALSE)
      part$value <- as.list(ev[[col]])
      part
    })
    events <- do.call(rbind, ev_parts)
  }
  # The initial value's NA time must share the events' time class: rbind of a
  # numeric NA_real_ init with a POSIXct/Date event column collapses the axis to
  # numeric, which then reads as a mixed time axis against the (temporal) ties.
  if (!is.null(events)) {
    init$time <- events$time[NA_integer_]
  }
  rbind(init, events)
}

# Whether the gathered fragments form a DyNAM/REM structure the assembler
# handles -- one-mode or two-mode alike, since a two-mode bundle now fuses onto
# the mode map. A bundle with no node set or no layer (raw interaction records)
# still falls back to the legacy environment path.
is_stocnet_assemblable <- function(objs) {
  is_layer <- function(o) {
    inherits(o, "network.goldfish") || inherits(o, "dependent.goldfish")
  }
  has_nodes <- any(vapply(objs, inherits, logical(1), "nodes.goldfish")) ||
    any(vapply(objs, inherits, logical(1), "network.goldfish"))
  has_layer <- any(vapply(objs, is_layer, logical(1)))
  if (!has_nodes || !has_layer) {
    return(FALSE)
  }
  layer_objs <- objs[vapply(objs, is_layer, logical(1))]
  node_set_names <- unique(unlist(lapply(layer_objs, legacy_layer_sides)))
  node_set_names <- node_set_names[
    !is.na(node_set_names) &
      nzchar(node_set_names)
  ]
  if (length(node_set_names) == 0) {
    return(FALSE)
  }
  # Every named node set must resolve to a node table in the bundle. The
  # constructors record the name by deparsing their argument, so a layer built
  # from `nodes = fx$actors` records `"fx$actors"`, which names nothing here --
  # such a bundle cannot be assembled and keeps the legacy environment path.
  # Tested by shape, not class: `make_network()` accepts a bare data frame, and
  # the shipped node sets are plain data frames.
  all(vapply(
    node_set_names,
    function(nm) is_node_table(objs[[nm]]),
    logical(1)
  ))
}

is_node_table <- function(o) {
  is.data.frame(o) && "label" %in% names(o)
}

# Assemble the gathered legacy fragments into one stocnet. `objs` is a named
# list (object name -> fragment); the names become the stocnet's layer names and
# the dependent aliases, so effect formulas keep referencing the caller's names.
assemble_stocnet_from_legacy <- function(objs, call = rlang::caller_env()) {
  by_class <- function(cls) {
    names(objs)[vapply(objs, inherits, logical(1), cls)]
  }
  net_names <- by_class("network.goldfish")
  dep_names <- by_class("dependent.goldfish")
  glob_names <- by_class("global.goldfish")

  node_set_names <- legacy_node_set_names(
    objs,
    c(net_names, dep_names),
    call = call
  )
  node_frames <- legacy_node_frames(objs, node_set_names)
  nodes_tbl <- legacy_fuse_nodes(objs, node_set_names)
  # Each layer's sides as node-set names, resolved once: they index node_frames
  # for the id remapping below and become the mode sets in `info`.
  layer_sides <- lapply(objs[net_names], legacy_layer_sides)

  update <- stats::setNames(character(length(net_names)), net_names)
  directed <- stats::setNames(logical(length(net_names)), net_names)
  observation <- stats::setNames(
    rep("event", length(net_names)),
    net_names
  )
  ties_list <- vector("list", length(net_names))
  names(ties_list) <- net_names
  # One time prototype for every layer's NA history so a static layer (no timed
  # events) does not inject a numeric NA that rbind would coerce a POSIXct axis
  # down to. Borrow the class from the first layer that carries timed events.
  time_proto <- legacy_time_proto(objs, net_names)
  for (ln in net_names) {
    net <- objs[[ln]]
    ev_name <- attr(net, "events")
    ev <- if (length(ev_name) > 0 && nzchar(ev_name[1])) {
      objs[[ev_name[1]]]
    } else {
      NULL
    }
    upd <- if (!is.null(ev) && "increment" %in% names(ev)) {
      "increment"
    } else {
      "replace"
    }
    dir <- isTRUE(attr(net, "directed"))
    update[ln] <- upd
    directed[ln] <- dir
    sides <- layer_sides[[ln]]
    ties_list[[ln]] <- legacy_layer_ties(
      net,
      ln,
      node_frames[[sides[1]]],
      node_frames[[sides[2]]],
      ev,
      upd,
      dir,
      time_proto
    )
  }
  ties <- do.call(rbind, unname(ties_list))
  # Preserve the legacy row order as the deterministic tie-break: same-time
  # same-target replaces (contiguity, attribute waves) fold last-in-order like
  # the per-object event loop did, instead of aborting as ambiguous.
  ties$order <- seq_len(nrow(ties))

  focal <- NULL
  dependents <- list()
  for (dn in dep_names) {
    dep <- objs[[dn]]
    layer <- attr(dep, "default_network")
    if (is.null(layer) || !nzchar(layer)) {
      cli::cli_abort(
        c(
          "The dependent events {.val {dn}} have no {.arg default_network}.",
          "i" = "Monadic dependent events are not supported on the stocnet \\
                 path yet."
        ),
        call = call
      )
    }
    focal <- focal %||% layer
    # The dependent's own sides, not the layer's: a dependent object names its
    # node sets independently, and its events are keyed against the layer ties.
    dep_sides <- legacy_layer_sides(dep)
    stamped <- legacy_stamp_flavor(
      ties,
      dep,
      dn,
      node_frames[[dep_sides[1]]],
      node_frames[[dep_sides[2]]],
      update[layer]
    )
    ties <- stamped$ties
    dependents[[dn]] <- list(layer = layer, flavor = stamped$flavor)
  }

  # Attribute/composition events are declared per node set, so each set's
  # stream resolves through its own frame before the streams are pooled.
  changes <- do.call(
    rbind,
    lapply(node_set_names, function(nm) {
      legacy_changes_table(objs[[nm]], objs, node_frames[[nm]])
    })
  )
  if (!is.null(changes)) {
    changes$order <- seq_len(nrow(changes))
  }
  global <- if (length(glob_names) > 0) {
    legacy_global_table(objs[[glob_names[1]]], objs)
  } else {
    NULL
  }
  if (!is.null(global)) {
    global$order <- seq_len(nrow(global))
  }

  info <- list(
    update = update,
    directed = directed,
    observation = observation
  )
  if (!is.null(focal)) {
    info$focal <- focal
  }
  if (length(dependents) > 0) {
    info$dependents <- dependents
  }
  # Mode sets only once the fused tibble actually carries several modes: a
  # single-node-set bundle has no `mode` column, and declaring sets against a
  # missing column is what the validator rejects. Each legacy node set is
  # exactly one mode, so one entry per layer per side suffices -- the repeated
  # names the mode map decodes appear only when a side spans several modes.
  if (length(node_set_names) > 1) {
    info$sender <- vapply(layer_sides, `[[`, character(1), 1)
    info$receiver <- vapply(layer_sides, `[[`, character(1), 2)
  }

  manynet::make_stocnet(
    info = info,
    nodes = nodes_tbl,
    ties = ties,
    changes = changes,
    global = global
  )
}

# The modeled dependent events as time/sender/receiver/increment, in the event
# schedule order estimation processed them (time, then the `order` tie-break),
# so the postestimation broom/diagnostic surface can align them with the
# per-event scores. On the stocnet path they are the focal layer's timed ties,
# filtered to the modeled flavor; `from`/`to` indices resolve to node labels.
stocnet_dependent_events <- function(data, layer, modeled_flavor = NULL) {
  ties <- as.data.frame(data$ties)
  sel <- ties$layer == layer & !is.na(ties$time)
  if (!is.null(modeled_flavor) && "flavor" %in% names(ties)) {
    sel <- sel & !is.na(ties$flavor) & ties$flavor == modeled_flavor
  }
  ties <- ties[sel, , drop = FALSE]
  order_key <- if (!is.null(ties$order)) ties$order else seq_len(nrow(ties))
  ties <- ties[order(ties$time, order_key), , drop = FALSE]
  labels <- data$nodes$label
  data.frame(
    time = ties$time,
    sender = labels[ties$from],
    receiver = labels[ties$to],
    increment = if (!is.null(ties$weight)) ties$weight else rep(1, nrow(ties)),
    stringsAsFactors = FALSE
  )
}

# Translate a dependent-events object name on a formula's left-hand side to its
# focal layer and modeled flavor, using the info$dependents association
# make_data() records. The direct estimate path carries the dependent name on
# the LHS (`create_bilat ~ ...`); rewriting it to the layer lets the rest of the
# pipeline treat it as any other focal layer, while the recorded flavor selects
# the modeled rows without the plain-formula "all rows modeled" inform. A
# layer-named LHS (the specification path) is not in the map and returns
# unchanged. An already-set `modeled_flavor` (from a flavor-keyed list) wins.
resolve_dependent_alias <- function(formula, data, modeled_flavor = NULL) {
  unchanged <- list(formula = formula, modeled_flavor = modeled_flavor)
  if (is.null(data) || is.environment(data) || length(formula) != 3L) {
    return(unchanged)
  }
  dependents <- data$info$dependents
  entry <- dependents[[deparse(formula[[2]])]]
  if (is.null(entry)) {
    return(unchanged)
  }
  formula[[2]] <- as.name(entry$layer)
  flavor <- modeled_flavor
  if (is.null(flavor) && !is.na(entry$flavor)) {
    flavor <- entry$flavor
  }
  list(formula = formula, modeled_flavor = flavor)
}
