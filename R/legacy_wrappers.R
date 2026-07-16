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
# One-mode event networks (the shipped datasets and every frozen baseline). A
# two-mode (nodes2) legacy layer aborts pointing at the stocnet workflow, where
# per-layer mode sets express it natively.
# =========================================================================== #

# A plain numeric matrix from a network.goldfish (strips class + attrs).
legacy_matrix <- function(net) {
  matrix(as.numeric(net), nrow = nrow(net), ncol = ncol(net))
}

# The node set's rows as a plain data frame, with `present` -> `active` (the
# stocnet composition column). Static attribute columns pass through as the
# nodes' initial values.
legacy_nodes_table <- function(node_obj) {
  tbl <- as.data.frame(node_obj, stringsAsFactors = FALSE)
  attrs <- setdiff(
    names(attributes(tbl)),
    c("names", "row.names", "class")
  )
  for (a in attrs) {
    attr(tbl, a) <- NULL
  }
  class(tbl) <- "data.frame"
  if ("present" %in% names(tbl) && !"active" %in% names(tbl)) {
    names(tbl)[names(tbl) == "present"] <- "active"
  }
  tbl
}

# One layer's ties: history from the initial matrix (`time = NA`) plus the timed
# linked events. Undirected layers take the upper triangle only so mirroring in
# the engine does not double the history. `weight` carries the tie value (matrix
# entry for history, increment/replace value for events).
legacy_layer_ties <- function(net, layer, labels, events, update, directed) {
  timed <- NULL
  if (!is.null(events) && nrow(events) > 0) {
    value <- if (update == "increment") events$increment else events$replace
    timed <- data.frame(
      from = match(events$sender, labels),
      to = match(events$receiver, labels),
      time = events$time,
      layer = layer,
      weight = as.numeric(value),
      stringsAsFactors = FALSE
    )
  }

  history <- NULL
  mat <- legacy_matrix(net)
  if (any(mat != 0)) {
    idx <- if (directed) {
      which(mat != 0, arr.ind = TRUE)
    } else {
      which(mat != 0 & upper.tri(mat), arr.ind = TRUE)
    }
    # NA time of the same class as the timed events (POSIXct vs numeric).
    na_time <- if (!is.null(timed)) {
      timed$time[rep(NA_integer_, nrow(idx))]
    } else {
      rep(NA_real_, nrow(idx))
    }
    history <- data.frame(
      from = idx[, 1],
      to = idx[, 2],
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
legacy_stamp_flavor <- function(ties, dep, dep_name, labels, update) {
  focal_rows <- which(
    ties$layer == attr(dep, "default_network") &
      !is.na(ties$time)
  )
  value <- if (update == "increment") dep$increment else dep$replace
  dep_key <- paste(
    match(dep$sender, labels),
    match(dep$receiver, labels),
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
legacy_changes_table <- function(node_obj, objs, labels) {
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
      node = match(node_col, labels),
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
  rbind(init, events)
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

  two_mode <- any(vapply(
    objs[net_names],
    function(n) isTRUE(attr(n, "is_two_mode")),
    logical(1)
  ))
  node_set_names <- unique(unlist(lapply(
    objs[c(net_names, dep_names)],
    attr,
    "nodes"
  )))
  node_set_names <- node_set_names[nzchar(node_set_names)]
  if (two_mode || length(node_set_names) > 1) {
    cli::cli_abort(
      c(
        "Two-mode legacy data cannot be auto-assembled into a stocnet.",
        "i" = "Build it with {.fn manynet::make_stocnet} and per-layer \\
               {.field sender}/{.field receiver} mode sets."
      ),
      call = call
    )
  }
  node_obj <- objs[[node_set_names[1]]]
  labels <- node_obj$label
  nodes_tbl <- legacy_nodes_table(node_obj)

  update <- stats::setNames(character(length(net_names)), net_names)
  directed <- stats::setNames(logical(length(net_names)), net_names)
  observation <- stats::setNames(
    rep("event", length(net_names)),
    net_names
  )
  ties_list <- vector("list", length(net_names))
  names(ties_list) <- net_names
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
    ties_list[[ln]] <- legacy_layer_ties(net, ln, labels, ev, upd, dir)
  }
  ties <- do.call(rbind, unname(ties_list))

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
    stamped <- legacy_stamp_flavor(ties, dep, dn, labels, update[layer])
    ties <- stamped$ties
    dependents[[dn]] <- list(layer = layer, flavor = stamped$flavor)
  }

  changes <- legacy_changes_table(node_obj, objs, labels)
  global <- if (length(glob_names) > 0) {
    legacy_global_table(objs[[glob_names[1]]], objs)
  } else {
    NULL
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

  manynet::make_stocnet(
    info = info,
    nodes = nodes_tbl,
    ties = ties,
    changes = changes,
    global = global
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
