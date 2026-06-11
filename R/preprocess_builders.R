#' Build the recipe state container
#'
#' Assembles the named-list state container that recipe loops own and update
#' in place (design D20): evolving networks as plain matrices in a named
#' sub-list, nodal attributes as one data frame per node set, and global
#' attributes as a one-row data frame. The mapping from each formula object
#' name to its container location is attached as the `object_keys` attribute
#' (columns `name`, `component`, `key`), consumed by `build_update_plan()`.
#'
#' @param object_names character vector with the formula object names,
#'   typically `rownames(objectsEffectsLink)`.
#' @param nodes,nodes2 names of the node sets of the dependent events.
#' @param envir environment where the data objects live.
#'
#' @return a list with components `networks`, `nodal`, `nodal2` (NULL for
#'   one-mode), and `globals`.
#' @noRd
build_state_container <- function(
    object_names, nodes, nodes2 = nodes, envir = new.env()) {
  objects_table <- getDataObjects(list(object_names), removeFirst = FALSE)
  n1 <- nrow(get(nodes, envir = envir))
  n2 <- nrow(get(nodes2, envir = envir))
  is_one_mode <- identical(nodes, nodes2)

  networks <- list()
  nodal_cols <- list()
  nodal2_cols <- list()
  global_cols <- list()
  components <- character(nrow(objects_table))
  keys <- character(nrow(objects_table))

  for (i in seq_len(nrow(objects_table))) {
    entry <- objects_table[i, ]
    if (!is.na(entry$object)) {
      mat <- get(entry$object, envir = envir)
      if (!is.matrix(mat)) {
        cli::cli_abort(
          "Object {.val {entry$object}} must be a matrix network."
        )
      }
      attributes(mat) <- attributes(mat)[c("dim", "dimnames")]
      networks[[entry$object]] <- mat
      components[i] <- "networks"
      keys[i] <- entry$object
    } else {
      container_obj <- get(entry$nodeset, envir = envir)
      value <- container_obj[[entry$attribute]]
      if (is.null(value)) {
        cli::cli_abort(
          "Attribute {.val {entry$attribute}} not found in
           {.val {entry$nodeset}}."
        )
      }
      if (inherits(container_obj, "global.goldfish")) {
        global_cols[[entry$attribute]] <- value
        components[i] <- "globals"
      } else if (entry$nodeset == nodes) {
        nodal_cols[[entry$attribute]] <- value
        components[i] <- "nodal"
      } else if (entry$nodeset == nodes2) {
        nodal2_cols[[entry$attribute]] <- value
        components[i] <- "nodal2"
      } else {
        cli::cli_abort(
          "Attribute {.val {entry$name}} belongs to node set
           {.val {entry$nodeset}}, which is neither {.val {nodes}} nor
           {.val {nodes2}}."
        )
      }
      keys[i] <- entry$attribute
    }
  }

  state <- list(
    networks = networks,
    nodal = data.frame(nodal_cols, row.names = NULL),
    nodal2 = if (is_one_mode) {
      NULL
    } else {
      data.frame(nodal2_cols, row.names = NULL)
    },
    globals = data.frame(global_cols, row.names = NULL)
  )
  if (length(nodal_cols) == 0) {
    state$nodal <- data.frame(matrix(nrow = n1, ncol = 0))
  }
  if (!is_one_mode && length(nodal2_cols) == 0) {
    state$nodal2 <- data.frame(matrix(nrow = n2, ncol = 0))
  }
  if (length(global_cols) == 0) {
    state$globals <- data.frame(matrix(nrow = 1, ncol = 0))
  }

  attr(state, "object_keys") <- data.frame(
    name = objects_table$name,
    component = components,
    key = keys,
    stringsAsFactors = FALSE
  )
  state
}

#' Build the recipe update plan
#'
#' Compiles the gid/fid registries and per-gid call templates that recipe
#' loops consume (design D21): which effects each object update routes to,
#' the state keys feeding each effect call in argument order, the effect
#' function formals matched once, and the `netUpdate` / `attUpdate`
#' positions per (effect, object) pair. The link matrices produced by the
#' formula parser are the only inputs — the parser itself is untouched.
#'
#' @param effects list of effect functions from `create_effects_functions()`.
#' @param events_objects_link data.frame from `get_events_and_objects_link()`.
#' @param events_effects_link matrix from `get_events_effects_link()`.
#' @param objects_effects_link matrix from `get_objects_effects_link()`.
#' @param state state container from `build_state_container()`.
#' @param stat_kind character, shape of the statistic the effects produce;
#'   part of gid identity so the same effect call with a different statistic
#'   shape gets a distinct gid.
#' @param envir environment where the data objects live.
#'
#' @return a list with `effects`, `objects`, `effect_objects` registries,
#'   `routing` (oid-indexed list of gids), and `templates` (gid-indexed call
#'   templates).
#' @noRd
build_update_plan <- function(
    effects, events_objects_link, events_effects_link, objects_effects_link,
    state, stat_kind = c("sender", "dyad"), envir = new.env()) {
  stat_kind <- match.arg(stat_kind)
  object_keys <- attr(state, "object_keys")
  object_names <- rownames(objects_effects_link)
  effect_names <- colnames(objects_effects_link)
  n_objects <- length(object_names)
  n_effects <- length(effect_names)

  if (!identical(object_keys$name, object_names)) {
    cli::cli_abort(
      "State container objects do not match {.arg objects_effects_link} rows."
    )
  }

  is_network <- object_keys$component == "networks"
  is_undirected <- vapply(
    seq_len(n_objects),
    function(oid) {
      if (!is_network[oid]) {
        return(FALSE)
      }
      object <- get(object_names[oid], envir = envir)
      inherits(object, "network.goldfish") && !attr(object, "directed")
    },
    logical(1)
  )
  shape <- ifelse(
    is_network, "dyad",
    ifelse(object_keys$component == "globals", "global", "node")
  )

  objects_registry <- data.frame(
    oid = seq_len(n_objects),
    name = object_names,
    component = object_keys$component,
    key = object_keys$key,
    shape = shape,
    is_undirected = is_undirected,
    stringsAsFactors = FALSE
  )

  effects_registry <- data.frame(
    gid = seq_len(n_effects),
    effect_name = effect_names,
    stat_kind = stat_kind,
    stringsAsFactors = FALSE
  )

  routing <- lapply(
    seq_len(n_objects),
    function(oid) unname(which(!is.na(objects_effects_link[oid, ])))
  )

  event_streams <- events_objects_link$events[-1]
  event_targets <- match(events_objects_link$name[-1], object_names)
  if (anyNA(event_targets)) {
    cli::cli_abort(
      "Event stream{?s} {.val {event_streams[is.na(event_targets)]}}
       update{?s/} object{?s} missing from {.arg objects_effects_link}."
    )
  }
  for (i in seq_along(event_streams)) {
    linked <- unname(which(!is.na(events_effects_link[i + 1L, ])))
    if (!identical(linked, routing[[event_targets[i]]])) {
      cli::cli_abort(
        "Link matrices are inconsistent: event stream
         {.val {event_streams[i]}} routes to effects
         {.val {linked}} but its object
         {.val {object_names[event_targets[i]]}} is used by effects
         {.val {routing[[event_targets[i]]]}}."
      )
    }
  }

  arg_pool_common <- c(
    "network", "attribute", "cache", "n1", "n2", "netUpdate", "attUpdate",
    "eventOrder", "interEventTime", "replace"
  )
  arg_pool <- list(
    dyad = c(arg_pool_common, "sender", "receiver"),
    node = c(arg_pool_common, "node"),
    global = arg_pool_common
  )

  templates <- vector("list", n_effects)
  effect_objects <- vector("list", n_effects)
  for (gid in seq_len(n_effects)) {
    positions <- objects_effects_link[, gid]
    used <- which(!is.na(positions))
    ordered <- used[order(positions[used])]
    ordered_components <- object_keys$component[ordered]
    is_net_arg <- ordered_components == "networks"
    formal_names <- names(formals(effects[[gid]][["effect"]]))
    templates[[gid]] <- list(
      fun = effects[[gid]][["effect"]],
      formal_names = formal_names,
      args_by_shape = lapply(
        arg_pool,
        function(pool) formal_names[formal_names %in% pool]
      ),
      net_keys = object_keys$key[ordered][is_net_arg],
      att_components = ordered_components[!is_net_arg],
      att_keys = object_keys$key[ordered][!is_net_arg],
      n_networks = sum(is_net_arg),
      n_attributes = sum(!is_net_arg)
    )
    effect_objects[[gid]] <- data.frame(
      gid = gid,
      oid = ordered,
      position = seq_along(ordered),
      net_update = ifelse(
        sum(is_net_arg) > 1L & is_net_arg,
        seq_along(ordered), NA_integer_
      ),
      att_update = ifelse(
        sum(!is_net_arg) > 1L & !is_net_arg,
        seq_along(ordered), NA_integer_
      ),
      stringsAsFactors = FALSE
    )
  }

  list(
    effects = effects_registry,
    objects = objects_registry,
    effect_objects = do.call(rbind, effect_objects),
    routing = routing,
    templates = templates
  )
}

#' Build the merged event schedule
#'
#' Merges all event streams into a single time-sorted structure of aligned
#' vectors (design D21), replacing the per-stream pointer search of the
#' monolithic loop. At equal timestamps dependent events come first and
#' remaining streams keep their list order (matching the pointer
#' semantics); within a stream the original row order is preserved. Window
#' expiry pseudo-events are part of the windowed object streams created at
#' parse time, so they are merged into the timeline here. Values are stored
#' untouched in a list to preserve their type; imputation against current
#' state stays in the recipe kernel.
#'
#' @param events named list of sanitized event data frames; the first
#'   element is the dependent events stream.
#' @param events_objects_link data.frame from `get_events_and_objects_link()`.
#' @param objects_registry the `objects` registry from `build_update_plan()`.
#'
#' @return a list of aligned vectors: `time`, `shape`, `target`,
#'   `semantics`, `sender`, `receiver`, `node`, `value`, `dependent`,
#'   `stream`, and the scalar `n`.
#' @noRd
build_event_schedule <- function(events, events_objects_link, objects_registry) {
  n_streams <- length(events)
  stream_rows <- vapply(events, nrow, integer(1))
  n_total <- sum(stream_rows)
  target_by_stream <- c(
    NA_integer_,
    match(events_objects_link$name[-1], objects_registry$name)
  )
  if (n_streams > 1 && anyNA(target_by_stream[-1])) {
    missing_streams <- names(events)[-1][is.na(target_by_stream[-1])]
    cli::cli_abort(
      "Event stream{?s} {.val {missing_streams}} target{?s/} object{?s}
       missing from the update plan."
    )
  }

  time <- numeric(n_total)
  shape <- character(n_total)
  semantics <- character(n_total)
  sender <- rep(NA_integer_, n_total)
  receiver <- rep(NA_integer_, n_total)
  node <- rep(NA_integer_, n_total)
  value <- vector("list", n_total)
  stream <- integer(n_total)

  offset <- 0L
  for (s in seq_len(n_streams)) {
    rows <- stream_rows[s]
    if (rows == 0L) next
    idx <- offset + seq_len(rows)
    stream_df <- events[[s]]
    cols <- names(stream_df)
    time[idx] <- stream_df$time
    stream[idx] <- s
    stream_semantics <- if ("increment" %in% cols) "increment" else "replace"
    semantics[idx] <- stream_semantics
    if ("node" %in% cols) {
      shape[idx] <- "node"
      node[idx] <- as.integer(stream_df$node)
    } else if ("sender" %in% cols) {
      shape[idx] <- "dyad"
      sender[idx] <- as.integer(stream_df$sender)
      receiver[idx] <- as.integer(stream_df$receiver)
    } else {
      shape[idx] <- "global"
    }
    value[idx] <- as.list(stream_df[[stream_semantics]])
    offset <- offset + rows
  }

  ordering <- order(time, stream, method = "radix")

  list(
    time = time[ordering],
    shape = shape[ordering],
    target = target_by_stream[stream[ordering]],
    semantics = semantics[ordering],
    sender = sender[ordering],
    receiver = receiver[ordering],
    node = node[ordering],
    value = value[ordering],
    dependent = stream[ordering] == 1L,
    stream = stream[ordering],
    n = n_total
  )
}
