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
#' Classify the effects' data objects into the state-container mapping
#'
#' Metadata-only companion to [build_state_container]: it resolves each data
#' object to its state `component` (`networks` / `nodal` / `nodal2` / `globals`)
#' and its `key`, reading object **class/structure only** — it copies no network
#' or nodal data. This is the single source of the `object_keys` mapping so the
#' upfront compile (`build_spec_map()`) can build the update plan + call templates
#' without materialising the state (design D8 metadata/data boundary); the same
#' validations (non-matrix network, missing attribute, foreign node set) fire
#' here.
#'
#' @param object_names character vector of data object names.
#' @inheritParams build_state_container
#' @return a `data.frame` with `name`, `component`, `key` columns.
#' @noRd
build_object_keys <- function(
    object_names, nodes, nodes2 = nodes, envir = new.env()) {
  objects_table <- getDataObjects(list(object_names), removeFirst = FALSE)
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
        components[i] <- "globals"
      } else if (entry$nodeset == nodes) {
        components[i] <- "nodal"
      } else if (entry$nodeset == nodes2) {
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

  data.frame(
    name = objects_table$name,
    component = components,
    key = keys,
    stringsAsFactors = FALSE
  )
}

build_state_container <- function(
    object_names, nodes, nodes2 = nodes, envir = new.env()) {
  objects_table <- getDataObjects(list(object_names), removeFirst = FALSE)
  object_keys <- build_object_keys(object_names, nodes, nodes2, envir = envir)
  n1 <- nrow(get(nodes, envir = envir))
  n2 <- nrow(get(nodes2, envir = envir))
  is_one_mode <- identical(nodes, nodes2)

  networks <- list()
  nodal_cols <- list()
  nodal2_cols <- list()
  global_cols <- list()

  for (i in seq_len(nrow(objects_table))) {
    entry <- objects_table[i, ]
    component <- object_keys$component[i]
    if (component == "networks") {
      mat <- get(entry$object, envir = envir)
      attributes(mat) <- attributes(mat)[c("dim", "dimnames")]
      networks[[entry$object]] <- mat
    } else {
      value <- get(entry$nodeset, envir = envir)[[entry$attribute]]
      if (component == "globals") {
        global_cols[[entry$attribute]] <- value
      } else if (component == "nodal") {
        nodal_cols[[entry$attribute]] <- value
      } else {
        nodal2_cols[[entry$attribute]] <- value
      }
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

  attr(state, "object_keys") <- object_keys
  state
}

#' Classify an effect's broadcast kind for the compact fan-out encoding
#'
#' Maps an effect to the broadcast `kind` it emits (design D2 / the task-0.2
#' eligibility audit): `0` = not broadcast-eligible (stays a point update),
#' `1` = constant over senders holding the alter (`alter`, degree
#' `type = "alter"`), `2` = constant over alters holding the ego (`ego`, degree
#' `type = "ego"`), `3` = constant over all actors (`global`). In sender-indexed
#' (rate) models only `global()` fans out (the per-actor effects emit distinct
#' values, so they stay point updates); dyad models additionally encode
#' `alter` / `ego` / degree projections.
#'
#' @param effect_name character, the effect's base name.
#' @param fmls the effect update function's formals (carries a resolved `type`).
#' @param stat_kind character, `"sender"` or `"dyad"` (plan-level shape).
#' @return integer broadcast kind in `{0, 1, 2, 3}`.
#' @noRd
classify_broadcast_kind <- function(effect_name, fmls, stat_kind) {
  if (identical(stat_kind, "sender")) {
    return(if (identical(effect_name, "global")) 3L else 0L)
  }
  switch(effect_name,
    alter = 1L,
    ego = 2L,
    global = 3L,
    indeg = ,
    outdeg = ,
    degree = {
      type <- if ("type" %in% names(fmls)) {
        tryCatch(eval(fmls[["type"]]), error = function(e) "alter")
      } else {
        "alter"
      }
      if (identical(type[1], "ego")) 2L else 1L
    },
    0L
  )
}

#' Collapse a fan-out change matrix into compact broadcast entries
#'
#' Re-encodes the expanded `(node1, node2, replace)` change matrix that a
#' broadcast-eligible effect produces into one `stat_mat_broadcast` column per
#' held index: `kind = 1` groups by the fixed alter (`node2`), `kind = 2` by the
#' fixed ego (`node1`), `kind = 3` is a single global entry. Each group must
#' carry a single `replace` value (constant-value fan-out); a group with mixed
#' values means the effect is not a pure broadcast and is rejected, enforcing
#' the spec's "one stat_kind per effect column".
#'
#' @param updates numeric matrix with named columns `node1`, `replace` and,
#'   for dyad models, `node2` (1-indexed actor ids).
#' @param kind integer broadcast kind in `{1, 2, 3}`.
#' @param gid integer effect id (1-indexed); stored 0-indexed as `effect`.
#' @return a 4 x g matrix with rows `kind`, `fixed`, `effect`, `replace`.
#' @noRd
broadcast_entries_from_updates <- function(updates, kind, gid) {
  abort_mixed <- function() {
    cli::cli_abort(c(
      "A broadcast-encoded effect produced non-constant fan-out values.",
      "x" = "One stat_kind per effect column: a broadcast effect must write a
             single value across its broadcast dimension.",
      "i" = "Effect id {.val {gid}} is classified as constant-value fan-out
             but emitted differing values for one held index."
    ))
  }
  if (kind == 3L) {
    reps <- updates[, "replace"]
    if (length(unique(reps)) != 1L) abort_mixed()
    return(rbind(3, 0, gid - 1, reps[1]))
  }
  fixed_col <- if (kind == 1L) "node2" else "node1"
  fixed_vals <- unique(updates[, fixed_col])
  blocks <- lapply(fixed_vals, function(fv) {
    rep_v <- updates[updates[, fixed_col] == fv, "replace"]
    if (length(unique(rep_v)) != 1L) abort_mixed()
    c(kind, fv - 1, gid - 1, rep_v[1])
  })
  do.call(cbind, blocks)
}

#' Build the per-effect call templates
#'
#' Compiles the gid-indexed call templates the recipe loops dispatch through
#' (design D8): the effect function with its formals matched once, the
#' per-shape argument lists, and the state keys feeding each call in argument
#' order. Split out of `build_update_plan()` so the templates are a distinct
#' `effects_template` object the upfront specification mapping
#' (`build_spec_map()`) owns, while the plan carries registries only.
#'
#' @param effects list of effect functions from `create_effects_functions()`.
#' @param objects_effects_link matrix from `get_objects_effects_link()`.
#' @param state state container from `build_state_container()`; only its
#'   `object_keys` attribute (name/component/key mapping) is consumed.
#'
#' @return a gid-indexed list; each element carries `fun`, `formal_names`,
#'   `args_by_shape`, `net_keys`, `att_components`, `att_keys`, `n_networks`,
#'   and `n_attributes`.
#' @noRd
build_effects_template <- function(effects, objects_effects_link, state) {
  object_keys <- attr(state, "object_keys")
  n_effects <- ncol(objects_effects_link)

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
  }
  templates
}

#' Build the recipe update plan
#'
#' Compiles the gid/fid registries that recipe loops consume (design D21):
#' which effects each object update routes to and the `netUpdate` / `attUpdate`
#' positions per (effect, object) pair. The link matrices produced by the
#' formula parser are the only inputs — the parser itself is untouched. The
#' per-effect call templates are split into `build_effects_template()`.
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
#' @return a list with `effects`, `objects`, `effect_objects` registries and
#'   `routing` (oid-indexed list of gids). The per-effect call templates are
#'   returned separately by `build_effects_template()`. The `effects` registry
#'   carries a `broadcast_kind` column (see `classify_broadcast_kind()`) that
#'   the recipe uses to route constant-value fan-out effects to the compact
#'   `stat_mat_broadcast` buffer.
#'
#' @section Reserved extension point — interaction effects (not implemented):
#' Interaction terms between effects (e.g. `global(x):alter(y)`) are a reserved
#' future capability; this change neither parses nor routes them. They matter
#' for the broadcast encoding because a constant-across-alternatives covariate —
#' notably `global()` — is **not identified as a main effect** in the DyNAM
#' choice and choice-coordination sub-models: its column is constant across the
#' alternatives, so it cancels in the multinomial likelihood. This is why
#' `classify_broadcast_kind()` only treats `global()` as a broadcast in sender
#' (rate / REM) models and why bare `global()` stays rejected in choice
#' sub-models (the `model-recipe-dispatch` `global()`-in-choice abort).
#'
#' The intended future support for `global()` (and other
#' constant-across-alternatives covariates) in choice / choice-coordination is
#' **through interaction terms with an alternative-varying effect and without a
#' main effect** (e.g. `global(x):alter(y)`, which varies across the
#' alternatives and is identified). When that work is taken up it SHOULD: (1)
#' recognise interaction syntax in the formula parser; (2) assign the product
#' effect a `stat_kind` so the recipe can route it as a point or broadcast
#' update under the broadcast-stat-updates encoding; and (3) keep the
#' bare-`global()`-in-choice abort until the interaction form exists. See the
#' `broadcast-stat-updates` eligibility rule and the `formula-parsing-link`
#' capability.
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

  broadcast_kind <- vapply(
    seq_len(n_effects),
    function(gid) {
      classify_broadcast_kind(
        effect_names[gid], formals(effects[[gid]][["effect"]]), stat_kind
      )
    },
    integer(1)
  )

  effects_registry <- data.frame(
    gid = seq_len(n_effects),
    effect_name = effect_names,
    stat_kind = stat_kind,
    broadcast_kind = broadcast_kind,
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

  effect_objects <- vector("list", n_effects)
  for (gid in seq_len(n_effects)) {
    positions <- objects_effects_link[, gid]
    used <- which(!is.na(positions))
    ordered <- used[order(positions[used])]
    ordered_components <- object_keys$component[ordered]
    is_net_arg <- ordered_components == "networks"
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
    routing = routing
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
