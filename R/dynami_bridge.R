# Internal stocnet -> environment bridge for DyNAM-i.
#
# TEMPORARY SEAM. DyNAM-i estimation still runs on the `preprocessInteraction`
# monolith (`R/model_preprocess_group.R`), which reads a legacy `data.goldfish`
# environment of named node-set / network / dependent objects and their event
# streams. The public surface now takes the single stocnet data object, so at
# the boundary this bridge reverses the assembled DyNAM-i stocnet back into that
# environment shape and hands it to the untouched front-end. It is created
# inside the estimation call and never accepted from or shown to the user.
# Both this bridge and the monolith go away once the DyNAM-i engine moves onto
# the recipe loop the other models use.
#
# Why it is faithful: the bridge extracts the raw components from the
# stocnet and rebuilds the environment objects through the SAME legacy
# constructors (`make_nodes()` / `make_network()` / `link_events()` /
# `make_dependent_events()`) the DyNAM-i data path always used, so the objects
# are constructor-identical by construction; the name-recording attributes
# (`events`, `nodes`, `default_network`) are then pinned to the bridge's own
# canonical env names so the environment is internally consistent regardless of
# how the objects were built.

# Canonical names the bridged environment binds its objects under. The focal
# layer keeps its own name (the interaction network); the derived streams and
# node sets take stable names the monolith resolves through the pinned
# attributes.
dynami_env_names <- function(focal) {
  list(
    actors = "actors",
    groups = "groups",
    interactions = focal,
    past = "past",
    # The raw dependent/exogenous event streams the network's `events` attribute
    # names, distinct from the `dependent.goldfish` object built from the
    # dependent stream (`dependent_object`).
    dependent = paste0(focal, "_dependent"),
    exogenous = paste0(focal, "_exogenous"),
    dependent_object = paste0(focal, "_dependent_events"),
    past_updates = "past_updates"
  )
}

# Rewrite a DyNAM-i stocnet-surface formula onto the bridged environment's names.
# On the stocnet surface networks are named by layer (`interactions` / `past`,
# which the bridge binds under those names) and nodal attributes are bare
# (`ego(age)`), consistent with the two-mode stocnet surface. The monolith parses
# against the environment, where an attribute lives on the `actors` node set, so
# each bare attribute operand is rewritten to `actors$<name>` and the focal layer
# name on the left-hand side is rewritten to the dependent-events object.
rewrite_dynami_formula <- function(formula, data) {
  focal <- data$info$focal
  nm <- dynami_env_names(focal)
  attribute_names <- setdiff(names(data$nodes), c("label", "mode"))
  actor_symbol <- as.symbol(nm$actors)

  # The left-hand side names the dependent process by its focal layer.
  if (
    length(formula) == 3L &&
      is.symbol(formula[[2L]]) &&
      identical(as.character(formula[[2L]]), focal)
  ) {
    formula[[2L]] <- as.symbol(nm$dependent_object)
  }

  # A bare symbol that names a nodal attribute reads it off the actor node set.
  rewrite_operands <- function(expr) {
    if (is.symbol(expr) && as.character(expr) %in% attribute_names) {
      return(call("$", actor_symbol, expr))
    }
    if (is.call(expr) && length(expr) > 1L) {
      for (i in 2:length(expr)) {
        expr[[i]] <- rewrite_operands(expr[[i]])
      }
    }
    expr
  }
  rhs <- length(formula)
  formula[[rhs]] <- rewrite_operands(formula[[rhs]])
  formula
}

# The flavor values a DyNAM-i focal layer stamps on its dependent rows.
dynami_focal_flavors <- function(data, focal) {
  focal_rows <- data$ties$layer == focal
  sort(unique(stats::na.omit(data$ties$flavor[focal_rows])))
}

# Per-event joining availability for the DyNAM-i choice, derived from the focal
# interaction layer's occupancy. The choice events are the focal layer's
# dependent join rows, in the reserved `order`; replaying membership over every
# focal tie (initial diagonal, then join/leave/exogenous increments in order)
# reconstructs, just before each join decision, which groups are occupied
# (`indeg >= 1`). The available set is the occupied groups -- Hoffman et al.
# Eq. 8's denominator over the present second-mode nodes, which includes the
# joiner's own singleton (an isolate may choose to stay isolated, and the
# construction records such observed choices as joining the own singleton, so it
# must remain in the choice set -- excluding it would zero the probability of
# those observed events). Returned as one length-n2 logical mask per join event,
# ready to fold into the dense `active_dyad`.
dynami_choice_availability <- function(data) {
  focal <- data$info$focal
  actor_mode <- unname(data$info$sender[[focal]])
  group_mode <- unname(data$info$receiver[[focal]])
  n_actors <- sum(data$nodes$mode == actor_mode)
  n_groups <- sum(data$nodes$mode == group_mode)

  ties <- data$ties[data$ties$layer == focal, , drop = FALSE]
  init_rows <- ties[is.na(ties$time), , drop = FALSE]
  event_rows <- ties[!is.na(ties$time), , drop = FALSE]
  event_rows <- event_rows[order(event_rows$order), , drop = FALSE]

  membership <- matrix(0, n_actors, n_groups)
  membership[cbind(init_rows$from, init_rows$to - n_actors)] <- init_rows$weight

  is_join <- !is.na(event_rows$flavor) & event_rows$flavor == "join"
  masks <- vector("list", sum(is_join))
  k <- 0L
  for (r in seq_len(nrow(event_rows))) {
    actor <- event_rows$from[r]
    grp <- event_rows$to[r] - n_actors
    if (is_join[r]) {
      k <- k + 1L
      masks[[k]] <- colSums(membership) > 0
    }
    membership[actor, grp] <- membership[actor, grp] + event_rows$weight[r]
  }
  masks
}

# Fold per-event joining availability masks into the choice statsList's dense
# `active_dyad` point encoding. The choice risk set reads only the event
# sender's row, so `build_active_dyad_point()` seeds the dense n1 x n2 init from
# the first event and emits net (sender, group, replace) flips per later event --
# the maintained-availability object `compute_step.default` consumes when
# `active_dyad_folded` is set. Base receiver presence is all groups present
# (groups never leave the object); the masks carry the occupancy restriction.
dynami_fold_availability <- function(prep, masks) {
  n1 <- length(prep$active_sender_init)
  n2 <- length(prep$active_dyad_init)
  n_stored <- length(prep$event_sender)
  recv <- rep(list(rep(TRUE, n2)), n_stored)
  senders <- as.list(prep$event_sender)
  build_active_dyad_point(prep, recv, masks, senders, n1, n2)
}

# The monolith object mapped to the recipe shape every shared consumer reads,
# joining availability folded in where the sub-model has one. Estimation and the
# statistics export both enter through here, so a gather stack and a fit
# describe the same risk set by construction rather than by two matching call
# sequences.
dynami_recipe_input <- function(
  prep,
  sub_model,
  is_two_mode,
  availability = NULL
) {
  prep <- dynami_recipe_statslist(prep, sub_model, is_two_mode)
  if (!is.null(availability)) {
    prep <- dynami_fold_availability(prep, availability)
  }
  prep
}

# The DyNAM-i choice availability as a support-constraint formula: a joining
# actor chooses among the groups occupied at the decision point (`indeg >= 1`).
# This is the grammar statement of what `dynami_choice_availability()` folds
# directly from occupancy; the own singleton is kept (Hoffman et al. Eq. 8's
# denominator over the present second-mode nodes).
dynami_availability_constraint <- function(focal) {
  focal_symbol <- as.symbol(focal)
  rhs <- call(">=", call("indeg", focal_symbol), 1)
  stats::as.formula(call("~", rhs), env = baseenv())
}

# Split a `+`-joined formula right-hand side into its individual terms, each a
# language object (the intercept `1` included).
split_rate_terms <- function(expr) {
  if (is.call(expr) && identical(expr[[1L]], as.symbol("+"))) {
    return(c(split_rate_terms(expr[[2L]]), split_rate_terms(expr[[3L]])))
  }
  list(expr)
}

# Desugar the DyNAM-i flavor-keyed rate list into the single legacy formula the
# monolith consumes. The two rate models -- joining (for isolated actors) and
# leaving (for grouped ones) -- ride the flavored grammar as
# `rate = list(join ~ ..., leave ~ ...)`, but the monolith reads one formula
# whose effects carry a per-effect `joining` flag: every `join`-keyed term gains
# `joining = 1`, every `leave`-keyed term `joining = -1` (an effect under both
# keys becomes two terms). A per-flavor `~ 1` intercept becomes an explicit
# `intercept(<focal>, joining = ...)` -- the bare `1` is inert for a DyNAM-i rate,
# so the intercept parameter is the flagged `intercept()` effect. The `joining`
# flag never appears on the keyed surface. The desugarer is a bridge seam,
# retired with the monolith by the DyNAM-i engine conversion.
desugar_dynami_rate <- function(rate, data, focal, call = rlang::caller_env()) {
  if (!is.list(rate) || inherits(rate, "formula")) {
    cli::cli_abort(
      c(
        "The DyNAM-i {.arg rate} must be a flavor-keyed list.",
        "i" = "Use {.code rate = list(join ~ ..., leave ~ ...)} to model the \\
               joining and leaving rates."
      ),
      call = call
    )
  }
  allowed <- dynami_focal_flavors(data, focal)
  focal_symbol <- as.symbol(focal)

  terms <- list()
  for (entry in rate) {
    if (!inherits(entry, "formula") || length(entry) != 3L) {
      cli::cli_abort(
        "Each DyNAM-i {.arg rate} entry must be a two-sided formula \\
         ({.code join ~ ...}).",
        call = call
      )
    }
    flavor <- as.character(entry[[2L]])
    if (!flavor %in% allowed) {
      cli::cli_abort(
        c(
          "Unknown DyNAM-i rate flavor {.val {flavor}}.",
          "i" = "The focal layer {.val {focal}} models flavor{?s} \\
                 {.or {.val {allowed}}}."
        ),
        call = call
      )
    }
    joining <- if (identical(flavor, "join")) 1 else -1
    for (term in split_rate_terms(entry[[3L]])) {
      if (is.numeric(term) && term == 1) {
        terms[[length(terms) + 1L]] <- as.call(
          list(as.symbol("intercept"), focal_symbol, joining = joining)
        )
      } else if (is.call(term)) {
        term[["joining"]] <- joining
        terms[[length(terms) + 1L]] <- term
      } else {
        cli::cli_abort(
          "Unsupported DyNAM-i rate term {.code {deparse(term)}}.",
          call = call
        )
      }
    }
  }

  rhs <- Reduce(function(a, b) call("+", a, b), terms)
  stats::as.formula(call("~", focal_symbol, rhs), env = environment(rate[[1L]]))
}

# Reverse a focal/past layer's ties into a legacy event data frame keyed by the
# node labels, ordered by the reserved `order` column, with the `order`
# attribute and update class the monolith reads restored.
dynami_layer_events <- function(rows, labels, event_class) {
  ord <- order(rows$order)
  events <- data.frame(
    time = rows$time[ord],
    sender = labels[rows$from[ord]],
    receiver = labels[rows$to[ord]],
    increment = rows$weight[ord],
    stringsAsFactors = FALSE
  )
  attr(events, "order") <- rows$order[ord]
  class(events) <- c(class(events), event_class)
  events
}

# Build the DyNAM-i legacy environment from an assembled stocnet.
stocnet_to_dynami_env <- function(
  data,
  parent_env = parent.frame(),
  call = rlang::caller_env()
) {
  # The bridge rebuilds through the deprecated constructors on purpose; their
  # lifecycle signal is an implementation detail here.
  withr::local_options(lifecycle_verbosity = "quiet")

  info <- data$info
  nodes <- data$nodes
  ties <- data$ties
  labels <- nodes$label
  focal <- info$focal
  nm <- dynami_env_names(focal)

  actor_mode <- unname(info$sender[[focal]])
  group_mode <- unname(info$receiver[[focal]])
  actor_rows <- nodes$mode == actor_mode
  group_rows <- nodes$mode == group_mode
  n_actors <- sum(actor_rows)

  actors_df <- nodes[actor_rows, setdiff(names(nodes), "mode"), drop = FALSE]
  groups_df <- data.frame(
    label = nodes$label[group_rows],
    present = TRUE,
    stringsAsFactors = FALSE
  )
  rownames(actors_df) <- NULL

  # Split the focal layer: dependent join/leave rows (flavor set), exogenous
  # rows (flavor NA, timed), and the initial diagonal (flavor NA, time NA).
  inter <- ties[ties$layer == focal, , drop = FALSE]
  is_dependent <- !is.na(inter$flavor)
  is_initial <- is.na(inter$flavor) & is.na(inter$time)
  is_exogenous <- is.na(inter$flavor) & !is.na(inter$time)

  dependent_events <- dynami_layer_events(
    inter[is_dependent, , drop = FALSE],
    labels,
    "interaction.groups.updates"
  )
  exogenous_events <- dynami_layer_events(
    inter[is_exogenous, , drop = FALSE],
    labels,
    "interaction.groups.updates"
  )

  initial <- inter[is_initial, , drop = FALSE]
  init_mat <- matrix(
    0,
    n_actors,
    nrow(groups_df),
    dimnames = list(actors_df$label, groups_df$label)
  )
  init_mat[cbind(initial$from, initial$to - n_actors)] <- initial$weight

  actors_obj <- make_nodes(actors_df)
  groups_obj <- make_nodes(groups_df)
  interactions_obj <- make_network(
    matrix = init_mat,
    nodes = actors_obj,
    nodes2 = groups_obj,
    directed = TRUE
  )
  interactions_obj <- link_events(
    interactions_obj,
    dependent_events,
    nodes = actors_obj,
    nodes2 = groups_obj
  )
  interactions_obj <- link_events(
    interactions_obj,
    exogenous_events,
    nodes = actors_obj,
    nodes2 = groups_obj
  )
  dependent_obj <- make_dependent_events(
    events = dependent_events,
    nodes = actors_obj,
    nodes2 = groups_obj,
    default_network = interactions_obj
  )

  past_rows <- ties[ties$layer == "past", , drop = FALSE]
  has_past <- nrow(past_rows) > 0
  if (has_past) {
    past_events <- dynami_layer_events(
      past_rows,
      labels,
      "interaction.network.updates"
    )
    past_obj <- make_network(nodes = actors_obj, directed = FALSE)
    past_obj <- link_events(past_obj, past_events, nodes = actors_obj)
  }

  # Pin the name-recording attributes to the bridge's canonical names AFTER all
  # constructors run (make_dependent_events() validates the network's `nodes`
  # against the dependent's node sets, so the pin must not precede it). The
  # constructors deparse their argument names -- the bridge's locals -- which are
  # not the env bindings; pinning makes the environment resolve consistently.
  attr(interactions_obj, "events") <- c(nm$dependent, nm$exogenous)
  attr(interactions_obj, "nodes") <- c(nm$actors, nm$groups)
  attr(dependent_obj, "default_network") <- nm$interactions
  attr(dependent_obj, "nodes") <- c(nm$actors, nm$groups)

  # The monolith evaluates effect operand expressions in this environment, so
  # its parent chain must reach the base functions they use (matching the legacy
  # `make_data()` environment, whose parent is the caller's frame).
  env <- new.env(parent = parent_env)
  assign(nm$actors, actors_obj, envir = env)
  assign(nm$groups, groups_obj, envir = env)
  assign(nm$interactions, interactions_obj, envir = env)
  assign(nm$dependent, dependent_events, envir = env)
  assign(nm$exogenous, exogenous_events, envir = env)
  assign(nm$dependent_object, dependent_obj, envir = env)
  if (has_past) {
    attr(past_obj, "events") <- nm$past_updates
    attr(past_obj, "nodes") <- nm$actors
    assign(nm$past, past_obj, envir = env)
    assign(nm$past_updates, past_events, envir = env)
  }

  env
}

# Convert the DyNAM-i monolith preprocessed object into the recipe statsList
# shape the shared Newton-Raphson kernel consumes.
#
# TEMPORARY SEAM (retired with the monolith by the DyNAM-i engine conversion).
# The interaction monolith (`preprocess_interaction`) still emits the pre-recipe
# preprocessing shape -- a 3D `initial_stats` (actors x groups x effects),
# per-event `dependent_stats_change` / `right_censored_stats_change` nested lists (one
# change matrix per effect), and `order_events` (1 = dependent, 2 =
# right-censored). The shared kernel (`run_nr_loop` / `compute_step.default`)
# instead reads the recipe shape: an integer `is_dependent`, a per-event
# `intervals` vector, a flat 4 x M point buffer `stat_mat_update` with a
# per-event `stat_mat_pointer`, `active_sender_init` / `active_dyad_init`
# presence vectors, and (for the rate intercept initialization) the
# `n_dep_events` / `total_time` / `avg_active_entity` scalars. This converter
# maps one to the other so DyNAM-i estimates on the same kernel as DyNAM / REM
# without touching the monolith.
#
# Rate reduction: the monolith carries an actors x groups statistic per effect,
# but the DyNAM-i rate is sender-indexed (competing risks over actors), so each
# effect slice reduces to one value per actor -- the row mean over groups for a
# two-mode object -- reproducing the pre-recipe reduce-matrix-to-vector step. The
# monolith already emits per-actor (2-column) rate change rows, so those apply as
# flat sender updates onto the reduced matrix. Choice keeps the 3D array and its
# 3-column change rows as dyad point updates.
dynami_recipe_statslist <- function(prep, sub_model, is_two_mode) {
  order_events <- unlist(prep$order_events)
  n_events <- length(order_events)
  is_dependent <- as.integer(order_events == 1L)
  is_rate <- sub_model == "rate"

  dims <- dim(prep$initial_stats)
  n1 <- dims[1L]
  n2 <- dims[2L]
  n_effects <- dims[3L]

  if (is_rate) {
    reduce_slice <- function(slice) {
      if (is_two_mode) {
        rowMeans(slice, na.rm = TRUE)
      } else {
        rowSums(slice, na.rm = TRUE) / (ncol(slice) - 1)
      }
    }
    initial_stats <- vapply(
      seq_len(n_effects),
      function(k) reduce_slice(prep$initial_stats[,, k]),
      numeric(n1)
    )
  } else {
    initial_stats <- prep$initial_stats
  }

  # Flatten each event's per-effect change matrices into the 4 x M point buffer
  # (rows: node1, node2, effect, replace; monolith indices are 1-based, the
  # buffer is 0-based). Node2 is unused for sender (rate) updates.
  dep_change <- prep$dependent_stats_change
  rc_change <- prep$right_censored_stats_change
  dep_ptr <- 0L
  rc_ptr <- 0L
  event_cols <- vector("list", n_events)
  for (i in seq_len(n_events)) {
    if (order_events[i] == 1L) {
      dep_ptr <- dep_ptr + 1L
      change_i <- dep_change[[dep_ptr]]
    } else {
      rc_ptr <- rc_ptr + 1L
      change_i <- rc_change[[rc_ptr]]
    }
    blocks <- vector("list", length(change_i))
    for (k in seq_along(change_i)) {
      mat <- change_i[[k]]
      if (is.null(mat) || nrow(mat) == 0L) {
        next
      }
      blocks[[k]] <- if (is_rate) {
        rbind(mat[, "node1"] - 1L, 0, k - 1L, mat[, "replace"])
      } else {
        rbind(
          mat[, "node1"] - 1L,
          mat[, "node2"] - 1L,
          k - 1L,
          mat[, "replace"]
        )
      }
    }
    blocks <- blocks[!vapply(blocks, is.null, logical(1))]
    event_cols[[i]] <- if (length(blocks)) do.call(cbind, blocks) else NULL
  }
  stat_mat_update <- do.call(cbind, event_cols)
  if (is.null(stat_mat_update)) {
    stat_mat_update <- matrix(0, 4L, 0L)
  }
  stat_mat_pointer <- cumsum(vapply(
    event_cols,
    function(x) if (is.null(x)) 0L else ncol(x),
    integer(1)
  ))

  # Interleave dependent and right-censored waiting times by event order.
  dep_iv <- unlist(prep$intervals)
  rc_iv <- unlist(prep$right_censored_intervals)
  intervals <- numeric(n_events)
  di <- 0L
  ri <- 0L
  for (i in seq_len(n_events)) {
    if (order_events[i] == 1L) {
      di <- di + 1L
      intervals[i] <- dep_iv[di]
    } else {
      ri <- ri + 1L
      intervals[i] <- rc_iv[ri]
    }
  }

  prep$initial_stats <- initial_stats
  prep$is_dependent <- is_dependent
  prep$intervals <- intervals
  prep$stat_mat_update <- stat_mat_update
  prep$stat_mat_pointer <- stat_mat_pointer
  prep$stat_mat_broadcast <- matrix(0, 4L, 0L)
  prep$stat_mat_broadcast_pointer <- rep(0, n_events)
  prep$event_time <- unlist(prep$event_time)
  prep$event_sender <- unlist(prep$event_sender)
  prep$event_receiver <- unlist(prep$event_receiver)
  prep$active_sender_init <- rep(TRUE, n1)
  prep$active_sender_changes <- list()
  prep$active_sender_update <- NULL
  prep$active_sender_update_pointer <- NULL
  prep$active_dyad_init <- rep(TRUE, if (is_rate) n1 else n2)
  prep$active_dyad_changes <- list()
  prep$active_dyad_update <- NULL
  prep$active_dyad_update_pointer <- NULL
  prep$active_dyad_encoding <- NULL

  if (is_rate) {
    # The rate intercept is initialized from a crude event rate; the exact
    # active-entity count only sets the Newton-Raphson start, not the optimum.
    prep$n_dep_events <- sum(is_dependent == 1L)
    prep$total_time <- sum(dep_iv, na.rm = TRUE) + sum(rc_iv, na.rm = TRUE)
    prep$avg_active_entity <- n1
  }

  prep
}
