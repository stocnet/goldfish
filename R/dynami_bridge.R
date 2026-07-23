# Internal stocnet -> environment bridge for DyNAM-i.
#
# TEMPORARY SEAM. DyNAM-i estimation still runs on the `preprocessInteraction`
# monolith (`R/model_preprocess_group.R`), which reads a legacy `data.goldfish`
# environment of named node-set / network / dependent objects and their event
# streams. The public surface now takes the single stocnet data object, so at
# the boundary this bridge reverses the assembled DyNAM-i stocnet back into that
# environment shape and hands it to the untouched front-end. It is created
# inside the estimation call and never accepted from or shown to the user.
# `refactor-dynami-engine` retires both this bridge and the monolith when the
# DyNAM-i engine moves onto the recipe loop.
#
# Correctness (design D3): the bridge extracts the raw components from the
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

# Split a `+`-joined formula right-hand side into its individual terms, each a
# language object (the intercept `1` included).
split_rate_terms <- function(expr) {
  if (is.call(expr) && identical(expr[[1L]], as.symbol("+"))) {
    return(c(split_rate_terms(expr[[2L]]), split_rate_terms(expr[[3L]])))
  }
  list(expr)
}

# Desugar the DyNAM-i flavor-keyed rate list into the single legacy formula the
# monolith consumes (design D9). The two rate models -- joining (for isolated
# actors) and leaving (for grouped ones) -- ride the flavored grammar as
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
