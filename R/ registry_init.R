.effect_registry <- new.env(parent = parent.env(environment()))

register_effect <- function(name, modify_data, init, update, check_args, meta = list()) {
  .effect_registry[[name]] <- list(
    init_data = modify_data, # function to modify the data: window and factor.
    init_stat = init,
    update_stat = update,
    meta = list(
      label = character(1),
      description = character(1),
      family = character(1)
    ),
    when_suitable = list(
      directed = logical(1),
      undirected = logical(1),
      two_mode = logical(1),
      factor = logical(1),
      sub_models = character(1), # look up table with modifications if needed, e.g., DyNAM_rate -> ego
      window = logical(1) # whether the effect is suitable for time windows, e.g., attribute effects are not suitable for time windows
    ),
    args_defaults = list(
      directed = FALSE,
      undirected = FALSE,
      ignore_repetitions = FALSE,
      two_mode = FALSE,
      factor = FALSE,
      history = c("pooling", "consecutive", "sequential"),
      type = "alter", # depends on the sub-model, e.g., DyNAM_rate -> ego
      window = Inf # NULL means no time window, "time" means time window
    ),
    check_args_fn = function(args) {
      # check arguments, inputs (network, attribute)
      if (!is.null(args$window) && args$window == "time") {
        warning("Indegree effect not meaningful for time windows.")
      }
      window <- force(args$window)
    }
  )
}

# wantWindow <- "5 min"
# depEvents ~ inertia(network, window = wantWindow)

get_effect_def <- function(name) {
  if (!exists(name, envir = .effect_registry)) {
    stop("Unknown effect: ", name)
  }
  .effect_registry[[name]]
}

make_effect <- function(name, args = list()) {
  def <- get_effect_def(name)
  def$suitable_fn(args, def$args_schema)
  structure(
    list(
      name = name,
      init = def$init,
      update = def$update,
      args = args,
      meta = def$meta,
      state = NULL
    ),
    class = "effect"
  )
}

## example
register_effect(
  name = "indeg",
  modify_data = function(network, ...) {
    # do one-time checks or setup
    list()
  },
  init = function(network, ...) {
    # do one-time checks or setup
    list()
  },
  update = function(event, network, ...) {
    # compute change statistic
    sender <- event$sender
    receiver <- event$receiver
    return(as.numeric(receiver %in% neighbors(network, "incoming")))
  },
  meta = list(
    label = "Indegree",
    description = "Returns 1 if the receiver has at least one incoming tie.",
    family = "degree"
  )
)
