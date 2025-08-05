# registry_init.R

# Initialize the shared effect registry
.effect_registry <- new.env(parent = parent.env(environment()))

# Define core functions for managing effects
register_effect <- function(name, init, update, meta = list(),
                            args_schema = list( # <--- Make sure this line is correct
                              window = list(default = NULL, allowed = c("count", "time")),
                              attr = list(default = NULL, required = FALSE)
                            ),
                            suitable_fn = function(args) { TRUE } # <--- And this line
) {
  if (exists(name, envir = .effect_registry)) {
    warning(sprintf("Effect '%s' is being re-registered. This will overwrite the previous definition.", name))
  }
  .effect_registry[[name]] <- list(
    init = init,
    update = update,
    meta = meta,
    args_schema = args_schema,
    suitable_fn = suitable_fn
  )
}

get_effect_def <- function(name) {
  if (!exists(name, envir = .effect_registry)) {
    stop("Unknown effect: ", name)
  }
  .effect_registry[[name]]
}

make_effect <- function(name, args = list()) {
  def <- get_effect_def(name)
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