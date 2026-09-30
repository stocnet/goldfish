# =========================================================================== #
# The augmenter contract: what an augment_seq_*() constructor builds and what
# the ABMCEM loop (run_abem_loop()) calls.
#
# An augmenter is built once, at estimate_dynes() entry, by a constructor
# `function(spec, recipe, control, call)` over the completed joint
# specification and the compiled augmentation_recipe, so the data window is
# bound at construction rather than passed per call. It provides
#
# * `draw(iteration, draw_index, theta)`: one endpoint-hitting sequence per
#   entry of `draw_index`, each `list(data, log_proposal, restarted)` -- `data`
#   the drawn stocnet (see emit_augmented_data()), `log_proposal` the finite
#   log proposal density of the full generative path, `restarted` whether a
#   reject-and-restart fired;
# * optionally `init(proposal_evaluator, theta)`, called once per EM iteration
#   with the theta-closed proposal evaluator an MCMC chain scores candidates
#   with, and `diagnostics()`, whose return the loop records in its trace.
#
# A draw depends only on the random number stream it runs on: the loop keys
# the substream (keyed_augmenter()), never the augmenter. An independent draw
# may be shipped to a worker, so `draw` holds everything it needs in its own
# closure and builds its mutable state -- the recipe cursor -- per draw rather
# than sharing one across draws.
#
# Augmenters drive the multi-process walk from outside: the augmenter owns the
# loop and steps a handle (walk_open(), walk_advance(), walk_evaluate(),
# walk_inject(), walk_schedule(), walk_replay(), walk_next_breakpoint()). No
# per-event hook is added to the preprocessing recipe loop. Live support comes
# from the walk at the current state; the recipe supplies only which events
# must be placed.
# =========================================================================== #

# The augmenter constructors, keyed by the `routine` set_algorithm_em() selects.
# A new variant is one more entry here: the loop reads the contract, never the
# routine. The augment_seq_*() augmenters land with the sequence-augmentation
# work; until a routine has one, estimate_dynes() stays unexported so no user
# can reach resolve_dynes_augmenter()'s abort.
dynes_augmenter_constructors <- function() {
  list(random = augment_seq_random, sim = augment_seq_sim)
}

# The augmenter the control selects, built once over the compiled recipe.
resolve_dynes_augmenter <- function(
  spec,
  recipe,
  control,
  call = rlang::caller_env()
) {
  routine <- control$augmenter$routine
  constructor <- dynes_augmenter_constructors()[[routine]]
  if (is.null(constructor)) {
    cli::cli_abort(
      c(
        "No augmenter is available for {.arg routine} =
         {.val {routine}} yet.",
        "i" = "The sequence augmenters are not part of this version of
               {.pkg goldfish}."
      ),
      class = "goldfish_dynes_no_augmenter",
      call = call
    )
  }
  augmenter <- constructor(spec, recipe, control, call = call)
  if (!inherits(augmenter, "goldfishAugmenter")) {
    cli::cli_abort(
      "The {.val {routine}} constructor must return a
       {.cls goldfishAugmenter} from {.fn new_dynes_augmenter}.",
      call = call
    )
  }
  augmenter
}

# Assemble and check an augmenter. `draw` is required; `init` and
# `diagnostics` are optional and left out of the object when NULL, so the
# loop's `is.function()` probes read their absence. `coupling` labels the
# support constraints a restart can come from, which the loop's coupling
# warning names.
new_dynes_augmenter <- function(
  draw,
  routine,
  init = NULL,
  diagnostics = NULL,
  coupling = character(0),
  call = rlang::caller_env()
) {
  check_augmenter_function(draw, "draw", c("iteration", "draw_index", "theta"))
  if (!is.null(init)) {
    check_augmenter_function(init, "init", c("proposal_evaluator", "theta"))
  }
  if (!is.null(diagnostics)) {
    check_augmenter_function(diagnostics, "diagnostics", character(0))
  }
  augmenter <- list(draw = draw, init = init, diagnostics = diagnostics)
  augmenter <- augmenter[!vapply(augmenter, is.null, logical(1))]
  augmenter$routine <- routine
  augmenter$coupling <- coupling
  structure(augmenter, class = "goldfishAugmenter")
}

check_augmenter_function <- function(
  fn,
  name,
  formals_expected,
  call = rlang::caller_env()
) {
  # as.character(): a function without arguments has NULL formals names.
  if (
    !is.function(fn) ||
      !identical(as.character(names(formals(fn))), formals_expected)
  ) {
    cli::cli_abort(
      "An augmenter's {.fn {name}} must be a function of
       {.arg {formals_expected}}.",
      call = call
    )
  }
  invisible(fn)
}

# The theta-free context every augmenter builds once from the completed
# specification and the recipe: the observed data with the latent part of the
# modeled panel layers removed (their timed rows inside the wave grid), which
# every drawn sequence completes. Rows at or before the observation start are
# the initial condition and stay; rows after the last wave lie outside the
# estimation window and stay too, since every sequence reaches the last wave in
# its observed state.
new_augmenter_context <- function(spec, recipe, call = rlang::caller_env()) {
  if (!inherits(recipe, "goldfishAugRecipe")) {
    cli::cli_abort(
      "{.arg recipe} must be a {.cls goldfishAugRecipe}.",
      call = call
    )
  }
  data <- spec$data
  ties <- data$ties
  waves <- recipe$wave_times
  latent <- ties$layer %in%
    names(recipe$layers) &
    !is.na(ties$time) &
    ties$time > waves[[1L]] &
    ties$time <= waves[[length(waves)]]
  data$ties <- ties[!latent, , drop = FALSE]
  rownames(data$ties) <- NULL
  structure(
    list(spec = spec, recipe = recipe, base = data),
    class = "goldfishAugContext"
  )
}

# The drawn stocnet of one sequence: the context's base data plus one ties row
# per recipe event at `times` (one time per `recipe$events` row, in its
# order). Each row carries the event's update value -- its step under
# `increment`, the state it sets under `replace` -- and, on a flavored layer,
# the flavor key add_flavor() stamps, because the pool entry routes every event
# to its flavor's processes by that key. The ties come out in time order with
# history rows first, and `info` and `nodes` are the specification's, so the
# sequence rebinds onto the specification unchanged (bind_candidate_data()).
emit_augmented_data <- function(context, times) {
  events <- context$recipe$events
  base <- context$base
  info <- base$info
  update <- unname(info$update[events$layer])
  value <- ifelse(
    update == "replace",
    events$to_value,
    events$to_value - events$from_value
  )
  ties <- base$ties
  drawn <- ties[rep(NA_integer_, nrow(events)), , drop = FALSE]
  drawn$from <- events$from
  drawn$to <- events$to
  drawn$time <- as.numeric(times)
  drawn$layer <- events$layer
  if (!"weight" %in% names(ties) && any(value != 1)) {
    ties$weight <- rep(1, nrow(ties))
  }
  if ("weight" %in% names(ties)) {
    drawn$weight <- value
  }
  if ("flavor" %in% names(ties) || any(!is.na(events$flavor))) {
    ties$flavor <- ties[["flavor"]] %||% rep(NA_character_, nrow(ties))
    drawn$flavor <- events$flavor
  }
  ties <- rbind(ties, drawn[names(ties)])
  ties <- ties[
    order(!is.na(ties$time), ties$time, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(ties) <- NULL
  base$ties <- ties
  base
}

# A walk over one sequence: the completed specification rebound to `data` and
# opened. The driver then steps it. Each open replays from the start of the
# data, so a driver keeps its handle for the whole draw.
#
# The default opens on the observed data, not the base. A modeled layer's rows
# are never applied by walk_advance() -- the driver injects them -- so the
# wave-stamped panel rows leave the walked state untouched, while the base
# would hand the panel processes an empty dependent-event set, which the
# preprocessing substrate does not expect (it warns taking the extent of no
# events).
open_augmenter_walk <- function(
  context,
  data = context$spec$data,
  control_preprocessing = set_preprocessing(),
  call = rlang::caller_env()
) {
  walk_open(
    bind_candidate_data(context$spec, data, call = call),
    control_preprocessing = control_preprocessing,
    call = call
  )
}
