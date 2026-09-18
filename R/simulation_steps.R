# =========================================================================== #
# The simulation driver's plug points.
#
# `simulate()` runs ONE loop. Everything a model variant changes about that
# loop is a closure the caller supplies, so a parametric clock, a coordination
# mark kernel, an endpoint-conditioned augmenter and an external latent-variable
# package are callers of the same driver rather than forks of it:
#
#   per replicate  parameters$init(replicate, handle)   replicate-level draws
#   per step       parameters$at(k, t, handle, latent)  this step's parameters
#                  evaluate(stats, theta, risk, meta)   the fid's values
#                  clock(rates, t, handle)              waiting time, which fid
#                  mark(fid, evaluation, handle)        who / whom
#                  accept(event, handle)                inject, or not
#
# Every slot defaults to goldfish's own step, keyed on the specification's
# behavioral descriptor, so a plain `simulate(fit, data = )` supplies none of
# them. The walk handle reaches a closure as an OPAQUE object: a step reads it
# only through the accessors at the bottom of this file, so the handle's
# internals stay free to change without breaking a consumer package.
# =========================================================================== #

# --------------------------------------------------------------------------- #
# The parameter provider (plug points P0 and P1)
# --------------------------------------------------------------------------- #

#' Supply simulation parameters that change during a run
#'
#' Builds the parameter provider [simulate()] calls to obtain the parameters of
#' each step. A provider is what `coef` becomes when the parameters are not one
#' constant vector per process: an actor random effect drawn once per replicate,
#' a hidden-Markov regime that switches between events, a posterior draw.
#'
#' `init()` runs once per replicate and returns whatever the run needs to carry
#' (a matrix of actor deviations, an initial regime, a draw index) — the
#' `latent` object, opaque to goldfish. `at()` runs once per step and returns
#' that step's parameters.
#'
#' `at()` returns a list with:
#' \describe{
#'   \item{`parameters`}{a list of parameter vectors named by formula id (as
#'     character), one entry per process the walk carries. Each is the vector
#'     the evaluation step accepts — for goldfish's own evaluator, length
#'     `p_fid`, a leading intercept for a timed rate.}
#'   \item{`latent`}{optional, the latent object to carry into the next step
#'     (a regime after a switch). Absent leaves it unchanged.}
#'   \item{`breakpoint`}{optional, a time before which the clock must call
#'     `at()` again. The driver treats it as a competing exit that switches
#'     parameters and injects no event, which keeps the exponential clock exact
#'     across a continuous-time regime jump.}
#' }
#'
#' @param init a function of `(replicate, handle)` returning the replicate's
#'   latent object, or `NULL` for a provider with no replicate-level draw.
#' @param at a function of `(k, t, handle, latent)` returning the step's
#'   parameters, as described above. Required.
#'
#' @return An object of class `goldfishParamProvider`, a list with components:
#'   \item{init}{Value from the `init` argument, or a function returning
#'     `NULL`.}
#'   \item{at}{Value from the `at` argument.}
#'
#' @seealso [set_simulation_steps()] for the other plug points,
#'   [set_parameters()] for the constant joint-specification form.
#' @family simulation
#' @export
#' @examples
#' # A two-regime provider: the parameters switch when the regime does.
#' provider <- set_parameter_provider(
#'   init = function(replicate, handle) list(regime = 1L),
#'   at = function(k, t, handle, latent) {
#'     theta <- if (latent$regime == 1L) c(-3, 0.2) else c(-3, -0.2)
#'     list(parameters = list(`1` = theta))
#'   }
#' )
#' class(provider)
set_parameter_provider <- function(init = NULL, at = NULL) {
  call <- rlang::current_env()
  if (is.null(at)) {
    cli::cli_abort(
      c(
        "{.fn set_parameter_provider} needs an {.arg at} function.",
        "i" = "{.arg at} is called once per step as
               {.code at(k, t, handle, latent)} and returns that step's
               parameters."
      ),
      call = call,
      class = "goldfish_sim_bad_step"
    )
  }
  assert_step_closure(at, "at", 4L, call)
  if (is.null(init)) {
    init <- function(replicate, handle) NULL
  } else {
    assert_step_closure(init, "init", 2L, call)
  }
  structure(list(init = init, at = at), class = "goldfishParamProvider")
}

is_parameter_provider <- function(x) inherits(x, "goldfishParamProvider")

# The provider every constant `coef` becomes: one fixed list of per-fid vectors,
# no replicate draw, no breakpoint. Resolving the constant forms to a provider
# rather than branching on them keeps the loop single-shaped -- the driver only
# ever calls `at()`.
constant_parameter_provider <- function(parameters) {
  set_parameter_provider(
    at = function(k, t, handle, latent) list(parameters = parameters)
  )
}

# --------------------------------------------------------------------------- #
# The step object (plug points PE, P2, P3, P4)
# --------------------------------------------------------------------------- #

#' Supply the steps of the simulation loop
#'
#' Builds the object [simulate()] takes as `steps`, replacing any of the
#' driver's own steps with the caller's. Every slot left `NULL` keeps
#' goldfish's step for that point, chosen from the specification's behavior, so
#' a caller overrides only what its model changes.
#'
#' The contracts, in the order one step of the loop calls them:
#'
#' \describe{
#'   \item{`parameters`}{a [set_parameter_provider()] object. Supplying it here
#'     is equivalent to passing it as `coef`; `coef` wins if both are given.}
#'   \item{`evaluate`}{`function(stats, theta, risk, meta)` returning the
#'     values over the process's candidate space — hazards for a timed family,
#'     probabilities for a multinomial one, exact zeros outside the risk set.
#'     `stats` is the process's statistics rows in sender-major order, `theta`
#'     the parameters as the provider returned them, `risk` the live risk set
#'     (`active_sender`, `active_dyad`, `encoding`), and `meta` a list naming
#'     the `family`, `n_actors1`, `n_actors2` and, for one sender's choice row,
#'     the `sender`. This is where a model variant lives: a per-actor random
#'     effect is a row-wise product over a matrix `theta`, a regime mixture a
#'     weighted sum, a non-log-linear hazard a different link.}
#'   \item{`clock`}{`function(rates, t, handle)` returning
#'     `list(wait, fid, kind)` — the waiting time, which process fires, and
#'     whether the draw is an `"event"` or a `"breakpoint"`. `rates` is the
#'     per-fid total rate at the current state.}
#'   \item{`mark`}{`function(fid, evaluation, handle)` returning the event to
#'     inject: a list with `layer`, and `sender`/`receiver` (a dyadic layer) or
#'     `node`, plus `increment` or `replace`.}
#'   \item{`accept`}{`function(event, handle)` returning `TRUE` to inject the
#'     event. The default always accepts; an endpoint-conditioned augmenter
#'     rejects what cannot reach its endpoint.}
#' }
#'
#' Each supplied closure is checked at construction — its argument count, and a
#' dry call on a one-actor toy problem — so a contract mistake surfaces here
#' rather than midway through a replicate.
#'
#' @param parameters a [set_parameter_provider()] object, or `NULL`.
#' @param evaluate a function of `(stats, theta, risk, meta)`, or `NULL` for
#'   goldfish's own evaluator.
#' @param clock a function of `(rates, t, handle)`, or `NULL` for the clock the
#'   specification's behavior selects.
#' @param mark a function of `(fid, evaluation, handle)`, or `NULL` for the
#'   family's own mark.
#' @param accept a function of `(event, handle)`, or `NULL` to accept every
#'   drawn event.
#'
#' @return An object of class `goldfishSimSteps`, a list with components:
#'   \item{parameters}{Value from the `parameters` argument.}
#'   \item{evaluate}{Value from the `evaluate` argument.}
#'   \item{clock}{Value from the `clock` argument.}
#'   \item{mark}{Value from the `mark` argument.}
#'   \item{accept}{Value from the `accept` argument.}
#'
#' @seealso [set_parameter_provider()], [simulate()].
#' @family simulation
#' @export
#' @examples
#' # An evaluate step giving each actor its own parameter row.
#' steps <- set_simulation_steps(
#'   evaluate = function(stats, theta, risk, meta) {
#'     rows_per_ego <- nrow(stats) / nrow(theta)
#'     ego <- rep(seq_len(nrow(theta)), each = rows_per_ego)
#'     linear <- rowSums(stats * theta[ego, , drop = FALSE])
#'     values <- numeric(length(linear))
#'     values[risk$active_sender] <- exp(linear[risk$active_sender])
#'     values
#'   }
#' )
#' class(steps)
set_simulation_steps <- function(
  parameters = NULL,
  evaluate = NULL,
  clock = NULL,
  mark = NULL,
  accept = NULL
) {
  call <- rlang::current_env()
  if (!is.null(parameters) && !is_parameter_provider(parameters)) {
    cli::cli_abort(
      c(
        "{.arg parameters} must be a {.cls goldfishParamProvider}.",
        "x" = "A {.cls {class(parameters)[1]}} was supplied.",
        "i" = "Build one with {.fn set_parameter_provider}."
      ),
      call = call,
      class = "goldfish_sim_bad_step"
    )
  }
  assert_step_closure(evaluate, "evaluate", 4L, call)
  assert_step_closure(clock, "clock", 3L, call)
  assert_step_closure(mark, "mark", 3L, call)
  assert_step_closure(accept, "accept", 2L, call)

  steps <- structure(
    list(
      parameters = parameters,
      evaluate = evaluate,
      clock = clock,
      mark = mark,
      accept = accept
    ),
    class = "goldfishSimSteps"
  )
  dry_run_steps(steps, call)
  steps
}

is_simulation_steps <- function(x) inherits(x, "goldfishSimSteps")

# The default steps object, for a caller that supplies none.
default_simulation_steps <- function() {
  structure(
    list(
      parameters = NULL,
      evaluate = NULL,
      clock = NULL,
      mark = NULL,
      accept = NULL
    ),
    class = "goldfishSimSteps"
  )
}

# --------------------------------------------------------------------------- #
# Construction-time checks
# --------------------------------------------------------------------------- #

# A step must be a function taking at least `n_args` arguments. A closure with
# `...` passes at any count: it can absorb what it is handed, which is how a
# caller writes a step ignoring arguments it does not read.
assert_step_closure <- function(fn, slot, n_args, call) {
  if (is.null(fn)) {
    return(invisible(NULL))
  }
  if (!is.function(fn)) {
    cli::cli_abort(
      c(
        "{.arg {slot}} must be a function.",
        "x" = "A {.cls {class(fn)[1]}} was supplied."
      ),
      call = call,
      class = "goldfish_sim_bad_step"
    )
  }
  formals_fn <- names(formals(fn))
  if (!"..." %in% formals_fn && length(formals_fn) < n_args) {
    cli::cli_abort(
      c(
        "{.arg {slot}} takes {length(formals_fn)} argument{?s}, not
         {n_args}.",
        "i" = "The {.arg {slot}} step is called as
               {.code {slot}({step_argument_names(slot)})}."
      ),
      call = call,
      class = "goldfish_sim_bad_step"
    )
  }
  invisible(NULL)
}

step_argument_names <- function(slot) {
  switch(
    slot,
    init = "replicate, handle",
    at = "k, t, handle, latent",
    evaluate = "stats, theta, risk, meta",
    clock = "rates, t, handle",
    mark = "fid, evaluation, handle",
    accept = "event, handle",
    slot
  )
}

# A handle-shaped stub for the construction-time dry call. It carries only what
# the accessors read, so a step reaching past them fails here -- which is the
# point: the handle is opaque, and a step that treats it as a walk would break
# the first time the walk's internals moved.
toy_walk_handle <- function() {
  handle <- new.env(parent = emptyenv())
  handle$process_map <- data.frame(
    fid = 1L,
    layer = "toy",
    flavor = NA_character_,
    family = "rate",
    stat_block = "DyNAM:rate",
    has_intercept = TRUE,
    stringsAsFactors = FALSE
  )
  handle$process_map$regime <- "modeled"
  handle$n_actors1 <- 1L
  handle$n_actors2 <- 1L
  handle$current_time <- 0
  handle$opened <- TRUE
  structure(handle, class = "goldfishWalk")
}

# Call every supplied step once on a one-actor toy problem. This catches the
# contract mistakes an arity check cannot see -- a step returning the wrong
# length, a mark forgetting its layer, an evaluate step indexing a vector theta
# as a matrix -- at the point the closure is written rather than deep inside a
# replicate, where the traceback would name the driver instead of the caller.
dry_run_steps <- function(steps, call) {
  handle <- toy_walk_handle()
  risk <- list(
    active_sender = TRUE,
    active_dyad = TRUE,
    encoding = "alter"
  )
  meta <- list(
    family = "rate",
    n_actors1 = 1L,
    n_actors2 = 1L,
    sender = NA_integer_
  )
  if (!is.null(steps$evaluate)) {
    # Vector first, then a one-row matrix. A step written for a per-actor
    # matrix cannot read a vector theta, and refusing it here would reject the
    # variant the plug point exists for -- so the dry call accepts a step that
    # works under either shape and only aborts when both fail.
    value <- dry_call_evaluate(steps$evaluate, risk, meta, call)
    if (!is.numeric(value) || length(value) != 1L) {
      abort_dry_shape("evaluate", "a numeric value per candidate", call)
    }
  }
  if (!is.null(steps$clock)) {
    value <- dry_call(steps$clock, list(c(`1` = 1), 0, handle), "clock", call)
    if (!is.list(value) || is.null(value$wait) || is.null(value$fid)) {
      abort_dry_shape(
        "clock",
        "a list with {.field wait} and {.field fid}",
        call
      )
    }
  }
  if (!is.null(steps$mark)) {
    evaluation <- list(model_type = "DyNAM-M-Rate", value = 1)
    value <- dry_call(steps$mark, list(1L, evaluation, handle), "mark", call)
    if (!is.list(value) || is.null(value$layer)) {
      abort_dry_shape("mark", "an event list naming a {.field layer}", call)
    }
  }
  if (!is.null(steps$accept)) {
    value <- dry_call(
      steps$accept,
      list(list(layer = "toy", sender = 1L, receiver = 1L), handle),
      "accept",
      call
    )
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      abort_dry_shape("accept", "{.val TRUE} or {.val FALSE}", call)
    }
  }
  invisible(NULL)
}

dry_call_evaluate <- function(fn, risk, meta, call) {
  stats <- matrix(1, 1L, 1L)
  # Speculative: a matrix-only step will fail or complain here, and neither is
  # news to the caller, so its noise is swallowed and the matrix try decides.
  vector_failed <- FALSE
  vector_try <- suppressWarnings(tryCatch(
    fn(stats, 1, risk, meta),
    error = function(e) {
      vector_failed <<- TRUE
      NULL
    }
  ))
  if (!vector_failed) {
    return(vector_try)
  }
  dry_call(fn, list(stats, matrix(1, 1L, 1L), risk, meta), "evaluate", call)
}

dry_call <- function(fn, args, slot, call) {
  tryCatch(
    do.call(fn, args),
    error = function(e) {
      cli::cli_abort(
        c(
          "The {.arg {slot}} step failed its dry call.",
          "x" = conditionMessage(e),
          "i" = "It was called as
                 {.code {slot}({step_argument_names(slot)})} on a one-actor
                 toy problem."
        ),
        call = call,
        class = "goldfish_sim_bad_step"
      )
    }
  )
}

abort_dry_shape <- function(slot, expected, call) {
  cli::cli_abort(
    c(
      "The {.arg {slot}} step returned the wrong shape.",
      "i" = paste(
        "It must return",
        expected,
        "— see
                  {.fn set_simulation_steps}."
      )
    ),
    call = call,
    class = "goldfish_sim_bad_step"
  )
}

# --------------------------------------------------------------------------- #
# The accessor surface a step closure sees
#
# A step receives the walk handle as `handle` and reads it through these four
# functions only. They are the whole contract: everything else about the handle
# -- its engines, its live statistics, its schedule -- is goldfish's to change.
# --------------------------------------------------------------------------- #

#' Read a simulation handle inside a step
#'
#' The `handle` a [set_simulation_steps()] closure receives is opaque. These
#' accessors are what a step may read from it; the handle's other contents are
#' internal and may change between releases.
#'
#' @param x the handle a simulation step was called with.
#' @param side which node set to count, `1` (senders) or `2` (receivers). They
#'   differ only for a two-mode process.
#' @param fid a formula id from `process_map(x)`, or `NULL` for every process.
#'
#' @return `n_actors()` an integer; `current_time()` the handle's clock as a
#'   number; `process_map()` the table of processes the walk carries, one row
#'   per formula id; `regime_of()` a character vector naming each process's
#'   regime — `"modeled"` (drawn from its own formula), `"completed"` (an
#'   auto-supplied uniform or pinned default) or `"anchored-replay"` (an
#'   unmodeled flavor replayed from the observed stream).
#'
#' @family simulation
#' @name simulation-handle
#' @examples
#' # Inside a step closure:
#' clock <- function(rates, t, handle) {
#'   stopifnot(current_time(handle) <= t)
#'   list(wait = stats::rexp(1, sum(rates)), fid = 1L, kind = "event")
#' }
NULL

#' @rdname simulation-handle
#' @export
n_actors <- function(x, side = 1L) {
  assert_walk_accessor(x, "n_actors")
  side <- as.integer(side)
  # The node universe is shared across a walk's engines, so the first engine
  # answers for all of them; the stamped fields are the toy handle's route.
  if (length(x$engines %||% list()) > 0L) {
    ctx <- x$engines[[1L]]$ctx
    return(as.integer(if (side == 2L) ctx$n2 else ctx$n1))
  }
  if (side == 2L) {
    return(as.integer(x$n_actors2 %||% x$n_actors1))
  }
  as.integer(x$n_actors1)
}

#' @rdname simulation-handle
#' @export
current_time <- function(x) {
  assert_walk_accessor(x, "current_time")
  x$current_time
}

#' @rdname simulation-handle
#' @export
process_map <- function(x) {
  assert_walk_accessor(x, "process_map")
  x$process_map
}

#' @rdname simulation-handle
#' @export
regime_of <- function(x, fid = NULL) {
  assert_walk_accessor(x, "regime_of")
  map <- x$process_map
  regime <- map$regime %||% rep("modeled", nrow(map))
  if (is.null(fid)) {
    return(stats::setNames(regime, as.character(map$fid)))
  }
  regime[match(fid, map$fid)]
}

assert_walk_accessor <- function(x, fn, call = rlang::caller_env()) {
  if (!inherits(x, "goldfishWalk")) {
    cli::cli_abort(
      c(
        "{.fn {fn}} needs the handle a simulation step was called with.",
        "x" = "A {.cls {class(x)[1]}} was supplied."
      ),
      call = call,
      class = "goldfish_sim_bad_handle"
    )
  }
  invisible(NULL)
}
