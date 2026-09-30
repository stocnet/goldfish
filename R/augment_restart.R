# =========================================================================== #
# The restart-ceiling helper the sim and random augmenters share.
#
# Under cross-dyad support coupling a drawn interval can be unusable: the
# greedy sim draw strands with flips unplaced, and a support-blind random
# ordering fires a flip whose support is not yet applicable. Both reject the
# draw and redraw the interval, so the delivered draw is conditioned on
# success, and both normalize its proposal density by the probability of
# success, estimated as the acceptance rate of the redraws (successes over
# attempts). This file is the single home for the loop, the ceiling it stops
# at, the estimator, and the abort.
#
# The bias warning a plug-in normalizer calls for is not here: it is sim's
# alone, because random's normalizer is a theta-free constant that cancels in
# the normalized weights.
# =========================================================================== #

# How many attempts an interval gets before a success is judged vanishingly
# likely. Past it, looping longer would only hide a specification whose
# coupling leaves almost no legal draw.
restart_ceiling <- function() 1000L

# Draw with `propose()` until `accept(value)` holds, at most `ceiling` times.
# `constraint` labels the coupled support constraints the abort names.
# Returns `list(value, attempts)`: the accepted draw and how many draws it took.
redraw_until_accepted <- function(
  propose,
  accept,
  constraint,
  ceiling = restart_ceiling(),
  call = rlang::caller_env()
) {
  for (attempt in seq_len(ceiling)) {
    value <- propose()
    if (isTRUE(accept(value))) {
      return(list(value = value, attempts = attempt))
    }
  }
  cli::cli_abort(
    c(
      "No support-legal draw of the interval in {ceiling} attempt{?s}.",
      "x" = "The coupled support {cli::qty(length(constraint))}
             constraint{?s} {.val {constraint}} leave{?s/} a legal draw
             vanishingly likely.",
      "i" = "A support constraint that reads another latent event of the same
             interval can disable a flip before it is placed; relax the
             constraint or observe the panel more often."
    ),
    class = "goldfish_dynes_restart_ceiling",
    call = call
  )
}

# The acceptance rate of a redraw loop, the plug-in estimate of the success
# probability that normalizes a success-conditioned proposal density. It is
# recorded with the parameters it was estimated at (`theta_ref`, NULL for a
# theta-free proposal), since a sim draw's success probability depends on them.
acceptance_rate <- function(successes, attempts, theta_ref = NULL) {
  list(
    rate = successes / attempts,
    successes = as.integer(successes),
    attempts = as.integer(attempts),
    theta_ref = theta_ref
  )
}
