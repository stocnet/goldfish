# Object layout epochs ----
#
# The epoch a newly built object is stamped with. Deliberately an integer rather
# than the package version: it identifies the *shape* of the object (which
# components exist and how they are spelled), so an ordinary release that does
# not touch the layout leaves every stored object current.
#
# Epoch 2 is the snake_case component set. Epoch 1 objects carry no record at
# all -- neither name existed, and the preprocessed object's earlier counter
# lived under `version` -- so the missing record is what identifies them,
# whatever else they contain.
#
# Two epochs under two component names, not one shared name: a preprocessed
# object can be carried inside a fitted object, so one name would put two
# different facts under one key at two depths of the same object.
#
# Two counters rather than one shared value, because the two objects can change
# shape independently -- a preprocessing-only change must not invalidate every
# stored fit.
#
# **An epoch moves once per RELEASE whose layout differs from the previous
# release's, never once per component added during a development line.** The
# last release is v1.7.0 (2025-06-23) and the epoch was introduced thirteen
# months later, so no released goldfish has ever written one: every object in
# the wild is epoch 1 ("no record"), epoch 2 exists only inside this dev line,
# and 2.0.0 will be the first release to ship an epoch at all. Bumping for a
# dev-line addition would burn a number no user could ever hold an object
# stamped with, and would refuse every dev-line fit for a change that only
# *adds* components.
#
# The cost of that rule, stated so it is chosen rather than discovered: within a
# development line an object can be **current in stamp but stale in content** --
# stamped 2, yet fitted before a component existed. The epoch cannot catch that,
# so the CONSUMER must: a diagnostic needing a component a fit may lack checks
# for it and aborts naming what it needs, exactly as `residual_stored()` names a
# missing stored primitive. That guard is per-consumer and legible; a global
# stamp for a dev-line addition is neither. The same rule governs
# `prep_version`.

# Recorded on a fitted result as `fit_version`.
FIT_VERSION <- 2L

# Recorded on a preprocessed object as `prep_version`.
PREP_VERSION <- 2L

# Establish the object's kind before consulting its stamp. The verdict does not
# depend on this -- a misrouted object carries no stamp under the name being
# read, so it is refused either way -- but the *diagnosis* does. Without it, a
# freshly built preprocessed object is refused as "fitted before goldfish
# 2.0.0, when the components of a fitted model were renamed", which is false
# about both its age and its kind, and sends the reader off to re-fit a model
# when what they actually did was pass the wrong object. Kind first, then age.
abort_if_not_class <- function(x, class, what, call = rlang::caller_env()) {
  if (inherits(x, class)) {
    return(invisible(FALSE))
  }
  cli::cli_abort(
    c(
      "Cannot compute {what} from a {.cls {class(x)[[1]]}} object.",
      "x" = "A {.cls {class}} object is required.",
      "i" = "Pass the fitted model itself, not one of its components."
    ),
    call = call
  )
}

# Recognize -- never translate. A translating shim would have to track every
# component as the object keeps evolving; this only ever answers "is this the
# layout I know", so it needs no maintenance when components change again.
#
# One rule, both objects: an object carrying no record predates the record. It
# needs no corroborating evidence, because every object built before the record
# existed lacks it, and nothing goldfish builds today lacks it. An object
# constructed by hand -- a test fixture, say -- therefore has to record which
# layout it emulates, which is the right obligation: a fixture that declares its
# format fails loudly when the layout moves again, where one relying on a
# permissive default passes silently forever.
result_format_status <- function(x) {
  format_status(x$fit_version, FIT_VERSION)
}

preprocessed_format_status <- function(x) {
  format_status(x$prep_version, PREP_VERSION)
}

format_status <- function(stamp, current) {
  if (length(stamp) == 1L && !is.na(stamp)) {
    stamp <- as.integer(stamp)
    if (identical(stamp, current)) {
      return("current")
    }
    return(if (stamp > current) "newer" else "outdated")
  }
  "outdated"
}

# The bullets every severity shares, so the diagnosis reads the same whether it
# was informed or thrown.
#
# `"retired_class"` is not a format status: it is the verdict for an object
# whose layout is CURRENT but whose class name is not. Those two populations get
# opposite advice, and telling a dev-line user that their components were
# renamed sends them looking for a component change that never happened -- so
# the retired-class branch says only what is true, that the class moved.
stale_result_bullets <- function(status) {
  if (identical(status, "newer")) {
    c(
      "x" = "It was fitted by a newer version of {.pkg goldfish} than the one
             loaded, so this version does not know its layout.",
      "i" = "Update {.pkg goldfish}, or re-fit the model with this version."
    )
  } else if (identical(status, "retired_class")) {
    c(
      "x" = "Its class {.cls result.goldfish} was retired in {.pkg goldfish}
             2.0.0: a fitted model is now {.cls goldfishFit}.",
      "i" = "Its components are current. Only the class name changed, so
             nothing inside the object needs repairing.",
      "i" = "goldfish attaches no fallback class, so no method dispatches on
             the retired name. Re-fit the model. See
             {.code news(package = \"goldfish\")} for the class table."
    )
  } else {
    c(
      "x" = "It was fitted before {.pkg goldfish} 2.0.0, when the components of
             a fitted model were renamed to snake_case.",
      "i" = "Re-fit the model to use it with this version. The old names are not
             translated: an object this old is also missing components the
             current methods need, so its spelling is not the only thing that
             would have to be repaired.",
      "i" = "See {.code news(package = \"goldfish\")} for the renamed
             components."
    )
  }
}

# print() / format(): inform and still show what can be shown. This is the first
# thing a user does with an unfamiliar object, so it is where the diagnosis
# belongs -- and refusing to print would leave them holding an object they
# cannot inspect to find out what it is.
inform_if_stale_result <- function(x) {
  status <- result_format_status(x)
  if (identical(status, "current")) {
    return(invisible(FALSE))
  }
  cli::cli_inform(c(
    "This {.cls goldfishFit} object was not fitted by this version of
     {.pkg goldfish}.",
    stale_result_bullets(status)
  ))
  invisible(TRUE)
}

# The computing surfaces: refuse. Returning a number is the failure this
# replaces -- `logLik()` on a pre-2.0.0 object yields a value whose `df` is
# NULL, and `AIC()` / `BIC()` then report a wrong number in silence, which is
# worse than an error and cannot be fixed by warning about it.
abort_if_stale_result <- function(
  x,
  what,
  call = rlang::caller_env()
) {
  abort_if_not_class(x, "goldfishFit", what, call = call)
  status <- result_format_status(x)
  if (identical(status, "current")) {
    return(invisible(FALSE))
  }
  cli::cli_abort(
    c(
      "Cannot compute {what} from a {.cls goldfishFit} object that was not
       fitted by this version of {.pkg goldfish}.",
      stale_result_bullets(status)
    ),
    call = call
  )
}

# The retired class name is the discriminator ----
#
# Objects fitted before 2.0.0 carry `result.goldfish`, which no longer names any
# class goldfish attaches. Exactly two methods survive on it (design: no other
# generic registers, so `coef()`, `logLik()`, `predict()` and the rest give R's
# own "no applicable method" error, which is already the right message).
#
# The two populations reachable through this name need opposite advice, and the
# recorded format epoch is what tells them apart: an object from a released
# goldfish (<= 1.7.0) records no epoch and its components were renamed as well,
# while an object from the development line records the current epoch and has
# only lost its class name. Diagnosing from the class alone would tell the
# second group to look for a component change that never happened.
retired_result_class_status <- function(x) {
  if (identical(result_format_status(x), "current")) {
    "retired_class"
  } else {
    "outdated"
  }
}

abort_retired_result_class <- function(x, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "This object carries the retired class {.cls result.goldfish}.",
      stale_result_bullets(retired_result_class_status(x))
    ),
    call = call
  )
}

#' @export
#' @noRd
print.result.goldfish <- function(x, ...) {
  abort_retired_result_class(x)
}

#' @export
#' @noRd
summary.result.goldfish <- function(object, ...) {
  abort_retired_result_class(object)
}
