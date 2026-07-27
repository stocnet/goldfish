# Object layout epochs ----
#
# The epoch a newly built object is stamped with. Deliberately an integer rather
# than the package version: it identifies the *shape* of the object (which
# components exist and how they are spelled), so an ordinary release that does
# not touch the layout leaves every stored object current.
#
# Epoch 2 is the snake_case component set. Epoch 1 objects are not stamped at
# all -- the stamp did not exist -- which is why recognizing one falls back to a
# retired camelCase component (for a fit) or to the absence of the stamp itself
# (for a preprocessed object, whose earlier counter lived under `version`).
#
# Two epochs under two component names, not one shared name: a preprocessed
# object can be carried inside a fitted object, so one name would put two
# different facts under one key at two depths of the same object.
#
# Two counters rather than one shared value, because the two objects can change
# shape independently -- a preprocessing-only change must not invalidate every
# stored fit.

# Recorded on a fitted result as `fit_version`.
FIT_VERSION <- 2L

# Recorded on a preprocessed object as `prep_version`.
PREP_VERSION <- 2L

# Components a current fit cannot carry. `logLikelihood` was retired by the
# component rename; `subModel` was retired earlier, by the 1.7.0 function
# renames -- which is why an object from the last CRAN release already fails
# today. Either one identifies an object built before the current layout, and
# the rename is what makes that identification exact.
retired_result_components <- c("logLikelihood", "subModel")

# Establish the object's kind before consulting its stamp. Distinct component
# names keep the two stamps unambiguous, but they do not stop a *misrouted*
# object: handed a current preprocessed object, the fit's detector reads
# `prep$fit_version` as NULL, falls through to the retired-component check,
# finds none, and would report "current" -- accepting the wrong kind in silence.
# The absent-stamp fallback that makes an unstamped old fit detectable is exactly
# what lets a modern preprocessed object through, so kind is checked separately.
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
# Returns "current", "outdated", or "newer". An unstamped object is only called
# outdated when a retired component confirms it: the stamp postdates the rename,
# so its absence alone is not evidence, and treating absence as outdated would
# reject the hand-built fixtures that tests legitimately construct.
result_format_status <- function(x) {
  when_absent <- if (any(retired_result_components %in% names(x))) {
    "outdated"
  } else {
    "current"
  }
  format_status(x$fit_version, FIT_VERSION, when_absent)
}

# The preprocessed object's own status, read from its own slot against its own
# epoch. Same three verdicts, so the estimator can tell "recompute it" from
# "update goldfish" rather than calling both cases outdated.
preprocessed_format_status <- function(x) {
  # Here absence IS conclusive, unlike the fit above: a preprocessed object is
  # only ever produced by goldfish itself, so there is no hand-built case to
  # protect, and every one built before this rename recorded its epoch under
  # `version`. That is what makes resetting the counter to its original value
  # safe -- the missing name identifies the object whatever number it carried.
  format_status(x$prep_version, PREP_VERSION, "outdated")
}

format_status <- function(stamp, current, when_absent) {
  if (length(stamp) == 1L && !is.na(stamp)) {
    stamp <- as.integer(stamp)
    if (identical(stamp, current)) {
      return("current")
    }
    return(if (stamp > current) "newer" else "outdated")
  }
  when_absent
}

# The bullets both severities share, so the diagnosis reads the same whether it
# was informed or thrown.
stale_result_bullets <- function(status) {
  if (identical(status, "newer")) {
    c(
      "x" = "It was fitted by a newer version of {.pkg goldfish} than the one
             loaded, so this version does not know its layout.",
      "i" = "Update {.pkg goldfish}, or re-fit the model with this version."
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
    "This {.cls result.goldfish} object was not fitted by this version of
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
  abort_if_not_class(x, "result.goldfish", what, call = call)
  status <- result_format_status(x)
  if (identical(status, "current")) {
    return(invisible(FALSE))
  }
  cli::cli_abort(
    c(
      "Cannot compute {what} from a {.cls result.goldfish} object that was not
       fitted by this version of {.pkg goldfish}.",
      stale_result_bullets(status)
    ),
    call = call
  )
}
