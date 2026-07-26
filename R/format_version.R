# Fitted-object layout epoch ----
#
# The epoch a newly fitted result is stamped with. Deliberately an integer
# rather than the package version: it identifies the *shape* of the object
# (which components exist and how they are spelled), so an ordinary release
# that does not touch the layout leaves every stored fit current.
#
# Epoch 2 is the snake_case component set. Epoch 1 objects are not stamped at
# all -- the stamp did not exist -- which is why recognizing one falls back to a
# retired camelCase component.
#
# The preprocessed object has its own, older counter for the same job
# (`PREPROCESSED_GOLDFISH_VERSION`), which already aborts estimation on a stale
# object; it is bumped rather than duplicated here.

goldfish_result_format <- 2L

# Components a current fit cannot carry. `logLikelihood` was retired by the
# component rename; `subModel` was retired earlier, by the 1.7.0 function
# renames -- which is why an object from the last CRAN release already fails
# today. Either one identifies an object built before the current layout, and
# the rename is what makes that identification exact.
retired_result_components <- c("logLikelihood", "subModel")

# Recognize -- never translate. A translating shim would have to track every
# component as the object keeps evolving; this only ever answers "is this the
# layout I know", so it needs no maintenance when components change again.
#
# Returns "current", "outdated", or "newer". An unstamped object is only called
# outdated when a retired component confirms it: the stamp postdates the rename,
# so its absence alone is not evidence, and treating absence as outdated would
# reject the hand-built fixtures that tests legitimately construct.
result_format_status <- function(x) {
  stamp <- x$format_version
  if (length(stamp) == 1L && !is.na(stamp)) {
    stamp <- as.integer(stamp)
    if (identical(stamp, goldfish_result_format)) {
      return("current")
    }
    return(if (stamp > goldfish_result_format) "newer" else "outdated")
  }
  if (any(retired_result_components %in% names(x))) {
    return("outdated")
  }
  "current"
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
