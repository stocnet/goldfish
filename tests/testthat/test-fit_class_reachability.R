# Every generic that reaches one fit class reaches all of them.
#
# The fit classes share the parent `goldfishBaseFit`, so a generic written once
# on the parent serves every fit. The hazard that creates is silence in the
# other direction: a generic written for one class and forgotten on another
# fails only when a user happens to try it. That is not hypothetical --
# `summary()`, `tidy()` and `glance()` were written for a single-process fit
# and missing from the container for as long as the classes were flat, and
# nobody decided that.
#
# The rule below is derived from the namespace alone. It carries no table of
# expected verdicts on purpose: a checked-in list of what *should* be
# registered can go stale and then passes by agreeing with itself, which is how
# the omission it replaces survived. Whether a class is deliberately excused is
# recorded where it can be argued -- the `fit-class-hierarchy` capability in
# `openspec/specs/` -- and a deliberate exemption still registers a method (one
# that refuses with a reason), so it satisfies reachability here rather than
# needing an exemption list.

fit_parent_class <- "goldfishBaseFit"

registered_s3_methods <- function() {
  getNamespaceInfo(asNamespace("goldfish"), "S3methods")[, 1:2, drop = FALSE]
}

# Classes stamped beside the parent, enumerated from the namespace rather than
# declared here, so a fit class minted later is found instead of passing by
# omission.
classes_stamped_with_parent <- function(expr) {
  found <- character()
  descend <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    parts <- as.list(e)[-1]
    if (length(parts) > 0 && all(vapply(parts, is.character, logical(1)))) {
      vector <- unlist(parts)
      if (fit_parent_class %in% vector) {
        found <<- c(found, setdiff(vector, fit_parent_class))
      }
    }
    for (i in seq_along(e)) {
      if (!identical(e[[i]], quote(expr = ))) descend(e[[i]])
    }
  }
  tryCatch(descend(expr), error = function(e) NULL)
  found
}

concrete_fit_classes <- function() {
  ns <- asNamespace("goldfish")
  functions <- Filter(is.function, mget(ls(ns, all.names = TRUE), envir = ns))
  sort(unique(unlist(lapply(functions, function(f) {
    classes_stamped_with_parent(body(f))
  }))))
}

# The gaps: a generic registered on any fit surface that some fit class can
# reach neither through its own method nor through the parent's. Factored out
# so the rule can itself be tested against a registry with a known hole -- a
# guard that cannot be shown to fail is a decoration.
unreachable_cells <- function(methods, classes, parent = fit_parent_class) {
  generics <- unique(methods[methods[, 2] %in% c(parent, classes), 1])
  gaps <- character()
  for (generic in generics) {
    inherited <- any(methods[, 1] == generic & methods[, 2] == parent)
    for (class in classes) {
      own <- any(methods[, 1] == generic & methods[, 2] == class)
      if (!own && !inherited) {
        gaps <- c(gaps, paste0(generic, "() on ", class))
      }
    }
  }
  sort(gaps)
}

test_that("the fit classes are enumerated from the namespace", {
  classes <- concrete_fit_classes()

  expect_true(length(classes) > 0)
  expect_false(fit_parent_class %in% classes)
  # Every enumerated class is one the package registers methods on; a name
  # picked up from some unrelated character vector would not be.
  methods <- registered_s3_methods()
  expect_true(all(classes %in% methods[, 2]))
})

test_that("no generic reaches one fit class and misses another", {
  classes <- concrete_fit_classes()
  gaps <- unreachable_cells(registered_s3_methods(), classes)

  # Named, not counted: a failure has to say which method to write, or which
  # class to excuse with a refusal that states its reason.
  expect_identical(gaps, character())
})

test_that("the reachability rule reports the gap it is meant to catch", {
  # The registry as it stood before the parent existed: the shape-level methods
  # on the single-process class, and the container missing three of them.
  flat <- rbind(
    cbind(c("print", "summary", "tidy", "glance", "coef"), "goldfishFit"),
    cbind(c("print", "coef"), "goldfishFlavFit")
  )

  expect_identical(
    unreachable_cells(flat, c("goldfishFit", "goldfishFlavFit")),
    c(
      "glance() on goldfishFlavFit",
      "summary() on goldfishFlavFit",
      "tidy() on goldfishFlavFit"
    )
  )

  # And the parent closes them without a per-class method being written.
  parented <- rbind(
    cbind(c("print", "summary", "tidy", "glance", "coef"), fit_parent_class),
    cbind(c("print", "coef"), "goldfishFlavFit")
  )
  expect_identical(
    unreachable_cells(parented, c("goldfishFit", "goldfishFlavFit")),
    character()
  )
})

test_that("a new fit class with no methods of its own is not a gap", {
  # It inherits every parent-borne generic, which is the point of the parent.
  # What such a class still owes is a decision for the generics that have no
  # parent default -- an obligation the capability spec carries, since it is an
  # argument to be made rather than a fact to be checked.
  parented <- rbind(
    cbind(c("print", "summary"), fit_parent_class),
    cbind("residuals", "goldfishFit")
  )

  expect_identical(
    unreachable_cells(parented, c("goldfishFit", "goldfishNewFit")),
    "residuals() on goldfishNewFit"
  )
})
