# Package-wide guard on the class-naming scheme: every class goldfish
# attaches is `goldfish<Thing>` in camelCase.
#
# The class list is enumerated from the loaded namespace, never read from a
# rename table. A table records what each existing class became; a class
# minted afterwards is absent from it and would pass by omission, which is
# how the earlier convention drifted. The namespace is used rather than the
# `R/` sources so the guard also runs where the sources are not installed.
#
# The enumeration is best-effort and cannot be proven complete. A
# non-conforming class found outside it is a gap in the exemptions below, not
# an entry to add to a table.

# Generics defined in the namespace (any function calling UseMethod) plus the
# generics of the registered S3 methods. Needed to split a method name into
# generic and class: `ds_network.goldfishSourceStocnet` yields the class only
# if `ds_network` is known to be a generic.
namespace_generics <- function(ns, objects) {
  calls_use_method <- function(e) {
    if (!is.call(e)) {
      return(FALSE)
    }
    if (is.name(e[[1]]) && identical(as.character(e[[1]]), "UseMethod")) {
      return(TRUE)
    }
    for (i in seq_along(e)) {
      if (identical(e[[i]], quote(expr = ))) {
        next
      }
      found <- tryCatch(calls_use_method(e[[i]]), error = function(e) FALSE)
      if (isTRUE(found)) {
        return(TRUE)
      }
    }
    FALSE
  }
  declared <- names(objects)[vapply(
    objects,
    function(f) {
      is.function(f) &&
        isTRUE(tryCatch(calls_use_method(body(f)), error = function(e) FALSE))
    },
    logical(1)
  )]
  unique(c(declared, getNamespaceInfo(ns, "S3methods")[, 1]))
}

# Classes reachable through a method name. The longest matching generic wins,
# so `print.summary.x` is read as a method on `print.summary` where that
# generic exists and on `print` otherwise -- the same rule R's own dispatch
# uses.
classes_from_method_names <- function(object_names, generics) {
  out <- lapply(object_names, function(nm) {
    hits <- generics[startsWith(nm, paste0(generics, "."))]
    if (length(hits) == 0) {
      return(character())
    }
    substring(nm, nchar(hits[which.max(nchar(hits))]) + 2L)
  })
  unique(unlist(out))
}

# Every character literal reachable inside an expression, so a class vector
# built with `c()` is read as a whole.
literal_strings <- function(e) {
  out <- character()
  descend <- function(y) {
    if (is.character(y)) {
      out <<- c(out, y)
    } else if (is.call(y)) {
      for (i in seq_along(y)) {
        if (!identical(y[[i]], quote(expr = ))) descend(y[[i]])
      }
    }
  }
  tryCatch(descend(e), error = function(e) NULL)
  out
}

# Classes named as literals at every stamping form (`class(x) <-`,
# `structure(class = )`, `attr(x, "class") <-`) and at every class test
# (`inherits()`, `is()`).
classes_from_literals <- function(objects) {
  found <- character()
  call_name <- function(fn) {
    if (is.name(fn)) {
      as.character(fn)
    } else if (is.call(fn) && identical(as.character(fn[[1]]), "::")) {
      as.character(fn[[3]])
    } else {
      ""
    }
  }
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    fname <- call_name(e[[1]])
    if (fname %in% c("<-", "=") && length(e) >= 3 && is.call(e[[2]])) {
      target <- call_name(e[[2]][[1]])
      is_attr_class <- identical(target, "attr") &&
        length(e[[2]]) >= 3 &&
        identical(e[[2]][[3]], "class")
      if (identical(target, "class") || is_attr_class) {
        found <<- c(found, literal_strings(e[[3]]))
      }
    }
    if (identical(fname, "structure")) {
      argument_names <- names(e)
      if (!is.null(argument_names) && "class" %in% argument_names) {
        found <<- c(
          found,
          literal_strings(e[[which(argument_names == "class")]])
        )
      }
    }
    if (fname %in% c("inherits", "is") && length(e) >= 3) {
      found <<- c(found, literal_strings(e[[3]]))
    }
    for (i in seq_along(e)) {
      if (!identical(e[[i]], quote(expr = ))) {
        tryCatch(walk(e[[i]]), error = function(e) NULL)
      }
    }
    invisible(NULL)
  }
  for (f in objects) {
    if (is.function(f)) tryCatch(walk(body(f)), error = function(e) NULL)
  }
  unique(found)
}

# The effect dispatch tags (`inertia`, `recip`, ...). Exempt because the
# effect registry retires their class role wholesale; enumerated from the
# model-keyed dispatchers rather than listed, so a new effect is exempt
# without editing this file.
effect_dispatch_tags <- function(object_names, generics) {
  dispatchers <- grep(
    "^(init|update)_(DyNAM|DyNAMi|REM)",
    generics,
    value = TRUE
  )
  classes_from_method_names(object_names, dispatchers)
}

# Classes goldfish neither defines nor owns: base R, tibble, lubridate, DBI,
# and the manynet `stocnet` shape goldfish validates against. Present because
# the package tests objects for them, not because it attaches them.
foreign_classes <- c(
  "array",
  "character",
  "Date",
  "data.frame",
  "DBIConnection",
  "Duration",
  "environment",
  "formula",
  "function",
  "integer",
  "list",
  "logical",
  "logLik",
  "matrix",
  "numeric",
  "object_size",
  "Period",
  "POSIXct",
  "POSIXlt",
  "POSIXt",
  "stocnet",
  "tbl",
  "tbl_df",
  "try-error"
)

# The deprecated path keeps its dotted names: their constructors already warn
# toward `manynet::make_stocnet()`, and the spelling now marks the legacy
# path. `data.goldfish` is the legacy environment only -- `as_goldfish()`
# stamps `goldfishData`.
deprecated_path_classes <- c(
  "nodes.goldfish",
  "network.goldfish",
  "dependent.goldfish",
  "global.goldfish",
  "data.goldfish"
)

goldfish_own_classes <- function() {
  ns <- asNamespace("goldfish")
  object_names <- ls(ns, all.names = TRUE)
  objects <- mget(object_names, envir = ns, ifnotfound = list(NULL))
  generics <- namespace_generics(ns, objects)
  registered <- getNamespaceInfo(ns, "S3methods")[, 2]
  enumerated <- unique(c(
    classes_from_method_names(object_names, generics),
    classes_from_literals(objects),
    registered
  ))
  exempt <- c(
    effect_dispatch_tags(object_names, generics),
    foreign_classes,
    deprecated_path_classes,
    # `default` is S3's fallback, not a class.
    "default"
  )
  own <- setdiff(enumerated, exempt)
  # Condition classes stay `goldfish_<snake_case>`: they are matched on the
  # class vector by `tryCatch()` and `expect_error(class = )`, never
  # dispatched on, and snake_case is the ecosystem convention for them.
  own[!grepl("^goldfish_[a-z0-9_]+$", own)]
}

conforms <- function(x) grepl("^goldfish[A-Z][A-Za-z0-9]*$", x)

# Classes still carrying a retired name while the rename lands cluster by
# cluster. Scaffolding, not a rename table: a class absent from here that
# breaks the rule still fails, and an entry that has been renamed must be
# removed, so the list cannot rot. It is empty when the change closes.
pending_rename <- c(
  "diagnose_changepoints",
  "diagnose_onset",
  "diagnose_outliers",
  "flavored_result.goldfish",
  "margin_table",
  "result.goldfish",
  "summary.result.goldfish",
  "test_gof",
  "test_parameter",
  "test_time"
)

test_that("every class goldfish attaches is goldfish<Thing>", {
  own <- goldfish_own_classes()
  expect_gt(length(own), 20)
  expect_setequal(own[!conforms(own)], pending_rename)
})

test_that("the enumeration reaches classes that only a method name names", {
  # `dyad_spec` has no `inherits()` site anywhere: it is reachable only
  # through `estimate_int.goldfishAxisDyad` and through the literal handed to
  # spec constructor. It escaped a first enumeration, so it is asserted.
  own <- goldfish_own_classes()
  hierarchy <- list(
    c("model_spec", "goldfishKind"),
    c("dynam_rate_spec", "goldfishKindDnRate"),
    c("dynam_rate_ordered_spec", "goldfishKindDnCox"),
    c("dynam_choice_spec", "goldfishKindDnChoice"),
    c("dynam_choice_coord_spec", "goldfishKindDnCoord"),
    c("dynami_rate_spec", "goldfishKindDniRate"),
    c("dynami_rate_ordered_spec", "goldfishKindDniCox"),
    c("dynami_choice_spec", "goldfishKindDniChoice"),
    c("rem_rate_spec", "goldfishKindRemRate"),
    c("rem_rate_ordered_spec", "goldfishKindRemCox"),
    c("sender_spec", "goldfishAxisSender"),
    c("dyad_spec", "goldfishAxisDyad")
  )
  reached <- vapply(
    hierarchy,
    function(spelling) any(spelling %in% own),
    logical(1)
  )
  expect_true(all(reached))
})

test_that("no class goldfish attaches carries a dot", {
  own <- goldfish_own_classes()
  dotted <- grep(".", own, fixed = TRUE, value = TRUE)
  expect_setequal(dotted, intersect(pending_rename, dotted))
})

test_that("the diagnostic constructors keep their exported names", {
  # The retired class names collide with these exported functions, so a
  # textual replace would rename the API. This is the anti-`sed` guard.
  exported <- getNamespaceExports("goldfish")
  expect_true(all(
    c(
      "test_gof",
      "test_time",
      "test_parameter",
      "diagnose_onset",
      "diagnose_outliers",
      "diagnose_changepoints",
      "margin_table",
      "evaluate_model"
    ) %in%
      exported
  ))
})
