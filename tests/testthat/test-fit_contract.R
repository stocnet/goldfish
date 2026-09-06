local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# The parent, and the classes stamped beside it.
#
# Enumerated from the namespace rather than read off the table: a table that
# also declared which classes exist could not catch a class minted without a
# column, which is the omission this whole contract exists to prevent.
fit_parent_class <- "goldfishBaseFit"

classes_stamped_with_parent <- function(expr) {
  found <- character()
  descend <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    parts <- as.list(e)[-1]
    if (all(vapply(parts, is.character, logical(1))) && length(parts) > 0) {
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
  objects <- mget(ls(ns, all.names = TRUE), envir = ns)
  functions <- Filter(is.function, objects)
  sort(unique(unlist(lapply(functions, function(f) {
    classes_stamped_with_parent(body(f))
  }))))
}

# Generics that reach a fit: every generic the package registers a method for
# on the parent or on a concrete fit class.
fit_generics <- function(classes) {
  methods <- getNamespaceInfo(asNamespace("goldfish"), "S3methods")
  sort(unique(methods[methods[, 2] %in% c(fit_parent_class, classes), 1]))
}

registers_method <- function(generic, class) {
  methods <- getNamespaceInfo(asNamespace("goldfish"), "S3methods")
  any(methods[, 1] == generic & methods[, 2] == class)
}

# The cells a contract owes, and the ones it has no business carrying. Split
# out so the failure modes can themselves be tested (a completeness test that
# cannot fail is a decoration).
contract_gaps <- function(contract, generics, classes) {
  owed <- expand.grid(
    generic = generics,
    class = classes,
    stringsAsFactors = FALSE
  )
  key <- function(d) paste(d$generic, d$class, sep = "\r")
  list(
    missing = owed[!key(owed) %in% key(contract), ],
    extra = contract[!key(contract) %in% key(owed), c("generic", "class")]
  )
}

test_that("the contract table parses into the shape the readers assume", {
  contract <- fit_contract()

  expect_named(
    contract,
    c("generic", "class", "verdict", "reason", "alternative")
  )
  expect_true(all(contract$verdict %in% FIT_CONTRACT_VERDICTS))
  # One verdict per cell: a duplicated row is how a table starts disagreeing
  # with itself, and `fit_contract_cell()` would silently take the first.
  expect_false(any(duplicated(contract[c("generic", "class")])))
})

test_that("every override and refuse records why", {
  contract <- fit_contract()
  decided <- contract[contract$verdict != "inherit", ]

  expect_true(all(nzchar(decided$reason)))
  # An `inherit` cell has nothing to explain -- the parent's method is simply
  # correct -- so a reason there is a leftover from an edited verdict.
  expect_true(all(!nzchar(contract$reason[contract$verdict == "inherit"])))
})

test_that("a refuse cell names the class, the reason and the alternative", {
  local_cli_context()

  expect_error(
    refuse_fit_generic("coef_layout", "goldfishFit"),
    class = "goldfish_generic_refused"
  )
  expect_snapshot(
    refuse_fit_generic("coef_layout", "goldfishFit"),
    error = TRUE
  )
})

test_that("refusing a cell the table does not refuse is reported as a bug", {
  local_cli_context()

  # `coef` on a single-process fit is an `inherit` cell, so a method calling
  # the refusal helper for it would be code and table disagreeing.
  expect_snapshot(refuse_fit_generic("coef", "goldfishFit"), error = TRUE)
  expect_snapshot(refuse_fit_generic("coef", "goldfishNoSuchFit"), error = TRUE)
})

test_that("every generic-by-class combination carries a verdict", {
  classes <- concrete_fit_classes()
  generics <- fit_generics(classes)
  gaps <- contract_gaps(fit_contract(), generics, classes)

  # Named, not merely counted: a failure has to say which cell to decide.
  expect_identical(
    paste(gaps$missing$generic, gaps$missing$class, sep = " on "),
    character()
  )
  expect_identical(
    paste(gaps$extra$generic, gaps$extra$class, sep = " on "),
    character()
  )
})

test_that("the completeness check fails on an undecided cell", {
  contract <- data.frame(
    generic = "coef",
    class = "goldfishFit",
    stringsAsFactors = FALSE
  )
  gaps <- contract_gaps(
    contract,
    generics = c("coef", "logLik"),
    classes = c("goldfishFit", "goldfishNewFit")
  )

  # A new generic leaves its row undecided, a new class its column, and a row
  # for a combination the package does not have is flagged the other way.
  expect_setequal(
    paste(gaps$missing$generic, gaps$missing$class),
    c("logLik goldfishFit", "coef goldfishNewFit", "logLik goldfishNewFit")
  )
  expect_identical(nrow(gaps$extra), 0L)

  gaps_extra <- contract_gaps(
    data.frame(
      generic = c("coef", "predict"),
      class = "goldfishFit",
      stringsAsFactors = FALSE
    ),
    generics = "coef",
    classes = "goldfishFit"
  )
  expect_identical(gaps_extra$extra$generic, "predict")
})

test_that("the code realizes the verdict the table records", {
  contract <- fit_contract()
  classes <- concrete_fit_classes()
  contract <- contract[contract$class %in% classes, ]

  registered <- mapply(registers_method, contract$generic, contract$class)

  # `inherit` means the class has no method of its own AND the parent does --
  # otherwise the cell is a claim about dispatch that dispatch does not honor.
  inherited <- contract$verdict == "inherit"
  expect_identical(
    paste(contract$generic, contract$class)[inherited & registered],
    character()
  )
  parent_has <- vapply(
    contract$generic[inherited],
    registers_method,
    logical(1),
    class = fit_parent_class
  )
  expect_true(all(parent_has))

  decided <- contract$verdict %in% c("override", "refuse")
  expect_identical(
    paste(contract$generic, contract$class)[decided & !registered],
    character()
  )
})

test_that("every refuse cell aborts rather than returning a value", {
  contract <- fit_contract()
  refused <- contract[contract$verdict == "refuse", ]
  skip_if(nrow(refused) == 0)

  for (i in seq_len(nrow(refused))) {
    # A degenerate object on purpose: a refusal must not need a well-formed
    # fit to explain itself, since the whole point is that it computes nothing.
    stub <- structure(
      list(),
      class = c(refused$class[i], fit_parent_class)
    )
    expect_error(
      do.call(refused$generic[i], list(stub)),
      class = "goldfish_generic_refused",
      info = paste(refused$generic[i], "on", refused$class[i])
    )
  }
})
