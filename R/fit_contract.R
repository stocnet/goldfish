# The fit-class contract ----
#
# Every fitted-model class carries `goldfishBaseFit` as its parent, so a
# method written once on the parent reaches every fit. That is convenient and
# it is dangerous in the same breath: a fit whose likelihood is a Monte-Carlo
# estimate would inherit an exact-likelihood `logLik()` and feed `AIC()` a
# number that looks like an information criterion and is not.
#
# The contract table is what makes the convenience safe. It records, for every
# generic dispatching on a fit class and every concrete fit class, exactly one
# verdict:
#
#   inherit  -- the parent's method is correct here, and no method of this
#               class's own is registered;
#   override -- this class needs its own method, for the reason recorded;
#   refuse   -- the generic is not meaningful here, and the call aborts with
#               that reason rather than returning a plausible value.
#
# `tests/testthat/test-fit_contract.R` reads this same file, enumerates the
# generics and classes from the package, and fails on a cell that has no
# verdict or whose verdict the code does not realize. A new fit class or a new
# generic therefore cannot be added without deciding every combination -- which
# is the point, since the omission this replaces (flavored fits silently
# lacking `summary`, `tidy` and `glance`) was nobody's decision.

FIT_CONTRACT_VERDICTS <- c("inherit", "override", "refuse")

# The parsed table, read once per session. `inst/` rather than an R literal so
# the artifact a human edits is the artifact the test reads.
fit_contract_cache <- new.env(parent = emptyenv())

fit_contract <- function() {
  if (is.null(fit_contract_cache$table)) {
    fit_contract_cache$table <- utils::read.csv(
      system.file(
        "fit-class-contract.csv",
        package = "goldfish",
        mustWork = TRUE
      ),
      colClasses = "character",
      stringsAsFactors = FALSE
    )
  }
  fit_contract_cache$table
}

# The single row for one generic-by-class cell, or NULL when the table has
# none. A missing row is a contract failure rather than a user error, so the
# callers below say so in those terms.
fit_contract_cell <- function(generic, class) {
  table <- fit_contract()
  hit <- table$generic == generic & table$class == class
  if (!any(hit)) {
    return(NULL)
  }
  table[which(hit)[1L], ]
}

# Abort for a `refuse` cell. Called from the method the table says refuses, so
# the generic and the class are named at the call site rather than sniffed off
# the object: a refusal has to work on an object too degenerate to inspect.
#
# The reason and the alternative are interpolated as data, not as cli markup,
# so the table can be edited by someone who does not write cli.
refuse_fit_generic <- function(generic, class, call = rlang::caller_env()) {
  cell <- fit_contract_cell(generic, class)
  if (is.null(cell) || cell$verdict != "refuse") {
    cli::cli_abort(
      c(
        "The fit-class contract has no {.val refuse} verdict for
         {.fn {generic}} on {.cls {class}}.",
        "i" = "This is a bug in goldfish: the method refuses, the table
               does not say so."
      ),
      call = call
    )
  }
  message <- c(
    "{.fn {generic}} is not defined for a {.cls {class}} object.",
    "x" = "{cell$reason}"
  )
  if (nzchar(cell$alternative)) {
    message <- c(message, "i" = "{cell$alternative}")
  }
  cli::cli_abort(
    message,
    class = "goldfish_generic_refused",
    call = call
  )
}
