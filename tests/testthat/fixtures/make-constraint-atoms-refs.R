# Regenerate tests/testthat/fixtures/constraint_atoms_refs.rds.
#
# Run from the package root at the PRE-migration code:
#   Rscript tests/testthat/fixtures/make-constraint-atoms-refs.R
#
# The file freezes (a) an unconstrained model's `plan$effects` for a rich rate
# and choice specification, so the assertion that atoms enter the plan ONLY when
# a constraint is present has a byte target, and (b) the folded presence axes
# and support-mask stream for a constrained rate, choice, REM and coordination
# model. The migration of the constraint atoms onto the main walk must leave all
# of it identical.
suppressMessages(devtools::load_all(quiet = TRUE))
source("tests/testthat/helper-parity-fixtures.R")
source("tests/testthat/helper-constraint-atoms.R")

suppressWarnings(suppressMessages({
  spec <- parity_toy_spec()
  unconstrained_plan_effects <- list(
    rate = parity_plan(spec, "rate")$effects,
    choice = parity_plan(spec, "choice")$effects
  )
  constrained <- lapply(
    constraint_atoms_families(),
    function(fam) constraint_atoms_capture(constraint_atoms_prep(fam))
  )
  names(constrained) <- constraint_atoms_families()
}))

refs <- c(
  list(unconstrained_plan_effects = unconstrained_plan_effects),
  constrained
)
saveRDS(refs, "tests/testthat/fixtures/constraint_atoms_refs.rds", version = 2)
cat(
  "wrote constraint_atoms_refs.rds with families:",
  paste(names(constrained), collapse = ", "),
  "\n"
)
