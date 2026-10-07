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
#
# The two `uncovered_*` families (a constraint on an object no formula term
# reads) were added on 2026-09-29 while the private atom walk still
# maintained them. Regenerating re-captures every family from the current
# code, so check each entry already in the file is `identical()` to its
# regenerated value before committing the new file.
#
# Regenerated on 2026-09-30 when a choice's point availability became the
# true grid of every sender (every flipped cell stored) instead of each
# event sender's row: `choice`, `uncovered_choice`, `joint_rate_choice` fid
# 2 and `flavored` fids 2 and 4 moved in `active_dyad_*` only, each
# dependent event's sender row checked equal first; every other entry is
# `identical()` to the previous file.
#
# Regenerated on 2026-10-01 when the preprocessed object stopped storing the
# per-change composition lists and the fold flags beside the update buffers
# (`active_sender_changes`, `active_dyad_changes`, `active_dyad_folded` left
# the capture). Every remaining field of every entry was checked
# `identical()` to the previous file first.
suppressMessages(devtools::load_all(quiet = TRUE))
source("tests/testthat/helper-parity-fixtures.R")
source("tests/testthat/helper-constraint-atoms.R")
source("tests/testthat/helper-flavored-fixtures.R")

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

# The merged walk's registry and schedule per family, captured before a
# constraint-only object joins the shared walk. That "before" cannot be
# produced by the code after the threading, so a capture already in the file
# is kept, never re-taken.
refs_path <- "tests/testthat/fixtures/constraint_atoms_refs.rds"
substrate <- if (file.exists(refs_path)) readRDS(refs_path)$substrate
if (is.null(substrate)) {
  substrate <- suppressWarnings(suppressMessages(lapply(
    constraint_atoms_substrate_families(),
    constraint_atoms_substrate
  )))
  names(substrate) <- constraint_atoms_substrate_families()
}

# Several outputs of one walk: a rate and a choice sharing an atom store, and
# a flavored layer's derived masks, captured per fid (added 2026-09-29).
joint <- suppressWarnings(suppressMessages(lapply(
  constraint_atoms_joint_families(),
  constraint_atoms_joint_capture
)))
names(joint) <- constraint_atoms_joint_families()

refs <- c(
  list(unconstrained_plan_effects = unconstrained_plan_effects),
  constrained,
  list(substrate = substrate),
  joint
)
saveRDS(refs, "tests/testthat/fixtures/constraint_atoms_refs.rds", version = 2)
cat(
  "wrote constraint_atoms_refs.rds with families:",
  paste(names(constrained), collapse = ", "),
  "\n"
)
