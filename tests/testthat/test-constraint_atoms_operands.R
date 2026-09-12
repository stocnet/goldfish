# Pins and detectors for moving the support-constraint atoms off their private
# walk and onto the main walk as `role = "constraint"` plan effects.
#
# The estimated columns of an unconstrained model are guarded by the frozen
# coefficient baselines; these pins guard the two things the baselines cannot
# see -- that plan construction is untouched when no constraint is present, and
# that a constrained model's folded presence axes and support-mask stream do not
# move as the atoms migrate.

constraint_atoms_refs <- function() {
  readRDS(test_path("fixtures", "constraint_atoms_refs.rds"))
}

# ---- 1.1 unconstrained plan construction is untouched ----------------------

test_that("an unconstrained model's plan effects carry no constraint role", {
  spec <- parity_toy_spec()
  for (family in c("rate", "choice")) {
    effects <- parity_plan(spec, family)$effects
    expect_false(
      "constraint" %in% effects$role,
      info = family
    )
  }
})

test_that("an unconstrained model's plan effects match the frozen capture", {
  refs <- constraint_atoms_refs()
  spec <- parity_toy_spec()
  expect_equal(
    parity_plan(spec, "rate")$effects,
    refs$unconstrained_plan_effects$rate
  )
  expect_equal(
    parity_plan(spec, "choice")$effects,
    refs$unconstrained_plan_effects$choice
  )
})

# ---- 1.2 the constrained references are the byte-identity target -----------

test_that("constrained mask and presence axes match the frozen capture", {
  refs <- constraint_atoms_refs()
  for (family in constraint_atoms_families()) {
    live <- constraint_atoms_capture(constraint_atoms_prep(family))
    expect_equal(live, refs[[family]], info = family)
  }
})

# ---- 2.1 the atoms join the estimated plan's effect registry ---------------

test_that("a constrained model's plan effects carry the atoms as operands", {
  fx <- constraint_atoms_social_data()
  spec <- suppressWarnings(make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    model = "DyNAM",
    layer = "dep",
    support_constraint = ~ tie(call_network),
    data = fx$data
  ))
  for (family in c("rate", "choice")) {
    plan <- parity_plan(spec, family)
    atoms <- plan$effects[plan$effects$role == "constraint", , drop = FALSE]
    # The atoms are the sub-plan's atoms, tagged non-estimated, held at the dyad
    # kernel whatever the estimated model's is, and placed above the estimated
    # columns so no estimated indexing reaches them.
    expect_equal(
      atoms$effect_name,
      plan$support_constraint$effects$effect_name,
      info = family
    )
    expect_false(any(atoms$estimate), info = family)
    expect_equal(unique(atoms$stat_kind), "dyad", info = family)
    estimated <- plan$effects[plan$effects$role != "constraint", , drop = FALSE]
    expect_true(all(estimated$gid < min(atoms$gid)), info = family)
  }
})

# ---- 3.0 the covered atom pool is maintained once, on the shared walk ------

test_that("a covered constrained model builds no private atom walk", {
  # The payoff of moving the atoms onto the merged walk: a constraint whose
  # objects the shared schedule visits is maintained inline by the recorder, so
  # the private `build_atom_maintainer()` walk is not built at all and the atom
  # store is seeded exactly once for the layer. `constraint_atoms_capture()`
  # already pins that the mask this produces is byte-identical to the private
  # walk's frozen output (task 1.2), so this is the walk COUNT that removal buys.
  private <- 0L
  stores <- 0L
  orig_store <- build_constraint_atom_store
  local_mocked_bindings(
    build_atom_maintainer = function(...) {
      private <<- private + 1L
      cli::cli_abort("a covered constraint must not build a private atom walk")
    },
    build_constraint_atom_store = function(...) {
      stores <<- stores + 1L
      orig_store(...)
    }
  )
  invisible(constraint_atoms_prep("rate"))
  expect_identical(private, 0L)
  expect_identical(stores, 1L)
})

# ---- 1.3 the two-layer DAG check survives the migration --------------------

test_that("an availability-derived constraint atom is rejected at parse time", {
  # The mask may not read the risk set it defines. No availability-derived
  # effect ships today, so the guard's registry is injected; the parse path
  # must keep routing through it once atoms are ordinary plan effects.
  is_avail <- function(name) name %in% "n_available"
  expect_error(
    reject_availability_atoms(
      atom_labels = c("tie(net)", "n_available(x)"),
      atom_names = c("tie", "n_available"),
      is_availability = is_avail
    ),
    "may not depend on the risk set"
  )
})

test_that("parse_and_validate_constraint routes through the DAG guard", {
  # Pins WHERE the check lives: in the parser, before the atoms are compiled
  # into the plan. A sentinel raised from the guard must surface, proving the
  # guard is on the parse path and not bypassed.
  called <- FALSE
  local_mocked_bindings(
    reject_availability_atoms = function(...) {
      called <<- TRUE
      rlang::abort("guard reached", class = "constraint_atoms_guard")
    }
  )
  expect_error(
    parse_and_validate_constraint(~ tie(net), has_dyad_part = TRUE),
    class = "constraint_atoms_guard"
  )
  expect_true(called)
})
