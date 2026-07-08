# Tests for the support_constraint boolean-tree grammar parser
# (parse_support_constraint). Effect-name recognition is injected so the grammar
# walk is tested in isolation from the per-model effect registry.

# A permissive predicate: treat these names as effect atoms, everything else
# (log, I, if, plain symbols) as out-of-grammar.
known <- function(names) function(name) name %in% names

# Evaluate a parsed constraint expression against supplied atom values, mapping
# each atom label to a value so the resulting mask can be checked numerically.
eval_mask <- function(parsed, values) {
  env <- new.env(parent = baseenv())
  for (k in seq_along(parsed$atoms)) {
    assign(paste0(".a", k), values[[parsed$atom_labels[k]]], envir = env)
  }
  eval(parsed$expr, envir = env)
}

test_that("bare effect is shorthand for a nonzero test", {
  parsed <- parse_support_constraint(
    ~ tie(net),
    is_effect = known("tie")
  )
  expect_length(parsed$atoms, 1)
  expect_identical(parsed$atom_labels, "tie(net)")
  expect_identical(parsed$expr, quote(.a1 != 0))
})

test_that("bare effect and explicit != 0 evaluate identically", {
  bare <- parse_support_constraint(~ tie(net), is_effect = known("tie"))
  expl <- parse_support_constraint(~ tie(net) != 0, is_effect = known("tie"))
  vals <- list(`tie(net)` = c(0, 2, 0, 5))
  expect_identical(eval_mask(bare, vals), eval_mask(expl, vals))
})

test_that("multi-term boolean constraint combines atoms", {
  parsed <- parse_support_constraint(
    ~ tie(net) & !same(dept),
    is_effect = known(c("tie", "same"))
  )
  expect_length(parsed$atoms, 2)
  # tie != 0 AND NOT(same != 0)  ==  tie present AND same absent
  vals <- list(
    `tie(net)` = c(1, 1, 0, 0),
    `same(dept)` = c(0, 1, 0, 1)
  )
  expect_identical(eval_mask(parsed, vals), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("effect-vs-effect comparison is per-cell", {
  parsed <- parse_support_constraint(
    ~ indeg(net) > outdeg(net),
    is_effect = known(c("indeg", "outdeg"))
  )
  expect_length(parsed$atoms, 2)
  vals <- list(
    `indeg(net)` = c(3, 1, 2),
    `outdeg(net)` = c(1, 4, 2)
  )
  expect_identical(eval_mask(parsed, vals), c(TRUE, FALSE, FALSE))
})

test_that("star inside a constraint is elementwise arithmetic, not interaction", {
  parsed <- parse_support_constraint(
    ~ ego(a) * alter(b) > 1,
    is_effect = known(c("ego", "alter"))
  )
  # Two operand atoms and one estimated interaction column are NOT created;
  # the parser yields exactly the two atoms and an arithmetic-product expression.
  expect_length(parsed$atoms, 2)
  expect_identical(parsed$expr, quote(.a1 * .a2 > 1))
  vals <- list(`ego(a)` = c(1, 2, 0), `alter(b)` = c(2, 2, 5))
  expect_identical(eval_mask(parsed, vals), c(TRUE, TRUE, FALSE))
})

test_that("duplicate atoms share a single placeholder", {
  parsed <- parse_support_constraint(
    ~ tie(net) & tie(net) != 0,
    is_effect = known("tie")
  )
  expect_length(parsed$atoms, 1)
})

test_that("parentheses follow R's own precedence", {
  parsed <- parse_support_constraint(
    ~ (indeg(net) + outdeg(net)) / 2 >= 1,
    is_effect = known(c("indeg", "outdeg"))
  )
  vals <- list(`indeg(net)` = c(1, 0), `outdeg(net)` = c(1, 1))
  expect_identical(eval_mask(parsed, vals), c(TRUE, FALSE))
})

test_that("out-of-grammar function call is rejected", {
  expect_error(
    parse_support_constraint(
      ~ log(indeg(net)) > 1,
      is_effect = known("indeg")
    ),
    "Unsupported construct"
  )
})

test_that("I() and if are rejected as non-effect calls", {
  expect_error(
    parse_support_constraint(~ I(tie(net)), is_effect = known("tie")),
    "Unsupported construct"
  )
  expect_error(
    parse_support_constraint(
      ~ if (tie(net)) 1 else 0,
      is_effect = known("tie")
    ),
    "Unsupported construct"
  )
})

test_that("a non-effect symbol is rejected", {
  expect_error(
    parse_support_constraint(~ tie(net) & foo, is_effect = known("tie")),
    "Unsupported construct"
  )
})

test_that("a two-sided formula is rejected", {
  expect_error(
    parse_support_constraint(dep ~ tie(net), is_effect = known("tie")),
    "one-sided"
  )
})

test_that("parser reports atom head names", {
  parsed <- parse_support_constraint(
    ~ indeg(net) > outdeg(net),
    is_effect = known(c("indeg", "outdeg"))
  )
  expect_identical(parsed$atom_names, c("indeg", "outdeg"))
})

# ---- anti-cycle rule (availability-derived atoms) ----

test_that("availability-derived atom is rejected", {
  # Inject an availability registry so the guard's error path is exercised even
  # though no availability-derived effect ships today.
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

test_that("ordinary atoms pass the anti-cycle guard", {
  expect_invisible(
    reject_availability_atoms(
      atom_labels = c("tie(net)", "same(dept)"),
      atom_names = c("tie", "same")
    )
  )
})

# ---- D12: dyadic atoms accepted (row-reduction) in sender-indexed-only specs ----

test_that("dyadic / alter atoms inform about the row-reduction in a rate-only spec", {
  # kinds: point = 0, alter = 1 (fold via the row-reduction); ego = 2,
  # global = 3 fold directly and emit no message.
  expect_message(
    inform_dyadic_sender_reduction(
      atom_labels = c("tie(net)", "ego(x)"),
      atom_kinds = c(0L, 2L)
    ),
    "row-reduction"
  )
  expect_message(
    inform_dyadic_sender_reduction(
      atom_labels = "alter(x)",
      atom_kinds = 1L
    ),
    "at least one allowed, present receiver"
  )
})

test_that("sender-axis atoms fold silently (no message)", {
  expect_no_message(
    inform_dyadic_sender_reduction(
      atom_labels = c("ego(x)", "global(g)"),
      atom_kinds = c(2L, 3L)
    )
  )
})

cli::test_that_cli(
  "the row-reduction message reads correctly",
  configs = "plain",
  {
    expect_snapshot(
      inform_dyadic_sender_reduction(
        atom_labels = c("tie(net)", "alter(x)"),
        atom_kinds = c(0L, 1L)
      )
    )
  }
)

# ---- augment_constraints: role tagging + mask storage kind ----

# Minimal constraint sub-plan as build_update_plan would return for the atoms.
fake_constraint_plan <- function(broadcast_kinds) {
  list(
    effects = data.frame(
      gid = seq_along(broadcast_kinds),
      effect_name = paste0("atom", seq_along(broadcast_kinds)),
      stat_kind = "dyad",
      broadcast_kind = as.integer(broadcast_kinds),
      role = "main",
      estimate = TRUE,
      stringsAsFactors = FALSE
    ),
    effect_objects = NULL,
    routing = list(),
    objects = NULL
  )
}

test_that("constraint atoms are tagged role = constraint, not estimated", {
  out <- augment_constraints(
    fake_constraint_plan(c(0L)),
    mask_expr = quote(.a1 != 0),
    atom_labels = "tie(net)"
  )
  expect_true(all(out$effects$role == "constraint"))
  expect_true(all(!out$effects$estimate))
  expect_identical(out$expr, quote(.a1 != 0))
  expect_identical(out$atom_labels, "tie(net)")
})

test_that("mask storage kind is the axis-union of atom broadcast kinds", {
  # point atom -> dense n1xn2 (kind 0)
  expect_identical(
    augment_constraints(fake_constraint_plan(0L), NULL, NULL)$mask_kind,
    0L
  )
  # ego-only -> length-n1 vector (kind 2)
  expect_identical(
    augment_constraints(fake_constraint_plan(2L), NULL, NULL)$mask_kind,
    2L
  )
  # global-only -> scalar (kind 3)
  expect_identical(
    augment_constraints(fake_constraint_plan(3L), NULL, NULL)$mask_kind,
    3L
  )
  # ego x alter -> point / dense (kind 0)
  expect_identical(
    augment_constraints(fake_constraint_plan(c(2L, 1L)), NULL, NULL)$mask_kind,
    0L
  )
  # ego + global -> ego vector (global is the identity)
  expect_identical(
    augment_constraints(fake_constraint_plan(c(2L, 3L)), NULL, NULL)$mask_kind,
    2L
  )
})
