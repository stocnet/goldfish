####################### #
#
# Goldfish package
# Parsing the support_constraint boolean-tree grammar
#
####################### #

# The restricted operator sets of the constraint grammar. `*` `+` `-` `/` here
# are ordinary elementwise arithmetic on the atoms' values, NOT the effects
# formula's interaction expansion — a constraint never creates estimated columns.
.constraint_bool_ops <- c("&", "|")
.constraint_cmp_ops <- c(">", "<", ">=", "<=", "==", "!=")
.constraint_arith_ops <- c("+", "-", "*", "/")

#' Is a language node a goldfish effect atom?
#'
#' A leaf effect call (`tie(net)`, `indeg(net, type = "ego")`, ...): a call whose
#' head is a symbol that names a known effect (validated by `is_effect`), as
#' opposed to a grammar operator or an arbitrary function such as `log`/`I`.
#' @noRd
is_effect_call <- function(node, is_effect) {
  is.call(node) &&
    is.name(node[[1]]) &&
    is_effect(as.character(node[[1]]))
}

#' Default effect-name predicate for the constraint parser
#'
#' Distinguishes an effect-shaped atom from a disallowed function (`log`, `I`,
#' `sqrt`, ...). Recognition is driven by the effect registry itself: the
#' `init_<model>_<sub_model>.<effect>` S3 methods `create_effects_functions()`
#' dispatches on, whose class component (everything after the dot) is the effect
#' name. Precise per-(model, sub_model) validity is enforced downstream when the
#' atom is seeded through `create_effects_functions()` (its "Unknown effect"
#' path); this only needs to separate effect names from arbitrary functions.
#'
#' `envir` is accepted for symmetry with a future custom-effect extension but is
#' not consulted here — resolving bare names against it would misclassify base
#' functions (`log`, `sqrt`) reachable through the environment chain as effects.
#' @noRd
default_is_effect <- function(name, envir) {
  ns <- asNamespace("goldfish")
  init_methods <- grep(
    "^init_[A-Za-z]+_[A-Za-z_]+\\.",
    ls(ns, all.names = TRUE),
    value = TRUE
  )
  effect_names <- sub("^init_[A-Za-z]+_[A-Za-z_]+\\.", "", init_methods)
  name %in% setdiff(effect_names, "default")
}

#' Parse a support_constraint formula into atoms and an evaluable mask expression
#'
#' Walks the restricted boolean-tree grammar over the one-sided
#' `support_constraint` formula, producing (a) the ordered list of distinct
#' effect atoms and (b) an evaluable expression over placeholder symbols
#' `.a1, .a2, ...` bound to those atoms' current statistic values. A bare effect
#' in boolean position is shorthand for `effect != 0`, so there is no separate
#' single-atom path. Operator precedence comes from R's own parse of the formula.
#'
#' Constructs outside the grammar (arbitrary function calls, `I()`, `if`,
#' non-effect symbols) are rejected with a single consistent `cli` error.
#'
#' @param constraint a one-sided `formula`.
#' @param is_effect predicate `function(name) -> logical` recognising effect
#'   atoms; defaults to `default_is_effect()` resolved against `envir`.
#' @param envir environment in which user-supplied effect functions live.
#' @return a list with `atoms` (list of distinct effect-call language objects),
#'   `atom_labels` (their deparsed labels, aligned with `atoms` and with the
#'   `.a{k}` placeholders), and `expr` (the evaluable mask expression).
#' @noRd
parse_support_constraint <- function(
  constraint,
  is_effect = NULL,
  envir = parent.frame()
) {
  if (!inherits(constraint, "formula")) {
    cli::cli_abort("{.arg support_constraint} must be a one-sided formula.")
  }
  if (length(constraint) != 2L) {
    cli::cli_abort(c(
      "{.arg support_constraint} must be a one-sided formula.",
      "x" = "It has a left-hand side: {.code {deparse1(constraint)}}."
    ))
  }
  if (is.null(is_effect)) {
    is_effect <- function(name) default_is_effect(name, envir)
  }

  registry <- new.env(parent = emptyenv())
  registry$atoms <- list()
  registry$labels <- character(0)

  # Deduplicate atoms by their deparsed form; identical atoms share a placeholder.
  register_atom <- function(call_obj) {
    label <- deparse1(call_obj)
    hit <- match(label, registry$labels)
    if (!is.na(hit)) {
      return(as.name(paste0(".a", hit)))
    }
    idx <- length(registry$atoms) + 1L
    registry$atoms[[idx]] <- call_obj
    registry$labels[idx] <- label
    as.name(paste0(".a", idx))
  }

  abort_grammar <- function(node) {
    cli::cli_abort(
      c(
        "Unsupported construct in {.arg support_constraint}:
         {.code {deparse1(node)}}.",
        "i" = "Allowed: effect atoms, numeric constants, boolean
               {.code & | !}, comparisons {.code > < >= <= == !=}, and
               elementwise arithmetic {.code + - * /} (with parentheses)."
      ),
      call = NULL
    )
  }

  # Two mutually recursive walkers track grammar context so the bare-effect
  # shorthand (`effect` -> `effect != 0`) applies in boolean position only, not
  # inside a comparison or arithmetic sub-expression.
  walk_bool <- function(node) {
    if (is.call(node)) {
      head <- as.character(node[[1]])
      if (head %in% .constraint_bool_ops) {
        return(as.call(list(
          node[[1]],
          walk_bool(node[[2]]),
          walk_bool(node[[3]])
        )))
      }
      if (head == "!") {
        return(as.call(list(node[[1]], walk_bool(node[[2]]))))
      }
      if (head == "(") {
        return(walk_bool(node[[2]]))
      }
      if (head %in% .constraint_cmp_ops) {
        return(as.call(list(
          node[[1]],
          walk_arith(node[[2]]),
          walk_arith(node[[3]])
        )))
      }
      if (is_effect_call(node, is_effect)) {
        return(call("!=", register_atom(node), 0))
      }
    }
    abort_grammar(node)
  }

  walk_arith <- function(node) {
    if (is.numeric(node)) {
      return(node)
    }
    if (is.call(node)) {
      head <- as.character(node[[1]])
      if (head %in% .constraint_arith_ops) {
        if (length(node) == 2L) {
          return(as.call(list(node[[1]], walk_arith(node[[2]]))))
        }
        return(as.call(list(
          node[[1]],
          walk_arith(node[[2]]),
          walk_arith(node[[3]])
        )))
      }
      if (head == "(") {
        return(walk_arith(node[[2]]))
      }
      if (is_effect_call(node, is_effect)) {
        return(register_atom(node))
      }
    }
    abort_grammar(node)
  }

  rhs <- constraint[[2L]]
  expr <- walk_bool(rhs)

  atom_names <- vapply(
    registry$atoms,
    function(a) as.character(a[[1L]]),
    character(1)
  )

  list(
    atoms = registry$atoms,
    atom_labels = registry$labels,
    atom_names = atom_names,
    expr = expr
  )
}

# ==== Context-dependent constraint validators ==============================
# Out-of-grammar rejection is intrinsic to the parser above; these two guards
# need model context (an availability registry / the atoms' axes), so the
# wiring supplies their inputs.

# Effects that read the availability mask / risk set. Empty today (no such
# effect exists); the anti-cycle guard below activates when one is added.
.constraint_availability_effects <- character(0)

#' Anti-cycle rule: a constraint atom may not read the mask it defines
#'
#' An atom whose statistic reads the active/risk set makes
#' `mask -> atom -> mask` a fixpoint rather than a DAG. Keeps the data flow a
#' two-layer DAG (atoms -> mask).
#'
#' @param atom_labels,atom_names deparsed labels and head names of the atoms.
#' @param is_availability predicate flagging availability-derived effect names;
#'   defaults to membership in `.constraint_availability_effects`.
#' @noRd
reject_availability_atoms <- function(
  atom_labels,
  atom_names,
  is_availability = NULL
) {
  if (is.null(is_availability)) {
    is_availability <- function(name) {
      name %in% .constraint_availability_effects
    }
  }
  bad <- vapply(atom_names, is_availability, logical(1))
  if (any(bad)) {
    cli::cli_abort(
      c(
        "A {.arg support_constraint} atom may not depend on the risk set it
         defines.",
        "x" = "Availability-derived atom{?s}: {.code {atom_labels[bad]}}.",
        "i" = "Constrain on networks or attributes, not on the active/risk set."
      ),
      call = NULL
    )
  }
  invisible(atom_labels)
}

#' Build a one-sided additive formula from a list of atom call objects
#'
#' Turns the distinct atoms extracted from the boolean tree into a plain
#' `~ a + b + ...` formula so the existing `get_rhs_names()` machinery normalises
#' them into the same per-effect shape main effects use (reuse verbatim).
#' @noRd
atoms_to_formula <- function(atoms) {
  rhs <- Reduce(function(a, b) call("+", a, b), atoms)
  stats::as.formula(call("~", rhs))
}

#' Parse and validate a support_constraint into a plan-ready structure
#'
#' Runs the boolean-tree parse, the anti-cycle guard, and — for a
#' sender-indexed-only specification (`has_dyad_part = FALSE`) — the D13 dyadic
#' rejection. Classifies each atom's broadcast kind (dyad classification) so the
#' mask storage kind is known up front. The result carries everything downstream
#' preprocessing needs without re-parsing.
#'
#' @param constraint a one-sided `support_constraint` formula.
#' @param has_dyad_part `TRUE` when the specification has a dyad-indexed part (a
#'   choice submodel, or REM); `FALSE` for a rate-only spec, where dyadic atoms
#'   are accepted and folded on the sender axis via the row-reduction (D12),
#'   with a one-time informational message.
#' @param envir environment where the constraint's objects live.
#' @return a `support_constraint_plan` object: the original `formula`, the
#'   `atoms` / `atom_labels` / `atom_names`, per-atom `atom_kinds`, the mask
#'   `expr`, and the derived `mask_kind`.
#' @noRd
parse_and_validate_constraint <- function(
  constraint,
  has_dyad_part,
  envir = parent.frame()
) {
  pc <- parse_support_constraint(constraint, envir = envir)
  reject_availability_atoms(pc$atom_labels, pc$atom_names)

  # Normalise atoms through get_rhs_names, then classify each on the dyad grid:
  # point (0) / alter (1) need the dyad kernel; ego (2) / global (3) are
  # sender-axis. `type` (deparsed in the rhs entry) is re-parsed for degree
  # effects so an ego-perspective degree classifies as sender-axis.
  constraint_rhs <- get_rhs_names(atoms_to_formula(pc$atoms))
  atom_kinds <- vapply(
    constraint_rhs,
    function(entry) {
      fmls <- if (!is.null(entry$type)) {
        list(type = str2lang(entry$type))
      } else {
        list()
      }
      classify_broadcast_kind(entry[[1L]], fmls, "dyad")
    },
    integer(1)
  )

  if (!has_dyad_part) {
    inform_dyadic_sender_reduction(pc$atom_labels, atom_kinds)
  }

  structure(
    list(
      formula = constraint,
      atoms = pc$atoms,
      atom_labels = pc$atom_labels,
      atom_names = pc$atom_names,
      atom_kinds = atom_kinds,
      expr = pc$expr,
      mask_kind = axis_union_kind(atom_kinds)
    ),
    class = "support_constraint_plan"
  )
}

#' Tag a constraint sub-plan and derive its mask storage kind
#'
#' A `support_constraint` is carried as a sibling sub-plan, never mixed into the
#' estimated `plan$effects`: its atoms produce no estimated column, so keeping
#' them out of the estimated registry leaves the coefficient columns (and the
#' 1e-6 baselines) bit-identical, and sidesteps the flat-update-stream reindexing
#' that holding a column out of estimation would require. The atoms are still
#' first-class plan entries, tagged `role = "constraint"`, `estimate = FALSE`.
#'
#' The mask's storage kind is the axis-union of its atoms' broadcast kinds (the
#' same rule interaction products use): `3` global -> scalar, `2` ego -> length-n1
#' vector, `1` alter -> length-n2 vector, `0` point -> dense n1xn2 matrix. A dense
#' matrix is allocated only when a genuinely dyadic (point) atom is present.
#'
#' @param constraint_plan a plan built by `build_update_plan()` over the
#'   constraint atoms alone (its `effects`/`routing`/`effect_objects`/`objects`).
#' @param mask_expr the evaluable mask expression over `.a{k}` placeholders.
#' @param atom_labels the atoms' deparsed labels, aligned with `.a{k}`.
#' @return the constraint sub-plan with atoms tagged `role = "constraint"`, the
#'   mask expression, atom labels, and the derived `mask_kind`.
#' @noRd
augment_constraints <- function(constraint_plan, mask_expr, atom_labels) {
  constraint_plan$effects$role <- "constraint"
  constraint_plan$effects$estimate <- FALSE
  list(
    effects = constraint_plan$effects,
    effect_objects = constraint_plan$effect_objects,
    routing = constraint_plan$routing,
    objects = constraint_plan$objects,
    expr = mask_expr,
    atom_labels = atom_labels,
    mask_kind = axis_union_kind(constraint_plan$effects$broadcast_kind)
  )
}

#' D12: accept dyadic atoms in a sender-indexed-only specification via the
#' row-reduction, informing about the reduction
#'
#' A dyadic (`point`- or `alter`-kind) `support_constraint` on a rate / rate
#' _ordered spec with no choice formula is consumed on the sender axis by the
#' row-reduction (folded into `active_sender` during preprocessing): a sender is
#' at risk iff it has at least one allowed, present receiver. This is the same
#' definition the joint specification uses,
#' so rejecting it would be a capability regression and a semantic break.
#' A one-time informational message explains the reduction and the cheaper
#' ego-kind reformulation, WITHOUT overclaiming: `outdeg(net) > 0` matches
#' `tie(net)` only under static receiver composition (outdeg counts ties to
#' absent receivers; the row-reduction does not).
#'
#' @param atom_labels deparsed atom labels.
#' @param atom_kinds their broadcast kinds under dyad classification
#'   (`0` point / `1` alter fold via the row-reduction; `2` ego / `3` global
#'   fold directly).
#' @noRd
inform_dyadic_sender_reduction <- function(atom_labels, atom_kinds) {
  dyadic <- atom_kinds %in% c(0L, 1L)
  if (any(dyadic)) {
    cli::cli_inform(c(
      "i" = "{.arg support_constraint}: {cli::qty(sum(dyadic))}the dyadic
             atom{?s} {.code {atom_labels[dyadic]}} {?is/are} consumed on the
             sender axis by row-reduction — a sender is at risk iff it has
             at least one allowed, present receiver.",
      "i" = "For a cheaper sender-axis formulation, use an ego-kind atom, e.g.
             {.code ~ tie(net)} becomes {.code ~ outdeg(net) > 0}.",
      "!" = "That reformulation is equivalent only under static receiver
             composition: {.fn outdeg} counts ties to absent receivers, the
             row-reduction does not."
    ))
  }
  invisible(atom_labels)
}
