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

  list(
    atoms = registry$atoms,
    atom_labels = registry$labels,
    expr = expr
  )
}
