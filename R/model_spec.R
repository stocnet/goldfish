#' Internal model specification classes
#'
#' Low-level constructors for the typed model specification objects that
#' carry the resolved model variant through preprocessing and estimation.
#' The class vector follows `c("<variant>_spec", "<indexing>_spec",
#' "model_spec")`. `sender_spec` variants are always one-mode: they force
#' `is_two_mode = FALSE` and use `nodes` for both modes.
#'
#' @param is_two_mode logical, whether sender and receiver node sets differ.
#' @param nodes,nodes2 names of the node sets of the dependent events.
#' @param ... additional fields stored in the spec object.
#'
#' @return an object of class `model_spec`.
#' @name model_spec
#' @noRd
model_spec_structure <- function(
    variant, indexing, model, sub_model, is_two_mode, nodes, nodes2, ...) {
  structure(
    list(
      model = model,
      sub_model = sub_model,
      is_two_mode = is_two_mode,
      nodes = nodes,
      nodes2 = nodes2,
      ...
    ),
    class = c(variant, indexing, "model_spec")
  )
}

#' @rdname model_spec
#' @noRd
dynam_rate_spec <- function(nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "dynam_rate_spec", "sender_spec", "DyNAM", "rate",
    is_two_mode = FALSE, nodes = nodes, nodes2 = nodes2, ...
  )
}

#' @rdname model_spec
#' @noRd
dynam_rate_ordered_spec <- function(nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "dynam_rate_ordered_spec", "sender_spec", "DyNAM", "rate_ordered",
    is_two_mode = FALSE, nodes = nodes, nodes2 = nodes2, ...
  )
}

#' @rdname model_spec
#' @noRd
dynam_choice_spec <- function(
    is_two_mode = FALSE, nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "dynam_choice_spec", "dyad_spec", "DyNAM", "choice",
    is_two_mode = is_two_mode, nodes = nodes, nodes2 = nodes2, ...
  )
}

#' @rdname model_spec
#' @noRd
dynam_choice_coord_spec <- function(
    is_two_mode = FALSE, nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "dynam_choice_coord_spec", "dyad_spec", "DyNAM", "choice_coordination",
    is_two_mode = is_two_mode, nodes = nodes, nodes2 = nodes2, ...
  )
}

#' @rdname model_spec
#' @noRd
dynami_rate_spec <- function(nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "dynami_rate_spec", "sender_spec", "DyNAMi", "rate",
    is_two_mode = FALSE, nodes = nodes, nodes2 = nodes2, ...
  )
}

#' @rdname model_spec
#' @noRd
dynami_rate_ordered_spec <- function(nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "dynami_rate_ordered_spec", "sender_spec", "DyNAMi", "rate_ordered",
    is_two_mode = FALSE, nodes = nodes, nodes2 = nodes2, ...
  )
}

#' @rdname model_spec
#' @noRd
dynami_choice_spec <- function(
    is_two_mode = FALSE, nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "dynami_choice_spec", "dyad_spec", "DyNAMi", "choice",
    is_two_mode = is_two_mode, nodes = nodes, nodes2 = nodes2, ...
  )
}

#' @rdname model_spec
#' @noRd
rem_rate_spec <- function(
    is_two_mode = FALSE, nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "rem_rate_spec", "dyad_spec", "REM", "rate",
    is_two_mode = is_two_mode, nodes = nodes, nodes2 = nodes2, ...
  )
}

#' @rdname model_spec
#' @noRd
rem_rate_ordered_spec <- function(
    is_two_mode = FALSE, nodes = NULL, nodes2 = nodes, ...) {
  model_spec_structure(
    "rem_rate_ordered_spec", "dyad_spec", "REM", "rate_ordered",
    is_two_mode = is_two_mode, nodes = nodes, nodes2 = nodes2, ...
  )
}
