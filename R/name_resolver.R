# =========================================================================== #
# Formula name resolution against the data components.
#
# With one data object a formula name no longer names an object in the caller's
# environment: it is a layer, a column of `nodes`, or a variable of `global`,
# and which one decides how the builders route it. The resolver answers that
# once, in the parser, by rewriting each effect's object reference into the
# `<nodeset>$<attribute>` form the object table already speaks -- so every
# builder downstream keeps reading the table it always read, and effects and
# support_constraint atoms resolve through this one path.
#
# The three namespaces are searched in order (layer, nodes column, global
# variable). A name in both `nodes` and `global` is ambiguous and aborts: the
# effect would silently read one of two different things.
# =========================================================================== #

# The nodeset slot a global reference carries. A global lives in its own
# component rather than a node set, but the object table has only this one slot
# to name it with; the leading dot keeps the marker apart from any node set,
# whose names are syntactic by validation.
GLOBAL_NODESET <- ".global"

#' Resolve a formula's object references against the data components
#'
#' @param rhs_names the parser's per-term name lists.
#' @param src a data source; the legacy source resolves names by `get()` in its
#'   environment, so it is returned untouched.
#' @param call calling environment for error reporting.
#' @param user_env the caller's environment, so the `df$var` deprecation is
#'   attributed to the code that wrote the formula rather than to goldfish.
#'
#' @return `rhs_names` with every object reference rewritten to a layer name or
#'   a `<nodeset>$<attribute>` reference.
#' @noRd
resolve_formula_names <- function(
  rhs_names,
  src,
  call = rlang::caller_env(),
  user_env = rlang::caller_env(2)
) {
  if (!inherits(src, "data_source_stocnet")) {
    return(rhs_names)
  }
  sides <- ds_side_names(src)
  nodes <- sides[1]
  nodes2 <- sides[2]
  for (i in seq_along(rhs_names)) {
    term <- rhs_names[[i]]
    effect <- term[[1]]
    slots <- object_ref_slots(term)
    for (k in seq_along(slots)) {
      j <- slots[[k]]
      rhs_names[[i]][[j]] <- resolve_object_ref(
        term[[j]],
        effect = effect,
        position = k,
        src = src,
        nodes = nodes,
        nodes2 = nodes2,
        call = call,
        user_env = user_env
      )
    }
  }
  rhs_names
}

# The effects whose statistic compares the two sides of the focal dyad, so a
# single written operand reads one attribute on both sides. On a two-mode focal
# those are different node spaces and the reference expands to one position per
# side; on a one-mode focal both positions name the same reference and
# get_data_objects()' unique() collapses them back to one, which is what keeps
# the one-mode path on its existing arity-1 route.
CROSS_SIDE_EFFECTS <- c("same", "diff", "sim")

# Which side of the focal dyad an attribute position reads. `alter` reads the
# receiver side; `ego_alter_interaction` reads the sender side with its first
# operand and the receiver side with its second; everything else reads the
# sender side.
attribute_side <- function(effect, position, nodes, nodes2) {
  if (identical(effect, "alter")) {
    return(nodes2)
  }
  if (identical(effect, "ego_alter_interaction") && position == 2) {
    return(nodes2)
  }
  nodes
}

# Which elements of a term are data-object references: everything after the
# effect name that is positional or carries a reserved argument name. This
# mirrors how get_data_objects() picks them out, so the two never disagree
# about what is an object.
object_ref_slots <- function(term) {
  if (length(term) < 2) {
    return(integer(0))
  }
  slots <- seq_along(term)[-1]
  arg_names <- names(term)
  if (is.null(arg_names)) {
    return(slots)
  }
  keep <- arg_names[slots] == "" | isReservedElementName(arg_names[slots])
  slots[keep]
}

# One reference, which may be a `list(a, b)` of them.
resolve_object_ref <- function(
  ref,
  effect,
  position,
  src,
  nodes,
  nodes2,
  call,
  user_env
) {
  if (!grepl("^list\\(", ref)) {
    return(resolve_one_name(
      ref,
      effect,
      position,
      src,
      nodes,
      nodes2,
      call,
      user_env
    ))
  }
  inner <- trimws(strsplit(gsub("^list\\((.+)\\)$", "\\1", ref), ",")[[1]])
  resolved <- vapply(
    seq_along(inner),
    function(k) {
      resolve_one_name(
        inner[[k]],
        effect = effect,
        position = k,
        src = src,
        nodes = nodes,
        nodes2 = nodes2,
        call = call,
        user_env = user_env,
        # Already inside a list: a cross-side expansion here would nest one
        # list inside another, which the object table cannot read.
        expand = FALSE
      )
    },
    character(1)
  )
  paste0("list(", paste(resolved, collapse = ", "), ")")
}

resolve_one_name <- function(
  name,
  effect,
  position,
  src,
  nodes,
  nodes2,
  call,
  user_env,
  expand = TRUE
) {
  bare <- drop_data_frame_prefix(name, user_env)

  if (bare %in% src$layers) {
    return(bare)
  }
  in_nodes <- bare %in% names(src$nodes)
  in_global <- bare %in% names(src$streams$global)
  if (in_nodes && in_global) {
    cli::cli_abort(
      c(
        "{.val {bare}} is both a {.field nodes} column and a {.field global}
         variable.",
        "x" = "Which one {.fn {effect}} should read is ambiguous.",
        "i" = "Rename one of them."
      ),
      call = call
    )
  }
  if (in_nodes) {
    # One nodes tibble serves every side, so a reference names the slice its
    # position reads. A comparison effect reads both sides from one written
    # operand, so it expands here into one reference per side.
    if (expand && effect %in% CROSS_SIDE_EFFECTS) {
      return(sprintf(
        "list(%s, %s)",
        paste(nodes, bare, sep = "$"),
        paste(nodes2, bare, sep = "$")
      ))
    }
    side <- attribute_side(effect, position, nodes, nodes2)
    return(paste(side, bare, sep = "$"))
  }
  if (in_global) {
    return(paste(GLOBAL_NODESET, bare, sep = "$"))
  }
  abort_unknown_name(bare, effect, src, call)
}

# The `df$var` prefix named the data frame an attribute lived in. There is one
# data object now, so the prefix has nothing left to select: drop it, resolve
# the bare name, and point at the bare syntax.
drop_data_frame_prefix <- function(name, user_env = rlang::caller_env(2)) {
  if (!grepl("$", name, fixed = TRUE)) {
    return(name)
  }
  parts <- strsplit(name, "$", fixed = TRUE)[[1]]
  bare <- parts[length(parts)]
  lifecycle::deprecate_warn(
    when = "1.9.0",
    what = I(sprintf("The data-frame prefix in `%s`", name)),
    with = I(sprintf("`%s`", bare)),
    details = "Attributes now resolve against the data object's components.",
    id = "goldfish_attribute_prefix",
    user_env = user_env
  )
  bare
}

abort_unknown_name <- function(bare, effect, src, call) {
  cli::cli_abort(
    c(
      "{.fn {effect}} refers to {.val {bare}}, which is not in the data.",
      "i" = "Available layer{?s}: {.val {src$layers}}.",
      "i" = "Available {.field nodes} column{?s}: {.val {names(src$nodes)}}.",
      "i" = "Available {.field global} variable{?s}:
             {.val {names(src$streams$global)}}."
    ),
    call = call
  )
}
