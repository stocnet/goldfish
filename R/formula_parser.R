####################### #
#
# Goldfish package
# Parsings (and checking) formulae
#
####################### #

#' parse formula
#' A valid formula should have:
#' - on the left side a list of dependent events
#' - on the right side a list of names that correspond to effects
#'   we have in our pre-defined functions
#' - parameters for the effects that are coherent with the documentation
#'   on top of this, we parse the formula to the right format
#'   for the rest of the estimation
#' @param formula a class \code{formula} object that defines the model
#'
#' @return a list with parsed values needed in the next steps
#' @noRd
#'
#' @examples
#' \donttest{
#' calls <- structure(
#'   list(time = 1, sender = "a", receiver = "b", replace = 1),
#'   class = "dependent.goldfish"
#' )
#' call_network <- structure(matrix(0, 3, 3), class = "network.goldfish")
#'
#' parse_formula(
#'   calls ~ outdeg(call_network, type = "ego") +
#'     indeg(call_network, type = "alter")
#' )
#' }
parse_formula <- function(
  formula,
  envir = new.env(),
  realize_windows = TRUE,
  data = NULL
) {
  # The formula's left-hand side names the dependent process, so it -- not
  # `info$focal` -- is the focal layer this parse resolves against, which is
  # what lets `layer` override the declaration.
  dep_name <- get_dependent_name(formula)
  # A wrapper-created dependent name resolves to its focal layer (via
  # info$dependents) so the legacy dependent identifier keeps naming the process
  # here too; a real layer name is not in the map and passes through unchanged.
  if (!is.null(data) && !is.environment(data)) {
    entry <- data$info$dependents[[dep_name]]
    if (!is.null(entry)) {
      dep_name <- entry$layer
    }
  }
  src <- new_data_source(data = data, envir = envir, focal = dep_name)
  ds_check_dependent(src, dep_name)
  rhs_names <- get_rhs_names(formula)
  # Per-term flags from get_rhs_names: offset (fixed-coefficient) and
  # the interaction roles is_main / is_operand + the interaction structure.
  # All are kept aligned with rhs_names through the intercept drop
  # below; interaction operand indices are in the variable frame (i.e. the
  # rhs_names frame after the `1` is dropped).
  is_offset <- attr(rhs_names, "offset")
  if (is.null(is_offset)) {
    is_offset <- logical(length(rhs_names))
  }
  offset_coef <- attr(rhs_names, "offset_coef")
  if (is.null(offset_coef)) {
    offset_coef <- rep(NA_real_, length(rhs_names))
  }
  is_main <- attr(rhs_names, "is_main")
  if (is.null(is_main)) {
    is_main <- rep(TRUE, length(rhs_names))
  }
  is_operand <- attr(rhs_names, "is_operand")
  if (is.null(is_operand)) {
    is_operand <- logical(length(rhs_names))
  }
  interactions <- attr(rhs_names, "interactions")
  if (is.null(interactions)) {
    interactions <- list()
  }
  attr(rhs_names, "offset") <- NULL
  attr(rhs_names, "offset_coef") <- NULL
  attr(rhs_names, "is_main") <- NULL
  attr(rhs_names, "is_operand") <- NULL
  attr(rhs_names, "interactions") <- NULL
  int <- parse_intercept(rhs_names)
  rhs_names <- int[[1]]
  has_intercept <- int[[2]]
  if (has_intercept) {
    is_offset <- is_offset[-1]
    offset_coef <- offset_coef[-1]
    is_main <- is_main[-1]
    is_operand <- is_operand[-1]
  }
  default_network_name <- ds_default_network(src, dep_name)
  if (!is.null(default_network_name)) {
    no_object_ids <- which(1 == vapply(rhs_names, length, integer(1)))
    for (i in no_object_ids) {
      rhs_names[[i]][[2]] <- default_network_name
    }
  }

  # Resolve object references against the data components before anything reads
  # them as objects: a bare attribute name is indistinguishable from a layer
  # name until it is looked up, and parse_time_windows() below rejects windows
  # on attributes by exactly that distinction.
  rhs_names <- resolve_formula_names(
    rhs_names,
    src,
    user_env = rlang::caller_env()
  )

  window_parameters <- lapply(rhs_names, getElement, "window")
  rhs_names <- parse_time_windows(
    rhs_names,
    envir = envir,
    realize_windows = realize_windows
  )
  # Metadata recipe for the windowed derivations: each windowed
  # effect's derived-network/dissolve-stream recipe, carried on parsed_formula so
  # `build_spec_map()` can fill plan$derivations from it. Unlike the other slots
  # it is NOT per-effect aligned (one row per windowed network), so it is excluded
  # from the elementwise slot comparison in compare_formulas(). The rewrite of the
  # effect object refs already happened in parse_time_windows(); realization is
  # deferred to state creation on the recipe path (realize_windows = FALSE).
  window_derivations <- attr(rhs_names, "window_derivations")
  attr(rhs_names, "window_derivations") <- NULL
  # Derived windowed networks may not be realized yet when realize_windows =
  # FALSE (recipe path); parse_multiple_effects() must recognize them as networks
  # from the recipe instead of fetching the (absent) object.
  derived_names <- vapply(
    window_derivations,
    function(d) d$derived_name,
    character(1)
  )
  mult <- parse_multiple_effects(
    rhs_names,
    envir = envir,
    derived_names = derived_names,
    src = src
  )
  rhs_names <- mult[[1]]
  ignore_rep_parameter <- mult[[2]]
  if (any(unlist(ignore_rep_parameter)) && is.null(default_network_name)) {
    stop(
      "No default network defined, thus ",
      dQuote("ignore_repetitions = TRUE"),
      " effects cannot be used.",
      call. = FALSE
    )
  }
  weighted_parameter <- lapply(rhs_names, function(x) {
    v <- getElement(x, "weighted")
    ifelse(!is.null(v) && substr(v, 1, 1) == "T", TRUE, FALSE)
  })
  type_parameter <- lapply(rhs_names, function(x) {
    v <- getElement(x, "type")
    ifelse(!is.null(v), eval(parse(text = v), envir = envir), "")
  })
  get_fun_name <- function(x, which) {
    v <- getElement(x, which)
    v <- ifelse(!is.null(v), v, "")
    v <- gsub("['\" ]", "", v) # replace quotation marks
    v <- ifelse(
      grepl("function.?\\(", v) || nchar(v) > 12,
      "userDefined",
      v
    ) # if it is a function, it is replace by short text
  }
  trans_parameter <- lapply(rhs_names, get_fun_name, "transformer_fn")
  summ_parameter <- lapply(rhs_names, get_fun_name, "summarizer_fn")
  joining_parameter <- lapply(rhs_names, function(x) {
    v <- getElement(x, "joining")
    ifelse(!is.null(v), v, "")
  })
  sub_type_parameter <- lapply(rhs_names, function(x) {
    v <- getElement(x, "sub_type")
    ifelse(!is.null(v), v, "")
  })

  # history parameter closure effects
  history_parameter <- lapply(rhs_names, function(x) {
    v <- getElement(x, "history")
    ifelse(!is.null(v), eval(parse(text = v), envir = envir), "")
  })

  res <- list(
    rhs_names = rhs_names,
    dep_name = dep_name,
    has_intercept = has_intercept,
    default_network_name = default_network_name,
    window_parameters = window_parameters,
    ignore_rep_parameter = ignore_rep_parameter,
    weighted_parameter = weighted_parameter,
    type_parameter = type_parameter,
    trans_parameter = trans_parameter,
    summ_parameter = summ_parameter,
    joining_parameter = joining_parameter,
    sub_type_parameter = sub_type_parameter,
    history_parameter = history_parameter,
    offset_parameter = as.list(is_offset),
    # The fixed value an `offset()` term carries in the formula itself
    # (`coef =`), NA where the term leaves it to `offset_coef`. Per effect, so
    # it stays aligned through the intercept drop above.
    offset_coef_parameter = as.list(offset_coef),
    # Interaction roles: an operand-only term is retained but not
    # freely estimated; a requested main effect (and an offset, whose
    # coefficient is fixed rather than estimated) is an estimated column.
    # `interactions` lists each
    # interaction's ordered operand indices (rhs_names frame), label, and arity.
    is_main_parameter = as.list(is_main),
    is_operand_parameter = as.list(is_operand),
    estimate_parameter = as.list(is_main | is_offset),
    interactions = interactions,
    window_derivations = window_derivations
  )
  return(res)
}


# Comparison of two parsed formulas for preprocessed
# throws errors when: dependent events or default network are not the same,
#  when there is righ-censoring
# for one and not the other
# returns: a list of the size of the new formula, with zeros when the effects
#    are new, and with the
#    the index of the effect in the old formula if the effect was already there
compare_formulas <- function(
  old_parsed_formula,
  new_parsed_formula,
  model,
  sub_model
) {
  # global check
  if (identical(old_parsed_formula, new_parsed_formula)) {
    return(seq_along(new_parsed_formula$rhs_names))
  }
  # test dependent events and default network
  if (old_parsed_formula$dep_name != new_parsed_formula$dep_name) {
    stop(
      "The dependent events in the formula are not the ones used in",
      " the preprocessed object given in preprocessed."
    )
  }
  if (
    !identical(
      old_parsed_formula$default_network_name,
      new_parsed_formula$default_network_name
    )
  ) {
    stop(
      "The default network in the formula is not the one used in",
      " the preprocessed object given in preprocessed."
    )
  }
  old_has_intercept <- old_parsed_formula$has_intercept
  new_has_intercept <- new_parsed_formula$has_intercept
  if (
    model %in%
      "DyNAM" &&
      sub_model %in% c("choice", "choice_coordination") &&
      old_has_intercept
  ) {
    old_has_intercept <- FALSE
    new_has_intercept <- FALSE
  }
  if (old_has_intercept && !new_has_intercept) {
    stop(
      "The preprocessing for the object in preprocessed was not done",
      " with the right-censored intervals that this formula requires."
    )
  }
  if (!old_has_intercept && new_has_intercept) {
    stop(
      "The preprocessing for the object in preprocessed was done",
      " with right-censored intervals and this formula does not include those."
    )
  }
  size_old <- length(old_parsed_formula$rhs_names)
  size_new <- length(new_parsed_formula$rhs_names)
  effects_indexes <- rep(0, size_new)
  slot_compare <- names(old_parsed_formula)
  slot_compare <- slot_compare[
    !slot_compare %in%
      c(
        "dep_name",
        "has_intercept",
        "default_network_name",
        # window_derivations is one row per windowed network, not per effect, so it
        # cannot be compared elementwise against the effect indices below.
        "window_derivations",
        # interactions is one row per interaction term, not per effect, so it is
        # likewise excluded from the elementwise effect comparison.
        "interactions",
        # Neither whether a term's coefficient is fixed nor the value it is
        # fixed at changes the statistic column that was preprocessed: an
        # `offset()` keeps the column and only decides that the coefficient is
        # not estimated. So a term must still be recognized as the effect
        # already computed when it is wrapped, unwrapped, or given another
        # value -- which is also what lets two processes of one specification
        # share a statistic while only one of them fixes it.
        "offset_parameter",
        "offset_coef_parameter"
      )
  ]
  for (i in seq.int(size_new)) {
    for (j in seq.int(size_old)) {
      flag <- FALSE
      for (slot_name in slot_compare) {
        if (
          !identical(
            old_parsed_formula[[slot_name]][[j]],
            new_parsed_formula[[slot_name]][[i]]
          )
        ) {
          flag <- TRUE
        }
      }
      if (flag) {
        next
      }
      effects_indexes[i] <- j
    }
  }
  return(effects_indexes)
}

#' Build the upfront specification mapping
#'
#' Umbrella that compiles the data-light engine structures once as the outcome
#' of parsing, so `preprocess()` consumes them instead of rebuilding
#' the update plan and call templates on every call. It orchestrates the trio:
#' the `parsed_terms` bundle (from `parse_formula()`), the per-effect call
#' templates (`build_effects_template()`), and the registries-only update plan
#' (`build_update_plan()`).
#'
#' The `spec_map` merges the dispatch `model_spec` (its fields and class vector)
#' so `preprocess()` dispatches directly on the returned object: the
#' effect closures, per-term window parameters, and the link matrices ride on it
#' rather than being threaded into `preprocess()` as separate bridge arguments.
#' The recipe loops unpack them from the `spec` they receive.
#'
#' Stage 1 scope: this still builds a throwaway state container to read object
#' keys, so it is not yet metadata-pure. The full metadata/data boundary —
#' keeping this mapping free of event/network tables and relocating
#' `sanitizeEvents`/windowing to state creation — is a later, deferred slice.
#'
#' @param parsed_formula the list returned by `parse_formula()`.
#' @param model_spec the dispatch `model_spec` from `new_model_spec()`; its
#'   fields (`model`, `nodes`, ...) and class vector are merged onto the result.
#' @param effects list of effect functions from `create_effects_functions()`.
#' @param window_parameters per-term window parameters (from the parsed
#'   formula), aligned with `effects`.
#' @param objects_effects_link matrix from `get_objects_effects_link()`.
#' @param events_objects_link,events_effects_link link structures from
#'   `build_events_objects_link()` / `get_events_effects_link()`.
#' @param fetch_plan the ordered event-stream fetch plan from
#'   `build_events_objects_link()`; carried on the spec_map so the recipe loop
#'   fetches events inside state creation.
#' @param envir environment where the data objects live.
#'
#' @return an S3 object of class `c(class(model_spec), "goldfishSpecMap")`
#'   carrying the `model_spec` fields plus `parsed_terms`, `plan`,
#'   `effects_template`, `effect_description` (the single source of truth for
#'   print/naming metadata), and the `effects`/`window_parameters`/
#'   link inputs the recipe loops consume.
#' @noRd
build_spec_map <- function(
  parsed_formula,
  model_spec,
  effects,
  window_parameters,
  objects_effects_link,
  events_objects_link,
  events_effects_link,
  fetch_plan,
  support_constraint = NULL,
  envir = new.env(),
  data = NULL,
  modeled_flavor = NULL
) {
  stat_kind <- if (inherits(model_spec, "goldfishAxisSender")) {
    "sender"
  } else {
    "dyad"
  }
  nodes <- model_spec$nodes
  nodes2 <- model_spec$nodes2
  # Metadata/data boundary: the plan + call templates need
  # only the object-keys mapping, so read it directly (class/structure only) —
  # do NOT materialise a state container here (no network/nodal data copied).
  object_keys <- build_object_keys(
    rownames(objects_effects_link),
    nodes,
    nodes2,
    envir = envir,
    derivations = parsed_formula$window_derivations,
    data = data
  )
  state_keys <- structure(list(), object_keys = object_keys)
  plan <- build_update_plan(
    effects,
    events_objects_link,
    events_effects_link,
    objects_effects_link,
    state_keys,
    stat_kind = stat_kind,
    envir = envir,
    derivations = parsed_formula$window_derivations,
    data = data
  )
  # Interaction columns: append one estimated product column per
  # interaction and set operand roles / estimate flags. No-op when the formula
  # has no interactions (registries stay empty, effects unchanged).
  plan <- augment_interactions(plan, parsed_formula, stat_kind)
  # Derived-input registry: one entry per derived object,
  # filled from metadata only. Today the sole `kind` is "window"; the effect
  # object refs were already rewired to `derived_name` in parse_time_windows().
  # State creation iterates this to realize each derived object (no tables or
  # assign happen here).
  plan$derivations <- build_derivations(
    parsed_formula$window_derivations,
    objects_effects_link,
    envir = envir,
    data = data
  )
  # A supplied support_constraint compiles into a sibling sub-plan (its atoms'
  # closures/links/update-plan) plus a `kind = "support_mask"` derivation; the
  # recipe loop realizes/updates the mask from the atoms' stat_state. When no
  # constraint is present this is a pure no-op, so unconstrained models stay
  # bit-identical.
  #
  # A multi-flavor walk supplies a NAMED LIST of parsed plans instead, one per
  # distinct `(layer, flavor)` constraint and keyed by its `constraint_id`. Each
  # compiles into its own sibling sub-plan under `plan$support_constraints`, and
  # each registers its own mask derivation; `plan$support_constraint` stays
  # empty there, so the single-constraint field keeps its exact meaning and
  # every existing path is untouched.
  if (!is.null(support_constraint)) {
    compile_one <- function(constraint_plan) {
      compile_support_constraint(
        constraint_plan,
        model = model_spec$model,
        dep_name = parsed_formula$dep_name,
        nodes = nodes,
        nodes2 = nodes2,
        window_derivations = parsed_formula$window_derivations,
        envir = envir,
        data = data
      )
    }
    if (inherits(support_constraint, "goldfishSupportPlan")) {
      plan$support_constraint <- compile_one(support_constraint)
      plan$derivations <- c(
        plan$derivations,
        list(support_mask_derivation(plan$support_constraint))
      )
    } else {
      plan$support_constraints <- lapply(support_constraint, compile_one)
      plan$derivations <- c(
        plan$derivations,
        lapply(plan$support_constraints, support_mask_derivation)
      )
    }
  }
  effects_template <- build_effects_template(
    effects,
    objects_effects_link,
    state_keys
  )
  # Print/naming metadata is formula-derived, so it is owned here once and
  # rendered on demand per context (console / db / export) by
  # CreateNames; the fixed-coefficient marking stays an estimation concern
  # appended downstream (offset terms).
  effect_description <- GetDetailPrint(objects_effects_link, parsed_formula)
  structure(
    c(
      unclass(model_spec),
      list(
        parsed_terms = parsed_formula,
        plan = plan,
        effects_template = effects_template,
        effect_description = effect_description,
        effects = effects,
        window_parameters = window_parameters,
        events_objects_link = events_objects_link,
        events_effects_link = events_effects_link,
        objects_effects_link = objects_effects_link,
        fetch_plan = fetch_plan,
        # The data object rides on the spec_map so state creation resolves the
        # same names the compile did, against the same components. The focal and
        # the modeled flavor ride with it because together they pick the
        # dependent rows: resolving them from `info$focal` at state creation
        # would silently model a different layer whenever `layer` overrode it.
        data = data,
        focal = parsed_formula$dep_name,
        modeled_flavor = modeled_flavor
      )
    ),
    class = c(class(model_spec), "goldfishSpecMap")
  )
}

# Build the derived-input registry from the window derivation recipe
# recorded by parse_time_windows(). Metadata only: reads the source objects'
# event-stream NAMES through the data-resolution seam and the effect columns
# that reference each derived object (already rewired to `derived_name` in
# objects_effects_link) — it fetches no event/network tables and assigns
# nothing. Returns a list with one entry per derived object
# `{derived_name, kind, source, source_streams, params, gids}`, or `NULL` when
# there are no derivations, which state creation iterates to realize each
# derived object into the state container.
build_derivations <- function(
  window_derivations,
  objects_effects_link,
  envir = new.env(),
  data = NULL
) {
  if (length(window_derivations) == 0) {
    return(NULL)
  }
  src <- new_data_source(data = data, envir = envir)
  lapply(window_derivations, function(d) {
    source_streams <- ds_object_streams(src, d$source_name)
    gids <- if (d$derived_name %in% rownames(objects_effects_link)) {
      which(!is.na(objects_effects_link[d$derived_name, ]))
    } else {
      integer(0)
    }
    list(
      derived_name = d$derived_name,
      kind = d$kind,
      source = d$source_name,
      source_streams = source_streams,
      params = list(window = d$window),
      gids = unname(gids)
    )
  })
}

# Compile a parsed support_constraint into an engine-ready sibling sub-plan.
#
# The constraint atoms are plain effects, so they reuse the same builders the
# estimated formula uses (`create_effects_functions`, `get_objects_effects_link`,
# `build_events_objects_link`, `get_events_effects_link`, `build_update_plan`) —
# but they are carried as a SIBLING sub-plan, never mixed into the estimated
# `plan$effects` (they produce no coefficient column, so keeping them out leaves
# the estimated columns and the 1e-6 baselines bit-identical). The atoms always
# use the dyad (`choice`) kernel — the mask is dyad-shaped aux state — with
# the coordination/undirected symmetry applied later at mask assembly, not at
# atom-compute time. Returns the augmented sub-plan plus the atom closures / link
# metadata / fetch plan the recipe loop needs to seed and update the atoms'
# `stat_state`.
compile_support_constraint <- function(
  constraint_plan,
  model,
  dep_name,
  nodes,
  nodes2,
  window_derivations,
  envir,
  data = NULL
) {
  atom_rhs_names <- get_rhs_names(atoms_to_formula(constraint_plan$atoms))
  # The constraint's atoms are plain effects, so their names resolve against the
  # components through the same resolver the estimated formula used.
  atom_rhs_names <- resolve_formula_names(
    atom_rhs_names,
    new_data_source(data = data, envir = envir),
    user_env = rlang::caller_env()
  )
  # Constraint atoms always use the dyad kernel (`init_*_choice`): a dyadic atom
  # needs the dyad machinery, and a sender-axis atom (ego/global) works under it
  # too (broadcasting to an ego row). This is the constraint-scoped auxiliary
  # dyad state — the atoms' kernel is chosen by the constraint, not by the
  # estimated submodel, so a rate pass over a two-formula spec still derives its
  # sender gate from the dyad support.
  atom_sub_model <- "choice"
  effects <- create_effects_functions(
    atom_rhs_names,
    model,
    atom_sub_model,
    envir = envir,
    derivations = window_derivations,
    data = data
  )
  objects_effects_link <- get_objects_effects_link(atom_rhs_names)
  link <- build_events_objects_link(
    dep_name,
    atom_rhs_names,
    nodes,
    nodes2,
    envir = envir,
    derivations = window_derivations,
    data = data
  )
  events_effects_link <- get_events_effects_link(
    atom_rhs_names,
    link$events_objects_link
  )
  object_keys <- build_object_keys(
    rownames(objects_effects_link),
    nodes,
    nodes2,
    envir = envir,
    derivations = window_derivations,
    data = data
  )
  state_keys <- structure(list(), object_keys = object_keys)
  # The mask is always a dyad object (aux dyad state), so the sub-plan is
  # built dyad-shaped regardless of the estimated submodel's `stat_kind`.
  sub_plan <- build_update_plan(
    effects,
    link$events_objects_link,
    events_effects_link,
    objects_effects_link,
    state_keys,
    stat_kind = "dyad",
    envir = envir,
    derivations = window_derivations,
    data = data
  )
  augmented <- augment_constraints(
    sub_plan,
    constraint_plan$expr,
    constraint_plan$atom_labels
  )
  # The atoms are seeded and updated by the recipe loop like main effects, so
  # carry their closures, link metadata, and fetch plan alongside the update
  # plan the mask evaluation consumes. `augmented$effects` is the tagged effect
  # registry (data.frame); the closures ride under a distinct name to avoid
  # shadowing it.
  c(
    augmented,
    list(
      effect_functions = effects,
      atom_rhs_names = atom_rhs_names,
      atom_sub_model = atom_sub_model,
      atom_kinds = constraint_plan$atom_kinds,
      events_objects_link = link$events_objects_link,
      events_effects_link = events_effects_link,
      objects_effects_link = objects_effects_link,
      fetch_plan = link$fetch_plan
    )
  )
}

# The support mask is a derived object: one `plan$derivations` entry,
# `kind = "support_mask"`, source = its atoms, stored at the axis-union broadcast
# kind so a dense n1xn2 matrix is allocated only for a genuinely dyadic (point)
# constraint. The recipe loop realizes/updates it from the atoms' `stat_state`.
support_mask_derivation <- function(constraint_subplan) {
  list(
    derived_name = "__support_mask__",
    kind = "support_mask",
    source = constraint_subplan$atom_labels,
    params = list(
      expr = constraint_subplan$expr,
      mask_kind = constraint_subplan$mask_kind
    ),
    gids = integer(0)
  )
}

# Shared derived -> source resolver. A derived object
# (today only a windowed network) inherits its structural metadata (nodesets,
# event-stream names) from a source object. Consumers that must read that
# metadata *before* the derived object is realized use this to read from the
# source instead of get()-ing the (possibly absent) derived object. Returns a
# named character vector derived_name -> source_name.
derived_source_map <- function(window_derivations) {
  if (length(window_derivations) == 0) {
    return(character(0))
  }
  stats::setNames(
    vapply(window_derivations, `[[`, character(1), "source_name"),
    vapply(window_derivations, `[[`, character(1), "derived_name")
  )
}

# Look up a single derivation entry by its derived name (or NULL if none).
find_derivation <- function(name, window_derivations) {
  for (d in window_derivations) {
    if (identical(d$derived_name, name)) {
      return(d)
    }
  }
  NULL
}

create_effects_functions <- function(
  effect_init,
  model,
  sub_model,
  envir = environment(),
  derivations = NULL,
  data = NULL
) {
  .stat_method <- paste("init", model, sub_model, sep = "_")
  src <- new_data_source(data = data, envir = envir)
  # Two-mode guard (below) evaluates the effect's network argument to read its
  # nodesets. On the recipe path a windowed effect's network arg is the *derived*
  # name, which may not be realized yet; resolve it against
  # a probe environment that binds each unrealized derived name to its source
  # object (identical nodesets), leaving realized/plain objects untouched.
  probe_envir <- envir
  src_map <- derived_source_map(derivations)
  if (length(src_map) > 0) {
    probe_envir <- new.env(parent = envir)
    for (derived_name in names(src_map)) {
      already_realized <- exists(derived_name, envir = envir, inherits = FALSE)
      source_name <- src_map[[derived_name]]
      if (!already_realized && exists(source_name, envir = envir)) {
        assign(
          derived_name,
          get(source_name, envir = envir),
          envir = probe_envir
        )
      }
    }
  }
  effects <- lapply(
    effect_init,
    function(x, model, sub_model) {
      fun_text <- paste("update", model, sub_model, x[[1]], sep = "_")
      FUN <- tryCatch(
        eval(parse(text = fun_text), envir = environment()),
        error = function(e) NULL
      )
      if (is.null(FUN)) {
        tryCatch(
          {
            FUN <- eval(parse(text = x[[1]]), envir = envir)
          },
          error = function(e) stop("Unknown effect ", x[[1]])
        )
      }
      .FUN_stat <- utils::getS3method(
        .stat_method,
        x[[1]],
        optional = TRUE,
        envir = environment()
      )
      if (is.null(.FUN_stat)) {
        .FUN_stat <- utils::getS3method(
          .stat_method,
          "default",
          optional = TRUE,
          envir = envir
        )
      }

      # Update signatures of the effects based on default parameters
      # and above specified parameters
      check_effect_attributes(
        effect = as.character(x[[1]]),
        refs = as.character(x[-1]),
        src = src
      )
      .signature <- formals(FUN)
      .args_names <- names(.signature)
      parms_to_set <- x[-1]
      if (is.null(names(parms_to_set))) {
        named_params <- rep(FALSE, length(parms_to_set))
      } else {
        named_params <- unlist(lapply(
          names(parms_to_set),
          function(v) is.character(v) && v != ""
        ))
      }
      name_arg <- parms_to_set[[1]]
      parms_to_set <- lapply(parms_to_set, function(s) {
        call("eval", parse(text = s))
      })
      .args_replace <- pmatch(names(parms_to_set), .args_names)
      names_ <- names(parms_to_set)[named_params]
      .signature[na.omit(.args_replace)] <- parms_to_set[!is.na(.args_replace)]
      is_condition <- isReservedElementName(.args_names) &
        !(.args_names %in% names_)
      .signature[is_condition] <- parms_to_set[!named_params]
      # Resolve the choice-set arguments (type/history/...) to their single
      # value here, while the closure's original defaults are still readable
      # from `formals(FUN)`, so the init, the gate below, and the update all
      # read an already-validated scalar.
      .signature <- resolve_effect_args(
        .signature,
        original = formals(FUN),
        effect = as.character(x[[1]]),
        envir = envir
      )
      # Two-modeness is resolved from each network argument's OWN layer, never
      # as a blanket from the focal layer, and the mode map is the source of
      # truth: a declared `is_two_mode` that disagrees with the data is a
      # mistake about the data, so the data wins and the user is told.
      if ("network" %in% .args_names) {
        arg_name <- if (length(x) > 1) x[[2]] else NULL
        is_two_mode <- ds_arg_is_two_mode(
          src,
          arg_name,
          eval(.signature[["network"]], envir = probe_envir)
        )
        if ("is_two_mode" %in% .args_names) {
          declared <- if (is.null(parms_to_set[["is_two_mode"]])) {
            NULL
          } else {
            eval(parms_to_set[["is_two_mode"]], envir = envir)
          }
          if (!is.null(declared) && !identical(declared, is_two_mode)) {
            warn_is_two_mode_mismatch(
              effect = as.character(x[[1]]),
              layer = arg_name,
              declared = declared,
              mode_pair = ds_layer_mode_pair(src, arg_name)
            )
          }
          .signature[["is_two_mode"]] <- is_two_mode
        }
        # The parser is the only place that sees which layer each argument
        # refers to, so the side signature is checked here rather than in the
        # effect's init -- including for the effects that carry no
        # `is_two_mode` formal at all.
        check_effect_sides(
          effect = as.character(x[[1]]),
          arg_name = arg_name,
          type = .signature[["type"]],
          src = src,
          model = model,
          sub_model = sub_model
        )
      } else if ("is_two_mode" %in% .args_names) {
        # An attribute-only effect has no network argument to resolve against,
        # and its positions are positions of the focal dyad -- so the focal
        # layer is the right source, and this stays a per-position fact rather
        # than a blanket: the flag says whether the sides this effect spans are
        # different node spaces. Without it these effects saw the hardcoded
        # FALSE and zeroed a diagonal that a two-mode statistic does not have.
        .signature[["is_two_mode"]] <- ds_model_is_two_mode(src)
      }
      formals(FUN) <- .signature
      return(list(effect = FUN, init_effect = .FUN_stat))
    },
    model,
    sub_model
  )
  structure(effects, class = "goldfishFormulae")
}

# The declared `is_two_mode` disagrees with what the argument's layer actually
# is. Name all three things the user needs to reconcile it: the effect, what
# they declared, and what the data says (as the layer's mode pair where the
# object carries modes).
warn_is_two_mode_mismatch <- function(effect, layer, declared, mode_pair) {
  actual <- if (is.null(mode_pair)) {
    NULL
  } else {
    c(
      "i" = "Layer {.val {layer}} sends from mode{?s} \\
             {.val {mode_pair$sender}} to mode{?s} {.val {mode_pair$receiver}}."
    )
  }
  cli::cli_warn(c(
    "{.arg is_two_mode} = {.val {declared}} in {.fn {effect}} disagrees with \\
     the data.",
    actual,
    "i" = "Using {.val {!declared}}, read from the layer's mode sets.",
    "i" = "Drop the argument to silence this: it is derived from the data."
  ))
}

create_windowed_events <- function(object_events, window) {
  dissolve_events <- object_events
  dissolve_events$time <- dissolve_events$time + window
  if ("increment" %in% names(object_events)) {
    dissolve_events$increment <- -dissolve_events$increment
  }
  if ("replace" %in% names(object_events)) {
    dissolve_events$replace <- 0
  }
  new_events <- rbind(object_events, dissolve_events)
  sort_order <- order(new_events$time)
  for (n in seq_along(names(new_events))) {
    name <- names(new_events)[n]
    new_events[[name]] <- new_events[[name]][sort_order]
  }
  return(new_events)
}

# Detect an explicit leading intercept (`1`) on the RHS.
# stats::terms() records the intercept in attr(., "intercept") (default 1 for
# every formula) and drops the literal `1` term, so it cannot distinguish an
# implicit intercept from an explicit one. goldfish's convention is the
# opposite of R's default: `has_intercept` is opt-in via an explicit leading
# `1` (see parse_intercept, which is order-sensitive). We therefore re-derive
# it by descending the leftmost operand of the `+`-tree and testing for a
# literal 1 -- the same term parse_intercept inspected under the old walker.
has_explicit_intercept <- function(rhs) {
  node <- rhs
  while (
    is.call(node) && length(node) == 3L && identical(node[[1]], as.name("+"))
  ) {
    node <- node[[2]]
  }
  is.numeric(node) && length(node) == 1L && node == 1
}

# stats::terms() admits constructs goldfish cannot dispatch (I(), |) as opaque
# term labels instead of rejecting them, so guard explicitly.
reject_unsupported_terms <- function(variables) {
  heads <- vapply(
    variables,
    function(e) if (is.call(e)) deparse(e[[1]]) else "",
    character(1)
  )
  bad <- heads %in% c("I", "|")
  if (any(bad)) {
    offenders <- vapply(variables[bad], deparse1, character(1))
    cli::cli_abort(c(
      "Unsupported term{?s} in the model formula: {.code {offenders}}.",
      "x" = "{.code I()} and {.code |} are not goldfish effects and cannot be
             dispatched to an effect function.",
      "i" = "Use goldfish effect functions and combine them with {.code +},
             {.code :}, or {.code *}."
    ))
  }
  invisible(NULL)
}

get_dependent_name <- function(formula) {
  dep <- list(formula[[2]])
  unlist(lapply(dep, deparse))
}

# Metadata half of the event-stream link: builds the
# events_objects_link incidence data.frame AND an ordered fetch plan describing
# each event stream to materialize (stream name + per-stream sanitize nodesets),
# reading only object attributes (event-stream NAMES, nodesets) — it fetches NO
# event tables and runs NO sanitizeEvents. The fetch plan rows are 1:1 with the
# incidence rows (and thus with the events list produced by fetch_events()), in
# the same order, so downstream indexing is unchanged.
build_events_objects_link <- function(
  dep_name,
  rhs_names,
  nodes = NULL,
  nodes2 = NULL,
  envir = environment(),
  derivations = NULL,
  data = NULL
) {
  src <- new_data_source(data = data, envir = envir)
  sanitize <- ds_needs_sanitize(src)
  object_names <- get_data_objects(rhs_names)
  dep_key <- ds_dependent_key(src, dep_name)
  events_objects_link <- data.frame(
    events = dep_key,
    name = NA,
    object = NA,
    nodeset = NA,
    attribute = NA,
    stringsAsFactors = FALSE
  )
  fetch_plan <- list(list(
    stream = dep_key,
    sanitize = sanitize,
    s_nodes = nodes,
    s_nodes2 = nodes2
  ))

  is_attribute <- is.na(object_names$object)
  for (i in which(is_attribute)) {
    node_set <- object_names[i, ]$nodeset
    attribute_name <- object_names[i, ]$attribute
    event_list_names <- ds_attribute_streams(src, node_set, attribute_name)
    if (length(event_list_names) == 0) {
      next
    }
    events_objects_link <- rbind(
      events_objects_link,
      cbind(events = event_list_names, object_names[i, ])
    )
    # A global attribute's events carry no node reference, so they are never
    # sanitized on either path.
    is_global <- ds_is_global(src, node_set)
    for (en in event_list_names) {
      fetch_plan <- c(
        fetch_plan,
        list(
          if (is_global) {
            list(stream = en, sanitize = FALSE)
          } else {
            list(
              stream = en,
              sanitize = sanitize,
              s_nodes = node_set,
              s_nodes2 = node_set
            )
          }
        )
      )
    }
  }
  for (i in which(!is_attribute)) {
    # A derived (windowed) network may not be realized yet on the recipe path
    # resolve its event-stream names and nodesets from
    # the source object + window instead of reading the absent derived object.
    # The realized derived network names its dissolve streams identically
    # (paste(source stream, window, sep = "_")) and copies the source nodesets,
    # so the incidence + fetch plan are unchanged.
    derivation <- find_derivation(object_names[i, ]$object, derivations)
    source_name <- if (!is.null(derivation)) {
      derivation$source_name
    } else {
      object_names[i, ]$object
    }
    ev_names <- ds_object_streams(src, source_name)
    if (!is.null(derivation) && length(ev_names) > 0) {
      ev_names <- paste(ev_names, derivation$window, sep = "_")
    }
    nodes_object <- ds_layer_sides(src, source_name)
    if (length(nodes_object) > 1) {
      net_nodes <- nodes_object[1]
      net_nodes2 <- nodes_object[2]
    } else {
      net_nodes <- net_nodes2 <- nodes_object
    }

    if (length(ev_names) > 0) {
      events_objects_link <- rbind(
        events_objects_link,
        cbind(events = ev_names, object_names[i, ], row.names = NULL)
      )
      for (en in ev_names) {
        fetch_plan <- c(
          fetch_plan,
          list(list(
            stream = en,
            sanitize = sanitize,
            s_nodes = net_nodes,
            s_nodes2 = net_nodes2
          ))
        )
      }
    }
  }

  list(events_objects_link = events_objects_link, fetch_plan = fetch_plan)
}

# Data half: materializes the events list from a fetch
# plan by fetching each stream's table through the data-resolution seam and
# running sanitizeEvents (label -> id) per its recorded nodesets. This is the
# data work relocated out of parsing; every stream the plan names must already
# be resolvable (including any windowed dissolve streams realized by the
# derivation realizer).
fetch_events <- function(fetch_plan, envir = environment(), src = NULL) {
  src <- src %||% new_data_source(envir = envir)
  events <- lapply(fetch_plan, function(p) {
    ev <- ds_fetch_stream(src, p$stream)
    if (isTRUE(p$sanitize)) {
      ev <- sanitizeEvents(ev, p$s_nodes, p$s_nodes2, envir = envir)
    }
    ev
  })
  names(events) <- vapply(fetch_plan, `[[`, character(1), "stream")
  events
}

get_events_and_objects_link <- function(
  dep_name,
  rhs_names,
  nodes = NULL,
  nodes2 = NULL,
  envir = environment(),
  derivations = NULL
) {
  link <- build_events_objects_link(
    dep_name,
    rhs_names,
    nodes,
    nodes2,
    envir = envir,
    derivations = derivations
  )
  events <- fetch_events(link$fetch_plan, envir = envir)
  list(events, link$events_objects_link)
}

# The events_effects_link shape (one row per event stream) is derived from the
# events_objects_link incidence (its `events` column lists the streams in fetch
# order) rather than a pre-fetched events list, so it needs no data: the
# recipe path fetches events only inside state creation.
get_events_effects_link <- function(rhs_names, events_objects_link) {
  stream_names <- events_objects_link$events
  events_effects_link <- matrix(
    data = NA,
    nrow = length(stream_names),
    ncol = length(rhs_names),
    dimnames = list(
      stream_names,
      vapply(rhs_names, FUN = "[[", FUN.VALUE = character(1), i = 1)
    )
  )
  for (i in seq_along(rhs_names)) {
    obj <- get_data_objects(rhs_names[i])$name
    event_ids <- which(events_objects_link$name %in% obj)
    events_effects_link[event_ids, i] <- 1
  }
  events_effects_link
}

get_objects_effects_link <- function(rhs_names) {
  obj_names <- get_data_objects(rhs_names)$name
  eff_names <- vapply(rhs_names, FUN = "[[", FUN.VALUE = character(1), i = 1)
  objects_effects_link <- matrix(
    data = NA,
    nrow = length(obj_names),
    ncol = length(eff_names),
    dimnames = list(obj_names, eff_names)
  )
  obj_as_params <- lapply(rhs_names, function(x) get_data_objects(list(x)))
  for (i in seq_along(obj_as_params)) {
    names_ <- obj_as_params[[i]]$name
    objects_effects_link[names_, i] <- seq_along(names_)
  }
  objects_effects_link
}

# Split an `offset()` term into the effect it wraps and the fixed coefficient
# value it may carry. Arguments are matched by name against `(object, coef)`, so
# `offset(coef = -1.2, inertia(net))` reads the same as the positional form.
# The value is evaluated rather than read as a literal because a negative number
# in a call is itself a call; it is evaluated in the formula's environment, the
# same place the term's own object references resolve from.
unwrap_offset_term <- function(term, envir) {
  matched <- tryCatch(
    match.call(function(object, coef) NULL, term),
    error = function(e) {
      cli::cli_abort(c(
        "{.fn offset} accepts a term and an optional {.arg coef} value.",
        "x" = "{.code {deparse1(term)}} has other arguments.",
        "i" = "Write {.code offset(term, coef = value)} to fix a coefficient
               in the formula."
      ))
    }
  )
  args <- as.list(matched)[-1]
  if (is.null(args$object)) {
    cli::cli_abort(c(
      "{.fn offset} needs a term to wrap.",
      "x" = "{.code {deparse1(term)}} wraps nothing."
    ))
  }
  value <- NA_real_
  if (!is.null(args$coef)) {
    value <- tryCatch(
      eval(args$coef, envir = envir),
      error = function(e) {
        cli::cli_abort(c(
          "The {.arg coef} value of {.code {deparse1(args$object)}} cannot be
           evaluated.",
          "x" = conditionMessage(e)
        ))
      }
    )
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      cli::cli_abort(c(
        "The {.arg coef} value of an {.fn offset} term must be a single finite
         number.",
        "x" = "{.code {deparse1(args$object)}} was given
               {.code {deparse1(args$coef)}}."
      ))
    }
  }
  list(term = args$object, coef = as.numeric(value))
}

get_rhs_names <- function(formula) {
  parsed <- stats::terms(formula, keep.order = TRUE)

  # attr "variables" is the call list(resp, term1, ...): each unique effect call
  # as a language object -- the same objects the old walker produced for
  # non-interaction formulas (verified term-for-term in discovery 0.3).
  variables <- as.list(attr(parsed, "variables"))[-1]
  response <- attr(parsed, "response")

  # `offset()` terms are recorded in attr "offset" as indices into the variables
  # list (1-based, response included). Unlike GLM model.matrix we KEEP the
  # statistic column and only TAG the term as fixed-coefficient: the
  # inner call is unwrapped and parsed like any effect, and `offset = TRUE` is
  # carried per term so the estimation front-end can assemble the
  # fixed-coefficient contract. The term may carry its own fixed value
  # (`offset(term, coef = -1.2)`), which is carried alongside the tag.
  offset_pos <- attr(parsed, "offset")
  if (response > 0) {
    variables <- variables[-response]
    offset_pos <- offset_pos - response
  }
  is_offset <- logical(length(variables))
  offset_coef <- rep(NA_real_, length(variables))
  if (length(offset_pos) > 0) {
    is_offset[offset_pos] <- TRUE
    formula_env <- environment(formula) %||% parent.frame()
    for (i in offset_pos) {
      unwrapped <- unwrap_offset_term(variables[[i]], formula_env)
      variables[[i]] <- unwrapped$term
      offset_coef[i] <- unwrapped$coef
    }
  }

  reject_unsupported_terms(variables)

  # Interaction terms (`a:b`, `a*b`) expand correctly via terms():
  # `a*b` -> a + b + a:b, and each `:` term references its operand *variables*
  # (read off the `factors` incidence). The unique operand/main variables become
  # the returned rhs terms (each parsed once, args preserved); the interaction
  # structure is carried on the `interactions` attribute (variable-index frame),
  # with per-variable `is_main` / `is_operand` roles. rhs_names for a
  # non-interaction formula is byte-identical to before.
  factors <- attr(parsed, "factors")
  term_order <- attr(parsed, "order")
  term_labels <- attr(parsed, "term.labels")
  n_vars <- length(variables)

  # With no non-offset term to record -- `~ 1`, `~ offset(x)`,
  # `~ 1 + offset(x)`, several offsets -- `terms()` returns `integer(0)`
  # rather than a 0-column
  # matrix, dropping the shape along with the columns, so `nrow()` below has
  # nothing to read. Restore it: one row per model variable, the response
  # included, and no columns. Everything downstream already tolerates zero
  # terms.
  if (!is.matrix(factors)) {
    factors <- matrix(0L, nrow = n_vars + (response > 0L), ncol = 0L)
  }

  # factors rows are 1:1 with attr "variables" (response included); map each
  # factors row back to its rhs-variable index (response dropped, offsets kept
  # in place — offsets have all-zero factor rows, so are never operands).
  row_to_rhs <- integer(nrow(factors))
  kept_rows <- if (response > 0) {
    seq_len(nrow(factors))[-response]
  } else {
    seq_len(nrow(factors))
  }
  row_to_rhs[kept_rows] <- seq_along(kept_rows)

  is_main <- logical(n_vars)
  is_operand <- logical(n_vars)
  interactions <- list()
  if (length(term_order) > 0) {
    for (cix in which(term_order == 1L)) {
      is_main[row_to_rhs[which(factors[, cix] != 0)]] <- TRUE
    }
    for (cix in which(term_order > 1L)) {
      operands <- row_to_rhs[which(factors[, cix] != 0)]
      is_operand[operands] <- TRUE
      interactions[[length(interactions) + 1L]] <- list(
        operands = operands,
        label = term_labels[cix],
        order = term_order[cix]
      )
    }
  }

  rhs_names <- lapply(variables, function(term) lapply(term, deparse))

  # terms() folds an explicit leading `1` into attr "intercept"; re-insert it as
  # the first term so parse_intercept() detects it exactly as before. The
  # intercept is never an offset / operand / interaction, so prepend FALSE to
  # keep the per-variable flags aligned; interaction operand indices are read in
  # the variable frame (after parse_intercept drops the `1` again).
  if (has_explicit_intercept(formula[[length(formula)]])) {
    rhs_names <- c(list(list("1")), rhs_names)
    is_offset <- c(FALSE, is_offset)
    offset_coef <- c(NA_real_, offset_coef)
    is_main <- c(FALSE, is_main)
    is_operand <- c(FALSE, is_operand)
  }

  attr(rhs_names, "offset") <- is_offset
  attr(rhs_names, "offset_coef") <- offset_coef
  attr(rhs_names, "is_main") <- is_main
  attr(rhs_names, "is_operand") <- is_operand
  attr(rhs_names, "interactions") <- interactions
  rhs_names
}

parse_intercept <- function(rhs_names) {
  intercept <- FALSE
  v <- NA
  tryCatch(
    v <- as.numeric(rhs_names[[1]][[1]]),
    warning = function(x) {}
  )
  if (!is.na(v) && v == 1) {
    intercept <- TRUE
    rhs_names <- rhs_names[-1]
  }
  return(list(rhs_names, intercept))
}

parse_multiple_effects <- function(
  rhs_names,
  default = FALSE,
  envir = environment(),
  derived_names = character(0),
  src = NULL
) {
  src <- src %||% new_data_source(envir = envir)
  multiple <- list()
  multiple_names <- character(0)
  for (i in seq_along(rhs_names)) {
    name <- ""
    id <- which(names(rhs_names[[i]]) == "ignore_repetitions")
    multiple_param <- ifelse(length(id) == 1, rhs_names[[i]][[id]], default)
    if (multiple_param %in% c("T", "F", "TRUE", "FALSE")) {
      multiple_param <- as.logical(multiple_param)
    }
    if (!multiple_param) {
      table <- get_data_objects(rhs_names[i])
      # A derived (windowed) network may not be realized yet on the recipe path;
      # it is always a network, so recognize it from the recipe without fetching.
      is_derived <- table$name %in% derived_names
      net_ids <- logical(nrow(table))
      net_ids[is_derived] <- TRUE
      if (any(!is_derived)) {
        net_ids[!is_derived] <- ds_table_is_network(
          src,
          table[!is_derived, , drop = FALSE]
        )
      }
      name <- table[net_ids, "name"][1]
    }
    if (is.character(multiple_param)) {
      name <- multiple_param
      multiple_param <- FALSE
    }
    # A derived (windowed) network is a valid object even before it is realized
    # on the recipe path, so it is exempt from the existence guard.
    if (
      !is.na(name) &&
        name != "" &&
        !(name %in% derived_names) &&
        !ds_object_exists(src, name)
    ) {
      stop(
        "Unknown object in 'ignore_repetitions' parameter: ",
        name,
        call. = FALSE
      )
    }
    multiple <- append(multiple, multiple_param)
    multiple_names <- c(multiple_names, name)
    rhs_names[[i]] <- if (length(id) > 0) {
      rhs_names[[i]][-id]
    } else {
      rhs_names[[i]]
    }
  }
  names(multiple) <- multiple_names
  return(list(rhs_names, multiple))
}

parse_time_windows <- function(
  rhs_names,
  envir = new.env(),
  realize_windows = TRUE
) {
  object_names <- get_data_objects(rhs_names)
  has_windows <- which(
    vapply(rhs_names, function(x) !is.null(getElement(x, "window")), logical(1))
  )

  # Phase 1: parse/validate window values and collect attribute violations.
  # Format errors on individual window values abort immediately (unambiguous).
  # Attribute+window violations are collected and reported together.
  windows_parsed <- vector("list", length(has_windows))
  violations <- character(0)

  for (idx in seq_along(has_windows)) {
    i <- has_windows[idx]
    window_name <- rhs_names[[i]]$window
    window <- tryCatch(
      eval(parse(text = window_name), envir = envir),
      error = function(e) {
        e$message <- paste(
          "Invalid window parameter for effect ",
          rhs_names[[i]][[1]],
          " ",
          rhs_names[[i]][[2]],
          ":\n",
          e$message
        )
        stop(e)
      }
    )
    is_valid_name <- grepl("^[[:alpha:]][[:alnum:]_.]+$", window_name)
    if (
      inherits(window, c("Period", "Duration")) &&
        "lubridate" %in% attr(attr(window, "class"), "package")
    ) {
      if (!is_valid_name) {
        window_name <- gsub("\\s", "", as.character(window))
        if (inherits(window, "Duration")) {
          window_name <- gsub(
            "^(\\d+s)\\s*(\\(.+\\))$",
            "\\1",
            as.character(window)
          )
        }
      }
    } else if (inherits(window, "character")) {
      if (!is_valid_name) {
        window_name <- gsub(" ", "", window)
      }
      if (
        !is.numeric(window) &&
          !grepl("^\\d+ (sec|min|hour|day|week|month|year)", window)
      ) {
        stop(
          "The window effect specified with the effect ",
          rhs_names[[i]][[1]],
          " ",
          rhs_names[[i]][[2]],
          " is not in the form 'number unit'\n",
          " or the number is not an integer number\n",
          " or the unit is not between the accepted options:\n\t",
          "seconds, minutes, hours, weeks, months, years"
        )
      }
      if (grepl("sec", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 1
      }
      if (grepl("min", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 60
      }
      if (grepl("hour", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 3600
      }
      if (grepl("day", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 86400
      }
      if (grepl("week", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 604800
      }
      if (grepl("month", window)) {
        # lubridate approximation
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 2629800
      }
      if (grepl("year", window)) {
        # lubridate approximation
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 31557600
      }
    } else if (is.numeric(window)) {
      # check numeric type
      if (window < 0) {
        stop(
          "The window specified with the effect ",
          rhs_names[[i]][[1]],
          " ",
          rhs_names[[i]][[2]],
          " is not a positive numeric value"
        )
      }
    }
    windows_parsed[[idx]] <- list(window = window, window_name = window_name)

    name <- rhs_names[[i]][[2]]
    effect_name <- rhs_names[[i]][[1]]
    if (grepl("^list\\(", name)) {
      inner_names <- trimws(
        strsplit(gsub("^list\\((.+)\\)$", "\\1", name), ",")[[1]]
      )
      attr_inners <- inner_names[grepl("\\$", inner_names)]
      for (attr_ref in attr_inners) {
        msg <- paste0(
          "Effect {.code ",
          effect_name,
          "} on {.code ",
          attr_ref,
          "} must not have a {.arg window} argument,",
          " remove it from the formula."
        )
        violations <- c(violations, c(x = msg))
      }
    } else {
      objects <- object_names[object_names$name == name, ]
      if (nrow(objects) > 0 && !is.na(objects$attribute[1])) {
        msg <- paste0(
          "Effect {.code ",
          effect_name,
          "} on {.code ",
          name,
          "} must not have a {.arg window} argument,",
          " remove it from the formula."
        )
        violations <- c(violations, c(x = msg))
      }
    }
  }

  if (length(violations) > 0) {
    cli::cli_abort(c(
      "{.arg window} is not supported for attribute effects.",
      violations,
      "i" = "Remove the {.arg window} argument from the listed effects in the formula."
    ))
  }

  # Phase 2: rewrite each windowed effect's object reference to the derived
  # windowed-network name and record a derivation recipe (metadata).
  # Realizing the derived network + dissolve-event streams into `envir` is gated
  # on `realize_windows`: the DyNAMi and legacy paths realize eagerly here so
  # behaviour is byte-identical; the recipe (DyNAM/REM) path passes FALSE and
  # defers realization to state creation, driven by plan$derivations. Only
  # reached when no attribute+window violations were found in Phase 1.
  derivations <- list()
  for (idx in seq_along(has_windows)) {
    i <- has_windows[idx]
    window <- windows_parsed[[idx]]$window
    window_name <- windows_parsed[[idx]]$window_name
    name <- rhs_names[[i]][[2]]

    if (grepl("^list\\(", name)) {
      inner_names <- trimws(
        strsplit(gsub("^list\\((.+)\\)$", "\\1", name), ",")[[1]]
      )
      new_inner_names <- character(length(inner_names))
      for (j in seq_along(inner_names)) {
        net_name <- inner_names[j]
        new_net_name <- paste(net_name, window_name, sep = "_")
        derivations[[length(derivations) + 1L]] <- list(
          derived_name = new_net_name,
          source_name = net_name,
          window = window,
          kind = "window"
        )
        if (realize_windows) {
          realize_windowed_network(net_name, new_net_name, window, envir)
        }
        new_inner_names[j] <- new_net_name
      }
      rhs_names[[i]][[2]] <-
        paste0("list(", paste(new_inner_names, collapse = ", "), ")")
    } else {
      new_name <- paste(name, window_name, sep = "_")
      derivations[[length(derivations) + 1L]] <- list(
        derived_name = new_name,
        source_name = name,
        window = window,
        kind = "window"
      )
      if (realize_windows) {
        realize_windowed_network(name, new_name, window, envir)
      }
      rhs_names[[i]][[2]] <- new_name
    }
  }
  attr(rhs_names, "window_derivations") <- derivations
  return(rhs_names)
}

# Realize a windowed-network derivation into `envir`. Builds
# an empty network with the source network's structure (dims, nodes, directed,
# class) and, for each source event stream, a windowed dissolve-event table
# (`create_windowed_events`) named `<stream>_<window>`; the derived network's
# `events` attribute lists those streams. Both the derived network and its
# dissolve streams are `assign()`ed into `envir`. Called eagerly by
# parse_time_windows() (legacy/DyNAMi) and by the recipe state-creation realizer.
realize_windowed_network <- function(source_name, derived_name, window, envir) {
  network <- get(source_name, envir = envir)
  new_network <- matrix(0, nrow = nrow(network), ncol = ncol(network))
  attr(new_network, "events") <- NULL
  attr(new_network, "nodes") <- attr(network, "nodes")
  attr(new_network, "directed") <- attr(network, "directed")
  dimnames(new_network) <- dimnames(network)
  class(new_network) <- class(network)
  all_events <- attr(network, "events")
  for (events in all_events) {
    object_events <- get(events, envir = envir)
    new_events <- create_windowed_events(object_events, window)
    name_new_events <- paste(events, window, sep = "_")
    attr(new_network, "events") <-
      c(attr(new_network, "events"), name_new_events)
    assign(name_new_events, new_events, envir = envir)
  }
  assign(derived_name, new_network, envir = envir)
}

# Registry-driven realizer: materialize every window
# derivation recorded by parse_time_windows() into `envir`. On the recipe path
# parse_time_windows() runs with realize_windows = FALSE (leaving the shared
# parser free of environment mutations); the recipe front-end calls this once
# from the recorded recipe so the eager assign() is driven by the derivation
# registry at a single controlled point instead of interleaved with parsing.
realize_windows_recipe <- function(window_derivations, envir) {
  for (d in window_derivations) {
    if (identical(d$kind, "window")) {
      realize_windowed_network(d$source_name, d$derived_name, d$window, envir)
    }
  }
  invisible(NULL)
}

# State-creation realizer driven by the `plan$derivations`
# registry (not the raw parse recipe): materialize each derived input into the
# recipe state container's environment before the loop reads it. Dispatches on
# `kind`; the only kind today is "window" (empty net + windowed dissolve
# streams). Idempotent — re-realizing from the unchanged source rebuilds an
# identical derived object.
realize_derivations <- function(derivations, envir) {
  for (d in derivations) {
    if (identical(d$kind, "window")) {
      realize_windowed_network(d$source, d$derived_name, d$params$window, envir)
    }
  }
  invisible(NULL)
}
