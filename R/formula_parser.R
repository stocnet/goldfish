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
#' callNetwork <- structure(matrix(0, 3, 3), class = "network.goldfish")
#'
#' parse_formula(
#'   calls ~ outdeg(callNetwork, type = "ego") +
#'     indeg(callNetwork, type = "alter")
#' )
#' }
parse_formula <- function(formula, envir = new.env(), realize_windows = TRUE) {
  dep_name <- get_dependent_name(formula)
  if (!inherits(get(dep_name, envir = envir), "dependent.goldfish")) {
    stop(
      "The left hand side of the formula should contain dependent events",
      " (check the function 'make_dependent_events()').",
      call. = FALSE
    )
  }
  rhs_names <- get_rhs_names(formula)
  if (length(rhs_names) == 0) {
    stop("A model without effects cannot be estimated.", call. = FALSE)
  }
  # Per-term flags from get_rhs_names: offset (fixed-coefficient, design D7) and
  # the interaction roles is_main / is_operand + the interaction structure
  # (design D9). All are kept aligned with rhs_names through the intercept drop
  # below; interaction operand indices are in the variable frame (i.e. the
  # rhs_names frame after the `1` is dropped).
  is_offset <- attr(rhs_names, "offset")
  if (is.null(is_offset)) {
    is_offset <- logical(length(rhs_names))
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
  attr(rhs_names, "is_main") <- NULL
  attr(rhs_names, "is_operand") <- NULL
  attr(rhs_names, "interactions") <- NULL
  int <- parse_intercept(rhs_names)
  rhs_names <- int[[1]]
  has_intercept <- int[[2]]
  if (has_intercept) {
    is_offset <- is_offset[-1]
    is_main <- is_main[-1]
    is_operand <- is_operand[-1]
  }
  default_network_name <- attr(get(dep_name, envir = envir), "default_network")
  if (!is.null(default_network_name)) {
    no_object_ids <- which(1 == vapply(rhs_names, length, integer(1)))
    for (i in no_object_ids) {
      rhs_names[[i]][[2]] <- default_network_name
    }
  }

  window_parameters <- lapply(rhs_names, getElement, "window")
  rhs_names <- parse_time_windows(
    rhs_names,
    envir = envir,
    realize_windows = realize_windows
  )
  # Metadata recipe for the windowed derivations (design D8): each windowed
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
    derived_names = derived_names
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
    v <- getElement(x, "subType")
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
    # Interaction roles (design D9): an operand-only term is retained but not
    # freely estimated; a requested main effect (and an offset, fixed via
    # `fixedParameters`) is an estimated column. `interactions` lists each
    # interaction's ordered operand indices (rhs_names frame), label, and arity.
    is_main_parameter = as.list(is_main),
    is_operand_parameter = as.list(is_operand),
    estimate_parameter = as.list(is_main | is_offset),
    interactions = interactions,
    window_derivations = window_derivations
  )
  return(res)
}


# Comparison of two parsed formulas for preprocessingInit
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
      " the preprocessed object given in preprocessingInit."
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
      " the preprocessed object given in preprocessingInit."
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
      "The preprocessing for the object in preprocessingInit was not done",
      " with the right-censored intervals that this formula requires."
    )
  }
  if (!old_has_intercept && new_has_intercept) {
    stop(
      "The preprocessing for the object in preprocessingInit was done",
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
        "interactions"
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
#' of parsing (design D8), so `preprocess()` consumes them instead of rebuilding
#' the update plan and call templates on every call. It orchestrates the trio:
#' the `parsed_terms` bundle (from `parse_formula()`), the per-effect call
#' templates (`build_effects_template()`), and the registries-only update plan
#' (`build_update_plan()`).
#'
#' The `spec_map` merges the dispatch `model_spec` (its fields and class vector)
#' so `preprocess()` dispatches directly on the returned object (task 2.3b): the
#' effect closures, per-term window parameters, and the link matrices ride on it
#' rather than being threaded into `preprocess()` as separate bridge arguments.
#' The recipe loops unpack them from the `spec` they receive.
#'
#' Stage 1 scope: this still builds a throwaway state container to read object
#' keys, so it is not yet metadata-pure. The full metadata/data boundary —
#' keeping this mapping free of event/network tables and relocating
#' `sanitizeEvents`/windowing to state creation — is design D8's later slice
#' (tasks 2.3c–2.3d).
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
#'   fetches events inside state creation (design D8, task 2.3f).
#' @param envir environment where the data objects live.
#'
#' @return an S3 object of class `c(class(model_spec), "spec_map.goldfish")`
#'   carrying the `model_spec` fields plus `parsed_terms`, `plan`,
#'   `effects_template`, `effect_description` (the single source of truth for
#'   print/naming metadata, design D8), and the `effects`/`window_parameters`/
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
  envir = new.env()
) {
  stat_kind <- if (inherits(model_spec, "sender_spec")) "sender" else "dyad"
  nodes <- model_spec$nodes
  nodes2 <- model_spec$nodes2
  # Metadata/data boundary (design D8, task 2.3c): the plan + call templates need
  # only the object-keys mapping, so read it directly (class/structure only) —
  # do NOT materialise a state container here (no network/nodal data copied).
  object_keys <- build_object_keys(
    rownames(objects_effects_link),
    nodes,
    nodes2,
    envir = envir,
    derivations = parsed_formula$window_derivations
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
    derivations = parsed_formula$window_derivations
  )
  # Interaction columns (design D9): append one estimated product column per
  # interaction and set operand roles / estimate flags. No-op when the formula
  # has no interactions (registries stay empty, effects unchanged).
  plan <- augment_interactions(plan, parsed_formula, stat_kind)
  # Derived-input registry (design D8, task 2.3d): one entry per derived object,
  # filled from metadata only. Today the sole `kind` is "window"; the effect
  # object refs were already rewired to `derived_name` in parse_time_windows().
  # State creation iterates this to realize each derived object (no tables or
  # assign happen here).
  plan$derivations <- build_derivations(
    parsed_formula$window_derivations,
    objects_effects_link,
    envir = envir
  )
  # A supplied support_constraint compiles into a sibling sub-plan (its atoms'
  # closures/links/update-plan) plus a `kind = "support_mask"` derivation; the
  # recipe loop realizes/updates the mask from the atoms' stat_state. When no
  # constraint is present this is a pure no-op, so unconstrained models stay
  # bit-identical.
  if (!is.null(support_constraint)) {
    plan$support_constraint <- compile_support_constraint(
      support_constraint,
      model = model_spec$model,
      sub_model = model_spec$sub_model,
      stat_kind = stat_kind,
      dep_name = parsed_formula$dep_name,
      nodes = nodes,
      nodes2 = nodes2,
      window_derivations = parsed_formula$window_derivations,
      envir = envir
    )
    plan$derivations <- c(
      plan$derivations,
      list(support_mask_derivation(plan$support_constraint))
    )
  }
  effects_template <- build_effects_template(
    effects,
    objects_effects_link,
    state_keys
  )
  # Print/naming metadata is formula-derived, so it is owned here once (design
  # D8) and rendered on demand per context (console / db / export) by
  # CreateNames; the fixed-coefficient marking stays an estimation concern
  # appended downstream (offset terms, group 5).
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
        fetch_plan = fetch_plan
      )
    ),
    class = c(class(model_spec), "spec_map.goldfish")
  )
}

# Build the derived-input registry (design D8) from the window derivation recipe
# recorded by parse_time_windows(). Metadata only: reads the source objects'
# event-stream NAMES (`attr(obj, "events")`) and the effect columns that
# reference each derived object (already rewired to `derived_name` in
# objects_effects_link) — it fetches no event/network tables and assigns
# nothing. Returns a list with one entry per derived object
# `{derived_name, kind, source, source_streams, params, gids}`, or `NULL` when
# there are no derivations, which state creation iterates to realize each
# derived object into the state container.
build_derivations <- function(
  window_derivations,
  objects_effects_link,
  envir = new.env()
) {
  if (length(window_derivations) == 0) {
    return(NULL)
  }
  lapply(window_derivations, function(d) {
    source_streams <- attr(get(d$source_name, envir = envir), "events")
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
# the estimated columns and the 1e-6 baselines bit-identical). The atom kernel
# maps onto the three existing init kernels: the sender kernel materialises
# sender-axis atoms for a rate-only spec, the choice/REM kernel materialises
# dyadic atoms; the coordination/undirected symmetry is applied later at mask
# assembly, not at atom-compute time. Returns the augmented sub-plan plus the
# atom closures / link metadata / fetch plan the recipe loop needs to seed and
# update the atoms' `stat_state`.
compile_support_constraint <- function(
  constraint_plan,
  model,
  sub_model,
  stat_kind,
  dep_name,
  nodes,
  nodes2,
  window_derivations,
  envir
) {
  atom_rhs_names <- get_rhs_names(atoms_to_formula(constraint_plan$atoms))
  atom_sub_model <- if (model == "REM") {
    "choice"
  } else if (stat_kind == "sender") {
    "rate"
  } else {
    "choice"
  }
  effects <- create_effects_functions(
    atom_rhs_names,
    model,
    atom_sub_model,
    envir = envir,
    derivations = window_derivations
  )
  objects_effects_link <- get_objects_effects_link(atom_rhs_names)
  link <- build_events_objects_link(
    dep_name,
    atom_rhs_names,
    nodes,
    nodes2,
    envir = envir,
    derivations = window_derivations
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
    derivations = window_derivations
  )
  state_keys <- structure(list(), object_keys = object_keys)
  sub_plan <- build_update_plan(
    effects,
    link$events_objects_link,
    events_effects_link,
    objects_effects_link,
    state_keys,
    stat_kind = stat_kind,
    envir = envir,
    derivations = window_derivations
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
      atom_kinds = constraint_plan$atom_kinds,
      events_objects_link = link$events_objects_link,
      events_effects_link = events_effects_link,
      objects_effects_link = objects_effects_link,
      fetch_plan = link$fetch_plan
    )
  )
}

# The support mask is a derived object (design D7): one `plan$derivations` entry,
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

# Shared derived -> source resolver (design D8, task 2.3f). A derived object
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
  derivations = NULL
) {
  .stat_method <- paste("init", model, sub_model, sep = "_")
  # Two-mode guard (below) evaluates the effect's network argument to read its
  # nodesets. On the recipe path a windowed effect's network arg is the *derived*
  # name, which may not be realized yet (design D8, task 2.3f); resolve it against
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
      if ("network" %in% .args_names && "is_two_mode" %in% .args_names) {
        is_two_mode <- length(attr(
          eval(.signature[["network"]], envir = probe_envir),
          "nodes"
        )) >
          1
        if (
          !is.null(parms_to_set[["is_two_mode"]]) &&
            eval(parms_to_set[["is_two_mode"]], envir = envir) != is_two_mode
        ) {
          warning(
            "The \"is_two_mode\" parameter in effect ",
            x[[1]],
            " has a different value than",
            " the attributes on network argument '",
            x[[2]],
            "'",
            call. = FALSE,
            immediate. = TRUE
          )
        } else if (is_two_mode && is.null(parms_to_set[["is_two_mode"]])) {
          .signature[["is_two_mode"]] <- is_two_mode
          warning(
            "Setting 'is_two_mode' parameter in effect ",
            x[[1]],
            " to TRUE for network '",
            x[[2]],
            "'",
            call. = FALSE,
            immediate. = TRUE
          )
        }
      }
      formals(FUN) <- .signature
      return(list(effect = FUN, initEffect = .FUN_stat))
    },
    model,
    sub_model
  )
  structure(effects, class = "goldfish.formulae")
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
# term labels instead of rejecting them, so guard explicitly (design D2).
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

# Metadata half of the event-stream link (design D8, task 2.3c): builds the
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
  derivations = NULL
) {
  object_names <- getDataObjects(rhs_names)
  events_objects_link <- data.frame(
    events = dep_name,
    name = NA,
    object = NA,
    nodeset = NA,
    attribute = NA,
    stringsAsFactors = FALSE
  )
  fetch_plan <- list(list(
    stream = dep_name,
    sanitize = TRUE,
    s_nodes = nodes,
    s_nodes2 = nodes2
  ))

  is_attribute <- is.na(object_names$object)
  for (i in which(is_attribute)) {
    nodeset_obj <- get(object_names[i, ]$nodeset, envir = envir)
    node_set <- object_names[i, ]$nodeset
    attribute_name <- object_names[i, ]$attribute

    if (inherits(nodeset_obj, "global.goldfish")) {
      event_list_names <- attr(nodeset_obj, "events")
      if (length(event_list_names) > 0) {
        events_objects_link <- rbind(
          events_objects_link,
          cbind(events = event_list_names, object_names[i, ])
        )
        for (en in event_list_names) {
          fetch_plan <- c(fetch_plan, list(list(stream = en, sanitize = FALSE)))
        }
      }
      next
    }

    dynamic_attributes <- attr(nodeset_obj, "dynamic_attributes")
    event_list_names <- attr(nodeset_obj, "events")
    ev_name <- event_list_names[which(dynamic_attributes == attribute_name)]
    if (length(ev_name) > 0) {
      events_objects_link <- rbind(
        events_objects_link,
        cbind(events = ev_name, object_names[i, ])
      )
      for (en in ev_name) {
        fetch_plan <- c(
          fetch_plan,
          list(list(
            stream = en,
            sanitize = TRUE,
            s_nodes = node_set,
            s_nodes2 = node_set
          ))
        )
      }
    }
  }
  for (i in which(!is_attribute)) {
    # A derived (windowed) network may not be realized yet on the recipe path
    # (design D8, task 2.3f): resolve its event-stream names and nodesets from
    # the source object + window instead of get()-ing the absent derived object.
    # The realized derived network names its dissolve streams identically
    # (paste(source stream, window, sep = "_")) and copies the source nodesets,
    # so the incidence + fetch plan are unchanged.
    derivation <- find_derivation(object_names[i, ]$object, derivations)
    if (!is.null(derivation)) {
      source_object <- get(derivation$source_name, envir = envir)
      ev_names <- paste(
        attr(source_object, "events"),
        derivation$window,
        sep = "_"
      )
      nodes_object <- attr(source_object, "nodes")
    } else {
      ev_names <- attr(get(object_names[i, ]$object, envir = envir), "events")
      nodes_object <- attr(
        get(object_names[i, ]$object, envir = envir),
        "nodes"
      )
    }
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
            sanitize = TRUE,
            s_nodes = net_nodes,
            s_nodes2 = net_nodes2
          ))
        )
      }
    }
  }

  list(events_objects_link = events_objects_link, fetch_plan = fetch_plan)
}

# Data half (design D8, task 2.3c): materializes the events list from a fetch
# plan by fetching each stream's table and running sanitizeEvents (label -> id)
# per its recorded nodesets. This is the data work relocated out of parsing; the
# streams it fetches must already exist in `envir` (including any windowed
# dissolve streams realized by the derivation realizer).
fetch_events <- function(fetch_plan, envir = environment()) {
  events <- lapply(fetch_plan, function(p) {
    ev <- get(p$stream, envir = envir)
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
# order) rather than a pre-fetched events list, so it needs no data (design D8,
# task 2.3f): the recipe path fetches events only inside state creation.
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
    obj <- getDataObjects(rhs_names[i])$name
    event_ids <- which(events_objects_link$name %in% obj)
    events_effects_link[event_ids, i] <- 1
  }
  events_effects_link
}

get_objects_effects_link <- function(rhs_names) {
  obj_names <- getDataObjects(rhs_names)$name
  eff_names <- vapply(rhs_names, FUN = "[[", FUN.VALUE = character(1), i = 1)
  objects_effects_link <- matrix(
    data = NA,
    nrow = length(obj_names),
    ncol = length(eff_names),
    dimnames = list(obj_names, eff_names)
  )
  obj_as_params <- lapply(rhs_names, function(x) getDataObjects(list(x)))
  for (i in seq_along(obj_as_params)) {
    names_ <- obj_as_params[[i]]$name
    objects_effects_link[names_, i] <- seq_along(names_)
  }
  objects_effects_link
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
  # statistic column and only TAG the term as fixed-coefficient (design D7): the
  # inner call is unwrapped and parsed like any effect, and `offset = TRUE` is
  # carried per term so the estimation front-end can assemble `fixedParameters`.
  offset_pos <- attr(parsed, "offset")
  if (response > 0) {
    variables <- variables[-response]
    offset_pos <- offset_pos - response
  }
  is_offset <- logical(length(variables))
  if (length(offset_pos) > 0) {
    is_offset[offset_pos] <- TRUE
    for (i in offset_pos) {
      variables[[i]] <- variables[[i]][[2]]
    }
  }

  reject_unsupported_terms(variables)

  # Interaction terms (`a:b`, `a*b`) expand correctly via terms() (design D2):
  # `a*b` -> a + b + a:b, and each `:` term references its operand *variables*
  # (read off the `factors` incidence). The unique operand/main variables become
  # the returned rhs terms (each parsed once, args preserved); the interaction
  # structure is carried on the `interactions` attribute (variable-index frame),
  # with per-variable `is_main` / `is_operand` roles (design D9). rhs_names for a
  # non-interaction formula is byte-identical to before.
  factors <- attr(parsed, "factors")
  term_order <- attr(parsed, "order")
  term_labels <- attr(parsed, "term.labels")
  n_vars <- length(variables)

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
    is_main <- c(FALSE, is_main)
    is_operand <- c(FALSE, is_operand)
  }

  attr(rhs_names, "offset") <- is_offset
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
  derived_names = character(0)
) {
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
      table <- getDataObjects(rhs_names[i])
      # A derived (windowed) network may not be realized yet on the recipe path;
      # it is always a network, so recognize it from the recipe without fetching.
      is_derived <- table$name %in% derived_names
      net_ids <- logical(nrow(table))
      net_ids[is_derived] <- TRUE
      if (any(!is_derived)) {
        net_ids[!is_derived] <- vapply(
          getElementFromDataObjectTable(
            table[!is_derived, , drop = FALSE],
            envir = envir
          ),
          FUN = inherits,
          FUN.VALUE = logical(1),
          what = "network.goldfish"
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
        !exists(name, envir = envir)
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
  object_names <- getDataObjects(rhs_names)
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
  # windowed-network name and record a derivation recipe (metadata, design D8).
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

# Realize a windowed-network derivation into `envir` (design D8 realizer). Builds
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

# Registry-driven realizer (design D8, task 2.3d): materialize every window
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

# State-creation realizer (design D8, task 2.3f) driven by the `plan$derivations`
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
