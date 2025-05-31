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
parse_formula <- function(formula, envir = new.env()) {
  dep_name <- get_dependent_name(formula)
  if (!inherits(get(dep_name, envir = envir), "dependent.goldfish")) {
    stop("The left hand side of the formula should contain dependent events",
      " (check the function 'make_dependent_events()').",
      call. = FALSE
    )
  }
  rhs_names <- get_rhs_names(formula)
  if (length(rhs_names) == 0) {
    stop("A model without effects cannot be estimated.", call. = FALSE)
  }
  int <- parse_intercept(rhs_names)
  rhs_names <- int[[1]]
  has_intercept <- int[[2]]
  default_network_name <- attr(get(dep_name, envir = envir), "default_network")
  if (!is.null(default_network_name)) {
    no_object_ids <- which(1 == vapply(rhs_names, length, integer(1)))
    for (i in no_object_ids) {
      rhs_names[[i]][[2]] <- default_network_name
    }
  }
  
  window_parameters <- lapply(rhs_names, getElement, "window")
  rhs_names <- parse_time_windows(rhs_names, envir = envir)
  mult <- parse_multiple_effects(rhs_names, envir = envir)
  rhs_names <- mult[[1]]
  ignore_rep_parameter <- mult[[2]]
  if (any(unlist(ignore_rep_parameter)) && is.null(default_network_name)) {
    stop("No default network defined, thus ", dQuote("ignoreRep = TRUE"),
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
      "userDefined", v
    ) # if it is a function, it is replace by short text
  }
  trans_parameter <- lapply(rhs_names, get_fun_name, "transformFun")
  aggre_parameter <- lapply(rhs_names, get_fun_name, "aggregateFun")
  joining_parameter <- lapply(rhs_names, function(x) {
    v <- getElement(x, "joining")
    ifelse(!is.null(v), v, "")
  })
  sub_type_parameter <- lapply(rhs_names, function(x) {
    v <- getElement(x, "subType")
    ifelse(!is.null(v), v, "")
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
    aggre_parameter = aggre_parameter,
    joining_parameter = joining_parameter,
    sub_type_parameter = sub_type_parameter
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
    old_parsed_formula, new_parsed_formula, model, sub_model) {
  # test dependent events and default network
  if (old_parsed_formula$dep_name != new_parsed_formula$dep_name) {
    stop(
      "The dependent events in the formula are not the ones used in",
      " the preprocessed object given in preprocessingInit."
    )
  }
  if (!identical(
    old_parsed_formula$default_network_name,
    new_parsed_formula$default_network_name
  )) {
    stop(
      "The default network in the formula is not the one used in",
      " the preprocessed object given in preprocessingInit."
    )
  }
  old_has_intercept <- old_parsed_formula$has_intercept
  new_has_intercept <- new_parsed_formula$has_intercept
  if (model %in% "DyNAM" && sub_model %in% c("choice", "choice_coordination") &&
    old_has_intercept) {
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
  for (i in seq.int(size_new)) {
    effect_name <- new_parsed_formula$rhs_names[[i]][[1]]
    effect_object <- new_parsed_formula$rhs_names[[i]][[2]]
    effect_window <- new_parsed_formula$window_parameters[[i]]
    effect_ignore_rep <- new_parsed_formula$ignore_rep_parameter[[i]]
    effect_weighted <- new_parsed_formula$weighted_parameter[[i]]
    effect_parameter <- new_parsed_formula$user_set_parameter[[i]]
    for (j in seq.int(size_old)) {
      if (!identical(old_parsed_formula$rhs_names[[j]][[1]], effect_name)) {
        next
      }
      if (!identical(old_parsed_formula$rhs_names[[j]][[2]], effect_object)) {
        next
      }
      if (!identical(old_parsed_formula$window_parameters[[j]], effect_window)) {
        next
      }
      if (!identical(
        old_parsed_formula$ignore_rep_parameter[[j]], effect_ignore_rep
      )) {
        next
      }
      if (!identical(old_parsed_formula$weighted_parameter[[j]], effect_weighted)) {
        next
      }
      if (!identical(old_parsed_formula$user_set_parameter[[j]], effect_parameter)) {
        next
      }
      effects_indexes[i] <- j
    }
  }
  return(effects_indexes)
}

create_effects_functions <- function(effect_init, model, sub_model,
                                   envir = environment()) {
  .stat_method <- paste("init", model, sub_model, sep = "_")
  effects <- lapply(
    effect_init, function(x, model, sub_model) {
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
      .FUN_stat <- utils::getS3method(.stat_method, x[[1]],
        optional = TRUE,
        envir = environment()
      )
      if (is.null(.FUN_stat)) {
        .FUN_stat <- utils::getS3method(.stat_method, "default",
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
      parms_to_set <- lapply(parms_to_set, function(s) call("eval", parse(text = s)))
      .args_replace <- pmatch(names(parms_to_set), .args_names)
      names_ <- names(parms_to_set)[named_params]
      .signature[na.omit(.args_replace)] <- parms_to_set[!is.na(.args_replace)]
      is_condition <- isReservedElementName(.args_names) &
        !(.args_names %in% names_)
      .signature[is_condition] <- parms_to_set[!named_params]
      if ("network" %in% .args_names && "isTwoMode" %in% .args_names) {
        is_two_mode <- length(attr(
          eval(.signature[["network"]], envir = envir), "nodes"
        )) > 1
        if (!is.null(parms_to_set[["isTwoMode"]]) &&
          eval(parms_to_set[["isTwoMode"]], envir = envir) != is_two_mode) {
          warning(
            "The \"isTwoMode\" parameter in effect ",
            x[[1]], " has a different value than",
            " the attributes on network argument '",
            x[[2]], "'",
            call. = FALSE, immediate. = TRUE
          )
        } else if (is_two_mode && is.null(parms_to_set[["isTwoMode"]])) {
          .signature[["isTwoMode"]] <- is_two_mode
          warning(
            "Setting 'isTwoMode' parameter in effect ", x[[1]],
            " to TRUE for network '", x[[2]], "'",
            call. = FALSE, immediate. = TRUE
          )
        }
      }
      formals(FUN) <- .signature
      return(list(effect = FUN, initEffect = .FUN_stat))
    },
    model, sub_model
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

extract_formula_terms <- function(rhs) {
  if (is.symbol(rhs)) {
    return(rhs)
  }
  if (!is.call(rhs[[1]]) &&
    rhs[[1]] != "+" &&
    rhs[[1]] != "*") {
    return(rhs)
  } else {
    return(c(extract_formula_terms(rhs[[2]]), rhs[[3]]))
  }
}

get_dependent_name <- function(formula) {
  dep <- list(formula[[2]])
  unlist(lapply(dep, deparse))
}

get_events_and_objects_link <- function(
    dep_name, rhs_names, nodes = NULL, nodes2 = NULL,
    envir = environment()) {
  object_names <- getDataObjects(rhs_names)
  events <- list()
  events[[1]] <- get(dep_name, envir = envir)
  names(events) <- dep_name
  events_objects_link <- data.frame(
    events = dep_name,
    name = NA,
    object = NA,
    nodeset = NA,
    attribute = NA, stringsAsFactors = FALSE
  )

  # replace dependent labels with ids
  events[[1]] <- sanitizeEvents(events[[1]], nodes, nodes2, envir = envir)
  is_attribute <- is.na(object_names$object)
  for (i in which(is_attribute)) {
    node_set <- object_names[i, ]$nodeset
    attribute_name <- object_names[i, ]$attribute
    dynamic_attributes <- attr(
      get(object_names[i, ]$nodeset, envir = envir),
      "dynamic_attributes"
    )
    event_list_names <- attr(
      get(object_names[i, ]$nodeset, envir = envir),
      "events"
    )
    ev_name <- event_list_names[which(dynamic_attributes == attribute_name)]
    if (length(ev_name) > 0) {
      events_objects_link <- rbind(
        events_objects_link,
        cbind(events = ev_name, object_names[i, ])
      )
      evs <- lapply(
        ev_name,
        function(x) {
          sanitizeEvents(get(x, envir = envir), node_set, envir = envir)
        }
      )

      events <- append(events, evs)
      names(events)[(length(events) - length(ev_name) + 1):length(events)] <-
        ev_name
    }
  }
  for (i in which(!is_attribute)) {
    ev_names <- attr(get(object_names[i, ]$object, envir = envir), "events")
    evs <- lapply(ev_names, get, envir = envir)
    nodes_object <- attr(get(object_names[i, ]$object, envir = envir), "nodes")
    if (length(nodes_object) > 1) {
      nodes <- nodes_object[1]
      nodes2 <- nodes_object[2]
    } else {
      nodes <- nodes2 <- nodes_object
    }

    # replace labels with ids
    if (length(ev_names) > 0) {
      for (j in seq_along(evs)) {
        evs[[j]] <- sanitizeEvents(evs[[j]], nodes, nodes2, envir = envir)
      }
      events_objects_link <- rbind(
        events_objects_link,
        cbind(events = ev_names, object_names[i, ], row.names = NULL)
      )
      events <- append(events, evs)
      names(events)[(length(events) - length(evs) + 1):length(events)] <-
        ev_names
    }
  }

  return(list(
    events,
    events_objects_link
  ))
}

get_events_effects_link <- function(events, rhs_names, events_objects_link) {
  events_effects_link <- matrix(
    data = NA, nrow = length(events), ncol = length(rhs_names),
    dimnames = list(
      names(events),
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
    data = NA, nrow = length(obj_names), ncol = length(eff_names),
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
  rhs <- extract_formula_terms(formula[[3]])
  if (!is.list(rhs)) rhs <- list(rhs)
  rhs_names <- lapply(rhs, function(term) lapply(term, deparse))
}

parse_intercept <- function(rhs_names) {
  intercept <- FALSE
  v <- NA
  tryCatch(
    v <- as.numeric(rhs_names[[1]][[1]]),
    warning = function(x) {
    }
  )
  if (!is.na(v) && v == 1) {
    intercept <- TRUE
    rhs_names <- rhs_names[-1]
  }
  return(list(rhs_names, intercept))
}

parse_multiple_effects <- function(
    rhs_names, default = FALSE, envir = environment()) {
  multiple <- list()
  multiple_names <- character(0)
  for (i in seq_along(rhs_names)) {
    name <- ""
    id <- which(names(rhs_names[[i]]) == "ignoreRep")
    multiple_param <- ifelse(length(id) == 1, rhs_names[[i]][[id]], default)
    if (multiple_param %in% c("T", "F", "TRUE", "FALSE")) {
      multiple_param <- as.logical(multiple_param)
    }
    if (!multiple_param) {
      table <- getDataObjects(rhs_names[i])
      net_ids <- vapply(getElementFromDataObjectTable(table, envir = envir),
        FUN = inherits,
        FUN.VALUE = logical(1),
        what = "network.goldfish"
      )
      name <- table[net_ids, "name"][1]
    }
    if (is.character(multiple_param)) {
      name <- multiple_param
      multiple_param <- FALSE
    }
    if (!is.na(name) && name != "" && !exists(name, envir = envir)) {
      stop("Unknown object in 'ignoreRep' parameter: ", name, call. = FALSE)
    }
    multiple <- append(multiple, multiple_param)
    multiple_names <- c(multiple_names, name)
    rhs_names[[i]] <- if (length(id) > 0) rhs_names[[i]][-id] else rhs_names[[i]]
  }
  names(multiple) <- multiple_names
  return(list(rhs_names, multiple))
}

parse_time_windows <- function(rhs_names, envir = new.env()) {
  object_names <- getDataObjects(rhs_names)
  has_windows <- which(
    vapply(rhs_names, function(x) !is.null(getElement(x, "window")), logical(1))
  )
  for (i in has_windows) {
    window_name <- rhs_names[[i]]$window
    window <- tryCatch(
      eval(parse(text = window_name), envir = envir),
      error = function(e) {
        e$message <- paste(
          "Invalid window parameter for effect ",
          rhs_names[[i]][[1]], " ", rhs_names[[i]][[2]],
          ":\n", e$message
        )
        stop(e)
      }
    )
    is_valid_name <- grepl("^[[:alpha:]][[:alnum:]_.]+$", window_name)
    if (inherits(window, c("Period", "Duration")) &&
      "lubridate" %in% attr(attr(window, "class"), "package")) {
      if (!is_valid_name) {
        window_name <- gsub("\\s", "", as.character(window))
        if (inherits(window, "Duration")) {
          window_name <- gsub(
            "^(\\d+s)\\s*(\\(.+\\))$", "\\1",
            as.character(window)
          )
        }
      }
    } else if (inherits(window, "character")) {
      if (!is_valid_name) window_name <- gsub(" ", "", window)
      if (!is.numeric(window) &&
        !grepl("^\\d+ (sec|min|hour|day|week|month|year)", window)) {
        stop(
          "The window effect specified with the effect ", rhs_names[[i]][[1]],
          " ", rhs_names[[i]][[2]], " is not in the form 'number unit'\n",
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
      if (grepl("month", window)) { # lubridate approximation
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 2629800
      }
      if (grepl("year", window)) { # lubridate approximation
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 31557600
      }
    } else if (is.numeric(window)) { # check numeric type

      if (window < 0) {
        stop(
          "The window specified with the effect ",
          rhs_names[[i]][[1]], " ", rhs_names[[i]][[2]],
          " is not a positive numeric value"
        )
      }
    }
    name <- rhs_names[[i]][[2]]
    objects <- object_names[object_names$name == name, ]
    is_attribute <- !is.na(objects$attribute)
    rhs_names[[i]][[2]] <- paste(rhs_names[[i]][[2]],
      window_name,
      sep = "_"
    )
    if (is_attribute) {
      name_nodes <- objects$nodeset
      nodes <- get(name_nodes, envir = envir)
      attribute <- objects$attribute
      new_attribute <- paste(attribute, window_name, sep = "_")
      nodes[new_attribute] <- nodes[attribute]
      all_events <- attr(nodes, "events")
      all_dynamic_attributes <- attr(nodes, "dynamic_attributes")
      all_events <- all_events[all_dynamic_attributes == attribute]
    } else {
      network <- get(name, envir = envir)
      new_network <- matrix(0, nrow = nrow(network), ncol = ncol(network))
      new_name <- paste(name, window_name, sep = "_")
      attr(new_network, "events") <- NULL
      attr(new_network, "nodes") <- attr(network, "nodes")
      attr(new_network, "directed") <- attr(network, "directed")
      dimnames(new_network) <- dimnames(network)
      class(new_network) <- class(network)
      all_events <- attr(network, "events")
    }
    for (events in all_events) {
      object_events <- get(events, envir = envir)
      new_events <- create_windowed_events(object_events, window)
      name_new_events <- paste(events, window, sep = "_")
      if (is_attribute) {
        attr(nodes, "events") <- c(attr(nodes, "events"), name_new_events)
        attr(nodes, "dynamic_attributes") <-
          c(attr(nodes, "dynamic_attributes"), new_attribute)
      } else {
        attr(new_network, "events") <-
          c(attr(new_network, "events"), name_new_events)
      }
      assign(name_new_events, new_events, envir = envir)
    }
    if (is_attribute) {
      assign(name_nodes, nodes, envir = envir)
    } else {
      assign(new_name, new_network, envir = envir)
    }
  }
  return(rhs_names)
}
