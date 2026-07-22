##################### ###
#
# Goldfish package
# Some utility functions useful in
# various parts of the code
#
##################### ###

#' get data objects
#'
#' @param namedList list
#' @param keepOrder logical.
#' @param remove_first logical.
#'
#' @return a
#' @noRd
#'
#' @examples
#' \donttest{
#' data("Social_Evolution")
#' call_network <- make_network(nodes = actors, directed = TRUE)
#' call_network <- link_events(
#'   x = call_network, change_events = calls, nodes = actors
#' )
#' calls_dependent <- defineDependentEvents(
#'   events = calls, nodes = actors, default_network = call_network
#' )
#' parsedformula <- parseFormula(calls_dependent ~ ego(actors$floor))
#' objects_effects_link <- get_objects_effects_link(parsedformula$rhs_names, 1L)
#' get_data_objects(list(rownames(objects_effects_link)), remove_first = FALSE)
#' }
get_data_objects <- function(
  namedList,
  keepOrder = FALSE,
  remove_first = TRUE
) {
  # strip function names
  objNames <- unlist(namedList)
  if (remove_first) {
    objNames <- unlist(lapply(namedList, "[", -1))
  }

  # strip named parameters except for reserved ones
  if (!is.null(names(objNames))) {
    ids <- isReservedElementName(names(objNames)) | names(objNames) == ""
    objNames <- objNames[ids]
  }

  if (!keepOrder) {
    objNames <- unique(objNames)
  }

  # # case list(...)
  areList <- grepl("list\\(\\s*(.+)\\s*\\)", objNames)
  .split <- ifelse(
    areList,
    gsub("list\\(\\s*(.+)\\s*\\)", "\\1", objNames),
    objNames
  )
  .split <- unlist(strsplit(.split, split = "\\s*,\\s*"))

  if (!keepOrder) {
    .split <- unique(.split)
  }
  # # case attributes
  split <- strsplit(.split, split = "$", fixed = TRUE)

  objNameTable <- Reduce(
    rbind,
    lapply(
      split,
      \(v) {
        if (length(v) == 1) {
          data.frame(
            object = v,
            nodeset = NA,
            attribute = NA,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            object = NA,
            nodeset = v[1],
            attribute = v[2],
            stringsAsFactors = FALSE
          )
        }
      }
    )
  )

  return(cbind(name = .split, objNameTable, stringsAsFactors = FALSE))
}


get_element_from_data_object_table <- function(x, envir = environment()) {
  elements <- list()
  if (nrow(x) == 0) {
    return(elements)
  }
  for (i in seq_len(nrow(x))) {
    elements[[i]] <- NA
    row <- x[i, ]
    if (!is.na(row$object)) {
      if (!exists(row$object, envir = envir)) {
        cli::cli_abort(c(
          "x" = "The object {.var {row$object}} does not exist.",
          "i" = "Check that the object used in the formula exists"
        ))
      }
      elements[[i]] <- get(row$object, envir = envir)
    }
    if (!is.na(row$nodeset) && !is.na(row$attribute)) {
      if (!exists(row$nodeset, envir = envir)) {
        cli::cli_abort(c(
          "x" = "The nodeset {.var {row$nodeset}} does not exist.",
          "i" = "Check that the nodeset used in the formula exists"
        ))
      }
      temp_attribute <- getElement(
        get(row$nodeset, envir = envir),
        row$attribute
      )
      if (is.null(temp_attribute)) {
        cli::cli_abort(c(
          "x" = "The attribute {.var {row$attribute}} in the nodeset
          {.var {row$nodeset}} does not exist.",
          "i" = "Check that the attribute used in the formula exists"
        ))
      } else {
        elements[[i]] <- temp_attribute
      }
    }
  }
  return(elements)
}


# Check whether (date) input is of POSIXct format
# currently not needed in parseTimeWindows because
# POSIXct doesn't have duration names and
# we don't want to have any unnecessary package dependencies.
# We keep this utility function however,
# as it may be useful elsewhere...
# is.POSIXct <- function(x) inherits(x, "POSIXct")

isReservedElementName <- function(x) {
  x %in% c("network", "attribute", "network2", "attribute2")
}


#' Sanitize events
#'
#' replace labels with IDs and specific time formats with numeric
#'
#' @param events a dataframe that represents a valid events list
#' @inheritParams link_events
#'
#' @return a data frame with IDs instead of labels and time in numeric format
#' @noRd
#'
#' @examples
#' \donttest{
#' data("Social_Evolution")
#' afterSanitize <- sanitizeEvents(calls, "actors")
#' }
sanitizeEvents <- function(events, nodes, nodes2 = nodes, envir = new.env()) {
  if (is.character(nodes)) {
    nodes <- get(nodes, envir = envir)
  }
  if (is.character(nodes2)) {
    nodes2 <- get(nodes2, envir = envir)
  }
  if (is.character(events$node)) {
    events$node <- match(events$node, nodes$label)
  }
  if (is.character(events$sender)) {
    events$sender <- match(events$sender, nodes$label)
  }
  if (is.character(events$receiver)) {
    events$receiver <- match(events$receiver, nodes2$label)
  }
  events$time <- as.numeric(events$time)
  events
}

#' Reduce preprocess output
#'
#' It took a preprocess object and return a matrix with all the
#' change statistics together for each effect.
#' `effect_pos` argument allows to reduce just for a subset of effects,
#' it won't reduce the time or memory space used.
#'
#' @param preproData a preprocess data object from preprocess.
#' @param type a character. `"withTime"` returns the dependent stats changes
#' with the time where they occur.
#' @param effect_pos a vector of integers of the effects to keep.
#'
#' @return a list with a matrix for each effect.
#' @noRd
#'
#' @examples
#' \donttest{
#' data("Social_Evolution")
#' call_network <- make_network(nodes = actors, directed = T)
#' call_network <- link_events(
#'   x = call_network, change_events = calls, nodes = actors
#' )
#' calls_dependent <- make_dependent_events(
#'   events = calls, nodes = actors, default_network = call_network
#' )
#' prep <- estimate_dynam(calls_dependent ~ inertia + trans,
#'   sub_model = "choice",
#'   preprocessing_only = TRUE, silent = TRUE
#' )
#' v00 <- ReducePreprocess(prep, "withTime")
#' v01 <- ReducePreprocess(prep, "withTime", c(1L, 3L))
#' v03 <- ReducePreprocess(prep, "withoutTime")
#' }
ReducePreprocess <- function(
  preproData,
  type = c("withTime", "withoutTime"),
  effect_pos = NULL
) {
  stopifnot(
    is.null(effect_pos) ||
      !is.null(effect_pos) && inherits(effect_pos, "integer")
  )
  type <- match.arg(type)

  is_rate <- length(dim(preproData$initialStats)) == 2L
  nEffects <- if (is_rate) {
    ncol(preproData$initialStats)
  } else {
    dim(preproData$initialStats)[3]
  }

  stopifnot(
    is.null(effect_pos) || !is.null(effect_pos) && max(effect_pos) <= nEffects
  )

  ReduceEffUpdates <- function(statsChange, event_time) {
    reduce <- Map(
      \(x, y) {
        lapply(
          x,
          \(z) {
            if (is.null(z)) {
              return(NULL)
            }
            if (nrow(z) == 1) {
              return(if (type == "withTime") cbind(time = y, z) else z)
            }
            dedup_cols <- if (is_rate) "node1" else c("node1", "node2")
            discard <- duplicated(
              z[, dedup_cols, drop = FALSE],
              fromLast = TRUE
            )
            changes <- cbind(
              time = if (type == "withTime") rep(y, sum(!discard)) else NULL,
              z[!discard, , drop = FALSE]
            )
            if (nrow(changes) == 1) {
              return(changes)
            }
            order_cols <- if (is_rate) "node1" else c("node1", "node2")
            changes[
              do.call(order, lapply(order_cols, function(col) changes[, col])),
            ]
          }
        )
      },
      statsChange,
      event_time
    )

    return(lapply(
      seq_len(nEffects),
      function(i) Reduce(rbind, lapply(reduce, "[[", i))
    ))
  }

  ReduceEffUpdatesFlat <- function(eventsKeep) {
    counts <- diff(c(0L, preproData$stat_mat_pointer))
    colEvent <- rep(seq_along(preproData$stat_mat_pointer), counts)
    colsKeep <- eventsKeep[colEvent]
    upd <- preproData$stat_mat_update[, colsKeep, drop = FALSE]
    updEvent <- colEvent[colsKeep]
    lapply(
      seq_len(nEffects),
      function(i) {
        effCols <- upd[3, ] == (i - 1)
        if (!any(effCols)) {
          return(NULL)
        }
        sub <- upd[, effCols, drop = FALSE]
        subEvent <- updEvent[effCols]
        key <- if (is_rate) {
          cbind(subEvent, sub[1, ])
        } else {
          cbind(subEvent, sub[1, ], sub[2, ])
        }
        keepCol <- !duplicated(key, fromLast = TRUE)
        sub <- sub[, keepCol, drop = FALSE]
        subEvent <- subEvent[keepCol]
        ordering <- if (is_rate) {
          order(subEvent, sub[1, ])
        } else {
          order(subEvent, sub[1, ], sub[2, ])
        }
        sub <- sub[, ordering, drop = FALSE]
        subEvent <- subEvent[ordering]
        cbind(
          time = if (type == "withTime") preproData$event_time[subEvent],
          node1 = sub[1, ] + 1,
          node2 = if (!is_rate) sub[2, ] + 1,
          replace = sub[4, ]
        )
      }
    )
  }

  ReduceBroadcastFlat <- function(eventsKeep) {
    bc <- preproData$stat_mat_broadcast
    bptr <- preproData$stat_mat_broadcast_pointer
    out <- vector("list", nEffects)
    if (is.null(bc) || ncol(bc) == 0L) {
      return(out)
    }
    counts <- diff(c(0L, bptr))
    colEvent <- rep(seq_along(bptr), counts)
    colsKeep <- eventsKeep[colEvent]
    bcK <- bc[, colsKeep, drop = FALSE]
    bcEvent <- colEvent[colsKeep]
    n1 <- dim(preproData$initialStats)[1]
    n2 <- if (is_rate) 1L else dim(preproData$initialStats)[2]
    # The mode-map reading rides on the spec; the side-name comparison stays only
    # as a fallback for a bare object that never carried a spec.
    is_two_mode <- preproData$model_spec$is_two_mode %||%
      !identical(preproData$nodes, preproData$nodes2)
    for (i in seq_len(nEffects)) {
      effCols <- bcK[3, ] == (i - 1)
      if (!any(effCols)) {
        next
      }
      sub <- bcK[, effCols, drop = FALSE]
      subEvent <- bcEvent[effCols]
      rows <- do.call(
        rbind,
        lapply(seq_len(ncol(sub)), function(cc) {
          kind <- sub[1, cc]
          fixed <- sub[2, cc] + 1L
          val <- sub[4, cc]
          ev <- subEvent[cc]
          if (is_rate) {
            cbind(event = ev, node1 = seq_len(n1), node2 = 0L, replace = val)
          } else if (kind == 1L) {
            nodes1 <- if (is_two_mode) {
              seq_len(n1)
            } else {
              setdiff(seq_len(n1), fixed)
            }
            cbind(event = ev, node1 = nodes1, node2 = fixed, replace = val)
          } else if (kind == 2L) {
            nodes2 <- if (is_two_mode) {
              seq_len(n2)
            } else {
              setdiff(seq_len(n2), fixed)
            }
            cbind(event = ev, node1 = fixed, node2 = nodes2, replace = val)
          } else {
            ij <- expand.grid(node1 = seq_len(n1), node2 = seq_len(n2))
            if (!is_two_mode) {
              ij <- ij[ij$node1 != ij$node2, ]
            }
            cbind(event = ev, node1 = ij$node1, node2 = ij$node2, replace = val)
          }
        })
      )
      key <- if (is_rate) {
        cbind(rows[, "event"], rows[, "node1"])
      } else {
        cbind(rows[, "event"], rows[, "node1"], rows[, "node2"])
      }
      keep <- !duplicated(key, fromLast = TRUE)
      rows <- rows[keep, , drop = FALSE]
      ord <- if (is_rate) {
        order(rows[, "event"], rows[, "node1"])
      } else {
        order(rows[, "event"], rows[, "node1"], rows[, "node2"])
      }
      rows <- rows[ord, , drop = FALSE]
      out[[i]] <- cbind(
        time = if (type == "withTime") preproData$event_time[rows[, "event"]],
        node1 = rows[, "node1"],
        node2 = if (!is_rate) rows[, "node2"],
        replace = rows[, "replace"]
      )
    }
    out
  }

  combine_point_broadcast <- function(point_list, eventsKeep) {
    bcast_list <- ReduceBroadcastFlat(eventsKeep)
    lapply(seq_len(nEffects), function(i) {
      if (!is.null(point_list[[i]])) point_list[[i]] else bcast_list[[i]]
    })
  }

  dep_idx <- preproData$is_dependent == 1L
  rc_idx <- preproData$is_dependent == 0L
  is_flat <- is.null(preproData$stats_change)

  outDependentStatChange <- if (is_flat) {
    combine_point_broadcast(ReduceEffUpdatesFlat(dep_idx), dep_idx)
  } else {
    ReduceEffUpdates(
      preproData$stats_change[dep_idx],
      preproData$event_time[dep_idx]
    )
  }

  if (
    (preproData$sub_model == "rate" || preproData$model == "REM") &&
      sum(rc_idx) > 0
  ) {
    rightCensoredStatChange <- if (is_flat) {
      combine_point_broadcast(ReduceEffUpdatesFlat(rc_idx), rc_idx)
    } else {
      ReduceEffUpdates(
        preproData$stats_change[rc_idx],
        preproData$event_time[rc_idx]
      )
    }

    reducedPrepro <- list()
    for (ii in seq.int(length(outDependentStatChange))) {
      reducedPrepro[[ii]] <- list(
        dependent = outDependentStatChange[[ii]],
        right_censored = rightCensoredStatChange[[ii]]
      )
    }

    if (!is.null(effect_pos)) {
      return(reducedPrepro[effect_pos])
    }
    return(reducedPrepro)
  } else if (!is.null(effect_pos)) {
    return(outDependentStatChange[effect_pos])
  } else {
    return(outDependentStatChange)
  }
}

#' Expand a set of changes
#'
#' given a `node` and a `replace` value, set the change to all the nodes in
#' the nodes `set`. Add the `time` to the array if provided.
#'
#' @param nodes a numeric vector with the sanitize position of the nodes
#' @param replace a numeric vector with the replace value
#' @param time a numeric vector with the time-stamp when the changes happen
#' @param set a numeric vector with the index id of the node set
#' @param is_two_mode logical, whether self ties are allow or not
#'
#' @return an array with columns `node1`, `node2`, `replace` and `time`
#' @noRd
#'
#' @examples
#' fillChanges(c(1, 3), c(4, 8), NULL, 1:5)
fillChanges <- function(nodes, replace, time, set, is_two_mode = FALSE) {
  times <- ifelse(is_two_mode, length(set), length(set) - 1)

  cbind(
    time = if (!is.null(time)) rep(time, each = times) else NULL,
    node1 = rep(nodes, each = times),
    node2 = Reduce(c, lapply(nodes, \(x) set[!set %in% x])),
    replace = rep(replace, each = times)
  )
}

#' Apply flat buffer updates to a running statistics array
#'
#' Writes a slice of the flat update buffer into the running statistics
#' array. `updates_slice` is a 4 x k matrix with rows
#' `(node1, node2, effect, replace)` where indices are 0-based as stored in
#' `stat_mat_update`. With duplicated cells the last column wins.
#'
#' @param statsArray a numeric matrix `n1 x nEffects` when
#'   `is_sender = TRUE`, otherwise a numeric array `n1 x n2 x nEffects`.
#' @param updates_slice a numeric matrix 4 x k, columns from
#'   `stat_mat_update`.
#' @param is_sender logical, whether `statsArray` is sender-indexed (2D).
#'
#' @return `statsArray` with the updates applied.
#' @noRd
apply_flat_update <- function(statsArray, updates_slice, is_sender) {
  if (length(updates_slice) == 0L) {
    return(statsArray)
  }
  if (is_sender) {
    statsArray[cbind(
      updates_slice[1, ] + 1L,
      updates_slice[3, ] + 1L
    )] <- updates_slice[4, ]
  } else {
    statsArray[cbind(
      updates_slice[1, ] + 1L,
      updates_slice[2, ] + 1L,
      updates_slice[3, ] + 1L
    )] <- updates_slice[4, ]
  }
  statsArray
}

#' Apply broadcast (constant-value fan-out) updates to a running stats array
#'
#' Decodes a slice of the `stat_mat_broadcast` buffer into the running
#' statistics array with in-place vectorised slice assignment.
#' Each column of `broadcast_slice` is one coded fan-out entry with rows
#' `(kind, fixed, effect, replace)` where `fixed` and `effect` are 0-indexed.
#' The decode mirrors the eager `to_alter()` (`kind = 1`), `to_ego()`
#' (`kind = 2`), and `fillChanges()` (`kind = 3`) expansion, including the
#' reflexive-diagonal exclusion for one-mode dyad models.
#'
#' @param statsArray a numeric matrix `n1 x nEffects` when `is_sender = TRUE`,
#'   otherwise a numeric array `n1 x n2 x nEffects`.
#' @param broadcast_slice a numeric matrix 4 x b, columns from
#'   `stat_mat_broadcast`. An empty slice is a no-op.
#' @param is_sender logical, whether `statsArray` is sender-indexed (2D).
#' @param n1,n2 integer dimensions of the dyad statistics array.
#' @param twomode_or_reflexive logical; when `FALSE` the reflexive diagonal
#'   cell is excluded, matching `to_ego()` / `to_alter()`.
#'
#' @return `statsArray` with the broadcast updates applied.
#' @noRd
apply_broadcast_update <- function(
  statsArray,
  broadcast_slice,
  is_sender,
  n1,
  n2,
  twomode_or_reflexive
) {
  if (length(broadcast_slice) == 0L) {
    return(statsArray)
  }
  for (b in seq_len(ncol(broadcast_slice))) {
    kind <- broadcast_slice[1, b]
    fixed <- broadcast_slice[2, b] + 1L
    effect <- broadcast_slice[3, b] + 1L
    value <- broadcast_slice[4, b]
    if (is_sender) {
      statsArray[, effect] <- value
    } else if (kind == 1L) {
      rows <- if (twomode_or_reflexive) {
        seq_len(n1)
      } else {
        setdiff(seq_len(n1), fixed)
      }
      statsArray[rows, fixed, effect] <- value
    } else if (kind == 2L) {
      cols <- if (twomode_or_reflexive) {
        seq_len(n2)
      } else {
        setdiff(seq_len(n2), fixed)
      }
      statsArray[fixed, cols, effect] <- value
    } else if (twomode_or_reflexive) {
      statsArray[,, effect] <- value
    } else {
      slice <- statsArray[,, effect]
      slice[row(slice) != col(slice)] <- value
      statsArray[,, effect] <- slice
    }
  }
  statsArray
}


#' Merge flat update buffers for reuse of a preprocessed object
#'
#' Combines the flat update buffer of a previous preprocessing result with
#' the buffer of the newly preprocessed effects, remapping effect indices
#' to the positions of the new formula. Both objects must cover the same
#' stored-event sequence.
#'
#' @param old_prep `preprocessed.goldfish` object reused through
#'   `preprocessing_init`.
#' @param new_prep `preprocessed.goldfish` object with the newly added
#'   effects, or NULL when the new formula adds no effects.
#' @param effects_indexes integer vector from `compare_formulas()`: for
#'   each effect of the new formula, its position in the old formula or 0
#'   when newly added.
#'
#' @return a list with the merged `stat_mat_update` and `stat_mat_pointer`.
#' @noRd
merge_flat_updates <- function(old_prep, new_prep, effects_indexes) {
  n_events <- length(old_prep$stat_mat_pointer)
  counts_old <- diff(c(0L, old_prep$stat_mat_pointer))
  event_old <- rep(seq_len(n_events), counts_old)
  position_old <- match(old_prep$stat_mat_update[3, ] + 1L, effects_indexes)
  keep <- !is.na(position_old)
  mat <- old_prep$stat_mat_update[, keep, drop = FALSE]
  mat[3, ] <- position_old[keep] - 1L
  event <- event_old[keep]

  if (!is.null(new_prep)) {
    counts_new <- diff(c(0L, new_prep$stat_mat_pointer))
    event_new <- rep(seq_len(length(new_prep$stat_mat_pointer)), counts_new)
    position_new <- which(effects_indexes == 0L)
    mat_new <- new_prep$stat_mat_update
    mat_new[3, ] <- position_new[mat_new[3, ] + 1L] - 1L
    ordering <- order(c(event, event_new), method = "radix")
    mat <- cbind(mat, mat_new)[, ordering, drop = FALSE]
    event <- c(event, event_new)[ordering]
  }

  list(
    stat_mat_update = mat,
    stat_mat_pointer = cumsum(tabulate(event, nbins = n_events))
  )
}

GetDetailPrint <- function(
  objects_effects_link,
  parsedformula,
  fixedParameters = NULL
) {
  # matrix with the effects in rows and objects in columns,
  # which net or actor att
  #
  # A comparison effect on a two-mode focal reads its one written operand on
  # both sides, so the parser resolved it into two references. Those are two
  # pieces of state but one operand, and the user must see what they wrote:
  # report the sender-side reference alone. Effects whose second operand the
  # user really did write (ego_alter_interaction) keep both.
  effect_names <- colnames(objects_effects_link)
  displayed <- lapply(seq_len(ncol(objects_effects_link)), function(k) {
    # Indexing a single-row link matrix drops the object names that `apply()`
    # used to preserve, so restore them from the rownames.
    x <- stats::setNames(
      objects_effects_link[, k],
      rownames(objects_effects_link)
    )
    notNA <- !is.na(x)
    objs <- x[notNA]
    objs <- names(objs[order(objs)])
    if (effect_names[k] %in% CROSS_SIDE_EFFECTS) objs[1] else objs
  })
  maxObjs <- max(lengths(displayed))
  effect_description <- matrix(
    unlist(lapply(displayed, function(objs) {
      c(objs, rep("", maxObjs - length(objs)))
    })),
    nrow = ncol(objects_effects_link),
    ncol = maxObjs,
    byrow = TRUE
  )
  # # handle degenerate case one effect one object
  dimnames(effect_description) <- list(
    colnames(objects_effects_link),
    if (ncol(effect_description) == 1) {
      "Object"
    } else {
      sprintf("Object %d", seq_len(ncol(effect_description)))
    }
  )

  objectsName <- colnames(effect_description)
  # adding other parameters: each effect refers to which network
  # or actor attribute

  # effect_description <- cbind(
  #   effect = rownames(effect_description),
  #   effect_description
  # )

  if (any(unlist(parsedformula$ignore_rep_parameter))) {
    effect_description <- cbind(
      effect_description,
      ignore_repetitions = ifelse(parsedformula$ignore_rep_parameter, "B", "")
    )
  }
  if (any(unlist(parsedformula$weighted_parameter))) {
    effect_description <- cbind(
      effect_description,
      weighted = ifelse(parsedformula$weighted_parameter, "W", "")
    )
  }
  if (any(parsedformula$type_parameter != "")) {
    effect_description <- cbind(
      effect_description,
      type = parsedformula$type_parameter
    )
  }
  has_windows <- FALSE
  if (!all(vapply(parsedformula$window_parameters, is.null, logical(1)))) {
    has_windows <- TRUE
    effect_description <- cbind(
      effect_description,
      window = vapply(
        parsedformula$window_parameters,
        function(x) ifelse(is.null(x), "", gsub("['\"]", "", x)),
        character(1)
      )
    )
    # reduce object name
    effect_description[, objectsName] <- t(apply(
      effect_description,
      1,
      \(x) {
        gsub(
          paste0("^(.+)_", gsub(" ", "", x["window"]), "$"),
          "\\1",
          x[objectsName]
        )
      }
    ))
  }
  if (any(parsedformula$trans_parameter != "")) {
    effect_description <- cbind(
      effect_description,
      transformer_fn = parsedformula$trans_parameter
    )
  }
  if (any(parsedformula$summ_parameter != "")) {
    effect_description <- cbind(
      effect_description,
      summarizer_fn = parsedformula$summ_parameter
    )
  }
  # DyNAMi
  if (any(parsedformula$joining_parameter != "")) {
    effect_description <- cbind(
      effect_description,
      joining = parsedformula$joining_parameter
    )
  }
  if (any(parsedformula$sub_type_parameter != "")) {
    effect_description <- cbind(
      effect_description,
      sub_type = parsedformula$sub_type_parameter
    )
  }
  if (any(parsedformula$historyParameter != "")) {
    effect_description <- cbind(
      effect_description,
      history = parsedformula$historyParameter
    )
  }
  # Interaction columns have no object/attribute of their own, so
  # they are absent from objects_effects_link; append one row per interaction after
  # the function-effect rows. The row keeps the term string as its (readable)
  # rowname; the compact `coef()` / export names are derived below from the
  # operand rows (an interaction's name is the join of its operands' names), so
  # the object columns are left empty here.
  n_inter <- length(parsedformula$interactions)
  if (n_inter > 0) {
    labels <- vapply(
      parsedformula$interactions,
      function(x) x$label,
      character(1)
    )
    inter_mat <- matrix(
      "",
      nrow = length(labels),
      ncol = ncol(effect_description),
      dimnames = list(labels, colnames(effect_description))
    )
    effect_description <- rbind(effect_description, inter_mat)
  }
  # rownames(effect_description) <- NULL
  if (parsedformula$has_intercept) {
    effect_description <- rbind("", effect_description)
    rownames(effect_description)[1] <- "Intercept"
  }

  if (!is.null(fixedParameters)) {
    effect_description <- cbind(
      effect_description,
      fixed = !is.na(fixedParameters)
    )
  }

  decoder <- .decoderColumns(effect_description)
  # Interaction rendering: each interaction's compact names are the
  # `:`-join of its operands' rendered names, so they inherit the operands' short
  # forms and object disambiguation. Operand rhs index j maps to function row
  # j + intercept_offset; the interaction rows are the final n_inter rows.
  if (n_inter > 0) {
    intercept_offset <- if (parsedformula$has_intercept) 1L else 0L
    n_total <- nrow(effect_description)
    join_cols <- c(
      ".effect_short",
      ".object_short",
      ".term_export",
      ".coef_name"
    )
    for (i in seq_len(n_inter)) {
      inter_row <- n_total - n_inter + i
      op_rows <- parsedformula$interactions[[i]]$operands + intercept_offset
      for (col in join_cols) {
        decoder[inter_row, col] <- paste(decoder[op_rows, col], collapse = ":")
      }
    }
  }

  effect_description <- cbind(effect_description, decoder)

  attr(effect_description, "has_windows") <- has_windows
  return(effect_description)
}

GetFixed <- function(object) {
  if ("fixed" %in% colnames(object$names)) {
    vapply(
      object$names[, "fixed"],
      function(x) eval(parse(text = x)),
      logical(1)
    )
  } else {
    rep(FALSE, length(object$parameters))
  }
}

checkArgsEstimation <- function(variables) {}

.goldfishEffectShort <- c(
  inertia = "inrt",
  recip = "rec",
  outdeg = "odeg",
  indeg = "ideg",
  common_sender = "cmm_sen",
  common_receiver = "cmm_rec",
  mixed_common_sender = "mix_cmm_sen",
  mixed_common_receiver = "mix_cmm_rec",
  mixed_cycle = "mix_cycle",
  mixed_trans = "mix_trans",
  ego_alter_interaction = "ego_alt",
  node_trans = "nd_trans",
  tertius = "tert",
  tertius_diff = "tert_diff",
  degree = "deg",
  global = "glob",
  triangle = "tri",
  alterdeg = "altdeg",
  alterpop = "altpop",
  dyadXdiff = "dyXdiff",
  dyadXego = "dyXego",
  sizeXdiff = "szXdiff",
  sizeXego = "szXego"
)

.shortEffect <- function(x) {
  hit <- x %in% names(.goldfishEffectShort)
  x[hit] <- .goldfishEffectShort[x[hit]]
  x
}

.trimObject <- function(x) sub("^[^$]*\\$", "", x)

.shortestUniquePrefix <- function(x, minLen = 3L, maxLen = 6L) {
  u <- unique(x[nzchar(x)])
  if (length(u) == 0) {
    return(stats::setNames(character(0), character(0)))
  }
  maxAvail <- max(nchar(u))
  hi <- min(maxLen, maxAvail)
  chosen <- maxAvail
  if (hi >= minLen) {
    for (len in seq.int(minLen, hi)) {
      chosen <- len
      if (!anyDuplicated(substr(u, 1, len))) break
    }
  }
  stats::setNames(substr(u, 1, chosen), u)
}

.windowToken <- function(w) {
  w <- trimws(w)
  m <- regmatches(w, regexec("^([0-9]{1,3})\\s+([A-Za-z]+)$", w))[[1]]
  if (length(m) == 3L) {
    unit <- tolower(m[3])
    code <- if (unit %in% c("s", "sec", "secs", "second", "seconds")) {
      "s"
    } else if (unit %in% c("m", "min", "mins", "minute", "minutes")) {
      "m"
    } else if (unit %in% c("h", "hr", "hrs", "hour", "hours")) {
      "h"
    } else if (unit %in% c("d", "day", "days")) {
      "d"
    } else if (unit %in% c("wk", "wks", "week", "weeks")) {
      "wk"
    } else if (unit %in% c("mo", "mon", "month", "months")) {
      "mo"
    } else if (unit %in% c("yr", "yrs", "year", "years")) {
      "yr"
    } else {
      NULL
    }
    if (!is.null(code)) {
      return(paste0(m[2], code))
    }
  }
  "wdw"
}

.fnToken <- function(v, forceFn) {
  if (isTRUE(forceFn)) {
    return("fn")
  }
  if (grepl("^[A-Za-z.][A-Za-z0-9._]*$", v) && nchar(v) <= 6L) v else "fn"
}

.isFixedToken <- function(v) {
  !is.na(v) && (identical(v, "TRUE") || identical(v, TRUE))
}

.rowTokens <- function(row, argCols, forceFn, subPref, joinPref) {
  toks <- character(0)
  has <- function(col) col %in% argCols && nzchar(row[[col]])
  if (has("weighted")) {
    toks <- c(toks, "W")
  }
  if (has("type")) {
    toks <- c(toks, row[["type"]])
  }
  if (has("window")) {
    toks <- c(toks, .windowToken(row[["window"]]))
  }
  hasT <- has("transformer_fn")
  hasS <- has("summarizer_fn")
  if (hasT && hasS) {
    toks <- c(
      toks,
      paste0("t:", .fnToken(row[["transformer_fn"]], forceFn)),
      paste0("s:", .fnToken(row[["summarizer_fn"]], forceFn))
    )
  } else if (hasT) {
    toks <- c(toks, .fnToken(row[["transformer_fn"]], forceFn))
  } else if (hasS) {
    toks <- c(toks, .fnToken(row[["summarizer_fn"]], forceFn))
  }
  if (has("sub_type")) {
    toks <- c(toks, unname(subPref[row[["sub_type"]]]))
  }
  if (has("joining")) {
    toks <- c(toks, unname(joinPref[row[["joining"]]]))
  }
  if (has("history")) {
    toks <- c(toks, substr(row[["history"]], 1, 3))
  }
  if (has("ignore_repetitions")) {
    toks <- c(toks, "IR")
  }
  if ("fixed" %in% argCols && .isFixedToken(row[["fixed"]])) {
    toks <- c(toks, "Fx")
  }
  toks
}

.objectForms <- function(row, objCols, objLk, useShort) {
  objs <- row[objCols]
  objs <- .trimObject(objs[nzchar(objs)])
  if (length(objs) == 0L) {
    return(character(0))
  }
  if (isTRUE(useShort)) unname(objLk[objs]) else objs
}

.assembleTerms <- function(
  mat,
  useShortEffect,
  useShortObject,
  forceFn,
  objCols,
  argCols,
  objLk,
  subPref,
  joinPref,
  objSep = "·",
  effSep = "/",
  tokenSep = ",",
  open = " [",
  close = "]"
) {
  effects <- rownames(mat)
  effForm <- if (useShortEffect) .shortEffect(effects) else effects
  vapply(
    seq_len(nrow(mat)),
    function(i) {
      row <- stats::setNames(mat[i, ], colnames(mat))
      objs <- .objectForms(row, objCols, objLk, useShortObject)
      toks <- .rowTokens(row, argCols, forceFn, subPref, joinPref)
      out <- effForm[i]
      if (length(objs)) {
        out <- paste0(out, effSep, paste(objs, collapse = objSep))
      }
      if (length(toks)) {
        out <- paste0(out, open, paste(toks, collapse = tokenSep), close)
      }
      out
    },
    character(1)
  )
}

.truncateTerms <- function(x, width) {
  long <- nchar(x) > width
  x[long] <- paste0(substr(x[long], 1, max(1L, width - 1L)), "…")
  x
}

.sanitizeExport <- function(x, maxLength) {
  x <- gsub("·", "_", x)
  x <- gsub("[/ ,:\\[\\]]+", "_", x, perl = TRUE)
  x <- make.names(x)
  x <- gsub("[._]+", "_", x)
  x <- sub("_+$", "", x)
  if (is.finite(maxLength)) {
    x <- .uniqueTruncate(x, maxLength)
  } else {
    x <- make.unique(x, sep = "_")
  }
  x
}

.uniqueTruncate <- function(x, maxLength) {
  out <- substr(x, 1, maxLength)
  if (!anyDuplicated(out)) {
    return(out)
  }
  seen <- new.env(parent = emptyenv())
  for (i in seq_along(out)) {
    base <- out[i]
    if (is.null(seen[[base]])) {
      assign(base, 1L, envir = seen)
      next
    }
    k <- get(base, envir = seen)
    repeat {
      k <- k + 1L
      suf <- paste0("_", k)
      cand <- paste0(substr(base, 1, max(1L, maxLength - nchar(suf))), suf)
      if (is.null(seen[[cand]])) break
    }
    assign(base, k, envir = seen)
    assign(cand, 1L, envir = seen)
    out[i] <- cand
  }
  out
}

.coefTerms <- function(mat, objCols, argCols, objLk, subPref, joinPref) {
  base <- .shortEffect(rownames(mat))
  out <- base
  dup <- base %in% base[duplicated(base)]
  if (any(dup)) {
    objStr <- vapply(
      seq_len(nrow(mat)),
      function(i) {
        row <- stats::setNames(mat[i, ], colnames(mat))
        paste(.objectForms(row, objCols, objLk, TRUE), collapse = "_")
      },
      character(1)
    )
    addObj <- dup & nzchar(objStr)
    out[addObj] <- paste0(base[addObj], "_", objStr[addObj])
    stillDup <- out %in% out[duplicated(out)]
    if (any(stillDup)) {
      argStr <- vapply(
        seq_len(nrow(mat)),
        function(i) {
          row <- stats::setNames(mat[i, ], colnames(mat))
          paste(
            .rowTokens(row, argCols, FALSE, subPref, joinPref),
            collapse = "_"
          )
        },
        character(1)
      )
      addArg <- stillDup & nzchar(argStr)
      out[addArg] <- paste0(out[addArg], "_", argStr[addArg])
    }
  }
  out <- gsub("·", "_", out)
  out <- make.names(out)
  out <- gsub("[._]+", "_", out)
  out <- sub("_+$", "", out)
  make.unique(out, sep = "_")
}

compact_term_strings <- function(
  names,
  mode = c("console", "export", "coef"),
  width = getOption("width"),
  max_length = 63L
) {
  mode <- match.arg(mode)
  if (is.null(dim(names))) {
    names <- as.matrix(names)
  }
  cols <- colnames(names)
  if (is.null(cols)) {
    cols <- "Object"
  }
  metaCol <- startsWith(cols, ".")
  objCol <- grepl("^Object( [0-9]+)?$", cols)
  objCols <- cols[objCol & !metaCol]
  argCols <- cols[!objCol & !metaCol]

  allObjs <- .trimObject(unlist(lapply(objCols, function(cc) names[, cc])))
  objLk <- .shortestUniquePrefix(allObjs, 3L, 6L)

  subPref <- if ("sub_type" %in% argCols) {
    .shortestUniquePrefix(names[, "sub_type"], 3L, 20L)
  } else {
    stats::setNames(character(0), character(0))
  }
  joinPref <- if ("joining" %in% argCols) {
    .shortestUniquePrefix(names[, "joining"], 3L, 20L)
  } else {
    stats::setNames(character(0), character(0))
  }

  if (mode == "export") {
    terms <- .assembleTerms(
      names,
      FALSE,
      FALSE,
      FALSE,
      objCols,
      argCols,
      objLk,
      subPref,
      joinPref
    )
    return(.sanitizeExport(terms, if (is.null(max_length)) Inf else max_length))
  }
  if (mode == "coef") {
    return(.coefTerms(names, objCols, argCols, objLk, subPref, joinPref))
  }

  configs <- list(
    c(FALSE, FALSE, FALSE),
    c(TRUE, FALSE, FALSE),
    c(TRUE, TRUE, FALSE),
    c(TRUE, TRUE, TRUE)
  )
  terms <- NULL
  for (cfg in configs) {
    terms <- .assembleTerms(
      names,
      cfg[1],
      cfg[2],
      cfg[3],
      objCols,
      argCols,
      objLk,
      subPref,
      joinPref
    )
    if (max(nchar(terms)) <= width) {
      return(stats::setNames(terms, rownames(names)))
    }
  }
  stats::setNames(.truncateTerms(terms, width), rownames(names))
}

.decoderColumns <- function(names) {
  if (is.null(dim(names))) {
    names <- as.matrix(names)
  }
  cols <- colnames(names)
  if (is.null(cols)) {
    cols <- "Object"
  }
  metaCol <- startsWith(cols, ".")
  objCol <- grepl("^Object( [0-9]+)?$", cols)
  objCols <- cols[objCol & !metaCol]
  allObjs <- .trimObject(unlist(lapply(objCols, function(cc) names[, cc])))
  objLk <- .shortestUniquePrefix(allObjs, 3L, 6L)
  objShort <- vapply(
    seq_len(nrow(names)),
    function(i) {
      row <- stats::setNames(names[i, ], colnames(names))
      paste(.objectForms(row, objCols, objLk, TRUE), collapse = "·")
    },
    character(1)
  )
  cbind(
    .effect_short = unname(.shortEffect(rownames(names))),
    .object_short = objShort,
    .term_export = compact_term_strings(names, "export"),
    .coef_name = compact_term_strings(names, "coef")
  )
}

term_label <- function(names, column, mode, ...) {
  cols <- colnames(names)
  if (!is.null(cols) && column %in% cols) {
    return(unname(names[, column]))
  }
  unname(compact_term_strings(names, mode = mode, ...))
}

.bracketTokens <- function(terms) {
  m <- regmatches(terms, regexpr("\\[[^]]*\\]", terms))
  if (!length(m)) {
    return(character(0))
  }
  m <- gsub("[][]", "", m)
  unique(unlist(strsplit(m, ",")))
}

.compactLegend <- function(terms, termsFull) {
  toks <- .bracketTokens(terms)
  lines <- character(0)
  if ("W" %in% toks) {
    lines <- c(lines, "W = weighted")
  }
  if ("IR" %in% toks) {
    lines <- c(lines, "IR = ignore_rep")
  }
  if ("Fx" %in% toks) {
    lines <- c(lines, "Fx = fixed")
  }
  hasWdw <- "wdw" %in% toks
  if (hasWdw) {
    lines <- c(lines, "wdw = window")
  }
  hasFn <- any(grepl("^(t:|s:)?fn$", toks))
  if (hasFn) {
    lines <- c(lines, "fn = user-defined function")
  }
  if (any(startsWith(toks, "t:")) && any(startsWith(toks, "s:"))) {
    lines <- c(lines, "t: = transformer, s: = summarizer")
  }
  if (length(lines)) {
    lossy <- hasWdw || hasFn || !identical(unname(terms), unname(termsFull))
    if (lossy) {
      lines <- c(
        lines,
        "(use compact = FALSE for full effect/object names & details)"
      )
    }
  }
  lines
}
