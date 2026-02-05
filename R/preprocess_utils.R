# helper to pick the column to inspect for increment-like values
#' Pick increment-like column
#'
#' Choose the first available column name among the common increment-like
#' candidates (`increment`, `replace`, `weight`) present in `df`.
#'
#' @param df A data.frame or tibble.
#' @return A character scalar with the selected column name or `NULL` if
#'   none of the candidate columns are present.
#' @keywords internal
#' @noRd
pick_inc_col <- function(df) {
    cols <- intersect(names(df), c("increment", "replace", "weight"))
    if (length(cols) == 0) {
        return(NULL)
    }
    cols[1]
}

#' Split events into flavours
#'
#' Derive a `flavour` column for `events_tbl` based on an `increment`-like
#' column or a provided `flavour_map`. The `flavour` is useful to distinguish
#' sub-types of the same event type (for example creation vs deletion).
#'
#' @param events_tbl A data.frame/tibble of event rows with at least a
#'   `type` column and optionally `increment`/`replace`/`weight` columns.
#' @param flavour_map Either a named character vector mapping increment
#'   values to flavour names (applied globally), or a named list mapping
#'   per-type values. If `NULL` or empty, flavours are synthesized from the
#'   values present in the increment-like column.
#' @return The input `events_tbl` with a `flavour` character column added or
#'   left intact when already present and non-NA.
#' @keywords internal
#' @noRd
split_flavours <- function(
    events_tbl,
    flavour_map = c("1" = "creation", "0" = "deletion")
) {
    # If flavour column already exists and has non-NA values, leave it alone
    if ("flavour" %in% names(events_tbl) && any(!is.na(events_tbl$flavour))) {
        return(events_tbl)
    }

    inc_col <- pick_inc_col(events_tbl)

    # If flavour_map is NULL or empty, synthesize flavours per-type from inc_col
    if (is.null(flavour_map) || length(flavour_map) == 0) {
        if (nrow(events_tbl) == 0) {
            return(events_tbl)
        }
        events_tbl$flavour <- NA_character_
        types <- unique(as.character(events_tbl$type))
        for (tp in types) {
            idx_tp <- which(as.character(events_tbl$type) == tp)
            if (length(idx_tp) == 0) {
                next
            }
            if (is.null(inc_col)) {
                # no increment-like column: flavour equals type
                events_tbl$flavour[idx_tp] <- tp
                next
            }
            inc_vals <- unique(as.character(events_tbl[[inc_col]][idx_tp]))
            inc_vals <- inc_vals[!is.na(inc_vals)]
            if (length(inc_vals) <= 1) {
                events_tbl$flavour[idx_tp] <- tp
            } else {
                for (inc in inc_vals) {
                    idx_inc <- idx_tp[which(
                        as.character(events_tbl[[inc_col]][idx_tp]) == inc
                    )]
                    events_tbl$flavour[idx_inc] <- paste0(tp, "_", inc)
                }
            }
        }
        return(events_tbl)
    }

    # flavour_map provided: allow global named vector or a named list per-type
    events_tbl$flavour <- NA_character_
    map_is_list <- is.list(flavour_map) && !is.null(names(flavour_map))
    # Determine inc_col even if mapping provided; mapping keys are matched to inc values as characters
    if (is.null(inc_col)) {
        inc_col <- pick_inc_col(events_tbl)
    }

    types <- unique(as.character(events_tbl$type))
    for (tp in types) {
        idx_tp <- which(as.character(events_tbl$type) == tp)
        if (length(idx_tp) == 0) {
            next
        }

        # If per-type map exists, use it; else if flavour_map is a global named vector, use that; else synthesize
        if (map_is_list && tp %in% names(flavour_map)) {
            mp <- flavour_map[[tp]]
            mp_names <- names(mp)
            for (i in seq_along(mp)) {
                key <- mp_names[i]
                val <- mp[[i]]
                if (!is.null(inc_col)) {
                    idx_inc <- idx_tp[which(
                        as.character(events_tbl[[inc_col]][idx_tp]) == key
                    )]
                    if (length(idx_inc) > 0) events_tbl$flavour[idx_inc] <- val
                }
            }
        } else if (!map_is_list && !is.null(names(flavour_map))) {
            # global mapping: apply to all rows matching any key
            mp <- flavour_map
            mp_names <- names(mp)
            for (i in seq_along(mp)) {
                key <- mp_names[i]
                val <- mp[[i]]
                if (!is.null(inc_col)) {
                    idx_inc <- idx_tp[which(
                        as.character(events_tbl[[inc_col]][idx_tp]) == key
                    )]
                    if (length(idx_inc) > 0) events_tbl$flavour[idx_inc] <- val
                }
            }
            # after applying global map, any remaining NA flavours for this type should be synthesized
            if (any(is.na(events_tbl$flavour[idx_tp]))) {
                remaining <- idx_tp[is.na(events_tbl$flavour[idx_tp])]
                if (is.null(inc_col)) {
                    events_tbl$flavour[remaining] <- tp
                } else {
                    inc_vals <- unique(as.character(events_tbl[[inc_col]][
                        remaining
                    ]))
                    inc_vals <- inc_vals[!is.na(inc_vals)]
                    if (length(inc_vals) <= 1) {
                        events_tbl$flavour[remaining] <- tp
                    } else {
                        for (inc in inc_vals) {
                            idx_inc <- remaining[which(
                                as.character(events_tbl[[inc_col]][
                                    remaining
                                ]) ==
                                    inc
                            )]
                            events_tbl$flavour[idx_inc] <- paste0(tp, "_", inc)
                        }
                    }
                }
            }
        } else {
            # no specific map for this type: synthesize based on inc_col
            if (is.null(inc_col)) {
                events_tbl$flavour[idx_tp] <- tp
            } else {
                inc_vals <- unique(as.character(events_tbl[[inc_col]][idx_tp]))
                inc_vals <- inc_vals[!is.na(inc_vals)]
                if (length(inc_vals) <= 1) {
                    events_tbl$flavour[idx_tp] <- tp
                } else {
                    for (inc in inc_vals) {
                        idx_inc <- idx_tp[which(
                            as.character(events_tbl[[inc_col]][idx_tp]) == inc
                        )]
                        events_tbl$flavour[idx_inc] <- paste0(tp, "_", inc)
                    }
                }
            }
        }
    }

    return(events_tbl)
}

#' Mark rows that belong to the initialization window
#'
#' Add or update a logical `history` column indicating whether each event
#' occurred before the preprocessing `startTime`. Rows with `time < startTime`
#' get `history = TRUE`.
#'
#' @param events_tbl A data.frame/tibble containing an event `time` column.
#' @param parsing_info (Unused) placeholder for compatibility with parsing
#'   workflow.
#' @param preprocessing_opt A list that may contain `startTime` numeric.
#' @return `events_tbl` with a `history` logical column.
#' @keywords internal
#' @noRd
add_window_events <- function(events_tbl, parsing_info, preprocessing_opt) {
    if (is.null(preprocessing_opt) || is.null(preprocessing_opt$startTime)) {
        return(events_tbl)
    }
    start_time <- preprocessing_opt$startTime
    if (is.infinite(start_time)) {
        return(events_tbl)
    }
    if (!"history" %in% names(events_tbl)) {
        events_tbl <- dplyr::mutate(events_tbl, history = FALSE)
    }
    events_tbl <- dplyr::mutate(
        events_tbl,
        history = dplyr::coalesce(history, FALSE),
        history = dplyr::if_else(
            !is.na(time) & time < start_time,
            TRUE,
            history
        )
    )
    return(events_tbl)
}

#' @importFrom tibble rowid_to_column
#' @importFrom dplyr filter slice_head pull
#' Find pointer to first row at or after a target time
#'
#' Locate the first row index in `tbl` whose `time` is greater than or
#' equal to `time`. Returns `1` when `time` is not finite (start of table),
#' and `NA_integer_` when no matching row exists or `tbl` lacks a `time`
#' column.
#'
#' @param tbl A data.frame/tibble with a `time` column.
#' @param time Numeric target time.
#' @return Integer scalar row index (1-based) or `NA_integer_`.
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr filter slice_head pull
#' @keywords internal
#' @noRd
get_pointer <- function(tbl, time) {
    if (is.null(tbl)) {
        return(NA_integer_)
    }
    if (!is.finite(time)) {
        return(1L)
    }

    if (!"time" %in% names(tbl)) {
        return(NA_integer_)
    }
    target_time <- time
    idx <- tbl %>%
        tibble::rowid_to_column('.orig_row') %>%
        dplyr::filter(!is.na(.data$time) & .data$time >= target_time) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::pull(.data$.orig_row)
    if (length(idx) == 0) {
        return(NA_integer_)
    }
    as.integer(idx[1])
}


#' Prepare event tables for preprocessing
#'
#' Extract and filter `network_events` (edges) and `covariate_events`
#' (graph attribute `changes`) from `data`, apply time windowing and order
#' rows. Returns a list with both tables and a `pointer` giving starting
#' indices for each table relative to a `startTime`.
#'
#' @param data A tidygraph/igraph graph object where edges represent tie
#'   events and graph attribute `changes` (accessible via
#'   `igraph::graph_attr(data, "changes")`) holds covariate change events.
#' @param parsing_info Placeholder for compatibility; not used.
#' @param preprocessing_opt A list that can contain `startTime` and
#'   `end_time` to filter the events.
#' @return A list with elements `network_events`, `covariate_events`, and
#'   `pointer` (named integer vector of start indices).
#' @keywords internal
#' @noRd
prepare_events <- function(
    data,
    parsing_info = NULL,
    preprocessing_opt = list()
) {
    if (is.null(preprocessing_opt)) {
        preprocessing_opt <- list()
    }
    startTime <- if (!is.null(preprocessing_opt$startTime)) {
        preprocessing_opt$startTime
    } else {
        -Inf
    }
    end_time <- if (!is.null(preprocessing_opt$end_time)) {
        preprocessing_opt$end_time
    } else {
        Inf
    }

    ties_tbl <- data %>%
        activate(edges) %>%
        as_tibble()

    changes_tbl <- igraph::graph_attr(data, "changes")

    # Ties: filter and order
    if (!is.infinite(end_time)) {
        ties_tbl <- dplyr::filter(ties_tbl, is.na(time) | time <= end_time)
    }
    ties_tbl <- dplyr::arrange(ties_tbl, is.na(time), time)

    pointer <- get_pointer(ties_tbl, start_time)

    # Covariate changes: filter and order
    if (!is.null(changes_tbl) && nrow(changes_tbl) > 0) {
        # if (!is.infinite(startTime)) {
        #     changes_tbl <- dplyr::filter(
        #         changes_tbl,
        #         is.na(time) | time >= startTime
        #     )
        # }
        if (!is.infinite(end_time)) {
            changes_tbl <- dplyr::filter(
                changes_tbl,
                is.na(time) | time <= end_time
            )
        }

        changes_tbl <- dplyr::arrange(changes_tbl, is.na(time), time)
        pointer <- c(pointer, get_pointer(changes_tbl, startTime))
    } else {
        pointer <- c(pointer, NA)
    }
    pointer <- purrr::set_names(
        pointer,
        c("network_events", "covariate_events")
    )
    return(
        list(
            network_events = ties_tbl,
            covariate_events = changes_tbl,
            pointer = pointer
        )
    )
}
