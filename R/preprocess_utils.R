split_flavours <- function(
    events_tbl,
    flavour_map = c("1" = "creation", "0" = "deletion")
) {
    # If flavour column already exists and has non-NA values, leave it alone
    if ("flavour" %in% names(events_tbl) && any(!is.na(events_tbl$flavour))) {
        return(events_tbl)
    }

    # helper to pick the column to inspect for increment-like values
    pick_inc_col <- function(df) {
        for (nm in c("increment", "replace", "weight")) {
            if (nm %in% names(df)) return(nm)
        }
        return(NULL)
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

add_window_events <- function(events_tbl, parsing_info, preprocessing_opt) {
    if (is.null(preprocessing_opt) || is.null(preprocessing_opt$startTime)) {
        return(events_tbl)
    }
    start_time <- preprocessing_opt$startTime
    if (is.infinite(start_time)) {
        return(events_tbl)
    }
    if (!"history" %in% names(events_tbl)) {
        events_tbl$history <- FALSE
    }
    events_tbl$history <- ifelse(
        !is.na(events_tbl$time) & events_tbl$time < start_time,
        TRUE,
        events_tbl$history
    )
    return(events_tbl)
}

order_events <- function(tbl) {
    if (nrow(tbl) == 0) {
        tbl$event_id <- integer()
        return(tbl)
    }
    # order with NA last
    ord <- order(tbl$time, na.last = TRUE)
    out <- tibble::as_tibble(tbl[ord, , drop = FALSE])
    out$event_id <- seq_len(nrow(out))
    return(out)
}

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
    endTime <- if (!is.null(preprocessing_opt$endTime)) {
        preprocessing_opt$endTime
    } else {
        Inf
    }

    ties_tbl <- tryCatch(
        {
            data %>%
                activate(edges) %>%
                as_tibble()
        },
        error = function(e) NULL
    )

    changes_tbl <- tryCatch(
        {
            data %>%
                activate(changes) %>%
                as_tibble()
        },
        error = function(e) NULL
    )

    # Filter by parsing_info types/objects if provided
    if (!is.null(parsing_info)) {
        types_keep <- NULL
        if (!is.null(parsing_info$types)) {
            types_keep <- unique(as.character(parsing_info$types))
        } else if (
            !is.null(parsing_info$objects) &&
                is.data.frame(parsing_info$objects)
        ) {
            if ("name" %in% names(parsing_info$objects)) {
                types_keep <- unique(as.character(parsing_info$objects$name))
            }
        }
        if (!is.null(types_keep) && length(types_keep) > 0) {
            ties_tbl <- dplyr::filter(
                ties_tbl,
                is.na(type) | type %in% types_keep
            )
        }
    }

    ties_tbl <- split_flavours(
        ties_tbl,
        flavour_map = flavour_map
    )

    ties_tbl <- add_window_events(ties_tbl, parsing_info, preprocessing_opt)

    # Keep events in observation window or history events required to reconstruct initial state
    if (!is.infinite(startTime)) {
        keep_idx <- ties_tbl$history |
            is.na(ties_tbl$time) |
            ties_tbl$time >= startTime
    } else {
        keep_idx <- rep(TRUE, nrow(ties_tbl))
    }
    if (!is.infinite(endTime)) {
        keep_idx <- keep_idx & (is.na(ties_tbl$time) | ties_tbl$time <= endTime)
    }
    ties_tbl <- ties_tbl[keep_idx, , drop = FALSE]
    ties_tbl <- order_events(ties_tbl)

    # Covariate changes: filter and order
    if (!is.null(changes_tbl) && nrow(changes_tbl) > 0) {
        keep_ch <- rep(TRUE, nrow(changes_tbl))
        if (!is.infinite(startTime)) {
            keep_ch <- keep_ch &
                (is.na(changes_tbl$time) | changes_tbl$time >= startTime)
        }
        if (!is.infinite(endTime)) {
            keep_ch <- keep_ch &
                (is.na(changes_tbl$time) | changes_tbl$time <= endTime)
        }
        changes_tbl <- changes_tbl[keep_ch, , drop = FALSE]
        if (nrow(changes_tbl) > 0) {
            changes_tbl <- tibble::as_tibble(changes_tbl[
                order(changes_tbl$time, na.last = TRUE),
                ,
                drop = FALSE
            ])
            changes_tbl$change_id <- seq_len(nrow(changes_tbl))
        }
    }

    return(
        list(
            network_events = ties_tbl,
            covariate_events = changes_tbl
        )
    )
}
