#' Get number of nodes per network type
#'
#' Computes counts of nodes classified as "agent" and "no-agent" for
#' each network type declared in the `networks_meta` attribute of `data`.
#'
#' @param data A tidygraph/igraph-like graph object with a vertex set and
#'   an attribute `networks_meta` (data.frame/tibble) describing network
#'   types and optional `nodeset_def` column.
#' @return An integer matrix with two rows ("agent", "no-agent") and one
#'   column per network type. If no network types are present, returns an
#'   empty 2 x 0 integer matrix.
#' @keywords internal
#' @noRd
get_nnodes_per_type <- function(data) {
    networks_meta <- attr(data, "networks_meta")
    net_types <- unique(as.character(networks_meta$type))
    node_tbl <- data %>% activate(nodes) %>% as_tibble()
    # Vectorized: compute counts per type (rows: "agent", "no-agent")
    if (length(net_types) == 0) {
        return(matrix(
            0L,
            nrow = 2,
            ncol = 0,
            dimnames = list(c("agent", "no-agent"), character(0))
        ))
    }

    counts <- vapply(
        net_types,
        FUN = function(nt) {
            nodeset_def <- NULL
            if (
                !is.null(networks_meta) &&
                    "nodeset_def" %in% colnames(networks_meta)
            ) {
                rows_meta <- tibble::as_tibble(networks_meta)
                rows_meta <- rows_meta[
                    as.character(rows_meta$type) == nt,
                    ,
                    drop = FALSE
                ]
                if (nrow(rows_meta) >= 1) {
                    nd_val <- rows_meta$nodeset_def[1]
                    if (!is.na(nd_val) && nzchar(as.character(nd_val))) {
                        nodeset_def <- as.character(nd_val)
                    }
                }
            }
            if (!is.null(nodeset_def) && nodeset_def %in% colnames(node_tbl)) {
                a <- sum(node_tbl[[nodeset_def]] == "agent", na.rm = TRUE)
                na <- sum(node_tbl[[nodeset_def]] == "no-agent", na.rm = TRUE)
                c(as.integer(a), as.integer(na))
            } else {
                c(as.integer(manynet::net_nodes(data)), 0L)
            }
        },
        FUN.VALUE = integer(2)
    )
    rownames(counts) <- c("agent", "no-agent")
    colnames(counts) <- net_types
    counts
}


#' Map names to integer indices
#'
#' Convert a character or numeric identifier `x` to an integer index using
#' `vnames_local`. If `vnames_local` is not provided the function attempts to
#' recover a `vnames` object from the parent frame. Returns `NA_integer_`
#' when mapping is not possible.
#'
#' @param x A scalar or vector of names or numeric indices.
#' @param vnames_local Optional character vector of names used for matching.
#' @return An integer vector of indices or `NA_integer_` when unmapped.
#' @keywords internal
#' @noRd
map_to_index <- function(x, vnames_local = NULL) {
    if (is.null(x)) {
        return(NA_integer_)
    }
    if (is.numeric(x)) {
        return(as.integer(x))
    }
    # determine vnames to use
    if (is.null(vnames_local)) {
        vnames_local <- tryCatch(
            get("vnames", envir = parent.frame()),
            error = function(e) NULL
        )
    }
    xi <- as.character(x)
    if (is.null(vnames_local)) {
        return(NA_integer_)
    }
    idx <- match(xi, vnames_local)
    as.integer(idx)
}


# Apply a batch of edge rows to a network matrix `mat`.
# - rows: tibble/data.frame with endpoint columns (as detected by `endpoints`)
# The function aggregates `increment` values by dyad (summing) and applies
# the last `replace`/`weight` value per dyad, falling back to 1 when needed.
#' Apply a batch of edge rows to a network matrix
#'
#' Apply a set of edge rows (as produced by event preprocessing) to an
#' existing adjacency/membership matrix. The function supports three
#' behaviours:
#' - `increment` values are summed per dyad and added to the cell value.
#' - `replace` (preferred) and `weight` values are considered per dyad; the
#'   last non-NA value (by row order) is written to the matrix.
#' - Missing index mappings are ignored.
#'
#' @param mat Integer matrix representing the current network (rows: senders,
#'   cols: receivers).
#' @param rows A data.frame/tibble containing edge rows with `from`, `to`
#'   columns (can be indices or names) and optional `increment`, `replace`,
#'   or `weight` columns.
#' @param vnames_local Optional character vector of vertex names for mapping
#'   character endpoints to indices.
#' @return The modified `mat` with increments and replacements applied.
#' @keywords internal
#' @noRd
update_network_from_rows <- function(
    mat,
    rows,
    vnames_local = NULL
) {
    if (is.null(rows) || nrow(rows) == 0) {
        return(mat)
    }

    si <- rows[["from"]]
    ri <- rows[["to"]]

    # si <- map_to_index(s_vec, vnames_local = vnames_local)
    # ri <- map_to_index(r_vec, vnames_local = vnames_local)

    valid <- !is.na(si) & !is.na(ri)
    if (!any(valid)) {
        return(mat)
    }
    si <- si[valid]
    ri <- ri[valid]
    rows_valid <- rows[valid, , drop = FALSE]

    # attach mapped indices for dyad grouping
    rows_valid$.si <- si
    rows_valid$.ri <- ri

    # Handle increments: sum per dyad and add to matrix (dplyr grouping)
    if ('increment' %in% names(rows_valid)) {
        inc_df <- rows_valid %>%
            dplyr::filter(!is.na(.data$increment)) %>%
            dplyr::group_by(.data$.si, .data$.ri) %>%
            dplyr::summarise(
                sum_inc = sum(as.integer(.data$increment), na.rm = TRUE),
                .groups = "drop"
            )
        if (nrow(inc_df) > 0) {
            for (i in seq_len(nrow(inc_df))) {
                si_i <- as.integer(inc_df$.si[i])
                ri_i <- as.integer(inc_df$.ri[i])
                mat[si_i, ri_i] <- mat[si_i, ri_i] +
                    as.integer(inc_df$sum_inc[i])
            }
        }
    }

    # Handle replacements/weights/defaults: take last value per dyad
    # Build a small tibble with candidate values (indices already attached above)
    # prefer replace, then weight, then default 1
    rows_valid$.set_value <- NA
    if ('replace' %in% names(rows_valid)) {
        rows_valid$.set_value[!is.na(rows_valid$replace)] <- rows_valid$replace[
            !is.na(rows_valid$replace)
        ]
    }
    if ('weight' %in% names(rows_valid)) {
        rows_valid$.set_value[
            is.na(rows_valid$.set_value) & !is.na(rows_valid$weight)
        ] <- rows_valid$weight[
            is.na(rows_valid$.set_value) & !is.na(rows_valid$weight)
        ]
    }
    rows_valid$.set_value[is.na(rows_valid$.set_value)] <- 1L

    # group by dyad and take last row (preserves order of rows_valid)
    if (nrow(rows_valid) > 0) {
        rows_grouped <- dplyr::group_by(rows_valid, .data$.si, .data$.ri)
        last_per_dyad <- dplyr::slice_tail(rows_grouped, n = 1L)
        last_per_dyad <- dplyr::ungroup(last_per_dyad)
        for (i in seq_len(nrow(last_per_dyad))) {
            si_i <- as.integer(last_per_dyad$.si[i])
            ri_i <- as.integer(last_per_dyad$.ri[i])
            val <- last_per_dyad$.set_value[i]
            # Only set when a non-increment candidate was present; increments are already handled
            mat[si_i, ri_i] <- val
        }
    }

    mat
}


# Build a single attribute column vector from rows for a given variable
#' Build a nodal attribute vector from event rows
#'
#' For a single attribute variable, this function computes the last observed
#' (pre-`start_time`) value per node and returns a vector of length
#' `n_nodes` indexed by `vnames`.
#'
#' @param rows_v A data.frame/tibble of covariate change events containing
#'   a node identifier column and a value column.
#' @param node_col The name of the column that identifies the node in
#'   `rows_v`.
#' @param val_col The name of the column that holds the attribute value.
#' @param vnames Character vector of vertex names (order corresponds to
#'   resulting vector indices).
#' @param n_nodes Integer number of nodes (length of `vnames`).
#' @param start_time Numeric threshold; only rows with `time < start_time`
#'   are considered for initialization.
#' @return A vector of length `n_nodes` with attribute values (or `NA`).
#' @keywords internal
#' @noRd
build_attribute_column <- function(
    rows_v,
    node_col,
    val_col,
    vnames,
    n_nodes,
    start_time
) {
    if (is.null(rows_v) || nrow(rows_v) == 0) {
        return(rep(NA, n_nodes))
    }
    if (is.infinite(start_time)) {
        return(rep(NA, n_nodes))
    }
    init_rows <- dplyr::filter(
        rows_v,
        !is.na(.data$time) & .data$time < start_time
    )
    if (nrow(init_rows) == 0) {
        return(rep(NA, n_nodes))
    }
    nodes <- init_rows[[node_col]]
    ni <- map_to_index(nodes, vnames_local = vnames)
    valid <- !is.na(ni)
    if (!any(valid)) {
        return(rep(NA, n_nodes))
    }
    rows_valid <- init_rows[valid, , drop = FALSE]
    rows_valid$.ni <- ni[valid]
    rows_valid$.val <- rows_valid[[val_col]]
    last_vals <- rows_valid %>%
        dplyr::group_by(.data$.ni) %>%
        dplyr::slice_tail(n = 1L) %>%
        dplyr::ungroup()
    # Vectorized assignment: build named vector of last values and assign by index
    col <- rep(NA, n_nodes)
    if (nrow(last_vals) > 0) {
        vals_vec <- last_vals$.val
        names(vals_vec) <- as.character(last_vals$.ni)
        idxs <- as.integer(names(vals_vec))
        col[idxs] <- vals_vec
    }
    col
}

# Impute missing values in attributes matrix.
# Numeric columns: impute mean (0 if all NA). Non-numeric: impute mode.
#' Impute missing attribute values
#'
#' Numeric columns are imputed with the column mean (or 0 if the mean is
#' not defined). Non-numeric columns are imputed with the modal value. If a
#' column is completely `NA`, it remains `NA` (for non-numeric) or becomes 0
#' (for numeric) according to the rules above.
#'
#' @param attributes A matrix-like object with nodal attributes (rows:
#'   nodes, cols: variables).
#' @return The same matrix with missing values imputed.
#' @keywords internal
#' @noRd
impute_attributes <- function(attributes) {
    if (is.null(attributes) || ncol(attributes) == 0) {
        return(attributes)
    }
    for (j in seq_len(ncol(attributes))) {
        colj <- attributes[, j]
        # treat as numeric if coercible
        numv <- suppressWarnings(as.numeric(colj))
        if (all(is.na(numv))) {
            # non-numeric: mode imputation
            vals <- colj[!is.na(colj)]
            if (length(vals) == 0) {
                attributes[, j] <- NA
            } else {
                uniq <- unique(vals)
                modev <- uniq[which.max(tabulate(match(vals, uniq)))]
                colj[is.na(colj)] <- modev
                attributes[, j] <- colj
            }
        } else {
            # numeric: impute mean (0 if mean NA)
            m <- mean(numv, na.rm = TRUE)
            if (is.na(m)) {
                m <- 0
            }
            numv[is.na(numv)] <- m
            attributes[, j] <- numv
        }
    }
    attributes
}

# Impute networks: replace NA with 0 and ensure integer storage
#' Impute missing values in network arrays
#'
#' Recursively traverse lists and arrays/matrices replacing `NA` with `0L`
#' and ensuring integer storage mode. Useful to produce clean network arrays
#' for model input.
#'
#' @param networks A matrix, array or (possibly nested) list of such
#'   structures representing network slices.
#' @return A structure mirroring `networks` with `NA`s replaced by zeros and
#'   integer storage enforced where applicable.
#' @keywords internal
#' @noRd
impute_networks <- function(networks) {
    # Recursively traverse lists/arrays and replace NA with 0L, ensuring integer storage
    if (is.null(networks) || length(networks) == 0) {
        return(networks)
    }
    if (is.list(networks) && !is.array(networks)) {
        for (nm in names(networks)) {
            networks[[nm]] <- impute_networks(networks[[nm]])
        }
        return(networks)
    }
    # At this point, networks is an atomic matrix or array
    if (
        is.matrix(networks) ||
            (is.array(networks) && length(dim(networks)) >= 2)
    ) {
        networks[is.na(networks)] <- 0L
        storage.mode(networks) <- 'integer'
        return(networks)
    }
    networks
}


# helper to get nodeset_def and two_mode flag for a network type
#' Extract metadata for a network type
#'
#' Retrieve the `nodeset_def` name (if any) and the `two_mode` logical flag
#' for a given network type from the `networks_meta` table.
#'
#' @param nt Character scalar giving the network type.
#' @param networks_meta A data.frame/tibble describing network metadata.
#' @return A named list with elements `nodeset_def` (character or NULL) and
#'   `two_mode` (logical).
#' @keywords internal
#' @noRd
meta_for_type <- function(nt, networks_meta) {
    nodeset_def <- NULL
    two_mode_flag <- FALSE
    if (!is.null(networks_meta)) {
        rows_meta <- tibble::as_tibble(networks_meta)
        rows_meta <- rows_meta[
            as.character(rows_meta$type) == nt,
            ,
            drop = FALSE
        ]
        if (nrow(rows_meta) >= 1) {
            if ("nodeset_def" %in% colnames(rows_meta)) {
                nd_val <- rows_meta$nodeset_def[1]
                if (!is.na(nd_val) && nzchar(as.character(nd_val))) {
                    nodeset_def <- as.character(nd_val)
                }
            }
            if ("two_mode" %in% colnames(rows_meta)) {
                two_mode_flag <- as.logical(rows_meta$two_mode[1])
            }
        }
    }
    list(nodeset_def = nodeset_def, two_mode = two_mode_flag)
}

#' Build initial matrix information for a network type
#'
#' Construct an initial adjacency/membership matrix (or a two-mode matrix)
#' for the given network type using `ties_tbl` and `node_tbl`. The returned
#' list contains the network `type`, a logical `two_mode` flag and the
#' constructed integer `mat` with row/col names.
#'
#' @param nt Character scalar network type.
#' @param data Graph object used to consult `networks_meta`.
#' @param ties_tbl Tibble/data.frame of tie events with `from`, `to`,
#'   `time` and `type` columns.
#' @param node_tbl Tibble/data.frame of nodes (first column must be names).
#' @param vnames Character vector of vertex names.
#' @param start_time Numeric threshold; only ties with `time < start_time`
#'   are used to initialize the matrix.
#' @return A list with elements `type`, `two_mode`, and `mat` (integer
#'   matrix).
#' @keywords internal
#' @noRd
mats_info_fun <- function(nt, data, ties_tbl, node_tbl, vnames, start_time) {
    meta <- meta_for_type(nt, networks_meta = attr(data, "networks_meta"))
    nodeset_def <- meta$nodeset_def
    two_mode_flag <- isTRUE(meta$two_mode)

    rows_nt <- dplyr::filter(ties_tbl, as.character(.data$type) == nt)

    # determine row/col names based on nodeset_def and two_mode
    if (!is.null(nodeset_def) && nodeset_def %in% colnames(node_tbl)) {
        node_names <- node_tbl[[1]]
        agent_names <- node_names[node_tbl[[nodeset_def]] == "agent"]
        noagent_names <- node_names[
            node_tbl[[nodeset_def]] == "no-agent"
        ]
    } else {
        agent_names <- vnames
        noagent_names <- character(0)
    }

    if (two_mode_flag) {
        mat <- matrix(
            0L,
            nrow = length(agent_names),
            ncol = length(noagent_names)
        )
        rownames(mat) <- agent_names
        colnames(mat) <- noagent_names
    } else {
        # one-mode: include both agent and no-agent membership if available
        if (!is.null(nodeset_def) && nodeset_def %in% colnames(node_tbl)) {
            member_names <- node_tbl[[1]][
                node_tbl[[nodeset_def]] %in% c("agent", "no-agent")
            ]
        } else {
            member_names <- vnames
        }
        mat <- matrix(
            0L,
            nrow = length(member_names),
            ncol = length(member_names)
        )
        rownames(mat) <- member_names
        colnames(mat) <- member_names
    }

    # initial state rows are those with time strictly smaller than start_time
    if (!is.infinite(start_time)) {
        initial_rows <- dplyr::filter(
            rows_nt,
            !is.na(.data$time) & .data$time < start_time
        )
    } else {
        initial_rows <- dplyr::filter(rows_nt, FALSE)
    }
    if (nrow(initial_rows) > 0) {
        # map endpoints to indices within this mat
        # endpoints in initial_rows are expected to be `from` and `to` matching vnames
        # map_to_index expects vnames for this matrix: use rownames/colnames
        from_idx <- map_to_index(
            initial_rows$from,
            vnames_local = rownames(mat)
        )
        to_idx <- map_to_index(
            initial_rows$to,
            vnames_local = colnames(mat)
        )
        rows_mapped <- initial_rows
        rows_mapped$from <- from_idx
        rows_mapped$to <- to_idx
        mat <- update_network_from_rows(
            mat,
            rows_mapped,
            vnames_local = c(rownames(mat), colnames(mat))
        )
    }

    list(type = nt, two_mode = two_mode_flag, mat = mat)
}

# Initialize data objects (networks and nodal attributes) for preprocessing
#' Initialize networks and nodal attributes for preprocessing
#'
#' From the output of `prepare_events()` and a graph `data` build initial
#' network matrices and nodal attribute matrices representing the state at
#' `start_time`. Networks are grouped by type and dimensions; attribute
#' columns are imputed and networks have `NA`s replaced by zeros.
#'
#' @param events A list as returned by `prepare_events()` with elements
#'   `network_events` and `covariate_events`.
#' @param data A tidygraph/igraph-like graph object containing node data and
#'   (optionally) `networks_meta` in its attributes.
#' @param start_time Numeric threshold (default `-Inf`) indicating the time
#'   before which events are considered part of the initial state.
#' @return A list with components `networks` (a list with `one_mode` and
#'   `two_mode` network arrays/matrices) and `attributes` (a matrix of nodal
#'   covariates or `NULL`).
#' @keywords internal
#' @noRd
initialize_objects <- function(
    events, # output of prepare_events(): list(network_events, covariate_events, pointer)
    data, # igraph/tidygraph graph with vertex set
    start_time = -Inf
) {
    ties_tbl <- events$network_events
    changes_tbl <- events$covariate_events

    # number of nodes per network type: counts of "agent" / "no-agent"
    nn_counts <- get_nnodes_per_type(data)
    node_tbl <- data %>% activate(nodes) %>% as_tibble()
    vnames <- node_tbl %>% pull(1)

    # AQUI

    networks <- list(two_mode = list(), one_mode = list())
    if (!is.null(ties_tbl) && nrow(ties_tbl) > 0) {
        net_types <- unique(as.character(ties_tbl$type))

        mats_info <- lapply(
            net_types,
            mats_info_fun,
            data = data,
            ties_tbl = ties_tbl,
            node_tbl = node_tbl,
            vnames = vnames,
            start_time = start_time
        )

        # Partition by two_mode and then group by matrix dimensions, aggregating equal-dimension mats into arrays
        partitioned <- split(
            mats_info,
            vapply(
                mats_info,
                function(x) as.character(x$two_mode),
                character(1)
            )
        )
        # keys will be "TRUE" or "FALSE"
        for (key in names(partitioned)) {
            group <- partitioned[[key]]
            # group by dimensions
            dims_key <- vapply(
                group,
                function(x) paste(nrow(x$mat), ncol(x$mat), sep = "x"),
                character(1)
            )
            dims_groups <- split(group, dims_key)
            out_list <- list()
            for (dg in names(dims_groups)) {
                gl <- dims_groups[[dg]]
                mats <- lapply(gl, function(x) x$mat)
                if (length(mats) == 1) {
                    out_list[[dg]] <- mats[[1]]
                    names(out_list)[length(out_list)] <- gl[[1]]$type
                } else {
                    arr <- simplify2array(mats)
                    # ensure 3d for single
                    if (length(dim(arr)) == 2) {
                        dim(arr) <- c(dim(arr), 1L)
                    }
                    dimnames(arr) <- list(
                        rownames(mats[[1]]),
                        colnames(mats[[1]]),
                        vapply(gl, function(x) x$type, character(1))
                    )
                    out_list[[dg]] <- arr
                }
            }
            if (key == "TRUE") {
                networks$two_mode <- out_list
            } else {
                networks$one_mode <- out_list
            }
        }
    }

    # Attributes / nodal covariates
    attributes <- NULL
    if (!is.null(changes_tbl) && nrow(changes_tbl) > 0) {
        # derive variable names from columns (exclude node and time)
        vars <- setdiff(colnames(changes_tbl), c("node", "time"))
        attributes <- matrix(NA, nrow = length(vnames), ncol = length(vars))
        rownames(attributes) <- vnames
        colnames(attributes) <- vars
        # number of nodes
        n_nodes <- length(vnames)

        # build attributes matrix columns using purrr::map_dfc (wide-format)
        # ensure the input vector is named so purrr/tibble won't auto-rename columns
        attrs_tbl <- purrr::map_dfc(
            purrr::set_names(vars),
            function(v) {
                build_attribute_column(
                    changes_tbl,
                    "node",
                    v,
                    vnames,
                    n_nodes,
                    start_time
                )
            }
        )
        # ensure matrix shape when single variable
        if (ncol(attrs_tbl) == 0) {
            attributes <- matrix(NA, nrow = n_nodes, ncol = 0)
            rownames(attributes) <- vnames
        } else {
            attrs_mat <- as.matrix(attrs_tbl)
            rownames(attrs_mat) <- vnames
            colnames(attrs_mat) <- vars
            attributes <- attrs_mat
        }

        # impute missing attribute values using helper
        attributes <- impute_attributes(attributes)
    }

    # final imputation for networks
    networks <- lapply(networks, impute_networks)

    return(list(
        networks = networks,
        attributes = attributes
    ))
}
