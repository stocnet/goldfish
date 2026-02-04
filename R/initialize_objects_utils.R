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

    # Handle increments: sum per dyad and add to matrix
    if ('increment' %in% names(rows_valid)) {
        inc_idx <- which(!is.na(rows_valid$increment))
        if (length(inc_idx) > 0) {
            inc_si <- si[inc_idx]
            inc_ri <- ri[inc_idx]
            inc_vals <- as.integer(rows_valid$increment[inc_idx])
            keys <- paste(inc_si, inc_ri, sep = '_')
            sums <- tapply(inc_vals, keys, sum)
            for (k in names(sums)) {
                parts <- as.integer(strsplit(k, '_', fixed = TRUE)[[1]])
                mat[parts[1], parts[2]] <- mat[parts[1], parts[2]] +
                    as.integer(sums[[k]])
            }
        }
    }

    # Handle replacements/weights/defaults: take last value per dyad
    # Build a small tibble with si, ri and candidate values
    rows_valid$.si <- si
    rows_valid$.ri <- ri
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
    col <- rep(NA, n_nodes)
    for (i in seq_len(nrow(last_vals))) {
        ni_i <- as.integer(last_vals$.ni[i])
        col[ni_i] <- last_vals$.val[i]
    }
    col
}

# Impute missing values in attributes matrix.
# Numeric columns: impute mean (0 if all NA). Non-numeric: impute mode.
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
impute_networks <- function(networks) {
    if (is.null(networks) || length(networks) == 0) {
        return(networks)
    }
    for (nm in names(networks)) {
        m <- networks[[nm]]
        m[is.na(m)] <- 0L
        storage.mode(m) <- 'integer'
        networks[[nm]] <- m
    }
    networks
}

# Initialize data objects (networks and nodal attributes) for preprocessing
initialize_objects <- function(
    events, # output of prepare_events(): list(network_events, covariate_events, pointer)
    data, # igraph/tidygraph graph with vertex set
    start_time = -Inf
) {
    ties_tbl <- events$network_events
    changes_tbl <- events$covariate_events

    n_nodes <- manynet::net_nodes(data)
    vnames <- data %>% activate(nodes) %>% as_tibble() %>% pull(1)

    networks <- list()
    if (!is.null(ties_tbl) && nrow(ties_tbl) > 0) {
        net_types <- unique(as.character(ties_tbl$type))
        networks <- lapply(net_types, function(nt) {
            # build an n x n matrix for this network (assume one-mode by default)
            mat <- matrix(0L, nrow = n_nodes, ncol = n_nodes)
            rownames(mat) <- vnames
            colnames(mat) <- vnames

            rows_nt <- dplyr::filter(ties_tbl, as.character(.data$type) == nt)

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
                mat <- update_network_from_rows(
                    mat,
                    initial_rows,
                    vnames
                )
            }

            mat
        })
        names(networks) <- net_types
        # convert list of matrices to 3D array: [n_nodes, n_nodes, n_networks]
        if (length(networks) > 0) {
            networks <- simplify2array(networks)
            # ensure third dimension exists even for single network
            if (length(dim(networks)) == 2) {
                dim(networks) <- c(dim(networks), 1L)
            }
            dimnames(networks) <- list(
                rownames(networks),
                colnames(networks),
                names(networks)
            )
        }
    }

    # Attributes / nodal covariates
    attributes <- NULL
    if (!is.null(changes_tbl) && nrow(changes_tbl) > 0) {
        # derive variable names from columns (exclude node and time)
        vars <- setdiff(colnames(changes_tbl), c("node", "time"))
        attributes <- matrix(NA, nrow = n_nodes, ncol = length(vars))
        rownames(attributes) <- vnames
        colnames(attributes) <- vars

        # build attributes matrix columns using sapply over vars (wide-format)
        attrs_mat <- sapply(
            vars,
            function(v) {
                build_attribute_column(
                    changes_tbl,
                    "node",
                    v,
                    vnames,
                    n_nodes,
                    start_time
                )
            },
            simplify = TRUE,
            USE.NAMES = TRUE
        )
        # ensure matrix shape when single variable
        if (is.null(dim(attrs_mat))) {
            attrs_mat <- matrix(
                attrs_mat,
                ncol = 1,
                dimnames = list(vnames, vars)
            )
        } else {
            rownames(attrs_mat) <- vnames
            colnames(attrs_mat) <- vars
        }
        attributes <- attrs_mat

        # impute missing attribute values using helper
        attributes <- impute_attributes(attributes)
    }

    # final imputation for networks
    networks <- impute_networks(networks)

    return(list(
        networks = networks,
        attributes = attributes
    ))
}
