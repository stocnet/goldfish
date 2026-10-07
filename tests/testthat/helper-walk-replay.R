# Shared by the walk-handle tests and the simulation cross-validation
# fixtures, which replay each constrained fixture through the handle.

# Replay a specification's observed schedule through the handle and compare
# each fid's live evaluation at its own dependent events against the batch
# materialization. Returns the number of comparisons, of excluded
# alternatives among the compared choice rows and of senders gated out of the
# compared rate rows, so a test can confirm the risk set it compared actually
# excluded something.
walk_replay_against_batch <- function(spec) {
  batch <- suppressWarnings(preprocess_joint(spec))
  map <- attr(batch, "process_map")
  handle <- suppressWarnings(walk_open(spec))
  schedule <- handle$schedule
  dep_count <- stats::setNames(integer(nrow(map)), as.character(map$fid))
  n_checked <- 0L
  n_excluded <- 0L
  n_gated <- 0L
  # Driven by schedule position, one stamp at a time: every dependent row
  # sharing a stamp is evaluated first, over the state strictly before it,
  # and only then are the covariate rows of that stamp applied in schedule
  # order, so no row stamped at t informs an evaluation at t. An exogenous
  # row is reached through walk_advance(), which owns its cursor; a focal
  # covariate row is the realization of a dependent event, which the driver
  # injects.
  stamp_end <- c(which(diff(schedule$time) != 0), schedule$n)
  stamp_start <- c(1L, stamp_end[-length(stamp_end)] + 1L)
  apply_row <- function(k) {
    if (!(schedule$layer[k] %in% handle$focal_layers)) {
      walk_advance(handle, schedule$time[k])
      return(invisible(NULL))
    }
    event <- list(
      layer = schedule$layer[k],
      sender = schedule$sender[k],
      receiver = schedule$receiver[k],
      time = schedule$time[k]
    )
    event[[schedule$semantics[k]]] <- schedule$value[[k]]
    walk_inject(handle, event)
  }
  for (g in seq_along(stamp_end)) {
    rows <- seq.int(stamp_start[g], stamp_end[g])
    for (k in rows[schedule$dependent[rows]]) {
      # The composition in force at a dependent event includes changes at its
      # own stamp; the exogenous rows sharing that stamp come after it.
      walk_advance_presence(handle, schedule$time[k])
      # A layer with one modeled flavor schedules its dependent rows with no
      # flavor: every one of them belongs to that flavor's processes.
      own <- map$fid[
        map$layer == schedule$layer[k] &
          (is.na(map$flavor) |
            is.na(schedule$flavor[k]) |
            map$flavor %in% schedule$flavor[k])
      ]
      for (fid in own) {
        key <- as.character(fid)
        dep_count[[key]] <- dep_count[[key]] + 1L
        row <- map[map$fid == fid, ]
        is_rate <- identical(row$family, "rate")
        out <- batch[[key]]
        # A dyad family's risk set is the whole grid: REM and coordination are
        # compared on every dyad, the self-dyads aside when counting exclusions.
        dyadic <- switch(
          row$stat_block,
          "REM:rate" = "REM",
          "REM:rate_ordered" = "REM-ordered",
          "DyNAM:choice_coordination" = "DyNAM-MM",
          NULL
        )
        n_parameters <- if (length(dim(out$initial_stats)) == 2L) {
          ncol(out$initial_stats) + row$has_intercept
        } else {
          dim(out$initial_stats)[3] +
            if (is.null(dyadic)) 0L else row$has_intercept
        }
        theta <- rep_len(c(0.4, -0.3, 0.25), n_parameters)
        live <- walk_evaluate(handle, fid, theta)
        model_type <- dyadic %||% if (is_rate) "DyNAM-M-Rate" else "DyNAM-M"
        state <- materialize_process_state(
          out,
          model_type = model_type,
          event_index = which(out$is_dependent == 1L)[dep_count[[key]]],
          has_intercept = row$has_intercept
        )
        reference <- evaluate_process_state(state, theta)$value
        if (!is.null(dyadic)) {
          expect_equal(live$value, reference)
          off_diagonal <- live$index[[1L]] != live$index[[2L]]
          n_excluded <- n_excluded + sum(live$value[off_diagonal] == 0)
        } else if (is_rate) {
          expect_equal(live$value, reference)
          n_gated <- n_gated + sum(live$value == 0)
        } else {
          sender <- schedule$sender[k]
          n2 <- state$n_actors2
          live_row <- live$value[(sender - 1L) * n2 + seq_len(n2)]
          expect_equal(live_row, reference)
          n_excluded <- n_excluded + sum(live_row[-sender] == 0)
        }
        n_checked <- n_checked + 1L
      }
    }
    for (k in rows[!schedule$dependent[rows]]) {
      apply_row(k)
    }
  }
  list(n_checked = n_checked, n_excluded = n_excluded, n_gated = n_gated)
}
