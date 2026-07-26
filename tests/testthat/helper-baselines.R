# Backends covered by the main coefficient set (v2). gather joined at v2 and is
# the only column computed fresh there; r and cpp are v1's numbers carried
# forward, so the floor frozen at b890cd0 is unchanged.
baselines_backends <- c("r", "cpp", "gather")

# The separately versioned `global_v1` set predates the gather freeze and holds
# no gather column, so it states its own coverage rather than inheriting a
# vector that has since grown. Extending it is a deliberate regeneration, not
# something that should happen by editing a shared constant.
baselines_backends_global <- c("r", "cpp")

baselines_social_evolution_data <- function() {
  data("Social_Evolution", envir = environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = calls,
    nodes = actors
  )
  friendshipNetwork <- make_network(nodes = actors, directed = TRUE)
  friendshipNetwork <- link_events(
    x = friendshipNetwork,
    change_event = friendship,
    nodes = actors
  )
  calls_dependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  make_data(
    calls_dependent,
    call_network,
    friendshipNetwork,
    calls,
    friendship,
    actors
  )
}

baselines_fisheries_data <- function() {
  data("Fisheries_Treaties_6070", envir = environment())
  states <- make_nodes(states)
  states <- link_events(states, sovchanges, attribute = "present")
  states <- link_events(states, regchanges, attribute = "regime")
  states <- link_events(states, gdpchanges, attribute = "gdp")
  bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
  bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
  contignet <- make_network(contignet, nodes = states, directed = FALSE)
  contignet <- link_events(contignet, contigchanges, nodes = states)
  create_bilat <- make_dependent_events(
    events = bilatchanges[bilatchanges$increment == 1, ],
    nodes = states,
    default_network = bilatnet
  )
  make_data(
    create_bilat,
    bilatnet,
    contignet,
    states,
    bilatchanges,
    contigchanges,
    sovchanges,
    regchanges,
    gdpchanges
  )
}

baselines_global_data <- function() {
  data("Social_Evolution", envir = environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = calls,
    nodes = actors
  )
  calls_dependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  seasons <- make_global_attributes(data.frame(winter = 0))
  # POSIXct on the same GMT axis as the (now POSIXct) call times; as.numeric()
  # round-trips to the identical epoch, so the frozen coefficients are unchanged.
  seasonChange <- data.frame(
    time = as.POSIXct(1222553311, origin = "1970-01-01", tz = "GMT"),
    replace = 1
  )
  seasons <- link_events(seasons, seasonChange)
  make_data(calls_dependent, call_network, calls, actors, seasons)
}

baselines_model_grid <- function() {
  list(
    se_dynam_rate = list(
      dataset = "social_evolution",
      model = "DyNAM",
      sub_model = "rate",
      formula = calls_dependent ~ 1 + indeg + outdeg + indeg(friendshipNetwork)
    ),
    se_dynam_rate_ordered = list(
      dataset = "social_evolution",
      model = "DyNAM",
      sub_model = "rate_ordered",
      formula = calls_dependent ~ indeg + outdeg + indeg(friendshipNetwork)
    ),
    se_dynam_choice = list(
      dataset = "social_evolution",
      model = "DyNAM",
      sub_model = "choice",
      formula = calls_dependent ~ inertia + recip + trans
    ),
    se_dynam_choice_coord = list(
      dataset = "social_evolution",
      model = "DyNAM",
      sub_model = "choice_coordination",
      formula = calls_dependent ~ inertia + trans
    ),
    se_rem = list(
      dataset = "social_evolution",
      model = "REM",
      formula = calls_dependent ~ 1 +
        indeg(call_network, type = "ego") +
        inertia +
        recip
    ),
    se_rem_ordered = list(
      dataset = "social_evolution",
      model = "REM",
      sub_model = "rate_ordered",
      formula = calls_dependent ~ indeg(call_network, type = "ego") +
        inertia +
        recip
    ),
    fish_dynam_rate = list(
      dataset = "fisheries",
      model = "DyNAM",
      sub_model = "rate",
      formula = create_bilat ~ 1 + indeg + ego(states$regime)
    ),
    fish_dynam_rate_ordered = list(
      dataset = "fisheries",
      model = "DyNAM",
      sub_model = "rate_ordered",
      formula = create_bilat ~ indeg + ego(states$regime)
    ),
    fish_dynam_choice = list(
      dataset = "fisheries",
      model = "DyNAM",
      sub_model = "choice",
      formula = create_bilat ~ inertia +
        tie(contignet) +
        alter(states$regime) +
        diff(states$regime)
    ),
    fish_dynam_choice_coord = list(
      dataset = "fisheries",
      model = "DyNAM",
      sub_model = "choice_coordination",
      formula = create_bilat ~ inertia +
        tie(contignet) +
        alter(states$regime) +
        diff(states$regime),
      estimation_args = list(initial_damping = 40, max_iterations = 30)
    ),
    fish_rem = list(
      dataset = "fisheries",
      model = "REM",
      formula = create_bilat ~ 1 +
        inertia +
        tie(contignet) +
        alter(states$regime)
    ),
    fish_rem_ordered = list(
      dataset = "fisheries",
      model = "REM",
      sub_model = "rate_ordered",
      formula = create_bilat ~ inertia + tie(contignet) + alter(states$regime)
    )
  )
}

baselines_global_model_grid <- function() {
  list(
    global_dynam_rate = list(
      dataset = "social_evolution_global",
      model = "DyNAM",
      sub_model = "rate",
      formula = calls_dependent ~ 1 + indeg + global(seasons$winter)
    ),
    global_rem = list(
      dataset = "social_evolution_global",
      model = "REM",
      formula = calls_dependent ~ 1 + inertia + global(seasons$winter)
    )
  )
}

baselines_fit <- function(spec, backend, data_list) {
  controlArgs <- c(list(backend = backend), spec$estimation_args)
  args <- list(
    x = spec$formula,
    data = data_list[[spec$dataset]],
    control_algo = do.call(set_algorithm_newton, controlArgs),
    progress = FALSE,
    verbose = FALSE
  )
  if (spec$model == "DyNAM") {
    args$sub_model <- spec$sub_model
    do.call(estimate_dynam, args)
  } else {
    # A REM grid entry may request the ordinal sub-model explicitly; without it
    # `estimate_rem()` uses its `rate` default (waiting times).
    if (!is.null(spec$sub_model)) {
      args$sub_model <- spec$sub_model
    }
    do.call(estimate_rem, args)
  }
}

# Worker count for parallel baseline fitting. Serial on Windows (no fork) and
# for a tiny grid; otherwise detectCores(), overridable via TESTTHAT_CPUS. The
# baseline blocks are skip_on_cran(), so CRAN's 2-core limit does not apply.
baselines_cores <- function(n_jobs) {
  if (.Platform$OS.type == "windows") {
    return(1L)
  }
  env <- Sys.getenv("TESTTHAT_CPUS", "")
  n <- if (nzchar(env)) {
    suppressWarnings(as.integer(env))
  } else {
    parallel::detectCores()
  }
  if (is.na(n) || n < 1L) {
    n <- 1L
  }
  max(1L, min(n, n_jobs))
}

# Fit every (model, backend) cell of a baseline grid up front, in parallel, so
# the per-cell test_that() blocks only assert. The independent fits are the
# suite's dominant cost; forking them (copy-on-write over the parent-loaded
# datasets) collapses that wall time without touching the frozen baselines.
# Returns a named list keyed "<model>::<backend>" holding each fit, or the
# captured error condition so the owning test can re-raise it with the right
# attribution. `run` gates fitting to when the tests will actually execute
# (skip_on_cran() stays authoritative): nothing is fit on CRAN.
baselines_precompute_fits <- function(
  grid,
  get_data,
  backends = baselines_backends,
  run = interactive() || identical(Sys.getenv("NOT_CRAN"), "true")
) {
  keys <- expand.grid(
    model = names(grid),
    backend = backends,
    stringsAsFactors = FALSE
  )
  labels <- paste(keys$model, keys$backend, sep = "::")
  if (!run) {
    return(stats::setNames(vector("list", length(labels)), labels))
  }
  # Load each dataset once in the parent so forked workers inherit it via
  # copy-on-write rather than re-reading it per cell.
  for (ds in unique(vapply(grid, `[[`, character(1), "dataset"))) {
    get_data(ds)
  }
  fit_cell <- function(i) {
    tryCatch(
      {
        spec <- grid[[keys$model[i]]]
        suppressWarnings(
          baselines_fit(spec, keys$backend[i], get_data(spec$dataset))
        )
      },
      error = function(e) e
    )
  }
  cores <- baselines_cores(nrow(keys))
  results <- if (cores > 1L) {
    parallel::mclapply(
      seq_len(nrow(keys)),
      fit_cell,
      mc.cores = cores,
      mc.preschedule = FALSE
    )
  } else {
    lapply(seq_len(nrow(keys)), fit_cell)
  }
  stats::setNames(results, labels)
}

# Retrieve a precomputed fit, re-raising a worker-side failure (error condition,
# try-error, or NULL from a crashed fork) as a test failure attributed here.
baselines_fits_cell <- function(fits, model, backend) {
  key <- paste(model, backend, sep = "::")
  fit <- fits[[key]]
  if (is.null(fit)) {
    stop("baseline fit missing or crashed for ", key, call. = FALSE)
  }
  if (inherits(fit, "condition")) {
    stop(
      "baseline fit errored for ",
      key,
      ": ",
      conditionMessage(fit),
      call. = FALSE
    )
  }
  if (inherits(fit, "try-error")) {
    stop(
      "baseline fit errored for ",
      key,
      ": ",
      as.character(fit),
      call. = FALSE
    )
  }
  fit
}
