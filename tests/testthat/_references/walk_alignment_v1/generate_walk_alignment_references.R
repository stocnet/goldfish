# Pre-change DyNES augmenter references.
#
# Seeded `augment_seq_sim()` draws (event times and `log_proposal`) on one
# uncoupled and one coupled state-panel fixture with three intervals, no
# stamp ties, no observed event at a wave time and no composition change. The
# stamp-semantics and cost work of the walk-alignment change must leave these
# draws unmoved: on such data the old and the new augmenter take the same
# steps. The script also prints the walk-open and `evaluate_fid()` counts per
# draw and per MCMC step, and the seconds and allocated MB per sim draw and
# per MCMC sweep, which the change's cost record keeps as its "before" values.
#
# Run from the package root, on the code state the references freeze:
#
#   NOT_CRAN=true Rscript \
#     tests/testthat/_references/walk_alignment_v1/generate_walk_alignment_references.R
#
# Regenerating overwrites the floor; do it only when a reference is meant to
# move (the new value derived beforehand), and say so in the commit.

suppressMessages(devtools::load_all(".", quiet = TRUE))
options(lifecycle_verbosity = "quiet")

waves <- c(5, 10, 15)
wave_grid <- c(0, waves)
n_nodes <- 12L
flip_intercept <- -3.2
draw_seeds <- 1:5
out_dir <- "tests/testthat/_references/walk_alignment_v1"

# --- Fixture ------------------------------------------------------------------
# The friendship/calls toy shape of the recovery study: a modeled flavored
# friendship panel and a densely observed calls stream. Coupled, calls reads
# friendship (rate indeg(friendship), choice tie(friendship)) and is
# supported only on present friendship ties, which makes every augmenter move
# a legality-checked one.

wa_nodes <- function() {
  data.frame(
    label = paste0("N", seq_len(n_nodes)),
    mode = "p",
    stringsAsFactors = FALSE
  )
}

wa_start_ties <- function(density = 0.2, seed = 11L) {
  withr::with_seed(seed, {
    d <- expand.grid(from = seq_len(n_nodes), to = seq_len(n_nodes))
    d <- d[d$from != d$to, ]
    d <- d[stats::runif(nrow(d)) < density, ]
  })
  data.frame(
    from = d$from,
    to = d$to,
    time = NA_real_,
    weight = 1,
    layer = "friendship"
  )
}

wa_data <- function(ties, observation = "panel") {
  add_flavor(
    list(
      info = list(
        name = "walk_alignment",
        focal = "calls",
        update = c(friendship = "increment", calls = "increment"),
        directed = c(friendship = TRUE, calls = TRUE),
        observation = c(friendship = observation, calls = "event")
      ),
      nodes = wa_nodes(),
      ties = ties
    ),
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

# Placeholder rows the generative spec needs: one timed creation and one
# dissolution at the horizon (so the flavor constraint is derived) and calls
# rows at the window start and end (so the clock opens at the start).
wa_placeholder <- function() {
  start <- wa_start_ties()
  present <- paste(start$from, start$to)
  absent <- expand.grid(from = seq_len(n_nodes), to = seq_len(n_nodes))
  absent <- absent[
    absent$from != absent$to & !paste(absent$from, absent$to) %in% present,
  ][1, ]
  rbind(
    start,
    data.frame(
      from = c(absent$from, start$from[1]),
      to = c(absent$to, start$to[1]),
      time = max(waves),
      weight = c(1, -1),
      layer = "friendship"
    ),
    data.frame(
      from = 1L,
      to = 2L,
      time = c(0, max(waves)),
      weight = 1,
      layer = "calls"
    )
  )
}

wa_spec <- function(data, coupled) {
  friendship <- make_specification(
    rate = list(creation ~ 1, dissolution ~ 1),
    choice = list(creation ~ recip, dissolution ~ recip),
    layer = "friendship",
    model = "DyNAM",
    data = data
  )
  calls <- if (coupled) {
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ inertia + tie(friendship),
      support_constraint = ~ tie(friendship),
      layer = "calls",
      model = "DyNAM",
      data = data
    )
  } else {
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    )
  }
  make_joint_specification(friendship, calls, data = data)
}

wa_truth <- function(spec, coupled) {
  args <- list(spec)
  args[["friendship › creation › rate"]] <- flip_intercept
  args[["friendship › dissolution › rate"]] <- flip_intercept
  args[["friendship › creation › choice"]] <- 1
  args[["friendship › dissolution › choice"]] <- -1
  if (coupled) {
    args[["calls › rate"]] <- c(-0.5, 0.3)
    args[["calls › choice"]] <- c(0.5, 1)
  } else {
    args[["calls › rate"]] <- -0.5
    args[["calls › choice"]] <- 0.5
  }
  do.call(set_parameters, args)
}

# Net change per dyad per interval, stamped at the closing wave; calls keep
# their event times.
wa_project <- function(events, start = wa_start_ties()) {
  bounds <- c(0, waves)
  ev <- events[events$time > 0 & events$time <= max(waves), ]
  fe <- ev[ev$layer == "friendship", ]
  fe$wave <- waves[findInterval(fe$time, bounds, left.open = TRUE)]
  panel <- if (nrow(fe) > 0L) {
    agg <- stats::aggregate(
      fe$increment,
      by = list(from = fe$sender, to = fe$receiver, time = fe$wave),
      FUN = sum
    )
    names(agg)[4] <- "weight"
    agg <- agg[agg$weight != 0, ]
    agg[order(agg$time, agg$from, agg$to), ]
  } else {
    data.frame(
      from = integer(),
      to = integer(),
      time = numeric(),
      weight = numeric()
    )
  }
  panel$layer <- rep("friendship", nrow(panel))
  ce <- ev[ev$layer == "calls", ]
  calls <- data.frame(
    from = ce$sender,
    to = ce$receiver,
    time = ce$time,
    weight = ce$increment,
    layer = rep("calls", nrow(ce))
  )
  list(
    ties = rbind(start, panel, calls),
    flips = as.integer(table(factor(panel$time, levels = waves)))
  )
}

# One simulated run, observed at the waves, with a creation and a dissolution
# net flip in every interval (so every interval has latent events to place)
# and no call exactly at a wave time.
wa_panel <- function(coupled) {
  gen <- wa_spec(wa_data(wa_placeholder(), observation = "event"), coupled)
  truth <- wa_truth(gen, coupled)
  pool <- suppressWarnings(suppressMessages(simulate(
    gen,
    nsim = 40L,
    seed = 20261012L,
    coef = truth,
    horizon = max(waves),
    control_prep = set_preprocessing(start_time = 0, end_time = max(waves)),
    control_sim = set_simulation_guard(max_events = 5e4)
  )))
  kept <- filter_simulation(pool, !capped)
  for (r in unclass(kept)) {
    p <- wa_project(r$events)
    timed <- p$ties$layer == "friendship" & !is.na(p$ties$time)
    calls_t <- p$ties$time[p$ties$layer == "calls"]
    ok <- all(p$flips >= 2L) &&
      all(p$flips <= 6L) &&
      any(p$ties$weight[timed] > 0) &&
      any(p$ties$weight[timed] < 0) &&
      !any(calls_t %in% wave_grid) &&
      !anyDuplicated(calls_t[calls_t > 0])
    if (ok) {
      return(list(ties = p$ties, flips = p$flips, truth = truth))
    }
  }
  stop("no replicate satisfies the fixture conditions")
}

# --- The augmenters, built as estimate_dynes() builds them -----------------

wa_theta <- function(truth) {
  lay <- coef_layout(truth)
  lay <- lay[!is.na(lay$index), ]
  lay$value[order(lay$index)]
}

wa_build <- function(spec, routine, burn_in = NULL, thinning = NULL) {
  completed <- suppressWarnings(complete_generative_spec(
    spec,
    consumer = "estimate_dynes",
    wave_times = wave_grid
  ))
  recipe <- compile_augmentation_recipe(completed, wave_grid)
  control <- set_algorithm_em(
    augmenter = set_augmenter_options(
      routine = routine,
      burn_in = burn_in,
      thinning = thinning
    ),
    seed = 11L
  )
  augmenter <- switch(
    routine,
    sim = augment_seq_sim(completed, recipe, control),
    mcmc = augment_seq_mcmc(completed, recipe, control)
  )
  list(
    spec = completed,
    recipe = recipe,
    control = control,
    augmenter = augmenter
  )
}

# --- Counters -----------------------------------------------------------------
# Wrap namespace functions in place (every internal caller resolves them
# through the namespace) and restore on exit.

counters <- new.env()
counters$walk_open <- 0L
counters$evaluate_fid <- 0L
counters$mcmc_step <- 0L
counters$walk_open_in_step <- 0L
counters$in_step <- FALSE

with_counters <- function(expr) {
  ns <- asNamespace("goldfish")
  originals <- list(
    walk_open = get("walk_open", ns),
    evaluate_fid = get("evaluate_fid", ns),
    mcmc_step = get("mcmc_step", ns)
  )
  wrappers <- list(
    walk_open = function(...) {
      counters$walk_open <- counters$walk_open + 1L
      if (counters$in_step) {
        counters$walk_open_in_step <- counters$walk_open_in_step + 1L
      }
      originals$walk_open(...)
    },
    evaluate_fid = function(...) {
      counters$evaluate_fid <- counters$evaluate_fid + 1L
      originals$evaluate_fid(...)
    },
    mcmc_step = function(...) {
      counters$mcmc_step <- counters$mcmc_step + 1L
      counters$in_step <- TRUE
      on.exit(counters$in_step <- FALSE)
      originals$mcmc_step(...)
    }
  )
  for (name in names(wrappers)) {
    unlockBinding(name, ns)
    assign(name, wrappers[[name]], envir = ns)
  }
  on.exit(
    for (name in names(originals)) {
      assign(name, originals[[name]], envir = ns)
    },
    add = TRUE
  )
  for (name in c("walk_open", "evaluate_fid", "mcmc_step")) {
    counters[[name]] <- 0L
  }
  counters$walk_open_in_step <- 0L
  force(expr)
}

# --- References and costs -----------------------------------------------------

draw_record <- function(draw) {
  ties <- as.data.frame(draw$data$ties)
  timed <- ties[
    ties$layer == "friendship" & !is.na(ties$time),
    c("from", "to", "time", "weight", "layer")
  ]
  timed <- timed[order(timed$time, timed$from, timed$to), ]
  rownames(timed) <- NULL
  list(
    friendship = timed,
    log_proposal = draw$log_proposal,
    log_proposal_raw = draw$log_proposal_raw,
    restarted = draw$restarted
  )
}

sim_references <- function(built, theta) {
  lapply(stats::setNames(draw_seeds, draw_seeds), function(seed) {
    withr::with_seed(
      seed,
      draw_record(built$augmenter$draw(1L, 1L, theta)[[1L]])
    )
  })
}

sim_cost <- function(built, theta) {
  per_seed <- lapply(draw_seeds, function(seed) {
    one <- function() {
      withr::with_seed(seed, built$augmenter$draw(1L, 1L, theta))
    }
    with_counters(one())
    opens <- counters$walk_open
    evals <- counters$evaluate_fid
    m <- bench::mark(one(), iterations = 1L, check = FALSE, memory = TRUE)
    c(
      walk_opens = opens,
      evaluate_fid = evals,
      seconds = as.numeric(m$median),
      mb = as.numeric(m$mem_alloc) / 1024^2
    )
  })
  per_seed <- do.call(rbind, per_seed)
  rownames(per_seed) <- draw_seeds
  list(per_seed = per_seed, mean = colMeans(per_seed))
}

mcmc_cost <- function(spec, theta, n_sweeps = 3L) {
  built <- wa_build(spec, "mcmc", burn_in = 0L, thinning = n_sweeps)
  decomposition <- decompose_joint_spec(built$spec)
  evaluator <- make_proposal_evaluator(
    decomposition,
    theta,
    spec = built$spec
  )
  built$augmenter$init(evaluator, theta)
  # The first draw builds the initial state; its cost is not a step's.
  withr::with_seed(1L, built$augmenter$draw(1L, 1L, theta))
  n_events <- nrow(built$recipe$events)
  counted <- with_counters(
    withr::with_seed(2L, built$augmenter$draw(1L, 1L, theta))
  )
  steps <- counters$mcmc_step
  opens_per_step <- counters$walk_open_in_step / steps
  evals_per_step <- counters$evaluate_fid / steps
  built2 <- wa_build(spec, "mcmc", burn_in = 0L, thinning = n_sweeps)
  built2$augmenter$init(
    make_proposal_evaluator(decomposition, theta, spec = built2$spec),
    theta
  )
  withr::with_seed(1L, built2$augmenter$draw(1L, 1L, theta))
  m <- bench::mark(
    withr::with_seed(2L, built2$augmenter$draw(1L, 1L, theta)),
    iterations = 1L,
    check = FALSE,
    memory = TRUE
  )
  list(
    n_events = n_events,
    steps = steps,
    sweeps = n_sweeps,
    walk_opens_per_step = opens_per_step,
    evaluate_fid_per_step = evals_per_step,
    seconds_per_sweep = as.numeric(m$median) / n_sweeps,
    mb_per_sweep = as.numeric(m$mem_alloc) / 1024^2 / n_sweeps
  )
}

result <- list()
for (coupled in c(FALSE, TRUE)) {
  name <- if (coupled) "coupled" else "uncoupled"
  cat("==", name, "\n")
  panel <- wa_panel(coupled)
  cat("flips per interval:", panel$flips, "\n")
  spec <- wa_spec(wa_data(panel$ties), coupled)
  theta <- wa_theta(panel$truth)
  built <- wa_build(spec, "sim")
  cat("recipe events:", nrow(built$recipe$events), "\n")
  references <- sim_references(built, theta)
  sim <- sim_cost(built, theta)
  mcmc <- mcmc_cost(spec, theta)
  cat("sim (per seed, then mean):\n")
  print(sim$per_seed)
  print(sim$mean)
  cat("mcmc:\n")
  print(unlist(mcmc))
  result[[name]] <- list(
    ties = panel$ties,
    flips = panel$flips,
    theta = theta,
    sim_draws = references,
    sim_cost = sim,
    mcmc_cost = mcmc
  )
}

attr(result, "commit") <- system("git rev-parse --short HEAD", intern = TRUE)
attr(result, "waves") <- wave_grid
saveRDS(
  result,
  file.path(out_dir, "walk_alignment_pre_change.rds"),
  compress = "xz"
)
cat("saved at", attr(result, "commit"), "\n")
