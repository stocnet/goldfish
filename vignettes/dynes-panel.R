## ----setup--------------------------------------------------------------------
library(goldfish)


## ----layers-------------------------------------------------------------------
se <- social_evolution
ties <- as.data.frame(se$ties)
friendship <- ties[ties$layer == "friendship", ]
waves <- sort(unique(friendship$time))
waves
wave1 <- friendship[friendship$time == waves[1], ]
wave2 <- friendship[friendship$time == waves[2], ]
c(wave1 = nrow(wave1), wave2 = nrow(wave2))


## ----node-set-----------------------------------------------------------------
both <- intersect(wave1$from, wave2$from)
omitted_wave1 <- sum(!wave1$from %in% both)
omitted_wave2 <- sum(!wave2$from %in% both)
c(actors = length(both), omitted_wave1 = omitted_wave1,
  omitted_wave2 = omitted_wave2)

ties <- ties[ties$from %in% both & ties$to %in% both, ]
ties$from <- match(ties$from, both)
ties$to <- match(ties$to, both)
n_actors <- length(both)


## ----adjacency----------------------------------------------------------------
friendship <- ties[ties$layer == "friendship", ]
wave1 <- friendship[friendship$time == waves[1], ]
wave2 <- friendship[friendship$time == waves[2], ]
survey_matrix <- function(wave) {
  adjacency <- matrix(0, n_actors, n_actors)
  adjacency[cbind(wave$from, wave$to)] <- 1
  adjacency
}
adjacency1 <- survey_matrix(wave1)
adjacency2 <- survey_matrix(wave2)
table(wave1 = adjacency1, wave2 = adjacency2)


## ----changes------------------------------------------------------------------
cells <- which(adjacency1 != adjacency2, arr.ind = TRUE)
changes <- data.frame(
  from = cells[, "row"],
  to = cells[, "col"],
  time = waves[2],
  weight = adjacency2[cells],
  layer = "friendship"
)
table(changes$weight)


## ----panel--------------------------------------------------------------------
wave1$time <- NA
ties <- rbind(ties[ties$layer == "calls", ], wave1, changes)
ties <- ties[order(ties$time, na.last = FALSE), ]
table(ties$layer)

panel <- se
panel$nodes <- as.data.frame(se$nodes)[both, ]
panel$ties <- ties


## ----flavor-------------------------------------------------------------------
panel <- add_flavor(
  panel,
  layer = "friendship",
  values_equivalence = c(creation = 1, dissolution = 0),
  flavor_style = "mutually_exclusive"
)


## ----prepare------------------------------------------------------------------
surveys <- se
surveys$nodes <- panel$nodes
surveys$ties <- rbind(ties[ties$layer == "calls", ], friendship)
prepared <- prepare_dynes_data(
  surveys,
  layers = c(friendship = "mutually_exclusive")
)
written <- prepared$ties[prepared$ties$layer == "friendship", ]
table(wave = format(written$time, "%Y-%m-%d"), flavor = written$flavor)


## ----prepare-check------------------------------------------------------------
key <- function(rows) paste(rows$from, rows$to, rows$weight)
setequal(key(written[written$time == waves[2], ]), key(changes))
identical(prepared$info$values_equivalence, panel$info$values_equivalence)
identical(prepared$info$flavor_style, panel$info$flavor_style)


## ----spec---------------------------------------------------------------------
spec <- make_joint_specification(
  make_specification(
    rate = list(creation ~ 1, dissolution ~ 1),
    choice = list(creation ~ recip, dissolution ~ recip),
    layer = "friendship",
    model = "DyNAM",
    data = panel
  ),
  make_specification(
    rate = ~ 1 + indeg(friendship),
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = panel
  ),
  data = panel
)


## ----quiet-support------------------------------------------------------------
support_seen <- character()
quiet_support <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("support_constraint", conditionMessage(w), fixed = TRUE)) {
        support_seen <<- union(support_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    }
  )
}


## ----recipe-------------------------------------------------------------------
wave_times <- as.numeric(waves)
prep <- set_preprocessing(start_time = wave_times[1], end_time = wave_times[2])
completed <- quiet_support(suppressWarnings(goldfish:::complete_generative_spec(
  goldfish:::snap_composition_to_waves(spec, wave_times),
  consumer = "estimate_dynes",
  wave_times = wave_times
)))
recipe <- goldfish:::compile_augmentation_recipe(completed, wave_times)
labels <- panel$nodes$label
as_time <- function(x) as.POSIXct(x, origin = "1970-01-01", tz = "UTC")

flips <- recipe$events
flips$sender <- labels[flips$from]
flips$receiver <- labels[flips$to]
table(flips$flavor)


## ----random-draw--------------------------------------------------------------
control <- set_algorithm_em(
  seed = 2008L,
  augmenter = set_augmenter_options(routine = "random")
)
set.seed(2008)
random <- goldfish:::augment_seq_random(
  completed,
  recipe,
  control,
  control_prep = prep
)
draw <- random$draw(1L, 1L, theta = NULL)[[1]]
sequence <- as.data.frame(draw$data$ties)
drawn <- sequence[
  sequence$layer == "friendship" & sequence$time > wave_times[1],
]
drawn$time <- as_time(drawn$time)
drawn$sender <- labels[drawn$from]
drawn$receiver <- labels[drawn$to]
head(drawn[c("time", "sender", "receiver", "flavor")], 8)


## ----theta0-------------------------------------------------------------------
layout <- coef_layout(completed)[!coef_layout(completed)$fixed, ]
seconds <- diff(wave_times)
n_events <- c(
  "friendship › creation › rate" = 70,
  "friendship › dissolution › rate" = 70,
  "calls › rate" = sum(panel$ties$layer == "calls")
)
theta0 <- ifelse(
  layout$name == "Intercept",
  log(n_events[layout$process] / (length(labels) * seconds)),
  0
)
data.frame(process = layout$process, effect = layout$name, theta0)


## ----chain--------------------------------------------------------------------
new_chain <- function(theta) {
  chain <- goldfish:::new_mcmc_chain(
    goldfish:::new_augmenter_context(completed, recipe, prep),
    set_algorithm_em(augmenter = set_augmenter_options(routine = "mcmc"))
  )
  goldfish:::mcmc_start(
    chain,
    goldfish:::mcmc_times_from_data(chain$context, draw$data)
  )
  attach_theta(chain, theta)
  chain
}
attach_theta <- function(chain, theta) {
  quiet_support(goldfish:::mcmc_attach(
    chain,
    goldfish:::make_proposal_evaluator(
      goldfish:::decompose_joint_spec(completed),
      theta,
      spec = completed,
      control_prep = prep
    )
  ))
}
show_move <- function(step) {
  moved <- which(step$current != step$proposed)
  cat(sprintf(
    "%s move: log alpha = %.2f, %s\n",
    step$move$type,
    step$log_alpha,
    if (step$accepted) "accepted" else "rejected"
  ))
  print(
    data.frame(
      sender = flips$sender[moved],
      receiver = flips$receiver[moved],
      flavor = flips$flavor[moved],
      before = as_time(step$current[moved]),
      after = as_time(step$proposed[moved])
    ),
    row.names = FALSE
  )
}


## ----moves-flat---------------------------------------------------------------
chain <- quiet_support(new_chain(theta0))
set.seed(1)
for (type in c("permute", "shift")) {
  repeat {
    step <- quiet_support(
      goldfish:::mcmc_step(chain, goldfish:::mcmc_pick_move(chain))
    )
    if (identical(step$move$type, type) && isTRUE(step$accepted)) break
  }
  show_move(step)
}


## ----moves-mixed--------------------------------------------------------------
catalog <- goldfish:::mcmc_move_catalog(recipe)
mixed <- catalog$pairs[
  flips$flavor[catalog$pairs$first] != flips$flavor[catalog$pairs$second],
][1, ]
show_move(quiet_support(goldfish:::mcmc_step(
  chain,
  list(type = "permute", events = c(mixed$first, mixed$second))
)))


## ----moves-active-------------------------------------------------------------
theta1 <- ifelse(layout$name == "Intercept", theta0, 1)
attach_theta(chain, theta1)
set.seed(2)
for (i in 1:3) {
  show_move(quiet_support(
    goldfish:::mcmc_step(chain, goldfish:::mcmc_pick_move(chain))
  ))
}


## ----sweep--------------------------------------------------------------------
chain$counts$proposals[] <- 0L
chain$counts$accepted[] <- 0L
sweep_time <- system.time(
  quiet_support(goldfish:::mcmc_sweeps(chain, 1L))
)[["elapsed"]]
goldfish:::mcmc_diagnostics(chain)[c("proposals", "accepted", "acceptance_rate")]


## ----support-seen-------------------------------------------------------------
cat(support_seen, sep = "\n\n")
labels[c(15, 18, 51)]


## ----fit, eval = FALSE--------------------------------------------------------
# fit_random <- estimate_dynes(
#   spec,
#   set_algorithm_em(
#     n_sequences = 50L,
#     seed = 2008L,
#     augmenter = set_augmenter_options(routine = "random", warm_start = TRUE)
#   ),
#   wave_times = waves
# )
# fit_mcmc <- estimate_dynes(
#   spec,
#   set_algorithm_em(
#     n_sequences = 50L,
#     seed = 2008L,
#     augmenter = set_augmenter_options(routine = "mcmc", warm_start = TRUE)
#   ),
#   wave_times = waves
# )






## ----summary-mcmc-------------------------------------------------------------
summary(fit_mcmc)


## ----glance-------------------------------------------------------------------
rbind(random = glance(fit_random), mcmc = glance(fit_mcmc))[
  c("converged", "iterations", "at_optimum", "newton_se", "n_sequences")
]


## ----ess-trace----------------------------------------------------------------
trace_random <- fit_random$em_trace
trace_random[c(1:6, 36:40), c("iteration", "pool_size", "ess")]




## ----ess-plot, fig.width = 7, fig.height = 3.5, fig.alt = "Line chart of the effective sample size at each of the 50 EM iterations of the random fit, on a log scale. The ESS stays between 1 and 3 throughout. A dashed line marks the trigger floor, 0.02 times the pool size: it sits at 1 for iterations 1 to 3, where the pool holds 50 paths, and rises to 2.8 and then 4.2 as the pool grows to 139 and 211."----
plot(
  trace_random$iteration,
  trace_random$ess,
  type = "b",
  pch = 19,
  log = "y",
  ylim = c(0.9, 6),
  xlab = "EM iteration",
  ylab = "ESS (log scale)",
  main = "random: ESS against the trigger floor"
)
lines(
  trace_random$iteration,
  0.02 * trace_random$pool_size,
  type = "s",
  lty = 2
)
legend(
  "topright",
  c("ESS", "0.02 x pool size"),
  lty = c(1, 2),
  pch = c(19, NA),
  bty = "n"
)


## ----ess-mcmc-----------------------------------------------------------------
fit_mcmc$em_trace[c("iteration", "retry", "decision", "pool_size", "ess")]


## ----chain-diagnostics--------------------------------------------------------
chain_diagnostics <- fit_mcmc$em_trace$chain_diagnostics
data.frame(
  iteration = fit_mcmc$em_trace$iteration,
  acceptance = vapply(
    chain_diagnostics,
    function(d) if (is.null(d)) NA_real_ else d$acceptance_rate,
    numeric(1)
  ),
  autocorrelation = fit_mcmc$em_trace$autocorrelation
)


## ----error-ratio--------------------------------------------------------------
error_ratio <- function(fit) {
  tidied <- tidy(fit)
  stats::setNames(tidied$mc.error / tidied$std.error, tidied$term)
}
round(cbind(random = error_ratio(fit_random), mcmc = error_ratio(fit_mcmc)), 3)


## ----compare------------------------------------------------------------------
compare <- data.frame(
  random = coef(fit_random),
  mcmc = coef(fit_mcmc),
  se_mcmc = sqrt(diag(vcov(fit_mcmc)))
)
compare$gap_in_se <- (compare$random - compare$mcmc) / compare$se_mcmc
round(compare, 3)

