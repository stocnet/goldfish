## ----setup, message=FALSE-----------------------------------------------------
library(goldfish)


## ----load-data----------------------------------------------------------------
data("fisheries_treaties")
# ?fisheries_treaties
fisheries_treaties


## ----load-raw, message=FALSE--------------------------------------------------
library(manynet)
data("Fisheries_Treaties_6070")


## ----build-treaties-----------------------------------------------------------
treaties <- as_stocnet(bilatnet) |> # bilatnet -> history ties (time = NA)
  join_nodes(states) |>
  rename_nodes() |>
  bind_ties(bilatchanges)


## ----build-contig-------------------------------------------------------------
contigchanges$order <- seq_len(nrow(contigchanges))
contiguity <- as_stocnet(contignet) |>
  bind_ties(contigchanges)


## ----build-merge--------------------------------------------------------------
fisheries_built <- from_ties(
  treaties = treaties,
  contiguity = contiguity
) |>
  add_info(
    name = "Fisheries Treaties",
    # focal is optional -- the specifications below name "treaties" via `layer`.
    focal = "treaties",
    directed = c(treaties = FALSE, contiguity = FALSE),
    observation = c(treaties = "event", contiguity = "event")
  ) |>
  bind_changes(changes = gdpchanges, var = "gdp") |>
  bind_changes(sovchanges, var = "active") |>
  bind_changes(regchanges, var = "regime")


## ----plot-teaching2, message=FALSE, warning=FALSE, fig.align='center'---------
library(igraph)
library(migraph)
library(patchwork)

start_time <- as.POSIXct("1960-01-02")
end_time <- as.POSIXct("1970-01-01")

# network and attributes at the beginning of the event sequence
startStates <- nodes_state_at(fisheries_treaties, time = start_time)
startNet <- network_state_at(fisheries_treaties, "treaties", time = start_time) |>
  as_igraph() |>
  add_node_attribute("present", startStates$active) |>
  add_node_attribute("regime", startStates$regime) |>
  add_node_attribute("gdp", startStates$gdp)

# network and attributes at the end of the event sequence
endStates <- nodes_state_at(fisheries_treaties, time = end_time)
endNet <- network_state_at(fisheries_treaties, "treaties", time = end_time) |>
  as_igraph() |>
  add_node_attribute("present", endStates$active) |>
  add_node_attribute("regime", endStates$regime) |>
  add_node_attribute("gdp", endStates$gdp)

# logical value indicating if states were present and with agreements
isStateActive <-
  (startStates$active & degree(startNet) > 0) |
  (endStates$active & degree(endNet) > 0)

# subset networks to active states
startNet <- delete_vertices(startNet, which(!isStateActive))
endNet <- delete_vertices(endNet, which(!isStateActive))

graphr(startNet, layout = "fr") | graphr(endNet, layout = "fr")


## ----hlp-effects, eval=FALSE--------------------------------------------------
# vignette("goldfishEffects")


## ----add-flavor---------------------------------------------------------------
fisheries_treaties <- add_flavor(
  fisheries_treaties,
  layer = "treaties",
  values_equivalence = c(signing = 1, ending = -1),
  flavor_style = "redundant"
)


## ----estimate-init------------------------------------------------------------
formula1 <- list(
  signing ~ inertia(treaties) + indeg(treaties) + trans(treaties) +
    tie(contiguity) +
    alter(regime) + diff(regime) +
    alter(gdp) + diff(gdp)
)

partnerSpec <- make_specification(
  choice = formula1,
  model = "DyNAM",
  choice_sub_model = "choice_coordination",
  layer = "treaties",
  data = fisheries_treaties
)

est_opts <- set_algorithm_newton(
  diagnostics = "loglik",
  initial_damping = 40,
  max_iterations = 30,
  backend = "r"
)

system.time(
  partnerModel <- estimate_dynam(
    partnerSpec,
    sub_model = "choice_coordination",
    data = fisheries_treaties,
    control_algo = est_opts
  )
)


## ----estimate-rerun-----------------------------------------------------------
est_opts <- set_algorithm_newton(
  diagnostics = "loglik",
  initial_damping = 40,
  max_iterations = 30,
  initial_parameters = coef(partnerModel),
  backend = "r"
)

partnerModel <- estimate_dynam(
  partnerSpec,
  sub_model = "choice_coordination",
  data = fisheries_treaties,
  control_algo = est_opts
)
summary(partnerModel)


## ----estimate-c---------------------------------------------------------------
formula2 <- list(
  signing ~ inertia(treaties, weighted = TRUE) +
    indeg(treaties) + trans(treaties) +
    tie(contiguity) + alter(regime) +
    diff(regime) + alter(gdp) + diff(gdp)
)

tieSpec <- make_specification(
  choice = formula2,
  model = "DyNAM",
  choice_sub_model = "choice_coordination",
  layer = "treaties",
  data = fisheries_treaties
)

est_opts <- set_algorithm_newton(
  diagnostics = "loglik",
  initial_damping = 40,
  max_iterations = 30
)

system.time(
  tieModel <- estimate_dynam(
    tieSpec,
    sub_model = "choice_coordination",
    data = fisheries_treaties,
    control_algo = est_opts
  )
)


## ----broom, message=FALSE-----------------------------------------------------
library(broom)
library(pixiedust)
dust(tidy(tieModel, conf.int = TRUE)) |>
  sprinkle(col = c(2:4, 6, 7), round = 3) |>
  sprinkle(col = 5, fn = quote(pvalString(value)))


## ----glance-------------------------------------------------------------------
glance(tieModel)


## ----plot-examine, fig.width=6, fig.height=4, fig.align='center', fig.retina=3----
diagnose_outliers(tieModel)
diagnose_changepoints(tieModel)




## ----diag-refit---------------------------------------------------------------
tieModelDiag <- estimate_dynam(
  tieSpec,
  sub_model = "choice_coordination",
  data = fisheries_treaties,
  control_algo = set_algorithm_newton(
    diagnostics = c("loglik", "scores"),
    initial_damping = 40,
    max_iterations = 30
  ),
  return_preprocessed = TRUE
)


## ----diag-gof2----------------------------------------------------------------
# Qualified: this vignette attaches migraph, which exports a `test_gof` of its
# own, so the bare name resolves to whichever package was attached last.
goldfish::test_gof(tieModelDiag)


## ----diag-time2---------------------------------------------------------------
goldfish::test_time(tieModelDiag)


## ----diag-time-plot2, eval = has_plots, fig.width=6, fig.height=4, fig.align='center', fig.retina=3, fig.alt = "Scaled Schoenfeld residuals per effect against model time, with a smooth and the fitted estimate as reference."----
# plot(goldfish::test_time(tieModelDiag))

