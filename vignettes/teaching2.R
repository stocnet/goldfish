## ----setup, message=FALSE-----------------------------------------------------
library(goldfish)


## ----load-data----------------------------------------------------------------
data("Fisheries_Treaties_6070")
# ?Fisheries_Treaties_6070


## ----examine-states-----------------------------------------------------------
tail(states)
class(states)


## ----make_nodes---------------------------------------------------------------
states <- make_nodes(states)
head(states)
class(states)


## ----examine-node-changes-----------------------------------------------------
head(sovchanges)
head(regchanges)
head(gdpchanges)


## ----present------------------------------------------------------------------
head(states$present) # or states[,2]


## ----link-present-------------------------------------------------------------
states <- link_events(states, sovchanges, attribute = "present")
# If you call the object now, what happens?
states


## ----states-------------------------------------------------------------------
str(states)


## ----link-states-vars---------------------------------------------------------
states <- link_events(states, regchanges, attribute = "regime") |>
  link_events(gdpchanges, attribute = "gdp")
str(states)


## ----examine-bilat-mat--------------------------------------------------------
bilatnet[1:12, 1:12]  # head(bilatnet, n = c(12, 12))


## ----define-bilat-net---------------------------------------------------------
bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)


## ----examine-bilat-net--------------------------------------------------------
class(bilatnet)
str(bilatnet)
bilatnet


## ----link-bilat-net-----------------------------------------------------------
bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
bilatnet


## ----contig-net---------------------------------------------------------------
contignet <- make_network(contignet, nodes = states, directed = FALSE) |>
  link_events(contigchanges, nodes = states)
class(contignet)
contignet


## ----define-dep-events--------------------------------------------------------
createBilat <- make_dependent_events(
  events = bilatchanges[bilatchanges$increment == 1,],
  nodes = states,
  default_network = bilatnet
)


## ----examine-dep-events-------------------------------------------------------
class(createBilat)
createBilat


## ----make-data----------------------------------------------------------------
fisheriesData <- make_data(createBilat, bilatnet, contignet, states)
fisheriesData


## ----hlp, eval = FALSE--------------------------------------------------------
# ?as.data.frame.nodes.goldfish
# ?as.matrix.network.goldfish


## ----plot-teaching2, message=FALSE, warning=FALSE, fig.align='center'---------
library(igraph)
library(manynet)

# network at the beginning of the event sequence
startStates <- as.data.frame(
  states,
  time = as.numeric(as.POSIXct("1960-01-02"))
)
startNet <- as.matrix(bilatnet, time = as.numeric(as.POSIXct("1960-01-02"))) |>
  as_igraph() |> 
  add_node_attribute("present", startStates$present) |>
  add_node_attribute("regime", startStates$regime) |>
  add_node_attribute("gdp", startStates$gdp)

# network at the end of the event sequence
endStates <- as.data.frame(states, time = as.numeric(as.POSIXct("1970-01-01")))
endNet <- as.matrix(bilatnet, time = as.numeric(as.POSIXct("1970-01-01"))) |>
  as_igraph() |>
  add_node_attribute("present", endStates$present) |>
  add_node_attribute("regime", endStates$regime) |>
  add_node_attribute("gdp", endStates$gdp)

# logical value indicating if states where present and with agreements
isStateActiveStart <- startStates$present & node_deg(startNet) > 0
isStateActiveEnd <- endStates$present & node_deg(endNet) > 0
isStateActive <- isStateActiveStart | isStateActiveEnd

# subset networks to active states
startNet <- delete_nodes(startNet, !isStateActive)
endNet <- delete_nodes(endNet, !isStateActive)

graphs(list(startNet, endNet), layout = "fr")


## ----hlp-effects, eval=FALSE--------------------------------------------------
# vignette("goldfishEffects")


## ----estimate-init------------------------------------------------------------
formula1 <-
  createBilat ~ inertia(bilatnet) + indeg(bilatnet, ignore_repetitions = TRUE) +
                trans(bilatnet, ignore_repetitions = TRUE) +
                tie(contignet) +
                alter(states$regime) + diff(states$regime) +
                alter(states$gdp) + diff(states$gdp)

est_opts <- set_estimation_opt(
  return_interval_loglik = TRUE,
  initial_damping = 40,
  max_iterations = 30,
  engine = "default"
)

system.time(
  partnerModel <- estimate_dynam(
    formula1,
    sub_model = "choice_coordination",
    data = fisheriesData,
    control_estimation = est_opts
  )
)


## ----estimate-rerun-----------------------------------------------------------
est_opts <- set_estimation_opt(
  return_interval_loglik = TRUE,
  initial_damping = 40,
  max_iterations = 30,
  initial_parameters = coef(partnerModel),
  engine = "default"
)

partnerModel <- estimate_dynam(
  formula1,
  sub_model = "choice_coordination",
  data = fisheriesData,
  control_estimation = est_opts
)
summary(partnerModel)


## ----estimate-c---------------------------------------------------------------
formula2 <-
  createBilat ~ inertia(bilatnet, weighted = TRUE) +
                indeg(bilatnet) + trans(bilatnet) +
                tie(contignet) + alter(states$regime) +
                diff(states$regime) + alter(states$gdp) + diff(states$gdp)

est_opts <- set_estimation_opt(
  return_interval_loglik = TRUE,
  initial_damping = 40,
  max_iterations = 30
)

system.time(
  tieModel <- estimate_dynam(
    formula2,
    sub_model = "choice_coordination",
    data = fisheriesData,
    control_estimation = est_opts
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
examine_outliers(tieModel)
examine_changepoints(tieModel)

