## ----setup, message=FALSE----------------------------------------------------------------------------
library(goldfish)




## ----load-data---------------------------------------------------------------------------------------
data("Fisheries_Treaties_6070")
# ?Fisheries_Treaties_6070


## ----examine-states----------------------------------------------------------------------------------
tail(states)
class(states)


## ----defineNodes-------------------------------------------------------------------------------------
states <- defineNodes(states)
head(states)
class(states)


## ----examine-node-changes----------------------------------------------------------------------------
head(sovchanges)
head(regchanges)
head(gdpchanges)


## ----present-----------------------------------------------------------------------------------------
head(states$present) # or states[,2]


## ----link-present------------------------------------------------------------------------------------
states <- linkEvents(states, sovchanges, attribute = "present")
# If you call the object now, what happens?
states


## ----states------------------------------------------------------------------------------------------
str(states)


## ----link-states-vars--------------------------------------------------------------------------------
states <- linkEvents(states, regchanges, attribute = "regime") |> 
  linkEvents(gdpchanges, attribute = "gdp")
str(states)


## ----objs--------------------------------------------------------------------------------------------
goldfishObjects()


## ----examine-bilat-mat-------------------------------------------------------------------------------
bilatnet[1:12, 1:12]  # head(bilatnet, n = c(12, 12))


## ----define-bilat-net--------------------------------------------------------------------------------
bilatnet <- defineNetwork(bilatnet, nodes = states, directed = FALSE)


## ----examine-bilat-net-------------------------------------------------------------------------------
class(bilatnet)
str(bilatnet)
bilatnet


## ----link-bilat-net----------------------------------------------------------------------------------
bilatnet <- linkEvents(bilatnet, bilatchanges, nodes = states)
bilatnet


## ----contig-net--------------------------------------------------------------------------------------
contignet <- defineNetwork(contignet, nodes = states, directed = FALSE) |> 
  linkEvents(contigchanges, nodes = states)
class(contignet)
contignet


## ----objs2-------------------------------------------------------------------------------------------
goldfishObjects()


## ----define-dep-events-------------------------------------------------------------------------------
createBilat <- defineDependentEvents(
  events = bilatchanges[bilatchanges$increment == 1,], 
  nodes = states, 
  defaultNetwork = bilatnet
)


## ----examine-dep-events------------------------------------------------------------------------------
class(createBilat)
createBilat


## ----objs3-------------------------------------------------------------------------------------------
goldfishObjects()


## ----hlp, eval = FALSE-------------------------------------------------------------------------------
## ?as.data.frame.nodes.goldfish
## ?as.matrix.network.goldfish


## ----plot-teaching2, message=FALSE, warning=FALSE, fig.width=7, fig.height=4, fig.align='center', fig.retina=3----
library(igraph)
library(migraph)

# network at the beginning of the event sequence
startStates <- as.data.frame(states, time = as.numeric(as.POSIXct("1960-01-02")))
startNet <- as.matrix(bilatnet, time = as.numeric(as.POSIXct("1960-01-02"))) |>
  add_node_attribute("present", startStates$present) |> 
  add_node_attribute("regime", startStates$regime) |> 
  add_node_attribute("gdp", startStates$gdp)

# network at the end of the event sequence
endStates <- as.data.frame(states, time = as.numeric(as.POSIXct("1970-01-01")))
endNet <- as.matrix(bilatnet, time = as.numeric(as.POSIXct("1970-01-01"))) |> 
  add_node_attribute("present", endStates$present) |> 
  add_node_attribute("regime", endStates$regime) |> 
  add_node_attribute("gdp", endStates$gdp)

# logical value indicating if states where present and with agreements
isStateActiveStart <- startStates$present & node_degree(startNet) > 0
isStateActiveEnd <- endStates$present & node_degree(endNet) > 0
isStateActive <- isStateActiveStart | isStateActiveEnd

# subset networks to active states
startNet <- delete_vertices(startNet, !isStateActive)
endNet <- delete_vertices(endNet, !isStateActive)

ggevolution(startNet, endNet, layout = "fr", based_on = "last")


## ----hlp-effects, eval=FALSE-------------------------------------------------------------------------
## vignette("goldfishEffects")


## ----estimate-init-----------------------------------------------------------------------------------
formula1 <-
  createBilat ~ inertia(bilatnet) + indeg(bilatnet, ignoreRep = TRUE) +
                trans(bilatnet, ignoreRep = TRUE) +
                tie(contignet) +
                alter(states$regime) + diff(states$regime) +
                alter(states$gdp) + diff(states$gdp)

estPrefs <- list(
  returnIntervalLogL = TRUE,
  initialDamping = 40,
  maxIterations = 30
  )

system.time(
  partnerModel <- estimate(
    formula1,
    model = "DyNAM", subModel = "choice_coordination",
    estimationInit = estPrefs
    )
)


## ----estimate-rerun----------------------------------------------------------------------------------
estPrefs <- list(
  returnIntervalLogL = TRUE,
  initialDamping = 40,
  maxIterations = 30,
  initialParameters = coef(partnerModel)
  )

partnerModel <- estimate(
  formula1,
  model = "DyNAM", subModel = "choice_coordination",
  estimationInit = estPrefs
  )
summary(partnerModel)


## ----estimate-c--------------------------------------------------------------------------------------
formula2 <-
  createBilat ~ inertia(bilatnet, weighted = TRUE) +
                indeg(bilatnet) + trans(bilatnet) +
                tie(contignet) + alter(states$regime) +
                diff(states$regime) + alter(states$gdp) + diff(states$gdp)

estPrefs <- list(
  returnIntervalLogL = TRUE,
  initialDamping = 40,
  maxIterations = 30,
  engine = "default_c"
  )

system.time(
  tieModel <- estimate(
    formula2,
    model = "DyNAM", subModel = "choice_coordination",
    estimationInit = estPrefs
    )
)


## ----broom-------------------------------------------------------------------------------------------
library(broom)
library(pixiedust)
dust(tidy(tieModel)) %>% 
  sprinkle(col = 2:4, round = 3) %>% 
  sprinkle(col = 5, fn = quote(pvalString(value)))


## ----glance------------------------------------------------------------------------------------------
glance(tieModel)


## ----examine, fig.width=6, fig.height=4, fig.align='center', fig.retina=3----------------------------
examineOutliers(tieModel)
examineChangepoints(tieModel)

