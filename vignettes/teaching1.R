## ----load, message=FALSE------------------------------------------------------
library(goldfish)
data("Social_Evolution")
# ?Social_Evolution
head(calls)
head(actors)


## ----quick--------------------------------------------------------------------
callNetwork <- make_network(nodes = actors, directed = TRUE) |> # 1
  link_events(change_events = calls, nodes = actors) # 2

# 3
callsDependent <- make_dependent_events(
  events = calls, nodes = actors,
  default_network = callNetwork
  )

# 4
socialEvolutionData <- make_data(callsDependent, callNetwork, actors)

# 5
mod00Rate <- estimate_dynam(
  callsDependent ~ indeg + outdeg,
  sub_model = "rate",
  data = socialEvolutionData
  )

summary(mod00Rate)

mod00Choice <- estimate_dynam(
  callsDependent ~ inertia + recip + trans,
  sub_model = "choice",
  data = socialEvolutionData
  )

summary(mod00Choice)


## ----actors-------------------------------------------------------------------
class(actors)
head(actors)


## ----make_nodes---------------------------------------------------------------
actors <- make_nodes(actors)
actors


## ----calls-events-------------------------------------------------------------
head(calls)


## ----hlp1, eval=FALSE---------------------------------------------------------
# ?make_network


## ----call-net-----------------------------------------------------------------
callNetwork <- make_network(nodes = actors, directed = TRUE)


## ----strNet-------------------------------------------------------------------
callNetwork


## ----hlp2, eval=FALSE---------------------------------------------------------
# ?link_events


## ----link-call-net------------------------------------------------------------
callNetwork <- link_events(
  x = callNetwork,
  change_events = calls,
  nodes = actors
)
callNetwork


## ----frdshp-net---------------------------------------------------------------
head(friendship)
friendshipNetwork <- make_network(nodes = actors, directed = TRUE)
friendshipNetwork <- link_events(
  x = friendshipNetwork,
  change_events = friendship,
  nodes = actors
  )
friendshipNetwork


## ----hlp3, eval=FALSE---------------------------------------------------------
# ?make_dependent_events


## ----call-dep-events----------------------------------------------------------
callsDependent <- make_dependent_events(
  events = calls, nodes = actors,
  default_network = callNetwork
  )
callsDependent


## ----hlp4, eval=FALSE---------------------------------------------------------
# ?make_data


## ----make-data----------------------------------------------------------------
socialEvolutionData <- make_data(
  callsDependent, callNetwork, actors, friendshipNetwork
)
socialEvolutionData


## ----plot-teaching1, message=FALSE, warning=FALSE-----------------------------
library(igraph)
library(ggraph)
library(migraph)
# The network at the beginning
callNetworkBgn <- as.matrix(callNetwork)
graphr(callNetworkBgn, labels = FALSE, layout = "fr")

# The network at half time
callNetworkHlf <- as.matrix(
  callNetwork,
  time = calls$time[floor(nrow(calls) / 2)]
) |>
  as_igraph() |>
  add_node_attribute("floor", actors$floor)

graphr(callNetworkHlf, labels = FALSE, layout = "fr") +
  geom_node_point(aes(color = as.factor(floor)), size = 2, show.legend = FALSE)

# The network at the end
callNetworkEnd <- as.matrix(callNetwork, time = max(calls$time) + 1) |>
  as_igraph() |>
  add_node_attribute("floor", actors$floor)

graphr(callNetworkEnd, labels = FALSE, layout = "fr") +
  geom_node_point(aes(color = as.factor(floor)), size = 2, show.legend = FALSE)


# The tie strength at the end
table(as.matrix(callNetwork, time = max(calls$time) + 1))


## ----effects, eval=FALSE------------------------------------------------------
# vignette("goldfishEffects")


## ----simple-formula-----------------------------------------------------------
simpleFormulaChoice <- callsDependent ~ tie(friendshipNetwork)


## ----simple-choice------------------------------------------------------------
mod01Choice <- estimate_dynam(
  simpleFormulaChoice,
  sub_model = "choice",
  data = socialEvolutionData
  )
summary(mod01Choice)


## ----complex-choice-----------------------------------------------------------
complexFormulaChoice <-
  callsDependent ~ inertia(callNetwork) + recip(callNetwork) +
                   tie(friendshipNetwork) + recip(friendshipNetwork) +
                   same(actors$gradeType) + same(actors$floor)

mod02Choice <- estimate_dynam(
  complexFormulaChoice,
  sub_model = "choice",
  data = socialEvolutionData
  )
summary(mod02Choice)


## ----simple-rate--------------------------------------------------------------
simpleFormulaRate <- callsDependent ~ indeg(friendshipNetwork)
mod01Rate <- estimate_dynam(
  simpleFormulaRate,
  sub_model = "rate",
  data = socialEvolutionData
  )


## ----estimate-init------------------------------------------------------------
mod01Rate <- estimate_dynam(
  simpleFormulaRate,
  sub_model = "rate",
  data = socialEvolutionData,
  control_estimation = set_estimation_opt(max_iterations = 40)
  )
summary(mod01Rate)


## ----complex-rate-------------------------------------------------------------
complexFormulaRate <-
  callsDependent ~ indeg(callNetwork) + outdeg(callNetwork) +
                   indeg(friendshipNetwork)

mod02Rate <- estimate_dynam(
  complexFormulaRate,
  sub_model = "rate",
  data = socialEvolutionData
)
summary(mod02Rate)


## ----intcpt-rate--------------------------------------------------------------
interceptFormulaRate <-
  callsDependent ~ 1 + indeg(callNetwork) + outdeg(callNetwork) +
                   indeg(friendshipNetwork)

mod03Rate <- estimate_dynam(
  interceptFormulaRate,
  sub_model = "rate",
  data = socialEvolutionData
)
summary(mod03Rate)


## ----waiting-time-------------------------------------------------------------
mod03RateCoef <- coef(mod03Rate)
1 / exp(mod03RateCoef[["Intercept"]]) / 3600
# or days:
1 / exp(mod03RateCoef[["Intercept"]]) / 86400

# But what if it is not just a random call?
# Expected waiting time of those who have five outgoing call ties
# (five different actors)
1 / exp(
  mod03RateCoef[["Intercept"]] + mod03RateCoef[["outdeg"]] * 5
  ) / 3600
# Expected waiting time of those who have five outgoing and incoming call ties
# (five different actors)
1 / exp(
  mod03RateCoef[["Intercept"]] +
    mod03RateCoef[["outdeg"]] * 5 +
    mod03RateCoef[["indeg"]] * 5
  ) / 3600


## ----windows-rate-------------------------------------------------------------
windowFormulaRate <-
  callsDependent ~ 1 + indeg(callNetwork) + outdeg(callNetwork) +
                   indeg(callNetwork, window = 300) +
                   outdeg(callNetwork, window = 300) +
                   indeg(friendshipNetwork)

mod04Rate <- estimate_dynam(
  windowFormulaRate,
  sub_model = "rate",
  data = socialEvolutionData
)
summary(mod04Rate)


## ----windows-choice-----------------------------------------------------------
windowFormulaChoice <-
  callsDependent ~ inertia(callNetwork) + recip(callNetwork) +
                   inertia(callNetwork, window = 300) +
                   recip(callNetwork, window = 300) +
                   tie(friendshipNetwork) + recip(friendshipNetwork) +
                   same(actors$gradeType) + same(actors$floor)

mod03Choice <- estimate_dynam(
  windowFormulaChoice,
  sub_model = "choice",
  data = socialEvolutionData
)
summary(mod03Choice)


## ----aic----------------------------------------------------------------------
# Compare different specifications of the subModel = "choice"
AIC(mod02Choice, mod03Choice)

# Compare different specifications of the subModel = "rate"
AIC(mod03Rate, mod04Rate)


## ----rem----------------------------------------------------------------------
allFormulaREM <-
  callsDependent ~ 
    1 + indeg(callNetwork, type = "ego") + outdeg(callNetwork, type = "ego") +
    indeg(friendshipNetwork, type = "ego") +
    inertia(callNetwork) + recip(callNetwork) +
    inertia(callNetwork, window = 300) + recip(callNetwork, window = 300) +
    tie(friendshipNetwork) + recip(friendshipNetwork) +
    same(actors$gradeType) + same(actors$floor)


## ----rem-gather, eval=FALSE---------------------------------------------------
# mod01REM <- estimate_rem(
#   allFormulaREM,
#   data = socialEvolutionData,
#   control_estimation =
#     set_estimation_opt(initial_damping = 40, engine = "default_c")
# )


## ----rem-c--------------------------------------------------------------------
mod01REM <- estimate_rem(
  allFormulaREM,
  data = socialEvolutionData,
  control_estimation = set_estimation_opt(engine = "gather_compute")
)

summary(mod01REM)

