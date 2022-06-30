## ----load-------------------------------------------------------------------------
library(goldfish)
data("Social_Evolution")
# ?Social_Evolution
head(calls)
head(actors)




## ----quick------------------------------------------------------------------------
callNetwork <- defineNetwork(nodes = actors, directed = TRUE) |> # 1
  linkEvents(changeEvent = calls, nodes = actors) # 2

# 3
callsDependent <- defineDependentEvents(
  events = calls, nodes = actors,
  defaultNetwork = callNetwork
  )

# 4
mod00Rate <- estimate(
  callsDependent ~ indeg + outdeg,
  model = "DyNAM", subModel = "rate"
  )

summary(mod00Rate)

mod00Choice <- estimate(
  callsDependent ~ inertia + recip + trans,
  model = "DyNAM", subModel = "choice"
  )

summary(mod00Choice)


## ----actors-----------------------------------------------------------------------
class(actors)
head(actors)


## ----define-actors----------------------------------------------------------------
actors <- defineNodes(actors)
actors


## ----objs-------------------------------------------------------------------------
goldfishObjects()


## ----calls-events-----------------------------------------------------------------
head(calls)


## ----hlp1, eval=FALSE-------------------------------------------------------------
## ?defineNetwork


## ----call-net---------------------------------------------------------------------
callNetwork <- defineNetwork(nodes = actors, directed = TRUE)


## ----strNet-----------------------------------------------------------------------
callNetwork


## ----hlp2, eval=FALSE-------------------------------------------------------------
## ?linkEvents


## ----link-call-net----------------------------------------------------------------
callNetwork <- linkEvents(x = callNetwork, changeEvent = calls, nodes = actors)
callNetwork


## ----frdshp-net-------------------------------------------------------------------
head(friendship)
friendshipNetwork <- defineNetwork(nodes = actors, directed = TRUE)
friendshipNetwork <- linkEvents(
  x = friendshipNetwork, 
  changeEvents = friendship, 
  nodes = actors
  )
friendshipNetwork


## ----hlp3, eval=FALSE-------------------------------------------------------------
## ?defineDependentEvents


## ----call-dep-events--------------------------------------------------------------
callsDependent <- defineDependentEvents(
  events = calls, nodes = actors,
  defaultNetwork = callNetwork
  )
callsDependent


## ----plot-teaching1, fig.width=7, fig.retina=3------------------------------------
library(ggraph)
library(migraph)
# The network at the beginning
callNetworkBgn <- as.matrix(callNetwork)
autographr(callNetworkBgn, labels = FALSE, layout = "fr")

# The network at half time
callNetworkHlf <- as.matrix(callNetwork,
                            time = calls$time[floor(nrow(calls) / 2)]) |> 
  add_node_attributes("floor", actors$floor)

autographr(callNetworkHlf, labels = FALSE, layout = "fr") +
  geom_node_point(aes(color = as.factor(floor)), size = 2, show.legend = FALSE)

# The network at the end
callNetworkEnd <- as.matrix(callNetwork, time = max(calls$time) + 1) |> 
  add_node_attributes("floor", actors$floor)

autographr(callNetworkEnd, labels = FALSE, layout = "fr") +
  geom_node_point(aes(color = as.factor(floor)), size = 2, show.legend = FALSE)


# The tie strength at the end
table(as.matrix(callNetwork, time = max(calls$time) + 1))


## ----effects, eval=FALSE----------------------------------------------------------
## ?goldfishEffects


## ----simple-formula---------------------------------------------------------------
simpleFormulaChoice <- callsDependent ~ tie(friendshipNetwork)


## ----simple-choice----------------------------------------------------------------
mod01Choice <- estimate(
  simpleFormulaChoice,
  model = "DyNAM", subModel = "choice"
  )
summary(mod01Choice)


## ----complex-choice---------------------------------------------------------------
complexFormulaChoice <-
  callsDependent ~ inertia(callNetwork) + recip(callNetwork) +
                   tie(friendshipNetwork) + recip(friendshipNetwork) +
                   same(actors$gradeType) + same(actors$floor)

mod02Choice <- estimate(
  complexFormulaChoice,
  model = "DyNAM", subModel = "choice"
  )
summary(mod02Choice)


## ----simple-rate------------------------------------------------------------------
simpleFormulaRate <- callsDependent ~ indeg(friendshipNetwork)
mod01Rate <- estimate(
  simpleFormulaRate,
  model = "DyNAM", subModel = "rate"
  )


## ----estimate-init----------------------------------------------------------------
mod01Rate <- estimate(
  simpleFormulaRate, 
  model = "DyNAM", subModel = "rate",
  estimationInit = list(maxIterations = 40)
  )
summary(mod01Rate)


## ----complex-rate-----------------------------------------------------------------
complexFormulaRate <- 
  callsDependent ~ indeg(callNetwork) + outdeg(callNetwork) + 
                   indeg(friendshipNetwork) 

mod02Rate <- estimate(complexFormulaRate, model = "DyNAM", subModel = "rate")
summary(mod02Rate)


## ----intcpt-rate------------------------------------------------------------------
interceptFormulaRate <-
  callsDependent ~ 1 + indeg(callNetwork) + outdeg(callNetwork) +
                   indeg(friendshipNetwork)

mod03Rate <- estimate(interceptFormulaRate, model = "DyNAM", subModel = "rate")
summary(mod03Rate)


## ----waiting-time-----------------------------------------------------------------
mod03RateCoef <- coef(mod03Rate)
1 / exp(mod03RateCoef[["Intercept"]]) / 3600
# or days:
1 / exp(mod03RateCoef[["Intercept"]]) / 86400

# But what if it is not just a random call?
# Expected waiting time of those who have five outgoing call ties (five different actors)
1 / exp(
  mod03RateCoef[["Intercept"]] + mod03RateCoef[["outdeg"]] * 5
  ) / 3600
# Expected waiting time of those who have five outgoing and incoming call ties (five different actors)
1 / exp(
  mod03RateCoef[["Intercept"]] +
    mod03RateCoef[["outdeg"]] * 5 +
    mod03RateCoef[["indeg"]] * 5
  ) / 3600


## ----windows-rate-----------------------------------------------------------------
windowFormulaRate <-
  callsDependent ~ 1 + indeg(callNetwork) + outdeg(callNetwork) +
                   indeg(callNetwork, window = 300) +
                   outdeg(callNetwork, window = 300) +
                   indeg(friendshipNetwork)

mod04Rate <- estimate(windowFormulaRate, model = "DyNAM", subModel = "rate")
summary(mod04Rate)


## ----windows-choice---------------------------------------------------------------
windowFormulaChoice <-
  callsDependent ~ inertia(callNetwork) + recip(callNetwork) +
                   inertia(callNetwork, window = 300) +
                   recip(callNetwork, window = 300) +
                   tie(friendshipNetwork) + recip(friendshipNetwork) +
                   same(actors$gradeType) + same(actors$floor)

mod03Choice <- estimate(windowFormulaChoice,
                         model = "DyNAM", subModel = "choice")
summary(mod03Choice)


## ----aic--------------------------------------------------------------------------
AIC(mod02Choice)
AIC(mod03Choice)
AIC(mod03Rate)
AIC(mod04Rate)


## ----objs2------------------------------------------------------------------------
goldfishObjects()
head(calls_300)
tail(calls_300)


## ----rem--------------------------------------------------------------------------
allFormulaREM <-
  callsDependent ~ 1 + inertia(callNetwork) + recip(callNetwork) +
                   inertia(callNetwork, window = 300) +
                   recip(callNetwork, window = 300) +
                   tie(friendshipNetwork) + recip(friendshipNetwork) +
                   same(actors$gradeType) + same(actors$floor)


## ----rem-gather, eval=FALSE-------------------------------------------------------
## mod01REM <- estimate(
##   allFormulaREM, model = "REM",
##   estimationInit = list(engine = "gather_compute")
##   )
## 


## ----rem-c------------------------------------------------------------------------
mod01REM <- estimate(
  allFormulaREM, model = "REM",
  estimationInit = list(initialDamping = 40, engine = "default_c")
  )

summary(mod01REM)

