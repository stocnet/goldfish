## ----load, message=FALSE------------------------------------------------------
library(goldfish)
data("social_evolution")
# ?social_evolution
social_evolution


## ----quick--------------------------------------------------------------------
mod00Rate <- estimate_dynam(
  calls ~ indeg + outdeg,
  sub_model = "rate",
  data = social_evolution
)
summary(mod00Rate)

mod00Choice <- estimate_dynam(
  calls ~ inertia + recip + trans,
  sub_model = "choice",
  data = social_evolution
)
summary(mod00Choice)


## ----load-raw, message=FALSE--------------------------------------------------
library(manynet)
data("Social_Evolution")
head(actors)
head(calls)
head(friendship)


## ----build--------------------------------------------------------------------
social_evolution_built <- from_ties(
  as_stocnet(friendship),
  as_stocnet(calls),
  layer_names = c("friendship", "calls")
) |>
  join_nodes(actors) |>
  rename_nodes()
social_evolution_built


## ----add-info-----------------------------------------------------------------
social_evolution_built <- add_info(
  social_evolution_built,
  name = "Social Evolution MIT",
  # focal is optional -- the formula LHS names the dependent. Recorded here only
  # to set the shipped object's default; drop it and `calls ~ ...` still works.
  focal = "calls",
  directed = c(friendship = TRUE, calls = TRUE),
  observation = c(friendship = "panel", calls = "event")
)


## ----plot-teaching1, message=FALSE, warning=FALSE-----------------------------
library(igraph)
library(ggraph)
library(migraph)
# The network at half time
half_time <- calls$time[floor(nrow(calls) / 2)]
callNetworkHlf <- network_state_at(social_evolution, "calls", time = half_time)
floorHlf <- nodes_state_at(social_evolution, time = half_time)$floor

callNetworkHlf <- callNetworkHlf |>
  as_igraph() |>
  add_node_attribute("floor", floorHlf)

graphr(callNetworkHlf, labels = FALSE, layout = "fr") +
  geom_node_point(aes(color = as.factor(floor)), size = 2, show.legend = FALSE)

# The tie strength at the end
end_time <- max(calls$time) + 1
callNetworkEnd <- network_state_at(social_evolution, "calls", time = end_time)
table(callNetworkEnd)


## ----effects, eval=FALSE------------------------------------------------------
# vignette("goldfishEffects")


## ----simple-formula-----------------------------------------------------------
simpleFormulaChoice <- calls ~ tie(friendship)


## ----simple-choice------------------------------------------------------------
mod01Choice <- estimate_dynam(
  simpleFormulaChoice,
  sub_model = "choice",
  data = social_evolution
)
summary(mod01Choice)


## ----complex-choice-----------------------------------------------------------
complexFormulaChoice <-
  calls ~ inertia(calls) + recip(calls) +
    tie(friendship) + recip(friendship) +
    same(gradeType) + same(floor)

mod02Choice <- estimate_dynam(
  complexFormulaChoice,
  sub_model = "choice",
  data = social_evolution
)
summary(mod02Choice)


## ----simple-rate--------------------------------------------------------------
simpleFormulaRate <- calls ~ indeg(friendship)
mod01Rate <- estimate_dynam(
  simpleFormulaRate,
  sub_model = "rate",
  data = social_evolution
)


## ----estimate-init------------------------------------------------------------
mod01Rate <- estimate_dynam(
  simpleFormulaRate,
  sub_model = "rate",
  data = social_evolution,
  control_algo = set_algorithm_newton(max_iterations = 40)
)
summary(mod01Rate)


## ----complex-rate-------------------------------------------------------------
complexFormulaRate <-
  calls ~ indeg(calls) + outdeg(calls) + indeg(friendship)

mod02Rate <- estimate_dynam(
  complexFormulaRate,
  sub_model = "rate",
  data = social_evolution
)
summary(mod02Rate)


## ----intcpt-rate--------------------------------------------------------------
interceptFormulaRate <-
  calls ~ 1 + indeg(calls) + outdeg(calls) + indeg(friendship)

mod03Rate <- estimate_dynam(
  interceptFormulaRate,
  sub_model = "rate",
  data = social_evolution
)
summary(mod03Rate)


## ----waiting-time-------------------------------------------------------------
mod03RateCoef <- coef(mod03Rate)
mod03RateCoef

1 / exp(mod03RateCoef[["Intercept"]]) / 3600
# or days:
1 / exp(mod03RateCoef[["Intercept"]]) / 86400

# But what if it is not just a random call?
# Expected waiting time of those who have five outgoing call ties
# (five different actors)
1 / exp(
  mod03RateCoef[["Intercept"]] + mod03RateCoef[["odeg"]] * 5
) / 3600
# Expected waiting time of those who have five outgoing and incoming call ties
# (five different actors)
1 / exp(
  mod03RateCoef[["Intercept"]] +
    mod03RateCoef[["odeg"]] * 5 +
    mod03RateCoef[["ideg_cal"]] * 5
) / 3600


## ----windows-rate-------------------------------------------------------------
windowFormulaRate <-
  calls ~ 1 + indeg(calls) + outdeg(calls) +
    indeg(calls, window = 300) +
    outdeg(calls, window = 300) +
    indeg(friendship)

mod04Rate <- estimate_dynam(
  windowFormulaRate,
  sub_model = "rate",
  data = social_evolution
)
summary(mod04Rate)


## ----windows-choice-----------------------------------------------------------
windowFormulaChoice <-
  calls ~ inertia(calls) + recip(calls) +
    inertia(calls, window = 300) +
    recip(calls, window = 300) +
    tie(friendship) + recip(friendship) +
    same(gradeType) + same(floor)

mod03Choice <- estimate_dynam(
  windowFormulaChoice,
  sub_model = "choice",
  data = social_evolution
)
summary(mod03Choice)


## ----aic----------------------------------------------------------------------
# Compare different specifications of the subModel = "choice"
AIC(mod02Choice, mod03Choice)

# Compare different specifications of the subModel = "rate"
AIC(mod03Rate, mod04Rate)


## ----rem----------------------------------------------------------------------
allFormulaREM <-
  calls ~
    1 + indeg(calls, type = "ego") + outdeg(calls, type = "ego") +
    indeg(friendship, type = "ego") +
    inertia(calls) + recip(calls) +
    inertia(calls, window = 300) + recip(calls, window = 300) +
    tie(friendship) + recip(friendship) +
    same(gradeType) + same(floor)


## ----rem-gather, eval=FALSE---------------------------------------------------
# mod01REM <- estimate_rem(
#   allFormulaREM,
#   data = social_evolution,
#   control_algo =
#     set_algorithm_newton(initial_damping = 40, backend = "cpp")
# )


## ----rem-c--------------------------------------------------------------------
mod01REM <- estimate_rem(
  allFormulaREM,
  data = social_evolution,
  control_algo = set_algorithm_newton(backend = "gather")
)

summary(mod01REM)




## ----diag-residuals-----------------------------------------------------------
mod01ChoiceDiag <- estimate_dynam(
  complexFormulaChoice,
  sub_model = "choice",
  data = social_evolution,
  return_preprocessed = TRUE
)

dev <- residuals(mod01ChoiceDiag, type = "deviance")
worst <- order(abs(dev), decreasing = TRUE)[1:3]
data.frame(
  event = worst,
  deviance = round(dev[worst], 2),
  fitted = round(fitted(mod01ChoiceDiag, type = "outcome")[worst], 4)
)


## ----diag-gof-----------------------------------------------------------------
# Qualified deliberately. This vignette attaches migraph further up, and
# migraph exports a (deprecated) `test_gof` of its own, so the bare name
# resolves to whichever package was attached last. RSiena publishes the same
# three test names too. `::` is the reliable route whenever more than one
# stocnet package is loaded.
goldfish::test_gof(mod01ChoiceDiag)


## ----diag-gof-plot, eval = has_plots, fig.alt = "Cumulative score process per effect against Brownian-bridge reference bands."----
# plot(goldfish::test_gof(mod01ChoiceDiag))


## ----diag-time----------------------------------------------------------------
goldfish::test_time(mod01ChoiceDiag)


## ----diag-time-plot, eval = has_plots, fig.alt = "Scaled Schoenfeld residuals per effect against model time, with a smooth and the fitted estimate."----
# plot(goldfish::test_time(mod01ChoiceDiag))

