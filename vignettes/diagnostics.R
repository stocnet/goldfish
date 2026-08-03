## ----setup--------------------------------------------------------------------
library(goldfish)




## ----fits---------------------------------------------------------------------
data("social_evolution")

# The two DyNAM sub-models of the same sequence, and the REM that models it as
# one dyadic process. `return_preprocessed = TRUE` on the choice fit is what
# lets the tests that need a pass run below.
calls_choice <- estimate_dynam(
  calls ~ inertia + recip + trans,
  sub_model = "choice",
  data = social_evolution,
  return_preprocessed = TRUE
)

calls_rate <- estimate_dynam(
  calls ~ 1 + indeg + outdeg,
  sub_model = "rate",
  data = social_evolution,
  control_algo = set_algorithm_newton(
    diagnostics = c("loglik", "scores", "margins")
  )
)


## ----cox-snell----------------------------------------------------------------
# Cox-Snell residuals are unit exponential under the model, so at the maximum
# their mean is 1 by the score equation of the intercept -- the fitted model
# reproduces the observed number of events.
cs <- residuals(calls_rate, type = "cox_snell")
c(mean = mean(cs), n_events = sum(!calls_rate$right_censored_events))


## ----outliers-----------------------------------------------------------------
outliers <- diagnose_outliers(calls_choice)
head(outliers[outliers$outlier, c("time", "sender", "receiver", ".series")], 3)


## ----margins------------------------------------------------------------------
margins <- margin_table(calls_rate)
head(margins, 3)


## ----gof----------------------------------------------------------------------
gof <- test_gof(calls_choice)
gof


## ----gof-plot, eval = has_plots, fig.alt = "Standardized cumulative score process for each effect, against Brownian-bridge reference bands."----
# plot(gof)


## ----onset--------------------------------------------------------------------
onset <- diagnose_onset(calls_choice)
head(onset$accrual, 3)
tail(onset$accrual, 2)


## ----onset-plot, eval = has_plots, fig.alt = "Per-coefficient parameter path windowed on its excursion, above the information-accrual curve against the proportional diagonal."----
# plot(onset)


## ----gof-information-clock----------------------------------------------------
test_gof(calls_choice, clock = "information", n_sim = 200)$effects[, c("term", "p_value")]


## ----time-trend---------------------------------------------------------------
test_time(calls_choice)


## ----time-plot, eval = has_plots, fig.alt = "Scaled Schoenfeld residuals per effect against model time, with a smooth and the fitted estimate as reference."----
# plot(test_time(calls_choice))


## ----time-periods-------------------------------------------------------------
regimes <- test_time(calls_choice, method = "periods", periods = 3L)
regimes$periods


## ----parameter----------------------------------------------------------------
held <- estimate_dynam(
  calls ~ inertia + recip + offset(trans, coef = 0),
  sub_model = "choice",
  data = social_evolution,
  return_preprocessed = TRUE
)
test_parameter(held)


## ----transformer--------------------------------------------------------------
plain <- estimate_dynam(
  calls ~ inertia + recip,
  sub_model = "choice", data = social_evolution
)
transformed <- estimate_dynam(
  calls ~ inertia(calls, transformer_fn = log1p) +
    recip(calls, transformer_fn = log1p),
  sub_model = "choice", data = social_evolution
)
# Identical -- not merely a rescaling.
c(plain = as.numeric(logLik(plain)), transformed = as.numeric(logLik(transformed)))
identical(unname(coef(plain)), unname(coef(transformed)))


## ----weighted-transform-------------------------------------------------------
weighted <- estimate_dynam(
  calls ~ inertia(calls, weighted = TRUE) + recip(calls, weighted = TRUE),
  sub_model = "choice", data = social_evolution
)
weighted_log <- estimate_dynam(
  calls ~ inertia(calls, weighted = TRUE, transformer_fn = log1p) +
    recip(calls, weighted = TRUE, transformer_fn = log1p),
  sub_model = "choice", data = social_evolution
)
c(
  weighted = as.numeric(logLik(weighted)),
  weighted_log = as.numeric(logLik(weighted_log))
)


## ----aggregation--------------------------------------------------------------
# Deviance summed per sender: which actors the model reproduces worst.
dev <- residuals(calls_choice, type = "deviance")
by_sender <- tapply(dev^2, calls_choice$dependent_events$sender, sum)
head(sort(by_sender, decreasing = TRUE), 3)


## ----comparison---------------------------------------------------------------
calls_rem <- estimate_rem(
  calls ~ 1 + inertia + recip + trans,
  data = social_evolution
)
c(
  rem = as.numeric(logLik(calls_rem)),
  dynam = as.numeric(logLik(calls_rate)) + as.numeric(logLik(calls_choice))
)

