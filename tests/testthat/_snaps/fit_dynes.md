# the singular-information warning for a coefficient at its start

    Code
      warn_singular_information(list(coefficients = 2L, groups = list()),
      singular_labels, start = c(0.1, 0, 0), value = c(0.4, 0, 0.2))
    Condition
      Warning:
      ! The pooled information matrix at the returned estimate is singular; the covariances are `NA`.
      x calls › choice: inertia/calls has no information in the pool; it was not estimated and stays at its starting value.
      i Consider removing its effect from the specification.

# the singular-information warning for a coefficient that moved

    Code
      warn_singular_information(list(coefficients = c(2L, 3L), groups = list()),
      singular_labels, start = c(0.1, 0, 0), value = c(0.4, 0, 0.2))
    Condition
      Warning:
      ! The pooled information matrix at the returned estimate is singular; the covariances are `NA`.
      x calls › choice: inertia/calls has no information in the pool; it was not estimated and stays at its starting value.
      x friendship › choice: indeg/friendship has no information in the final pool; the returned value 0.2 is not an estimate.
      i Consider removing their effects from the specification.

# the singular-information warning naming no coefficient

    Code
      warn_singular_information(no_attribution, singular_labels, start = c(0.1, 0, 0),
      value = c(0.4, 0, 0.2))
    Condition
      Warning:
      ! The pooled information matrix at the returned estimate is singular; the covariances are `NA`.
      i No single coefficient is without information.

# an effect with no variation on any path is named at its start

    Code
      cat(conditionMessage(run$warnings[[1]]))
    Output
      ! The pooled information matrix at the returned estimate is singular; the covariances are `NA`.
      x friendship › dissolution › choice: indeg/friendship and calls › choice: inertia/calls have no information in the pool; they were not estimated and stay at their starting values.
      i Consider removing their effects from the specification.

# logLik, AIC and BIC refuse on a Monte-Carlo fit

    Code
      logLik(fit)
    Condition
      Error in `logLik()`:
      ! `logLik()` is not defined for a <goldfishDynesFit> object.
      x The estimator ascends a Monte-Carlo estimate of the observed-data likelihood, so there is no exact log-likelihood to return, and `AIC()` and `BIC()` cannot be computed for this fit.
      i The ascent diagnostics are in the fit's em_trace.

# print and summary group per process and show both errors

    Code
      print(fit)
    Message
      -- <goldfishDynesFit> ----------------------------------------------------------
      ! Not converged.
      `max_iterations` ended the run after 2 accepted EM iterations; the estimates
      are the last accepted ones.
      
      friendship › rate
    Output
            1  
      -1.7918  
    Message
      
      friendship › choice
    Output
         inertia/friendship  recip/friendship [Fx]  
                        0.6                   -0.5  
    Message
      
      calls › rate
    Output
             Intercept  indeg/friendship  
                 -1.25              0.30  
    Message
      
      calls › choice
    Output
       1  
      NA  
    Code
      summary(fit)
    Message
      -- <goldfishSummDynesFit> ------------------------------------------------------
      ! Not converged.
      `max_iterations` ended the run after 2 accepted EM iterations; the estimates
      are the last accepted ones.
      
      friendship › rate
    Output
        Estimate Std. Error MC Error z-value Pr(>|z|)
      1  -1.7918                                     
    Message
      
      friendship › choice
    Output
                            Estimate Std. Error MC Error z-value Pr(>|z|)
      inertia/friendship        0.60       0.20     0.02       3   0.0027
      recip/friendship [Fx]    -0.50                                     
    Message
      
      calls › rate
    Output
                       Estimate Std. Error MC Error z-value  Pr(>|z|)
      Intercept           -1.25       0.30     0.03 -4.1667 3.091e-05
      indeg/friendship     0.30       0.10     0.01  3.0000    0.0027
    Message
      
      calls › choice
    Output
        Estimate Std. Error MC Error z-value Pr(>|z|)
      1                                              
    Message
      
      MC Error is the Monte-Carlo error of the estimate from the final pool of 4
      augmented sequences.

