# an observed information that is not positive definite warns once

    Code
      cat(message)
    Output
      ! The Monte-Carlo estimate of the observed information at the returned estimate is not positive definite; the covariances are `NA`.
      x The pool's estimate of the missing information is as large as the complete information in some direction.
      i Consider a larger `n_sequences` or a model-driven `routine` ("sim" or "mcmc").

# a non-converged fit is told the estimate may not be a maximum

    Code
      cat(message)
    Output
      ! The Monte-Carlo estimate of the observed information at the returned estimate is not positive definite; the covariances are `NA`.
      x The pool's estimate of the missing information is as large as the complete information in some direction.
      i The run did not converge, so the estimate may not be a maximum, and the observed information need not be positive definite there.
      i Consider a larger `n_sequences` or a model-driven `routine` ("sim" or "mcmc").

# summary() reports the diagnostics for each covariance status

    Code
      print(ok)
    Message
      -- <goldfishSummDynesFit> ------------------------------------------------------
      v Converged after 1 accepted EM iteration.
      v At the final pool's optimum: Newton step 0.0123 standard errors.
      
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
      Largest fraction of missing information: 0.432.
      Covariance effective sample size: 17.5.

---

    Code
      print(not_pd)
    Message
      -- <goldfishSummDynesFit> ------------------------------------------------------
      v Converged after 1 accepted EM iteration.
      
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
      ! Standard errors are blank: the observed information is not positive
      definite.
      The pool's estimate of the missing information is as large as the complete
      information in some direction. Consider a larger `n_sequences` or a
      model-driven `routine`.
      Covariance effective sample size: 2.5.

---

    Code
      print(singular)
    Message
      -- <goldfishSummDynesFit> ------------------------------------------------------
      v Converged after 1 accepted EM iteration.
      
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
      ! Standard errors are blank: the complete-data information is singular.
      An effect with no variation across the augmented sequences carries no
      information; see the warning for the coefficients.
      Covariance effective sample size: 9.25.

---

    Code
      print(dropped)
    Message
      -- <goldfishSummDynesFit> ------------------------------------------------------
      v Converged after 1 accepted EM iteration.
      
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
      ! Standard errors are blank: the pool stopped with no finite likelihood.

# an unknown information type is rejected

    Code
      vcov(fit, information = "complete")
    Condition
      Error in `vcov()`:
      ! `information` must be one of "observed" or "complete_data", not "complete".
      i Did you mean "complete_data"?

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
      v At the final pool's optimum: Newton step 0.0123 standard errors.
      
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
      v At the final pool's optimum: Newton step 0.0123 standard errors.
      
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
      Largest fraction of missing information: 0.25.
      Covariance effective sample size: 3.5.

# print and summary state the optimum gate beside converged

    Code
      print(at)
    Message
      -- <goldfishDynesFit> ----------------------------------------------------------
      v Converged after 1 accepted EM iteration.
      v At the final pool's optimum: Newton step 0.0123 standard errors.
      
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
      summary(at)
    Message
      -- <goldfishSummDynesFit> ------------------------------------------------------
      v Converged after 1 accepted EM iteration.
      v At the final pool's optimum: Newton step 0.0123 standard errors.
      
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
      Largest fraction of missing information: 0.25.
      Covariance effective sample size: 3.5.

---

    Code
      print(off)
    Message
      -- <goldfishDynesFit> ----------------------------------------------------------
      v Converged after 1 accepted EM iteration.
      ! Not at the final pool's optimum.
      The Newton step to it is 0.457 standard errors, above the 0.1 threshold of
      at_optimum.
      
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
      summary(off)
    Message
      -- <goldfishSummDynesFit> ------------------------------------------------------
      v Converged after 1 accepted EM iteration.
      ! Not at the final pool's optimum.
      The Newton step to it is 0.457 standard errors, above the 0.1 threshold of
      at_optimum.
      
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
      Largest fraction of missing information: 0.25.
      Covariance effective sample size: 3.5.

# a resampling fit's alert names the objective it gates

    Code
      print(off)
    Message
      -- <goldfishDynesFit> ----------------------------------------------------------
      v Converged after 1 accepted EM iteration.
      ! Not at the final pool's optimum.
      The Newton step to it is 0.457 standard errors, above the 0.1 threshold of
      at_optimum.
      i The gate reads the importance-weighted pool, not the resampled multiset the
        M-step optimized.
      
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

