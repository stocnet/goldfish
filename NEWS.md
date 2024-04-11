# goldfish 1.6.7

* Debug and update yaml workflow files for GitHub Actions.
* Solve note mismatches generic/method consistency

# goldfish 1.6.6

* Debugging and extend documentation for `GatherPreprocessing()`.
* Fix note from CRAN checks.
* Debug issue when using a preprocess init object in `estimate()`.
* Debug issue with opportunity list in `estimate()` for `DyNAM` models.
* Fix error in printing output from `estimate()` when using
  a parameter is fixed to a value.  

# goldfish 1.6.5

* Solves `startTime` and `endTime` bug on `DyNAM` and `REM` models
  preprocessing.
* Export `GatherPreprocessing()`. Experimental functionality.
* Clean unnecessary functions imports.
* Solves `aes_string()` deprecation.
* Solves issue on `C++` engine on DyNAM-rate.
* Enforcing an 80-character line length on C++ code.
* Comply with code style.
* Changes that reduce execution time in the R estimation engine.

# goldfish 1.6.4

* Change mentioning of `.GlobalEnv` and drop `goldfishObjects()`.

# goldfish 1.6.3

## New features

* Add DyNAM-i vignette.
* Implement method `vcov()`, now is possible to use `stats::confint()` to obtain
  confidence interval for parameters estimates under asymptotic normality, and
  also get them from a `tidy()` call.

## Breaking changes

* `silent` parameter replace by `progress`.
  `progress` and `verbose` can be set with global options with the same name.

## Minor improvements and fixes

* Reimplement method `logLik()`, now is possible to use `AIC()` and `BIC()`
  from `stats` package and make likelihood ratio test of nested models using
  `lmtest::lrtest()`.
* Improve documentation of `define` functions, `linkEvents()` and `print` methods.
  The reserved names and expected variables are explained.
* Drop `fig.retina` from figures in vignettes and keeps default size.
* Update `vignette("goldfishEffects")` documentation for closure effects.
* Window effects doesn't create new object on the global environment.
* Add references in description.
* Fix bug composition change on estimation routines.
* Includes environment argument on estimation routines.

# goldfish 1.6.2

* Fix additional URL on Vignette "How to start".

# goldfish 1.6.1

* Fix URLs. 
* Estimate examples now use the `C` engine.

# goldfish 1.6.0

* Cleaning submission to CRAN.

# goldfish 1.5.3

* Effects documentation migrated to a vignette.
* Precompile vignettes with long-run time for compilation.

# goldfish 1.5.2

* Fix minor bugs on `opportunityList`
* New effect ego alter interaction `egoAlterInt(list(egoAttr, alterAttr))`
* Cleaning vignettes and changing networks visualization to use `migraph` package

## Breaking changes

* Closure effects renaming. 
  
  ```R
  clSender(net) -> commonSender(net)
  clReceiver(net) -> commonReceiver(net)
  mixedClSender(list(net1, net2)) -> mixedCommonSender(list(net1, net2))
  mixedClReceiver(list(net1, net2)) -> mixedCommonReceiver(list(net1, net2))
  ```
  
* Updated `examine.outliers()` and `examine.changepoints()` to plot more informative plots (with `{ggplot2}`)
  - These functions no longer print to the console
* Added vignette `teaching2.Rmd` that also indicates these extensions
* Added vignette `teaching1.Rmd` with the "How to start"


# goldfish 1.5.1

* Re #3 added `{broom}`-related functions `tidy()` and `glance()`
* Updated `examine.outliers()` and `examine.changepoints()` to plot more informative plots (with `{ggplot2}`)
  - These functions no longer print to the console
* Added vignette `teaching2.Rmd` that also indicates these extensions
* Added vignette `teaching1.Rmd` with the "How to start"

# goldfish 1.5.0

## New features

* New DyNAM-i model available to model face-to-face interaction data, collected through video or RFID badges.
* Functions to transform interaction data into DyNAM-compatible objects

  ```R
  defineGroups_interaction(interactions_data, actors, seed.randomization)
  ```
* Estimation functions for the joining and leaving rates of a DyNAM-i model

  ```R
  estimate(
    dependent.events_interactions ~  1 + intercept(network_interactions, joining = 1) +
                                  ego(actors$age,joining = 1, subType = "centered") +
                                 ego(actors$age,joining = -1, subType = "centered"),
    model = "DyNAMi", subModel = "rate")
  ```
  
* Estimation functions for the choice part of a DyNAM-i model

  ```R
  estimate(
    dependent.events_interactions ~ diff(actors$age,subType="averaged_sum") ,
    model = "DyNAMi", subModel = "choice", 
    estimationInit = list(opportunitiesList = opportunities))
  ```
* New closure effects for `model = "DyNAM"` with `subModel = "choice"` and `model = "REM"`.
  Documentation is updated accordingly. 
  
  ```R
  cycle(bilatnet)
  clSender(bilatnet)
  clReceiver(bilatnet)
  mixedCycle(list(bilatnet, contignet))
  mixedClSender(list(bilatnet, contignet))
  mixedClReceiver(list(bilatnet, contignet))
  ```
  
## Minor improvements and fixes

* Minor bugs in parsing and printing solved.
* Solves some warnings regarding S3 generic/method consistency.
* Solves no visible binding for global variables.
* Documentation improvements creating new documentation pages
  aggregating similar functions on `print-method` and `update-method`.

## Breaking changes

* `tertius_diff()` and `node_trans()` are changed to `tertiusDiff()` and 
  `nodeTrans()` complaining with naming convention.

# goldfish 1.4.3

## New features

* `mixedTrans()` effect is created. It requires the definition of two networks
  to compute the number of two paths between nodes.

  ```R
  mixedTrans(list(bilatnet, contignet))
  ```

## Minor improvements and fixes

* `estimate()` no longer give warnings in R 4.0.0.
* Classes and types checkings are revisited and extended.
* `linkEvents()` is refactored as a S3 method.
* New `print` or `summary` methods for `goldfish` objects are developed.
* `head()` and `tail()` methods for `nodes.goldfish`, `network.goldfish`
  and `dependent.goldfish` are available.

# goldfish 1.3.2

## New features

* `tertius()` and `tertius_diff()` effects are created. It requires the
  definition of a network and an attribute.

  ```R
  tertius_diff(bilatnet, states$gdp)
  tertius(bilatnet, states$gdp)
  ```

# goldfish 1.3.1

## New features

* `estimate()` now estimation routine in `C` for `model = "DyNAM"` and
  `subModel = "choice_coordination"`.

# goldfish 1.3.0 

## New features

* `estimate()` parameter `engine` through `estimationInit` argument control the estimation
  routine used.
  
  ```R
  mod01 <- estimate(callsDependent ~ inertia + recip + trans,
                    model = "DyNAM", subModel = "choice",
                    estimationInit = list(engine = "gather_compute"))
  ```
  
* `estimate()` now estimation routine in `C` except for `model = "DyNAM"` and
  `subModel = "choice_coordination"`.
* New effects definition for `model = "DyNAM"` and `subModel = "rate"`.

## Minor improvements and fixes
* `inertia` fixed bug when `weighted = TRUE`.

## Breaking changes

* `modelType` argument in `estimate()` have been deprecated in favor of `model` 
   and `submodel`.
   
## Internal changes

* New implementation of effects to improve preprocessing time in the initialization
  of the statistical matrices and the reintroduction of a cache object for complex 
  structural network effects.

## Minor improvements and fixes

* Added more examples for `estimate()`.
* Extend documentation of `defineDependentEvents()`, `defineGlobalAttribute()`, 
  `defineNetwork()`, `defineNodes()` and `linkEvents()`.

# goldfish 1.2.1 

* Introduce a new efficient estimation routine.