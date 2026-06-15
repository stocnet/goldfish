# goldfish 1.8.1

This patch release compacts how constant-value fan-out effects are stored
during preprocessing. Coefficient estimates and log-likelihoods reproduce to
within 1e-6 of the previous implementation on both estimation engines.

## Internal changes

* Constant-value fan-out effect updates — `alter()`, `ego()`, the degree
  effects (`indeg` / `outdeg` / `degree`) in their alter/ego projection, and
  `global()` — are now stored as a single coded entry in a new
  `stat_mat_broadcast` buffer (with `stat_mat_broadcast_pointer`) instead of
  one duplicate column per affected cell in `stat_mat_update`. This removes the
  dominant preprocessing memory cost for dyad models that use these effects
  (for example, `stat_mat_update` shrinks by more than an order of magnitude on
  REM models dominated by `alter()` / `ego()`), and avoids the integer overflow
  that large fan-out models could previously hit. Both estimation engines (the
  R `default` and the C++ `default_c` / `gather_compute`) decode the broadcast
  buffer per event, honouring the reflexive-diagonal and two-mode rules; the
  gather and database writers expand it when materialising the gather stack.
* The `preprocessed.goldfish` format version is bumped. Objects preprocessed
  with an earlier goldfish version are rejected by the `preprocessing_init`
  version check with a message to recompute them; the public R API is
  unchanged.

# goldfish 1.8.0

This release refactors the preprocessing and estimation pipeline around typed
model specifications and a writer strategy, and adds a public preprocessing
entry point. Coefficient estimates reproduce to within 1e-6 of the previous
implementation on both estimation engines.

## New features and user-facing changes

* New exported function `compute_stats()` runs the preprocessing stage of a
  model and returns the change statistics as a `"preprocessed.goldfish"`
  object, without estimating the model. The result can be passed to the
  estimation functions through their `preprocessing_init` argument. Its
  `output` argument selects the statistics writer: `"default"` (the
  estimation-ready `preprocessed.goldfish` object), `"gather"` (the gather
  stack, one row per event × alternative), and `"db"` (gather rows streamed
  to a database table).
* `estimate_dynam()` gains `sub_model = "rate_ordered"` to declare the
  ordinal activity rate model (only the order of the events is modeled,
  partial likelihood as in the CoxPH model) explicitly. The previous
  specification of this model, `sub_model = "rate"` with a formula without
  the time intercept, is deprecated and now emits a warning suggesting
  `"rate_ordered"`.
* `estimate_rem()` gains a `sub_model` argument with valid values `"rate"`
  (full dyadic hazard model, the default) and `"rate_ordered"` (ordinal
  case). The internal `"choice"` label used so far for REM models is kept
  as a deprecated alias of `"rate"` and emits a warning.
* The `global()` effect now errors for `sub_model = "choice"` and
  `"choice_coordination"`: a global covariate is constant across
  alternatives, so its main effect is not identified in a multinomial
  choice model. Support through interaction effects is planned for a
  future release; rate sub-models keep accepting `global()`.
* Effects with `ignore_repetitions = TRUE` now error immediately: the
  previous implementation computed incorrect statistics (it always masked
  repetitions using the dependent network instead of the network the
  effect is applied to). The feature is disabled pending a correct
  reimplementation (#105).
* `gather_model_data()` is reimplemented as a thin wrapper over
  `compute_stats(..., output = "gather")` and now also handles one-mode
  rate models, which previously errored.
* `compute_stats(..., output = "db")` streams the gather rows to the
  database table configured via `set_preprocessing_opt(db = , db_table = )`
  in event-aligned batches, returning a lightweight descriptor instead of
  the in-memory stack. This makes the previously reserved `db` / `db_table`
  options functional. `RSQLite` is added to Suggests for the round-trip
  tests.
* `preprocessed.goldfish` objects now carry a format `version`; passing an
  object preprocessed with a previous goldfish version through
  `preprocessing_init` errors with a message to recompute it with
  `compute_stats()`.
* The model summary now reports the two convergence criteria below the return
  code: the score (`score_rel_norm`, the likelihood-scaled relative gradient
  norm checked against `score_tol`) and the step (`maxAbsUpdate`, the maximum
  absolute parameter update checked against `step_tol`). `score_rel_norm` is
  also added to the `convergence` list of the fitted object.
* Fixed an integer overflow when preprocessing very large models: the growing
  statistics buffer used 32-bit integer arithmetic, so models with more than
  ~2^31 statistic updates failed with `invalid 'ncol' value (too large or NA)`.
  Buffer sizing now uses double-precision arithmetic, and a model that would
  exceed R's hard matrix column limit (2^31 - 1) now stops with an informative
  error suggesting `compute_stats(output = "db")` or fewer effects instead of a
  cryptic overflow.

## Internal changes

* Model variants are carried through the pipeline as typed `model_spec` S3
  objects constructed once from `(model, sub_model, is_two_mode)`;
  preprocessed and fitted objects store the spec in a `model_spec` field,
  and preprocessing and estimation dispatch on the spec class.
* Preprocessing runs through dedicated recipe methods for every model
  variant. The DyNAM rate and rate-ordered variants share a sender-indexed
  event-loop kernel; DyNAM choice, choice-coordination, REM, and
  REM-ordered share a dyad-indexed kernel. DyNAMi variants participate in
  the dispatch but delegate to the existing DyNAMi loop unchanged.
* The preprocessing result stores statistics updates in a single flat
  matrix (`stat_mat_update` with `stat_mat_pointer` and `is_dependent`)
  covering dependent and right-censored events; the nested `stats_change`
  list is no longer produced. Rate models additionally store the intercept
  scalars (`n_dep_events`, `total_time`, `avg_active_actors`) and the
  presence composition changes in the format the estimation engines consume.
* Recipes emit their output exclusively through a writer strategy
  (`init()` / `write_event()` / `finalize()`), so a single event loop
  serves every output format (`compute_stats(output = )` selects the
  writer). The gather stack is produced in R from the flat buffer; the
  post-hoc C++ `gather_()` routines (`gather_sender_model()`,
  `gather_receiver_model()`, `gather_sender_receiver_model()`) have been
  removed and `engine = "gather_compute"` consumes the native output.
* Estimation is self-contained and dispatches on the model specification
  class. `estimate_int()`, the per-event contribution
  (`compute_event_contribution()`, formerly `getEventValues()`), and the
  per-event update (`compute_step()`) are S3 generics resolved once at the
  start of estimation; no model-type string comparison or S3 dispatch runs
  inside the event loop. The Newton-Raphson outer loop is shared through
  `run_nr_loop()` with the `score_tol` / `step_tol` stopping criteria
  unchanged.
* The R and C++ estimation engines consume the combined flat update buffer
  directly. The estimation routines no longer reach back into the
  preprocessing environment (`prepEnvir` / `get()`), re-derive actor
  counts, or apply the dead per-event mean imputation; statistics are
  asserted NA-free at estimation entry. The C++ routines take the combined
  buffer with `is_dependent` instead of separate right-censored matrices.
* The `ignore_repetitions` masking, `modifyStatisticsList()` /
  `reduceStatisticsList()` hot-path calls, and the dual-read shim for the
  old `stats_change` format are removed; a thin `prepare_statslist()`
  handles the intercept prepend and `excludeParameters` dropping.

# goldfish 1.7.3

* `make_global_attributes()` (renamed from `make_global_attribute()`) now
  accepts a one-row snapshot data frame with named numeric columns.
  The plural name reflects that one object holds multiple global attribute
  columns.
* `global(df$col)` effect is now functional for DyNAM-rate and REM models.
  The effect broadcasts a time-varying scalar to all actors at initialization
  and emits per-actor change statistics whenever the global attribute is
  updated by a linked event.
* `link_events()` gains a method for `global.goldfish` objects.
  Only replace-semantics events (no `increment` column) are supported;
  the `replace` argument names the value column in the events data frame.

* `window` is now correctly supported for the four `mixed_*` effects
  (`mixed_trans`, `mixed_cycle`, `mixed_common_sender`, `mixed_common_receiver`)
  when the network argument is a `list(net1, net2)` expression. Previously
  the formula parser errored with a cryptic "object not found" message.
* Using `window` with an attribute-only effect (e.g., `alter(nodes$attr, window = 5)`,
  `same`, `sim`, `diff`, `ego`, `ego_alter_interaction`) now raises a descriptive
  error identifying the effect and attribute, and lists all violations in the formula
  at once. Previously this silently produced results with undefined semantics.
* Two-path cache counts in closure effects (`trans`, `cycle`, `common_sender`,
  `common_receiver`, and all `mixed_*` variants) are now clamped to zero via
  `pmax(0L, ...)` instead of filtering out negative rows. Previously, when all
  incremental counts were negative the cache was silently left unchanged;
  counts are now always non-negative and all affected entries are updated.
* All four `mixed_*` effects (`mixed_trans`, `mixed_cycle`,
  `mixed_common_sender`, `mixed_common_receiver`) gain a `history` parameter
  accepting `"pooled"` (default, existing behavior) or `"sequential"`. With
  `"sequential"`, the argument order of the two networks defines the temporal
  sequence: additions to the second network count existing first-network
  neighbors, while additions to the first network produce no new two-path
  entries. Removals always update the cache regardless of `history`.
* Improved convergence diagnostics in the Newton-Raphson optimizer:
  two stopping criteria are now checked independently and the first
  criterion met is reported as a return code in the model summary.
  `set_estimation_opt()` gains `score_tol` (default `1e-6`, scale-invariant
  relative gradient criterion) and `step_tol` (default `1e-8`, damped Newton
  step size criterion). The summary line now reads
  "Return code 1: gradient close to zero" or
  "Return code 2: step size close to zero (damped)" and is followed by
  the number of free and fixed parameters estimated.
* `set_estimation_opt(convergence_criterion)` is deprecated.
  Use `score_tol` instead (default `1e-6`).
* Fix premature convergence in the Newton-Raphson optimizer: when a damped
  step overshot and the trial log-likelihood was non-finite (`-Inf`), the
  likelihood-scaled score criterion evaluated to zero and the algorithm
  stopped reporting "gradient close to zero" at unconverged estimates.
  The stopping criteria, extracted into an internal `check_convergence()`
  helper shared by both estimation engines, are now evaluated on the
  accepted iterate, and the score criterion only on iterations whose step
  improved the log-likelihood. The step size criterion remains active on
  rejected steps as the stalled exit.

# goldfish 1.7.2

* Refactored DyNAM-rate preprocessing pipeline: statistics are now stored as
  an `n1 × nEffects` matrix instead of a 3D array, eliminating the
  `reduceMatrixToVector` step at estimation time.
* Unified preprocessing output fields: `stats_change`, `intervals`,
  `is_dependent`, `event_time`, `event_sender`, `event_receiver` (snake_case);
  composition change vectors (`active_mode1/2_init`, `active_mode1/2_changes`)
  are now stored in the preprocessing object instead of being re-derived during
  estimation.
* DyNAM-rate effect functions are now the base implementations returning
  2-column `(node1, replace)` changes; REM and choice effects delegate to them
  via `to_ego()` / `to_alter()` helpers (new `R/utils_effects.R`).
* New DyNAM-rate effects: `degree()` (undirected), `triangle()` (undirected),
  and `global()` for time-varying global covariates.
* Added `db` and `db_table` parameters to `set_preprocessing_opt()` (reserved
  options for streaming statistics to a DBI-compatible database; the streaming
  writer that consumes them was implemented in a later release).
* Added `DBI` to package imports.

# goldfish 1.7.1

* Created new data objects for diagnostic methods (#116). 
* Plotting for diagnostic methods migrated to autograph (#115).
* Added mode imputation for non-numeric attributes (#112).
* Fix bug when preprocessed data is used in `estimate_` functions due to
  an incorrect comparison of formulas (#111).
* Provided more informative error messages when missing data is present in
  data objects (#82).
* Provided more informative error messages when elements that are not available
  are used in estimate formula (#81).
* Allow for `max_iterations` in `set_estimation_opt()` to be set to `0` to
  retain model statistics without running the estimation algorithm.
* Remove unused variable in `compute_poisson_selection()`.

# goldfish 1.7.0

* Rename functions to follow tidyverse style guide.
* Rename `define` functions to `make_`: `make_nodes()`, `make_network()`,
  `make_dependent_events()`.
* Introduce `set_` functions to set options: `set_estimation_opt()` 
  for algorithm options, and `set_preprocess_opt()` for preprocessing options.
* Introduce `make_data()` function to create a single data object with all
  the data needed for estimation (#100).
* Introduce `history = c("pooling", "sequential", "consecutive")` argument
  to `trans()` and `cycle()` effect functions that define how the
  previous history of events is used to compute the effect (#105).
* Add a function for each model implemented in the package: `estimate_dynam()`,
  `estimate_dynami()` and `estimate_rem()`.

# goldfish 1.6.13

* Fix problem when parallelizing code and using preprocess objects.
* Fix incorrect computation of number of right-censored events during 
  preprocessing when `startTime` is specified.
* Add `lifecycle` and `cli` to imports.
* Rename functions names to `snake_case` to comply with the tidyverse
  style guide and `stocnet` interface.
* Add defunct documentation with functions that changed names.

# goldfish 1.6.12

* Fix invalid URL link in the `goldfish-package.Rd`.

# goldfish 1.6.11

* Register S3 methods for internal functions.
* Update R version dependency to 4.1.0 to use the anonymous function syntax.
* Delete configure files; OpenMP support is now managed directly through
  the Makevars files.

# goldfish 1.6.10

* Solve invalid URL link for the MIT Social Evolution dataset.

# goldfish 1.6.9

* Solving missing package anchors in the documentation.
* Update deprecated functions in the vignettes from `manynet`.
* Solves issue on `Makevars.in` file that produced a note when check as CRAN. 

# goldfish 1.6.8

* `checkEvents()` method addresses S3 generic/method consistency.
* Solve invalid URL link in the bibliography.

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

* Solve `startTime` and `endTime` bug on `DyNAM` and `REM` models
  preprocessing.
* Export `GatherPreprocessing()`. Experimental functionality.
* Clean unnecessary functions imports.
* Solve `aes_string()` deprecation.
* Solve issue on `C++` engine on DyNAM-rate.
* Enforcing an 80-character line length on C++ code.
* Comply with code style.
* Change that reduce execution time in the R estimation engine.

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
* Solve some warnings regarding S3 generic/method consistency.
* Solve no visible binding for global variables.
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