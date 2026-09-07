## Breaking changes

* Renamed every S3 class goldfish attaches to the `goldfish<Thing>` scheme.
  * Stored fits and stored preprocessed objects no longer dispatch: **re-fit**.
  * A script testing `inherits(x, "result.goldfish")` (or any retired name
    below) must be updated; there is no fallback class and no deprecation
    cycle, because this line has never reached CRAN.
  * The seven names autograph plots on are adopted verbatim from autograph, so
    plotting needs an autograph past its own rename (1.0.6 or later).
  * Exported function names are unchanged: `test_gof()` is still `test_gof()`,
    only the class of what it returns moved.
* Renamed the fitted-model and summary classes.
  * `result.goldfish` is now `goldfishFit`.
  * `flavored_result.goldfish` is now `goldfishFlavFit`.
  * `summary()` on a fit returns `goldfishSummFit`, was
    `summary.result.goldfish`; no live class carries a dot any more.
* Renamed the diagnostic classes.
  * `test_gof` is now `goldfishGOF`; `test_time` is now `goldfishTimeTest`.
  * `test_parameter` is now `goldfishParamTest`.
  * `diagnose_onset` is now `goldfishOnset`.
  * `diagnose_outliers` is now `goldfishOutliers`.
  * `diagnose_changepoints` is now `goldfishChangepoints`.
  * `margin_table` is now `goldfishMargins`.
  * `attr(x, "diagnostic")` carries the new class string, not the old one.
* Renamed the preprocessing, specification and algorithm classes.
  * `preprocessed.goldfish` is now `goldfishStat`, named for
    `compute_statistics()`, the function a user calls.
  * `preprocessed_db.goldfish` is now `goldfishStatDB`.
  * `flavored_preprocessed.goldfish` is now `goldfishFlavPrep`.
  * `flavored_statistics.goldfish` is now `goldfishFlavStat`.
  * `preprocessing.goldfish` is now `goldfishPrepCtrl`.
  * `specification.goldfish` is now `goldfishSpec`.
  * `spec_map.goldfish` is now `goldfishSpecMap`.
  * `algorithm.goldfish` is now `goldfishAlgo`.
  * `algorithm_newton.goldfish` is now `goldfishAlgoNewton`.
  * `goldfish.formulae` is now `goldfishFormulae`.
* Split `data.goldfish` into two classes that no longer share a print method.
  * `as_goldfish()` stamps `goldfishData`.
  * The legacy environment `make_data()` and the DyNAM-i path build keeps
    `data.goldfish` as a deprecated-path name.
* Renamed the internal classes too; the rule is not limited to what users see.
  * `writer_default` / `writer_gather` / `writer_db` are now
    `goldfishWriterDefault` / `goldfishWriterGather` / `goldfishWriterDB`,
    under the parent `goldfishWriter`.
  * `data_source_envir` / `data_source_stocnet` are now
    `goldfishSourceEnvir` / `goldfishSourceStocnet`, under `goldfishSource`.
  * The `model_spec` hierarchy is now `goldfishKind` plus nine
    `goldfishKind<Variant>` classes, and the risk-set axis is
    `goldfishAxisSender` / `goldfishAxisDyad`.
  * `support_constraint_plan` is now `goldfishSupportPlan`.
  * `fixed_spec` / `initial_spec` are now `goldfishCoefFixed` /
    `goldfishCoefInit`.
  * `intercept_only_rate` is now `goldfishCteRate`.
  * `walk_handle.goldfish` is now `goldfishWalk`.
  * `joint_preprocessed.goldfish` is now `goldfishJointPrep`.
  * `merged_blocks.goldfish` is now `goldfishBlock`.
  * The DyNAM-i update classes are now `goldfishInterNet` / `goldfishInterGrp`
    / `goldfishInterWindow`.
* Retained `result.goldfish` as a diagnostic stub carrying two methods only.
  * `print()` and `summary()` explain that the object must be re-fitted, and
    say whether its components moved or only its class name.
  * Every other generic gives R's own "no applicable method" error.
* Retained the deprecated-path classes unchanged: `nodes.goldfish`,
  `network.goldfish`, `dependent.goldfish`, `global.goldfish`, and the legacy
  `data.goldfish` environment.
* Retained the effect dispatch tags (`inertia`, `recip`, ...) and the
  `goldfish_<snake_case>` condition classes unchanged.
