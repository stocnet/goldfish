* Fixed `estimate_dynes()` standard errors to use Louis's observed information.
  * They used the missing information as the middle of a sandwich.
  * So they were too small by orders of magnitude when little is missing.
  * The Monte-Carlo error now uses the same observed-information bread.
  * A non-positive-definite observed information gives `NA` and warns.
* Improved `estimate_dynes()` fits to report their covariance diagnostics.
  * `glance()` gains `covariance_status`, `fraction_missing`, `covariance_ess`.
  * `summary()` reports them and explains a blank standard-error column.
  * The fit stores `information`: complete, missing and observed matrices.
  * `vcov(information = "complete_data")` returns the complete-data inverse.
