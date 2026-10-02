* Improved the speed of the `cpp` and `gather` estimation backends:
  * The compiled likelihood allocates its work buffers once per evaluation.
  * The default `"loglik"` diagnostic no longer costs a second `exp` pass.
  * The `gather` backend fits REM about three times faster.
* Fixed the diagnostics of a fit whose last Newton step was rejected:
  they described the rejected trial, not the parameters the fit returns.
  Diagnostics are now computed once, after the Newton loop.
