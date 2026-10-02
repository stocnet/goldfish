* Improved the speed of the `cpp` and `gather` estimation backends:
  * The compiled likelihood allocates its work buffers once per evaluation.
  * The default `"loglik"` diagnostic no longer costs a second `exp` pass.
  * The `gather` backend fits REM about three times faster.
