* Improved the speed of the `cpp` and `gather` estimation backends:
  * The compiled likelihood allocates its work buffers once per evaluation.
  * The default `"loglik"` diagnostic no longer costs a second `exp` pass.
  * The `gather` backend fits REM about three times faster.
* Improved the `r` estimation backend, which now writes its state in place:
  * REM fits run about three times faster with a third of the memory.
  * DyNAM choice fits run about five times faster.
* Fixed the diagnostics of a fit whose last Newton step was rejected:
  * They now describe the parameters the fit returns, not the rejected step.
  * Diagnostics are computed once, after the Newton loop.
* Removed unused arguments of the internal estimation engines:
  * `impute` from the compiled kernels, never set by any caller.
  * `parallelize`, `cpus` and `get_data_matrix`, read nowhere.
