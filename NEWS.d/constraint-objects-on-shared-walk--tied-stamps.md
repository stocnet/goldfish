* Fixed `simulate(times = "observed")` letting tied draws restrict each other.
* Fixed preprocessed joint specifications reading events at their own stamp:
  * a tied event of another process;
  * a process's own event, when an earlier process reads its layer.
