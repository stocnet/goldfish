## Internal

* Unified DyNAM and REM batch preprocessing on the merged single-clock walk,
  now the sole path. The separate sender and dyad recipe loops
  (`run_sender_recipe_loop()`, `run_dyad_recipe_loop()`) are removed, so batch
  and replay share one substrate and stay identical event-for-event.
