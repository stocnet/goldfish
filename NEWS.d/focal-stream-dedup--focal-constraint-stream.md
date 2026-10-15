* Fixed a support constraint that reads its own layer being applied twice:
  * with weights and no formula term on that layer, each event was counted
    twice, so `estimate_dynam()` could abort with an empty risk set;
  * the DyNES augmenters agree with the likelihood on such a constraint.
