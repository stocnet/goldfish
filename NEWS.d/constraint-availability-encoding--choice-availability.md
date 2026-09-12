* Added a `support_constraint` warning when a DyNAM choice model gates a sender
  out of every event, mirroring the rate family's "never at risk". A sender that
  never appears is a legitimate model, so it warns rather than errors.
* Fixed the choice `support_constraint` validation reading the receiver
  composition frozen at its starting value, which under-reported the
  never-a-candidate warning and could report a late-arriving observed receiver
  as an excluded dyad.

## Internal

* An ego-kind `support_constraint` on a DyNAM choice model now folds to two
  factor vectors instead of a dense sender-by-receiver grid, so a constrained
  preprocessed object is no larger than an unconstrained one.
