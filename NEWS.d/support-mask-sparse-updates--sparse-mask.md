* Improved `support_constraint` preprocessing, which no longer stores one mask
  per event.
  * A constrained model's preprocessed object is now the size of an
    unconstrained one; on 1899 actors the mask was 97% of it.
  * Constrained preprocessing runs about as fast as unconstrained, where it was
    two orders of magnitude slower.
  * A `choice_coordination` constraint fits a realistic sequence, which it
    previously could not.
