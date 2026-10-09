* Improved `estimate_dynes()` for actors that join or leave inside a panel
  interval.
  * An actor involved in an interval, as the sender or receiver of a modeled
    event or the `from`/`to` of a flip, is now present over the whole
    interval: a join takes effect at the interval's start, a leave at the
    next wave.
  * A `goldfish_composition_snapped_warning` names each moved actor, the
    interval, and the real change times now ignored (in the data's time
    class), since the rewrite changes the model.
  * This is a v1 approximation; it removes the stranded and zero-mass draws
    of the sim augmenter. Data without such a change fits identically.
* Improved `compute_lik_seq()`: scoring a fit's specification against
  the original data's sequence, or the reverse, now aborts with
  `goldfish_candidate_mismatch` when the two compositions differ, instead of
  silently scoring the candidate's composition.
