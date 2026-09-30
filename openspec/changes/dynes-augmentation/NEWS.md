# dynes-augmentation — change-local NEWS staging

These bullets stage the user-facing changes for this change. They are folded
into the root `NEWS.md` at branch merge, together with the `DESCRIPTION`
version bump — neither the root `NEWS.md` nor the `DESCRIPTION` version is
touched on this branch (see `progress.md`, "Version / NEWS / archival").

## Panel seam (Group 2)

* A panel layer modeled as a dependent process is now the augmentation target
  of `estimate_dynes()`, with no separate flag; the event-stream estimators
  refuse it and point to `estimate_dynes()`, and a panel layer read only as a
  covariate stays a static step-covariate.
* Two panel kinds are diffed between waves: a flavored **state** panel into one
  flip per changed dyad, named by its flavor, and an unflavored **count**
  panel, whose cumulative counts rise by `Δ`, into `Δ` indistinguishable
  events on that dyad. Incomplete (`NA`) snapshots, states outside the flavor
  mapping and decreasing counts abort before diffing.
* A change-list row stamped exactly at a wave belongs to that wave's snapshot,
  for the wave diff and for a completed rate's pinned periods alike, so a
  pinned period counts the events of the interval it pins (on both panel
  kinds). This fixes a first period pinned to a zero hazard.
* `estimate_dynes()` infers the wave grid from the modeled panel layers' row
  times, opened by `set_preprocessing(start_time =)` when given and otherwise
  by the first wave, and takes an optional `wave_times` override. It compiles
  the wave diffs, per-dyad chains and per-wave states once at entry and shares
  them with every augmented draw.

## Sequence augmenters (Group 3)

* `estimate_dynes()` now draws the latent panel events between waves with one
  of three augmenters, picked by `set_augmenter_options(routine =)`:
  `"random"` places each interval's events at uniform times,
  `"sim"` draws them from the model at the current parameters, and `"mcmc"`
  runs one Metropolis-Hastings chain of permute and shift moves over the
  whole sequence. Every draw reaches each observed wave state exactly and
  records the log density it was proposed with.
* Under a support constraint that couples dyads, `"random"` redraws an
  ordering until it is support-legal, `"sim"` restarts an interval it cannot
  complete and warns once per run about the plug-in normalizer this needs,
  and `"mcmc"` rejects an illegal move. When no legal draw is found within
  the restart ceiling, estimation aborts naming the constraint.
* Each draw depends only on the random number stream it is given, so a pool
  drawn from one `seed` is the same at any `n_cores`. Drawn sequences carry
  the flavor keys `add_flavor()` stamps, so each event is scored under its
  own flavor.
* The documentation of `set_augmenter_options()` now states that
  `routine = "random"` is a sound cold start only near a zero starting
  parameter vector, and that `"sim"` or `"mcmc"` should be preferred away
  from it.

## Pool evaluator (Group 4)

* `estimate_dynes()` now scores its pool of augmented sequences on the
  compiled likelihood engine, one pass per sequence and sub-model, instead of
  a zero-iteration Newton fit per sequence: about an order of magnitude
  faster, with the same log-likelihood, score and Fisher information. A
  pooled sequence keeps its statistics and no longer its data object, and the
  start state of the window is held once for the whole pool, so a sequence
  costs memory in proportion to its events, not to the number of actors.
* New experimental `compute_lik_seq()` returns the joint log-likelihood of one
  complete sequence under a joint specification, with its score and Fisher
  information over the free coefficients: the per-sequence quantities
  `estimate_dynes()` weights, computed through the same evaluation.
