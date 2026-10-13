# walk_alignment_v1

Pre-change references for the DyNES augmenters, recorded at `b0c14308`:
after `estimate_dynes()` began snapping the composition changes of actors
taking part in an interval to the interval's waves, and before the
augmenters read stamps strictly before them or shed their unused walks and
rate evaluations.

- `walk_alignment_pre_change.rds`: per fixture (`uncoupled`, `coupled`), the
  panel ties, theta, seeded `augment_seq_sim()` draws (timed friendship rows,
  `log_proposal`, `log_proposal_raw`, `restarted`) for seeds 1–5, and the
  walk-open / `evaluate_fid()` counts, seconds and allocated MB per sim draw
  and per MCMC sweep.
- `generate_walk_alignment_references.R`: the generator, with its run
  command. Regenerating overwrites the floor; do it only when a reference is
  meant to move.

Draws are comparable only on data with no stamp tie, no observed event at a
wave time and no in-interval composition change. Seconds and MB are
machine-specific; the counts and the draws are not.
