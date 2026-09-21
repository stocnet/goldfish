# abmcem — change-local NEWS staging

These bullets stage the user-facing changes for this change. They are folded
into the root `NEWS.md` at branch merge, together with the `DESCRIPTION`
version bump — neither the root `NEWS.md` nor the `DESCRIPTION` version is
touched on this branch (see `progress.md`, "Version / NEWS / archival").

* Group 1 (control constructors) complete: `set_algorithm_em()` and its three
  nested option bundles — `set_augmenter_options()`, `set_weights_options()`,
  `set_sgd_options()` — ship with child-local validation, the cross-object
  validity matrix and precedence rules, and the cold-start construction guard
  (`routine = "random"` with a non-zero `initial_parameters` or
  `warm_start = TRUE` warns).
* Group 2 (weighting machinery and Q/ASE dispatch) complete: importance-weight
  reference records with likelihood-ratio reweighting, weight transformations,
  the three resampling schemes, the ESS redraw guard, and the classed E-step
  object with `compute_q()`/`compute_ase()` scheme dispatch. Rounding out the
  E-step diagnostics: a one-shot cold-start ESS diagnostic for a pathological
  startup pool, a mid-run recommendation to switch to `augment_seq_mcmc()` when
  importance-sampling weights collapse (its numeric floor sourced from the
  `dynes-augmentation` degeneracy study, not guessed), and run-level dedup so
  the `sim` restart-coupling warning fires at most once per estimation run.
* Group 3 (SGD M-step optimizer) complete: `optimize_abem_sgd()` owns its inner
  loop over the concatenated parameter vector — an injected score-only batch
  evaluator plus the normalized E-step weights, weighted (unweighted-mean) or
  cyclic (importance-weighted, weight × K/B) batches ascending the same
  weighted objective, constant/decay/AdaGrad/Adam/momentum step schedules at
  fixed literature defaults, per-M-step accumulator reset, an optional immobile
  parameter mask, and gradient-norm (EMA under minibatch) or fixed-iteration
  stopping with a warn-on-unmet-gradient-at-cap diagnostic.
* Group 4 (prototype-path pool evaluator) complete: `build_sequence_pool()`
  preprocesses each pooled sequence once, on entry, through the existing
  pipeline via an injectable per-sequence map seam; `evaluate_sequence_pool()`
  is the v1 evaluator-contract adapter, harvesting per-sequence
  loglik/score/Fisher from the existing engine at zero optimizer iterations,
  honoring `what` at the harvest level and wrapping the per-sequence engine
  call so a singular information matrix degrades to a non-finite result
  instead of aborting the pool. The pool-entry path stays attachment-optional
  — a caller may attach a precomputed `(stats, loglik@theta_ref)` record via
  `new_pool_attachment()`, ignored in this version so a later reuse
  fast-follow needs no signature change — and `make_proposal_evaluator()`
  ships the theta-closed single-candidate proposal evaluator the ABEM loop
  will inject into an augmenter, returning its `(loglik, rates)` pair as a
  named list so that contract can widen without breaking callers.
