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
* Group 5 (ABEM loop, estimation surface, and results) complete: the ABEM
  loop (accept/grow/stop against Q and its standard error, harmonic growth,
  a `max_pool`-clamped post-acceptance pool size, retry exhaustion aborting
  with the accumulated `em_trace` attached, `max_iterations` returning the last
  accepted estimate flagged `converged = FALSE`) and the experimental,
  not-yet-exported `estimate_dynes()` surface (completion once at entry,
  decomposition of the joint specification into pool sub-models, warm start,
  L'Ecuyer-CMRG substreams keyed by iteration and draw, and a mirai map seam
  behind the serial default). `estimate_dynes()` returns a new
  `goldfishDynesFit`. Its `vcov()` is the sandwich covariance from one
  full-pool evaluation at the estimate, populated whether or not the run
  converged, and the Monte-Carlo error is carried beside it. `coef()` and
  `vcov()` are flat over the free coefficients, and `coef_layout()` lists
  completed and `offset()` processes as fixed rows. `print()`, `summary()`,
  `tidy()` and `glance()` show both errors and the convergence status, and
  `simulate()` runs the joint specification the fit holds. `logLik()` refuses,
  and `AIC()` and `BIC()` with it, because the estimator ascends a Monte-Carlo
  estimate of the likelihood.
* Group 6 (review follow-ups) complete. `set_algorithm_em()` now defaults to
  a serial `n_cores = 1L`, and any value above 1 starts that many mirai
  daemons, which from a development tree load the same goldfish the session
  runs. The pool is scored on the joint single-clock likelihood: one merged
  walk per sequence, where every other process's events are right-censoring
  boundaries for the timed rates. Flavored joint specifications are accepted.
  On daemons, pool data stays resident: each sequence's data and statistics
  are sent once, when it enters the pool, and each evaluation task carries only
  sequence ids and parameters. A pool generation counter rejects a task from a
  superseded pool. Citations now use the Levine & Casella (2001) author order.
