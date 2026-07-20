# goldfish 1.9.2

## New features

* **Competing-process flavors (`add_flavor()`).** A relational-state layer --
  friendship, treaties -- typically evolves through competing sub-processes:
  ties are created and later dissolved. `add_flavor()` records that reading on a
  `stocnet` object, stamping the reserved `ties$flavor` column from each event's
  update value and recording the value->flavor mapping (`values_equivalence`)
  and the `flavor_style` (`mutually_exclusive` / `redundant`) in the layer info.
  It is thin -- it derives no support constraints. Only dichotomous states are
  supported (increment `+1`/`-1`, replace `1`/`0`). *Experimental.*

* **Multi-flavor specifications.** `make_specification()` accepts a flavor-keyed
  `rate` / `choice` list with more than one key, e.g.
  `list(creation ~ ..., dissolution ~ ...)`, building each flavor as a parallel
  competing process on the same focal layer. On a `mutually_exclusive` layer
  each flavor derives its own support constraint -- creation is supportable only
  where no tie exists (`~ !tie(layer)`), dissolution only where one does
  (`~ tie(layer)`) -- AND-composed with any user `support_constraint`. An
  unflavored layer under a flavor-keyed list infers the mapping from its update
  semantics and says so. When both `rate` and `choice` are keyed they must key
  the same flavor set. The specification print nests one section per flavor.
  Per-flavor preprocessing and estimation land in a following release; for now
  estimate one flavor at a time. *Experimental.*

# goldfish 1.9.1

## New features

* **Prebuilt `stocnet` datasets.** `social_evolution` and `fisheries_treaties`
  ship as ready-to-model `stocnet` objects (plain lists of tibbles), so examples
  and vignettes load data instead of constructing it. `fisheries_treaties` carries
  the treaty create/dissolve distinction on a reserved `flavor` column. The raw
  `Social_Evolution` `calls$time` / `friendship$time` are now human-readable
  POSIXct (GMT); the change is coefficient-neutral.

* **Bare attribute references in formulas.** Nodal attributes are referenced by
  bare name -- `ego(floor)`, `same(gradeType)` -- resolved against the object's
  `nodes`, and global attributes by their `global` name. The legacy `df$var`
  prefix keeps working for one cycle with a deprecation warning pointing at the
  bare syntax.

* **Node identity on exports.** Preprocessed / estimation results and the
  gather / db exports carry a `node_lookup` table (`side`, `local`, `global`,
  `label`) so `index_i` / `index_j` resolve back to the original nodes without
  re-deriving the mode map.

* **`?goldfish_data`** documents the single-object construction workflow
  (`manynet::as_stocnet()` / `make_stocnet()` / `from_ties()` / `add_info()`),
  the event / panel observation types and panel dissolution rows, `time = NA`
  history, node mode sets, and the reserved `flavor` / `order` columns. The
  teaching vignettes are rewritten onto this workflow.

## Bug fixes

* A specification estimated from `make_specification()` now prints a clean
  `estimate_dynam()` / `estimate_rem()` call in its `summary()`, rather than an
  internal `tryCatch` frame.

# goldfish 1.9.0

## New features

* **Single data object.** `estimate_dynam()`, `estimate_rem()`, and
  `make_specification()` now accept a **`stocnet`** object (from
  `manynet::make_stocnet()` / `as_stocnet()`) as `data`, replacing the
  environment-of-objects representation for DyNAM and REM. `as_goldfish()`
  validates and stamps such an object early. Layer sides are declared per layer
  via `info$sender` / `info$receiver` mode sets (two-mode support without a second
  node set); a reserved `ties$flavor` column plus a flavor-keyed
  `rate = list(creation ~ ...)` syntax models a subset of a focal layer's events.
  State-at-time helpers (`network_state_at()`, `nodes_state_at()`) evaluate a
  layer or the nodes at a time point.

## Deprecations

* The legacy data constructors -- `make_nodes()`, `make_network()`,
  `make_dependent_events()`, `make_global_attributes()`, `link_events()`,
  `make_data()` (and the `make_data_goldfish()` alias) -- are **soft-deprecated**
  in favor of building a `stocnet` object with manynet. They keep working for one
  cycle: `make_data()` now returns the assembled `stocnet` (never a legacy
  environment), and each constructor points at its stocnet replacement. A saved
  pre-1.9.0 `data.goldfish` environment must be rebuilt as a `stocnet`.

# goldfish 1.8.8

## New features

* Estimation gains an **experimental `optimizer` argument**
  (`set_estimation_opt(optimizer = c("newton_raphson", "bfgs", "bhhh",
  "nelder_mead"))`). The default `"newton_raphson"` is the existing damped
  Newton–Raphson loop; `"bfgs"`, `"bhhh"`, and `"nelder_mead"` are backed by the
  [maxLik](https://CRAN.R-project.org/package=maxLik) package (added to
  `Suggests`, **not** `Imports`) and run on the compiled `default_c` engine.
  `"bhhh"` uses the new per-event score matrix as its gradient. The result is
  mapped into the standard goldfish result object, so `summary()`, `vcov()`, and
  `logLik()` behave unchanged.
* `set_estimation_opt(return_event_scores = TRUE)` returns a new `event_scores`
  result component: the per-event score contributions (`n_events` ×
  `n_parameters`, columns named by effect) whose column sums equal the aggregate
  score at convergence. Useful for sandwich / clustered standard errors,
  score-process diagnostics, and event-influence measures. Available on the
  `default` (R) and `default_c` (C++) engines.
* `gather_model_data()` and the `write_gather_to_db()` long table now carry
  `index_i` / `index_j` columns identifying the candidate dyad of each row, so
  rows in a filtered (constrained) risk set remain decodable to their node
  labels. One-mode `DyNAM` `choice_coordination` export no longer emits the
  reflexive (diagonal) self-dyad rows.

## Improvements

* **Numerically stable multinomial likelihoods.** The `choice`,
  `choice_coordination`, ordinal `rate`, and ordinal `REM` contributions now use
  an in-house single-pass stable softmax on both the `default` (R) and
  `default_c` (C++) engines, so the log-likelihood, score, and information matrix
  stay finite under extreme linear predictors that previously produced `-Inf` or
  `NaN`. The timed hazard path (`rate` and timed `REM`) is deliberately unchanged.
* **Faster default-engine estimation.** The per-event contribution helpers were
  rewritten to BLAS-level operations — information matrices as weighted
  cross-products (`crossprod(D, D * w)`), no-copy `dim<-` reshapes, and
  single matrix-product linear predictors — and the compiled `default_c` REM and
  coordination kernels to a staged BLAS (GEMV/GEMM) form. On the bundled
  fixtures, timed `REM` estimation on the `default` engine is roughly 30× faster
  and the compiled coordination kernel about 2× faster. No new package
  dependencies were added.
* Constrained `DyNAM` `choice_coordination` now runs **natively on the
  `gather_compute` engine**: the redirect to `default_c` (and its informational
  message) introduced in 1.8.7 is removed. The coordination gather path is now
  index-based and ragged-safe on every engine.

# goldfish 1.8.7

## New features

* `support_constraint` now runs **natively on every engine** for every wired
  family — the compiled `default_c` and `gather_compute` engines no longer
  downgrade to the default (R) engine when a constraint is supplied:
  * `DyNAM` `choice`, `rate`, and `REM` (standard and ordinal) constrained
    estimation is native on `default` / `gather_compute` / `default_c`, matching
    the default engine to numerical precision.
  * `DyNAM` `choice_coordination` gains `support_constraint` support: the
    constraint is symmetrised (`support[i, j] & support[j, i]`) so both
    directions of the mutual likelihood are masked consistently, and it runs
    natively on `default` / `default_c` (a constrained `gather_compute` request
    is redirected to `default_c`, which is identical, with an informational
    message).
* A dyadic (`tie(net)`-kind) `support_constraint` on a `DyNAM` `rate` model is
  now **accepted** (previously rejected): a sender is at risk when it has at
  least one allowed, present receiver (`rowSums(support & available) > 0`). A
  one-time informational message explains the reduction and the cheaper
  `ego()`-kind reformulation (equivalent only under static receiver composition).
* `ego(attribute)` is now dispatchable in `DyNAM` `choice` / `choice_coordination`
  (previously `Unknown effect ego`): it is usable as an interaction operand
  (`~ ... + ego(x):inertia`) and as a `support_constraint` atom. Identification is
  unchanged — a bare `ego()` main effect is still rejected in choice as
  unidentified in the softmax.

## Internal

* The support mask and per-event availability are now maintained as
  encoding-aware flat statistics: one availability object per preprocessing loop
  — `active_sender` (sender loop) or `active_dyad` (dyad loop) — stored at its
  minimal encoding (scalar / ego / alter / outer / point; dense only at the point
  encoding). This replaces the per-event list of dense mask matrices and the
  scattered `presence*` / `active_mode*` objects, and lets the mask cross the
  R↔C++ boundary through the same channel as the model statistics. The deprecated
  `opportunities_list` is folded into `active_dyad` during preprocessing instead
  of being recomputed per estimation iteration.

# goldfish 1.8.6

## New features

* `support_constraint`: a first-class, per-event risk-set restriction for DyNAM
  models, supplied as a one-sided formula on `estimate_dynam()` (and carried by
  `make_specification()`). It uses a restricted boolean-tree grammar — effect
  atoms (`tie(net)`, `indeg(net)`, ...) combined with `&`, `|`, `!`, comparisons
  (`> < >= <= == !=`), and elementwise arithmetic (`+ - * /`); a bare effect
  means `effect != 0`. Inside a constraint `*` is elementwise arithmetic, never
  the effects formula's interaction expansion.
  * `DyNAM` `choice` / `choice_coordination`: the constraint restricts each
    event's receiver set, shrinking the candidate set and reindexing the chosen
    alternative. A constraint over an allowed-dyad network reproduces the
    coefficients of the (deprecated) opportunity list.
  * `DyNAM` `rate`: the constraint reduces to a per-event sender gate (a sender
    is at risk only with at least one allowed, present receiver) and drives the
    constrained intercept denominator.
  * `REM` (with a time intercept): the constraint removes disallowed dyads from
    the 2D risk set (their event rate is zeroed), so an all-allowing constraint
    reproduces the unconstrained fit.
  * Mis-specified constraints fail fast in preprocessing (an observed dyad
    excluded, or an empty risk set, errors; a forced choice or never-active node
    warns).
  * This release wires the default (R) engine; ordinal REM and the compiled
    engines (`default_c` / `gather_compute`) follow.

## Deprecations

* `set_preprocessing_opt(opportunities_list = )` is soft-deprecated in favour of
  the `support_constraint` argument of `estimate_dynam()` / `make_specification()`,
  which generalizes the per-event choice-set restriction to a per-`(sender,
  receiver)` risk-set constraint that works on every engine. It still works (with
  a one-time warning); an equivalent `support_constraint` over an allowed-dyad
  network reproduces the opportunity-list coefficients.

# goldfish 1.8.5

## New features

* Interaction terms (`:`/`*`) are now supported in DyNAM `rate` / `rate_ordered`
  models, extending the dyad-model support added in 1.8.4. A rate interaction's
  statistic is the per-sender elementwise product of its operands. Interaction
  operands must vary on the sender axis (`ego`, `global`, degree `type = "ego"`);
  an `alter`-perspective operand is rejected because a rate model has no receiver
  axis. `global()` is permitted as an interaction operand even in `rate_ordered`,
  where it is not identified as a bare main effect, because the interaction
  restores per-sender variation.

## Bug fixes

* An interaction whose operand is a non-identified effect (e.g. `global(x):outdeg`
  in a choice model) no longer aborts: interaction operands are validated by a
  role-aware rule rather than as bare main effects.

# goldfish 1.8.4

This release completes Stage 2 of the formula-parsing refactor, adding several
formula features on top of the Stage 1 compile reorganisation. Existing formulas
and their fitted coefficients are unchanged.

## New features

* Interaction terms in model formulas. `a:b` adds the interaction only and `a*b`
  expands to `a + b + a:b`, following R's formula conventions. An interaction's
  statistic is the elementwise product of its operands' statistics, computed
  incrementally during preprocessing, and its name composes from the operands
  (e.g. `inrt:rec` in `coef()`). Operands of a bare `a:b` are kept in the design
  but not estimated. Available for DyNAM `choice` / `choice_coordination` and REM.
* `make_specification()` (experimental) bundles the rate and/or choice formulas
  of a model with its `model`, sub-model(s), the dependent process (named by
  `layer`, with an empty formula left-hand side), and `data` into a reusable
  `specification.goldfish` object with a `cli`-rendered overview. It can be passed
  directly to `estimate_dynam()` / `estimate_rem()` in place of a formula.
* `offset()` fixed-coefficient terms. Wrapping a term in `offset()` holds its
  coefficient fixed (rather than estimating it), with the value(s) supplied via a
  new `offset_coef` argument to `set_estimation_opt()`. The statistic column is
  kept and contributes to the linear predictor.
* Native `type = "ego"` in DyNAM `choice` / `choice_coordination` degree-family
  effects (`indeg`, `outdeg`, `node_trans`, `tertius`), numerically identical to
  the equivalent REM expansion, and `global()` is now computable in choice
  (produced by `compute_stats()` as a design column). Both are usable as
  interaction operands; they remain unidentified as bare main effects in choice.

## Deprecations

* `set_estimation_opt(fixed_parameters = )` is superseded by wrapping the term in
  `offset()` and supplying `offset_coef`. The positional vector still works with a
  soft-deprecation message.

# goldfish 1.8.3

This release completes Stage 1 of the formula-parsing refactor: an internal,
behaviour-preserving reorganisation of how a model formula is compiled into the
preprocessing recipe. There are no user-visible changes — fitted coefficients,
standard errors, and printed output are identical to 1.8.2.

## Internal changes

* Model formulas are now compiled once, up front, into a specification map
  (`build_spec_map()`) that separates *metadata* (the effects, links, and a
  registry of derived inputs) from *data*. The shared formula parser no longer
  mutates the caller's data environment.
* Windowed networks and their dissolve-event streams are realised, and event
  streams fetched, inside preprocessing state creation (driven by the derived-
  input registry) rather than eagerly during parsing. A derived object's
  inherited metadata (node sets, event streams, direction) is resolved from its
  source object.
* The update plan carries new registries reserved for interaction terms and
  multivariate models (`role`/`estimate`/`fid`/`lid`, `interactions`,
  `operand_of`, `stat_state_spec`, `formula_effects`); these are populated
  trivially for now and activated in Stage 2.

# goldfish 1.8.2

This patch release introduces a shared compact term-string renderer so an
effect reads identically across the printed summary, `tidy()`, and
`gather_model_data()` export names.

## User-visible changes

* `print()` on a `summary()` of a fitted model now prints a single
  coefficients table by default (`compact = TRUE`), with compact term row
  labels of the form `effect/obj·obj2 [args]` and a short legend keying any
  opaque argument codes present (for example `W = weighted`, `Fx = fixed`). Pass
  `compact = FALSE` to restore the previous two-table view with the full
  "Effects details" table.
* `coef()` and `vcov()` now name parameters with a minimal-unique short form
  (curated short effect name plus the smallest disambiguating suffix), so names
  are always unique — fixing the previous hazard where duplicate bare names
  broke name-based subsetting. `vcov()` dimnames equal `coef()` names. This
  changes the names returned by `coef()`/`vcov()`.
* `tidy(compact = TRUE)`'s `term` column is now produced by the shared builder
  in export mode (valid, unique R names) instead of an ad-hoc paste.
* `gather_model_data()` gains a `max_length` argument (default 63, a
  database-safe value) bounding the length of the produced effect/column names,
  which are valid and unique within a model.

## Internal changes

* Width-independent decoder renderings (`.effect_short`, `.object_short`,
  `.term_export`, `.coef_name`) are computed once at construction and stored as
  dot-prefixed columns on the effect-description matrix carried by
  `result$names` and `gather_model_data()`'s `effectDescription`; display
  methods read these (read-if-present-else-compute) and skip dot-prefixed
  columns when iterating, so the metadata never leaks into rendered output.

# goldfish 1.8.1

This patch release compacts how constant-value fan-out effects are stored
during preprocessing. Coefficient estimates and log-likelihoods reproduce to
within 1e-6 of the previous implementation on both estimation engines.

## Internal changes

* Constant-value fan-out effect updates — `alter()`, `ego()`, the degree
  effects (`indeg` / `outdeg` / `degree`) in their alter/ego projection, and
  `global()` — are now stored as a single coded entry in a new
  `stat_mat_broadcast` buffer (with `stat_mat_broadcast_pointer`) instead of
  one duplicate column per affected cell in `stat_mat_update`. This removes the
  dominant preprocessing memory cost for dyad models that use these effects
  (for example, `stat_mat_update` shrinks by more than an order of magnitude on
  REM models dominated by `alter()` / `ego()`), and avoids the integer overflow
  that large fan-out models could previously hit. Both estimation engines (the
  R `default` and the C++ `default_c` / `gather_compute`) decode the broadcast
  buffer per event, honouring the reflexive-diagonal and two-mode rules; the
  gather and database writers expand it when materialising the gather stack.
* The `preprocessed.goldfish` format version is bumped. Objects preprocessed
  with an earlier goldfish version are rejected by the `preprocessing_init`
  version check with a message to recompute them; the public R API is
  unchanged.

# goldfish 1.8.0

This release refactors the preprocessing and estimation pipeline around typed
model specifications and a writer strategy, and adds a public preprocessing
entry point. Coefficient estimates reproduce to within 1e-6 of the previous
implementation on both estimation engines.

## New features and user-facing changes

* New exported function `compute_stats()` runs the preprocessing stage of a
  model and returns the change statistics as a `"preprocessed.goldfish"`
  object, without estimating the model. The result can be passed to the
  estimation functions through their `preprocessing_init` argument. Its
  `output` argument selects the statistics writer: `"default"` (the
  estimation-ready `preprocessed.goldfish` object), `"gather"` (the gather
  stack, one row per event × alternative), and `"db"` (gather rows streamed
  to a database table).
* `estimate_dynam()` gains `sub_model = "rate_ordered"` to declare the
  ordinal activity rate model (only the order of the events is modeled,
  partial likelihood as in the CoxPH model) explicitly. The previous
  specification of this model, `sub_model = "rate"` with a formula without
  the time intercept, is deprecated and now emits a warning suggesting
  `"rate_ordered"`.
* `estimate_rem()` gains a `sub_model` argument with valid values `"rate"`
  (full dyadic hazard model, the default) and `"rate_ordered"` (ordinal
  case). The internal `"choice"` label used so far for REM models is kept
  as a deprecated alias of `"rate"` and emits a warning.
* The `global()` effect now errors for `sub_model = "choice"` and
  `"choice_coordination"`: a global covariate is constant across
  alternatives, so its main effect is not identified in a multinomial
  choice model. Support through interaction effects is planned for a
  future release; rate sub-models keep accepting `global()`.
* Effects with `ignore_repetitions = TRUE` now error immediately: the
  previous implementation computed incorrect statistics (it always masked
  repetitions using the dependent network instead of the network the
  effect is applied to). The feature is disabled pending a correct
  reimplementation (#105).
* `gather_model_data()` is reimplemented as a thin wrapper over
  `compute_stats(..., output = "gather")` and now also handles one-mode
  rate models, which previously errored.
* `compute_stats(..., output = "db")` streams the gather rows to the
  database table configured via `set_preprocessing_opt(db = , db_table = )`
  in event-aligned batches, returning a lightweight descriptor instead of
  the in-memory stack. This makes the previously reserved `db` / `db_table`
  options functional. `RSQLite` is added to Suggests for the round-trip
  tests.
* `preprocessed.goldfish` objects now carry a format `version`; passing an
  object preprocessed with a previous goldfish version through
  `preprocessing_init` errors with a message to recompute it with
  `compute_stats()`.
* The model summary now reports the two convergence criteria below the return
  code: the score (`score_rel_norm`, the likelihood-scaled relative gradient
  norm checked against `score_tol`) and the step (`maxAbsUpdate`, the maximum
  absolute parameter update checked against `step_tol`). `score_rel_norm` is
  also added to the `convergence` list of the fitted object.
* Fixed an integer overflow when preprocessing very large models: the growing
  statistics buffer used 32-bit integer arithmetic, so models with more than
  ~2^31 statistic updates failed with `invalid 'ncol' value (too large or NA)`.
  Buffer sizing now uses double-precision arithmetic, and a model that would
  exceed R's hard matrix column limit (2^31 - 1) now stops with an informative
  error suggesting `compute_stats(output = "db")` or fewer effects instead of a
  cryptic overflow.

## Internal changes

* Model variants are carried through the pipeline as typed `model_spec` S3
  objects constructed once from `(model, sub_model, is_two_mode)`;
  preprocessed and fitted objects store the spec in a `model_spec` field,
  and preprocessing and estimation dispatch on the spec class.
* Preprocessing runs through dedicated recipe methods for every model
  variant. The DyNAM rate and rate-ordered variants share a sender-indexed
  event-loop kernel; DyNAM choice, choice-coordination, REM, and
  REM-ordered share a dyad-indexed kernel. DyNAMi variants participate in
  the dispatch but delegate to the existing DyNAMi loop unchanged.
* The preprocessing result stores statistics updates in a single flat
  matrix (`stat_mat_update` with `stat_mat_pointer` and `is_dependent`)
  covering dependent and right-censored events; the nested `stats_change`
  list is no longer produced. Rate models additionally store the intercept
  scalars (`n_dep_events`, `total_time`, `avg_active_actors`) and the
  presence composition changes in the format the estimation engines consume.
* Recipes emit their output exclusively through a writer strategy
  (`init()` / `write_event()` / `finalize()`), so a single event loop
  serves every output format (`compute_stats(output = )` selects the
  writer). The gather stack is produced in R from the flat buffer; the
  post-hoc C++ `gather_()` routines (`gather_sender_model()`,
  `gather_receiver_model()`, `gather_sender_receiver_model()`) have been
  removed and `engine = "gather_compute"` consumes the native output.
* Estimation is self-contained and dispatches on the model specification
  class. `estimate_int()`, the per-event contribution
  (`compute_event_contribution()`, formerly `getEventValues()`), and the
  per-event update (`compute_step()`) are S3 generics resolved once at the
  start of estimation; no model-type string comparison or S3 dispatch runs
  inside the event loop. The Newton-Raphson outer loop is shared through
  `run_nr_loop()` with the `score_tol` / `step_tol` stopping criteria
  unchanged.
* The R and C++ estimation engines consume the combined flat update buffer
  directly. The estimation routines no longer reach back into the
  preprocessing environment (`prepEnvir` / `get()`), re-derive actor
  counts, or apply the dead per-event mean imputation; statistics are
  asserted NA-free at estimation entry. The C++ routines take the combined
  buffer with `is_dependent` instead of separate right-censored matrices.
* The `ignore_repetitions` masking, `modifyStatisticsList()` /
  `reduceStatisticsList()` hot-path calls, and the dual-read shim for the
  old `stats_change` format are removed; a thin `prepare_statslist()`
  handles the intercept prepend and `excludeParameters` dropping.

# goldfish 1.7.3

* `make_global_attributes()` (renamed from `make_global_attribute()`) now
  accepts a one-row snapshot data frame with named numeric columns.
  The plural name reflects that one object holds multiple global attribute
  columns.
* `global(df$col)` effect is now functional for DyNAM-rate and REM models.
  The effect broadcasts a time-varying scalar to all actors at initialization
  and emits per-actor change statistics whenever the global attribute is
  updated by a linked event.
* `link_events()` gains a method for `global.goldfish` objects.
  Only replace-semantics events (no `increment` column) are supported;
  the `replace` argument names the value column in the events data frame.

* `window` is now correctly supported for the four `mixed_*` effects
  (`mixed_trans`, `mixed_cycle`, `mixed_common_sender`, `mixed_common_receiver`)
  when the network argument is a `list(net1, net2)` expression. Previously
  the formula parser errored with a cryptic "object not found" message.
* Using `window` with an attribute-only effect (e.g., `alter(nodes$attr, window = 5)`,
  `same`, `sim`, `diff`, `ego`, `ego_alter_interaction`) now raises a descriptive
  error identifying the effect and attribute, and lists all violations in the formula
  at once. Previously this silently produced results with undefined semantics.
* Two-path cache counts in closure effects (`trans`, `cycle`, `common_sender`,
  `common_receiver`, and all `mixed_*` variants) are now clamped to zero via
  `pmax(0L, ...)` instead of filtering out negative rows. Previously, when all
  incremental counts were negative the cache was silently left unchanged;
  counts are now always non-negative and all affected entries are updated.
* All four `mixed_*` effects (`mixed_trans`, `mixed_cycle`,
  `mixed_common_sender`, `mixed_common_receiver`) gain a `history` parameter
  accepting `"pooled"` (default, existing behavior) or `"sequential"`. With
  `"sequential"`, the argument order of the two networks defines the temporal
  sequence: additions to the second network count existing first-network
  neighbors, while additions to the first network produce no new two-path
  entries. Removals always update the cache regardless of `history`.
* Improved convergence diagnostics in the Newton-Raphson optimizer:
  two stopping criteria are now checked independently and the first
  criterion met is reported as a return code in the model summary.
  `set_estimation_opt()` gains `score_tol` (default `1e-6`, scale-invariant
  relative gradient criterion) and `step_tol` (default `1e-8`, damped Newton
  step size criterion). The summary line now reads
  "Return code 1: gradient close to zero" or
  "Return code 2: step size close to zero (damped)" and is followed by
  the number of free and fixed parameters estimated.
* `set_estimation_opt(convergence_criterion)` is deprecated.
  Use `score_tol` instead (default `1e-6`).
* Fix premature convergence in the Newton-Raphson optimizer: when a damped
  step overshot and the trial log-likelihood was non-finite (`-Inf`), the
  likelihood-scaled score criterion evaluated to zero and the algorithm
  stopped reporting "gradient close to zero" at unconverged estimates.
  The stopping criteria, extracted into an internal `check_convergence()`
  helper shared by both estimation engines, are now evaluated on the
  accepted iterate, and the score criterion only on iterations whose step
  improved the log-likelihood. The step size criterion remains active on
  rejected steps as the stalled exit.

# goldfish 1.7.2

* Refactored DyNAM-rate preprocessing pipeline: statistics are now stored as
  an `n1 × nEffects` matrix instead of a 3D array, eliminating the
  `reduceMatrixToVector` step at estimation time.
* Unified preprocessing output fields: `stats_change`, `intervals`,
  `is_dependent`, `event_time`, `event_sender`, `event_receiver` (snake_case);
  composition change vectors (`active_mode1/2_init`, `active_mode1/2_changes`)
  are now stored in the preprocessing object instead of being re-derived during
  estimation.
* DyNAM-rate effect functions are now the base implementations returning
  2-column `(node1, replace)` changes; REM and choice effects delegate to them
  via `to_ego()` / `to_alter()` helpers (new `R/utils_effects.R`).
* New DyNAM-rate effects: `degree()` (undirected), `triangle()` (undirected),
  and `global()` for time-varying global covariates.
* Added `db` and `db_table` parameters to `set_preprocessing_opt()` (reserved
  options for streaming statistics to a DBI-compatible database; the streaming
  writer that consumes them was implemented in a later release).
* Added `DBI` to package imports.

# goldfish 1.7.1

* Created new data objects for diagnostic methods (#116). 
* Plotting for diagnostic methods migrated to autograph (#115).
* Added mode imputation for non-numeric attributes (#112).
* Fix bug when preprocessed data is used in `estimate_` functions due to
  an incorrect comparison of formulas (#111).
* Provided more informative error messages when missing data is present in
  data objects (#82).
* Provided more informative error messages when elements that are not available
  are used in estimate formula (#81).
* Allow for `max_iterations` in `set_estimation_opt()` to be set to `0` to
  retain model statistics without running the estimation algorithm.
* Remove unused variable in `compute_poisson_selection()`.

# goldfish 1.7.0

* Rename functions to follow tidyverse style guide.
* Rename `define` functions to `make_`: `make_nodes()`, `make_network()`,
  `make_dependent_events()`.
* Introduce `set_` functions to set options: `set_estimation_opt()` 
  for algorithm options, and `set_preprocess_opt()` for preprocessing options.
* Introduce `make_data()` function to create a single data object with all
  the data needed for estimation (#100).
* Introduce `history = c("pooling", "sequential", "consecutive")` argument
  to `trans()` and `cycle()` effect functions that define how the
  previous history of events is used to compute the effect (#105).
* Add a function for each model implemented in the package: `estimate_dynam()`,
  `estimate_dynami()` and `estimate_rem()`.

# goldfish 1.6.13

* Fix problem when parallelizing code and using preprocess objects.
* Fix incorrect computation of number of right-censored events during 
  preprocessing when `startTime` is specified.
* Add `lifecycle` and `cli` to imports.
* Rename functions names to `snake_case` to comply with the tidyverse
  style guide and `stocnet` interface.
* Add defunct documentation with functions that changed names.

# goldfish 1.6.12

* Fix invalid URL link in the `goldfish-package.Rd`.

# goldfish 1.6.11

* Register S3 methods for internal functions.
* Update R version dependency to 4.1.0 to use the anonymous function syntax.
* Delete configure files; OpenMP support is now managed directly through
  the Makevars files.

# goldfish 1.6.10

* Solve invalid URL link for the MIT Social Evolution dataset.

# goldfish 1.6.9

* Solving missing package anchors in the documentation.
* Update deprecated functions in the vignettes from `manynet`.
* Solves issue on `Makevars.in` file that produced a note when check as CRAN. 

# goldfish 1.6.8

* `checkEvents()` method addresses S3 generic/method consistency.
* Solve invalid URL link in the bibliography.

# goldfish 1.6.7

* Debug and update yaml workflow files for GitHub Actions.
* Solve note mismatches generic/method consistency

# goldfish 1.6.6

* Debugging and extend documentation for `GatherPreprocessing()`.
* Fix note from CRAN checks.
* Debug issue when using a preprocess init object in `estimate()`.
* Debug issue with opportunity list in `estimate()` for `DyNAM` models.
* Fix error in printing output from `estimate()` when using
  a parameter is fixed to a value.  

# goldfish 1.6.5

* Solve `startTime` and `endTime` bug on `DyNAM` and `REM` models
  preprocessing.
* Export `GatherPreprocessing()`. Experimental functionality.
* Clean unnecessary functions imports.
* Solve `aes_string()` deprecation.
* Solve issue on `C++` engine on DyNAM-rate.
* Enforcing an 80-character line length on C++ code.
* Comply with code style.
* Change that reduce execution time in the R estimation engine.

# goldfish 1.6.4

* Change mentioning of `.GlobalEnv` and drop `goldfishObjects()`.

# goldfish 1.6.3

## New features

* Add DyNAM-i vignette.
* Implement method `vcov()`, now is possible to use `stats::confint()` to obtain
  confidence interval for parameters estimates under asymptotic normality, and
  also get them from a `tidy()` call.

## Breaking changes

* `silent` parameter replace by `progress`.
  `progress` and `verbose` can be set with global options with the same name.

## Minor improvements and fixes

* Reimplement method `logLik()`, now is possible to use `AIC()` and `BIC()`
  from `stats` package and make likelihood ratio test of nested models using
  `lmtest::lrtest()`.
* Improve documentation of `define` functions, `linkEvents()` and `print` methods.
  The reserved names and expected variables are explained.
* Drop `fig.retina` from figures in vignettes and keeps default size.
* Update `vignette("goldfishEffects")` documentation for closure effects.
* Window effects doesn't create new object on the global environment.
* Add references in description.
* Fix bug composition change on estimation routines.
* Includes environment argument on estimation routines.

# goldfish 1.6.2

* Fix additional URL on Vignette "How to start".

# goldfish 1.6.1

* Fix URLs. 
* Estimate examples now use the `C` engine.

# goldfish 1.6.0

* Cleaning submission to CRAN.

# goldfish 1.5.3

* Effects documentation migrated to a vignette.
* Precompile vignettes with long-run time for compilation.

# goldfish 1.5.2

* Fix minor bugs on `opportunityList`
* New effect ego alter interaction `egoAlterInt(list(egoAttr, alterAttr))`
* Cleaning vignettes and changing networks visualization to use `migraph` package

## Breaking changes

* Closure effects renaming. 
  
  ```R
  clSender(net) -> commonSender(net)
  clReceiver(net) -> commonReceiver(net)
  mixedClSender(list(net1, net2)) -> mixedCommonSender(list(net1, net2))
  mixedClReceiver(list(net1, net2)) -> mixedCommonReceiver(list(net1, net2))
  ```
  
* Updated `examine.outliers()` and `examine.changepoints()` to plot more informative plots (with `{ggplot2}`)
  - These functions no longer print to the console
* Added vignette `teaching2.Rmd` that also indicates these extensions
* Added vignette `teaching1.Rmd` with the "How to start"


# goldfish 1.5.1

* Re #3 added `{broom}`-related functions `tidy()` and `glance()`
* Updated `examine.outliers()` and `examine.changepoints()` to plot more informative plots (with `{ggplot2}`)
  - These functions no longer print to the console
* Added vignette `teaching2.Rmd` that also indicates these extensions
* Added vignette `teaching1.Rmd` with the "How to start"

# goldfish 1.5.0

## New features

* New DyNAM-i model available to model face-to-face interaction data, collected through video or RFID badges.
* Functions to transform interaction data into DyNAM-compatible objects

  ```R
  defineGroups_interaction(interactions_data, actors, seed.randomization)
  ```
* Estimation functions for the joining and leaving rates of a DyNAM-i model

  ```R
  estimate(
    dependent.events_interactions ~  1 + intercept(network_interactions, joining = 1) +
                                  ego(actors$age,joining = 1, subType = "centered") +
                                 ego(actors$age,joining = -1, subType = "centered"),
    model = "DyNAMi", subModel = "rate")
  ```
  
* Estimation functions for the choice part of a DyNAM-i model

  ```R
  estimate(
    dependent.events_interactions ~ diff(actors$age,subType="averaged_sum") ,
    model = "DyNAMi", subModel = "choice", 
    estimationInit = list(opportunitiesList = opportunities))
  ```
* New closure effects for `model = "DyNAM"` with `subModel = "choice"` and `model = "REM"`.
  Documentation is updated accordingly. 
  
  ```R
  cycle(bilatnet)
  clSender(bilatnet)
  clReceiver(bilatnet)
  mixedCycle(list(bilatnet, contignet))
  mixedClSender(list(bilatnet, contignet))
  mixedClReceiver(list(bilatnet, contignet))
  ```
  
## Minor improvements and fixes

* Minor bugs in parsing and printing solved.
* Solve some warnings regarding S3 generic/method consistency.
* Solve no visible binding for global variables.
* Documentation improvements creating new documentation pages
  aggregating similar functions on `print-method` and `update-method`.

## Breaking changes

* `tertius_diff()` and `node_trans()` are changed to `tertiusDiff()` and 
  `nodeTrans()` complaining with naming convention.

# goldfish 1.4.3

## New features

* `mixedTrans()` effect is created. It requires the definition of two networks
  to compute the number of two paths between nodes.

  ```R
  mixedTrans(list(bilatnet, contignet))
  ```

## Minor improvements and fixes

* `estimate()` no longer give warnings in R 4.0.0.
* Classes and types checkings are revisited and extended.
* `linkEvents()` is refactored as a S3 method.
* New `print` or `summary` methods for `goldfish` objects are developed.
* `head()` and `tail()` methods for `nodes.goldfish`, `network.goldfish`
  and `dependent.goldfish` are available.

# goldfish 1.3.2

## New features

* `tertius()` and `tertius_diff()` effects are created. It requires the
  definition of a network and an attribute.

  ```R
  tertius_diff(bilatnet, states$gdp)
  tertius(bilatnet, states$gdp)
  ```

# goldfish 1.3.1

## New features

* `estimate()` now estimation routine in `C` for `model = "DyNAM"` and
  `subModel = "choice_coordination"`.

# goldfish 1.3.0 

## New features

* `estimate()` parameter `engine` through `estimationInit` argument control the estimation
  routine used.
  
  ```R
  mod01 <- estimate(callsDependent ~ inertia + recip + trans,
                    model = "DyNAM", subModel = "choice",
                    estimationInit = list(engine = "gather_compute"))
  ```
  
* `estimate()` now estimation routine in `C` except for `model = "DyNAM"` and
  `subModel = "choice_coordination"`.
* New effects definition for `model = "DyNAM"` and `subModel = "rate"`.

## Minor improvements and fixes
* `inertia` fixed bug when `weighted = TRUE`.

## Breaking changes

* `modelType` argument in `estimate()` have been deprecated in favor of `model` 
   and `submodel`.
   
## Internal changes

* New implementation of effects to improve preprocessing time in the initialization
  of the statistical matrices and the reintroduction of a cache object for complex 
  structural network effects.

## Minor improvements and fixes

* Added more examples for `estimate()`.
* Extend documentation of `defineDependentEvents()`, `defineGlobalAttribute()`, 
  `defineNetwork()`, `defineNodes()` and `linkEvents()`.

# goldfish 1.2.1 

* Introduce a new efficient estimation routine.