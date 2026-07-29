# goldfish 1.9.21

* **BREAKING: `diagnose_outliers()` and `diagnose_changepoints()` return
  different objects, and different numbers.** The returns are now classed
  tibbles -- `diagnose_outliers` and `diagnose_changepoints` rather than the
  shared `diagnostic.goldfish` -- carrying the diagnostic metadata contract
  (which method, which threshold, which intervals were analyzed) so a saved
  object explains itself and a plot method reads what it is looking at instead
  of guessing from the columns present. The `outlier` and `cpt` columns are
  logical.

* **BREAKING: the two functions now analyze the dependent intervals by
  default, so the flagged events and detected changepoints move on rate and
  REM fits.** A dependent interval contributes a log density including the
  observed alternative's term; a right-censored one contributes only the
  timing term. Pooling them let the median, the interquartile range, the
  Hampel window and the changepoint segmentation describe the censoring
  pattern rather than the fit -- most visibly with windowed effects, where a
  window opens at each event and closes a fixed time later, so the two kinds
  of interval alternate almost one for one and a changepoint is reported at
  nearly every closure. Pass `include_censored = TRUE` for the old pooled
  behavior. The returned table keeps one row per interval either way: the
  censored rows are present and simply never flagged. Multinomial sub-models
  have no right-censored intervals, so their results are unchanged.

* **`diagnose_changepoints()` and `diagnose_outliers()` gain `effect =`**,
  switching from a question about the model to one about a single coefficient:
  changepoints then segment that term's scaled Schoenfeld residuals, whose
  level is the coefficient an interval votes for, so a break is a regime shift
  in the effect rather than in overall fit; outliers rank by the term's
  absolute `dfbeta`, so they localize which intervals carry the estimate
  rather than which were surprising. The selected term is recorded on the
  returned object for plot labelling. A changepoint found this way is
  exploratory -- the split was chosen by looking at the data, so re-testing it
  on the same data is not confirmatory, and the documentation says so.

* **`model_terms()` reports every name a term answers to**, and every argument
  that takes a term now accepts any of them. goldfish renders a term three
  ways -- the compact string the summary prints, the export form, and the
  `coef()` abbreviation -- and until now `initial_parameters` accepted only the
  last, which is the one nobody can guess from reading a summary. It also
  accepts the coefficient's position. `model_terms(fit, pattern = )` lists and
  searches them, and returns the **full** compact strings: the printed summary
  abbreviates to the console width, and the abbreviation can silently collide
  (two `indeg` terms on different networks both render `ideg/networ` on a
  narrow console), so a string copied from there may select the wrong term. An
  unrecognized or ambiguous name now says which spelling resolves it.

* **`diagnose_onset()` measures what the start of the sequence did to the
  estimate.** Early events carry a left-censored history: the endogenous
  statistics are still at their initial values, so every alternative looks
  alike and the endogenous score contributions are exactly zero. The function
  reports the leave-initial-segment-out parameter path and the cumulative
  share of the information the sequence has delivered, both from the stored
  score rows and the fit's information matrix -- no evaluation pass, no
  preprocessed statistics -- and both indexed by dependent-event count rather
  than by interval, so the axis counts history rather than window closures.
  The result is descriptive: it reports which events carry the estimate, and
  no p-value is computed. `information = "expected"`, the exact per-event
  Fisher form, is not available yet and says so rather than quietly returning
  the outer-product one. Its object is the first goldfish diagnostic that is a
  classed *list* of tibbles rather than a single classed tibble -- a parameter
  path, an accrual curve and a per-coefficient summary -- carrying the same
  metadata contract either way.

* `augment()` places its rows in interval order. Previously the right-censored
  rows were appended after the dependent events while every per-interval
  column was in interval order, so from the first censored interval onwards
  each column was paired with the wrong event -- and `diagnose_outliers()` /
  `diagnose_changepoints()` read that table. It also gains the broom-convention
  `.fitted` and `.resid` columns (`NA` on right-censored intervals, which
  realize no outcome), and is now registered as an S3 method on a re-exported
  `generics::augment` like `tidy()` and `glance()`.

# goldfish 1.9.20

* **A fit can say how much each actor was at risk, not just how often it
  acted.** `set_algorithm_newton(diagnostics = "availability")` stores two
  per-actor vectors: `n_opportunities`, the number of dependent events whose
  realized risk set contained the actor, and -- on the exact-time sub-models --
  `exposure`, the time it spent at risk, summed over every interval including
  the right-censored ones. They are the denominators the stored margins are
  read against, in exactly the margins' shape: the same actor labels, and the
  same `_sender` / `_receiver` split on a REM fit, since a dyad at risk makes
  its sender available on one side and its receiver on the other. Both count
  per actor **membership** -- an actor at risk in many dyads of one interval
  contributes that interval once -- so `observed / exposure` is an event rate
  per unit time at risk rather than a count of dyads. `evaluate_model(return =
  c("exposure", "n_opportunities"))` computes the same vectors on demand for a
  fit that did not store them.

* **`residuals(type = "schoenfeld")` becomes available on the exact-time
  sub-models**, through the new `"conditional_scores"` primitive: the score
  rows of the model's conditional (partial) likelihood, which is what a
  Schoenfeld residual is. They are computed during estimation because they
  cannot be recovered afterwards -- a stored score row carries an exposure term
  that cannot be removed without the observed alternative's own statistic row.
  Requesting them on a multinomial family stores nothing and warns nothing:
  those likelihoods are already conditional, so their `event_scores` *are* the
  conditional rows.

* **Every fit now carries the interval clock of its likelihood** -- `intervals`,
  `start_time` and `end_time` -- regardless of what `diagnostics` asked for. A
  diagnostic can put a per-event quantity on observed time rather than event
  index, and the Cox-Snell residual of an exact-time fit is the exact product
  `intervals * total_rate`, needing neither an evaluation pass nor the
  preprocessed statistics. The fitted object's format version is deliberately
  unchanged: it names a released layout and moves once per release, so a purely
  additive component does not invalidate any stored object.

# goldfish 1.9.19

* **One argument names what estimation stores per event.**
  `set_algorithm_newton(diagnostics = )` takes the primitives you want kept
  on the fit -- `"loglik"`, `"scores"`, `"ranks"`, `"margins"`,
  `"probabilities"` -- with `TRUE` (the default pair `c("loglik", "scores")`),
  `"all"` and `FALSE` as shorthands. `return_interval_loglik` and
  `return_probabilities` keep working and now warn once, naming the primitive
  they map to; supplying a flag and a conflicting `diagnostics` value aborts.
  `return_event_scores` is removed outright rather than deprecated: it never
  shipped in a public release, so no released script can break. Asking for
  `"probabilities"` on a long sequence now says up front how large the stored
  vectors will be.

* **`margin_table()` reads a fit's per-actor margins as one table.**
  Five columns -- `actor`, `role`, `observed`, `expected_probability`,
  `expected_count` -- identically shaped on every family, so comparing
  observed against expected actor activity does not branch on the model: rate
  fits contribute `sender` rows, choice fits `receiver` rows, coordination
  fits `endpoint` rows (whose observed column totals twice the event count,
  each event crediting both members), and a REM fit both sides per actor from
  the same fit. `expected_count` is `NA` on the multinomial families, which
  means the compensator scale is not defined for the model class -- not that
  it was not computed. Those families therefore have a calibration ratio but
  no martingale residual. A multi-process fit row-binds its processes with
  `flavor` and `family` columns.

* **The stored margins say which actor and which scale.** Each vector is named
  by actor label, and each expected vector records its scale: `"probability"`
  for the next-event probability sums every family accumulates over dependent
  events, `"expected_count"` for the compensator sums exact-time fits
  additionally accumulate over all intervals, right-censored ones included.
  The two answer different questions -- the probability scale totals the event
  count at any parameter vector, the compensator scale only at the maximum --
  and both are attached by the same helper on every backend, so a fit's
  margins carry the same labels whichever implementation produced them.

# goldfish 1.9.18

* **A fixed coefficient can be written where it belongs: in the formula.**
  `offset(inertia(net), coef = -1.2)` holds that term at that value, in that
  formula. This is what makes a multi-process specification estimable with
  offsets at all: its processes are estimated from one shared control object,
  so `offset_coef` there cannot say which process a value is for -- the same
  term can appear in several of them, on the log-rate and the log-odds scale
  at once, under the same coefficient label. A formula belongs to exactly one
  process, so a value written in it cannot be misread. `offset_coef` and the
  superseded `fixed_parameters` now abort on a multi-process specification and
  point at `coef =`; single-process models keep both routes, and a term given
  a value by both aborts naming the term. A specification where only one
  process carries an `offset()` term now estimates, instead of aborting at the
  processes that carry none.

* **Starting values align to terms by name.**
  `set_algorithm_newton(initial_parameters = c(inertia = 0.5))` seeds the
  coefficients it names -- matched against the labels `coef()` reports -- and
  leaves every other one at its default. Seeding one term no longer means
  counting positions for all of them, and it no longer silently disables the
  rate intercept's data-derived starting value: that now depends on whether
  the intercept itself was seeded, not on whether any starting vector was
  supplied. The full-length unnamed form is unchanged. On a multi-process
  specification a flat named vector applies to every process carrying a
  matching label, and a list keyed by flavor -- optionally by family within it
  -- targets particular processes.

* **Errors about fixed and seeded coefficients name the term.** An
  `offset_coef` of the wrong length, a value from two sources, an offset term
  with no value, a misspelled coefficient name: each says which term it means,
  and an unknown name lists the ones that were available. Internally, which
  coefficients are held and where they start is now decided once, at the point
  where term names and coefficient positions are both known, and read by every
  estimation path from one place.

* **A fitted model records fixedness as a logical.** The effect description is
  a typed table with a stable set of columns, so `tidy()` reports `fixed` as a
  logical rather than the string `"TRUE"`. Objects fitted before goldfish
  2.0.0 are unaffected by this in practice: they are already refused by the
  result-format check, which tells you to re-fit.

# goldfish 1.9.17

* **`compute_statistics()` is the single statistics-product function, and it
  now covers everything `gather_model_data()` did plus two products it did
  not.** `output` takes `"preprocessed"` (the estimation-ready replay object),
  `"gather"` (the stack), `"data.frame"` (new -- the ready-to-estimate long
  frame), and `"db"` (the DBI stream). `gather_model_data()` is soft-deprecated
  onto it and keeps working for a release cycle; it rejected
  `sub_model = "rate_ordered"` at its own `match.arg` while estimation ran that
  model perfectly well, which is the drift a single function removes -- the
  model vocabulary is now validated once, on the estimation path, so the two
  surfaces cannot disagree again.

* **`output = "data.frame"` returns a long frame you can hand straight to a
  standard estimator.** One row per event x candidate -- the same rows the
  gather stack holds, so a `support_constraint` has already removed what it
  excludes -- with `event`, `chosen`, `sender`, `receiver`, `index_i`,
  `index_j`, `timespan`, `is_dependent`, then one column per effect named by
  that effect. `sender` and `receiver` are the labels of *that row's* dyad,
  decoded from its own indices rather than the observed event's actors
  repeated down the event's rows. The help page carries four verified recipes
  on it: `survival::clogit` and `survival::coxph` for the choice sub-models,
  `mlogit::mlogit` through `dfidx()` keyed on `index_j`, and a Poisson `glm()`
  with `offset(log(timespan))` for the exact-time ones. Each reproduces the
  corresponding `estimate_*()` fit; a test pins the conditional-logit route at
  1e-4 (observed: 2e-6).

* **A db export is now readable without the session that wrote it.** Statistic
  columns are named by their effect instead of `stat_<i>`, and every export --
  flavored or not -- writes one statistics table per modeled process
  (`<db_table>_<fid>`), a `<db_table>_map` naming those tables with the process
  identity, and a `<db_table>_nodes` resolving `index_i` / `index_j` back to
  the original nodes in SQL. One reader therefore handles both cases, and a
  model that later gains flavors does not change the schema. The single-process
  table is renamed in the process (`<db_table>` becomes `<db_table>_1`), which
  costs nothing: `output = "db"` has only ever existed in the unreleased 2.0.0
  line.

* **Constrained `gather` and `db` output worked in neither form before and
  works in both now.** Products are rendered *after* a `support_constraint` is
  realized and folded into availability, and the expansion honors the
  availability encoding that folding produces. Previously the first defect
  raised `order(): argument 1 is not a vector` and the second read a dense mask
  as a vector and indexed past the statistics matrix.

* **Flavored specifications return every output form**, as a list with one
  element per modeled process, keyed by the integer formula id and carrying the
  `process_map` identity table -- the same keying `estimate_*()` returns.

* **`model = "DyNAMi"` is supported at every output form.** Its preprocessing
  runs through the interaction front-end and is converted with the same bridge
  estimation uses, so the exported rows are the risk set the fit has, joining
  availability included.

* **`has_intercept` and `right_censored` are reported on every output form**,
  so a consumer never has to infer the likelihood shape from the columns. Both
  are `TRUE` for `sub_model = "rate"` and `FALSE` for `"rate_ordered"` and the
  choice sub-models. Index statistic columns by name: the exact-time sub-models
  prepend an `Intercept` column, so the same formula puts the same effect at
  different positions.

* `estimate_*(preprocessing_only = TRUE)` is soft-deprecated in favor of
  `compute_statistics(output = "preprocessed")`, which returns the same object.

* `check_model_par()` now reports an unavailable `sub_model` with a cli error
  listing what the model does allow, and the deprecated
  `model = "REM", sub_model = "choice"` warning names both successors --
  `"rate"` (exact-time) and `"rate_ordered"` (ordinal). The remap lands on the
  exact-time flavor with a force-added intercept, which shifts every statistic
  column by one; the warning now says so.

# goldfish 1.9.16

* **BREAKING** -- **every component of every object goldfish returns is now
  snake_case.** The 1.7.0 renames retired camelCase from the exported function
  API but never swept the returned objects, so a single fitted model carried
  `logLikelihood` beside `event_scores`. A reader could not predict which
  convention a component followed, and reading the wrong spelling yields `NULL`
  rather than an error -- silently, because `NULL[[i]]` is `NULL` again, so code
  written against a wrong name runs and reports nothing.

  On a fitted model: `standardErrors` becomes `standard_errors`,
  `logLikelihood` becomes `log_likelihood`, `finalScore` becomes `final_score`,
  `finalInformationMatrix` becomes `final_information_matrix`, `nIterations`
  becomes `n_iterations`, `nEvents` becomes `n_events`, `nParams` becomes
  `n_params`, `intervalLogL` becomes `interval_log_lik`, `eventProbabilities`
  becomes `event_probabilities`, and `sizeIntermediate` becomes
  `size_intermediate`.

  In the nested `convergence` report, where four of the five components were
  camelCase and one was not: `isConverged` becomes `is_converged`, `returnCode`
  becomes `return_code`, `maxAbsScore` becomes `max_abs_score`, and
  `maxAbsUpdate` becomes `max_abs_update`. `score_rel_norm` is unchanged.

  `summary()` now returns `coef_mat` rather than `coefMat`. `augment()` now
  returns the columns `interval_log_lik` and `right_censored_event` rather than
  `intervalLogL` and `rightCensoredEvent`.

  On a `gather_model_data()` export: `namesEffects` becomes `names_effects` and
  `isDependent` becomes `is_dependent`.

  On a preprocessed object: `initialStats` becomes `initial_stats`,
  `dependentStatsChange` becomes `dependent_stats_change`,
  `rightCensoredStatsChange` becomes `right_censored_stats_change`,
  `rightCensoredIntervals` becomes `right_censored_intervals`, `orderEvents`
  becomes `order_events`, `startTime` becomes `start_time`, `endTime` becomes
  `end_time`, and `version` becomes `prep_version`.

* **The old component names stop working immediately, rather than after a
  deprecation period.** This is deliberate, not an oversight. A fitted object
  from the last CRAN release (1.6.12) is already unusable for reasons unrelated
  to spelling: the 1.7.0 renames left it without components the current methods
  require, so its `summary()` fails and its `logLik()` returns a value carrying
  no degrees of freedom, which makes `AIC()` and `BIC()` report wrong numbers
  without saying so. Keeping the old spellings alive would have preserved
  `fit$logLikelihood` on an object whose model comparison is silently wrong,
  and would have cost permanent bookkeeping to do it.

* **Objects now record the layout they were built with, and it is format version
  2 for everything 2.0.0 produces.** A fitted model records `fit_version` and a
  preprocessed object records `prep_version` — two names rather than one, so both
  stay readable when a fit carries the preprocessed object it was estimated from.
  The number identifies the *shape* of the object rather than the release, so it
  moves only when the components change and a stored object stays current across
  ordinary upgrades.

  An older fit is now recognized rather than met with an internal error.
  `print()` reports that the object predates the current version and still shows
  the coefficients it can; `summary()`, `logLik()`, `vcov()`, `augment()` and the
  `AIC()` / `BIC()` path fail with the same explanation, because returning a
  number whose degrees of freedom are unknown is worse than refusing. `coef()` is
  unaffected — `parameters` was never renamed. A preprocessed object from an
  earlier version is refused when it is passed back for estimation, with a
  message telling you to recompute it.

* The internal event-ordering helper is renamed `arrange_events()`, so that the
  preprocessed component now spelled `order_events` no longer shares a name with
  an unrelated function. Neither is user-facing.

* The internal model-specification constructor no longer carries the reserved
  `engine` parameter. It accepted only its default and aborted on every other
  value, reserving an incremental estimation variant that was never
  implemented, and it reintroduced `engine` -- the vocabulary replaced by
  `backend`.

# goldfish 1.9.15

* `risk_set_axis()` is a new exported accessor reporting which axis a position in
  a per-event diagnostic component refers to -- `"sender"`,
  `"receiver_given_sender"`, `"dyad"` or `"dyad_symmetric"`. Fitted models carry
  the value as `$risk_set_axis`. Two fits over the same actors can return
  per-event vectors of identical length that index different things: on a
  DyNAM-rate model position `i` is a sender, on a DyNAM-choice model it is a
  candidate receiver. The axis was recoverable only through an unexported
  function, so a diagnostic written outside the package had to reach in with
  `:::` or re-derive the geometry from `model` / `sub_model`.

* The fitted object's `node_lookup` is now documented as *the* resolver for a
  per-event index: join a position to the side the axis names, and read the
  original node row and label from there. Per-event components deliberately carry
  no actor names of their own -- one lookup table per fit rather than one copy per
  event. Nothing about the stored data changed; the contract it satisfies is now
  written down.

# goldfish 1.9.14

* **Every per-event diagnostic primitive is now available on every backend.**
  `loglik`, `scores`, `ranks`, `margins` and `probabilities` are produced by
  `backend = "cpp"`, `"r"` and `"gather"` alike, and agree numerically across
  the three -- ranks exactly, everything else to 1e-10 at a fixed parameter
  vector. Previously the answer to "which backend gives me which diagnostic"
  had four different shapes: an abort for `scores` on `gather`, a silent drop
  of that same primitive when it came from the default rather than being named,
  an unmarked absence for `ranks` and `margins`, and a backend substitution for
  `probabilities`.

  Because the substitution ran first, the verdict for one primitive depended on
  what else was requested alongside it -- asking for *more* could return
  *less*:

  ```r
  # before 1.9.14
  set_algorithm_newton(backend = "cpp", diagnostics = c("loglik", "ranks", "margins"))
  #> carries ranks and margins
  set_algorithm_newton(backend = "cpp", diagnostics = "all")   # a superset
  #> silently dropped ranks and margins by moving the fit onto `backend = "r"`
  ```

  There is now one rule: a requested primitive is produced by the backend you
  chose, or the call aborts naming the backends that produce it. Nothing is
  dropped, downgraded, or rerouted.

* **BREAKING** -- `set_algorithm_newton(diagnostics = "probabilities")` no
  longer moves the fit to `backend = "r"`. Code that relied on the redirect now
  gets per-event probabilities computed by the backend it asked for. The
  redirect already emitted a warning, so no silent path changes behavior; but a
  fit that was quietly produced by `r` is now produced by `cpp` or `gather`,
  and the coefficients differ at the usual cross-backend tolerance.

* **BREAKING** -- the per-event probability vectors in `$eventProbabilities`
  are indexed by actor over the whole node set, with `0` for alternatives
  outside that event's risk set. They previously covered only the event's
  reduced risk set, so a position meant a different actor at different events
  (on one fixture with composition change, 13 distinct lengths across 215
  events), and a non-reflexive receiver was dropped rather than zeroed. Code
  that indexed into these vectors positionally must be updated; code that
  matched them against actor ids now works without replaying the presence
  history, and they are indexed consistently with `$margins`.

* **BREAKING** -- the control object returned by `set_algorithm_newton()` no
  longer carries `$engine`. Read `$backend` instead. A control list built by a
  pre-2.0.0 constructor (restored from an `.rds`, say) still estimates: its
  legacy token is translated on read, silently, since the deprecated surface
  already warned at construction time.

* Fitted models record which backend produced them, in a new `backend`
  component, so a diagnostic can tell a `gather` fit from a `cpp` one instead
  of inferring it. Fits from earlier versions lack the component; consumers
  treat its absence as unknown rather than erroring.

* Exact-time sub-models (DyNAM-rate, REM) gain two per-event quantities on the
  `cpp` backend that it never computed: the Cox partial-likelihood contribution
  `log p_obs`, and actor margins on the probability scale beside the existing
  expected-count scale. The two margin scales answer different questions -- the
  probability scale totals the event count at any parameter vector and
  calibrates *who* moves, while the expected-count scale integrates exposure
  over every interval and totals the event count only at the maximum. Both are
  stored under `"margins"`.

* DyNAM-rate gains the `probabilities` primitive, defined there as the
  competing-risks probability that each sender creates the *next* event. It
  sums to 1 per event, exactly as the choice model's does.

# goldfish 1.9.13

* `set_algorithm_newton(backend =)` replaces `engine =`, with values that name
  what actually runs rather than recording implementation history: `"cpp"` (the
  C++ event loop, still the default), `"r"` (the R reference implementation),
  and `"gather"` (the gather-stack C++ variant). The old values misled twice
  over -- `"default_c"` *was* the default while `"default"` was not, and
  `"gather_compute"` is an internal strategy name on a user-facing argument.

  | Old | New |
  |---|---|
  | `set_algorithm_newton(engine =)` | `set_algorithm_newton(backend =)` |
  | `engine = "default_c"` | `backend = "cpp"` |
  | `engine = "default"` | `backend = "r"` |
  | `engine = "gather_compute"` | `backend = "gather"` |

  Every previous call keeps working. `engine =` remains a sentinel, and the
  legacy values are accepted wherever they are supplied -- on `engine`, or on
  `backend` in a half-migrated call -- each producing exactly one
  soft-deprecation warning that names the final spelling, never an intermediate.
  Nothing about the computation changes: the value resolves at the constructor
  to the token the estimation path already read, so coefficients are identical.
  Removal follows the rest of the 2.0.0 alias layer, no earlier than goldfish
  3.0.0.

* Estimation messages that name a computational path now speak the same
  vocabulary: the maxLik-optimizer gate asks for `backend = "cpp"`, and the
  `opportunities_list` redirect says it is estimating with `backend = "r"`
  instead. (The per-event score and `return_probabilities` messages this bullet
  originally also listed are gone in 1.9.14, which makes every primitive
  available on every backend.)

# goldfish 1.9.12

* `compute_statistics()` replaces `compute_stats()`, which is deleted without a
  stub or deprecation cycle (the name only ever existed in the unreleased 2.0.0
  development line). The new function is born under the final naming: the
  formula comes first as `x`, model selectors precede `data` (matching
  `estimate_*()`), the preprocessing control is an explicit `control_prep`
  argument -- resolving the unusable `compute_stats(control_preprocessing = )`
  pass-through left by the naming migration -- and the estimation-ready object
  is requested with `output = "preprocessed"` (previously `"default"`). The
  gather and db output forms are unchanged.

* `to_ego()` and `to_alter()` are no longer exported. They are internal
  preprocessing helpers that expand rate-level change matrices to the dyadic
  format; their export only existed in the development line, no user-facing
  surface documents them, and the `to_*` prefix reads as manynet's
  transformation family.

# goldfish 1.9.11

## Naming migration (2.0.0)

The estimation control surface now follows the naming conventions shared across
the [stocnet](https://github.com/stocnet) family: control constructors are
`set_algorithm_*()` / `set_*()`, estimators take them as `control_algo` /
`control_prep`, and model criticism is `diagnose_[mode]()`. Every old name keeps
working: renamed functions are thin wrappers and renamed arguments are
sentinels, each emitting a soft-deprecation warning that names its replacement.

| Old | New |
|---|---|
| `set_estimation_opt()` | `set_algorithm_newton()` |
| `set_preprocessing_opt()` | `set_preprocessing()` |
| `examine_outliers()` | `diagnose_outliers()` |
| `examine_changepoints()` | `diagnose_changepoints()` |
| `estimate_*(control_estimation =)` | `estimate_*(control_algo =)` |
| `estimate_*(control_preprocessing =)` | `estimate_*(control_prep =)` |
| `estimate_*(preprocessing_init =)` | `estimate_*(preprocessed =)` |
| `diagnose_outliers(parameter =)` | `diagnose_outliers(threshold =)` |

* `set_algorithm_newton()` is named for the algorithm family it configures --
  direct maximization with Newton-type steps -- so the ascent-based Monte Carlo
  EM algorithm arriving with DyNES gets its own constructor rather than
  overloading this one. Its result carries the shared `algorithm.goldfish`
  superclass, which is the single check estimators validate against.

* `preprocessed =` is now the one name for supplying a `preprocessed.goldfish`
  object, matching what the diagnostic consumers already use.

* `gather_model_data()` deliberately keeps `control_preprocessing`: it is
  superseded as a whole in the next release, so renaming an argument on a call
  you are about to replace would be churn for nothing.

* `estimate_*(preprocessing_only = TRUE)` is **not** deprecated yet. Its
  replacement, `compute_statistics(output = "preprocessed")`, lands in the next
  release; the argument is retired then, together with its replacement.

* **Removal horizon.** This 2.0.0 alias layer and the 1.7.0 camelCase layer
  (`examineOutliers()`, `defineNodes()`, ...) are both scheduled for removal no
  earlier than goldfish 3.0.0.

# goldfish 1.9.10

## Breaking changes

* **`sub_model = "rate"` always models the waiting times between events.** A
  rate formula written without an explicit time intercept (e.g.
  `estimate_rem(y ~ inertia, sub_model = "rate")`) no longer auto-converts to
  the ordinal (order-only) model: the time intercept -- the baseline hazard the
  waiting-time likelihood needs -- is added, with an informative message. To
  model only the order of events, request the ordinal likelihood explicitly with
  `sub_model = "rate_ordered"`. Formulas that already carried a `1` intercept, or
  that already used `sub_model = "rate_ordered"`, are unaffected.

## Internal

* Risk-set dispatch is now spec-driven end to end. The typed model spec carries
  a risk-set descriptor (axis, fold target, encoding, symmetrization,
  normalizer), decided once at parse time; preprocessing, the estimation guard,
  the compiled interface, and the gather routines read it instead of
  re-deriving the model family from model-type strings or statistics-array
  dimensionality. A single engine-capability table guards constrained
  estimation. An ego-kind (outer) `support_constraint` on a DyNAM-choice model
  now folds into the maintained availability like every other kind, so
  estimation consumes maintained buffers only. The internal model-type string
  vocabulary (`legacy_model_type()`) has been removed. No change to fitted
  coefficients: the frozen 1e-6 baselines and C++ golden tests pass unchanged.

# goldfish 1.9.9

## Breaking changes

* **`make_data()` never silently returns a legacy environment.** A bundle of
  constructor objects either assembles to a `stocnet` or aborts with the reason.
  The common case is a layer built from a compound expression -- e.g.
  `make_network(nodes = fx$actors)` records the node set as an unresolvable name
  -- which previously degraded to a legacy environment with no signal and now
  errors, telling you to bind the node set to a plain name
  (`actors <- fx$actors; make_network(nodes = actors, ...)`). Bundles with no
  node set or no layer abort likewise.

* **Legacy `data.goldfish` environments are rejected at the public surface.**
  `estimate_dynam()`, `estimate_rem()`, `estimate_dynami()`, and
  `make_specification()` now abort when `data` is an environment (obtainable only
  from objects saved before the 2.0.0 flip), since every model family assembles
  to a `stocnet`. Rebuild the object with `make_data()` /
  `make_groups_interaction()`, both of which return a `stocnet`.

# goldfish 1.9.8

## New features

* **DyNAM-i on the single `stocnet` data object.** `estimate_dynami()` and
  `make_specification(model = "DyNAMi")` accept the single `stocnet` data object,
  matching the API the DyNAM and REM families already use.
  `make_groups_interaction()` returns that object directly, so the records ->
  model-ready flow is one step. The actors x groups interaction data is a
  two-mode `stocnet` layer (`interactions`) with a one-mode past-interaction
  covariate layer (`past`); nodal attributes are named bare (`ego(age)`,
  `same(gender)`) as on the rest of the stocnet surface, and networks by layer
  name (`interactions`, `past`). The two rate processes ride the flavor-keyed
  grammar `rate = list(join ~ ..., leave ~ ...)`; the joining `choice` is a plain
  formula. The joining choice's group-availability restriction (the groups
  occupied at each decision point, Hoffman et al. 2020 Eq. 8) is derived from the
  object's occupancy and folded automatically -- there is no `opportunities`
  argument. The vignette M1 rate and choice models on the RFID data reproduce
  the goldfish 1.7.0 coefficients to 1e-6, now frozen as regression baselines.

## Breaking changes

* **`make_groups_interaction()` returns a `stocnet`, not a five-component list.**
  The records->events transformation is unchanged, but its result is the
  assembled multipartite `stocnet` (ready to pass to `estimate_dynami()` /
  `make_specification()`), replacing the previous `groups` /
  `dependent.events` / `exogenous.events` / `interaction.updates` /
  `opportunities` list. The `opportunities` component is retired: group
  availability for the joining choice is derived from the object's occupancy at
  estimation, not stored.

# goldfish 1.9.7

## Bug fixes

* **The modeled layer, not `info$focal`, drives focal resolution during
  estimation.** A hand-built `stocnet` with no `info$focal` now estimates
  whenever the formula's left-hand side (or a specification's `layer`) names the
  dependent -- for one-mode and two-mode objects alike. Previously such an object
  aborted with an internal indexing error. `info$focal` is now only the optional
  *default* for which layer to model; the layer actually being modeled drives
  every focal, side, and mode lookup, so an `info$focal` that names a different
  layer than the one modeled no longer wins over the modeled layer.

# goldfish 1.9.6

## New features

* **Multipartite DyNAM and REM models over two-mode networks.** A `stocnet`
  whose focal layer runs between two different modes (an actor sending to a
  concept, an author to a paper) is now a first-class modeling object across the
  DyNAM rate, DyNAM choice, and REM engines. Node identity is carried by the
  object's mode map -- one nodes tibble with a `mode` column resolves each
  layer's side pair -- so an object may mix one-mode and two-mode layers, and a
  model over a two-mode focal layer reads one-mode covariate layers through
  their own side pairs. `make_data()` on the legacy two-node-set constructors
  now assembles to the same `stocnet` representation, and estimates to identical
  coefficients (a 1e-6 equivalence guard covers both construction paths). Two-mode
  preprocessed and estimation results carry a per-side node lookup, so gather and
  database exports join `index_i` / `index_j` back to each side's labels.

* **Effect-validity contract for two-mode focal layers.** Each effect's side
  requirements are checked at parse time against the data's mode map, per index
  position rather than as a blanket "works on two-mode" verdict. A one-mode
  closure effect on a two-mode focal layer (`recip`, `trans`, `cycle`) is
  rejected by name, comparing the two modes; the message lists the effects a
  two-mode focal layer does admit. Effects whose `type` variant selects a side
  (`indeg`, `outdeg`, `tertius`) reject the degenerate variant that would count
  ties that cannot exist.

## Bug fixes

* **A two-mode rate model no longer crashes with `'x' is too short`.** The rate
  spec constructor dropped the receiver side, collapsing the two-mode dimensions
  to the sender count.

* **`indeg()` / `degree()` no longer crash on a two-mode network with a
  `'length = 2'` coercion error.** The choice-set arguments (`type`, `history`,
  ...) are now resolved to a single validated value once, at the parser, before
  any effect init reads them, so a two-mode `indeg(x)` can no longer reach a
  length-2 `type` comparison. An out-of-set value (`type = "bogus"`) aborts at
  the formula, naming the effect, the argument, and the allowed set.

* **`make_specification()` and flavor-keyed specs no longer reject a
  `global()` / `ego()` interaction operand** as an unidentified main effect. An
  operand feeding an interaction term is held out of the main-effect
  identifiability check, mirroring the plain-formula path, so period-interacted
  choice models (`effect:global(period)`) build.

* **Categorical attribute imputation no longer writes `NA` into state.** A
  categorical nodal attribute with a missing `replace` value in an event stream
  reached a `mean()` of character values; the same event's effect update then
  received `NA` and died on an equality comparison.

* **Two-mode `choice_coordination` now aborts cleanly**, naming the sub-model
  and layer, instead of crashing in the C++ engine: coordination reads both
  directed dyads over a single node set, which a disjoint two-mode side pair
  cannot supply.

## Documentation

* **New `two-mode` vignette** reproducing the multimodal DyNAM of Haunss &
  Hollway (2023) on `manynet::irps_nuclear`: the `manynet` verb pipeline that
  builds the `stocnet` on goldfish's native POSIXct axis, the two-mode
  effect-validity discussion, the rate and choice models with two-mode *tertius*
  effects, discursive periods as both interacted global dummies and separate
  window fits (which agree to machine precision), and the distinction between
  structural and informative missingness.

* **`?goldfish_data` gains a multipartite section** describing mode sets, the
  manynet-to-stocnet conversion pattern, and a pointer to the vignette. The
  effects vignette's blanket "cannot be used for two-mode networks" claims are
  corrected: `four`, `same`, `diff`, `sim`, and `mixed_trans` do apply, and the
  degree and shared-partner statements are type-qualified.

# goldfish 1.9.5

## New features

* **`set_preprocessing_opt()` gains an `impute` argument** declaring a
  per-attribute imputation policy, a named character vector keyed by nodal
  attribute. The default `"summary"` is the published contract, so omitting the
  argument changes nothing. `"as_category"` recodes a factor or character
  attribute's missing values to a reserved `"(missing)"` level -- in the initial
  table and in the attribute's event streams -- so missingness by design
  survives to the summarizers as an ordinary category instead of being filled
  with the most common value. Validation aborts on an unknown attribute, an
  attribute no effect reads, `"as_category"` on a numeric attribute, a
  collision with an observed `"(missing)"` value, and the reserved-but-
  unimplemented `"locf"` value. The policy requires stocnet data objects.

## Internal

* **The R estimation engine's broadcast fan-out now keeps the reflexive
  diagonal cell on the same condition as the risk set** -- `allowReflexive ||
  is_two_mode` -- instead of testing `is_two_mode` alone, matching the C++
  engines. This is currently a no-op: `allowReflexive` is not exposed and is
  always `FALSE`, so the two conditions coincide for every fitted model and the
  frozen coefficient baselines are unchanged. It removes a dormant asymmetry so
  that if self-ties are ever allowed on a one-mode model, a broadcast-eligible
  effect will not silently drop the diagonal dyad the risk set retains.

# goldfish 1.9.4

## Behavior changes

* **A missing global attribute value now aborts** at schedule construction,
  before the walk begins, naming the object and -- for an event-stream value --
  its time. A global attribute holds a single value, so its imputation pool is
  empty by construction; the previous behavior gave two different wrong answers
  (a not-a-number from the mean of a length-one missing vector at the start of
  the window, an arbitrary zero written into state during the walk). Supply an
  observed initial value, a first event that sets one, or an explicit event
  value.

## Documentation

* **The missing-data contract is published** as a shape-by-time table on the
  `goldfish_data` help topic (inherited by the estimation topics) and in the
  DyNAM modeling vignette: the rule for every object shape (dyad, global, nodal
  numeric, nodal categorical) at both evaluation times, the disclosure that an
  imputed value joins the state and informs later imputations (so single
  imputation understates uncertainty), and a recommendation to combine multiple
  imputations under Rubin's rules with `mitools::MIcombine()`.

# goldfish 1.9.3

## New features

* **Multi-flavor estimation.** A specification keying several flavors now
  estimates. `estimate_dynam()` / `estimate_rem()` preprocess all its processes
  in one pass over the event sequence -- effects shared between flavors are
  computed once -- and fit each process separately, which the factorized
  competing-process likelihood makes exact rather than approximate. The result
  is a container holding one fit per process, printed in a section per flavor
  with its sub-models nested inside. `coef()` and `vcov()` return one component
  per process; `logLik()` sums them, so `AIC()` and `BIC()` follow. Components
  are labelled from an internal identity table rather than pasted keys, so
  layer and flavor names containing dots or colliding with one another cannot
  be mistaken for structure. A process's fit is identical to estimating that
  flavor alone with the equivalent constraint supplied by hand. *Experimental.*

* **`fisheries_treaties` ships unflavored, with `signing` / `ending` flavors.**
  The dataset no longer carries a pre-stamped `flavor` column; the examples and
  the coordination vignette call `add_flavor()` instead, so the reader sees the
  decision being made. The flavors are renamed because a treaty tie here counts
  agreements rather than recording whether one exists -- Russia and the USA sign
  fourteen times over five years against a single ending -- which makes them
  `flavor_style = "redundant"`, not a created-and-dissolved pair. Estimates from
  the vignette are unchanged.

## Bug fixes

* **A flavor style is no longer assumed.** A flavor-keyed list on an unflavored
  layer inferred the value mapping *and* silently assumed the flavors were
  mutually exclusive, which derived support constraints restricting the risk set
  without the user asking. Inference now resolves the mapping only and says that
  no constraint follows from it. Nothing in a layer's update values can
  distinguish a tie that toggles from one whose `+1`/`-1` increments accumulate,
  so the style is declared through `add_flavor()`.

* **Declaring a style with no value mapping no longer does nothing.** Deriving a
  constraint needs both the style and `values_equivalence`; supplying only the
  style left the risk set unrestricted in silence, and now aborts.

* **`add_flavor()` checks a `mutually_exclusive` declaration against the
  events**, warning and naming the first event that contradicts it -- one that
  lands where its own derived mask would forbid it, which is the failure
  estimation would otherwise raise much later. `flavor_style` now takes its
  allowed values in the signature and is matched with `rlang::arg_match()`, so a
  typo gets a suggestion.

* **R (>= 4.4.0) is now required.** The declared minimum was 4.1.0, but the
  package has been using base R's `%||%` since 1.9.0 and that operator entered
  base in 4.4.0, so installation on 4.1--4.3 failed at load. The declaration now
  matches what the code needs.

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
  Per-flavor preprocessing and estimation land in 1.9.3. *Experimental.*

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
