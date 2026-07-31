# model-evaluation-pass

A single no-iteration evaluation of a fitted model's engine at an arbitrary
parameter vector, returning requested quantities.

## ADDED Requirements

### Requirement: evaluate_model single-pass evaluator
goldfish SHALL provide `evaluate_model(x, at = coef(x), return, preprocessed
= NULL, ...)` performing exactly one evaluation pass (no Newton-Raphson
iterations) of the model's likelihood machinery at the parameter vector
`at`. The `return` argument SHALL accept any subset of `c("loglik",
"score", "information", "interval_loglik", "total_rate",
"conditional_logl", "event_scores", "conditional_scores", "ranks",
"recall", "margins", "probabilities", "exposure", "n_opportunities")`, and
the returned list SHALL contain
exactly the requested components. The per-event components SHALL mean what
the same-named stored components mean, so a recomputed quantity and a
stored one differ only in provenance; `"recall"` SHALL return one
proportion per requested threshold, the share of dependent events whose
observed alternative ranked within the top k, derived from the ranks of the
same pass so that a rank and a recall statistic cannot disagree about which
alternatives are tied.

The vocabulary SHALL additionally accept `"weighted_information"` and
`"event_information_trace"`, and the signature SHALL gain a public
`weights = NULL` argument. `"weighted_information"` requires `weights`, an
`n x m` numeric matrix over the stored intervals, and SHALL return the
`p x p x m` array whose `m`-th slice is `sum_k weights[k, m] * I_k` with `I_k`
the per-interval Fisher contribution **on the scale the total information is
accumulated at** — the timespan- or compensator-scaled block in the exact-time
families — so that a column of ones reproduces `"information"` exactly; the
third dimension SHALL be named by `colnames(weights)`.
`"event_information_trace"` takes no weights and SHALL return the length-`n`
vector of `trace(I_k)`. Both index **intervals**, right-censored ones included,
since those contribute to the information too. Requesting
`"weighted_information"` without `weights`, or with a matrix whose row count
does not match the intervals, SHALL abort naming the mismatch.

These SHALL NOT be materialized as per-event matrices: the accumulation
happens inside the single pass, so a caller asking for `m` weight columns pays
`m` scalar multiply-accumulates against a block the engine already forms, and
never `n` stored blocks. An implementation SHALL skip a zero weight, so that a
grouping expressed as indicator columns costs one accumulation per interval
rather than `m`.

`weights` is public because `evaluate_model()` is the only surface with access
to per-event information, and a weighted-information return is what lets a
caller write a diagnostic the package does not ship. Dispatch SHALL follow the fitted model's
model/submodel routing. Statistics SHALL come from the attached or supplied
`preprocessed.goldfish` per the diagnostic-primitives precedence rules.

#### Scenario: weighted information sums the per-event blocks
- **WHEN** `evaluate_model(fit, return = "weighted_information", weights = w)`
  is called with `w` a single column of ones
- **THEN** the returned `p x p x 1` array's only slice equals the model's
  information matrix, and with an arbitrary weight column the slice equals the
  same sum weighted by it

#### Scenario: a grouping costs one accumulation per interval
- **WHEN** `weights` is a set of disjoint indicator columns
- **THEN** the result is the per-group information blocks, and the run time
  does not grow with the number of groups

#### Scenario: the trace is per interval
- **WHEN** `evaluate_model(fit, return = "event_information_trace")` is called
- **THEN** it returns one value per stored interval, whose sum equals the trace
  of the model's information matrix

#### Scenario: evaluation at the MLE reproduces the fit
- **WHEN** `evaluate_model(fit, at = coef(fit), return = c("loglik",
  "score"))` runs on a converged fixture fit
- **THEN** the log-likelihood equals `logLik(fit)` within 1e-10 and the
  score of the free (non-offset) parameters is near zero (max absolute
  component below the convergence tolerance; offset columns carry the
  fixed value's nonzero score and are reported, not tested).

#### Scenario: evaluation at a constrained vector
- **WHEN** `evaluate_model` is called at a parameter vector with one
  effect fixed to zero on a model whose statistics include that effect
- **THEN** it returns the score and information of the full model evaluated
  at that vector, with dimensions matching the full effect set.

### Requirement: evaluator uses the estimation backend
`evaluate_model()` SHALL default to the backend used for the original
estimation and SHALL record which backend produced its output. Requesting a
backend that does not support a requested quantity SHALL abort with a cli
error naming the supported backends for that quantity.

#### Scenario: backend defaults to the fit's backend
- **WHEN** a fit estimated with `backend = "cpp"` is evaluated without
  an explicit backend
- **THEN** the evaluation runs on `cpp` and per-event quantities agree
  with those stored on the fit within 1e-10.

### Requirement: derived quantities computed in-pass
For `"ranks"`, `"recall"`, and `"margins"`, the evaluator SHALL compute the
quantities inside the engine's event loop and return only the summary
vectors (integer ranks per event; recall proportions at requested
thresholds; named per-actor margins). The full per-event probability matrix
SHALL NOT be materialized unless `"probabilities"` is explicitly requested.

#### Scenario: ranks without probability matrix
- **WHEN** `evaluate_model(fit, return = "ranks")` runs on a REM fixture
- **THEN** an integer vector of observed ranks (one per event) is returned
  and no probability matrix is allocated in the returned object.

#### Scenario: rank correctness on a small fixture
- **WHEN** ranks are computed on a fixture small enough to enumerate
  probabilities in R
- **THEN** each observed rank equals the rank of the observed event's
  probability within its realized risk set.

### Requirement: on-demand per-actor exposure quantities
The evaluator SHALL compute `"exposure"` (exact-time submodels only) and
`"n_opportunities"` (every family) on demand, accumulated inside the same
realized risk-set loop that computes the margins: per-actor total exposure
time — the sum of interval lengths over **all intervals, dependent and
right-censored**, during which the actor is at risk — and per-actor
opportunity counts — the number of **dependent events** whose realized
risk/choice set contains the actor. Both SHALL honor the same realized
risk set as estimation (support constraints, presence/composition changes,
state-derived flavor masks, the unified `twomode_or_reflexive` flag), and
SHALL be named by actor label. Both SHALL accumulate per **actor
membership**, not per risk-set position: an actor at risk in many dyads of
the same interval contributes that interval's length once, and that
event's opportunity once — so on the dyadic families the quantities are
counts over intervals and events, never over dyads. Membership SHALL be
counted **per side**, in the shape the fit's margins take: one vector on
the sender, receiver and endpoint families, and a sender and a receiver
vector on the two-sided REM families, since a dyad at risk makes its sender
available on one side and its receiver on the other. They SHALL be
obtainable both on demand here and, when requested at estimation, from the
fit's stored components, computed by the same engine flag in either case.
Requesting `"exposure"` from the evaluator on a multinomial-only fit SHALL
abort with a cli error naming `"n_opportunities"` as the defined
alternative. The accumulation sets
deliberately mirror the margins scales: exposure pairs with the
`"expected_count"` compensator (all intervals), opportunities with the
`"probability"` margins (dependent events only).

#### Scenario: exposure honors availability masks
- **WHEN** `evaluate_model(fit, return = "exposure")` runs on an
  exact-time fixture with a composition change removing an actor mid-window
- **THEN** that actor's exposure equals the summed interval lengths of only
  the intervals where it was present and at risk, and actors never at risk
  carry exposure zero, named by actor label.

#### Scenario: opportunities count dependent events only
- **WHEN** `evaluate_model(fit, return = "n_opportunities")` runs on a
  fixture with right-censored intervals
- **THEN** each actor's count equals the number of dependent events whose
  realized risk/choice set contained it, with right-censored intervals
  contributing nothing.

#### Scenario: exposure undefined on multinomial fits
- **WHEN** `"exposure"` is requested on a choice-submodel fit
- **THEN** the evaluator aborts with a cli error naming
  `"n_opportunities"` as the defined per-actor availability quantity for
  multinomial families.

#### Scenario: a dyadic fit counts intervals, not dyads
- **WHEN** `"exposure"` and `"n_opportunities"` are computed on a
  one-mode REM fixture where every actor is at risk in many dyads of each
  interval
- **THEN** no actor's exposure exceeds the observation window, and no
  actor's opportunity count exceeds the number of dependent events

#### Scenario: evaluated and stored availability agree
- **WHEN** the same fixture is fitted once with `"availability"` requested
  and once without, and the second is evaluated on demand
- **THEN** the two carry identical vectors, actor labels included
