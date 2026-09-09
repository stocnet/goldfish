## ADDED Requirements

### Requirement: Simulation-based goodness-of-fit entry point

The package SHALL provide an experimental entry point `gof_dynes(fit)` that takes a
single fitted `estimate_dynes()` result and returns a classed simulation-based
goodness-of-fit object comparing observed auxiliary statistics against their
simulated distribution. The entry point MUST let the user select which built-in
auxiliary statistics to evaluate and the number of simulated sequences, and MUST
resolve its base pool by the shared pool-source rule (reuse the fit's pool when
present and consistent, else regenerate from the fit's recipe).

#### Scenario: Simulation GoF produced

- **WHEN** the user calls `gof_dynes(fit)` selecting one or more built-in statistics
- **THEN** it returns a classed object carrying, per selected statistic, the observed
  value, the simulated distribution, and the Monte-Carlo Mahalanobis distance

### Requirement: Simulation at the point estimate

Simulation for the goodness-of-fit SHALL draw all sequences at the fitted point
estimate `θ̂`. The simulated auxiliary-statistic distribution therefore reflects the
model's stochasticity and Monte-Carlo noise but not estimation uncertainty. Drawing
`θ ~ N(θ̂, vcov)` per sequence is out of scope for v1.

#### Scenario: All simulations at theta-hat

- **WHEN** the goodness-of-fit simulates its sequences
- **THEN** every sequence is drawn at `θ̂`, with no per-sequence parameter draw from
  `vcov()`

### Requirement: Parallel simulation behind a map seam

The simulation batches SHALL run behind a single map seam whose default backend is
`mirai`, under the non-nested thread budget shared with estimation, with a serial
fallback when no daemons are configured. Parallel simulation MUST use stream-split
seeds so the simulated pool — and hence every statistic and distance — is identical
for a fixed seed regardless of the number of workers.

#### Scenario: Serial and parallel agree under a fixed seed

- **WHEN** the simulation runs serially and with `mirai` daemons under the same fixed
  seed
- **THEN** both produce identical simulated statistics and Mahalanobis distances

### Requirement: Layer-isolated fixed-time simulation

Simulation for the goodness-of-fit SHALL fix every event time — both the observed RE
times and the sampled PE times — and redraw only the sender–receiver–flavor tuple at
each time stamp, constraining the flavor at each stamp to the flavors of the layer
that owns that stamp (`φ ∈ 𝓕(X₁)` at an RE stamp, `φ ∈ 𝓕(X₂)` at a PE stamp), so
each layer is simulated in isolation. This SHALL be the time-anchored variant of
the general simulator (`simulate(times = "observed")`) driven with a
layer-restricted mark step, not a separate simulation mode, and the
documentation SHALL use the vocabulary "time-anchored" for it. The constrained
augmenters used for estimation MUST NOT be used for this simulation, because
they only ever reproduce observed changes.

#### Scenario: Flavor constrained to the owning layer

- **WHEN** the next fixed time stamp belongs to an RE layer
- **THEN** the redrawn tuple's flavor is sampled only from the RE layer's flavor set,
  and analogously a PE stamp draws only from the PE layer's flavor set

#### Scenario: Times held fixed across the simulation

- **WHEN** a sequence is simulated for the goodness-of-fit
- **THEN** the set of event times equals the fixed input times exactly, and only the
  sender, receiver, and flavor of each event are resampled

### Requirement: Capped replicates and unmodeled components are excluded from the statistics pool

The goodness-of-fit SHALL read, from each simulated sequence, the capped flag
the explosion guard sets and the per-component regime record (modeled /
completed / anchored-replay), and SHALL exclude capped replicates from the
statistics pool by default, reporting their count in aggregate; auxiliary
statistics that touch a completed or replayed component SHALL be excluded from
the comparison and named in the result rather than silently included.

#### Scenario: capped replicates are reported, not pooled

- **WHEN** 3 of 200 simulated sequences carry the capped flag
- **THEN** the statistics pool holds 197 sequences and the result reports
  "3 of 200 replicates hit the guard"

#### Scenario: a statistic on a completed component is excluded

- **WHEN** a flavor's choice was completed with the uniform default and an
  auxiliary statistic reads that flavor's receiver structure
- **THEN** that statistic is excluded from the Mahalanobis comparison and
  listed on the result with the reason "completed component"

### Requirement: Built-in auxiliary statistics

The package SHALL provide a fixed set of built-in cross-layer auxiliary statistics
(no user-extension surface in v1): the **number of RE events per observed
panel-data flavor**,
`A_{φ,φ',c} = #{ (i,j) : (i,j,φ') ∈ Υ(X₂), Σ_k 1{(i,j,t_k,φ) ∈ Ω} = c }`, and the
**closing of RE 2-paths on the panel layer**,
`A_{φ,φ',c} = #{ (i,j) : (i,j,φ') ∈ Υ(X₂), Σ_k 1{∃ u<v : (i,k,t_u,φ),(k,j,t_v,φ) ∈ Ω} = c }`.
Each statistic MUST be returned as a count vector `A_{φ,φ'} = (A_{φ,φ',1}, …,
A_{φ,φ',C})ᵀ` over `c = 1..C` for a chosen `C` (default: the maximum observed
count), and MUST be evaluated for every ordered pair of an RE-layer flavor `φ` and a
PE-layer flavor `φ'`.

#### Scenario: Events-per-flavor count vector

- **WHEN** the events-per-panel-flavor statistic is computed for flavors `(φ, φ')`
- **THEN** the result is the vector whose `c`-th entry counts the dyads showing the
  `φ'` panel change and exactly `c` relational events of flavor `φ`

#### Scenario: 2-path closure count vector

- **WHEN** the 2-path-closure statistic is computed for flavors `(φ, φ')`
- **THEN** the result is the vector whose `c`-th entry counts the dyads showing the
  `φ'` panel change and exactly `c` ordered RE 2-paths of flavor `φ` connecting them

#### Scenario: Truncation at C

- **WHEN** a count exceeds the chosen `C`
- **THEN** it is folded into the top cell `C` (or the default `C` is set to the
  observed maximum so no fold is needed), consistently for observed and simulated
  sequences

### Requirement: Monte-Carlo Mahalanobis comparison

The simulation GoF SHALL summarize the discrepancy between an observed auxiliary
statistic `A(z)` and its simulated distribution by the Monte-Carlo Mahalanobis
distance `MD = (A(z) − μ̃)ᵀ Σ̃⁻¹ (A(z) − μ̃)`, with `μ̃` and `Σ̃` the mean and
covariance estimated from the simulated statistics. When `Σ̃` is singular or
ill-conditioned (e.g. an all-zero or constant count cell), a generalized inverse or
documented regularization MUST be used, and the fact MUST be surfaced to the user
rather than silently producing an unstable distance.

#### Scenario: Mahalanobis distance per statistic

- **WHEN** the observed and simulated values of a selected statistic are available
- **THEN** the result reports the Mahalanobis distance of the observed vector from
  the simulated distribution, together with its Monte-Carlo tail position

#### Scenario: Singular covariance handled

- **WHEN** the simulated covariance `Σ̃` for a statistic is singular or
  ill-conditioned
- **THEN** the routine applies a generalized inverse or documented regularization and
  records a warning on the result rather than returning `Inf`/`NaN` silently

### Requirement: Reporting and plotting of the simulation goodness-of-fit result

The simulation-GoF object SHALL provide `print()`, `summary()`, and `plot()`
methods. `print()`/`summary()` MUST render through cli semantic elements, listing
each selected statistic with its Mahalanobis distance and Monte-Carlo tail
position; `plot()` MUST show the observed statistic against its simulated
distribution (per flavor pair / count cell). The object MUST be marked experimental
via a lifecycle badge in its documentation.

#### Scenario: Observed-vs-simulated plot

- **WHEN** the user plots a simulation-GoF object
- **THEN** it displays the simulated distribution of each selected auxiliary
  statistic with the observed value overlaid for comparison

#### Scenario: Summary lists distances

- **WHEN** the user summarizes a simulation-GoF object
- **THEN** the output lists, per selected statistic, its Mahalanobis distance and the
  fraction of simulations at least as extreme as the observed value
