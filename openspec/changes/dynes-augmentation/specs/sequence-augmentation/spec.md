## ADDED Requirements

### Requirement: Wave diffing produces the candidate flip set

The system SHALL diff consecutive snapshots of a panel-semantics layer into the
candidate event multiset for each modeled interval, consuming the layer's
transition/support specification. For a **state panel** (flavored, a wave recording the
tie state) each observed value change maps through the
(from-value, to-value) → flavor mapping, which SHALL be validated as injective (the
flavor is never latent); a change larger than one allowed step SHALL decompose into
a per-dyad ordered chain of events; net-zero changes SHALL produce no events. For a
**count panel** (unflavored and monotone, a wave recording the cumulative count as of
that wave) the difference `Δ = c(t_next) − c(t)` SHALL yield that dyad's `Δ`
indistinguishable latent events for the interval, carrying no flavor and deriving no
support constraint; `Δ < 0` SHALL abort as a data error, monotonicity being what makes
the count diff exact. The wave grid SHALL be inferred from the data as the distinct times of the modeled
panel layers' rows (an exogenous panel covariate contributes no boundary), unless the
user supplies an explicit `wave_times` grid. When an observation start (`start_time`)
is given, the intervals SHALL be anchored on it, so the **first wave is the right
endpoint of the first modeled interval**, never a history boundary: that interval's
entering state is the initial condition (the data object's starting network, or zero
counts where the layer has no prior history), and M waves yield M modeled intervals
over M+1 boundaries. When no observation start is given, the first recorded wave SHALL
open the grid and its snapshot SHALL be the initial condition, so M waves yield M−1
modeled intervals.
All
panel-semantics layers SHALL share one wave grid, the node set SHALL be constant
over the period, each wave snapshot SHALL be complete (no `NA` tie values — an
incomplete snapshot SHALL abort before diffing rather than be coerced to
tie-absence; missing-data waves are a recorded future goal), and an observed
relational event falling exactly on a wave time
SHALL belong to the earlier interval (preceding the snapshot). Likewise a panel
change-list row stamped at a wave time SHALL be part of that wave's snapshot, and the
wave snapshots the diff reads SHALL be the same snapshots any other consumer of the
wave grid reads (in particular the per-period counts a completed rate is pinned from),
so a pinned period and the interval it pins always count the same flips. The diffing function
SHALL be usable standalone on a validated data object. The diff output SHALL be
compiled once, at estimation entry, into a shared `augmentation_recipe` — a θ-free,
sequence-free plan holding the per-interval flip sets, the per-dyad ordered chains,
the injective flavor map (validated exactly once, here), the risk-set membership
universe (which dyads flip and their chain order; live support applicability stays in
the walk handle), and the same-wave pair/constraint graph — which every augmenter reads
while holding only a mutable per-draw cursor over it, never mutating the shared plan.
The plan SHALL also cache the per-wave start states (identical across all
endpoint-hitting sequences, θ-free), materialized once and shared by every sequence's
re-preprocess rather than reconstructed per draw. This restriction to a single shared
wave grid is a v1 bound; multiple/nested panel grids are a recorded future development. Every augmented sequence for an interval MUST consist of flip
events that transform the interval's start state exactly into its end state
(endpoint-hitting), with event times strictly inside the interval boundaries and
same-dyad chain order respected.

#### Scenario: two waves diff into flips
- **WHEN** wave t has ties {(1,2)} and wave t+1 has ties {(1,2 absent), (3,4)}
- **THEN** the candidate set for the interval is one dissolution (1,2) and one creation
  (3,4).

#### Scenario: a row stamped at a wave belongs to that wave's snapshot
- **WHEN** a panel layer records a creation as a change-list row stamped exactly at
  the first wave time
- **THEN** the flip is in the first interval's candidate set, and a completed rate
  pinned on the same wave grid counts it in the first period (its first plateau is
  finite, not a zero-hazard pin).

#### Scenario: incomplete wave aborts before diffing
- **WHEN** a panel-semantics layer's snapshot at a wave carries `NA` tie values
- **THEN** wave diffing aborts with an informative error requiring complete snapshots,
  rather than treating the missing entries as tie-absence and emitting spurious flips.

#### Scenario: count wave difference yields a multiset
- **WHEN** a count panel layer records 3 calls on dyad (1,2) at one wave and 7 at the
  next
- **THEN** the interval's candidate multiset holds four indistinguishable call events on
  that dyad, with no flavor and no derived support constraint, and every dyad remains at
  risk throughout.

#### Scenario: a decreasing count aborts
- **WHEN** a count panel layer records a lower cumulative count at a later wave
- **THEN** wave diffing aborts as a data error rather than emitting negative or
  compensating events; a layer whose value genuinely decreases is a state or ordinal
  panel, not a count panel.

#### Scenario: the first wave is modeled, not history
- **WHEN** a panel layer's first wave falls after the resolved observation start
- **THEN** the interval from the observation start to the first wave is diffed and
  augmented like any other, and modeled relational events inside it contribute
  likelihood terms; only events before the observation start are burn-in.

#### Scenario: without an observation start the first wave opens the grid
- **WHEN** a panel layer records waves at 5 and 10 and neither `start_time` nor
  `wave_times` is given
- **THEN** the wave grid is (5, 10): the wave-5 snapshot is the initial condition and
  one interval is augmented.

#### Scenario: multi-step change decomposes into an ordered chain
- **WHEN** a dyad's value changes 0→2 between waves under ±1-step transitions
- **THEN** the flip set holds two events on that dyad (0→1 then 1→2) with a forced
  order that every augmenter respects.

#### Scenario: endpoint-hitting holds by construction
- **WHEN** an augmenter returns a sequence for an interval
- **THEN** applying the sequence to the start state reproduces the end state exactly;
  this holds by construction and is asserted in tests and an opt-in debug path, not
  re-checked per draw on the production hot path.

#### Scenario: augmentation recipe compiled once and shared
- **WHEN** `estimate_dynes()` opens a panel specification whose augmenters run over
  many EM iterations
- **THEN** the wave-diff-derived flip sets, per-dyad chains, injective flavor map,
  risk-set universe, and pair/constraint graph are compiled once into the shared
  `augmentation_recipe`, the injective map is validated exactly once, and each
  augmenter reads that static plan through its own mutable per-draw cursor without
  re-diffing per sequence or per iteration.

### Requirement: Augmenter contract with three built-in variants

The package SHALL define an augmenter contract: an augmenter is an object built once
by its constructor, which closes over the shared compiled `augmentation_recipe`
(per-interval flip sets, per-dyad chains, injective flavor map, risk-set universe,
pair/constraint graph and per-wave start states — θ-free, built once at estimation
entry), so the data window is bound at construction rather than passed per call. The
object SHALL provide `draw(iteration, draw_index, theta)`, returning for each entry of
`draw_index` one endpoint-hitting sequence as `list(data, log_proposal, restarted)` —
`data` the drawn stocnet (the panel layer's rows retimed, its change list
chronological, and `info` identical to the specification's data), `log_proposal` the
finite log proposal density of the full generative path, and `restarted` whether a
reject-and-restart fired — and MAY provide `init(proposal_evaluator, theta)`, called
once per EM iteration with the θ-closed proposal evaluator, and `diagnostics()`,
whose return is recorded in the EM trace. Each draw SHALL depend only on the random
number stream it is run on, never choosing or reading its own substream key. The
package SHALL provide three constructors: `augment_seq_random()`
(iid-uniform times per interval, same-dyad chains ordered by within-chain sorting; being
otherwise **support-blind**, under cross-dyad support coupling — the same regime that
strands `sim` — the sorted ordering can fire a flip without applicable support, so the
draw SHALL be checked for support-legality and an illegal ordering **rejected and
redrawn** until legal, the recorded proposal density carrying the `− log P(legal)`
normalizer, with P(legal) the θ-free legal-ordering fraction estimated by the redraw
acceptance rate — of a pilot run once when the augmenter is built, so every draw records
the same value — and ≡ 1 on any coupling-free spec; because this normalizer is a
θ-free pool-wide constant it cancels in the normalized importance weights, so unlike
`sim` the redraw introduces **no** plug-in-normalizer bias and SHALL NOT emit the
coupling bias warning — its purpose is to keep `f = 0` / walk-erroring draws out of the
pool; a restart-count ceiling SHALL abort naming the coupled constraint when a legal
ordering is vanishingly likely),
`augment_seq_sim()` (constrained sequential simulation from the model at `theta` as an
external driver of the `multi-process-walk` handle — running as **supplied steps on the
`process-simulation` drawing core** rather than a loop of its own: it SHALL supply that
core's `mark`, `clock` and `accept` steps and SHALL NOT supply its `evaluate` step, so
every rate and choice it draws from is the package's own evaluator at `theta`, the
identity the proposal/target density pin below requires. Wave-endpoint conditioning:
sender–flavor risk sets
restricted in R to support-applicable remaining events plus the single globally-next
unplaced modeled relational event; receivers among remaining observed receivers;
waiting times at the selected pair's rate, truncated at the next anchor — the earlier
of the next relational event and the wave end — and censored at the walk's next
self-applied breakpoint before it (an exogenous row, a queued window expiry, a
composition change): a draw reaching the breakpoint advances the walk to it and the
same pair redraws at the rate held there, so the frozen-rate draw never spans a time at
which the rate changes; if the
risk set empties with flips
still unplaced — possible only under cross-dyad support coupling, where a greedy step
disables another latent flip's support before it is placed — the interval draw SHALL
**reject-and-restart** — redrawing only that interval, over the earlier intervals as
already drawn — drawing until the interval completes, so the delivered sequence is
success-conditioned and its recorded proposal density is **restart-normalized** (the
accumulated per-step density divided by the draw's completion probability, which SHALL be
estimated by the restart acceptance rate — completed draws ÷ attempted draws — recorded per
draw with its reference parameters, and which is exactly 1 on any coupling-free spec where
no draw ever strands); the loop SHALL emit **exactly one** deduplicated warning per
estimation run — never per iteration or per draw — the first time any interval restarts,
noting the resulting importance weights may carry a plug-in-normalizer bias and naming the
coupled constraint; a restart-count ceiling SHALL abort with a diagnostic naming the
coupling rather than loop when completion is vanishingly likely), and
`augment_seq_mcmc()` (one serial global
chain over the whole sequence; within-wave permute and shift moves mixed by the
user-facing `move_probs`; rate-based truncated-exponential time proposals frozen at
the state after the preceding panel event of the sequence the move starts from — the
forward rates read on the current sequence, the reverse rates on the candidate;
burn-in and thinning counted in sweeps;
moves violating same-dyad chain order excluded before proposal, while *cross-dyad*
support-illegal proposals SHALL NOT be excluded from the proposal set — doing so would
make the valid-move set state-dependent and break the pick-probability cancellation the
acceptance ratio relies on — but SHALL instead be detected by a support lookup and
rejected via the MH ratio as `f(Ω′) = 0 ⇒ α = 0`, a normal (rejected) step on the chain
requiring **no** redraw normalizer, the lookup sparing the doomed proposal's preprocess
and the walk's not-guaranteed-−Inf-on-illegal-transition hazard). The MCMC augmenter
SHALL receive an injected proposal-evaluator closure providing each candidate's
log-likelihood and the named-pair rates its reverse densities need, and, on request
without the log-likelihood (no preprocess), the named-pair rates of the current
sequence its forward draws need before the candidate exists; it SHALL NOT
call the pool evaluator directly. Augmenters SHALL NOT compute importance weights;
they report proposal densities for the evaluator. New augmenter variants SHALL be
addable without modifying the ABEM loop (selection by contract, mirroring the writer
strategy).

Augmenters SHALL NOT break ties among observed events with a rule of their own, nor
sample their order: the only tie an augmented sequence can carry is between two observed
relational events (a sampled panel event never equals an anchor, by the strictly-inside
time rule; continuous panel-event draws tie with probability zero), and such a tied pair
is forced adjacent by construction. Their mutual state-fold order SHALL be taken from the
observed event stream (the order the `tied-event-times` change makes explicit and
user-controllable), and the augmenter (`q`) and the evaluator (`f`) SHALL fold state over
tied observed events in the identical order, so the importance weight stays consistent
regardless of which order the input carries.

#### Scenario: random augmenter yields valid sequences
- **WHEN** `augment_seq_random()` draws 10 sequences for an interval
- **THEN** each is endpoint-hitting with times inside the interval, chain order
  respected, and orderings vary across draws.

#### Scenario: random augmenter redraws a support-illegal ordering
- **WHEN** `augment_seq_random()` draws an ordering that, under cross-dyad support
  coupling, fires a flip whose support is not yet applicable
- **THEN** the ordering is rejected and redrawn until legal (so every delivered draw is
  support-legal and no `f = 0` / walk-erroring sequence enters the pool), the recorded
  proposal density carries the `− log P(legal)` normalizer with P(legal) the θ-free
  legal-ordering fraction estimated by the redraw acceptance rate, and — because that
  normalizer is a θ-free pool-wide constant that cancels in the normalized weights — the
  coupling bias warning is NOT emitted for `random`; a coupling-free spec redraws never
  (P(legal) ≡ 1) and exceeding the restart ceiling aborts naming the coupled constraint.

#### Scenario: simulation augmenter drives the walk handle
- **WHEN** `augment_seq_sim()` draws a sequence at parameters `theta`
- **THEN** each next event is drawn from the model's rates/choices obtained by
  `walk_evaluate()` at the process state after the previously injected event, and the
  recorded log proposal density includes every selection step — relational-event
  selections included.

#### Scenario: a rate change inside the interval truncates the draw
- **WHEN** `augment_seq_sim()` draws a waiting time in an interval whose next
  change is a window expiry or an exogenous covariate row before the next
  relational event and before the wave end
- **THEN** the draw is censored at that breakpoint rather than bounded only at
  the relational event: a draw reaching it advances the walk to it and the same pair
  redraws at the rate the walk holds there, truncated at the relational event or wave
  end, so the accumulated proposal density is a product of exact per-segment densities
  (survival up to each breakpoint, then the truncated density) at the same rates the
  evaluator's compensator integrates.

#### Scenario: simulation augmenter restarts a stranded draw
- **WHEN** `augment_seq_sim()`'s greedy forward draw empties the risk set with flips still
  unplaced (cross-dyad support coupling)
- **THEN** the interval draw is rejected and restarted until it completes, the delivered
  sequence is endpoint-hitting, and its recorded proposal density is restart-normalized —
  divided by the completion probability estimated as the restart acceptance rate (completed
  ÷ attempted draws) recorded with its reference parameters — so the importance weight uses
  the success-conditioned proposal; exceeding the restart ceiling aborts with a diagnostic
  naming the coupling.

#### Scenario: coupling emits exactly one warning per run
- **WHEN** an estimation run performs any restart in any interval at any iteration
- **THEN** exactly one deduplicated warning is emitted for the whole run, the first time a
  restart occurs, noting the plug-in-normalizer bias the success-conditioned proposal may
  carry and naming the coupled constraint — never one warning per iteration or per draw; a
  coupling-free run (no restart ever fires) emits no such warning.

#### Scenario: mutation preserves validity
- **WHEN** `augment_seq_mcmc()` proposes a move on a valid sequence
- **THEN** the proposed sequence is endpoint-hitting (moves stay within one wave and
  respect chain order) and the forward/reverse truncated-exponential time densities
  are reported for the acceptance computation.

#### Scenario: mcmc rejects a cross-dyad support-illegal proposal
- **WHEN** `augment_seq_mcmc()` proposes a permute whose swap fires a flip without
  applicable support (cross-dyad support coupling)
- **THEN** the proposal is not excluded from the proposal set (so the pick-probability
  cancellation is preserved) but is detected by a support lookup and rejected via the MH
  ratio as `f(Ω′) = 0 ⇒ α = 0` — the chain stays at the current state, counted as an
  ordinary rejected step in the sweep, with no redraw normalizer applied and the doomed
  proposal's preprocess spared.

#### Scenario: mcmc reads forward rates without a likelihood pass
- **WHEN** `augment_seq_mcmc()` draws a move's times from the current sequence
- **THEN** it reads the moved pairs' rates on that sequence through the injected
  closure with the log-likelihood skipped (no preprocess, the chain already holding
  `f(Ω)`), and reads the reverse rates on the candidate in the same call that scores
  `f(Ω′)`, each at the state after the preceding panel event of its own sequence.

#### Scenario: single-event intervals still mix
- **WHEN** a between-wave interval contains exactly one panel event
- **THEN** shift moves redraw its time (its position relative to relational events
  can change), so the chain does not freeze that interval.

#### Scenario: tied observed relational events fold identically in augmenter and evaluator
- **WHEN** two observed relational events share a timestamp inside an interval and a
  sequence carrying them is both drawn by an augmenter and scored by the evaluator
- **THEN** both fold state over the tied pair in the order the observed event stream
  carries (neither invents a tie-break nor samples the order), the pair is placed
  adjacent with no sampled panel event between them, and the augmenter's proposal
  density and the evaluator's model likelihood agree (within 1e-10) on that ordering so
  the importance weight is unaffected by which input order the tie carries.

### Requirement: Batched pool evaluation with per-sequence sugar

The evaluator SHALL compute, for a pool of augmented sequences at a parameter vector,
the requested per-sequence quantities — log-likelihood, score, and/or Fisher
contribution, selected by a `what` request flag so callers pay only for what they
consume (the SGD loop runs score-only; Fisher is computed at convergence and for
opt-in trace SEs) — through the compiled engine (the existing C++ backend that
decodes each sequence's flat broadcast statistics without materializing the dense
statistics stack; zero optimizer iterations), called once per sequence over its flat
preprocessed objects, with no separate batching kernel. Importance weights are formed
from model density over proposal density by the estimation loop from these
per-sequence quantities and the stored reference records.
For the result contract's asymptotic uncertainty the evaluator SHALL expose, per
sequence, **both** the complete-data score and the complete-data information (not Fisher
alone), since those are the ingredients the estimation loop combines into **Louis's
observed-data information** (`louis-observed-information` D1, Louis 1982:
`I_obs = Σ w̄ᵢ Jᵢ − (Σ w̄ᵢ SᵢSᵢᵀ − S̄S̄ᵀ)` with `S̄ = Σ w̄ᵢ Sᵢ`, `vcov = I_obs⁻¹` — the
complete information needs the per-sequence information, the missing information
subtracted from it needs the per-sequence scores); exposing only the complete-data
information would leave the missing information unformable and force an
anti-conservative complete-data inverse `vcov()`.
*Amended 2026-10-05 (`louis-observed-information`):* this paragraph previously cited
abmcem D9's sandwich `I_c⁻¹ (Σ w̄ᵢ SᵢSᵢᵀ) I_c⁻¹` as the consumer. That form's middle term
is the missing information, not a sampling variance, and its SEs were 50× or more too
small against a closed form; the covariance is now Louis's. The score-and-information
obligation on the evaluator is unchanged; only the formula it feeds moved.
The evaluator SHALL return the requested per-sequence quantities (log-likelihood,
score, information) together with a per-sequence degeneracy flag, and SHALL NOT depend
on the weighting scheme; the scheme-classed E-step object (`goldfishEstepIS`,
`goldfishEstepResampling`, or `goldfishEstepUniform`, sharing a `goldfishDynesEstep`
parent) on which the `compute_q()` / `compute_ase()` generics dispatch is built from
those quantities by the estimation loop specified by the `abmcem` change.
The batched evaluator SHALL NOT compute named-pair rates at named positions: the
reverse-density rates the MCMC augmenter's proposal evaluator consumes are read from
the walk handle by the `rate-queries` capability, so they are the same intensities the
time proposals draw with, never a second evaluation path over preprocessed statistics.
Each pooled sequence SHALL permanently carry its reference record (the parameters it
was drawn under, its log-likelihood there, and its log proposal density), kept on the
log scale, so cross-iteration reweighting is a likelihood ratio, never a re-draw.
The package SHALL export `compute_lik_seq(spec, sequence, theta)` as per-sequence sugar
over the same batched path (named to avoid the `logLik()` S3 collision). Each drawn
sequence SHALL be fully preprocessed through the existing recipe path (no incremental
patching), except that the per-wave start states — identical across all
endpoint-hitting sequences and θ-free — SHALL be materialized once and shared rather
than reconstructed per drawn sequence; rate/probability evaluation at a given state
SHALL reuse the estimation kernels. Every target-density path (pool entry, the MCMC
proposal evaluator, the warm start) SHALL preprocess over the observation window from
the resolved observation start to the last wave, so each timed rate's trailing
censored interval up to the last wave enters the likelihood, and a completed rate's
interval spanning an interior wave SHALL be scored at each wave period's own plateau.

#### Scenario: batched evaluation matches per-sequence estimation
- **WHEN** a pool of 10 sequences is evaluated at `theta`
- **THEN** each sequence's log-likelihood equals (within 1e-10) the existing engine's
  zero-iteration evaluation of the equivalent single-sequence model at `theta`.

#### Scenario: weights live in the evaluator
- **WHEN** a pool drawn by `augment_seq_random()` is evaluated under the model
  density at `theta`
- **THEN** each sequence's importance weight is the model-to-proposal density ratio
  computed by the evaluator, and weights normalize over the pool.

#### Scenario: evaluator output is scheme-agnostic
- **WHEN** the same pool is evaluated at the same parameters under importance
  weighting and under resampling
- **THEN** the evaluator returns identical per-sequence quantities in both cases, and
  the scheme-classed E-step object the `compute_q()` / `compute_ase()` generics
  dispatch on is built from them by the estimation loop.

#### Scenario: the trailing interval reaches the last wave
- **WHEN** a pooled sequence's last event falls before the last wave time
- **THEN** each timed rate's likelihood includes the censored interval from that event
  to the last wave, so moving the last event changes the log-likelihood by the
  corresponding exposure term.

#### Scenario: evaluator exposes both score and information for the observed information
- **WHEN** Fisher is requested at convergence for a weighted pool
- **THEN** the evaluator returns, per sequence, the complete-data score **and** the
  complete-data information (not information alone), so the estimation loop can form
  Louis's observed information `Σ w̄ᵢ Jᵢ − Σ w̄ᵢ SᵢSᵢᵀ + S̄S̄ᵀ` and `vcov()` is not the
  anti-conservative complete-data inverse.

### Requirement: Pool storage stays memory-bounded

The pool SHALL be held as an in-memory list of flat preprocessed objects by default,
with each between-wave window's start state (and any start-state-derived engine
object) held once per process and referenced by every sequence rather than copied
into each, so a pooled sequence's marginal memory grows with its path events and not
with the number of actors, and with **no fixed byte threshold**: the acceptance bound is set empirically by the
Phase-1 profiling spike on the broadcast (`stat_mat_update` + pointers) representation,
measuring the pool footprint over the n × events × K grid, rather than by a constant
(the earlier ~2 GB / ~5 GB figures referred to a superseded heavier representation).
No user-facing cap argument is exposed; the spill fallback is the whole memory story.
If the measured budget is exceeded, the fallback SHALL reuse existing machinery (a
broadcast-aware on-disk variant of the default format, or the DBI writer) rather than
introducing a new storage format; the chosen strategy and its measurements SHALL be
recorded in the change's design before the evaluator phase is implemented.

#### Scenario: start state shared across the pool
- **WHEN** a pool of sequences over the same waves is built
- **THEN** each window's start state is stored once per process (every sequence refers
  to the same object), and a sequence's marginal memory stays within 1 KB per path
  event.

#### Scenario: profiled decision recorded
- **WHEN** the Phase-1 memory spike completes
- **THEN** the design records wall/RSS measurements over the (n × events × K) grid and
  the storage decision they imply, and the implementation follows it.
