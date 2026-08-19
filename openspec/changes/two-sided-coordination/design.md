# Design: two-sided-coordination

## Context

The published DyNAM-coordination model (Stadtfeld, Hollway & Block 2017) is
the conjunctive member of Snijders & Pickup's (2017) five two-sided choice
mechanisms. `.plan/sp/undirected_coordination_marked_ph.md` derives all five
as marked point processes λ_kl = h₀(t)·φ_kl with a common baseline: the mark
kernel q_kl = φ_kl/Σφ_ab is independent of h₀, so the Cox partial likelihood
over marks estimates every parameter appearing in φ — including the rate
parameters β when rates vary by actor — from event ordering and risk sets
alone. Constant rates recover the published special cases (conjunctive
collapses to the current `choice_coordination` likelihood).

The current surface reaches conjunctive coordination as
`estimate_dynam(sub_model = "choice_coordination")` (released at v1.7.0) and
`make_specification(choice_sub_model = "choice_coordination")` (dev-line
only). Interview decisions (2026-08-19, ADR-0022/0023): dedicated estimator
`estimate_dynamu()` as the three-part container; `make_specification()` stays
out; Cox-only timing with no `distribution` argument; full joint estimation
for all five mechanisms; deprecate_warn redirect for the released token.

Post-merge context (1.9.29, `feature/dynes` merged 2026-08-19): the DyNES
substrate is now load-bearing on the `choice_coordination` token in exactly
the surface this change empties. The living spec `multivariate-specification`
REQUIRES that "DyNAM (rate, choice, choice_coordination) and REM processes,
timed or ordered, MAY be freely mixed" in `make_joint_specification()`, its
generative-readiness completion fills "a missing `choice_coordination`" with
a uniform draw on both sides (`R/complete_generative_spec.R`), and the walk
handle dispatches generation on `sub_model == "choice_coordination"`
(`R/walk_handle.R`). Deleting the token from `make_specification()` as
written would sever coordination processes from the DyNES composition path;
resolved by D14–D16 (option (a) of the review, chosen 2026-08-19; ADR-0024,
superseded the same day by ADR-0029): coordination reaches
`make_joint_specification()` as a `make_specification(model = "DyNAMu")`
object — one constructor, one more model.

## Goals / Non-Goals

**Goals:**
- One estimator for undirected two-sided relational events: three formula
  parts (choice positional, `rate =`, `acceptance =`), `mechanism =` selector,
  conjunctive default backward-compatible to 1e-6.
- Mark/partial-likelihood estimation for all five mechanisms; joint (β, θ, α)
  when a rate formula is present.
- Deprecation redirect from `estimate_dynam(sub_model =
  "choice_coordination")`; `make_specification()` token deleted.
- r/cpp backend parity for every mechanism.

**Non-Goals:**
- Parametric waiting-time baselines for coordination (no `distribution`
  argument at all — see D4/ADR-0023).
- Flavored lists on `estimate_dynamu()`'s direct formula surface (flavored
  coordination goes through `make_specification(model = "DyNAMu")`).
- Mixed timed+ordered joint compositions (single-regime stands; the mixed
  alternative is recorded in ADR-0030 for discussion with Maria).
- Frailty/random effects; auxiliary proposal data; DyNAMi.
- The full Grambsch–Therneau / sup-norm diagnostic suite for the new
  mechanisms (per-event scores are stored like other fits; dedicated
  coordination diagnostics are a follow-up).

## Decisions

### D1 — A dedicated estimator, named `estimate_dynamu()`
The two-sided model leaves `estimate_dynam()`: with actor-varying rates the
parameters are estimated jointly from one likelihood, so neither `sub_model =
"rate"` nor `"choice*"` names the fit honestly, and the mechanism axis would
otherwise multiply tokens. Name chosen by the user: `dynamu` (u = undirected),
parallel to `estimate_dynami()`'s suffix form. *Alternatives rejected*:
`estimate_coordination()` (clean but drops the DyNAM lineage),
`estimate_dynam_coordination()` (27 chars), `estimate_two_sided()` (abstract,
collides mentally with two-sided tests). (ADR-0022.)

### D2 — Three-part container signature
`estimate_dynamu(x, rate = NULL, acceptance = NULL, mechanism =
c("conjunctive", "forcing", "confirmation", "disjunctive", "compensatory"),
data, ...)` where `x` is the choice formula (the part every mechanism needs).
`rate` absent ⇒ constant rates (rate factor cancels from the mark kernel).
`acceptance` is only legal with `mechanism = "confirmation"` — supplying it
elsewhere aborts (inert arguments are signaled, ADR-0007); omitting it under
confirmation aborts too. No `sub_model` argument exists. *Alternative
rejected*: all-named formulas (loses the `estimate_*(formula, ...)` family
shape and the one-liner constant-rate case).

### D3 — `make_specification()` carries DyNAMu as a model (revised 2026-08-19)
Originally "make_specification() stays out" on the pollution argument
(ADR-0022). Revised after the dynes merge made the specification object the
composition currency: `make_specification()` gains `model = "DyNAMu"` with
`mechanism =` and `acceptance =`, both aborting when supplied for any other
model — the pollution is two arguments policed by ADR-0007 aborts, smaller
than the cost of a lone sibling constructor (see D14 / ADR-0029). What
survives of the original decision: `estimate_dynamu()` remains the only
*fitting* surface, its direct path stays simple formulas, and the dev-line
`"choice_coordination"` token is still deleted from `choice_sub_model` —
`model = "DyNAMu"` replaces it as the coordination vocabulary.

### D4 — Cox-only timing; no `distribution` argument (ADR-0023)
Under Cox the partial likelihood over marks estimates all of (β, θ, α), and
compensatory's C(t) normalization is dyad-common so it cancels — no
convention needs fixing. A parametric waiting-time model would model the
*observed* inter-event times while the mechanisms thin *unobserved* rejected
proposals; whether that combination is coherent (or needs augmentation) is an
unresolved theoretical question. Until it is resolved we do not open the
floor: the argument does not exist, rather than existing and aborting.

### D5 — Joint estimation only; no two-stage route
With `rate =` present, β enters the mark kernel (additively for
conjunctive/disjunctive/compensatory via log(ρ̃_k + ρ̃_l); as mixture weights
for forcing/confirmation) and everything is maximized jointly. Two-stage
(rate first, choice after) is inconsistent and is not offered even as an
option; the derivation's initializer role for it may be used internally.

### D6 — Generic score/Hessian assembly with per-mechanism g-functions
One assembly (score = ∂g_obs − E_D[∂g]; Hessian = ∂²g_obs − E_D[∂²g] −
Cov_D(∂g)) consumes per-mechanism implementations of g and its first/second
derivatives, built from four primitives (multinomial moments, Bernoulli
acceptance, rate-sum, log-sum mixture rule). This keeps the five mechanisms
one file of g-functions rather than five estimators.

### D7 — Estimation safeguards and identifiability monitoring
Damped Newton with step-halving; outer-product (OPG) information substituted
when the observed information loses definiteness (only compensatory under
constant rates is globally concave). The fit monitors the known
identifiability weaknesses — β needs mixed dyads (x_k ≠ x_l); mixture-variant
β is identified only through initiator mixing; (β, θ) ridges under homophily
— and reports non-convergence with the suspected cause rather than looping.

### D8 — Predictability discipline (t⁻) is inherited, and asserted
All statistics and weights evaluate at left limits; simultaneous events are
strictly ordered by the existing preprocessing. This is the content of the
martingale proof, not a convention — a test asserts that an event's own
update cannot feed its own evaluation.

Cross-reference (2026-08-19): the coordination-mechanisms spec's t⁻
requirement is the canonical wording; the `recency-effects` change
inherits it for its rank-buffer reads rather than restating it. Recency
statistics reaching the coordination side flow through the directed
p_ij like any other statistic — nothing in the two-sided derivations
changes.

### D9 — r backend reference first, cpp port parity-gated
The r backend implements the mechanisms as the readable reference (validated
by simulation recovery); the cpp engine ports them for speed; parity tests
compare per-event contributions and final fits (backend-primitive-parity
discipline). The conjunctive constant-rate path must reproduce current
`choice_coordination` coefficients to 1e-6 — that bridge is the regression
gate for the refactor.

### D10 — Deprecation: redirect the released token, delete the dev-line one
`estimate_dynam(sub_model = "choice_coordination")` warns via
`lifecycle::deprecate_warn()` (single-hop message pointing at
`estimate_dynamu()`, `with =` set) and forwards to the new estimator so
results keep flowing; removal ≥3.0.0 per the naming-deprecations policy.
`make_specification()`'s token is dev-line-only: deleted outright, NEWS
records it.

### D11 — Result object: one joint fit with labeled parameter blocks
A single fitted object (class per the class-naming scheme — ADR-0031,
`goldfish<Thing>` camelCase; updated 2026-08-19 from the superseded
`_goldfish`-suffix wording) whose coefficient vector is labeled by
block (rate / choice /
acceptance); `summary()` prints the blocks separately (cli-rendered);
`coef()`/`vcov()`/`logLik()` cover the joint fit. Not the flavored
multi-block container: these parameters share one likelihood and one
information matrix, unlike independent flavor blocks (contrast ADR-0002's
case, where the likelihood factorizes).

### D12 — Mechanism vocabulary is Snijders–Pickup's
`c("conjunctive", "forcing", "confirmation", "disjunctive", "compensatory")`
— the literature's own terms, so the argument reads against the paper.
`"confirmation"` (not `"initiative_confirmation"`) keeps the token short; the
docs give the full name.

### D13 — Delta against post-parametric-rates specs
This change's `model-specification` delta is written against the wording
`parametric-rates` leaves behind (both changes modify the same requirement).
`parametric-rates` archives first; if the order flips, reconcile this delta
before syncing (the placement check verifies headers, not content).

## Risks / Trade-offs

- [Likelihood ridges / weak β identification] → D7 monitoring + documented
  simulation-recovery checks; docs state the mixed-dyad requirement.
- [Wrong-mechanism misfit reads as time-varying effects] → docs pair residual
  reading with AIC comparison across (non-nested, equal-count) mechanisms.
- [Silent behavior drift on the conjunctive path] → the 1e-6 bridge test and
  the frozen baselines gate every commit (not-cran-test).
- [Mixture-variant Hessians indefinite] → OPG fallback + damping (D7).
- [Redirect creates a second entry point] → redirect is thin (argument
  translation only), tested, and carries a removal horizon.

## Migration Plan

Released users: `estimate_dynam(sub_model = "choice_coordination")` keeps
working with a deprecation warning naming `estimate_dynamu()`; NEWS shows the
one-line migration. Dev-line users: `make_specification()` token gone; NEWS
records it. Rollback: the redirect and the new estimator are additive; the
conjunctive engine path is shared, so reverting the surface does not touch
the numbers.

### D14 — Coordination is a model of the one `make_specification()` (revised 2026-08-19)
No dedicated constructor. `make_specification()` — already the multi-model
constructor (`model = c("DyNAM", "REM", "DyNAMi")`) — gains `model = "DyNAMu"`
plus the two coordination arguments, `mechanism =` and `acceptance =`, which
abort with a cli error when supplied for any other model (and `acceptance`
stays gated to `mechanism = "confirmation"` in both directions). The `choice`
formula slot carries the coordination choice part, so **flavored coordination**
(`choice = list(creation ~ ..., dissolution ~ ...)` — creation and dissolution
of coordination ties, the published use case) inherits the entire flavored
machinery for free. `estimate_dynamu()`'s direct surface stays **simple
formulas only**; flavored or composed work goes through the specification,
which `estimate_dynamu()` accepts like its siblings accept theirs — and
`estimate_dynam()` on a DyNAMu specification redirects to `estimate_dynamu()`
(the same pattern as event-stream estimators rejecting a joint specification
by naming `estimate_dynes()`). `make_joint_specification()` needs **no new
acceptance path**: a DyNAMu process is a `make_specification()` object like
any other. (ADR-0029, superseding ADR-0024's dedicated
`make_dynamu_specification()`; narrows ADR-0022's "stays out entirely" to
"the estimator is the only *fitting* surface".) *Alternatives rejected*: the
lone sibling constructor (breaks the constructor's multi-model pattern, and
every substrate consumer — composer, completion, walk — must learn a second
object shape and rebuild flavor lists); one constructor per model
(four exports and a deprecation of a surface that just stabilized, for no
consumer benefit); the `choice_coordination` token surviving internally (two
vocabularies for one model family); excluding coordination from composition
(shrinks a substrate promise the dynes merge just landed).

### D15 — Composition accepts every mechanism; consumers gate
`make_joint_specification()` composes a DyNAMu specification of any mechanism
— the substrate's own rule is that construction never owns viability. Each
consumer gates what it can handle: the completion transform fills a
coordination process's missing choice half with a uniform **conjunctive**
draw on both sides (byte-equivalent to today's default, restated in mechanism
vocabulary); the walk/`simulate()`/augmenter engines and `estimate_dynes()`
initially generate conjunctive only, aborting on the other four mechanisms
with a cli error naming the mechanism and the limitation. The gate is cheap
to lift later: `walk_engine_model_type()` shows coordination is a dyad engine
whose mark kernel is the single mechanism-specific point, and the derivation
note's §7 gives each mechanism its generative thinning construction.

### D16 — Regimes in composition: single-regime for now, ordered is coordination's native home (revised 2026-08-19)
A joint composition keeps `make_joint_specification()`'s existing
single-regime rule (mixing timed and ordered processes rejects at join
time). Within that rule a DyNAMu process composes in **either** regime: in an
**ordered** composition it is Cox-native — no clock, no pinned rate, the
partial likelihood evaluates on the (augmented) ordering; in a **timed**
composition an explicit rate formula becomes the pair-opportunity clock
(ρ̃_k + ρ̃_l) on the shared exponential clock, and a missing rate completes
to the pinned intercept-only rate like any other flavor. Neither reading
contradicts ADR-0023: the thinning obstacle is an *estimation* problem,
whereas generation simulates the latent proposal process directly — the
opportunity clock IS the derivation's §7 thinning construction. The deeper
point (Alvaro, 2026-08-19): the DyNES samplers place augmented events **into
the sequence of observed events**, and each process then contributes its own
likelihood over the augmented sequence — ordering is the primitive,
timestamps are extra information some likelihoods use. That makes a **mixed**
timed+ordered composition possible in principle ("every model uses what it
needs"); it stays a recorded alternative to discuss with Maria (ADR-0030),
owned by the substrate (`multivariate-specification` /
`dynes-augmentation`), not this change. Parametric-rates D13's regime rules
(weibull/gompertz rejected in composition) apply unchanged.

### Cross-change coordination (active, not-yet-applied changes this touches)
- `process-simulation` (0/10): enumerates `choice_coordination` in its family
  list and simulates it by mutual-choice rejection — that is the conjunctive
  mechanism only. The other four mechanisms have their own thinning
  constructions (the derivation note's §7 derives all five generatively);
  process-simulation should key its coordination DGP by mechanism, and this
  change's simulation fixtures (task 2.5) are its natural seed.
- `coordination-tie-consistency` (0/10): defined on `sub_model =
  "choice_coordination"`; its subject migrates to `estimate_dynamu()` — its
  proposal wording needs the new surface when taken up.
- `review-rem-directed`: its D11 symmetric support mask for coordination
  applies unchanged to `estimate_dynamu()` risk sets.
- `gather-rem-coordination-format` (0/14): the coordination gather-backend
  format this estimator inherits; the mechanism axis widens what that format
  must carry (per-mechanism g-derivative inputs).
- `identifiability-diagnostics` (post-release): D7's non-convergence and
  identifiability warning wording should align with it, as tied-event-times
  already notes for its own warnings.
- `effect-term-registry` (0/54): registry variants enumerate `(model,
  sub_model)` — coordination effects re-key to the dynamu surface when both
  land.

## Open Questions

- ~~BLOCKING — DyNES composition vs D3~~ **Resolved 2026-08-19, revised same
  day**: first as a dedicated `make_dynamu_specification()` (ADR-0024, now
  superseded), then folded into the one `make_specification()` as
  `model = "DyNAMu"` (D14, ADR-0029) — the constructor was already
  multi-model, and flavored coordination inherits the flavored machinery.
  The `multivariate-specification` deltas in this change carry the wording.
- Mixed timed+ordered composition ("every model uses what it needs" under
  sequence augmentation) — recorded as the alternative in ADR-0030, to be
  discussed with Maria; single-regime stands for now.
- Parametric baselines for coordination await the thinning-coherence question
  (ADR-0023) — deliberately not blocking this change. Note the merged
  substrate sharpens it: the generative walk *simulates* the latent thinning
  directly, so a parametric coordination clock may be simulable (via
  augmentation) before it is estimable.
- Dedicated coordination diagnostics (generalized Schoenfeld smoothing,
  sup-norm tests per mechanism) are a candidate follow-up change once
  `residuals-gof` phase 2 settles the shared machinery.
