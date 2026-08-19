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
- `make_specification()` support for coordination models.
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

### D3 — `make_specification()` stays out
Coordination pieces (mechanism, acceptance formula) would pollute the
specification surface with arguments meaningless for every other model
(user's call). `estimate_dynamu()` is itself the multi-part container; if a
future multi-model spec needs coordination, it composes then. The dev-line
`"choice_coordination"` token is deleted from `choice_sub_model`.

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
A single fitted object (class per the class-naming scheme, `_goldfish`
suffix) whose coefficient vector is labeled by block (rate / choice /
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

## Open Questions

- Parametric baselines for coordination await the thinning-coherence question
  (ADR-0023) — deliberately not blocking this change.
- Dedicated coordination diagnostics (generalized Schoenfeld smoothing,
  sup-norm tests per mechanism) are a candidate follow-up change once
  `residuals-gof` phase 2 settles the shared machinery.
