## Context

Created 2026-07-20 out of `flavored-processes` section 5. Building an estimation
fixture there surfaced that `inertia` is structurally unidentifiable under a
mutually exclusive layer's derived `!tie(L)` mask, and that goldfish reports this
— and every other identifiability failure — with one guess-shaped message that
names nothing.

Two failure sites raise it: `R/estimation_core.R:507` (Newton-Raphson, R engine)
and `R/cpp_interface.R:499` (the compiled path's equivalent). Both hold
`informationMatrixUnfixed` at the moment they give up. A third site,
`R/cpp_interface.R:726`, inverts for standard errors after convergence.

The coefficient-naming path already exists (`term_label(object$names,
".coef_name", "coef")`, used by `coef()`/`vcov()`), and `flavored-processes`
added process labels rendered from the process_map. Diagnostics reuse both rather
than inventing names.

## Goals / Non-Goals

**Goals:**
- Attribute a singular information matrix to named coefficients.
- Distinguish "this effect has no variation" from "these effects are collinear".
- Surface separation, which today returns a plausible-looking fit silently.
- Report conditioning on fits that succeed.

**Non-Goals:**
- Automatically dropping or fixing offending effects — diagnosis, not repair;
  which effect to remove is the analyst's modelling decision.
- Symbolic/algebraic proof of non-identifiability from formula structure (D2).
- Any change to a successful fit's estimates, standard errors, or convergence
  path; the frozen baselines must be untouchable by construction (D1).
- Rank-deficiency handling inside the C++ likelihood itself.

## Decisions

### D1 — Failure-path and post-hoc only; the hot path is not touched
Every diagnostic runs either where estimation currently raises an error, or after
it has converged. Nothing is added to an engine inner loop, no statistic is
recomputed during a successful fit, and no code on the path a frozen baseline
exercises changes behaviour. This is what makes the change safe to land at any
point in the release train, and it is a hard constraint on every decision below —
a diagnostic that would require instrumenting the likelihood loop is out of scope
even if it would be more informative. *Rejected:* accumulating per-effect ranges
during the first likelihood evaluation, which is nearly free per event and would
let the model fail *before* the first solve with a fuller explanation — but it
edits both engines' inner loops, and the resulting baseline risk is not worth
buying an earlier version of a message the failure path can deliver anyway.

### D2 — Diagnose numerically from the matrix, not symbolically from the formula
Attribution comes from the null space of the singular information matrix (an SVD
of a p×p matrix, only on the failure path, p being the parameter count). A basis
vector concentrated on one coefficient means that effect alone is degenerate;
mass spread across several means those are collinear as a group.

*Rejected:* a symbolic rule set over (constraint atom, effect) pairs — e.g.
"`!tie(L)` implies `inertia(L)` ≡ 0". That rule is true, and it was the first
design, but it needs a hand-written case per effect family, silently misses every
pair nobody enumerated, and risks warning about valid models when a rule
over-reaches. The numerical route catches the same case plus collinearity and
no-variation uniformly, with no rule table to maintain. The symbolic insight
survives where it belongs — as documentation in the competing-processes vignette,
warning users off the trap before they hit it.

### D3 — A single degenerate effect gets a why, and the why comes from the statistics
When D2's null space implicates exactly one coefficient, the message should
distinguish two cases the user acts on differently:
- **no variation anywhere**: the statistic is constant across the whole risk set
  (the `inertia` under `!tie(L)` case — always 0). The effect cannot enter the
  model at all.
- **constant within each event's alternatives**: the statistic varies across
  events but not across the alternatives of any one event, so it cancels in a
  choice softmax. The effect is meaningful but not estimable *in a choice
  sub-model*; it may belong in the rate sub-model instead.

That second message is the one that teaches something, and it is only reachable
by looking at the statistics rather than the matrix. Since it runs on the failure
path, the cost of the extra pass is irrelevant.

### D4 — Separation is reported, never silently corrected
Detection is a post-convergence check: a coefficient whose magnitude and standard
error are both large while the log-likelihood sits at its ceiling indicates the
alternative is perfectly predicted. Report probable complete or quasi-complete
separation naming the effect, and say the estimate is not trustworthy. No
penalization, no Firth correction, no automatic refit — those are modelling
choices with their own literature and belong in a separate change if wanted.
Thresholds are heuristic and MUST be documented as such in the message.

### D5 — Conditioning is reported, not enforced
`summary()` reports the information matrix's condition number and warns above a
documented threshold. A near-singular fit that inverted successfully still has
unstable standard errors, and today nothing says so. Reporting only: no fit is
refused for being ill-conditioned.

## Risks / Trade-offs

- **A wrong attribution is worse than no attribution** — a message naming the
  wrong effect sends the user down a false trail. Mitigation: report the null
  space's actual loadings rather than a single verdict when the vector is not
  clearly concentrated, and phrase group collinearity as a set, never as a
  ranked guess.
- **Heuristic thresholds (separation, conditioning) will misfire in both
  directions** → both are warnings that name their threshold and say it is a
  heuristic; neither blocks a fit.
- **Failure-path-only means the diagnostic arrives after the full preprocessing
  and iteration cost** — accepted deliberately (D1). The failure is already late
  today; this makes it informative, not earlier.
- **Two inversion sites drifting apart** → the diagnostic is one shared helper
  called from both, not duplicated logic.

## Open Questions

- Whether the condition-number report belongs in `summary()` output by default
  or only above the warning threshold — a question about output noise, decidable
  once the numbers are visible on real fits.
- Whether separation detection should also cover the rate sub-model (where the
  analogue is an effect perfectly predicting event timing) or stay scoped to
  choice, where it is well defined.
