# Design — backend-vocabulary

## Context

`set_algorithm_newton()` (R/set_opt.R, renamed from `set_estimation_opt()` by
the archived `algorithm-naming` change) carries
`engine = c("default_c", "default", "gather_compute")`. The values name
implementation history rather than behavior: `"default_c"` is the actual
default, `"default"` is the R reference implementation, `"gather_compute"` is
the gather-stack strategy. The estimation path reads the value at
`cpp_interface.R` and the writers select on it; the compiled interface has its
own engine strings, which no user sees.

Grounding for the sweep (surveyed 2026-07-25): **16 living requirements across
8 capabilities** name one of these values. They are not all the same kind of
reference —

- what a *user passes or selects* (`engine = "gather_compute"`, "on the
  `default_c` engine"): the vocabulary being renamed;
- what the *implementation is* (the C++ `default_c` estimator's BLAS staging,
  the multinomial normalizer loops): internal, and D2 below keeps those names;
- the generic word "engine" in prose ("cross-engine tolerance", "the
  formula→engine boundary"): domain vocabulary, not the argument.

Only the first kind is in scope. `optimizer-selection` (4 requirements) already
had a delta written inside `revise-gather-output`; it moves here wholesale.
`preprocess-output-writers` (2 requirements) stays with `revise-gather-output`,
which is modifying those requirements for other reasons and writes them under
the final vocabulary — so the two changes never modify the same requirement.

## Goals / Non-Goals

**Goals:**

- One descriptive, user-facing vocabulary for choosing what runs an estimation.
- Every previous call keeps working through 2.x, with one warning that names
  the final spelling.
- A living spec that describes the surface as it ships, including saying
  "both compute backends" wherever both genuinely provide the behavior.

**Non-Goals:**

- Renaming the compiled interface's internal engine tokens, or the R-level
  helper names that carry them.
- Changing which backend is the default, what any backend computes, or any
  numerical result.
- Touching `preprocess-output-writers` (owned by `revise-gather-output`) or the
  `output` vocabulary of `compute_statistics()`.
- Retiring `engine =` outright — it is a released surface; removal is a
  ≥3.0.0 concern, together with the rest of the 2.0.0 alias layer.

## Decisions

### D1 — `backend = c("cpp", "r", "gather")`, mapped at the constructor boundary

The argument becomes `backend`; the values name what actually runs — `"cpp"`
(the C++ event loop, default), `"r"` (the R reference implementation),
`"gather"` (the gather-stack C++ path). Rejected: keeping `engine` with new
values (a term of art in parsnip/knitr, but the plainer word won); `evaluator`
(exports internal vocabulary); `implementation` (no precedent in the family).

The rename stops at the constructor. `set_algorithm_newton()` resolves
`backend` to the token the compiled interface already expects, exactly as
`compute_statistics()` maps `output = "preprocessed"` onto the writers' legacy
`"default"` token. Nothing downstream of the constructor changes, so no
numerical path is touched and the C++ interface is untouched.

### D2 — Internal implementation names are kept, and that decides the sweep's edge

A requirement that describes *what the implementation does* keeps its names:
"The `default_c` REM estimators SHALL compute each event's normalizer in staged
BLAS form" is about the C++ code, whose token is unchanged. A requirement that
describes *what a user selects* is renamed: "requesting it with the
`gather_compute` engine SHALL abort" becomes "with `backend = "gather"`".

This edge is what keeps the sweep honest in both directions — renaming
implementation prose would contradict D1's boundary, and skipping user-facing
prose leaves the living spec documenting values that are deprecated sentinels.
Where a single requirement mixes both (a C++-internals requirement whose
scenario compares against the R backend), the internals keep their names and
the comparison is stated in backend terms.

### D3 — Where behavior has more than one backend, the requirement says so

Several living requirements pin a sentence to `default_c` and mention the R
implementation as an afterthought, even though both provide the behavior. Those
are restated to name both compute backends as first-class: e.g.
`return_event_scores` is "honored by both compute backends — `backend = "cpp"`
(via the evaluator flag) and `backend = "r"` (captured in its contribution
loop) — while `backend = "gather"` SHALL abort". Where a behavior genuinely
belongs to one backend (maxLik-driven optimizers run only on `cpp`; the staged
BLAS form is C++-only) the requirement stays single-backend. "When possible" is
the rule, not "always".

This is the one place the sweep changes meaning rather than wording: it makes
the R backend a documented equal where it already was one in fact, which is
what makes it usable as the reference implementation in cross-backend parity
requirements.

### D4 — Legacy values map with one warning, no two-hop

`engine =` stays as a `lifecycle::deprecated()` sentinel folded in by the
existing `fold_renamed_arg()` helper (added by `algorithm-naming`), and the
legacy *values* map (`default_c → cpp`, `default → r`, `gather_compute →
gather`). Supplying the old argument, the old value, or both produces exactly
one soft-deprecation warning, and it names the final spelling — never an
intermediate. A legacy value passed to the new argument
(`backend = "default_c"`) is also accepted and mapped, since that is the likely
half-migrated call; it warns the same way.

### D5 — Sequenced before `revise-gather-output` finishes, to avoid overlapping deltas

Both changes are in the same release. This one implements first, for the reason
`algorithm-naming` implemented before `revise-gather-output`: so the other
change's new and modified requirements are born under the final vocabulary. The
division of the two capabilities they both care about is strict —
`optimizer-selection` moves here, `preprocess-output-writers` stays there — so
no requirement is modified by both changes, which would otherwise mean whichever
archived second silently overwrote the first.

## Risks / Trade-offs

- [A vocabulary sweep across 8 capabilities is transcription-heavy, and a
  mis-placed `## MODIFIED` block silently becomes an ADD at archive, leaving
  the old wording in place] → each delta is written per capability and verified
  by grepping the living spec for the exact requirement header before the
  block is accepted; `openspec validate` checks SHALL/scenario structure but
  NOT section placement, so the header check is the real gate.
- [Renaming the argument on a released surface] → sentinel plus value map, one
  warning, removal deferred to ≥3.0.0 with the rest of the alias layer.
- [The sweep could drift into renaming implementation prose] → D2 states the
  edge, and the implementing task lists the requirements that deliberately keep
  their names, so a reviewer can tell "skipped" from "missed".
- [Two changes in flight over the same capabilities] → D5's strict division;
  the boundary is checked before archiving either.
