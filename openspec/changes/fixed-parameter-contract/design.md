# Design — fixed-parameter-contract

## Context

Verified on the tree (2026-07-25, during the backend-parity explore that
carved this change out):

- `assemble_fixed_parameters()` (formula_validate.R:230) is the **single
  assembler**: it knows term names, rhs positions, the +1-intercept prepend
  rule, interaction columns (always estimated), and the
  choice-constant-offset warning. It then flattens all of that into a
  positional NA-vector and returns it.
- `estimate_c_int()` (cpp_interface.R:84-108) and `estimate_int()`
  (estimation_core.R:84-108) carry **byte-similar duplicated decode
  blocks**: length check against `nParams`, `likelihoodOnly <-
  all(!is.na(...))`, parameter overwrite, `idUnfixedCompnents` /
  `idFixedCompnents` (typo shared by both). Downstream masking: score
  zeroing (cpp_interface.R:454), information sub-matrix inversion
  (:509-524), stdErrors (:588), and the maxLik adapter's `id_fixed`
  (:363).
- cpp_interface.R:194 gates the Poisson intercept warm start on
  `is.null(fixedParameters) || is.na(fixedParameters[1])` — the wire
  format's ordering convention ("position 1 is the intercept") leaked into
  an unrelated numeric decision.
- The fit stores fixedness as `names[, "fixed"]` — `"TRUE"`/`"FALSE"`
  **strings** — and `GetFixed()` (utils.R:815-823) decodes them with
  `eval(parse(text = x))` per coefficient. Five-plus methods consume it
  (methods_display.R ×3, methods_postestimate.R ×2, `result$nParams`).
- The string column shape shipped in public releases (fixed-coefficient
  printing predates v1.7.0), so old serialized fits carry it — per the
  deprecation-scope audit (backend-parity D11) the *reader* must tolerate
  it, while the internal wire (never public) is free to change outright.
  **Superseded 2026-07-27**: `snake-case-result-components` has since
  shipped the no-window policy — a pre-rename fit is detected by the
  result-format guard and refused with re-fit guidance. Any fit old enough
  to carry the string column also carries pre-rename component names, so
  it trips that guard before `GetFixed()` runs; D3 is revised accordingly.
- `residuals-gof` task 2.1 will generalize `evaluate_default_c` into
  `evaluate_model()` — the fourth would-be copy of the decode if the wire
  stays as is.

Verified 2026-07-27 (the scope-expansion explore session):

- `set_algorithm_newton(initial_parameters =)` is the positional sibling:
  full-length numeric, no names, length-checked only. The Poisson rate
  intercept warm start (cpp_interface.R:195-206, the
  `log(n_dep_events / total_time / avg_active_entity)` seed) is gated on
  `is.null(initialParameters)` — supplying any initial vector disables it.
  Seeding order today: initials first, fixed values overwrite.
- The flavored path (estimate_flavored.R) estimates per fid by re-entering
  `estimate_wrapper()` with each fid's own formula and the **same shared
  `control_algo`**. Consequences on HEAD: per-flavor offset values are
  inexpressible; `offset_coef` with one offset-less flavor aborts at that
  fid ("offset_coef was supplied but the formula has no offset() terms");
  a shared positional initial vector can silently seed the wrong
  coefficients of a different flavor.
- `excludeParameters` is never passed by the estimation front end
  (`args_estimation`, model_estimate.R:2146+ omits it); the only other
  occurrences are NULL-default signatures (preprocess_writers.R:433,
  process_state_evaluators.R:45). `assemble_fixed_parameters()` is
  exclusion-unaware, so a non-NULL value would shift every fixed position.
- The fit's `names` is a **character matrix** built by `GetDetailPrint()`
  (utils.R:625) via conditional `cbind()` — columns
  (`ignore_repetitions`, `weighted`, `type`, `window`, `fixed`) exist only
  when relevant; fixedness is a string *because* a character matrix cannot
  hold a logical column. Consumers: `compact_term_strings()` — including
  its token builder's `Fx` badge, which reads
  `.isFixedToken(row[["fixed"]])` at utils.R:957 and becomes a plain
  logical test — `term_label()` (coefficient names for `tidy()`/`vcov()`),
  `GetFixed()`, the `compact = FALSE` summary details table.
  Front-end call order: `assemble_fixed_parameters()` runs at
  model_estimate.R:2051 but `GetDetailPrint()` (the label source
  `term_label()` reads) only at :2074 — the D6 resolver needs labels at
  assembly time, so the label build moves above the assembly or the
  resolver derives labels itself. `GetDetailPrint()` also
  feeds formula_parser.R:454 and preprocess_export.R:214 (the gather
  export's `effect_description` attribute) — the shape change may ripple
  beyond the fit.
- Coefficient labels are unique within a fit: `term_label()` dedups
  collisions (documented: suffixes appended to every member of a colliding
  group). Name-matching at the user surface is therefore sound; D1's
  named-vector rejection applies to the wire, not the surface.

## Goals / Non-Goals

**Goals:**

- One structured contract each for fixedness and seeding, assembled once
  where names are authoritative, consumed everywhere through one helper.
- One alignment story at 2.0.0: values align to terms **by name** at the
  user surface (for fixing and seeding alike) and **by validated index**
  on the wire — never by counting positions against a sentinel vector.
- Alignment errors impossible to make silently: indices validated, errors
  naming terms; the flavored per-fid re-entry resolves names against each
  fid's own labels.
- The fit's fixed record a plain logical in a typed per-term table;
  `GetFixed()` keeps its return type so no consumer changes.
- Numerics untouched — baselines PASS on every commit.

**Non-Goals:**

- Any *breaking* user-surface change: `offset()` / `offset_coef` /
  `initial_parameters` / the superseded `fixed_parameters` argument all
  keep their documented behavior and lifecycle stages; the named forms
  (D6, D7) are strictly additive.
- The offset *semantics* (choice-axis warning, stat column retention) —
  unchanged, only restated where the wire is mentioned.
- The maxLik adapter's evaluation strategy or any optimizer behavior.
- `evaluate_model()` itself (residuals-gof task 2.1) — this change only
  makes the contract it will consume.

## Decisions

### D1 — The contract: `fixed_spec` with idx/values/names, predicates derived

`assemble_fixed_parameters()` returns `NULL` (nothing fixed) or a
`fixed_spec`: `idx` (integer positions into the final coefficient vector,
intercept included), `values` (numeric, same length), `names` (character
term labels). Derived, never stored: `likelihood_only :=
length(idx) == n_params`, `intercept_fixed := 1L %in% idx`. The NA-vector
ceases to exist internally; the superseded `fixed_parameters` argument is
converted to a `fixed_spec` at the same assembler (positions are what the
user supplied; names fall back to coefficient labels).

Rejected: a named numeric vector keyed by coefficient labels (labels are
not unique across flavors/interactions and the resolution to positions
would just move the fragile step); keeping the NA-vector wire with a
single constructor (still carries the sentinel semantics and the length-n
shape every consumer must interpret).

### D2 — One decode helper, three consumers, two explicit inputs

A single internal helper (estimation-side sibling of the assembler) takes
`(fixed_spec, initial_spec, n_params)`, validates both `idx` sets against
`nParams` once, and produces the pieces the loops need: the unfixed index
vector, the seeded parameter vector (seed first, fixed values overwrite —
the order the current code implements implicitly, now stated),
`likelihood_only`, `intercept_fixed`, and `intercept_seeded`.
`estimate_c_int()`, `estimate_int()`, and the maxLik adapter consume it;
the duplicated blocks and the `Compnents` typo go. The cpp_interface.R
warm-start gate reads the two predicates (`!intercept_fixed &&
!intercept_seeded`) instead of `is.na(fixedParameters[1])` and
`is.null(initialParameters)` — so seeding a non-intercept term no longer
disables the intercept's data-derived start, and no numeric decision reads
a wire encoding. The dead `excludeParameters` plumbing is removed from the
internal estimator signatures in the same sweep (never passed by the
front end; exclusion-unaware position math would corrupt `fixed_spec`
alignment if it ever were).

### D3 — The fit records a logical; string-column fits are refused, not read

*(Revised 2026-07-27; the original tolerant-reader decision is retained
below as the rejected alternative.)*

The fit's effect-description gains fixedness as a plain logical (stored
alongside `names`, exact shape decided at implementation against the
current `GetDetailPrint` structure). `GetFixed()` reads the logical only;
`eval(parse())` is removed outright. A fit carrying only the string column
is a pre-change fit and is refused through the result-format guard with
re-fit guidance — the same no-deprecation-window policy
`snake-case-result-components` established for the component rename
(mechanism, corrected 2026-07-27: **no stamp bump**. The stamp machinery
is unreleased, so every real user fit is *unstamped* and the guard's
missing-stamp branch already refuses it; a string-column-only fit is
definitionally pre-stamp. Epochs stay frozen until 2.0.0 ships — bumps
are a post-release tool). `GetFixed()`'s
return type is unchanged, so the five consumers need no edits.

Rejected: the tolerant fallback (`GetFixed()` parsing the string column
for fits from earlier versions, the backend-parity D10 pattern for the
missing `$backend`). Any serialized fit that predates the logical also
predates the snake_case component rename, so the rename guard refuses it
before `GetFixed()` is reached — the fallback is unreachable for real user
objects and only shields dev-line fits produced between v1.9.16 and this
change, which re-fit instead. One policy, one guard, no dead
`eval(parse())` branch.

### D4 — Sequencing: after backend-parity, before residuals-gof task 2.1

This change follows backend-parity's shared-helper pattern and touches the
same two estimator files backend-parity is finishing; landing it mid-kernel
work would break that change's bisectability discipline. It must land
before `evaluate_model()` exists so the evaluator is born on `fixed_spec`.
Cross-change protocol: residuals-gof task 2.1 consumes this contract;
no spec requirement is modified by both changes (this change owns
`offset-fixed-terms`, residuals-gof does not touch it).

### D5 — The effect description becomes a typed per-term data.frame (2026-07-27)

`GetDetailPrint()` returns a data.frame — rownames = effect names, a
**stable schema** (every *flag* column always present —
`ignore_repetitions`, `weighted`, `type`, `window`, `fixed` — with
`""`/`NA` where a feature is unused; the `Object <k>` columns stay
formula-dependent, their count being inherent to the terms) — instead of
the character matrix with conditional columns.
`fixed` is a plain logical column, which is the storage D3 required and
the reason strings existed at all. `[, "col"]` indexing and rownames
survive the conversion, so the enumerated consumers
(`compact_term_strings()`, `term_label()`, `GetFixed()`, the
`compact = FALSE` details table) are near-drop-in; `tidy()` gains typed
columns natively. No `FIT_VERSION` bump (user decision 2026-07-27): the
stamp machinery is unreleased, so the current epoch **is** the 2.0.0
epoch and pre-release shape changes ride inside it — no user object
carries a stamp, and unstamped fits are already refused by the guard's
missing-stamp branch. Dev-line fits between v1.9.16 and this change are
the maintainers' own and owed nothing (the deprecation-scope policy).

Rejected: a parallel logical component beside the untouched matrix (two
objects to keep aligned forever, the conditional-columns wart survives);
a bare list of vectors with `as.data.frame()` on demand (it is a
data.frame minus the class — every tabular consumer converts first and
all `[, "col"]` indexing is rewritten to `$`, for no gain the class does
not already give).

`GetDetailPrint()` also feeds the parsed bundle (formula_parser.R:454)
and the gather export's `effect_description` attribute
(preprocess_export.R:214) — task 3.1's consumer sweep covers those two
sites like any other. This is purely a code-sweep concern: no stamp
bumps either way until 2.0.0 (see the D3 mechanism correction).

### D6 — Named-partial `initial_parameters`, positional form unchanged (2026-07-27)

`initial_parameters` accepts, additionally, a **named numeric vector**
matched against the fit's deduped coefficient labels (the `term_label()`
output `tidy()` renders): named entries seed only those coefficients,
everything else keeps its default — including the rate intercept's warm
start, per D2's `intercept_seeded` predicate (a full positional vector
seeds every coefficient, so it disables the warm start exactly as today).
An unnamed vector of partial length aborts naming the expected length; an
unknown name aborts listing the available labels. The full-length unnamed
positional form keeps its exact current behavior — the named form is
additive, so no lifecycle is owed on public surface. The resolver is one
internal function resolving at the same front-end site
where the assembler runs (names and positions both known), emitting an
`initial_spec` with the same `idx`/`values`/`names` shape as D1.

Flavored semantics (user, 2026-07-27, second explore session): a flat
named vector **broadcasts** — each fid seeds the labels it has; a name
matching no fid at all aborts. Broadcast is the right default because an
initial value only changes the optimization path, never the optimum, so
one seed per label across fids is usually exactly what is wanted. For
per-process tuning, a **nested list form** is also accepted:
`initial_parameters = list(<flavor> = c(...))` applies to all of that
flavor's fids, and `list(<flavor> = list(<family> = c(...)))` targets one
fid; list names are validated against the process map (unknown flavor or
family aborts naming the valid ones); the nested and flat forms cannot be
mixed in one call. The unnamed full-length positional vector aborts on a
flavored specification (positions are meaningless across differing
per-fid parses).

Rejected: a new `initial_coef` argument (falsely symmetric with
`offset_coef`, which aligns to `offset()` formula markers by order — two
arguments with two alignment rules is the confusion this change exists to
end); positional-with-NA-means-default (still positional, still the
counting trap, no help for flavored); a formula-side `init()` marker
(pins algorithm state into a reusable specification object — a stored
spec would silently carry starting values into every future estimation).

### D7 — Offset values ride the formula: `offset(term, coef = value)` (revised 2026-07-27)

*(The first cut of this decision — a named `offset_coef` with a
tolerated-elsewhere matching rule — was refuted the day it was written by
the minimal counterexample: a two-flavor DyNAM whose rate AND choice
formulas all carry the same windowed-inertia term produces four fids with
the **same coefficient label**, so name matching cannot tell them apart
and a flat named value would broadcast one number across the log-rate and
log-odds scales, silently. Name-matching disambiguates across different
labels; it cannot disambiguate the same label used in four models.)*

`offset()` gains a `coef` argument: `offset(inertia(ties, window =
"2 hours"), coef = -1.2)` fixes that term at that value *in that
formula*. The formula is the only fid-local object in a flavored
specification, so this is exact by construction — no matching rule, no
broadcast, and the specification becomes self-contained (estimable
without companion control arguments). Precedent: base R offsets carry
their values inside the formula (`offset(log(exposure))`); a fixed
coefficient is part of the model being specified, not of the algorithm
settings. A fid whose formula carries no `offset()` simply has nothing
fixed — the current mixed-presence abort disappears because there is no
shared control value to be left dangling.

Rules: a **flavored** specification takes offset values from formulas
only — supplying `offset_coef` alongside a flavored specification aborts
with guidance to use `coef =`, and the superseded positional
`fixed_parameters` argument aborts there for the same reason (positions
are meaningless across differing per-fid parses). A **single-process** model keeps
`offset_coef` (positional as documented, plus the named form where labels
are unique) alongside `coef =`; the same term receiving a value from both
sources aborts naming the term; `offset()` terms whose value arrives from
neither source abort as today. `offset_coef` is dev-line surface (absent
at v1.7.0), so none of this owes lifecycle machinery.

The tolerated-elsewhere matching rule survives only where it is sound:
`initial_parameters` broadcast (D6), where the same seed for the same
label across fids is semantically harmless.

- [Alignment regressions while swapping the wire] → the assembler is the
  only producer and already position-aware; the new unit tests pin
  idx/values against formula fixtures with and without intercept and with
  interactions; frozen 1e-6 baselines PASS gates every commit (offset
  fixtures are in the suite).
- [Old serialized fits with the string column] → refused by the
  result-format guard with re-fit guidance (revised D3), with an explicit
  test that a string-column-only fit aborts through the guard rather than
  reaching `GetFixed()`.
- [A third change touching cpp_interface.R/estimation_core.R while two are
  in flight] → strict sequencing (D4): this change starts only after
  backend-parity's task list is complete; same branch, sequential
  implementation, no parallel edits.
- [Name-matching typos silently ignored] → they are not: an unknown name
  aborts listing the available labels (D6); initials-broadcast tolerates
  only names that resolve in *some* fid, and the nested list form
  validates flavor/family keys against the process map. Offsets have no
  matching rule at all — `coef =` is fid-exact by construction (D7).
- [The names-table conversion breaks a consumer not on the enumerated
  list] → task 3.1 sweeps by `git grep` on `$names` / `names[, ` before
  converting; the format guard plus the full suite gate the commit.
- [Flavored value semantics surprise users] → offsets carry no resolver
  at all (`coef =` is local to its formula); the initials broadcast and
  nested-list rules are documented on `initial_parameters` and exercised
  by the four-fid fixture tests (same label in every fid), including the
  aborts for `offset_coef`-with-flavored and positional-with-flavored.

## Open Questions

(scoped 2026-07-25; expanded 2026-07-27; closed in the second 2026-07-27
explore session — the storage-shape judgment is decided by D5, the
flavored value channel by the revised D7, and the former `PREP_VERSION`
grounding item dissolved with the no-bump rule: epochs are frozen until
2.0.0 ships, so pre-release shape changes never bump; the
`effect_description` sites are covered by task 3.1's ordinary consumer
sweep.)
