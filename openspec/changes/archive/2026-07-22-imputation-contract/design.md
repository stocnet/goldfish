# imputation-contract — design

## Context

`multimode-network-support` task 3.9 (design D9, settled 2026-07-21) lands the
*mechanics*: one typed resolver called from the initial pass
(`impute_attribute()`, `R/data_source.R:953`) and the walk sites
(`R/model_preprocess.R:775`, `:1611`, `:2310`), pooling within the imputed
node's mode category, excluding the node itself, with recorded value-type /
missingness metadata and an empty-pool abort at schedule construction. Its
`nodal-imputation` spec deltas cover the **node** row of the shape × time table.

This change owns the *policy*: the remaining rows of the table (the `global`
cell disagrees with itself — length-1 `mean()` → `NaN` at the start of the
window, `NA → 0` during the walk), a first-class route for deliberate
missingness (today destroyed at the `ds_impute_missing()` seam,
`R/model_preprocess.R:452`, before any effect initializer runs), the tertius
cache sites (`R/functions_effects_DyNAM_choice.R:3019`, `:3217`, `:3230`), and
whether last-observation-carried-forward belongs in the surface at all.

Two constraints frame every decision:

- **The frozen 1e-6 baselines.** Any change to a *default* imputation rule
  moves coefficients on data with missing values. A default may only change
  where no frozen baseline exercises it; everything else is opt-in.
- **The engine cannot accept `NA` past the imputation seam.** Effect updates
  compare `old_value == replace`, so a missing `replace` value fails at the
  very update that receives it — "missing value where TRUE/FALSE needed" —
  before the value even reaches state (reproduced 2026-07-22; see multimode
  design D9 for the repro recipe), and a missing value that *did* enter state
  would fail the next comparison the same way. Any deliberate-missingness
  route must therefore *convert* missingness into a representable value before
  the walk, not let raw `NA` flow.

## Goals / Non-Goals

**Goals:**

- Publish the complete shape × time table as a documented public contract with
  every cell defined and every cell implemented by the resolver, not
  rediscovered per call site.
- Resolve the `global` cell's self-disagreement with one rule at both times.
- Give deliberate missingness a route that a summarizer can see, without
  changing any default.
- Classify the tertius cache sites (imputation vs. summarizer contract) and
  make the documentation match the classification.
- Decide LOCF's place: in this surface, or explicitly deferred.

**Non-Goals:**

- Re-doing anything task 3.9 owns: the resolver, mode-category stratification,
  per-category definedness, the schedule-construction abort, the categorical
  walk-time fix. This change starts from that resolver and extends it.
- Changing the *default* nodal rules (mean / most common value within mode
  category). They stay; the contract documents them.
- Multiple-imputation or model-based imputation (mice-style). Out of scope for
  a preprocessing contract.
- DyNAMi mean-centering (`sub_type == "mean_centered"`), owned by
  `refactor-dynami-engine`.
- Implementing LOCF (see D5).

## Decisions

### D1 — The shape × time table is one published contract, one resolver

The table becomes user-facing documentation (a "Missing data" section on the
data-object help topic plus the modeling vignette), and every cell routes
through the task-3.9 resolver — including the shapes 3.9 did not touch:

| shape | at the start of the window | during the walk |
|---|---|---|
| dyad (network) | `NA → 0` (no tie) | `NA → 0` (increment and replace) |
| global | **abort** (empty pool, D2) | **abort** (empty pool, D2) |
| node, numeric | mean of mode-category pool, self excluded | same rule, state at event time |
| node, categorical | most common value in pool, self excluded | same rule, state at event time |

The dyad cells are already consistent and stay: a missing tie value *is* the
absence of a tie, which is a definition, not a summary — the contract records
it as such rather than pretending it goes through a pool.

The documentation SHALL also disclose the **imputation feedback** property the
resolver has by construction (and the `nodal-imputation` spec makes
normative): an imputed value joins the state, so a missing value after the
start of the window is summarized from a pool that may contain *earlier
imputed values*, not only observed ones. This is a property users must know to
interpret results — it compounds the uncertainty understatement of single
imputation — and it motivates D6's recommendation.

**Why:** four sites in three files re-deriving the rule is how the table
drifted to three broken cells. 3.9 collapses the node row; this change makes
the collapse total, so the documentation can promise what the code provably
does.

**Alternative considered:** documenting current behavior cell-by-cell without
unifying the remaining sites — rejected; it would enshrine the `global`
disagreement as a promise.

### D2 — A missing global value aborts at schedule construction

A global attribute has exactly one value; the imputation rule ("summarize the
variable's state at this moment, excluding the value being imputed") has an
**empty pool by construction**. Under the empty-pool principle 3.9 already
establishes for singleton mode categories, both `global` cells resolve to the
same answer: abort at schedule construction, naming the object and, for
walk-time missingness, the event time. The metadata pass already records
missingness in initial tables and event streams, so both cases are detectable
before any state exists.

**Why:** the current pair (`NaN` at start, `0` during walk) is two different
wrong answers. `0` is an arbitrary value on an arbitrary scale — for a global
covariate like a rate modifier it silently rewrites the model. `NaN`
propagates into every statistic that reads the value. Aborting is the only
cell value consistent with the contract the node row already has.

**Behavior change, gated:** this changes the walk default (`NA → 0` today).
Before implementation, audit the frozen baselines and shipped datasets for
missing global values (expected: none — see Migration). If any baseline
exercises the cell, the abort ships behind a deprecation cycle instead
(`lifecycle::deprecate_warn` on the old behavior); otherwise it lands directly
with a NEWS entry.

**Alternatives considered:** `0` at both times (consistent but arbitrary, and
silently model-altering); carry the previous value forward during the walk
(LOCF-for-globals — defensible mid-walk, but undefined at the start of the
window, so it still needs the abort there, and a shape whose two cells differ
is the disease this change treats).

### D3 — Deliberate missingness: an opt-in per-attribute policy at the existing seam

`set_preprocessing_opt()` gains an imputation policy argument: a named
character vector keyed by attribute (e.g.
`impute = c(party = "as_category")`), consumed by `ds_impute_missing()`.
Values:

- **`"summary"`** (default) — the D1 contract exactly as today. Omitting the
  argument changes nothing; no baseline moves.
- **`"as_category"`** — for factor/character attributes only: at the
  `ds_impute_missing()` seam, `NA` is recoded to an explicit level (a
  documented reserved level, e.g. `"(missing)"`), in the initial table and in
  the attribute's event streams. From that point the engine sees an ordinary
  value: state never holds `NA`, comparisons work, and — the point — every
  summarizer and effect initializer *sees the missingness as a category*
  instead of the most common party. Declaring it on a numeric attribute aborts
  at validation with the reason (no numeric encoding of "missing by design"
  exists that effects could consume); naming an unknown attribute aborts.

The policy value set is an enum designed to grow (`"locf"` reserved, D5).

**Why this shape:** the failure in the proposal is *ordering* — imputation
runs before effect init, so missingness is destroyed before anything can honor
it. Recoding at the same seam keeps the invariant that seam exists to provide
(no `NA` past this point) while *converting* the information instead of
destroying it. Putting the declaration in `set_preprocessing_opt()` needs no
data-format change, works identically for the envir and stocnet sources, and
sits where the consuming code runs.

**Alternatives considered:**

- *Raw opt-out* (`impute = c(x = "none")`, `NA` flows through): rejected — the
  engine cannot hold `NA` in state (see Context); every effect would need
  NA-tolerance it does not have.
- *Conversion-time sentinels only* (the D6 `irps_nuclear` status quo): remains
  valid and supported — `as_category` is exactly that recode, performed by the
  package under a declared policy instead of by every user rediscovering it in
  their data-preparation script. The vignette's hand-rolled recodes become one
  argument (coordinate with multimode tasks 5.1/5.3).
- *Declaring the policy on the data object* (`make_nodes()` metadata):
  conceptually attractive (missingness-by-design is a property of the data),
  but it would need a stocnet/manynet representation for the flag and a format
  version bump; the preprocessing option reaches both paths today. Revisit if
  the policy ever needs to travel with saved data objects.

### D4 — The tertius cache sites are a summarizer contract, not imputation

`update_DyNAM_rate_tertius`'s `impute_changes` branch replaces cache entries
for nodes with an **empty in-neighborhood** — nodes for which the aggregated
statistic is *undefined*, not nodes with a missing attribute. Classification:
this is part of the **effect's definition** ("for a node with no in-neighbors,
the statistic defaults to the mean of the defined entries"), analogous to a
summarizer's declared value on an empty window, not an attribute-imputation
rule.

Consequences: behavior is **unchanged** (the sites feed frozen baselines); the
sites are documented in the tertius effect docs as the empty-neighborhood
default, their internal naming/comments stop calling it imputation, and the D1
contract explicitly excludes statistic-level defaults from its scope.

**Why:** folding these sites into the attribute contract would force a
mode-category pool onto a quantity that is not an attribute and would move
baselines for zero user benefit. Whether the empty-neighborhood default should
itself be per-mode in two-mode networks is a question about tertius
*semantics* — it belongs to a future effect-definition change, on top of a
clear classification made here.

**Alternative considered:** routing them through the D1 resolver — rejected:
the resolver pools an attribute over nodes; these sites pool a *statistic*
over dyad entries. Same verb, different noun.

### D5 — LOCF is deferred, and the surface is shaped so it can land later

Last-observation-carried-forward is **not implemented** in this change. It is
documented in the contract as a considered alternative, and the D3 policy enum
reserves `"locf"` so it can arrive as a third opt-in value without touching
the API shape.

**Why:** LOCF changes what the model estimates — "what this node last was" vs.
"the mean of everyone else right now" is an estimator choice with
time-varying-covariate semantics, not a missing-data repair. Nothing in the
2.0.0 line needs it; shipping the enum slot costs nothing and commits to
nothing.

**Alternative considered:** shipping it now as opt-in — rejected for scope: it
needs its own answer to "carried forward from when?" at the start of the
window (there is no last observation), which is the same design work a
time-varying-covariate change has to do anyway.

### D6 — The documentation recommends multiple imputation; the engine stays single-imputation

The contract documentation gains a "better strategies" subsection: for data
where missingness matters to the conclusions, impute **before** building
goldfish objects — multiple imputation producing *m* completed datasets
(initial nodes table and, for time-varying attributes, the attribute event
streams), fit the model *m* times, and combine the estimates under Rubin's
rules. goldfish supports this today with no code change:
`coef.result.goldfish` and `vcov.result.goldfish` exist
(`R/methods_postestimate.R:35`, `:101`, plus the flavored variants), which is
exactly the interface `mitools::MIcombine()` consumes on a plain list of
fits. The docs show that workflow with a small guarded example and state the
validity caveat: Rubin's rules assume approximately normal, congenial
estimates — reasonable for simple model specifications, to be treated with
care beyond that.

In-engine single imputation (the D1 contract) remains the default and the
only engine behavior: it is a *convenience floor*, and the documentation says
so plainly — imputed values are treated as observed and feed later
imputations (D1's feedback disclosure), so standard errors are understated.

Mechanical consequences: `mitools` enters `Suggests` (never Imports — the
engine takes no dependency on it); the example is guarded with
`@examplesIf requireNamespace("mitools", quietly = TRUE)` and the
verification test with `skip_if_not_installed("mitools")`. The claim "works
with MIcombine" is verified by a test that combines two real fits, not
asserted from the interface by inspection.

**Why not implement MI in-engine:** *m* preprocessing runs over *m* completed
datasets is a driver loop the user can write in five lines; owning it would
mean owning imputation models (predictive mean matching, chained equations —
`mice`-shaped machinery) far outside a preprocessing contract. The
documentation route delivers the statistical guidance at zero engine risk and
zero baseline movement.

**Alternatives considered:** a `goldfish_mi()` convenience wrapper (fit list →
combined table) — deferred; if demand materializes it is a small
post-estimation helper, and D3's policy enum is not the place for it since MI
happens before data objects exist. Recommending `mice` + `broom`-style
pooling instead of `mitools` — `mitools::MIcombine()` is the smallest
dependency that consumes `coef`/`vcov` directly; `mice::pool()` requires a
`tidy`/`glance` interface goldfish does not have.

## Risks / Trade-offs

- **[D2 is a behavior change]** → gated on the baseline/dataset audit
  (Migration step 1); if any frozen baseline carries a missing global value,
  the abort ships behind a `lifecycle` deprecation instead of directly.
- **[Reserved level collision]** (`as_category` recode value already present
  as a real level in someone's data) → validation at the seam: if the reserved
  level already exists among observed values, abort naming the attribute and
  the clash; the level string is documented as reserved.
- **[Two source paths]** (legacy `impute_missing_data()` envir path vs.
  stocnet `ds_impute_missing()`) → the policy is enforced at the generic, so
  both methods receive it; the envir path either honors `as_category`
  identically or — if the legacy environment plumbing makes the event-stream
  recode unreasonable — aborts with "policy requires stocnet data objects",
  decided at implementation time (Open Question 2). Silent divergence between
  the paths is the one outcome not allowed.
- **[Warning-text churn]** → the contract docs and the existing cli warnings
  say **most common value** (never "mode" for the statistic, per D9
  vocabulary); snapshot tests pin the wording once, in a reproducible cli
  context.
- **[Sequencing on multimode 3.9]** → this change consumes the resolver 3.9
  creates; it cannot start before 3.9 is committed. If 3.9 slips, D1's doc
  work could start early, but no code task can.

## Migration Plan

1. **Audit first** (no behavior change): enumerate missing values in every
   dataset the frozen `global_v1` baselines and shipped examples touch, by
   shape. Expected result — no missing *global* values anywhere; record the
   result in `progress.md`. This decides whether D2 lands direct or behind
   deprecation.
2. Land D1 documentation + D4 reclassification (docs and naming only, no
   coefficient movement), then D2, then D3.
3. Every step: tests green with `NOT_CRAN=true`, baselines PASS not SKIP; NEWS
   entry per user-visible step; version bump at the phase milestone.
4. Rollback: each decision is an independent commit series; D2 and D3 are
   individually revertable without disturbing D1/D4.
5. Coordinate with `multimode-network-support` 5.1/5.3: if this change ships
   first, the `irps_nuclear` vignette uses `impute = c(party = "as_category")`
   instead of hand recodes; if the vignette ships first, a follow-up swap is
   noted there.

## Open Questions

1. **Exact argument name and reserved level string** for D3
   (`impute` vs. `impute_policy`; `"(missing)"` vs. `"<NA>"`): settle at
   implementation against `set_preprocessing_opt()`'s existing naming
   conventions; snake_case and American English regardless.
2. **Legacy envir path and `as_category`** (see Risks): honor identically or
   abort-with-reason — decide once the event-stream recode is prototyped on
   the stocnet path.
3. **Does `estimate_dynami()` interact with the policy surface?** DyNAMi's
   interaction-group updates read nodal attributes through the same
   preprocessing; expected to inherit D1/D3 for free, but verify before
   documenting the contract as model-independent.
