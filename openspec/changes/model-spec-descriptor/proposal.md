# Model behavior is a descriptor, not a class hierarchy

## Why

The model spec encodes what a model *is* three times, in three shapes, and
they disagree about how many kinds there are.

The class vector is `c(variant, indexing, "model_spec")`, where `variant` is
the cartesian product of `model` and `sub_model` — nine classes. That
granularity is real for exactly one consumer. An audit of the current code
(2026-09-05) found:

- **Preprocessing needs two bits, not nine classes.** Every non-DyNAMi
  `preprocess.*` method is a one-line call to a recipe loop with two flags:

  | variant | loop | `right_censored` | `intercept_scalars` |
  | --- | --- | --- | --- |
  | `dynam_rate` | sender | TRUE | TRUE |
  | `dynam_rate_ordered` | sender | FALSE | FALSE |
  | `dynam_choice` | dyad | FALSE | FALSE |
  | `dynam_choice_coord` | dyad | FALSE | FALSE |
  | `rem_rate` | dyad | TRUE | TRUE |
  | `rem_rate_ordered` | dyad | FALSE | FALSE |

  `dynam_choice` and `dynam_choice_coord` are **byte-identical**. `model`
  contributes nothing — `rem_rate` differs from `dynam_rate` only in the loop,
  which is the indexing axis and already its own class.

- **The two flags never disagree.** `right_censored == intercept_scalars` in
  all six rows: one fact wearing two names, neither of which says what it
  encodes (that the sub-model is a *timed rate* — a waiting time with an
  exposure denominator, rather than an ordinal comparison). The pair spans
  **130 sites across 18 files**.

- **Three of the nine likelihoods are aliases.**
  `compute_event_contribution.dynami_rate_spec <- compute_event_contribution.dynam_rate_spec`
  and its two siblings are literal assignments. DyNAMi differs only in
  preprocessing (31-line methods against 9-line ones), and the effect-registry
  work is expected to close even that.

- **43 downstream sites re-derive behavior** from `model`/`sub_model` strings
  (`sub_model == "rate"`, `sub_model %in% c("choice", "choice_coordination")`,
  …) outside `R/model_spec.R`, each restating a rule the spec already knows.

The cost is not tidiness. An alias means a debugger stops in a method whose
name does not match the object; a re-derivation means a rule can be corrected
in one place and stay wrong in forty-two others; two names for one fact means
a reader cannot tell whether they are the same knob.

The pressure is about to multiply. `parametric-rates` adds Weibull and
Gompertz, and ADR-0025 already rules the waiting-time distribution an
**orthogonal axis**; `two-sided-coordination` adds four more mechanisms that
differ *only* in likelihood; DyNES adds joint estimation. Flattened into
`variant`, those multiply into a table of near-identical classes. Deciding
this before they land is much cheaper than after.

The precedent is in-tree and explicit. `risk-set-dispatch` already requires
the risk-set descriptor to be "decided once, at parse time, on the model spec"
with "no site re-deriving the family". This change applies that same rule to
the rest of the variant space.

## What Changes

- **A single descriptor is computed once, at spec construction**, carrying
  every behavioral fact a downstream component needs: the risk-set axis, the
  timing regime, the likelihood family, the input shape, and (reserved) the
  waiting-time distribution. It subsumes the existing `risk_set` descriptor
  rather than sitting beside it — one object, one construction site.
- **Every component reads the descriptor; none re-derives.** The 43
  `model`/`sub_model` string tests outside the constructor are replaced by
  descriptor reads. `model` and `sub_model` remain on the spec as *provenance*
  — what the user asked for — not as switches.
- **One name per behavior.** `right_censored` and `intercept_scalars` are
  retired in favor of a single field naming the behavior they encode. The
  rename is mechanical but wide (130 sites, 18 files).
- **The variant classes lose their preprocessing and estimation-entry roles.**
  Preprocessing dispatches on the descriptor's recipe fields; `estimate_int`
  continues to dispatch on the axis. S3 dispatch is retained **only** where
  implementations genuinely differ — the likelihood — so
  `compute_event_contribution` keeps a class to dispatch on, and the three
  DyNAMi aliases are deleted rather than re-registered.
- **BREAKING (internal only)** — the `model_spec` class vector changes shape.
  No exported function, argument, or return value changes; no user-visible
  behavior changes; no coefficient moves.

- **The preprocessing output classes collapse too** (design D9). Four marker
  classes carry one `print` method and three `inherits()` checks between them,
  and a fifth shape — `compute_statistics(output = "gather")` — is returned to
  users with no class at all. They become one `goldfishStat` class whose
  `storage` (`pointer`/`stack`/`db`) and `scope` (`single`/`flavored`) fields
  carry what the class strings encoded, which also closes the unclassed-gather
  gap by construction.

- **Six mechanical flavored fan-outs collapse** (design D10). Of the
  seventeen generics implemented once per fit class, six carry no behavior of
  their own — `fitted`, `predict`, `residuals` and `evaluate_model` are already
  one-liners over `flavored_component_apply()`, and `coef` and `vcov` are the
  same loop written by hand. They become one fan-out path. The shared fit-class
  parent and its contract table are **not** in scope: that is a correctness
  question about which generics may safely inherit, not a duplication one.

## Capabilities

### New Capabilities

- `model-spec-descriptor`: the single behavioral descriptor — what it carries,
  that it is computed exactly once at construction, that every consumer reads
  it, that no site re-derives from `model`/`sub_model`, and the one-name-per-
  behavior rule that retires the `right_censored`/`intercept_scalars` pair.

### Modified Capabilities

- `model-recipe-dispatch`: the "S3 model_spec class hierarchy" requirement is
  replaced — the hierarchy narrows to what actually dispatches, and the recipe
  choice becomes a descriptor read. The DyNAMi delegation requirement is
  restated without aliases.
- `risk-set-dispatch`: the risk-set descriptor becomes a *component* of the
  one descriptor rather than a free-standing object; its decided-once and
  no-re-derivation rules are generalized, not weakened.

## Impact

- **Code**: `R/model_spec.R` (constructor and descriptor), `R/model_preprocess.R`
  (the recipe methods and the flag pair), `R/estimation_core.R` (likelihood
  dispatch, alias removal), plus the 43 re-derivation sites and the 130
  flag sites across 18 files.
- **Tests**: the model-spec unit tests assert class vectors directly and will
  be rewritten against the descriptor; the frozen 1e-6 coefficient baselines
  and the C++ goldens **must not move** — this change alters no arithmetic,
  and a moved baseline stops the work.
- **Not affected**: `src/` (the C++ core never sees these classes), the
  exported API, and the deprecated path.
- **Sequencing**: lands best *before* `parametric-rates` and
  `two-sided-coordination` add axes to the variant space, and independently of
  `class-naming-scheme` — that change renames class strings, some of which this
  one removes, so running them concurrently on the same files would conflict.
