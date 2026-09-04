## Why

`goldfish` can already compose several *network* (dyadic tie) processes that
coevolve — `make_joint_specification()` couples two or more `make_specification()`
objects over a shared node set (see `multivariate-specification`). It has no way to
model a nodal **attribute** as a stochastic, actor-driven outcome process in its own
right: attribute change events only ever enter as exogenous, deterministic covariates
via `link_events()`/`nodal-imputation`. There is no way to express that an actor's
tie-formation depends on an evolving attribute (selection) while that same attribute's
evolution simultaneously depends on the actor's network position (influence) — the
joint dynamic that Snijders & Steglich (2017, *Modeling the Coevolution of Networks
and Behavior*) formalize as two coupled objective functions, one for network change
and one for behavior change, driven by the same actors. Researchers who want to test
selection-vs-influence questions ("do friends become similar, or do similar people
become friends?") cannot express that model in `goldfish` today.

## What Changes

- Introduce a **behavior process**: a nodal attribute can be declared as a modeled
  dependent process (a "behavior layer") with its own actor-oriented rate/choice
  formulas, analogous to how a network layer is declared today — instead of only
  being a static or exogenously-updated covariate column.
- Add **behavior objective-function effects** (the Snijders–Steglich behavior
  evaluation function terms): linear and quadratic shape effects, average
  similarity/alter-influence effects computed from the coevolving network's current
  neighborhood, and any other terms needed to reproduce the book's worked examples.
- Extend the network-side effect vocabulary so tie-formation effects (e.g.
  `similarity()`, `ego()`, `alter()`) can read the *current, dynamically modeled*
  state of a behavior layer, not just a fixed exogenous trajectory.
- Extend composition (`make_joint_specification()` or an equivalent constructor) to
  accept one network process and one behavior process over the same node set,
  producing a joint specification whose likelihood factorizes per the existing
  multi-process machinery, and whose simulation interleaves network and behavior
  ministeps/events on the shared clock.
- Extend estimation so the joint network+behavior specification can be fit — either
  by reusing `estimate_dynam()`/the multi-process walk per-process, or (if the two
  processes are not separable, e.g. simultaneous behavior-dependent rate terms) a
  dedicated joint estimation path.
- Add a vignette section (or new vignette) walking through a Snijders–Steglich-style
  worked example (e.g. friendship + a behavior such as smoking or delinquency),
  showing how to declare, estimate, and interpret both objective functions.

## Capabilities

### New Capabilities
- `behavior-coevolution`: declaring a nodal attribute as a modeled dependent
  (behavior) process, its rate/choice-style formula surface, the behavior
  evaluation-function effect vocabulary (shape, similarity/influence terms), and how
  it composes with a network process over a shared node set to reproduce the
  Snijders & Steglich (2017) network–behavior coevolution model.

### Modified Capabilities
- `multivariate-specification`: `make_joint_specification()` gains the ability to
  compose a behavior process alongside network processes (today it composes DyNAM
  rate/choice/choice_coordination and REM processes only), including how
  cross-process reads resolve when one process's "layer" is a behavior rather than a
  network.
- `multi-process-walk`: the merged single-clock walk's `stat_block` keying gains a
  sender-mode-only block kind for a behavior process's choice submodel (today
  `stat_block` assumes choice/ordered fids are always dyad-indexed, i.e. have a
  receiver mode; a behavior choice submodel's risk set is the actor's own reachable
  states, with no receiver side).

## Impact

- **Data objects** (`R/make_data.R`): a new or extended constructor path for
  declaring a nodal attribute's change events as a dependent (modeled) process,
  alongside the existing exogenous `link_events()` path.
- **Specification** (`R/make_specification.R`): `layer`/`info$focal` resolution and
  validation must accept a behavior layer, not only a network layer.
- **Effects** (`R/functions_effects_DyNAM_*.R`, likely a new
  `R/functions_effects_behavior.R`): new behavior evaluation-function effects, and
  extensions to existing network effects that read attribute state.
- **Preprocessing / multi-process walk** (`R/model_preprocess.R` and the shared walk
  behind `multivariate-specification`): the merged single-clock walk must host and
  update behavior-process state alongside dyad/rate state.
- **Estimation** (`R/model_estimate.R`): joint (or per-process factorized)
  estimation of network + behavior specifications.
- **Documentation**: new vignette content demonstrating the coevolution workflow.
