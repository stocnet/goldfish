# Design — snake-case-result-components

## Context

The camelCase components on returned objects, catalogued from the code and the
roxygen `@return` blocks:

**Fitted result (`result.goldfish`)** — documented unless noted:

| current | proposed |
|---|---|
| `standardErrors` | `standard_errors` |
| `logLikelihood` | `log_likelihood` |
| `finalScore` | `final_score` |
| `finalInformationMatrix` | `final_information_matrix` |
| `nIterations` | `n_iterations` |
| `nEvents` | `n_events` |
| `nParams` (undocumented) | `n_params` |
| `intervalLogL` | `interval_log_lik` |
| `eventProbabilities` | `event_probabilities` |

**`result$convergence`** — mixed within one list:

| current | proposed |
|---|---|
| `isConverged` | `is_converged` |
| `returnCode` | `return_code` |
| `maxAbsScore` | `max_abs_score` |
| `maxAbsUpdate` | `max_abs_update` |
| `score_rel_norm` | *(already correct)* |

**Gather export:** `namesEffects` → `names_effects`, `isDependent` →
`is_dependent`.

**Preprocessed object:** `initialStats` → `initial_stats`,
`rightCensoredIntervals` → `right_censored_intervals`,
`rightCensoredStatsChange` → `right_censored_stats_change`,
`orderEvents` → `order_events`, `dependentStatsChange` →
`dependent_stats_change`.

Roughly eighteen names across three objects. `event_probabilities` is the one
that has already bitten: the components beside it are snake_case, so it is the
name a reader assumes, and reading it yields `NULL` silently.

## Goals / Non-Goals

**Goals:**

- One convention across every component a user can read.
- The sweep happens once. A partial rename would leave the same guessing problem
  with a shorter list.
- A pre-2.0.0 fit object is *recognized and reported*, replacing today's opaque
  `summary()` error and silent `AIC()` misreport.

**Non-Goals:**

- Renaming internal variables that never surface. The existing file-by-file
  linter policy covers those.
- Changing any value, shape or semantics. This is spelling only.
- Adding or removing components.

## Decisions

### D1 — one sweep, not per-change renames

The components are spread over three objects that different changes own — the
gather export belongs to `revise-gather-output`, the diagnostic components to
`residuals-gof`, and `logLikelihood` / `convergence` to nobody in particular.
Splitting the rename by owner would break user scripts three times for one
conceptual change, and would leave the core result surface unowned and therefore
unrenamed. One change, one break, one NEWS entry.

Rejected: folding it into `residuals-gof` (owns only the diagnostic components).
Rejected: folding into `revise-gather-output` (owns only two names).

### D2 (revised) — no deprecation window; detect old objects instead

The first draft gave the renamed components a deprecation window. Checking what
a CRAN fit object actually looks like removed the case for it.

CRAN is **1.6.12** (2025-04-13). Its result object carries:

```
parameters, standardErrors, logLikelihood, finalScore, finalInformationMatrix,
convergence(isConverged, maxAbsScore), nIterations, nEvents,
call, formula, model, names, nParams, rightCensored, subModel
```

Note `subModel` and `rightCensored`: **the 1.7.0 renames already broke these
objects**, because today's code reads `sub_model` and `right_censored`. Running
today's methods on a 1.6.12-shaped object:

| method | result today |
|---|---|
| `print`, `coef`, `vcov` | run |
| `logLik` | returns a value with **`df = NULL`**, so `AIC()` / `BIC()` silently misreport |
| `summary` | **errors**: "argument is of length zero" |

So the object is already unusable, and fails in the two worst ways available —
one opaque error and one silent misreport feeding model comparison. A window on
component *spellings* would preserve `fit$logLikelihood` on an object whose
`summary()` errors and whose `AIC()` is wrong: compatibility theater, paid for
with bookkeeping on top of the deprecations the package already carries. The
2.0.0 line also changes the preprocessing format and the fit object's shape
independently of this rename, so there is no version in which an old object
works apart from spelling.

**Decision.** Rename outright, no window. Instead, **recognize** a pre-2.0.0
object and say so. The distinction is the whole point:

```
deprecation shim  = COMPATIBILITY  ongoing; must track every component
                                   as the object keeps evolving
version detector  = DIAGNOSIS      one-off; only ever recognizes "old",
                                   never translates, never needs updating
```

Two mechanisms, both cheap:

- **Positive stamp.** New fits record the format version they were built with, so
  detection does not rest on absence-reasoning going forward.
- **Negative marker for objects that already exist.** After this rename, the
  presence of a retired camelCase component (`logLikelihood`, or the
  already-retired `subModel`) identifies a pre-2.0.0 object unambiguously. The
  rename is what makes the detector exact — the change feared to break old
  objects is what makes them recognizable.
  **Superseded by D7:** the negative marker turned out to be redundant. Every
  real pre-2.0.0 object also lacks the positive stamp, so the stamp's absence
  already identifies it and the retired-component list is removed.

Rejected: a `$` method translating old names (the compatibility theater above).
Rejected: doing nothing, which leaves today's opaque error and silent `df = NULL`
in place.

### D2a — the detector guards every entry point, with severity by consequence

Attaching this only to `print()` would miss the dangerous case. A user who
prints a mystery object sees the message; a user whose script calls `AIC()`
never prints anything, and `AIC()` is exactly where the silent `df = NULL`
does its damage. So one shared guard, called by every S3 method on the class,
with severity chosen by what happens if it stays quiet:

| surface | behavior | why |
|---|---|---|
| `print` / `format` | inform, and still show what it can | the first thing a user does with an unfamiliar object; this is where "re-fit to use current features" belongs |
| `logLik`, `vcov`, `summary`, and the `AIC`/`BIC` path | **error** | a silent `df = NULL` feeding model comparison is worse than a refusal, and that is the bug today; warning would not fix it |

Rejected: warning everywhere — it preserves the current failure mode (a wrong
number is still returned) while merely annotating it. Rejected: erroring on
`print` too, which would leave a user holding an object they cannot even
inspect to find out what it is.

### D2b — `nParams` is internal and renames without ceremony

Every reader is ours: `R/methods_display.R` (print / summary) and
`R/methods_postestimate.R:94`, where it supplies `logLik()`'s `df` attribute.
Users reach it through `logLik()` / `AIC()` / `BIC()`, never directly, so it is
internal-but-reachable and needs no window and no decision.

It is also the reason an old object's `AIC()` fails quietly rather than loudly:
`nParams` is absent, `df` becomes `NULL`, and nothing complains — which is the
concrete argument for D2a's severity split.

### D3 — the reserved `engine` parameter is deleted, not renamed

`new_model_spec(engine = "default")` is reserved for an incremental REM variant
(`rem_rate_fast_spec`) that does not exist. It is never passed a non-default
value and aborts on anything else, so no behavior depends on it. Renaming it
would preserve dead surface; deleting it removes both the dead parameter and a
second meaning of the word `engine` from a codebase that just spent a change
retiring the first one.

The comments at `R/estimation_core.R:357` and `:1199` mention "the future
incremental REM engine" while explaining why estimation state is reusable by
algorithm variants. That reasoning stands on its own and stays; only the naming
of a variant that will not ship goes.

Note `openspec/specs/support-constraint/spec.md` also uses the word
"incremental", for incremental *mask maintenance*. Different concept, untouched.

### D4 — `order_events()` is renamed to `arrange_events()`

The rename of the preprocessed component `orderEvents` → `order_events` (D1's
sweep) put a component name on top of an existing internal function,
`order_events()` in `R/event_streams.R`, which sorts an event stream. Two
unrelated concepts under one name: a 1/2 dependent-vs-right-censored marker, and a
sort.

Nothing breaks, and it is worth being precise about why: R resolves a symbol in
*call position* by searching for a **function** binding, so `order_events(x)`
still finds the function in a scope where a local list named `order_events`
shadows it. The hazard is a reader's, not the interpreter's.

The function is unexported (`@noRd`) with one definition, four call sites in
`R/data_source.R`, and its tests. `arrange_events()` is free, and `arrange()` is
the established verb for this operation, so the new name is unlikely to collide
again. The component keeps `order_events` as D1 specifies.

Rejected: renaming the component instead. The component name is fixed by the
snake_case rule applied to `orderEvents`; the function name is free.

Also renamed: the test helper `make_order_events()` → `make_unorder_events()`. It
builds *unordered* events to feed the function, so translating the old name
literally would describe the opposite of what it makes.

### D5 — two sibling version constants, two distinct component names

Supersedes the earlier intent to give both objects the same component name. The
new information is that `residuals-gof` will **attach the preprocessed object to
the fit** (`estimate_*(return_preprocessed = TRUE)`, its task 1.8), and that on a
flavored fit each process result carries its own preprocessed linkage. So the
nesting is real and imminent, up to three levels deep:

```
flavored_result.goldfish                    (container, unstamped)
└── results[["1"]]  result.goldfish
                    ├── fit_version   = 2L
                    └── preprocessed  preprocessed.goldfish
                                      └── prep_version = 2L
```

One shared component name would put two different facts under one key at two
depths of the same object — indistinguishable in `str()`, in serialization, and to
any helper that reads the key without knowing what it was handed. So:

| | constant | component |
|---|---|---|
| fitted result | `FIT_VERSION <- 2L` | `fit_version` |
| preprocessed | `PREP_VERSION <- 2L` | `prep_version` |

Constants in SCREAMING_SNAKE (permitted by `.lintr`'s
`^[A-Z][A-Z0-9_]*$` regex); components in snake_case, because components of
returned objects are exactly what this change's first requirement governs. Both
constants are siblings in `R/format_version.R` with parallel structure.

**These are the only two.** They supersede `goldfish_result_format` and
`PREPROCESSED_GOLDFISH_VERSION`, which are removed rather than deprecated: the
former was introduced inside this change, and the latter was introduced at `2L`
after the v1.7.0 tag and appears in no release, so no window is owed. `PREP_VERSION`
therefore returns to the value the counter was born with rather than continuing a
dev-only 3L → 4L → 5L sequence.

Two constants rather than one shared value: the two objects can change shape
independently, and a single constant would force a preprocessing-only change to
invalidate every stored fit.

Renaming the component is also what makes resetting the preprocessed counter to
`2L` safe, which is D2's argument reappearing — *the rename is what makes the
detector exact*. Every pre-rename preprocessed object lacks `prep_version`
whatever number it carried under the old `version`:

| stored object | `version` | `prep_version` | guard reads | result |
|---|---|---|---|---|
| CRAN 1.6.12 | absent | absent | NULL | refused |
| dev, 2L era | 2L | absent | NULL | refused |
| dev, 3L / 4L / 5L era | 3L/4L/5L | absent | NULL | refused |
| current | — | 2L | 2L | accepted |

### D5a (revised) — the guards gate on class, for the message rather than the verdict

The first version of this decision argued that without a class gate a misrouted
preprocessed object would be *silently accepted*: the fit's detector reads
`prep$fit_version` → `NULL`, falls back to the retired-component check, finds
none, and reports "current". **D7 removes that fallback, so the claim no longer
holds** — a misrouted prep now reads "outdated" and is refused with or without a
class gate.

The gate is kept for a narrower and honest reason: without it the refusal carries
the wrong diagnosis. A freshly built preprocessed object would be reported as
"fitted before goldfish 2.0.0, when the components of a fitted model were renamed"
— which is false about both its age and its kind, and sends the user to re-fit a
model when the actual mistake was passing the wrong object. So each guard
establishes the object's kind (`inherits(x, "result.goldfish")`) and only then
consults the stamp, and the two mechanisms are separate because they answer
different questions: *what is this?* before *how old is it?*

Rejected: relying on the stamp alone now that absence is conclusive. It reaches the
right verdict by the wrong reasoning, and says so in the error message.

### D6 — the version story is one statement, not a sequence of bumps

Neither counter has ever been public, so the numbering is entirely ours to define.
Both read `2L` under the same convention — *absent or 1 = the unversioned
pre-2.0.0 shape, 2 = the 2.0.0 shape* — and NEWS says that once, for both objects,
rather than narrating a dev-only 4 → 5 step that no user could have observed.
`DESCRIPTION` carries a single bump for the whole change.

### D7 — absence of the stamp is conclusive, for both objects alike

Supersedes the asymmetry D2 and D5 left in place, in which a fit needed a retired
camelCase component to confirm an absent stamp while a preprocessed object did
not. The two objects are recognized by one rule: **no version record means the
object predates the record.**

The asymmetry was an artifact of build order, not a design. The fit's detector was
written while `resModObject` already existed unstamped, so a fallback looked
necessary; the preprocessed detector was written later and correctly made absence
conclusive; and the fit's fallback was then preserved without being re-examined.
The result was a justification for a difference that existed only because a test
fixture had been grandfathered.

**The two designs are indistinguishable on every object a user can possess.**
`logLikelihood` stayed camelCase until this change, so every real pre-2.0.0 fit —
CRAN 1.6.12 and every dev-line build alike — lacks the stamp *and* carries a
retired component, and both rules return "outdated" for it. Every 2.0.0 fit carries
the stamp and both return "current".

| object a user can hold | stamp | retired component | with fallback | absence conclusive |
|---|---|---|---|---|
| CRAN 1.6.12 fit | absent | present | outdated | outdated |
| dev-line fit (1.7.0-1.9.15) | absent | present | outdated | outdated |
| 2.0.0+ fit | present | — | current | current |
| hand-built list in a test | absent | absent | current | **outdated** |

The only objects the fallback changes are hand-built fixtures. It bought nothing
for users and cost a constant, a branch, and the asymmetry — so
`retired_result_components` is removed and the two status functions become
one-line siblings, which is the parallel structure D5 asked for and did not quite
reach.

The two fixtures that relied on the fallback record the layout they emulate
instead (`resModObject` in `R/zzz_testthat_helpers.R`, `fake_fit()` in
`test-baselines_helper.R` — one line each). That is the better arrangement: a
fixture that declares its format fails loudly when the layout moves again, where
one riding a fallback passes silently forever.

Rejected: keeping the retired-component list to refine the wording between "a
1.6.x fit" and "an object with no record". The advice is *re-fit* in both cases, so
the distinction buys a nuance no reader acts on.

Accepted cost: a third party constructing a `result.goldfish` by hand would have
to stamp it, and `FIT_VERSION` is unexported. Nothing does this today — autograph
consumes fits and never builds them, and there is no exported constructor. If it
ever matters the answer is an exported constructor, not a fallback: a constructor
that declares the format is better API than a detector that guesses.

## Risks / Trade-offs

- [The largest user-visible break in the 2.0.0 line] → taken deliberately (D2):
  the objects a window would protect are already broken for unrelated reasons,
  and 2.0.0 is the release to do it in, before the surface freezes.
- [Users upgrade and their scripts break with no transition] → the detector
  (D2/D2a) turns that into a message naming the cause and the fix, which is more
  than they get today; and the NEWS entry lists every old and new spelling.
- [Downstream consumers break] → `autograph` and the `.plan/` harnesses are known
  and in-repo or adjacent; they migrate with the change.
- [A partial sweep is worse than none] → the catalogue above is the completeness
  gate; task 0.1 re-derives it from the code rather than trusting this table.
