# manynet Develop Alignment — the declared floor becomes the used floor

## Why

goldfish declares `manynet (>= 2.1.0)` in `Imports`. It has not been true for
some time, and on 2026-09-06 it stopped being harmless.

Rebuilding the vignettes during `class-naming-scheme` produced fourteen errors
in `vignettes/two-mode.Rmd`, a document that had built cleanly when it was last
committed. The first one is

```
Error in `reserved_cols()` at manynet/R/class_validate.R:82:3:
! 'info$layers' must be of length 2.
```

The cause was measured, not guessed. The vignette calls
`add_info(ties = c("support", "contestation"))` on a two-layer stocnet, and
what that does depends on the manynet version:

| manynet | result of `add_info(ties = c(...))` |
| --- | --- |
| the version the vignette was authored against | sets `info$layers` to the two names |
| **2.3.2** (installed) | stores an unrecognised `info$ties` field; `info$layers` keeps its one-element `as_stocnet()` value |
| **2.3.3** (`a8076517`, tagged, on `origin/develop` and `origin/main`) | `.conform_info_names()` maps `ties` onto the reserved `layers`, and `.check_layer_names()` verifies the count |

So the vignette is not broken by manynet moving on. It broke when manynet
2.3.x reserved `layers`, and it is **already fixed upstream** in 2.3.3, whose
commit message names this case exactly: "'nodes' and 'ties' map onto 'modes'
and 'layers' as `as_stocnet()` maps them." Updating collects a fix rather than
absorbing a hazard.

The version landscape makes the size of the lie plain:

| Where | manynet |
| --- | --- |
| goldfish `DESCRIPTION` floor | 2.1.0 |
| CRAN | 2.3.1 |
| installed locally | 2.3.2 |
| `origin/develop` / `origin/main`, tagged `v2.3.3` | 2.3.3 |
| what goldfish's own vignettes require | **2.3.3** |

An untrue floor is worse than a high one. It converts a clear install-time
failure into a confusing runtime one, which is exactly how this was found: as
fourteen errors inside a document, rather than as an unmet dependency.

The decision to track manynet's develop branch for the rest of the pre-2.0.0
line, rather than pin to CRAN and code defensively across versions, is
ADR-0047.

## What Changes

- **BREAKING for developers, not for users** — `DESCRIPTION` raises
  `Imports: manynet` from `>= 2.1.0` to the version goldfish actually uses.
  The number is confirmed by task 1, not assumed; 2.3.3 is the expectation.
  A contributor whose manynet is older now fails at install time with an
  unmet dependency instead of inside a vignette.
- `vignettes/two-mode.Rmd` is rebuilt against the aligned manynet and the
  rebuild is verified error-free. It is currently committed at a state that
  predates the drift, so the shipped document is right by accident: nothing
  regenerated it since manynet changed.
- The `single-data-object` capability gains a requirement fixing **which
  manynet behavior goldfish relies on**, so the seam is written down rather
  than rediscovered. goldfish's manynet surface is nine verbs in `R/`
  (`make_stocnet`, `as_stocnet`, `bind_changes`, `from_ties`, `bind_ties`,
  `add_info`, `join_nodes`, `rename_nodes`) plus `mutate_globals` and
  `irps_nuclear` in the vignettes — small enough to state precisely.
- A vignette rebuild becomes the **named** check for this seam. It is already
  the only thing exercising manynet end-to-end; the unit tests cannot see
  drift here because they build fixtures directly rather than through
  manynet's verbs. Naming it turns an accident into a procedure.
- A 2.0.0 release blocker is recorded: **the floor must name a manynet version
  available on CRAN before goldfish releases.** 2.3.3 is tagged but not
  published, so goldfish's declared dependency is temporarily unsatisfiable
  from CRAN alone. That is acceptable only while goldfish is itself
  unreleased (ADR-0016).

## Capabilities

### Modified Capabilities

- `single-data-object`: adds the manynet compatibility floor and the behavior
  goldfish depends on it for, to the capability that already owns the manynet
  delegation seam ("manynet SHALL be listed in Imports pinned `>= 2.1.0`" is
  the sentence going stale, and it lives there).

## Impact

- **goldfish — code**: `DESCRIPTION` only. No `R/` change is expected; the fix
  is upstream, and goldfish's calls are already written for the behavior 2.3.3
  restores. Task 1 verifies that expectation before task 2 acts on it.
- **goldfish — docs**: `vignettes/two-mode.Rmd` rebuilt.
- **Contributors**: must install manynet from `origin/develop` (or the
  `v2.3.3` tag) before rebuilding vignettes or running
  `devtools::check(vignettes = TRUE)`.
- **Users**: none while goldfish is unreleased. At 2.0.0 this becomes a
  release blocker, not a user-facing change.
- **Not affected**: the C++ core, the frozen 1e-6 baselines, and the test
  suite — which never reaches manynet's verbs, and is the reason this drifted
  unseen.

## Out of scope

- **The `dynami-example.Rmd` failure is not this change's.** The same rebuild
  produced 26 errors there, and the cause is different: `estimate_dynami()`
  aborts with "Effect 'intercept' (1) cannot initialized with objects
  interactions / 'what' must be a function or character string" against the
  **installed** goldfish, while the identical call passes under
  `devtools::load_all()`. Installing the pre-rename commit `e505c85`
  reproduces it, so it predates `class-naming-scheme` and has nothing to do
  with manynet. It is an install-versus-`load_all()` divergence in the DyNAM-i
  effect-init path (`create_effects_functions()` resolves effect functions by
  `eval(parse(text = ...))` against two different environments), and it is
  **invisible to the test suite, which runs entirely under `load_all()`**. It
  needs its own change; folding it here would put two unrelated causes behind
  one title.
- Raising the floor for `autograph`. That bound is also understated after
  `class-naming-scheme` (plot dispatch now needs an autograph past `03ec996`),
  but it is a `Suggests`, it blocks nothing, and it waits on an autograph
  release that does not exist yet.
