---
name: regen-baselines
description: Guarded, deliberate regeneration of the frozen coefficient / C++ golden baselines. Almost never the right move during a refactor — defaults to creating a NEW versioned baseline, never silently overwriting the frozen floor. User-invoked only.
disable-model-invocation: true
---

# regen-baselines

The coefficient baselines (`tests/testthat/_baselines/coefficient_baselines_v1.rds`,
`_baselines/global_v1/coefficient_baselines_global.rds`) and the C++ golden tests
are the package's **1e-6 regression floor** (design D18). They were generated at a
frozen pre-refactor commit and are asserted by `test-coefficient_baselines.R` /
`test-coefficient_baselines_global.R`. A failure there means a regression — the fix
is (almost always) to the code, **not** the baseline.

This skill exists so that the rare, legitimate regeneration is a **deliberate,
documented, versioned** act — not an accident. It is user-invoked only.

> ⚠️ `Edit`/`Write` to `_baselines/` is denied by a PreToolUse hook. The generator
> writes via `Rscript` + `saveRDS`, which bypasses that guard — so this skill must
> not run without an explicit, recorded justification.

## Step 0 — Stop and ask why (blocking)

Before doing anything, establish and record the justification. Regeneration is
legitimate ONLY when the numerics changed **intentionally** (e.g. a corrected
statistic, a new effect definition, an intended algorithm change) — never to
"make a failing test pass" during a behaviour-preserving refactor.

Ask the user:
- What changed the numerics, and why is the new value correct?
- Is this a behaviour-preserving refactor? → **If yes, do NOT regenerate.** The
  test failure is the regression it is designed to catch; fix the code.

If there is no documented, intentional reason, **stop here.**

## Step 1 — Default to a NEW version, never overwrite v1

Overwriting the frozen `*_v1.rds` destroys the historical floor. Prefer bumping the
version:

1. Copy the generator to a new version and change its output path
   (`coefficient_baselines_v2.rds`), OR parameterise the existing generator.
2. Keep `v1` in place; add `test-coefficient_baselines.R` coverage for `v2` (or
   migrate the assertion with a NEWS/design note explaining the version bump).

Only overwrite `v1` in place if the user explicitly confirms the old floor is being
*retired* (rare) — and record that decision.

## Step 2 — Show what moved (before writing anything)

Run the fits and diff the new coefficients / logLik against the current baseline so
the user can see exactly which models/engines changed and by how much:

```r
NOT_CRAN=true Rscript -e '
  devtools::load_all(".")
  source("tests/testthat/helper-baselines.R")
  old <- readRDS("tests/testthat/_baselines/coefficient_baselines_v1.rds")
  d <- list(social_evolution = baselines_social_evolution_data(),
            fisheries = baselines_fisheries_data())
  g <- baselines_model_grid()
  for (m in names(g)) for (e in baselines_engines) {
    fit <- baselines_fit(g[[m]], e, d)
    dc  <- max(abs(coef(fit) - old[[m]][[e]]$coef))
    cat(sprintf("%-28s [%s] max|dcoef|=%.3e  dlogLik=%.3e\n",
                m, e, dc, as.numeric(logLik(fit)) - old[[m]][[e]]$logLik))
  }'
```

Present the diff table and confirm with the user that every non-zero delta is
expected before writing.

## Step 3 — Regenerate (only after confirmation)

```sh
NOT_CRAN=true Rscript tests/testthat/_baselines/generate_coefficient_baselines.R
```

(Adjust the output path/version per Step 1. The `global_v1` set is generated
separately — see `_baselines/README.md`.)

## Step 4 — Record the provenance

- Update `tests/testthat/_baselines/README.md`: the new version, the generating
  commit, and the documented justification.
- Note it in `NEWS.md` if the numeric change is user-visible.
- Re-run `NOT_CRAN=true` and confirm the baseline tests PASS (not SKIP).

## Guardrails
- Never regenerate to silence a failing test during a behaviour-preserving change.
- Never overwrite `*_v1.rds` without explicit "retire the old floor" confirmation.
- Always show the coefficient diff before writing.
- Always record the justification + commit in the README.
