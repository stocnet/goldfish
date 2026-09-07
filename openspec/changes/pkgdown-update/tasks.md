# Tasks — pkgdown-update

Disciplines (openspec/config.yaml): one focused conventional commit per
task, tests green at every commit, `devtools::document()` inline whenever
roxygen changes, `air format` touched R files, `NOT_CRAN=true` baselines
PASS before each commit. Apply AFTER residuals-gof phase 2 (its man pages
must exist).

## 1. Reference index

- [ ] 1.1 `_pkgdown.yml`: add `starts_with("test")` and `evaluate_model`
      to the Diagnostics section; index the post-estimation method topics
      including the re-wired `augment`; run `pkgdown::check_pkgdown()`
      until clean (fix any other uncovered topic it surfaces).
- [ ] 1.2 `pkgdown::build_reference()` warning-free on the updated yaml;
      spot-check the rendered Diagnostics section groups the
      residuals-gof surface as intended.

## 1a. Vignette tooling (added 2026-07-27, user decision)

- [ ] 1.3 Fix `vignettes/precompile.R`: honor its `vignette_to_run`
      argument (today it regenerates all four vignettes regardless) and
      stop re-rendering `README.Rmd` as a side effect (precompile.R:88-90)
      — README rendering becomes its own explicit call, not part of
      vignette precompilation. Verify by precompiling a single vignette
      and confirming the other three `.Rmd` and `README.md` are untouched.
      (Recorded twice in progress journals as an unowned bug; the stale
      teaching/dynami vignettes get regenerated against the current print
      methods in the same session, since this change runs right before
      release-prep.)

## 1b. Website CI gate (added 2026-09-03, autograph parity)

- [ ] 1.4 Add a `website-builds` job to `.github/workflows/prchecks.yml`,
      mirroring the one autograph added in `b76438a`: `setup-r` +
      `setup-r-dependencies` with `extra-packages: any::pkgdown, local::.`
      and `needs: website`, then `pkgdown::check_pkgdown()` followed by
      `pkgdown::build_site(preview = FALSE, install = FALSE,
      new_process = FALSE)`. The site itself keeps deploying from
      `pushrelease.yml`; this job only reports whether it *can* build, so a
      topic dropped from the reference index fails the PR instead of
      silently stopping the site at the next release. Land it AFTER task
      1.1, since `check_pkgdown()` is red until the index gaps close.
      Note the job runs on `pull_request` to `main` only, matching the
      existing workflow trigger.

## 2. Examples gate and timing ledger

- [ ] 2.1 Run all examples in a fresh subprocess
      (`devtools::run_examples()`, `\donttest` included) with per-topic
      timing; fix any failing example; append the first
      `topic, seconds, date, version` rows to `.plan/example_timings.csv`
      (local-only ledger).
- [ ] 2.2 Wrap any topic measuring over ~5s (outside `\donttest`) with
      `\donttest` + one-line rationale, keeping a fast demonstrative
      part; `devtools::document()`; re-run the gate to confirm the
      threshold holds.
- [ ] 2.3 Record the gate as a step in the release pre-flight notes
      (release-prep skill checklist) and in this change's progress.md;
      NEWS entry only if user-visible examples changed; DESCRIPTION bump
      per milestone discipline.
