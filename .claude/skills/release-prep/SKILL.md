---
name: release-prep
description: Local pre-flight for a goldfish release — precompile vignettes/README, spell-check, coverage report, build the pkgdown site, run CRAN checks, and assemble the CRAN-submission file. Complements the pushrelease.yml CI (which renders + builds on merge to main) and wires in the r-lib:cran-extrachecks and open-source:create-release-checklist skills. User-invoked before opening the release PR.
disable-model-invocation: true
---

# release-prep

The local pre-flight to run **before opening the release PR into `main`**.
`pushrelease.yml` handles rendering vignettes/README and building the platform
tarballs *after* the PR merges — this skill does the checks that must pass first,
so nothing surprising happens on `main`. Run from the package root.

Do not invent bookkeeping this repo already automates: **start** by delegating the
checklist + GitHub issue to the existing skill, and **finish** the CRAN compliance
sweep with the existing CRAN skill. This skill is the R-build glue between them.

## 0. Open the release checklist (delegate)

Invoke **`/open-source:create-release-checklist`** to generate the release
checklist and tracking issue (version bump, NEWS finalization, reverse-dependency
notes). Confirm before continuing:

- `DESCRIPTION` `Version:` is bumped and `NEWS.md` has the release section
  (per the repo's per-milestone discipline — see `openspec/config.yaml`).
- Working tree clean except the intended release changes.

## 1. Precompile vignettes + README (mirrors the CI render step)

`pushrelease.yml` runs this on merge; run it locally first so the rendered
artifacts are reviewed before they land:

```bash
Rscript ./vignettes/rebuild-all.R
```

## 2. Spell check

```bash
Rscript -e 'devtools::spell_check()'
```

Resolve real misspellings; add deliberate terms to `inst/WORDLIST`
(`spelling::update_wordlist()`), not by silencing the check.

## 3. Test coverage report

```bash
Rscript -e 'covr::report(x = covr::package_coverage(type = "tests"))'
```

Run with `NOT_CRAN=true` if you want the baseline/golden tests counted in
coverage. Review uncovered lines in changed code; this mirrors `test-coverage.yaml`.

## 4. Build the pkgdown site

```bash
Rscript -e 'pkgdown::build_site()'
```

Check that new exports/vignettes render and the reference index has no missing
topics (`_pkgdown.yml`).

## 5. CRAN checks + submission file

```bash
# Full check as CRAN sees it (manual + remote where available).
Rscript -e 'devtools::check(manual = TRUE, remote = TRUE, error_on = "warning")'
```

Then update **`cran-comments.md`** (already in the repo): the R CMD check summary
(0 errors / 0 warnings / note explanations), test environments, and
reverse-dependency results. For wider platform coverage, note
`rhub::rhub_check()` / `devtools::check_win_devel()` if this is a CRAN submission.

## 6. CRAN extra-checks sweep (delegate)

Invoke **`/r-lib:cran-extrachecks`** to catch the ad-hoc CRAN requirements that
`devtools::check()` does **not** flag (DESCRIPTION field conventions, URL
validation, `\value` in `.Rd`, example runtime, administrative items). Fix
anything it surfaces before finalizing `cran-comments.md`.

## Sequence summary

1. `/open-source:create-release-checklist` → checklist + issue; confirm version/NEWS.
2. `vignettes/rebuild-all.R` → rendered vignettes/README.
3. `devtools::spell_check()`.
4. `covr::report(covr::package_coverage(type = "tests"))`.
5. `pkgdown::build_site()`.
6. `devtools::check(...)` + update `cran-comments.md`.
7. `/r-lib:cran-extrachecks` → CRAN compliance sweep.

Only open the release PR into `main` once 2–7 are clean. Merging then triggers
`pushrelease.yml` (render + multi-platform build + release assets).
