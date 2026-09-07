# Tasks — manynet-develop-alignment

Drafted 2026-09-06 from ADR-0047. Small change; the ordering is the point
(design D1: measure the floor before declaring it).

## 1. Measure the floor

- [ ] 1.1 Install manynet from `origin/develop` (or tag `v2.3.3`) into the
  development library, recording the exact version and commit sha in
  `progress.md`. The local checkout at `~/Documents/repos/manynet` is on
  `develop` but 13 commits behind `origin/develop` as of 2026-09-06.
- [ ] 1.2 Rebuild `vignettes/two-mode.Rmd` (install goldfish first, then
  `Rscript vignettes/rebuild-all.R`) and record whether the fourteen errors
  are gone. Compare error counts against the committed document rather than
  eyeballing: `grep -cE '^(#>|##) *(Error|! )'` before and after.
- [ ] 1.3 **Stop and report if the errors persist.** The change's premise is
  that manynet 2.3.3's `a8076517` fixes this; if it does not, the floor is not
  the answer and the shape of the change has to be revisited rather than the
  bound raised to a guess (design D1).
- [ ] 1.4 Record which other vignettes the rebuild changes. `two-mode` is the
  one known to touch the changed `add_info()` behavior;
  `multivariate-specification` also calls `add_info()` and may or may not
  reach it (design, open question 2).

## 2. Declare it

- [ ] 2.1 Raise `Imports: manynet` in `DESCRIPTION` to the measured version.
  Do not write 2.3.3 unless task 1 measured 2.3.3.
- [ ] 2.2 Commit the rebuilt vignettes together with the bump, so the document
  and the bound that makes it build travel in one commit.
- [ ] 2.3 Verification: `devtools::check(vignettes = TRUE)` — this change is
  precisely the one where excluding vignettes from the check would hide the
  thing being fixed. Record the warning/note counts against the baseline in
  `progress.md`; three warnings are pre-existing (non-ASCII, a
  `walk_handle.Rd` link, a `margin_table.Rd` usage block).

## 3. Contributor install path

- [ ] 3.1 Determine whether `devtools::install_deps()` on a clean library
  resolves an acceptable manynet without a `Remotes:` entry. CRAN carries
  2.3.1, so the expectation is that it does not.
- [ ] 3.2 If it does not, add `Remotes: stocnet/manynet@develop` with a
  comment naming the manynet release that removes it again, and confirm
  `R CMD check --as-cran` treats it as a NOTE rather than a WARNING
  (design D4).

## 4. Write down the seam

- [ ] 4.1 Fold the two added requirements into the `single-data-object`
  living spec via this change's delta: the compatibility floor (with the
  `add_info()` behavior and the nine-verb surface it names) and the
  vignette-rebuild check.
- [ ] 4.2 Add "the manynet bound names a CRAN-available version" to the
  `release-prep` pre-flight checklist, not only to this change (design D5) —
  the change archives, the checklist does not.
- [ ] 4.3 Verification: `bash .plan/opsx-spec-placement-check.sh
  manynet-develop-alignment` exits 0, and `openspec validate
  manynet-develop-alignment --strict` passes.

## 5. Close

- [ ] 5.1 **not-cran-test** on the whole suite. Expected to be unaffected —
  the suite never reaches manynet's verbs, which is the gap this change
  documents rather than closes — so a moved coefficient here would mean
  something other than a dependency bump happened.
- [ ] 5.2 `NEWS.d/` fragment (ADR-0040: this is a branch, so no `DESCRIPTION`
  Version bump and no `NEWS.md` edit; the manynet *Imports* bump in task 2.1
  is not a package Version bump and is in scope). One bullet: the raised
  manynet requirement and what a contributor must install.
- [ ] 5.3 Record in `progress.md` whether the DyNAM-i
  install-versus-`load_all()` failure is still present, so the successor
  change has a dated observation rather than a recollection.
