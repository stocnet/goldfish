# Content guard on the frozen coefficient baselines.
#
# The PreToolUse hook that protects `_baselines/` matches the Edit / Write /
# MultiEdit tools on their `file_path`, so it cannot see a write that arrives
# through the shell -- and it must not, because the sanctioned regeneration path
# is `Rscript .../generate_coefficient_baselines.R` + `saveRDS()`, which is a
# shell write. Closing that path at the tool layer would block the one
# legitimate writer, so the guard lives here at the content layer instead: this
# fails whichever tool modified the file.
#
# Deliberately NOT checksummed: `coefficient_baselines_v2.rds`. Its `gather`
# column is expected to be regenerated whenever gather numerics legitimately
# change, so pinning it would turn a supported operation into a test failure.
# (Its `r` and `cpp` columns are v1's numbers carried forward bit-identically,
# and v1 *is* pinned below, so the frozen floor stays guarded either way.)
# The omission is intentional -- please do not "fix" it by adding v2 here.
v1_digest <- "f3e171266d40f9bb150157280cd349b6"
global_v1_digest <- "4771894fc10d1ab29d83258555414d53"
frozen_baseline_digests <- c(
  "coefficient_baselines_v1.rds" = v1_digest,
  "global_v1/coefficient_baselines_global.rds" = global_v1_digest
)

test_that("the frozen coefficient baselines are unmodified", {
  paths <- test_path("_baselines", names(frozen_baseline_digests))
  # Keyed by the relative name, not the absolute path, so a failure names the
  # file that moved rather than printing two anonymous hex strings.
  digests <- stats::setNames(
    unname(tools::md5sum(paths)),
    names(frozen_baseline_digests)
  )
  expect_equal(digests, frozen_baseline_digests)
})
