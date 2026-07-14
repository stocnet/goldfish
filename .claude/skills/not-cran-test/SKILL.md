---
name: not-cran-test
description: Run the goldfish test suite with NOT_CRAN=true and confirm the frozen 1e-6 baselines report PASS (not SKIP). Use to verify a change before committing an OpenSpec task.
---

# not-cran-test

Run goldfish's full test suite the way its verification discipline requires:
`NOT_CRAN=true`, so the coefficient baselines and the C++ golden tests actually
execute instead of `skip_on_cran()`-ing. A bare `Rscript` run silently SKIPs the
1e-6 regression floor — this skill exists to prevent that footgun.

## Run

From the package root:

```bash
NOT_CRAN=true Rscript -e '
  suppressMessages(devtools::load_all("."))
  library(testthat)
  res <- test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
  df <- as.data.frame(res)
  cat("\n=== TOTALS ===\n")
  cat("PASS:", sum(df$passed), " FAIL:", sum(df$failed),
      " SKIP:", sum(df$skipped), " WARN:", sum(df$warning), "\n")
  fails <- df[df$failed > 0 | df$error, c("file", "test", "failed", "error")]
  if (nrow(fails)) { cat("\n=== FAIL/ERROR ===\n"); print(fails) }
  baseline <- df[grepl("baseline|golden", df$file), c("file", "passed", "skipped")]
  cat("\n=== BASELINE / GOLDEN FILES (must be PASS, not SKIP) ===\n")
  print(baseline)
'
```

The suite is slow (minutes) — run it with `run_in_background: true` and wait for
the completion notification rather than polling.

## Interpret

Report **PASS / FAIL / SKIP** totals. The run only verifies the change if:

- **FAIL = 0** and **ERROR = 0**.
- The baseline/golden files (`test-coefficient_baselines*.R`, the C++ golden
  tests) show **passed > 0 and skipped == 0**. If any baseline shows `skipped > 0`,
  the 1e-6 floor did **not** run — treat the verification as **incomplete**, not
  green, and check that `NOT_CRAN=true` was actually set.
- Note that ~19 pre-existing dissolve/imputation WARNINGS are expected and not a
  failure.

Never regenerate or edit `tests/testthat/_baselines/` to make a run pass — those
are the frozen regression floor (design D18). A baseline mismatch is a signal to
fix the code, not the baseline.