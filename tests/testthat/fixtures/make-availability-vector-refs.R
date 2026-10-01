# Regenerate tests/testthat/fixtures/availability_vector_refs.rds.
#
# Run from the package root:
#   Rscript tests/testthat/fixtures/make-availability-vector-refs.R
#
# Freezes, for each of `availability_vector_families()` (a REM with an alter-,
# an ego- and a global-kind constraint and receiver composition changes, and a
# coordination model with an alter-kind constraint), the folded availability
# stream as the dense point fold stored it, and the estimates on the r, cpp
# and gather backends. The stream is the compact form of the dense cell values
# at every stored row: `availability_cells()` replays it. A fold that stores
# the same constraint as two factor vectors must reproduce every cell and every
# estimate.
#
# Captured 2026-10-01 on the point fold. Regenerating after the fold moves to
# factor vectors re-captures the outer form: check `availability_cells()` of
# each new entry is identical to the old entry's, and the estimates equal
# within 1e-10, before committing the new file.
#
# Re-saved 2026-10-01 in the outer form once the fold kept these constraints
# as two factor vectors: every entry's cells identical to the point capture at
# every row, the estimates within 4e-15 on r and identical on cpp and gather.
suppressMessages(devtools::load_all(quiet = TRUE))
source("tests/testthat/helper-constraint-atoms.R")

availability_fields <- c(
  "event_time",
  "active_dyad_encoding",
  "active_dyad_init",
  "active_dyad_update",
  "active_dyad_update_pointer",
  "active_sender_init",
  "active_sender_update",
  "active_sender_update_pointer"
)

refs <- lapply(availability_vector_families(), function(family) {
  prep <- availability_vector_run(family)
  fits <- lapply(c("r", "cpp", "gather"), function(backend) {
    fit <- availability_vector_run(family, backend)
    list(coef = coef(fit), log_likelihood = fit$log_likelihood)
  })
  names(fits) <- c("r", "cpp", "gather")
  list(stream = prep[intersect(availability_fields, names(prep))], fits = fits)
})
names(refs) <- availability_vector_families()

saveRDS(
  refs,
  "tests/testthat/fixtures/availability_vector_refs.rds",
  version = 2
)
cat("wrote availability_vector_refs.rds:", names(refs), "\n")
