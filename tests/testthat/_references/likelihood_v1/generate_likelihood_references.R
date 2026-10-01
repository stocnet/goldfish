# Frozen per-event likelihood pins.
#
# One engine pass per baseline grid entry (six families on Social Evolution and
# Fisheries) and per backend, at the converged cpp coefficients, with every
# opt-in component requested. Each backend is pinned against ITSELF: the
# three-way cross-backend agreement is test-backend_parity.R's job, these
# references exist so a rewrite of one backend's per-event loop can be shown
# to move none of its own numbers past 1e-10.
#
# Run from the package root, on the code state the pins are meant to freeze:
#
#   NOT_CRAN=true Rscript \
#     tests/testthat/_references/likelihood_v1/generate_likelihood_references.R
#
# Regenerating overwrites the floor; do it only under the rule the frozen
# baselines follow (a reference moves only when the new value was derived
# beforehand), and say so in the commit.

suppressMessages(devtools::load_all(".", quiet = TRUE))
source("tests/testthat/helper-baselines.R")
source("tests/testthat/helper-likelihood-pins.R")
options(lifecycle_verbosity = "quiet")

grid <- baselines_model_grid()
data_list <- likelihood_pin_data()

references <- lapply(names(grid), function(name) {
  entry <- grid[[name]]
  fit <- suppressWarnings(baselines_fit(entry, "cpp", data_list))
  stopifnot(isTRUE(fit$convergence$is_converged))
  at <- unname(fit$parameters)
  prep <- likelihood_pin_prep(entry, data_list)
  passes <- lapply(
    stats::setNames(nm = baselines_backends),
    function(backend) likelihood_pin_pass(prep, at, backend)
  )
  cat(name, "done\n")
  list(at = at, passes = passes)
})
names(references) <- names(grid)

attr(references, "commit") <- system(
  "git rev-parse --short HEAD",
  intern = TRUE
)
saveRDS(
  references,
  "tests/testthat/_references/likelihood_v1/likelihood_references.rds",
  compress = "xz"
)
