# Each backend's per-event likelihood outputs, at a fixed parameter vector,
# against its own frozen references (`_references/likelihood_v1/`). The frozen
# coefficient baselines see a refactor only through the optimum it reaches;
# these see every per-event number on the way, at the living spec's 1e-10.

test_that("every backend reproduces its frozen per-event likelihood outputs", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  references <- readRDS(test_path(
    "_references",
    "likelihood_v1",
    "likelihood_references.rds"
  ))
  grid <- baselines_model_grid()
  expect_setequal(names(references), names(grid))
  data_list <- likelihood_pin_data()

  for (name in names(references)) {
    reference <- references[[name]]
    prep <- likelihood_pin_prep(grid[[name]], data_list)
    for (backend in names(reference$passes)) {
      expected <- reference$passes[[backend]]
      observed <- likelihood_pin_pass(prep, reference$at, backend)
      label <- paste(name, backend)
      expect_setequal(names(observed), names(expected))
      for (field in names(expected)) {
        expect_equal(
          observed[[field]],
          expected[[field]],
          tolerance = 1e-10,
          info = paste(label, field)
        )
      }
    }
  }
})
