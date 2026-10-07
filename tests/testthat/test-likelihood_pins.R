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
      # The score is pinned at the optimum, where it is a near-zero sum of
      # large per-event terms: a reordered sum moves it by rounding that is
      # tiny against those terms but large against the near-zero total. It is
      # held at 1e-10 of the summed magnitude of its terms (the frozen event
      # scores) rather than of itself.
      score_field <- intersect(c("derivative", "score"), names(expected))
      n_parameters <- length(expected[[score_field]])
      score_scale <- colSums(abs(matrix(
        expected$event_scores,
        ncol = n_parameters
      )))
      expect_true(
        all(
          abs(observed[[score_field]] - expected[[score_field]]) <=
            1e-10 * score_scale
        ),
        info = paste(label, score_field)
      )
      for (field in setdiff(names(expected), score_field)) {
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
