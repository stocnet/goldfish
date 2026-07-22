# Well-formedness of the imputation-policy regression fixtures: each carries the
# missingness its later behavioral tests rely on, at the shape they name, and
# resolves through the stocnet source accessors.

test_that("the nodal fixture carries by-design and sparse-numeric NA", {
  fixture <- make_stocnet_fixture_missing_nodal()
  nodes <- fixture$nodes

  citizens <- nodes$mode == "citizen"
  expect_true(all(is.na(nodes$party[citizens])))
  expect_false(anyNA(nodes$party[!citizens]))

  # Sparse numeric: some but not all values missing, across both categories.
  expect_true(anyNA(nodes$income))
  expect_false(all(is.na(nodes$income)))

  src <- new_data_source(data = fixture, focal = "contact")
  expect_true(anyNA(ds_attribute(src, "nodes", "party")))
  expect_true(anyNA(ds_attribute(src, "nodes", "income")))
})

test_that("the missing-global fixtures expose the NA at the named time", {
  init <- new_data_source(
    data = make_stocnet_fixture_missing_global("init"),
    focal = "calls"
  )
  expect_true(ds_is_global(init, ".global"))
  expect_true(is.na(ds_global_value(init, "climate")))

  event <- new_data_source(
    data = make_stocnet_fixture_missing_global("event"),
    focal = "calls"
  )
  # An observed initial value, then a timed replace that is missing.
  expect_equal(ds_global_value(event, "climate"), 0)
  stream <- event$streams$global$climate
  timed <- stream[!is.na(stream$time), ]
  expect_true(is.na(timed$value[[1]]))
})
