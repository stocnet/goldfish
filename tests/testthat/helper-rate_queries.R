# Shared rate-query fixtures: a small stocnet with deliberate stamp ties, a
# candidate that moves its calls ties in time, and a completed joint
# specification with the sub-model descriptors that number its free layout.

# calls is modeled; friendship and emails are exogenous layers the formulas
# read. Stamps are tied on purpose. The schedule at t = 2 runs: the dependent
# rows of calls events 2 and 3 (rows 4, 5), friendship 3 -> 4 (row 6), the
# realizations of calls events 2 and 3 (rows 7, 8), emails 2 -> 1 (row 9).
rq_tied_data <- function(calls = "event") {
  ties <- rbind(
    data.frame(
      from = c(1L, 3L, 4L),
      to = c(2L, 4L, 5L),
      time = c(1, 2, 3),
      layer = "friendship"
    ),
    data.frame(
      from = c(1L, 2L, 3L, 4L),
      to = c(2L, 3L, 4L, 5L),
      time = c(1, 2, 2, 3),
      layer = "calls"
    ),
    data.frame(
      from = c(2L, 5L),
      to = c(1L, 2L),
      time = c(2, 3),
      layer = "emails"
    )
  )
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(
        friendship = "increment",
        calls = "increment",
        emails = "increment"
      ),
      directed = c(friendship = TRUE, calls = TRUE, emails = TRUE),
      observation = c(friendship = "panel", calls = calls, emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = ties
  )
}

# The same data with the calls ties moved in time: what an augmented
# candidate changes.
rq_candidate_data <- function() {
  data <- rq_tied_data()
  is_calls <- data$ties$layer == "calls"
  data$ties$time[is_calls] <- data$ties$time[is_calls] + 0.5
  data
}

# A completed joint specification whose layout has every slot kind: timed rates
# (a leading intercept), an offset between two free choice effects, and an
# autocompleted uniform choice on emails. Free slots are numbered 1..6 in
# canonical order: calls rate (1, 2), calls choice (3, 4), emails rate (5, 6).
rq_layout_spec <- function(calls = "event") {
  suppressWarnings(complete_generative_spec(rq_layout_raw_spec(calls)))
}

# rq_layout_spec() before completion: its three authored fids are calls rate
# (1), calls choice (2) and emails rate (3).
rq_layout_raw_spec <- function(calls = "event") {
  data <- rq_tied_data(calls)
  make_joint_specification(
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ inertia + offset(tie(friendship), coef = -0.5) + tie(emails),
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

rq_layout_submodels <- function(calls_choice = 3:4) {
  list(
    new_pool_submodel(
      calls ~ 1 + indeg(friendship),
      "DyNAM",
      "rate",
      1:2,
      fid = 1L
    ),
    new_pool_submodel(
      calls ~ inertia + offset(tie(friendship), coef = -0.5) + tie(emails),
      "DyNAM",
      "choice",
      calls_choice,
      fid = 2L
    ),
    new_pool_submodel(emails ~ 1 + indeg, "DyNAM", "rate", 5:6, fid = 3L)
  )
}

# The hand-numbered sub-models over rq_layout_spec()'s pool engine spec.
rq_layout_decomposition <- function(calls_choice = 3:4) {
  new_pool_decomposition(
    rq_layout_submodels(calls_choice),
    pool_engine_spec(rq_layout_spec())
  )
}
