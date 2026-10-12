# The DyNES augmenter reference fixtures of `_references/walk_alignment_v1/`:
# a 12-actor flavored friendship panel beside a calls stream, waves
# c(0, 5, 10, 15), with no stamp tie, no observed event at a wave time and no
# composition change. The coupled variant supports calls only on present
# friendship ties. The builders mirror the generator script beside the
# references; the panel ties and theta are read back from the saved file.

wa_reference <- function() {
  readRDS(test_path(
    "_references",
    "walk_alignment_v1",
    "walk_alignment_pre_change.rds"
  ))
}

wa_data <- function(ties) {
  add_flavor(
    list(
      info = list(
        name = "walk_alignment",
        focal = "calls",
        update = c(friendship = "increment", calls = "increment"),
        directed = c(friendship = TRUE, calls = TRUE),
        observation = c(friendship = "panel", calls = "event")
      ),
      nodes = data.frame(
        label = paste0("N", 1:12),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = ties
    ),
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

wa_spec <- function(data, coupled) {
  friendship <- make_specification(
    rate = list(creation ~ 1, dissolution ~ 1),
    choice = list(creation ~ recip, dissolution ~ recip),
    layer = "friendship",
    model = "DyNAM",
    data = data
  )
  calls <- if (coupled) {
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ inertia + tie(friendship),
      support_constraint = ~ tie(friendship),
      layer = "calls",
      model = "DyNAM",
      data = data
    )
  } else {
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    )
  }
  make_joint_specification(friendship, calls, data = data)
}

# The sim augmenter on one reference fixture, built as estimate_dynes() builds
# it, with the fixture's theta and its recorded draws.
wa_sim_fixture <- function(name, reference = wa_reference()) {
  fixture <- reference[[name]]
  waves <- attr(reference, "waves")
  spec <- wa_spec(wa_data(fixture$ties), coupled = name == "coupled")
  completed <- suppressWarnings(complete_generative_spec(
    spec,
    consumer = "estimate_dynes",
    wave_times = waves
  ))
  recipe <- compile_augmentation_recipe(completed, waves)
  control <- set_algorithm_em(
    augmenter = set_augmenter_options(routine = "sim"),
    seed = 11L
  )
  list(
    spec = completed,
    recipe = recipe,
    waves = waves,
    theta = fixture$theta,
    draws = fixture$sim_draws,
    augmenter = augment_seq_sim(completed, recipe, control)
  )
}

# A draw reduced to what the references record.
wa_draw_record <- function(draw) {
  ties <- as.data.frame(draw$data$ties)
  timed <- ties[
    ties$layer == "friendship" & !is.na(ties$time),
    c("from", "to", "time", "weight", "layer")
  ]
  timed <- timed[order(timed$time, timed$from, timed$to), ]
  rownames(timed) <- NULL
  list(
    friendship = timed,
    log_proposal = draw$log_proposal,
    log_proposal_raw = draw$log_proposal_raw,
    restarted = draw$restarted
  )
}

wa_seeded_draw <- function(fixture, seed) {
  withr::with_seed(seed, fixture$augmenter$draw(1L, 1L, fixture$theta)[[1L]])
}
