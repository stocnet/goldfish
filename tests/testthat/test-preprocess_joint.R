# The merged-walk substrate for a joint specification: each process is compiled
# into a walk-ready spec_map, the maps are grouped into statistic blocks, and one
# shared state container and one shared event schedule are built over the union
# of the processes' objects and events. No event loop runs here -- these read the
# compiled substrate structure directly.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A plain two-process join over one node set: calls and emails are both DyNAM
# rate + choice processes, and friendship is a panel-observed layer both choice
# formulas read as an exogenous covariate. Four fids (calls rate/choice, emails
# rate/choice) across two statistic blocks (DyNAM:rate sender, DyNAM:choice
# dyad). indeg is focal-relative (splits per process); tie(friendship) is
# absolute (pools to one column).
joint_two_process <- function() {
  nodes <- data.frame(
    label = paste0("N", 1:6),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L),
      to = c(2L, 3L, 4L, 5L),
      time = c(1, 2, 3, 4),
      layer = "friendship"
    ),
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls"
    ),
    data.frame(
      from = c(2L, 3L, 4L),
      to = c(1L, 2L, 3L),
      time = c(1, 2, 3),
      layer = "emails"
    )
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(
      friendship = "increment",
      calls = "increment",
      emails = "increment"
    ),
    directed = c(friendship = TRUE, calls = TRUE, emails = TRUE),
    observation = c(friendship = "panel", calls = "event", emails = "event")
  )
  data <- list(info = info, nodes = nodes, ties = ties)
  calls_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  make_joint_specification(calls_spec, emails_spec, data = data)
}

# ---- Per-process compilation and block grouping -----------------------------

test_that("build_merged_blocks compiles one spec_map per process, grouped by block", {
  mb <- build_merged_blocks(joint_two_process())

  expect_s3_class(mb, "merged_blocks.goldfish")
  # One unit per (focal, family): calls/emails x rate/choice.
  expect_setequal(
    names(mb$units),
    c("calls:rate", "calls:choice", "emails:rate", "emails:choice")
  )
  # Each unit carries a walk-ready spec_map resolved against its own focal.
  for (key in names(mb$units)) {
    expect_s3_class(mb$units[[key]]$spec_map, "spec_map.goldfish")
  }
  expect_equal(mb$units[["emails:choice"]]$spec_map$focal, "emails")
  expect_equal(mb$units[["calls:rate"]]$spec_map$focal, "calls")

  # Two statistic blocks, each owning the two same-family fids of the join.
  expect_setequal(names(mb$blocks), c("DyNAM:rate", "DyNAM:choice"))
  expect_equal(mb$blocks[["DyNAM:rate"]]$fids, c(1L, 3L))
  expect_equal(mb$blocks[["DyNAM:choice"]]$fids, c(2L, 4L))
  expect_setequal(
    mb$blocks[["DyNAM:rate"]]$unit_keys,
    c("calls:rate", "emails:rate")
  )
})

test_that("rate and choice blocks carry the sender (2D) and dyad (3D) shapes", {
  mb <- build_merged_blocks(joint_two_process())

  # The rate block is sender-indexed (a 2D sender x effects statistic); the
  # choice block is dyad-indexed (a 3D sender x receiver x effects statistic).
  expect_true(mb$blocks[["DyNAM:rate"]]$is_sender)
  expect_false(mb$blocks[["DyNAM:choice"]]$is_sender)

  # The shape flag comes from the compiled spec_map's statistic class.
  expect_s3_class(mb$units[["calls:rate"]]$spec_map, "sender_spec")
  expect_s3_class(mb$units[["emails:rate"]]$spec_map, "sender_spec")
  expect_s3_class(mb$units[["calls:choice"]]$spec_map, "dyad_spec")
  expect_s3_class(mb$units[["emails:choice"]]$spec_map, "dyad_spec")
})

# ---- Cross-process effect union (per block) ---------------------------------

test_that("focal-relative effects split per focal while absolute effects pool", {
  mb <- build_merged_blocks(joint_two_process())

  # Rate block: indeg is focal-relative, so it splits into one column per
  # process -- indeg(calls) for fid 1, indeg(emails) for fid 3 -- never pooled.
  rate <- mb$blocks[["DyNAM:rate"]]$union
  expect_setequal(rate$union_labels, c("indeg(calls)", "indeg(emails)"))
  expect_false(rate$effect_maps[["1"]] == rate$effect_maps[["3"]])

  # Choice block: inertia splits per focal, tie(friendship) is an absolute-layer
  # effect that pools to a single shared column referenced by both fids.
  choice <- mb$blocks[["DyNAM:choice"]]$union
  expect_equal(sum(choice$union_labels == "tie(friendship)"), 1L)
  expect_setequal(
    choice$union_labels,
    c("inertia(calls)", "inertia(emails)", "tie(friendship)")
  )
  tie_col <- which(choice$union_labels == "tie(friendship)")
  # Both choice fids reference the one shared tie(friendship) column ...
  expect_true(tie_col %in% choice$effect_maps[["2"]])
  expect_true(tie_col %in% choice$effect_maps[["4"]])
  # ... but their inertia columns are distinct (per focal).
  inertia2 <- setdiff(choice$effect_maps[["2"]], tie_col)
  inertia4 <- setdiff(choice$effect_maps[["4"]], tie_col)
  expect_false(inertia2 == inertia4)
})

# ---- Shared state over the object union -------------------------------------

test_that("the shared state holds the union of the processes' objects", {
  mb <- build_merged_blocks(joint_two_process())

  # The shared object registry is the by-identity union: calls and emails (each
  # a focal network) and friendship (read by both choice formulas), once each.
  expect_setequal(mb$objects$name, c("calls", "friendship", "emails"))
  expect_equal(nrow(mb$objects), 3L)
  expect_false(any(duplicated(mb$objects$key)))

  # The shared state container materializes every unioned network once.
  expect_setequal(names(mb$state$networks), c("calls", "friendship", "emails"))
  expect_setequal(
    attr(mb$state, "object_keys")$name,
    c("calls", "friendship", "emails")
  )

  # Each process maps shared oids to its own object indices: the calls choice
  # unit reads calls (shared 1 -> local 1) and friendship (shared 2 -> local 2)
  # but not emails (shared 3 -> NA).
  cc <- mb$units[["calls:choice"]]$shared_to_local
  calls_oid <- match("calls", mb$objects$name)
  friend_oid <- match("friendship", mb$objects$name)
  emails_oid <- match("emails", mb$objects$name)
  expect_equal(cc[calls_oid], 1L)
  expect_false(is.na(cc[friend_oid]))
  expect_true(is.na(cc[emails_oid]))
  # The rate units read only their own focal network.
  expect_true(is.na(mb$units[["calls:rate"]]$shared_to_local[friend_oid]))
})

# ---- Shared schedule over the event union -----------------------------------

test_that("the merged schedule spans the union of events over shared targets", {
  mb <- build_merged_blocks(joint_two_process())
  sch <- mb$schedule

  # Dependent (evaluation) rows plus one state-update row per distinct covariate
  # stream: calls 5 + emails 3 dependent, calls 5 + friendship 4 + emails 3
  # covariate = 20 rows.
  expect_equal(sch$n, 20L)
  expect_true(!is.unsorted(sch$time))

  # A process's dependent rows evaluate but write no state (target NA); every
  # covariate row targets an object present in the shared registry.
  cov <- !sch$dependent
  expect_true(all(is.na(sch$target[sch$dependent])))
  expect_true(all(sch$target[cov] %in% mb$objects$oid))

  # The focal layers' events appear both as dependent (keyed by the focal layer)
  # and as covariate updates to their networks.
  expect_setequal(unique(sch$layer[sch$dependent]), c("calls", "emails"))
  expect_setequal(
    unique(sch$layer[cov]),
    c("calls", "friendship", "emails")
  )
  # friendship, an unmodeled panel covariate, only ever updates state.
  expect_false("friendship" %in% sch$layer[sch$dependent])
})

# ---- The merged single-clock walk -------------------------------------------

# A single flavored specification over a mutually-exclusive layer: creation and
# dissolution compete on `calls`, so the walk exercises the support-mask fold and
# the per-flavor intercept scalars. This is the frozen-baseline gate's fixture --
# the merged walk must reproduce `preprocess_flavored()` on it byte-for-byte.
two_flavor_data <- function() {
  nodes <- data.frame(
    label = c("A", "B", "C", "D"),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 3L, 1L, 2L, 3L),
    to = c(2L, 4L, 2L, 3L, 4L),
    time = c(NA, 1, 2, 3, 4),
    layer = "calls",
    weight = c(1, 1, -1, 1, -1),
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "toy4",
    focal = "calls",
    update = c(calls = "increment"),
    directed = c(calls = TRUE),
    observation = c(calls = "event")
  )
  add_flavor(
    list(info = info, nodes = nodes, ties = ties),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1)
  )
}

two_flavor_spec <- function(data = two_flavor_data(), ...) {
  make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg + outdeg),
    choice = list(creation ~ inertia, dissolution ~ inertia),
    model = "DyNAM",
    data = data,
    ...
  )
}

# A plain single-process spec whose choice reads a friendship covariate placed at
# times BETWEEN the calls events, so the rate (which reads only calls) and the
# choice (which reads friendship) reference disjoint interior event times.
calls_with_offbeat_friendship <- function() {
  nodes <- data.frame(
    label = paste0("N", 1:5),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L),
      to = c(2L, 3L, 4L),
      time = c(1.5, 2.5, 3.5),
      layer = "friendship"
    ),
    data.frame(
      from = c(1L, 2L, 3L, 4L),
      to = c(2L, 3L, 4L, 5L),
      time = c(1, 2, 3, 4),
      layer = "calls"
    )
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(friendship = "increment", calls = "increment"),
    directed = c(friendship = TRUE, calls = TRUE),
    observation = c(friendship = "panel", calls = "event")
  )
  data <- list(info = info, nodes = nodes, ties = ties)
  make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
}

# Two uncoupled processes whose events fall at DISTINCT times, so a modeled
# dependent event of one is a genuine interior right-censoring boundary of the
# other's rate integral (calls at integers, emails staggered between them).
joint_staggered <- function() {
  nodes <- data.frame(
    label = paste0("N", 1:6),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls"
    ),
    data.frame(
      from = c(2L, 3L),
      to = c(1L, 2L),
      time = c(1.5, 2.5),
      layer = "emails"
    )
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(calls = "increment", emails = "increment"),
    directed = c(calls = TRUE, emails = TRUE),
    observation = c(calls = "event", emails = "event")
  )
  data <- list(info = info, nodes = nodes, ties = ties)
  calls_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  list(
    joint = make_joint_specification(calls_spec, emails_spec, data = data),
    calls = calls_spec
  )
}

# The wrapper decorations `estimate_wrapper()` / `preprocess_flavored()` add
# AFTER the walk (metadata for estimation re-entry); the merged driver produces
# the raw walk output, so strip these before a byte-identity comparison.
strip_prep_deco <- function(prep) {
  deco <- c(
    "formula",
    "model",
    "sub_model",
    "nodes",
    "nodes2",
    "node_lookup",
    "model_spec",
    "support_validated"
  )
  prep[setdiff(names(prep), deco)]
}

prep_by <- function(out, layer, family) {
  map <- attr(out, "process_map")
  key <- as.character(map$fid[map$layer == layer & map$family == family])
  out[[key]]
}

prep_by_flavor <- function(out, flavor, family) {
  map <- attr(out, "process_map")
  key <- as.character(map$fid[map$flavor == flavor & map$family == family])
  out[[key]]
}

test_that("the merged walk returns one preprocessed object per fid", {
  out <- suppressWarnings(preprocess_joint(joint_two_process()))
  map <- attr(out, "process_map")

  expect_s3_class(out, "joint_preprocessed.goldfish")
  expect_equal(names(out), as.character(sort(map$fid)))
  expect_true(all(vapply(
    out,
    function(p) inherits(p, "preprocessed.goldfish"),
    logical(1)
  )))
})

test_that("the merged walk reproduces the flavored two-walk byte-for-byte", {
  spec <- two_flavor_spec()
  oracle <- suppressWarnings(preprocess_flavored(spec))
  merged <- suppressWarnings(preprocess_joint(spec))
  omap <- attr(oracle, "process_map")

  # Aligned by (flavor, family): the joint map is flavor-major, the flavored map
  # family-major, so the fid NUMBER differs while the content must not.
  for (i in seq_len(nrow(omap))) {
    o <- oracle[[as.character(omap$fid[i])]]
    m <- prep_by_flavor(merged, omap$flavor[i], omap$family[i])
    expect_equal(strip_prep_deco(m), strip_prep_deco(o))
  }
})

test_that("a plain single process matches its standalone preprocessing", {
  spec <- calls_with_offbeat_friendship()
  merged <- suppressWarnings(preprocess_joint(spec))

  rate_solo <- suppressWarnings(compute_statistics(
    calls ~ 1 + indeg,
    data = spec$data,
    model = "DyNAM",
    sub_model = "rate"
  ))
  choice_solo <- suppressWarnings(compute_statistics(
    calls ~ inertia + tie(friendship),
    data = spec$data,
    model = "DyNAM",
    sub_model = "choice"
  ))

  expect_equal(
    strip_prep_deco(prep_by(merged, "calls", "rate")),
    strip_prep_deco(rate_solo)
  )
  expect_equal(
    strip_prep_deco(prep_by(merged, "calls", "choice")),
    strip_prep_deco(choice_solo)
  )
})

test_that("a choice-only covariate never right-censors a rate fid", {
  # friendship is read only by the choice formula and fires at 1.5, 2.5, 3.5.
  # The rate fid reads only calls, so those events update state for the choice
  # but leave no row on the rate timeline -- the per-block-schedule RC gate.
  merged <- suppressWarnings(preprocess_joint(calls_with_offbeat_friendship()))
  rate <- prep_by(merged, "calls", "rate")
  choice <- prep_by(merged, "calls", "choice")

  expect_equal(rate$event_time, c(1, 2, 3, 4))
  expect_false(any(c(1.5, 2.5, 3.5) %in% rate$event_time))
  # The choice sees friendship as state (its tie() column changes) but records
  # only its own dependent events, never a right-censored friendship row.
  expect_equal(choice$event_time, c(1, 2, 3, 4))
  expect_true(all(choice$is_dependent == 1L))
})

test_that("choice fids are unchanged by an uncoupled competing process", {
  fx <- joint_staggered()
  mv <- suppressWarnings(preprocess_joint(fx$joint))
  solo <- suppressWarnings(preprocess_joint(fx$calls))

  expect_equal(
    strip_prep_deco(prep_by(mv, "calls", "choice")),
    strip_prep_deco(prep_by(solo, "calls", "choice"))
  )
})

test_that("a modeled dependent event right-censors another process's rate fid", {
  fx <- joint_staggered()
  mv <- suppressWarnings(preprocess_joint(fx$joint))
  solo <- suppressWarnings(preprocess_joint(fx$calls))

  mv_rate <- prep_by(mv, "calls", "rate")
  solo_rate <- prep_by(solo, "calls", "rate")

  # Standalone: calls-rate records only its own five events.
  expect_equal(solo_rate$event_time, c(1, 2, 3, 4, 5))
  # Joined: the two emails events at 1.5 and 2.5 add interior right-censoring
  # boundaries, subdividing the rate integral without changing the dependent
  # observations.
  expect_equal(mv_rate$event_time, c(1, 1.5, 2, 2.5, 3, 4, 5))
  expect_equal(mv_rate$is_dependent, c(1L, 0L, 1L, 0L, 1L, 1L, 1L))
  # The extra rows only partition existing intervals: the same total observation
  # time and the same dependent-event count as standalone.
  expect_equal(sum(mv_rate$intervals), sum(solo_rate$intervals))
  expect_equal(mv_rate$n_dep_events, solo_rate$n_dep_events)
})

test_that("focal is resolved per fid, never stamped on the shared state", {
  # The joint data object carries a single info$focal ("calls"); the emails
  # process must still resolve its own dependent rows and sender indices against
  # emails, not the object-level focal. If a shared focal leaked in, the emails
  # rate fid would key its dependent events off the calls layer.
  mv <- suppressWarnings(preprocess_joint(joint_two_process()))
  emails_rate <- prep_by(mv, "emails", "rate")
  # emails has three events (senders 2, 3, 4); the dependent rows carry those
  # senders, resolved against emails' own focal.
  dep <- emails_rate$is_dependent == 1L
  expect_equal(emails_rate$event_sender[dep], c(2L, 3L, 4L))
})

# ---- Per-fid driver: decoration, engine-readiness, compile-once -------------

test_that("each fid's output carries the oracle's estimation-re-entry decoration", {
  # The per-fid driver stamps each output with the metadata that makes it
  # estimable on its own -- its OWN two-sided formula, model, sub-model, node
  # sides, node lookup, and model spec -- resolved against its own per-fid focal
  # (D8a). On the flavored fixture these must equal the two-walk oracle's
  # decoration fid-for-fid (aligned by (flavor, family), since the joint map is
  # flavor-major and the flavored map family-major).
  spec <- two_flavor_spec()
  oracle <- suppressWarnings(preprocess_flavored(spec))
  merged <- suppressWarnings(preprocess_joint(spec))
  omap <- attr(oracle, "process_map")

  for (i in seq_len(nrow(omap))) {
    o <- oracle[[as.character(omap$fid[i])]]
    m <- prep_by_flavor(merged, omap$flavor[i], omap$family[i])
    for (field in c(
      "formula",
      "model",
      "sub_model",
      "nodes",
      "nodes2",
      "node_lookup",
      "model_spec"
    )) {
      expect_identical(m[[field]], o[[field]], info = field)
    }
    # A rate fid reports sub_model "rate", a choice fid "choice".
    expect_identical(m$sub_model, omap$family[i])
  }
})

test_that("engine-readiness stamps every fid and an empty risk set names the process", {
  # Every fid is validated once and stamped so estimation does not re-run the
  # check (mirroring the flavored driver).
  merged <- suppressWarnings(preprocess_joint(two_flavor_spec()))
  expect_true(all(vapply(
    merged,
    function(p) isTRUE(p$support_validated),
    logical(1)
  )))

  # `~ tie(calls)` contradicts creation's derived `~ !tie(calls)`, leaving the
  # creation process no supportable dyad; the abort names WHICH fid is empty,
  # rendered from the process_map.
  local_cli_context()
  bad <- suppressMessages(make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg + outdeg),
    choice = list(creation ~ inertia, dissolution ~ inertia),
    model = "DyNAM",
    data = two_flavor_data(),
    support_constraint = ~ tie(calls)
  ))
  err <- expect_error(
    suppressWarnings(preprocess_joint(bad)),
    "gated out"
  )
  expect_match(
    paste(conditionMessage(err), collapse = "\n"),
    "calls › creation › rate"
  )
})

test_that("each constraint is compiled once into the merged plan, snapshot per fid", {
  # Compile-once (D3b): the merged plan holds one compiled sub-plan per
  # constraint_id (not one per family), and the per-unit compiles no longer
  # carry the constraint at all -- it is hoisted to the merged level.
  spec <- two_flavor_spec()
  mb <- build_merged_blocks(single_process_joint(spec))
  map <- mb$process_map
  expect_setequal(
    names(mb$support_constraints),
    as.character(unique(stats::na.omit(map$constraint_id)))
  )
  for (key in names(mb$units)) {
    expect_null(mb$units[[key]]$spec_map$plan$support_constraints)
  }

  # Snapshot-per-fid: creation's rate fid (timed, carries right-censored rows)
  # and its choice fid (no right-censoring) share ONE constraint_id, so they read
  # the SAME compiled sub-plan yet snapshot it against DIFFERENT stored
  # timelines. The masks-before-any-event are therefore identical (one compiled
  # constraint) while the snapshot sequences differ in length.
  merged <- suppressWarnings(preprocess_joint(spec))
  rate <- prep_by_flavor(merged, "creation", "rate")
  choice <- prep_by_flavor(merged, "creation", "choice")
  expect_identical(rate$support_mask$initial, choice$support_mask$initial)
  expect_false(
    length(rate$support_mask$support) == length(choice$support_mask$support)
  )
  expect_equal(length(rate$support_mask$support), length(rate$event_time))
  expect_equal(length(choice$support_mask$support), length(choice$event_time))
})

# ---- Multilevel per-fid focal (D8a) -----------------------------------------

# advice staff -> director and nominations director -> project over one shared
# node universe, distinct mode-pairs sharing the whole `director` mode. The
# object's single info$focal names only nominations, so advice -- modeled by
# advice_spec -- is the NON-focal two-mode process whose sides must still resolve
# against its own layer (staff -> director), never info$focal's
# (director -> project). Both processes carry a timed rate, so a dependent event
# of either right-censors the other's rate fid across the mode-pair boundary. A
# third layer `mentoring` (director -> staff) sits in the data unmodeled, so
# info$focal can be flipped to a real non-modeled layer for the poison-check.
multilevel_data <- function(advice = "panel") {
  nodes <- data.frame(
    label = c("S1", "S2", "S3", "D1", "D2", "P1", "P2"),
    mode = c(
      "staff",
      "staff",
      "staff",
      "director",
      "director",
      "project",
      "project"
    ),
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 1L),
      to = c(4L, 5L, 4L, 5L),
      time = c(1, 2, 3, 4),
      layer = "advice",
      stringsAsFactors = FALSE
    ),
    data.frame(
      from = c(4L, 5L, 4L),
      to = c(6L, 7L, 7L),
      time = c(1.5, 2.5, 3.5),
      layer = "nominations",
      stringsAsFactors = FALSE
    ),
    data.frame(
      from = c(4L, 5L),
      to = c(1L, 2L),
      time = c(2, 3),
      layer = "mentoring",
      stringsAsFactors = FALSE
    )
  )
  info <- list(
    name = "ml",
    focal = "nominations",
    update = c(
      advice = "increment",
      nominations = "increment",
      mentoring = "increment"
    ),
    directed = c(advice = TRUE, nominations = TRUE, mentoring = TRUE),
    observation = c(
      advice = advice,
      nominations = "event",
      mentoring = "panel"
    ),
    sender = c(
      advice = "staff",
      nominations = "director",
      mentoring = "director"
    ),
    receiver = c(
      advice = "director",
      nominations = "project",
      mentoring = "staff"
    )
  )
  list(info = info, nodes = nodes, ties = ties)
}

joint_multilevel <- function() {
  event_data <- multilevel_data(advice = "event")
  advice_spec <- make_specification(
    rate = ~ 1 + outdeg,
    choice = ~inertia,
    support_constraint = ~ !tie(advice),
    layer = "advice",
    model = "DyNAM",
    data = event_data
  )
  nominations_spec <- make_specification(
    rate = ~ 1 + outdeg,
    choice = ~inertia,
    layer = "nominations",
    model = "DyNAM",
    data = event_data
  )
  list(
    joint = make_joint_specification(
      advice_spec,
      nominations_spec,
      data = multilevel_data(advice = "panel")
    ),
    event_data = event_data,
    advice = advice_spec
  )
}

# The node labels on a fid's modeled side (1 = sender, 2 = receiver), read from
# its node_lookup -- the two-mode side-validity resolved during the walk.
fid_side_labels <- function(prep, side) {
  prep$node_lookup$label[prep$node_lookup$side == side]
}

test_that("each two-mode process resolves its own mode-pair, not info$focal", {
  # D8a, the walk-boundary analogue of the 1b.3 construction-time test: the object
  # carries one info$focal ("nominations"), yet advice -- the non-focal modeled
  # process -- must resolve staff -> director (its own pair), and its snapshotted
  # support mask must be sized over that pair (staff x director), never
  # info$focal's director -> project. A shared stamped focal would collapse both
  # processes' side/mode resolution onto nominations' pair.
  fx <- joint_multilevel()
  out <- suppressWarnings(preprocess_joint(fx$joint))

  advice_rate <- prep_by(out, "advice", "rate")
  advice_choice <- prep_by(out, "advice", "choice")
  expect_identical(fid_side_labels(advice_rate, 1), c("S1", "S2", "S3"))
  expect_identical(fid_side_labels(advice_rate, 2), c("D1", "D2"))
  expect_identical(fid_side_labels(advice_choice, 1), c("S1", "S2", "S3"))
  expect_identical(fid_side_labels(advice_choice, 2), c("D1", "D2"))
  # The snapshotted mask is staff x director (advice's own pair): 3 x 2, not the
  # 2 x 2 that info$focal's director -> project would produce.
  expect_identical(dim(advice_choice$support_mask$initial), c(3L, 2L))

  # nominations, the info$focal layer, resolves its own director -> project pair.
  expect_identical(
    fid_side_labels(prep_by(out, "nominations", "rate"), 1),
    c("D1", "D2")
  )
  expect_identical(
    fid_side_labels(prep_by(out, "nominations", "choice"), 2),
    c("P1", "P2")
  )
})

test_that("flipping info$focal to a non-modeled layer leaves every fid identical", {
  # The poison that distinguishes per-fid focal (Pattern A) from a shared stamped
  # focal (Pattern B): the walk must never consult the object-level info$focal, so
  # pointing it at a third, non-modeled layer (mentoring, director -> staff) must
  # change nothing. A shared-focal fallback would re-resolve the fids against
  # mentoring and differ.
  fx <- joint_multilevel()
  base <- suppressWarnings(preprocess_joint(fx$joint))

  poison <- fx$joint
  poison$data$info$focal <- "mentoring"
  flipped <- suppressWarnings(preprocess_joint(poison))

  expect_identical(unclass(base), unclass(flipped))
})

test_that("a non-focal process's fid matches its standalone preprocessing", {
  # advice reads only its own layer, so joining it with nominations leaves its
  # choice fid byte-identical to preprocessing the advice spec on its own -- the
  # per-fid focal resolves advice's staff -> director sides identically inside the
  # join and standalone, despite info$focal naming nominations.
  fx <- joint_multilevel()
  out <- suppressWarnings(preprocess_joint(fx$joint))

  advice_solo <- suppressWarnings(compute_statistics(
    advice ~ inertia,
    data = fx$event_data,
    model = "DyNAM",
    sub_model = "choice",
    support_constraint = ~ !tie(advice)
  ))
  expect_equal(
    strip_prep_deco(prep_by(out, "advice", "choice")),
    strip_prep_deco(advice_solo)
  )
})

test_that("a dependent event right-censors a rate fid over a different mode-pair", {
  # The merged walk's cross-process right-censoring crosses mode-pair boundaries:
  # an advice (staff x director) event is a right-censoring boundary for the
  # nominations (director x project) rate fid and vice versa, exactly as a
  # cross-flavor event would be, despite the differing dyad shape.
  fx <- joint_multilevel()
  out <- suppressWarnings(preprocess_joint(fx$joint))

  advice_rate <- prep_by(out, "advice", "rate")
  nominations_rate <- prep_by(out, "nominations", "rate")

  # advice fires at 1, 2, 3, 4; nominations at 1.5, 2.5, 3.5. Each other's events
  # add interior right-censoring rows (is_dependent 0) without adding dependent
  # observations.
  expect_identical(advice_rate$event_time, c(1, 1.5, 2, 2.5, 3, 3.5, 4))
  expect_identical(advice_rate$is_dependent, c(1L, 0L, 1L, 0L, 1L, 0L, 1L))
  expect_identical(nominations_rate$event_time, c(1.5, 2, 2.5, 3, 3.5, 4))
  expect_identical(nominations_rate$is_dependent, c(1L, 0L, 1L, 0L, 1L, 0L))

  # Standalone, advice-rate carries only its own four events; the cross-mode-pair
  # rows only partition existing intervals, so the total observation time and the
  # dependent-event count are unchanged.
  solo_rate <- prep_by(
    suppressWarnings(preprocess_joint(fx$advice)),
    "advice",
    "rate"
  )
  expect_identical(solo_rate$event_time, c(1, 2, 3, 4))
  expect_equal(sum(advice_rate$intervals), sum(solo_rate$intervals))
  expect_equal(advice_rate$n_dep_events, solo_rate$n_dep_events)
})

test_that("the merged walk does not regress the single-process hot path", {
  # The single-process hot path is the untouched two-walk oracle; the merged
  # driver runs a separate walk that single-process specs will route through once
  # the dispatch flips. This records the merged/oracle preprocess-time ratio on
  # the flavored fixture and guards only a catastrophic regression, staying immune
  # to CI timing noise on a tiny fixture.
  skip_on_cran()
  spec <- two_flavor_spec()
  suppressWarnings(preprocess_joint(spec)) # warm up the compile caches

  reps <- 20L
  oracle_s <- system.time(
    for (i in seq_len(reps)) {
      suppressWarnings(preprocess_flavored(spec))
    }
  )[["elapsed"]]
  merged_s <- system.time(
    for (i in seq_len(reps)) {
      suppressWarnings(preprocess_joint(spec))
    }
  )[["elapsed"]]
  ratio <- merged_s / oracle_s

  message(sprintf(
    "merged/oracle preprocess ratio on the flavored fixture: %.2fx (oracle %.3fs, merged %.3fs over %d reps)",
    ratio,
    oracle_s,
    merged_s,
    reps
  ))
  expect_lt(ratio, 10)
})
