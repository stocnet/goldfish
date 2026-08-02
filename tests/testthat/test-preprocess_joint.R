# The merged-walk substrate for a joint specification: each process is compiled
# into a walk-ready spec_map, the maps are grouped into statistic blocks, and one
# shared state container and one shared event schedule are built over the union
# of the processes' objects and events. No event loop runs here -- these read the
# compiled substrate structure directly.

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
two_flavor_spec <- function() {
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
  data <- add_flavor(
    list(info = info, nodes = nodes, ties = ties),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1)
  )
  make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg + outdeg),
    choice = list(creation ~ inertia, dissolution ~ inertia),
    model = "DyNAM",
    data = data
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

  rate_solo <- suppressWarnings(compute_stats(
    calls ~ 1 + indeg,
    data = spec$data,
    model = "DyNAM",
    sub_model = "rate"
  ))
  choice_solo <- suppressWarnings(compute_stats(
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
