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
