# Cross-process preprocessing planning for a joint specification: the effect
# union widened from flavors-of-one-layer to every fid sharing a statistic block
# across processes, and consumer routing widened from a flavor key to a
# (layer, flavor) key. These test the planning helpers the merged single-clock
# walk consumes -- no walk is executed here, so the assertions read the parsed
# plan structure directly.

# A plain two-process join over one node set: calls and emails are both DyNAM
# rate+choice processes, and friendship is a panel-observed layer read (as an
# exogenous covariate) by both choice formulas -- meeting the panel-reference
# requirement without friendship being modeled. Four fids: calls rate/choice,
# emails rate/choice.
mv_join <- function() {
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

# A hand-built route index mirroring a flavored friendship (creation/dissolution
# rate+choice, fids 1-4) plus a plain calls (rate+choice, fids 5-6) walk. Only
# the routing-relevant columns are needed; `has_intercept` is TRUE exactly for
# the timed rate fids.
hand_route_index <- function() {
  build_route_index(data.frame(
    fid = 1:6,
    layer = c(
      "friendship",
      "friendship",
      "friendship",
      "friendship",
      "calls",
      "calls"
    ),
    flavor = c("creation", "creation", "dissolution", "dissolution", NA, NA),
    family = c("rate", "choice", "rate", "choice", "rate", "choice"),
    has_intercept = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  ))
}

# ---- Routing: (layer, flavor) -> fid partitions -----------------------------

test_that("route_partition splits fids into dependent / right-censored / state-only", {
  ri <- hand_route_index()

  # A plain calls event: dependent for the calls rate+choice fids; every OTHER
  # timed rate fid (the two friendship rates) right-censors; the remaining
  # friendship choice fids are state-only.
  calls <- route_partition(ri, "calls", NA)
  expect_equal(calls$dependent, c(5L, 6L))
  expect_equal(calls$right_censored, c(1L, 3L))
  expect_equal(calls$state_only, c(2L, 4L))

  # A friendship *creation* event: dependent for the creation rate+choice fids;
  # the dissolution rate and the calls rate right-censor (cross-flavor and
  # cross-process alike); the dissolution and calls choice fids are state-only.
  creation <- route_partition(ri, "friendship", "creation")
  expect_equal(creation$dependent, c(1L, 2L))
  expect_equal(creation$right_censored, c(3L, 5L))
  expect_equal(creation$state_only, c(4L, 6L))
})

test_that("an event no modeled process owns is only a right-censoring boundary", {
  ri <- hand_route_index()
  # An event on an unmodeled layer: no dependent fid, every timed rate fid
  # right-censors, and the choice fids advance state only.
  part <- route_partition(ri, "emails", NA)
  expect_length(part$dependent, 0L)
  expect_equal(part$right_censored, c(1L, 3L, 5L))
  expect_equal(part$state_only, c(2L, 4L, 6L))
})

test_that("resolve_route_fids matches on (layer, flavor) including NA flavor", {
  ri <- hand_route_index()
  # Both fids of a (layer, flavor) -- rate and choice -- own the event.
  expect_equal(resolve_route_fids(ri, "friendship", "creation"), c(1L, 2L))
  # A plain process's NA flavor matches only NA-flavor rows.
  expect_equal(resolve_route_fids(ri, "calls", NA), c(5L, 6L))
  # A flavor absent from a layer, and a flavor name handed to a plain layer, both
  # resolve to no owner.
  expect_identical(
    resolve_route_fids(ri, "friendship", "nonexistent"),
    NA_integer_
  )
  expect_identical(resolve_route_fids(ri, "calls", "creation"), NA_integer_)
})

test_that("build_route_index carries one routing row per fid of a joint spec", {
  ri <- build_route_index(mv_join()$process_map)
  expect_identical(nrow(ri), 4L)
  expect_named(ri, c("fid", "layer", "flavor", "family", "has_intercept"))
  # A calls event right-censors the emails rate fid (cross-process), and the
  # emails choice fid only advances state.
  part <- route_partition(ri, "calls", NA)
  emails_rate <- ri$fid[ri$layer == "emails" & ri$family == "rate"]
  emails_choice <- ri$fid[ri$layer == "emails" & ri$family == "choice"]
  expect_true(emails_rate %in% part$right_censored)
  expect_true(emails_choice %in% part$state_only)
})

# ---- Cross-process effect union ---------------------------------------------

test_that("a focal-relative effect splits per focal, an absolute effect pools", {
  js <- mv_join()
  blocks <- plan_block_unions(js)
  choice <- blocks[["DyNAM:choice"]]

  # Bare `inertia` is focal-relative: it resolves to each process's OWN focal, so
  # calls' inertia and emails' inertia are DIFFERENT update columns. Pooling them
  # on the shared raw label would be a silent cross-focal wrong-answer bug. The
  # absolute `tie(friendship)` resolves identically under any focal and pools to
  # one shared column.
  expect_setequal(
    choice$union_labels,
    c("inertia(calls)", "inertia(emails)", "tie(friendship)")
  )

  map <- js$process_map
  calls_choice <- as.character(
    map$fid[map$layer == "calls" & map$family == "choice"]
  )
  emails_choice <- as.character(
    map$fid[map$layer == "emails" & map$family == "choice"]
  )
  inertia_calls <- match("inertia(calls)", choice$union_labels)
  inertia_emails <- match("inertia(emails)", choice$union_labels)
  tie_col <- match("tie(friendship)", choice$union_labels)

  # Each process maps its own inertia column and the ONE shared tie column, in
  # its formula order (`~ inertia + tie(friendship)`).
  expect_equal(choice$effect_maps[[calls_choice]], c(inertia_calls, tie_col))
  expect_equal(choice$effect_maps[[emails_choice]], c(inertia_emails, tie_col))
})

test_that("a bare rate effect splits per focal too", {
  js <- mv_join()
  blocks <- plan_block_unions(js)
  rate <- blocks[["DyNAM:rate"]]

  # `indeg` is bare in both rate formulas (`~ 1 + indeg`), so it resolves to each
  # process's own focal -- the same per-focal split the choice block shows.
  expect_setequal(rate$union_labels, c("indeg(calls)", "indeg(emails)"))

  map <- js$process_map
  calls_rate <- as.character(
    map$fid[map$layer == "calls" & map$family == "rate"]
  )
  emails_rate <- as.character(
    map$fid[map$layer == "emails" & map$family == "rate"]
  )
  expect_equal(
    rate$effect_maps[[calls_rate]],
    match("indeg(calls)", rate$union_labels)
  )
  expect_equal(
    rate$effect_maps[[emails_rate]],
    match("indeg(emails)", rate$union_labels)
  )
})

test_that("effects are not deduplicated across statistic-block families", {
  js <- mv_join()
  blocks <- plan_block_unions(js)
  # Rate and choice are separate blocks with disjoint label sets: a choice effect
  # never lands in the rate union and vice versa, so gids stay block-scoped.
  expect_true("DyNAM:rate" %in% names(blocks))
  expect_true("DyNAM:choice" %in% names(blocks))
  expect_false("inertia" %in% blocks[["DyNAM:rate"]]$union_labels)
  expect_false("indeg" %in% blocks[["DyNAM:choice"]]$union_labels)
})

test_that("the union shares the intercept flag but preserves each fid's own", {
  # One member carries a time intercept, the other does not, and their effect
  # sets overlap only partially.
  bundles <- list(
    "1" = list(input_formula = ~ x + y, has_intercept = TRUE),
    "2" = list(input_formula = ~ y + z, has_intercept = FALSE)
  )
  union <- build_effect_union(bundles)

  # The union walk stores right-censored rows whenever ANY member needs them.
  expect_true(union$union_intercept)
  # But each fid keeps its OWN intercept scalar, and its OWN projection onto the
  # shared columns -- so downstream each gets its own mask and its own intercept.
  expect_identical(union$has_intercept[["1"]], TRUE)
  expect_identical(union$has_intercept[["2"]], FALSE)
  expect_equal(union$union_labels, c("x", "y", "z"))
  expect_equal(union$effect_maps[["1"]], c(1L, 2L))
  expect_equal(union$effect_maps[["2"]], c(2L, 3L))
})

# ---- fid-ordering invariant -------------------------------------------------

test_that("joint_fid_bundles denotes the same fids as the process_map", {
  js <- mv_join()
  bundles <- joint_fid_bundles(js)
  map <- js$process_map

  expect_identical(as.integer(names(bundles)), map$fid)
  for (i in seq_len(nrow(map))) {
    key <- as.character(map$fid[i])
    expect_identical(bundles[[key]]$layer, map$layer[i])
    expect_identical(bundles[[key]]$flavor, map$flavor[i])
    expect_identical(bundles[[key]]$family, map$family[i])
    # The bundle carried is the parsed sub-model driving this fid's statistics.
    expect_false(is.null(bundles[[key]]$bundle$input_formula))
  }
})
