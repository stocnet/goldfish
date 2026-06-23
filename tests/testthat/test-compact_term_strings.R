mkNames <- function(effects, ...) {
  cols <- list(...)
  mat <- matrix("", nrow = length(effects), ncol = length(cols),
    dimnames = list(effects, names(cols))
  )
  for (nm in names(cols)) mat[, nm] <- cols[[nm]]
  mat
}

test_that("layout grammar: effect/object and bracket omission", {
  m <- mkNames(c("inertia", "recip"), Object = c("friendship", "advice"))
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_equal(unname(out), c("inertia/friendship", "recip/advice"))
})

test_that("object-less effect omits the slash", {
  m <- mkNames(c("Intercept", "inertia"), Object = c("", "friendship"))
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_equal(unname(out[1]), "Intercept")
  expect_false(grepl("/", out[1]))
})

test_that("multiple objects joined with middle dot", {
  m <- matrix(c("friendship", "advice"), nrow = 1,
    dimnames = list("mixed_trans", c("Object 1", "Object 2"))
  )
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_equal(unname(out), "mixed_trans/friendship·advice")
})

test_that("arguments collected in one bracket block", {
  m <- mkNames("inertia",
    Object = "friendship", weighted = "W", type = "ego", fixed = "TRUE"
  )
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_equal(unname(out), "inertia/friendship [W ego Fx]")
})

test_that("ignore_repetitions renders as IR", {
  m <- mkNames("inertia", Object = "friendship", ignore_repetitions = "B")
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_match(out, "\\[IR\\]")
})

test_that("object trimming drops data-frame prefix", {
  m <- mkNames("ego", Object = "actors$age")
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_equal(unname(out), "ego/age")
})

test_that("network names shortened to unique prefix on overflow", {
  m <- mkNames(c("inertia", "recip"),
    Object = c("friendship", "friendsXX")
  )
  out <- compact_term_strings(m, mode = "console", width = 16)
  expect_false(any(duplicated(sub("^[^/]+/", "", out))))
})

test_that("no abbreviation when the column fits", {
  m <- mkNames(c("inertia", "common_sender"),
    Object = c("net", "net")
  )
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_true(any(grepl("inertia", out)))
  expect_true(any(grepl("common_sender", out)))
})

test_that("column-uniform abbreviation when overflowing", {
  m <- mkNames(c("inertia", "common_receiver"),
    Object = c("friendshipNetwork", "collaborationNetwork")
  )
  out <- compact_term_strings(m, mode = "console", width = 18)
  expect_true(all(grepl("inrt|cmm_rec", out)))
  expect_false(any(grepl("inertia", out)))
})

test_that("inertia and tie remain distinguishable", {
  m <- mkNames(c("inertia", "tie"),
    Object = c("friendshipNetwork", "friendshipNetwork")
  )
  out <- compact_term_strings(m, mode = "console", width = 12)
  expect_match(out[["inertia"]], "^inrt")
  expect_match(out[["tie"]], "^tie")
  expect_false(grepl("inrt", out[["tie"]]))
})

test_that("window short forms", {
  m <- mkNames(c("inertia", "recip", "outdeg"),
    Object = c("net", "net", "net"),
    window = c("7 days", "2 weeks", "30")
  )
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_match(out[["inertia"]], "\\[7d\\]")
  expect_match(out[["recip"]], "\\[2wk\\]")
  expect_match(out[["outdeg"]], "\\[wdw\\]")
})

test_that("transformer/summarizer tokens", {
  m <- mkNames(c("a", "b", "c"),
    Object = c("net", "net", "net"),
    transformer_fn = c("sqrt", "", "verylongfunctionname"),
    summarizer_fn = c("mean", "", "")
  )
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_match(out[["a"]], "\\[t:sqrt s:mean\\]")
  expect_match(out[["c"]], "\\[fn\\]")
})

test_that("subType uses shortest unique prefix", {
  m <- mkNames(c("a", "b"),
    Object = c("net", "net"),
    subType = c("proximity", "popularity")
  )
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_match(out[["a"]], "\\[pro\\]")
  expect_match(out[["b"]], "\\[pop\\]")
})

test_that("history uses first three characters", {
  m <- mkNames("a", Object = "net", history = "consecutive")
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_match(out, "\\[con\\]")
})

test_that("ellipsis truncation as final fallback", {
  m <- mkNames("common_receiver",
    Object = "averyveryverylongnetworkname",
    weighted = "W", type = "alter", history = "consecutive"
  )
  out <- compact_term_strings(m, mode = "console", width = 12)
  expect_true(nchar(out) <= 12)
  expect_match(out, "…$")
})

test_that("export names are valid, unique, length-bounded", {
  m <- mkNames(c("inertia", "inertia", "recip"),
    Object = c("friendship", "friendship", "advice"),
    weighted = c("W", "W", "")
  )
  out <- compact_term_strings(m, mode = "export", max_length = 20)
  expect_equal(out, make.names(out))
  expect_false(any(grepl("[/·\\[\\] ]", out)))
  expect_false(anyDuplicated(out) > 0)
  expect_true(all(nchar(out) <= 20))
})

test_that("coef minimal-unique short names", {
  m <- mkNames(c("indeg", "indeg", "recip"),
    Object = c("callNetwork", "otherNetwork", "callNetwork")
  )
  out <- compact_term_strings(m, mode = "coef")
  expect_equal(unname(out[3]), "rec")
  expect_false(anyDuplicated(out) > 0)
  expect_true(all(grepl("^ideg", out[1:2])))
})

test_that("dot-prefixed columns are ignored", {
  m <- mkNames("inertia",
    Object = "friendship", weighted = "W"
  )
  m <- cbind(m, .coef_name = "inrt", .term_export = "inertia_friendship")
  out <- compact_term_strings(m, mode = "console", width = 200)
  expect_equal(unname(out), "inertia/friendship [W]")
})
