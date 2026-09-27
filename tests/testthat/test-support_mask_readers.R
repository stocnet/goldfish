# The readers of a stored support mask answer the same questions a dense grid
# would, at every stored kind. The expectations come from `mask_grids()`, the
# test helper's own expansion, so a reader that stops building the grid is
# still checked against one.

reader_cases <- function() {
  withr::local_seed(20260927)
  shapes <- list(
    list(n1 = 5L, n2 = 5L, one_mode = TRUE),
    list(n1 = 4L, n2 = 6L, one_mode = FALSE)
  )
  cases <- list()
  for (shape in shapes) {
    n1 <- shape$n1
    n2 <- shape$n2
    masks <- list(
      "0" = stats::runif(n1 * n2) > 0.5,
      "1" = stats::runif(n2) > 0.4,
      "2" = stats::runif(n1) > 0.4,
      "3" = TRUE
    )
    for (kind in names(masks)) {
      for (present in list(rep(TRUE, n2), seq_len(n2) %% 3L != 0L)) {
        cases[[length(cases) + 1L]] <- c(
          shape,
          list(kind = as.integer(kind), mask = masks[[kind]], present = present)
        )
      }
    }
    # Random masks leave every sender some allowed, present receiver, so the
    # senders a gate turns on are built by hand: sender 1 is allowed only
    # the absent receiver 3, sender 2 nothing, and on one node set sender 4
    # only itself.
    grid <- matrix(TRUE, n1, n2)
    grid[1L, ] <- seq_len(n2) == 3L
    grid[2L, ] <- FALSE
    if (shape$one_mode) {
      grid[4L, ] <- seq_len(n2) == 4L
    }
    cases[[length(cases) + 1L]] <- c(
      shape,
      list(kind = 0L, mask = as.logical(grid), present = seq_len(n2) != 3L)
    )
  }
  cases
}

case_label <- function(case) {
  sprintf(
    "kind %d, %dx%d, %d absent",
    case$kind,
    case$n1,
    case$n2,
    sum(!case$present)
  )
}

oracle_receiver_count <- function(grid, present, one_mode) {
  allowed <- grid & matrix(present, nrow(grid), ncol(grid), byrow = TRUE)
  if (one_mode) {
    diag(allowed) <- FALSE
  }
  rowSums(allowed)
}

test_that("support_row() reads each sender's row of the grid", {
  for (case in reader_cases()) {
    grid <- mask_grids(list(case$mask), case$kind, case$n1, case$n2)[[1L]]
    for (sender in seq_len(case$n1)) {
      expect_identical(
        support_row(case$mask, case$kind, sender, case$n1, case$n2),
        grid[sender, ],
        label = paste(case_label(case), "sender", sender)
      )
    }
  }
})

test_that("sender_gate_from_mask() matches the grid's sender gate", {
  for (case in reader_cases()) {
    grid <- mask_grids(list(case$mask), case$kind, case$n1, case$n2)[[1L]]
    for (drop in unique(c(FALSE, case$one_mode))) {
      expect_identical(
        sender_gate_from_mask(
          case$mask,
          case$kind,
          case$present,
          case$n1,
          case$n2,
          drop_diagonal = drop
        ),
        oracle_sender_gate(grid, case$present, one_mode = drop),
        label = paste(case_label(case), "drop", drop)
      )
    }
  }
})

test_that("initial_receiver_count() counts the grid's allowed receivers", {
  for (case in reader_cases()) {
    grid <- mask_grids(list(case$mask), case$kind, case$n1, case$n2)[[1L]]
    for (drop in unique(c(FALSE, case$one_mode))) {
      expect_equal(
        initial_receiver_count(
          case$mask,
          case$kind,
          case$present,
          case$n1,
          case$n2,
          drop_diagonal = drop
        ),
        oracle_receiver_count(grid, case$present, one_mode = drop),
        ignore_attr = TRUE,
        label = paste(case_label(case), "drop", drop)
      )
    }
  }
})

test_that("reading a dyadic mask's rows costs far less than a grid a row", {
  skip_on_cran()
  skip_if_not_installed("bench")
  # A 300 x 300 logical grid is 0.34 MB, so rebuilding it for each of the 300
  # rows allocates about 103 MB. The bound is a tenth of a grid per row.
  n <- 300L
  withr::local_seed(1L)
  support <- stats::runif(n * n) > 0.5
  used <- bench::bench_memory(
    for (sender in seq_len(n)) {
      support_row(support, 0L, sender, n, n)
    }
  )
  expect_lt(as.numeric(used$mem_alloc), n * (n * n * 4) / 10)
})

test_that("counting a dyadic mask's receivers builds no grid", {
  skip_on_cran()
  skip_if_not_installed("bench")
  # With every receiver present there is nothing to subset, so a count is a
  # pass over the stored vector. Reshaping it into a grid and taking columns
  # costs two grids a call, 0.69 MB here; the bound is a tenth of one.
  n <- 300L
  withr::local_seed(1L)
  support <- stats::runif(n * n) > 0.5
  present <- rep(TRUE, n)
  budget <- 50 * (n * n * 4) / 10
  gate <- bench::bench_memory(
    for (i in 1:50) {
      sender_gate_from_mask(support, 0L, present, n, n, drop_diagonal = TRUE)
    }
  )
  count <- bench::bench_memory(
    for (i in 1:50) {
      initial_receiver_count(support, 0L, present, n, n, drop_diagonal = TRUE)
    }
  )
  expect_lt(as.numeric(gate$mem_alloc), budget)
  expect_lt(as.numeric(count$mem_alloc), budget)
})
