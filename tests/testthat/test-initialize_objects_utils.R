test_that("map_to_index handles numeric and character inputs", {
    expect_equal(map_to_index(1:3), as.integer(1:3))
    vnames <- c("a", "b", "c")
    expect_equal(
        map_to_index(c("b", "a", "x"), vnames_local = vnames),
        as.integer(c(2, 1, NA))
    )
})

test_that("impute_attributes imputes numeric mean and non-numeric mode", {
    attrs <- matrix(c("1", "2", NA, "x", "y", NA), nrow = 3)
    # col1 numeric-ish, col2 non-numeric
    res <- impute_attributes(attrs)
    expect_equal(as.numeric(res[, 1]), c(1, 2, 1.5))
    expect_equal(res[3, 2], "x")
})

test_that("impute_networks replaces NAs with zeros and sets integer storage", {
    m <- matrix(c(1, NA, 3, NA), nrow = 2)
    out <- impute_networks(m)
    expect_equal(out[1, 2], 0L)
    expect_equal(storage.mode(out), "integer")

    lst <- list(a = m, b = list(c = m))
    out2 <- impute_networks(lst)
    expect_equal(out2$a[1, 2], 0L)
    expect_equal(storage.mode(out2$b$c), "integer")
})

test_that("update_network_from_rows sums increments and applies last replacement/weight", {
    mat <- matrix(0L, nrow = 3, ncol = 3)
    rownames(mat) <- colnames(mat) <- c("a", "b", "c")
    rows <- data.frame(
        from = c(1, 1, 1, 2),
        to = c(2, 2, 2, 3),
        increment = c(1L, 2L, NA, 1L),
        replace = c(NA, NA, 5L, NA),
        weight = c(NA, NA, NA, 7L)
    )
    out <- update_network_from_rows(mat, rows)
    expect_equal(out[1, 2], 5)
    expect_equal(out[2, 3], 7)
})

test_that("build_attribute_column returns last pre-start values per node", {
    rows_v <- tibble::tibble(
        node = c("a", "b", "a", "c"),
        time = c(1, 2, 3, 0),
        val = c("x", "y", "z", "w")
    )
    vnames <- c("a", "b", "c", "d")
    res <- build_attribute_column(
        rows_v,
        "node",
        "val",
        vnames,
        4,
        start_time = 3
    )
    expect_equal(res, c("x", "y", "w", NA))
})

test_that("meta_for_type extracts nodeset_def and two_mode flag", {
    networks_meta <- tibble::tibble(
        type = c("t1", "t2"),
        nodeset_def = c("set1", NA_character_),
        two_mode = c(TRUE, FALSE)
    )
    res <- meta_for_type("t1", networks_meta)
    expect_equal(res$nodeset_def, "set1")
    expect_true(res$two_mode)
    res2 <- meta_for_type("missing", networks_meta)
    expect_null(res2$nodeset_def)
    expect_false(res2$two_mode)
})

test_that("mats_info_fun builds two-mode matrix and applies initial ties", {
    networks_meta <- tibble::tibble(
        type = "t1",
        nodeset_def = "set1",
        two_mode = TRUE
    )
    data <- list()
    attr(data, "networks_meta") <- networks_meta
    node_tbl <- tibble::tibble(
        name = c("A", "B", "C"),
        set1 = c("agent", "agent", "no-agent")
    )
    vnames <- node_tbl$name
    ties_tbl <- tibble::tibble(
        from = c("A"),
        to = c("C"),
        time = c(0),
        type = c("t1")
    )
    info <- mats_info_fun(
        "t1",
        data,
        ties_tbl,
        node_tbl,
        vnames,
        start_time = 1
    )
    expect_true(info$two_mode)
    expect_equal(dim(info$mat), c(2, 1))
    expect_equal(info$mat["A", "C"], 1)
})
