# Helper data for effect tests --------------------------------

# Networks  --------------------------------
# Defaults
m <- matrix(
  c(
    0, 1, 1, 0, 0,
    1, 0, 1, 0, 0,
    0, 1, 0, 2, 0,
    2, 0, 0, 0, 1,
    NA, 0, 0, 0, 0
  ),
  nrow = 5, ncol = 5, byrow = TRUE,
  dimnames = list(
    sprintf("Actor %d", 1:5),
    sprintf("Actor %d", 1:5)
  )
)
m0 <- matrix(0, nrow = 5, ncol = 5,
  dimnames = list(
    sprintf("Actor %d", 1:5),
    sprintf("Actor %d", 1:5)
  )
)
m1 <- matrix(
  c(
    0, 1, 1, 1, 0,
    1, 0, 1, 0, 0,
    0, 1, 0, 2, 0,
    2, 0, 0, 0, 1,
    0, 0, 0, 0, 0
  ),
  nrow = 5, ncol = 5, byrow = TRUE,
  dimnames = list(
    sprintf("Actor %d", 1:5),
    sprintf("Actor %d", 1:5)
  )
)

# Two-mode
mTwoMode <- matrix(0, 5, 5)
mTwoMode[1, 2] <- 1
mTwoMode[3, 4] <- 1
mTwoMode[3, 2] <- 1
mTwoMode[1, 5] <- NA
mTwoMode[3, 5] <- 1
mTwoModeStats <- matrix(0, 5, 5)
mTwoModeStats[1, 4] <- 1

# Bipartide
mBipar <- matrix(0, 5, 5)
mBipar[1, 3] <- 1
mBipar[2, 4] <- 1
mBipar[1, 5] <- NA
mBipar[2, 5] <- NA
mBiparStats <- matrix(0, 5, 5)
mBiparStats[1, 4] <- 1

# Caches --------------------------------
mCache <- matrix(
  c(
    0, 2, 1, 0, 0,
    1, 0, 1, 0, 0,
    0, 1, 1, 1, 0,
    0, 0, 0, 0, 1,
    NA, 0, 0, 0, 0
  ),
  nrow = 5, ncol = 5, byrow = TRUE,
  dimnames = list(
    sprintf("Actor %d", 1:5),
    sprintf("Actor %d", 1:5)
  )
)
vCache <- c(0, 2, 3, 1, 0)

# Attributes  --------------------------------
attr <- data.frame(
  label = as.factor(c("Christoph", "James", "Per", "Timon", "Marion", "Mepham", "Xiaolei", "Federica")),
  fishingSkill = c(10, NA, 5, 10, 8, 8, 3, NA),
  fishCaught = c(1, 99, 15, 12, 15, 8, 0, 2),
  fishSizeMean = c(9.9, 0.1, 0.5, 0.45, 0.25, 0.3, NA, 10),
  fishingComplete = c(10, 0, 5, 10, 8, 8, 3, 2)
)

# Effect Functions  --------------------------------
effectFUN_tie <- function(network,
                      sender, receiver, replace,
                      weighted = FALSE, transformFun = identity)
  update_DyNAM_choice_tie(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    weighted = weighted, transformFun = transformFun
    )
effectFUN_tie_weighted <- function(network,
                                   sender, receiver, replace,
                                   weighted = TRUE, transformFun = identity)
  update_DyNAM_choice_tie(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    weighted = weighted, transformFun = transformFun
  )
effectFUN_same <- function(attribute,
                      node, replace,
                      isTwoMode = FALSE)
  update_DyNAM_choice_same(
    attribute = attribute,
    node = node, replace = replace,
    isTwoMode = isTwoMode
  )


effectFUN_indeg <- function(
  network,
  sender, receiver, replace,
  cache, n1, n2,
  isTwoMode = FALSE,
  weighted = FALSE, transformFun = identity) {
  update_DyNAM_choice_indeg(
    network = network,
    sender = sender, receiver = receiver, replace = replace, cache = cache,
    n1 = n1, n2 = n2, isTwoMode = isTwoMode,
    weighted = weighted, transformFun = transformFun
  )
}

effectFUN_trans <- function(network,
                      sender,
                      receiver,
                      replace, cache,
                      isTwoMode = FALSE,
                      transformFun = identity)
  update_DyNAM_choice_trans(
    network = network,
    sender = sender, receiver = receiver, replace = replace,
    cache = cache,
    isTwoMode = isTwoMode, transformFun = transformFun
  )
effectFUN_tertius <- function(network,
                      attribute,
                      sender = NULL,
                      receiver = NULL,
                      node = NULL,
                      replace,
                      cache,
                      isTwoMode = FALSE,
                      n1 = n1, n2 = n2,
                      transformFun = abs,
                      aggregateFun = function(x) mean(x, na.rm = TRUE))
  update_DyNAM_choice_tertius_diff(network = network,
                                   attribute = attribute,
                                   sender = sender,
                                   receiver = receiver,
                                   node = node,
                                   replace = replace,
                                   cache = cache,
                                   isTwoMode = isTwoMode,
                                   n1 = n1, n2 = n2,
                                   transformFun = transformFun,
                                   aggregateFun = aggregateFun)

effectFUN_REM_ego <- function(attribute,
                      node, replace,
                      n1, n2,
                      isTwoMode = FALSE)
  update_REM_choice_ego(attribute = attribute,
                        node = node, replace = replace,
                        n1 = n1, n2 = n2,
                        isTwoMode = isTwoMode)
effectFUN_REM_diff <- function(attribute, node, replace,
                      n1, n2,
                      isTwoMode = FALSE,
                      transformFun = abs)
  update_DyNAM_choice_diff(
    attribute = attribute,
    node = node, replace = replace,
    isTwoMode = isTwoMode,
    n1 = n1, n2 = n2,
    transformFun = transformFun
  )
effectFUN_REM_sim <- function(attribute,
                      node, replace,
                      isTwoMode = FALSE)
  update_DyNAM_choice_same(
    attribute = attribute,
    node = node, replace = replace,
    isTwoMode = isTwoMode
  )
