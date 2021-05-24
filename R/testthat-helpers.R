##################### ##
#
# Goldfish package
# Helper data testing -----------------------------------------------
#
#################### ###

# DyNAM -------------------------------------------------------------

# Networks  ---------------------------------------------------------
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

# Caches ------------------------------------------------------------
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

# Attributes  -------------------------------------------------------
testAttr <- data.frame(
  label = as.factor(c("Christoph", "James", "Per", "Timon", "Marion", "Mepham",
                      "Xiaolei", "Federica")),
  fishingSkill = c(10, NA, 5, 10, 8, 8, 3, NA),
  fishCaught = c(1, 99, 15, 12, 15, 8, 0, 2),
  fishSizeMean = c(9.9, 0.1, 0.5, 0.45, 0.25, 0.3, NA, 10),
  fishingComplete = c(10, 0, 5, 10, 8, 8, 3, 2)
)

# Effect Functions  -------------------------------------------------
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
  update_DyNAM_choice_tertiusDiff(network = network,
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

# Preprocessing DyNAM ---------------------------------------------------------
# direct network
eventsIncrement <- data.frame(
  time = cumsum(
    c(1, 5, 3, 4, 2, 1, 3, 4, 5, 1, 3, 4)),
  sender = sprintf("Actor %d",
    c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5, 1)),
  receiver = sprintf("Actor %d",
    c(2, 2, 3, 3, 1, 5, 4, 4, 2, 3, 2, 2)),
  increment =
    c(1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)

actorsEx <- data.frame(
  label = sprintf("Actor %d", 1:5),
  present = rep(TRUE, 5),
  attr1 = c(9.9, 0.1, 0.5, 0.45, 0.25),
  stringsAsFactors = FALSE
)

networkState <- matrix(
  c(0, 3, 0, 0, 0,
    1, 0, 1, 1, 0,
    0, 0, 0, 1, 0,
    0, 0, 1, 0, 0,
    0, 0, 0, 0, 0),
  nrow = 5, ncol = 5, byrow = TRUE,
  dimnames = list(sprintf("Actor %d", 1:5),
                  sprintf("Actor %d", 1:5))
)

# defining objects
networkState <- defineNetwork(
  matrix = networkState, nodes = actorsEx,
  directed = TRUE)
networkState <- linkEvents(
  x = networkState,
  changeEvent = eventsIncrement,
  nodes = actorsEx)
depNetwork <- defineDependentEvents(
  events = eventsIncrement,
  nodes = actorsEx,
  defaultNetwork = networkState)

# exogenous network
eventsExogenous <- data.frame(
  time =
    c(7, 14, 15, 18, 18, 25, 25),
  sender = sprintf("Actor %d",
    c(4,  2,  5,  4,  4,  1,  3)),
  receiver = sprintf("Actor %d",
    c(2,  3,  1,  5,  2,  3,  5)),
  increment =
    c(1,  1,  3,  1, -1,   2, 3),
  stringsAsFactors = FALSE
)

networkExog <- matrix(
  c(0, 0, 0, 1, 0,
    0, 0, 0, 0, 0,
    0, 2, 0, 0, 0,
    1, 0, 0, 0, 0,
    1, 2, 0, 0, 0),
  nrow = 5, ncol = 5, byrow = TRUE,
  dimnames = list(sprintf("Actor %d", 1:5),
                  sprintf("Actor %d", 1:5))
)

# define goldfish objects
networkExog <- defineNetwork(
  matrix = networkExog,
  nodes = actorsEx, directed = TRUE)
networkExog <- linkEvents(
  x = networkExog,
  changeEvent = eventsExogenous,
  nodes = actorsEx)


# DyNAM-i -----------------------------------------------------------
# Attributes --------------------------------------------------------
actors_DyNAMi <- data.frame(
  label = sprintf("Actor %d", 1:4),
  present = rep(TRUE, 4),
  attr1 = c(20, 22, 26, 30),
  attr2 = c(1, 0, 1, 0),
  stringsAsFactors = FALSE
)

groups_DyNAMi <- data.frame(
  label = sprintf("Group %d", 1:4),
  present = rep(TRUE, 4),
  stringsAsFactors = FALSE
)

compchanges_DyNAMi <- data.frame(
  time = c(6, 11, 11, 20, 25, 25),
  node = sprintf("Group %d", c(1, 3, 4, 1, 3, 4)),
  replace = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

# Actor x Group matrix ----------------------------------------------
covnetwork_DyNAMi <- matrix(
  c(0, 1, 1, 0,
    1, 0, 1, 0,
    1, 1, 0, 0,
    0, 0, 0, 0),
  nrow = 4, ncol = 4, byrow = TRUE,
  dimnames = list(sprintf("Actor %d", 1:4),
                  sprintf("Group %d", 1:4))
)

# Events ------------------------------------------------------------
depevents_DyNAMi <- data.frame(
  time =      c(5, 10, 10, 20, 20, 20, 25, 25),
  sender =    sprintf("Actor %d", c(1, 3, 4, 1, 3, 3, 1, 4)),
  receiver =  sprintf("Group %d", c(2, 2, 2, 2, 2, 1, 1, 2)),
  increment = c(1, 1, 1, -1, -1, 1, -1, -1),
  stringsAsFactors = FALSE
)
attr(depevents_DyNAMi,"order") <- c(1, 4, 8, 13, 15, 17, 20, 22)
class(depevents_DyNAMi) <-
  c(class(depevents_DyNAMi), "interaction.groups.updates")

exoevents_DyNAMi <- data.frame(
  time =      c(5, 10, 10, 20, 20, 20, 25, 25),
  sender =    sprintf("Actor %d", c(1, 3, 4, 1, 3, 3, 1, 4)),
  receiver =  sprintf("Group %d", c(1, 3, 4, 1, 3, 3, 3, 4)),
  increment = c(-1, -1, -1, 1, 1, -1, 1, 1),
  stringsAsFactors = FALSE
)
attr(exoevents_DyNAMi,"order") <- c(3, 7, 12, 14, 16, 19, 21, 23)
class(exoevents_DyNAMi) <-
  c(class(exoevents_DyNAMi), "interaction.groups.updates")

pastupdates_DyNAMi <- data.frame(
  time =      c(5, 10, 10, 10, 10, 10, 20),
  sender =    sprintf("Actor %d", c(1, 3, 3, 4, 4, 4, 3)),
  receiver =  sprintf("Actor %d", c(2, 1, 2, 1, 2, 3, 1)),
  increment = c(1, 1, 1, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)
attr(pastupdates_DyNAMi,"order") <- c(2, 5, 6, 9, 10, 11, 18)
class(pastupdates_DyNAMi) <-
  c(class(pastupdates_DyNAMi), "interaction.network.updates")

# goldfish Objects --------------------------------------------------
actors_DyNAMi <- defineNodes(actors_DyNAMi)
groups_DyNAMi <- defineNodes(groups_DyNAMi)
#groups <- linkEvents(x = groups, compchanges, attribute = "present")

initnetwork_DyNAMi <- structure(
  diag(x = 1, nrow(actors_DyNAMi), nrow(actors_DyNAMi)),
  dimnames = list(sprintf("Actor %d", 1:4), sprintf("Group %d", 1:4)))

interaction_network_DyNAMi <- defineNetwork(
  matrix = initnetwork_DyNAMi,
  nodes = actors_DyNAMi, nodes2 = groups_DyNAMi, directed = TRUE)

interaction_network_DyNAMi <- linkEvents(
  x = interaction_network_DyNAMi, changeEvent = depevents_DyNAMi,
  nodes = actors_DyNAMi, nodes2 = groups_DyNAMi)
interaction_network_DyNAMi <- linkEvents(
  x = interaction_network_DyNAMi, changeEvent = exoevents_DyNAMi,
  nodes = actors_DyNAMi, nodes2 = groups_DyNAMi)

past_network_DyNAMi <- defineNetwork(nodes = actors_DyNAMi, directed = FALSE)
past_network_DyNAMi <- linkEvents(
  x = past_network_DyNAMi, changeEvents = pastupdates_DyNAMi,
  nodes = actors_DyNAMi)

dependent.depevents_DyNAMi <- defineDependentEvents(
  events = depevents_DyNAMi,
  nodes = actors_DyNAMi, nodes2 = groups_DyNAMi,
  defaultNetwork = interaction_network_DyNAMi)
