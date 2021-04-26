# Data for effects tests: DyNAM-i choice --------------------------------

actors_DyNAMi_c <- data.frame(
  label = sprintf("Actor %d", 1:4),
  present = rep(TRUE, 4),
  attr1 = c(20, 22, 26, 30),
  attr2 = c(1, 0, 1, 0),
  stringsAsFactors = FALSE
)

groups_DyNAMi_c <- data.frame(
  label = sprintf("Group %d", 1:4),
  present = rep(TRUE, 4),
  stringsAsFactors = FALSE
)

compchanges_DyNAMi_c <- data.frame(
  time = c(6, 11, 11, 20, 25, 25),
  node = sprintf("Group %d", 
                   c(1, 3, 4, 1, 3, 4)),
  replace = c(F, F, F, T, T, T),
  stringsAsFactors = FALSE
)

covnetwork_DyNAMi_c <- matrix(
  c(0, 1, 1, 0,
    1, 0, 1, 0, 
    1, 1, 0, 0,
    0, 0, 0, 0), 
  nrow = 4, ncol = 4, byrow = TRUE,
  dimnames = list(sprintf("Actor %d", 1:4),
                  sprintf("Group %d", 1:4))
)

depevents_DyNAMi_c <- data.frame(
  time =      c(5, 10, 10, 20, 20, 20, 25, 25),
  sender =    sprintf("Actor %d", c(1, 3, 4, 1, 3, 3, 1, 4)),
  receiver =  sprintf("Group %d", c(2, 2, 2, 2, 2, 1, 1, 2)),
  increment = c(1, 1, 1, -1, -1, 1, -1, -1),
  stringsAsFactors = FALSE
)
attr(depevents_DyNAMi_c,"order") <- c(1, 4, 8, 13, 15, 17, 20, 22)
class(depevents_DyNAMi_c) <- c(class(depevents_DyNAMi_c), "interaction.groups.updates")

exoevents_DyNAMi_c <- data.frame(
  time =      c(5, 10, 10, 20, 20, 20, 25, 25),
  sender =    sprintf("Actor %d", c(1, 3, 4, 1, 3, 3, 1, 4)),
  receiver =  sprintf("Group %d", c(1, 3, 4, 1, 3, 3, 3, 4)),
  increment = c(-1, -1, -1, 1, 1, -1, 1, 1),
  stringsAsFactors = FALSE
)
attr(exoevents_DyNAMi_c,"order") <- c(3, 7, 12, 14, 16, 19, 21, 23)
class(exoevents_DyNAMi_c) <- c(class(exoevents_DyNAMi_c), "interaction.groups.updates")

pastupdates_DyNAMi_c <- data.frame(
  time =      c(5, 10, 10, 10, 10, 10, 20),
  sender =    sprintf("Actor %d", c(1, 3, 3, 4, 4, 4, 3)),
  receiver =  sprintf("Actor %d", c(2, 1, 2, 1, 2, 3, 1)),
  increment = c(1, 1, 1, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)
attr(pastupdates_DyNAMi_c,"order") <- c(2, 5, 6, 9, 10, 11, 18)
class(pastupdates_DyNAMi_c) <- c(class(pastupdates_DyNAMi_c), "interaction.network.updates")

# defining objects
actors_DyNAMi_c <- defineNodes(actors_DyNAMi_c)
groups_DyNAMi_c <- defineNodes(groups_DyNAMi_c)
#groups <- linkEvents(x = groups, compchanges, attribute = "present")

options(warn = -1)

initnetwork_DyNAMi_c <- structure(diag(x = 1, nrow(actors_DyNAMi_c), nrow(actors_DyNAMi_c)),
                                  dimnames = list(sprintf("Actor %d", 1:4), sprintf("Group %d", 1:4)))
interaction_network_DyNAMi_c <- defineNetwork(matrix = initnetwork_DyNAMi_c,
                                              nodes = actors_DyNAMi_c, nodes2 = groups_DyNAMi_c, directed = TRUE)
interaction_network_DyNAMi_c <- linkEvents(x = interaction_network_DyNAMi_c, changeEvent = depevents_DyNAMi_c,
                                           nodes = actors_DyNAMi_c, nodes2 = groups_DyNAMi_c)
interaction_network_DyNAMi_c <- linkEvents(x = interaction_network_DyNAMi_c, changeEvent = exoevents_DyNAMi_c,
                                           nodes = actors_DyNAMi_c, nodes2 = groups_DyNAMi_c)
past_network_DyNAMi_c <- defineNetwork(nodes = actors_DyNAMi_c, directed = F)
past_network_DyNAMi_c <- linkEvents(x = past_network_DyNAMi_c, changeEvents = pastupdates_DyNAMi_c,
                                    nodes = actors_DyNAMi_c)

dependent.depevents_DyNAMi_c <- defineDependentEvents(events = depevents_DyNAMi_c,
                                                      nodes = actors_DyNAMi_c, nodes2 = groups_DyNAMi_c,
                                                      defaultNetwork = interaction_network_DyNAMi_c)


options(warn = 0)
