# Helper functions for tests
# Not sure if loading of libraries is needed.
library(dplyr)
library(purrr)
library(manynet)
library(igraph)
library(tidygraph)


data("Social_Evolution")
# Small description of the data: 
#  - actors: 84 students with floor and gradeType attributes
#  - calls: directed network of phone calls among students between "2008-09-06 22:37:50 CEST" and "2008-10-19 01:39:12 CEST"
#  - friendship: two surveys of friendships recorded at  "2008-09-09 02:00:00 CEST" and "2008-10-19 02:00:00 CEST"

nodes <- actors |>
  rename(active = present)

edges_calls <- calls |>
  rename(from = sender, to = receiver) |>
  mutate(type = "call") |>
  mutate(flavour = "call")

# Define adjacency matrices with the nodeset as `nodes`
timestamp_matrix_1 <- friendship |>
  filter(time == unique(time)[1]) |>
  select(2, 3) |>
  graph_from_data_frame(directed = TRUE, vertices = nodes) |>
  as_adjacency_matrix()

timestamp_matrix_2 <- friendship |>
  filter(time == unique(time)[2]) |>
  select(2, 3) |>
  graph_from_data_frame(directed = TRUE, vertices = nodes) |>
  as_adjacency_matrix()

# Define edges_friendship as the changes between timestamp_matrix_1 and timestamp_matrix_2, recording the difference in an `increment` column
edges_friendship <- as_data_frame(graph_from_adjacency_matrix(timestamp_matrix_2 - timestamp_matrix_1, mode = "directed", weighted = TRUE)) |>
  rename(increment = weight) |>
  filter(increment != 0) |>
  mutate(
    time = unique(friendship$time)[1],
    type = "friendship",
    flavour = ifelse(increment > 0, "creation", "deletion")
  )
 
total_edges <- edges_calls |>
  rbind(edges_friendship) 

my_data <- tbl_graph(
  nodes = nodes,
  edges = total_edges,
  directed = TRUE
) 
my_data <- structure(my_data, class = c("mnet", class(my_data)))

start_time <- unique(friendship$time)[1]
end_time <- unique(friendship$time)[2]

 
# Sample formulas

friend_spec <- make_specification(
  rate = list(
    creation ~ 1 + outdeg(calls),
    deletion ~ 1 + outdeg(friendship)
  ),
  choice = list(
    creation ~ recip + tie(calls),
    deletion ~ indeg + alter(floor)
  ),
  model = "icecream"
)

calls_spec <- make_specification(
  rate = calls ~ 1 + outdeg(calls),
  choice = calls ~ cycle + tie(friendship) + mixed_trans(list(friendship, calls)),
  model = "icecream"
)

coevol <- make_multivariate_spec(
  friend_spec,
  calls_spec,
  data = my_data
)


# Updated sample effects based on the sample specifications
# In the example specifications we have 9 unique effects in total: 
#   1. outdegree(calls)
#   2. outdegree(friendship)
#   3. reciprocity
#   4. tie(calls)
#   5. tie(friendship)
#   6. indegree
#   7. alter(floor)
#   8. cycle
#   9. mixed_trans(friendship,calls)



# Examples of the input data contest for preprocessing.

formulas_rate <- data.frame(
    fid = c(1, 2, 5),  # Global formula ID (unique across all formulas)
    type = c("friendship", "friendship", "call"),  # "friendship", "call"
    flavour = c("creation", "deletion", "call"),  # "creation", "deletion", NA_character_
    intercept = c(TRUE, TRUE, TRUE),  # Logical for intercept
)
formulas_choice <- data.frame(
    fid = c(3, 4, 6),  # Global formula ID (unique across all formulas)
    type = c("friendship", "friendship", "call"),  # "friendship", "call"
    flavour = c("creation", "deletion", "call"),  # "creation", "deletion", NA_character_
    intercept = c(FALSE, FALSE, FALSE),  # Logical for intercept
)


effects_rate = data.frame(
    gid = c(1, 2),              # Global effect ID (unique across all formulas)
    effect_name = c("outdeg", "outdeg"),    # "indeg", "recip", "tie", etc.
    object = I(list("calls", "friendship")),         # Object/event(s) the effect depends on, a character vector of arguments in the order given by the effect call
    params = I(list(c(0,0),0)),          # List of parameters with defaults (or additional variables)
    stringsAsFactors = FALSE
)

effects_choice = data.frame(
    gid = c(3, 4, 5, 6, 7, 8, 9),              # Global effect ID (unique across all formulas)
    effect_name = c("recip",  "tie", "tie", "indeg", "alter", "cycle", "mixed_trans"),    # "indeg", "recip", "tie", etc.
    object = I(list("friendship", "calls", "friendship", "friendship", "floor", "calls", c("friendship", "calls"))),         # Object/event(s) the effect depends on, a character vector of arguments in the order given by the effect call
    params = I(list(0,0,0,0,0,0,0)),          # List of parameters with defaults (or additional variables)
    stringsAsFactors = FALSE
)

# rows = gids, cols = fids, values = lid (or NA/0)
formulas_effects_rate = matrix(0, nrow = 2, ncol = 3)
col_names(formulas_effects_rate) <- c(1,2,5)  # fids
row_names(formulas_effects_rate) <- c(1,2)    # gids
formulas_effects_rate[1, c(1,3)] <- 1  # outdeg(calls) in formula 1 and 
formulas_effects_rate[2, c(2)] <- 1  # outdeg(friendship) in formula 2  

formulas_effects_choice = matrix(0, nrow = 7, ncol = 3)
col_names(formulas_effects_choice) <- c(3,4,6)  # fids
row_names(formulas_effects_choice) <- c(3,4,5,6,7,8,9)    # gids
formulas_effects_choice[, 1] <- c(1,2,rep(0,5)) #lids
formulas_effects_choice[, 2] <- c(0,0,0,1,2,0,0) # lids
formulas_effects_choice[, 3] <- c(0,0,2,0,0,1,3) # lids

# Assume in the following:
objects <- list (
  "networks" = array(0, dim = c(84, 84, 2), dimnames = list(NULL, NULL, c("friendship", "calls"))),  # 2 networks
  "nodal_covariate" = matrix(0, nrow = 84, ncol = 3, dimnames = list(NULL, c("present","floor","gradeType")))  # 1 nodal covariate
)

objects_meta_rate = data.frame(
    oid = c(1,2,1,2,3),              # Object ID: position in the respective object (matrix/array)
    name = c("friendship", "calls", "present", "floor", "gradeType"),        # Covariate name, network name
    class = c("numeric","numeric","lgl","numeric","numeric"),        # "factor", "numeric"
    missing = c(FALSE, FALSE, FALSE, FALSE, TRUE),            # Is there missing data?
    gid = I(list(2),c(1),NULL,NULL,NULL),            # effect IDs,
    kind = c("network", "network", "nodal_covariate", "nodal_covariate", "nodal_covariate")         # "nodal_covariate", "network"
)

objects_meta_choice = data.frame(
    oid = c(1,2,1,2,3),              # Object ID: position in the respective object (matrix/array)
    name = c("friendship", "calls", "present", "floor", "gradeType"),        # Covariate name, network name
    class = c("numeric","numeric","lgl","numeric","numeric"),        # "factor", "numeric"
    missing = c(FALSE, FALSE, FALSE, FALSE, TRUE),            # Is there missing data?
    gid = I(list(3,5,6,9),c(4,8,9),NULL,c(7),NULL),            # effect IDs,
    kind = c("network", "network", "nodal_covariate", "nodal_covariate", "nodal_covariate")         # "nodal_covariate", "network"
)


event_effect_link_rate = matrix(0, nrow = 5, ncol = 2)
row_names(event_effect_link_rate) <- c("friendship", "calls","present", "floor", "gradeType")    
colnames(event_effect_link_rate) <- c(1,2)
event_effect_link_rate["friendship", 1] <- 1
event_effect_link_rate["calls", 1] <- 1

event_effect_link_choice = matrix(0, nrow = 5, ncol = 7)
row_names(event_effect_link_choice) <- c("friendship", "calls","present", "floor", "gradeType")    
colnames(event_effect_link_choice) <- c(3,4,5,6,7,8,9)
event_effect_link_choice["friendship", c(1,3,4,7)] <- 1
event_effect_link_choice["calls", c(2,6)] <- 1
event_effect_link_choice["calls", 7] <- 2
event_effect_link_choice["floor", 5] <- 1


parsing_info <- list(
  # Submodel-specific data
  rate = list(
    effects = effects_rate,      # As defined in section 2.2
    formulas = formulas_rate,    # As defined in section 2.2

    # Linking tables (shared)
    object_registry = objects_meta_rate,
    event_effects = event_effect_link_rate,
    formula_effects = formulas_effects_rate

  ),

  choice = list(
    effects = effects_choice,      # As defined in section 2.2
    formulas = formulas_choice,    # As defined in section 2.2

    # Linking tables (shared)
    object_registry = objects_meta_choice,
    event_effects = event_effect_link_choice,
    formula_effects = formulas_effects_choice
  ),

  rem = list(

  )
)
