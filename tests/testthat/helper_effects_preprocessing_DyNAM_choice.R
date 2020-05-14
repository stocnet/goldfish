# Data for effects tests: directed network --------------------------------
eventsIncrement <- data.frame(
  time = cumsum(c(1, 5, 3, 4, 2, 1, 3, 4, 5, 1, 3, 4)),
  sender = sprintf(
    "Actor %d",
    c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5, 1)
  ),
  receiver = sprintf(
    "Actor %d",
    c(2, 2, 3, 3, 1, 5, 4, 4, 2, 3, 2, 2)
  ),
  increment = c(1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)

actorsEx <- data.frame(
  label = sprintf("Actor %d", 1:5),
  present = rep(TRUE, 5),
  attr1 = c(9.9, 0.1, 0.5, 0.45, 0.25),
  stringsAsFactors = FALSE
)

networkState <- matrix(
  c(
    0, 3, 0, 0, 0,
    1, 0, 1, 1, 0,
    0, 0, 0, 1, 0,
    0, 0, 1, 0, 0,
    0, 0, 0, 0, 0
  ),
  nrow = 5, ncol = 5, byrow = TRUE,
  dimnames = list(
    sprintf("Actor %d", 1:5),
    sprintf("Actor %d", 1:5)
  )
)

# defining objects
networkState <- defineNetwork(matrix = networkState, nodes = actorsEx, directed = TRUE)
networkState <- linkEvents(x = networkState, changeEvents = eventsIncrement, nodes = actorsEx)
depNetwork <- defineDependentEvents(events = eventsIncrement, nodes = actorsEx, defaultNetwork = networkState)

# exogenous network
eventsExogenous <- data.frame(
  time = c(7, 14, 15, 18, 18, 25, 25),
  sender = sprintf(
    "Actor %d",
    c(4, 2, 5, 4, 4, 1, 3)
  ),
  receiver = sprintf(
    "Actor %d",
    c(2, 3, 1, 5, 2, 3, 5)
  ),
  increment = c(1, 1, 3, 1, -1, 2, 3),
  stringsAsFactors = FALSE
)

networkExog <- matrix(
  c(
    0, 0, 0, 1, 0,
    0, 0, 0, 0, 0,
    0, 2, 0, 0, 0,
    1, 0, 0, 0, 0,
    1, 2, 0, 0, 0
  ),
  nrow = 5, ncol = 5, byrow = TRUE,
  dimnames = list(
    sprintf("Actor %d", 1:5),
    sprintf("Actor %d", 1:5)
  )
)

networkExog <- defineNetwork(matrix = networkExog, nodes = actorsEx, directed = TRUE)
networkExog <- linkEvents(x = networkExog, changeEvent = eventsExogenous, nodes = actorsEx)
