# Data for effects tests: directed network --------------------------------
eventsIncrement <- data.frame(
  time = cumsum(    c(1, 5, 3, 4, 2, 1, 3, 4, 5, 1, 3, 4)),
  sender = sprintf("Actor %d",
                    c(1, 3, 2, 2, 5, 1, 3, 3, 4, 2, 5, 1)),
  receiver = sprintf("Actor %d",
                    c(2, 2, 3, 3, 1, 5, 4, 4, 2, 3, 2, 2)),
  increment =       c(1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)

actors <- data.frame(
  label = sprintf("Actor %d", 1:5),
  present = rep(TRUE, 5),
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
