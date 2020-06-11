######################################
#
# global utility functions for the TSS project
#
######################################

division <- function(a, b) a / b

resetNetwork <- function(network) matrix(0, dim(network))

binarizeNetwork <- function(network) {
  net <- 1 * (network >= 1)
  diag(net) <- 0
  net
}

# Impute missing data.
# Network missings are set to 0
# Attribute missings are set to the current mean of the variable
# If all NA's, values are set to zero
# TODO think about how this logic can be improved
imputeMissings <- function(processStateObject) {
  data <- processStateObject$data
  if (is.null(data)) {
    return(processStateObject)
  }
  if (all(!is.na(data))) {
    return(processStateObject)
  }
  if (is.matrix(data)) {
    data[is.na(data)] <- 0
    processStateObject$data <- data
    return(processStateObject)
  } else {
    if (all(is.na(data))) {
      processStateObject$data <- rep(0, length(data))
      return(processStateObject)
    }
    data[is.na(data)] <- mean(data, na.rm = TRUE)
    processStateObject$data <- data
    return(processStateObject)
  }
  processStateObject
}

######################################
#
# deprecated utility functions
#
######################################

# given a network and a window, this function returns a list where:
# - the first element is the dynamic window network
# - the associated event lists
getTimeWindowData <- function(network, window, envir = environment()) {

  # get initial network's name
  name <- as.character(substitute(network))

  # get network, create windowed network, get related events to be windowed later
  newNetwork <- network
  newName <- paste(name, window, sep = "_")
  attr(newNetwork, "events") <- NULL
  allEvents <- attr(network, "events")
  allNewEvents <- NULL

  # return the dynamic network and the windowed events
  res <- list()
  res[[newName]] <- newNetwork

  # create new windowed events lists, link them
  for (events in allEvents) {
    objectEvents <- get(events, envir = envir)
    newEvents <- createWindowedEvents(objectEvents, window)
    allNewEvents <- c(allNewEvents, newEvents)
    nameNewEvents <- paste(events, window, sep = "_")
    attr(res[[newName]], "events") <- c(attr(res[[newName]], "events"), nameNewEvents)
    res[[nameNewEvents]] <- newEvents
    if (!is.null(envir)) {
      attr(newNetwork, "events") <- c(attr(newNetwork, "events"), nameNewEvents)
      assign(nameNewEvents, newEvents, envir = envir)
    }
  }

  if (!is.null(envir)) {
    assign(newName, newNetwork, envir = envir)
  }


  if (!is.null(envir)) {
    cat("Created new objects in environment.")
    return(NULL)
  }
  return(res)
}

# TODO: This is dangerous. What if a multipleParameter disappears?
# Should multiple ONLY work with a default network?
removeMultipleEvents <- function(updates, multipleParameter, envir = environment()) {
  multIds <- which(!unlist(multipleParameter))
  for (i in multIds) {
    net <- get(names(multipleParameter)[multIds], envir = envir)
    # append all forced zero events to the event list;
    # those will overwrite any positive updates and positive values in the statistics function
    addIds <- which(net > 0, arr.ind = TRUE)
    if (nrow(addIds) > 0) {
      updates[[i]] <- rbind(
        updates[[i]],
        cbind(node1 = addIds[, 1], node2 = addIds[, 2], replace = 0)
      )
    }
  }
  return(updates)
}
