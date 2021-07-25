# define methods ----------------------------------------------------------
# init cache data structure: vector or matrix
init_DyNAMi_choice <- function(effectFun, network, attribute)
  UseMethod("init_DyNAMi_choice", effectFun)

# default -----------------------------------------------------------------

#init_DyNAMi_choice.default <- function(effectFun, network, attribute)
#  NULL  # # effect without cache object

init_DyNAMi_choice.default <- function(effectFun,
                                       network = NULL, attribute = NULL,
                                       groupsNetwork, window,
                                       n1, n2) {

  # print(match.call())
  if (is.null(network) && is.null(attribute)) {
    # this check could be unnecessary
    stop("the effect function doesn't specify neither a network nor an attribute as argument")
  }

  # if multiple networks, attributes or combination of both are specified.
  # The initialization is done over the fist network
  # lenNetwork <- length(network)
  hasNetwork  <- length(network)   >= 1
  hasMultNets <- length(network)   >= 1 & is.list(network)
  hasMultAtt  <- length(attribute) >= 1 & is.list(attribute)

  .argsNames <- names(formals(effectFun))
  # if network inputs, just the first network is empty.
  stats <- matrix(0, nrow = n1, ncol = n2) # check for poss

  if (hasNetwork) {
    # check if not empty network to initialize the statistical matrix
    # create a copy of the network to iterate over
    if (hasMultNets) {
      areEmpty <- vapply(network, function(x) all(x[!is.na(x)] == 0), logical(1))
      if ((!is.null(window) && !is.infinite(window)) || any(areEmpty)) {
        return(stats)
      }
      netIter <- network[[1]]
    } else {
      if ((!is.null(window) && !is.infinite(window)) || all(network[!is.na(network)] == 0)) {
        return(stats)
      }
      netIter <- network
    }

    emptyObject <- array(0, dim = dim(netIter))
  } else {
    if (hasMultAtt) {
      areEmpty <- vapply(attribute, function(x) all(x[!is.na(x)] == 0), logical(1))
      if (any(areEmpty)) {
        return(stats)
      }
      attIter <- attribute[[1]]
    } else {
      if (all(attribute[!is.na(attribute)] == 0)) {
        return(stats)
      }
      attIter <- attribute
    }

    emptyObject <- vector(mode = "numeric", length = length(attIter))
  }
  # iterate over not empty entries and compute updates
  if (hasNetwork) {
    # it has define network(s) as argument(s)
    # not empty rows
    # rowsIter <- which(rowSums(netIter != 0, na.rm = TRUE) > 0)
    for (i in seq.int(n1)) {
      # colsIter <- which(!is.na(netIter[i, ]) & netIter[i, ] != 0)
      for (j in seq.int(n2)) {
        # feed empty object to the effect function
        if (hasMultNets) {
          netArg <- network
          netArg[[1]] <- emptyObject
        } else {
          netArg <- emptyObject
        }
        # set arguments values and only keep the ones in formals(effectFun)
        .argsFUN <- list(
          network = netArg,
          attribute = attribute,
          sender = i,
          receiver = j,
          replace = netIter[i, j],
          n1 = if ("n1" %in% .argsNames) n1 else NULL,
          n2 = if ("n2" %in% .argsNames) n2 else NULL,
          groupsNetwork = groupsNetwork,
          statistics = stats
        )
        .argsKeep <- pmatch(.argsNames, names(.argsFUN))
        # construct network objects step by step from empty objects
        res <- do.call(effectFun, .argsFUN[na.omit(.argsKeep)])
        if (!is.null(res) && nrow(res) > 0) {
          stats[cbind(res[, 1], res[, 2])] <- res[, 3]
        }
        # update networks
        # hack: if it's not the same dimension, the network shouldn't be updated
        if (dim(netIter)[1] == n1 && dim(netIter)[2] == n2)
          emptyObject[i, j] <- netIter[i, j]
      }
    }
  } else {
    # just attribute(s)
    nodesIter <- which(!is.na(attIter) & attIter != 0)
    for (i in nodesIter) {
      # feed empty object to the effect function
      if (hasMultAtt) {
        attArg <- attribute
        attArg[[1]] <- emptyObject
      } else {
        attArg <- emptyObject
      }
      # set arguments values and only keep the ones in formals(effectFun)
      # PATCH Marion
      attArg <- attribute
      .argsFUN <- list(
        attribute = attArg,
        node = i,
        replace = attIter[i],
        n1 = if ("n1" %in% .argsNames) n1 else NULL,
        n2 = if ("n2" %in% .argsNames) n2 else NULL,
        groupsNetwork = groupsNetwork,
        statistics = stats
      )
      .argsKeep <- pmatch(.argsNames, names(.argsFUN))
      # construct network objects step by step from empty objects
      res <- do.call(effectFun, .argsFUN[na.omit(.argsKeep)])
      if (!is.null(res) && nrow(res) > 0) {
        stats[cbind(res[, 1], res[, 2])] <- res[, 3]
      }
      # update cache if any
      # update networks
      emptyObject[i] <- attIter[i]
    }
  }

  return(stats)
}

# Structural effects ------------------------------------------------------
# tie ---------------------------------------------------------------------
# init_DyNAMi_choice_tie <- function()

update_DyNAMi_choice_tie <- function(network,
                                    groupsNetwork,
                                    sender, receiver, replace,
                                    n1, n2, statistics,
                                    weighted = FALSE, subType = "proportion") {

  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {

      members <- which(groupsNetwork[, j] == 1)
      nmembers <- length(members)

      if (nmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      smembers <- members[members != i]
      snmembers <- length(smembers)
      if (snmembers == 0) {
        if (statistics[i, j] != 0)
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))

        next
      }

      if (subType == "count") {
        rep <- sum(network[i, smembers] > 0)
      }
      if (subType == "proportion") {
        rep <- sum(network[i, smembers] > 0) / snmembers
      }
      if (subType == "presence") {
        rep <- max(network[i, smembers] > 0)
      }
      if (subType == "min") {
        rep <- min(network[i, smembers])
      }
      if (subType == "mean") {
        rep <- mean(network[i, smembers])
      }
      if (subType == "max") {
        rep <- max(network[i, smembers])
      }

      if (statistics[i, j] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = rep))
      }
    }
  }

  return(reptotal)
}

# inertia -----------------------------------------------------------------
# init_DyNAMi_choice_inertia <- function()

update_DyNAMi_choice_inertia <- function(network,
                                         groupsNetwork,
                                         sender, receiver, replace,
                                         n1, n2, statistics,
                                         weighted = FALSE, subType = "proportion") {

  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {

      members <- which(groupsNetwork[, j] == 1)
      nmembers <- length(members)

      if (nmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      smembers <- members[members != i]
      snmembers <- length(smembers)
      if (snmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      if (subType == "count") {
        rep <- sum(network[i, smembers] > 0)
      }
      if (subType == "proportion") {
        rep <- sum(network[i, smembers] > 0) / snmembers
      }
      if (subType == "presence") {
        rep <- max(network[i, smembers] > 0)
      }
      if (subType == "min") {
        rep <- min(network[i, smembers])
      }
      if (subType == "mean") {
        rep <- mean(network[i, smembers])
      }
      if (subType == "max") {
        rep <- max(network[i, smembers])
      }

      if (statistics[i, j] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = rep))
      }
    }
  }

  return(reptotal)
}


# alterdeg -------------------------------------------------------------------
# init_DyNAMi_choice_alterdeg <- function()

#' alterdeg effects DyNAM-i choice
#' @importFrom stats sd
#' @noRd
update_DyNAMi_choice_alterdeg <- function(network,
                                          groupsNetwork,
                                          sender, receiver, replace,
                                          n1, n2, statistics,
                                          weighted = FALSE, subType = "mean") {

  reptotal <- NULL
  meandeg <- mean(rowSums(network))
  sddeg <- sd(rowSums(network))

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groupsNetwork[, j] == 1)
      nmembers <- length(members)
      if (nmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      smembers <- members[members != i]
      snmembers <- length(smembers)
      if (snmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      if (snmembers == 1) {
        if (subType == "mean") {
          rep <- sum(network[smembers, ])
        }
        if (subType == "mean_centered") {
          rep <- sum(network[smembers, ]) - meandeg
        }
        if (subType == "mean_normalized") {
          if (sddeg > 0) rep <- (sum(network[smembers, ]) - meandeg) / sddeg else rep <- 0
        }
        if (subType == "min") {
          rep <- sum(network[smembers, ])
        }
        if (subType == "max") {
          rep <- sum(network[smembers, ])
        }
      } else{
        if (subType == "mean") {
          rep <- mean(rowSums(network[smembers, ]))
        }
        if (subType == "mean_centered") {
          rep <- mean(rowSums(network[smembers, ])) - meandeg
        }
        if (subType == "mean_normalized") {
          if (sddeg > 0) rep <- (mean(rowSums(network[smembers, ])) - meandeg) / sddeg else rep <- 0
        }
        if (subType == "min") {
          rep <- min(rowSums(network[smembers, ]))
        }
        if (subType == "max") {
          rep <- max(rowSums(network[smembers, ]))
        }
      }

      if (statistics[i, j] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = rep))
      }

    }

  }

  return(reptotal)
}


# alterpop -------------------------------------------------------------------
# init_DyNAMi_choice_alterpop <- function()

update_DyNAMi_choice_alterpop <- function(network,
                                          groupsNetwork,
                                          sender, receiver, replace,
                                          n1, n2, statistics,
                                          weighted = FALSE, subType = "mean_normalized") {
update_DyNAMi_choice_alterdeg(network = network,
                              groupsNetwork = groupsNetwork,
                              sender = sender, receiver = receiver, replace = replace,
                              n1 = n1, n2 = n2, statistics = statistics,
                              weighted = weighted, subType = subType)
}


# size -------------------------------------------------------------------
# init_DyNAMi_choice_size <- function()

update_DyNAMi_choice_size <- function(network,
                                      groupsNetwork,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      weighted = FALSE, subType = "identity") {

  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groupsNetwork[, j] == 1)
      nmembers <- length(members)
      if (nmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      if (subType == "identity") {
        rep <- nmembers
      }
      if (subType == "squared") {
        rep <- nmembers^2
      }
      if (subType == "dummy") {
        rep <- nmembers > 2
      }

      if (statistics[i, j] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = rep))
      }
    }
  }

  return(reptotal)
}


# dyad -------------------------------------------------------------------
# init_DyNAMi_choice_dyad <- function()

update_DyNAMi_choice_dyad <- function(network,
                                      groupsNetwork,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      weighted = FALSE, subType = "identity") {

  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groupsNetwork[, j] == 1)
      nmembers <- length(members)
      if (nmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      if (subType == "identity") {
        if (nmembers == 1) {
          rep <- 1
        } else {
          rep <- 0
        }
      }

      if (statistics[i, j] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = rep))
      }
    }
  }

  return(reptotal)
}


# Covariate effects -------------------------------------------------------

# alter -------------------------------------------------------------------
# init_DyNAMi_choice_alter <- function()

update_DyNAMi_choice_alter <- function(attribute,
                                       groupsNetwork,
                                       sender, receiver, replace,
                                       n1, n2, statistics,
                                       subType = "mean",
                                       node = 0) {

  reptotal <- NULL
  meanatt <- mean(attribute)
  sdatt <- sd(attribute)

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groupsNetwork[, j] == 1)
      nmembers <- length(members)
      if (nmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      smembers <- members[members != i]
      snmembers <- length(smembers)
      if (snmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      if (subType == "mean") {
        rep <- mean(attribute[smembers])
      }
      if (subType == "mean_squared") {
        rep <- mean(attribute[smembers])^2
      }
      if (subType == "mean_centered") {
        rep <- mean(attribute[smembers]) - meanatt
      }
      if (subType == "mean_normalized") {
        if (sdatt > 0) rep <- (mean(attribute[smembers]) - meanatt) / sdatt else rep <- 0
      }
      if (subType == "min") {
        rep <- min(attribute[smembers])
      }
      if (subType == "max") {
        rep <- max(attribute[smembers])
      }
      if (subType == "range") {
        rep <- max(attribute[smembers]) - min(attribute[smembers])
      }

      if (statistics[i, j] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = rep))
      }
    }
  }

  return(reptotal)
}

# same --------------------------------------------------------------------
# init_DyNAMi_choice_same <- function()

update_DyNAMi_choice_same <- function(attribute,
                                      groupsNetwork,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      subType = "proportion",
                                      node = 0) {
  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groupsNetwork[, j] == 1)
      nmembers <- length(members)
      if (nmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      smembers <- members[members != i]
      snmembers <- length(smembers)
      if (snmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      if (subType == "proportion") {
        rep <- sum(attribute[smembers] == attribute[i]) / snmembers
      }
      if (subType == "count") {
        rep <- sum(attribute[smembers] == attribute[i])
      }
      if (subType == "presence") {
        rep <- min(attribute[smembers] == attribute[i])
      }

      if (statistics[i, j] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = rep))
      }
    }
  }

  return(reptotal)
}

# diff --------------------------------------------------------------------
# init_DyNAMi_choice_diff <- function()

update_DyNAMi_choice_diff <- function(attribute,
                                      groupsNetwork,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      subType = "averaged_sum",
                                      node = 0) {
  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groupsNetwork[, j] == 1)
      nmembers <- length(members)
      if (nmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      smembers <- members[members != i]
      snmembers <- length(smembers)
      if (snmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      if (subType == "averaged_sum") {
        rep <- sum(abs(attribute[smembers] - attribute[i])) / snmembers
      }
      if (subType == "mean") {
        rep <- abs(mean(attribute[smembers]) - attribute[i])
      }
      if (subType == "min") {
        rep <- abs(min(attribute[smembers]) - attribute[i])
      }
      if (subType == "max") {
        rep <- abs(max(attribute[smembers]) - attribute[i])
      }

      if (statistics[i, j] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = rep))
      }
    }
  }

  return(reptotal)
}


# sim ---------------------------------------------------------------------
# init_DyNAMi_choice_sim <- function()

update_DyNAMi_choice_sim <- function(attribute,
                                     groupsNetwork,
                                     sender, receiver, replace,
                                     n1, n2, statistics,
                                     subType = "averaged_sum",
                                     node = 0) {
  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groupsNetwork[, j] == 1)
      nmembers <- length(members)
      if (nmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      smembers <- members[members != i]
      snmembers <- length(smembers)
      if (snmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      if (subType == "averaged_sum") {
        rep <- (-1) * sum(abs(attribute[smembers] - attribute[i])) / snmembers
      }
      if (subType == "mean") {
        rep <- (-1) * abs(mean(attribute[smembers]) - attribute[i])
      }
      if (subType == "min") {
        rep <- (-1) * abs(min(attribute[smembers]) - attribute[i])
      }
      if (subType == "max") {
        rep <- (-1) * abs(max(attribute[smembers]) - attribute[i])
      }

      if (statistics[i, j] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = rep))
      }
    }
  }

  return(reptotal)
}

# Interaction structural and Covariate effects ----------------------------

# sizeXdiff ---------------------------------------------------------------
# init_DyNAMi_choice_sizeXdiff <- function()

update_DyNAMi_choice_sizeXdiff <- function(attribute,
                                      groupsNetwork,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      subType = "averaged_sum",
                                      node = 0) {
  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groupsNetwork[, j] == 1)
      nmembers <- length(members)
      if (nmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      smembers <- members[members != i]
      snmembers <- length(smembers)
      if (snmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      if (subType == "averaged_sum") {
        rep <- snmembers * sum(abs(attribute[smembers] - attribute[i])) / snmembers
      }
      if (subType == "mean") {
        rep <- snmembers * abs(mean(attribute[smembers]) - attribute[i])
      }
      if (subType == "min") {
        rep <- snmembers * abs(min(attribute[smembers]) - attribute[i])
      }
      if (subType == "max") {
        rep <- snmembers * abs(max(attribute[smembers]) - attribute[i])
      }

      if (statistics[i, j] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = rep))
      }
    }
  }

  return(reptotal)
}


# dyadXdiff ---------------------------------------------------------------------
# init_DyNAMi_choice_dyadXdiff <- function()

update_DyNAMi_choice_dyadXdiff <- function(attribute,
                                           groupsNetwork,
                                           sender, receiver, replace,
                                           n1, n2, statistics,
                                           subType = "averaged_sum",
                                           node = 0) {
  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groupsNetwork[, j] == 1)
      nmembers <- length(members)
      if (nmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      smembers <- members[members != i]
      snmembers <- length(smembers)
      if (snmembers == 0) {
        if (statistics[i, j] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = 0))
        }
        next
      }

      if (snmembers == 1) {
        m <- 1
      } else {
        m <- 0
      }

      if (subType == "averaged_sum") {
        rep <- m * sum(abs(attribute[smembers] - attribute[i])) / snmembers
      }
      if (subType == "mean") {
        rep <- m * abs(mean(attribute[smembers]) - attribute[i])
      }
      if (subType == "min") {
        rep <- m * abs(min(attribute[smembers]) - attribute[i])
      }
      if (subType == "max") {
        rep <- m * abs(max(attribute[smembers]) - attribute[i])
      }

      if (statistics[i, j] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = j, replace = rep))
      }
    }
  }

  return(reptotal)
}
