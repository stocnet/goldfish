# define methods ----------------------------------------------------------
# init cache data structure: vector or matrix
init_DyNAMi_choice <- function(effectFun, network, attribute)
  UseMethod("init_DyNAMi_choice", effectFun)

# default -----------------------------------------------------------------
init_DyNAMi_choice.default <- function(effectFun, network, attribute)
  NULL  # # effect without cache object

init_DyNAMi_choice.default <- function(effectFun,
                                    network = NULL, attribute = NULL,
                                    window,
                                    n1, n2) {
  init_DyNAM_choice.default(effectFun = effectFun,
                            network = network, attribute = attribute,
                            window = window,
                            n1 = n1, n2 = n2)
}

# Structural effects ------------------------------------------------------
# tie ---------------------------------------------------------------------
# init_DyNAMi_choice_tie <- function()

update_DyNAMi_choice_tie <- function(network,
                                    groups.network,
                                    sender, receiver, replace,
                                    n1, n2, statistics,
                                    weighted = FALSE, subType = "proportion") {

  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {

      members <- which(groups.network[, j] == 1)
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
                                        groups.network,
                                        sender, receiver, replace,
                                        n1, n2, statistics,
                                        weighted = FALSE, subType = "proportion") {

  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {

      members <- which(groups.network[, j] == 1)
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

update_DyNAMi_choice_alterdeg <- function(network,
                                          groups.network,
                                          sender, receiver, replace,
                                          n1, n2, statistics,
                                          weighted = FALSE, subType = "mean") {

  reptotal <- NULL
  meandeg <- mean(rowSums(network))
  sddeg <- sd(rowSums(network))

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groups.network[, j] == 1)
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
                                              groups.network,
                                              sender, receiver, replace,
                                              n1, n2, statistics,
                                              weighted = FALSE, subType = "mean_normalized") {
update_DyNAMi_choice_alterdeg(network,
                              groups.network,
                              sender, receiver, replace,
                              n1, n2, statistics,
                              weighted, subType)
}


# size -------------------------------------------------------------------
# init_DyNAMi_choice_size <- function()

update_DyNAMi_choice_size <- function(network,
                                      groups.network,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      weighted = FALSE, subType = "identity") {

  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groups.network[, j] == 1)
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

# Covariate effects -------------------------------------------------------

# alter -------------------------------------------------------------------
# init_DyNAMi_choice_alter <- function()

update_DyNAMi_choice_alter <- function(attribute,
                                      groups.network,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      subType = "mean",
                                      node = 0) {

  reptotal <- NULL
  meanatt <- mean(attribute)
  sdatt <- sd(attribute)

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groups.network[, j] == 1)
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
                                      groups.network,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      subType = "proportion",
                                      node = 0) {
  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groups.network[, j] == 1)
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
                                      groups.network,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      subType = "averaged_sum",
                                      node = 0) {
  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groups.network[, j] == 1)
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
                                    groups.network,
                                    sender, receiver, replace,
                                    n1, n2, statistics,
                                    subType = "averaged_sum",
                                    node = 0) {
  reptotal <- NULL

  for (i in seq.int(n1)) {
    for (j in seq.int(n2)) {
      members <- which(groups.network[, j] == 1)
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
