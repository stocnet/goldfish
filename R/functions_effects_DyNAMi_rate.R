# define methods ----------------------------------------------------------
# init cache data structure: vector or matrix
init_DyNAMi_rate <- function(effectFun, network, attribute)
  UseMethod("initCache_DyNAMi_rate", effectFun)

# default -----------------------------------------------------------------
init_DyNAMi_rate.default <- function(effectFun,
                                    network = NULL, attribute = NULL,
                                    window,
                                    n1, n2) {
  init_DyNAM_choice.default(effectFun = effectFun,
                            network = network, attribute = attribute,
                            window = window,
                            n1 = n1, n2 = n2)
}

# Structural effects ------------------------------------------------------
# intercept ---------------------------------------------------------------------
# initStat_DyNAMi_rate_intercept <- function()

update_DyNAMi_rate_intercept <- function(network,
                                      groups.network,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      weighted = FALSE,
                                      joining = 1) {

  reptotal <- NULL

  # JOINING RATE
  if (joining == 1) {

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (!isingroup) {
        if (statistics[i, 1] != 1) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 1))
        }
        next
      } else {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
      }
    }
  }

  # LEAVING RATE
  if (joining == -1) {

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (isingroup) {
        if (statistics[i, 1] != 1) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 1))
        }
        next
      } else {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
      }
    }
  }

  return(reptotal)
}


# inertia ---------------------------------------------------------------------
# initStat_DyNAMi_rate_inertia <- function()

update_DyNAMi_rate_inertia <- function(network,
                                      groups.network,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      weighted = TRUE, subType = "proportion",
                                      joining = -1) {
update_DyNAMi_rate_tie(network,
                      groups.network,
                      sender, receiver, replace,
                      n1, n2, statistics,
                      weighted, subType,
                      joining)
}



# tie ---------------------------------------------------------------------
# initStat_DyNAMi_rate_tie <- function()

update_DyNAMi_rate_tie <- function(network,
                                  groups.network,
                                  sender, receiver, replace,
                                  n1, n2, statistics,
                                  weighted = FALSE, subType = "proportion",
                                  joining = -1) {

  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {
    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (!isingroup) {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
        next
      }

      members <- which(groups.network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

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

      if (statistics[i, 1] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = rep))
      }
    }
  }

  return(reptotal)
}


# egodeg -------------------------------------------------------------------
# initStat_DyNAMi_rate_egodeg <- function()

update_DyNAMi_rate_egodeg <- function(network,
                                      groups.network,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      weighted = TRUE, subType = "identity",
                                      joining = 1) {

  reptotal <- NULL
  meandeg <- mean(rowSums(network))
  sddeg <- sd(rowSums(network))

  # JOINING RATE
  if (joining == 1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i,] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (!isingroup) {
        if (subType == "identity") {
          rep <- sum(network[i, ])
        }
        if (subType == "centered") {
          rep <- sum(network[i, ]) - meandeg
        }
        if (subType == "normalized") {
          if (sddeg > 0) rep <- (sum(network[i, ]) - meandeg) / sddeg else rep <- 0
        }

        if (statistics[i, 1] != rep) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = rep))
        }
        next
      } else {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
      }
    }
  }

  # LEAVING RATE
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (isingroup) {
        if (subType == "identity") {
          rep <- sum(network[i, ])
        }
        if (subType == "centered") {
          rep <- sum(network[i, ]) - meandeg
        }
        if (subType == "normalized") {
          if (sddeg > 0) rep <- (sum(network[i, ]) - meandeg) / sddeg else rep <- 0
        }

        if (statistics[i, 1] != rep) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = rep))
        }
        next
      } else {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
      }
    }
  }

  return(reptotal)
}


# egopop -------------------------------------------------------------------
# initStat_DyNAMi_rate_egopop <- function()

update_DyNAMi_rate_egopop <- function(network,
                                      groups.network,
                                      sender, receiver, replace,
                                      n1, n2, statistics,
                                      weighted = TRUE, subType = "normalized",
                                      joining = 1) {
  update_DyNAMi_rate_egodeg(network,
                            groups.network,
                            sender, receiver, replace,
                            n1, n2, statistics,
                            weighted, subType,
                            joining)
}

# alterdeg -------------------------------------------------------------------
# initStat_DyNAMi_rate_alterdeg <- function()

update_DyNAMi_rate_alterdeg <- function(network,
                                        groups.network,
                                        sender, receiver, replace,
                                        n1, n2, statistics,
                                        weighted = TRUE, subType = "mean",
                                        joining = -1) {

  reptotal <- NULL
  meandeg <- mean(rowSums(network))
  maxdeg <- max(rowSums(network))
  sddeg <- sd(rowSums(network))

  # LEAVING MODEL
  if (joining == -1) {

    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (!isingroup) {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
        next
      }

      members <- which(groups.network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

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
      } else {
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
          rep <- min(rowSums(network[smembers, ])) / maxdeg
        }
        if (subType == "max") {
          rep <- max(rowSums(network[smembers, ])) / maxdeg
        }
      }

      if (statistics[i, 1] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = rep))
      }
    }

  }

  return(reptotal)
}

# alterpop -------------------------------------------------------------------
# initStat_DyNAMi_rate_alterpop <- function()

update_DyNAMi_rate_alterpop <- function(network,
                                        groups.network,
                                        sender, receiver, replace,
                                        n1, n2, statistics,
                                        weighted = TRUE, subType = "mean_normalized",
                                        joining = -1) {
  update_DyNAMi_rate_alterdeg(network,
                              groups.network,
                              sender, receiver, replace,
                              n1, n2, statistics,
                              weighted, subType,
                              joining)
}

# size -------------------------------------------------------------------
# initStat_DyNAMi_rate_size <- function()

update_DyNAMi_rate_size <- function(network,
                                    groups.network,
                                    sender, receiver, replace,
                                    n1, n2, statistics,
                                    weighted = FALSE, subType = "identity",
                                    joining = -1) {

  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {

    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (!isingroup) {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
        next
      }

      members <- which(groups.network[, owngroup] == 1)
      nmembers <- length(members)

      if (subType == "identity") {
        rep <- nmembers
      }
      if (subType == "squared") {
        rep <- nmembers^2
      }
      if (subType == "dummy") {
        rep <- nmembers > 2
      }

      if (statistics[i, 1] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = rep))
      }
    }

  }

  return(reptotal)
}

# Covariate effects -------------------------------------------------------


# ego -------------------------------------------------------------------
# initStat_DyNAMi_rate_ego <- function()

update_DyNAMi_rate_ego <- function(attribute,
                                  groups.network,
                                  sender, receiver, replace,
                                  n1, n2, statistics,
                                  subType = "identity",
                                  joining = 1,
                                  node = 0) {

  reptotal <- NULL
  meanatt <- mean(attribute)
  sdatt <- sd(attribute)

  # JOINING RATE
  if (joining == 1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (!isingroup) {
        if (subType == "identity") {
          rep <- attribute[i]
        }
        if (subType == "squared") {
          rep <- attribute[i]^2
        }
        if (subType == "centered") {
          rep <- attribute[i] - meanatt
        }
        if (subType == "normalized") {
          if (sdatt > 0) rep <- (attribute[i] - meanatt) / sdatt else rep <- 0
        }

        if (statistics[i, 1] != rep) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = rep))
        }
        next
      } else {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
      }
    }

  }

  # LEAVING RATE
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (isingroup) {
        if (subType == "identity") {
          rep <- attribute[i]
        }
        if (subType == "squared") {
          rep <- attribute[i]^2
        }
        if (subType == "centered") {
          rep <- attribute[i] - meanatt
        }
        if (subType == "normalized") {
          if (sdatt > 0) rep <- (attribute[i] - meanatt) / sdatt else rep <- 0
        }

        if (statistics[i, 1] != rep) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = rep))
        }
        next
      } else {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
      }
    }

  }

  return(reptotal)
}

# alter -------------------------------------------------------------------
# initStat_DyNAMi_rate_alter <- function()

update_DyNAMi_rate_alter <- function(attribute,
                                    groups.network,
                                    sender, receiver, replace,
                                    n1, n2, statistics,
                                    subType = "mean",
                                    joining = -1,
                                    node = 0) {

  reptotal <- NULL
  meanatt <- mean(attribute)
  sdatt <- sd(attribute)

  # LEAVING MODEL
  if (joining == -1) {

    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (!isingroup) {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
        next
      }

      members <- which(groups.network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

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

      if (statistics[i, 1] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = rep))
      }
    }

  }

  return(reptotal)

}

# same --------------------------------------------------------------------
# initStat_DyNAMi_rate_same <- function()

update_DyNAMi_rate_same <- function(attribute,
                                    groups.network,
                                    sender, receiver, replace,
                                    n1, n2, statistics,
                                    subType = "proportion",
                                    joining = -1,
                                    node = 0) {
  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {

    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (!isingroup) {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
        next
      }

      members <- which(groups.network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

      if (subType == "proportion") {
        rep <- sum(attribute[smembers] == attribute[i]) / snmembers
      }
      if (subType == "count") {
        rep <- sum(attribute[smembers] == attribute[i])
      }
      if (subType == "presence") {
        rep <- min(attribute[smembers] == attribute[i])
      }

      if (statistics[i, 1] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = rep))
      }
    }

  }

  return(reptotal)
}

# diff --------------------------------------------------------------------
# initStat_DyNAMi_rate_diff <- function()

update_DyNAMi_rate_diff <- function(attribute,
                                    groups.network,
                                    sender, receiver, replace,
                                    n1, n2, statistics,
                                    subType = "averaged_sum",
                                    joining = -1,
                                    node = 0) {
  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {

    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (!isingroup) {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
        next
      }

      members <- which(groups.network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

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

      if (statistics[i, 1] != rep) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = rep))
      }
    }

  }

  return(reptotal)
}


# sim ---------------------------------------------------------------------
# initStat_DyNAMi_rate_sim <- function()

update_DyNAMi_rate_sim <- function(attribute,
                                  groups.network,
                                  sender, receiver, replace,
                                  n1, n2, statistics,
                                  subType = "averaged_sum",
                                  node = 0) {
  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {

    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups.network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) isingroup <- length(which(groups.network[, owngroup] == 1)) > 1

      if (!isingroup) {
        if (statistics[i, 1] != 0) {
          reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = 0))
        }
        next
      }

      members <- which(groups.network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

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

      if (statistics[i, 1] != 1) {
        reptotal <- rbind(reptotal, cbind(node1 = i, node2 = seq.int(n2), replace = rep))
      }
    }

  }

  return(reptotal)
}
