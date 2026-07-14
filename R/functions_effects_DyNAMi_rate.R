# define methods ----------------------------------------------------------
# init cache data structure: vector or matrix
init_DyNAMi_rate <- function(
  effect_fun,
  network,
  attribute,
  groups_network,
  window,
  n1,
  n2
) {
  UseMethod("init_DyNAMi_rate", effect_fun)
}

# default -----------------------------------------------------------------
#' @export
init_DyNAMi_rate.default <- function(
  effect_fun,
  network = NULL,
  attribute = NULL,
  groups_network,
  window,
  n1,
  n2
) {
  init_DyNAMi_choice.default(
    effect_fun = effect_fun,
    network = network,
    attribute = attribute,
    groups_network = groups_network,
    window = window,
    n1 = n1,
    n2 = n2
  )
}

# Structural effects ------------------------------------------------------
# intercept -------------------------------------------------------------------
# initStat_DyNAMi_rate_intercept <- function()

update_DyNAMi_rate_intercept <- function(
  network,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  weighted = FALSE,
  joining = 1
) {
  reptotal <- NULL

  # JOINING RATE
  if (joining == 1) {
    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (statistics[i] != 1) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 1)
          )
        }
        next
      } else {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
      }
    }
  }

  # LEAVING RATE
  if (joining == -1) {
    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (isingroup) {
        if (statistics[i] != 1) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 1)
          )
        }
        next
      } else {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
      }
    }
  }

  return(reptotal)
}


# inertia ---------------------------------------------------------------------
# initStat_DyNAMi_rate_inertia <- function()

update_DyNAMi_rate_inertia <- function(
  network,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  weighted = TRUE,
  sub_type = "proportion",
  joining = -1
) {
  update_DyNAMi_rate_tie(
    network = network,
    groups_network = groups_network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    n1 = n1,
    n2 = n2,
    statistics = statistics,
    weighted = weighted,
    sub_type = sub_type,
    joining = joining
  )
}

# tie ---------------------------------------------------------------------
# initStat_DyNAMi_rate_tie <- function()

update_DyNAMi_rate_tie <- function(
  network,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  weighted = FALSE,
  sub_type = "proportion",
  joining = -1
) {
  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {
    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
        next
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

      if (sub_type == "count") {
        rep <- sum(network[i, smembers] > 0)
      }
      if (sub_type == "proportion") {
        rep <- sum(network[i, smembers] > 0) / snmembers
      }
      if (sub_type == "presence") {
        rep <- max(network[i, smembers] > 0)
      }
      if (sub_type == "min") {
        rep <- min(network[i, smembers])
      }
      if (sub_type == "mean") {
        rep <- mean(network[i, smembers])
      }
      if (sub_type == "max") {
        rep <- max(network[i, smembers])
      }

      if (statistics[i] != rep) {
        reptotal <- rbind(
          reptotal,
          cbind(node1 = i, replace = rep)
        )
      }
    }
  }

  return(reptotal)
}

# egodeg -------------------------------------------------------------------
# initStat_DyNAMi_rate_egodeg <- function()

update_DyNAMi_rate_egodeg <- function(
  network,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  weighted = TRUE,
  sub_type = "identity",
  joining = 1
) {
  reptotal <- NULL
  meandeg <- mean(rowSums(network))
  sddeg <- sd(rowSums(network))

  # JOINING RATE
  if (joining == 1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (sub_type == "identity") {
          rep <- sum(network[i, ])
        }
        if (sub_type == "centered") {
          rep <- sum(network[i, ]) - meandeg
        }
        if (sub_type == "normalized") {
          if (sddeg > 0) {
            rep <- (sum(network[i, ]) - meandeg) / sddeg
          } else {
            rep <- 0
          }
        }

        if (statistics[i] != rep) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = rep)
          )
        }
        next
      } else {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
      }
    }
  }

  # LEAVING RATE
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (isingroup) {
        if (sub_type == "identity") {
          rep <- sum(network[i, ])
        }
        if (sub_type == "centered") {
          rep <- sum(network[i, ]) - meandeg
        }
        if (sub_type == "normalized") {
          if (sddeg > 0) {
            rep <- (sum(network[i, ]) - meandeg) / sddeg
          } else {
            rep <- 0
          }
        }

        if (statistics[i] != rep) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = rep)
          )
        }
        next
      } else {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
      }
    }
  }

  return(reptotal)
}


# egopop -------------------------------------------------------------------
# initStat_DyNAMi_rate_egopop <- function()

update_DyNAMi_rate_egopop <- function(
  network,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  weighted = TRUE,
  sub_type = "normalized",
  joining = 1
) {
  update_DyNAMi_rate_egodeg(
    network = network,
    groups_network = groups_network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    n1 = n1,
    n2 = n2,
    statistics = statistics,
    weighted = weighted,
    sub_type = sub_type,
    joining = joining
  )
}

# alterdeg -------------------------------------------------------------------
# initStat_DyNAMi_rate_alterdeg <- function()

update_DyNAMi_rate_alterdeg <- function(
  network,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  weighted = TRUE,
  sub_type = "mean",
  joining = -1
) {
  reptotal <- NULL
  meandeg <- mean(rowSums(network))
  maxdeg <- max(rowSums(network))
  sddeg <- sd(rowSums(network))

  # LEAVING MODEL
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
        next
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

      if (snmembers == 1) {
        if (sub_type == "mean") {
          rep <- sum(network[smembers, ])
        }
        if (sub_type == "mean_centered") {
          rep <- sum(network[smembers, ]) - meandeg
        }
        if (sub_type == "mean_normalized") {
          if (sddeg > 0) {
            rep <- (sum(network[smembers, ]) - meandeg) / sddeg
          } else {
            rep <- 0
          }
        }
        if (sub_type == "min") {
          rep <- sum(network[smembers, ])
        }
        if (sub_type == "max") {
          rep <- sum(network[smembers, ])
        }
      } else {
        if (sub_type == "mean") {
          rep <- mean(rowSums(network[smembers, ]))
        }
        if (sub_type == "mean_centered") {
          rep <- mean(rowSums(network[smembers, ])) - meandeg
        }
        if (sub_type == "mean_normalized") {
          if (sddeg > 0) {
            rep <- (mean(rowSums(network[smembers, ])) - meandeg) / sddeg
          } else {
            rep <- 0
          }
        }
        if (sub_type == "min") {
          rep <- min(rowSums(network[smembers, ])) / maxdeg
        }
        if (sub_type == "max") {
          rep <- max(rowSums(network[smembers, ])) / maxdeg
        }
      }

      if (statistics[i] != rep) {
        reptotal <- rbind(
          reptotal,
          cbind(node1 = i, replace = rep)
        )
      }
    }
  }

  return(reptotal)
}

# alterpop -------------------------------------------------------------------
# initStat_DyNAMi_rate_alterpop <- function()

update_DyNAMi_rate_alterpop <- function(
  network,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  weighted = TRUE,
  sub_type = "mean_normalized",
  joining = -1
) {
  update_DyNAMi_rate_alterdeg(
    network = network,
    groups_network = groups_network,
    sender = sender,
    receiver = receiver,
    replace = replace,
    n1 = n1,
    n2 = n2,
    statistics = statistics,
    weighted = weighted,
    sub_type = sub_type,
    joining = joining
  )
}

# size -------------------------------------------------------------------
# initStat_DyNAMi_rate_size <- function()

update_DyNAMi_rate_size <- function(
  network,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  weighted = FALSE,
  sub_type = "identity",
  joining = -1
) {
  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
        next
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)

      if (sub_type == "identity") {
        rep <- nmembers
      }
      if (sub_type == "squared") {
        rep <- nmembers^2
      }
      if (sub_type == "dummy") {
        rep <- nmembers > 2
      }

      if (statistics[i] != rep) {
        reptotal <- rbind(
          reptotal,
          cbind(node1 = i, replace = rep)
        )
      }
    }
  }

  return(reptotal)
}


# dyad -------------------------------------------------------------------
# initStat_DyNAMi_rate_dyad <- function()

update_DyNAMi_rate_dyad <- function(
  network,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  weighted = FALSE,
  sub_type = "identity",
  joining = -1
) {
  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
        next
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)

      if (sub_type == "identity") {
        if (nmembers == 2) {
          rep <- 1
        } else {
          rep <- 0
        }
      }

      if (statistics[i] != rep) {
        reptotal <- rbind(
          reptotal,
          cbind(node1 = i, replace = rep)
        )
      }
    }
  }

  return(reptotal)
}


# Covariate effects -------------------------------------------------------

# ego -------------------------------------------------------------------
# initStat_DyNAMi_rate_ego <- function()

update_DyNAMi_rate_ego <- function(
  attribute,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  sub_type = "identity",
  joining = 1,
  node = 0
) {
  reptotal <- NULL
  meanatt <- mean(attribute)
  sdatt <- sd(attribute)

  # JOINING RATE
  if (joining == 1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (sub_type == "identity") {
          rep <- attribute[i]
        }
        if (sub_type == "squared") {
          rep <- attribute[i]^2
        }
        if (sub_type == "centered") {
          rep <- attribute[i] - meanatt
        }
        if (sub_type == "normalized") {
          if (sdatt > 0) {
            rep <- (attribute[i] - meanatt) / sdatt
          } else {
            rep <- 0
          }
        }

        if (statistics[i] != rep) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = rep)
          )
        }
        next
      } else {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
      }
    }
  }

  # LEAVING RATE
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (isingroup) {
        if (sub_type == "identity") {
          rep <- attribute[i]
        }
        if (sub_type == "squared") {
          rep <- attribute[i]^2
        }
        if (sub_type == "centered") {
          rep <- attribute[i] - meanatt
        }
        if (sub_type == "normalized") {
          if (sdatt > 0) {
            rep <- (attribute[i] - meanatt) / sdatt
          } else {
            rep <- 0
          }
        }

        if (statistics[i] != rep) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = rep)
          )
        }
        next
      } else {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
      }
    }
  }

  return(reptotal)
}

# alter -------------------------------------------------------------------
# initStat_DyNAMi_rate_alter <- function()

update_DyNAMi_rate_alter <- function(
  attribute,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  sub_type = "mean",
  joining = -1,
  node = 0
) {
  reptotal <- NULL
  meanatt <- mean(attribute)
  sdatt <- sd(attribute)

  # LEAVING MODEL
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
        next
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

      if (sub_type == "mean") {
        rep <- mean(attribute[smembers])
      }
      if (sub_type == "mean_squared") {
        rep <- mean(attribute[smembers])^2
      }
      if (sub_type == "mean_centered") {
        rep <- mean(attribute[smembers]) - meanatt
      }
      if (sub_type == "mean_centered_squared") {
        rep <- (mean(attribute[smembers]) - meanatt)^2
      }
      if (sub_type == "mean_normalized") {
        if (sdatt > 0) {
          rep <- (mean(attribute[smembers]) - meanatt) / sdatt
        } else {
          rep <- 0
        }
      }
      if (sub_type == "min") {
        rep <- min(attribute[smembers])
      }
      if (sub_type == "min_squared") {
        rep <- min(attribute[smembers])^2
      }
      if (sub_type == "min_centered") {
        rep <- min(attribute[smembers] - meanatt)
      }
      if (sub_type == "min_centered_squared") {
        rep <- min(attribute[smembers] - meanatt)^2
      }
      if (sub_type == "max") {
        rep <- max(attribute[smembers])
      }
      if (sub_type == "max_squared") {
        rep <- max(attribute[smembers])^2
      }
      if (sub_type == "max_centered") {
        rep <- max(attribute[smembers] - meanatt)
      }
      if (sub_type == "max_centered_squared") {
        rep <- max(attribute[smembers] - meanatt)^2
      }
      if (sub_type == "range") {
        rep <- max(attribute[smembers]) - min(attribute[smembers])
      }

      if (statistics[i] != rep) {
        reptotal <- rbind(
          reptotal,
          cbind(node1 = i, replace = rep)
        )
      }
    }
  }

  return(reptotal)
}

# same --------------------------------------------------------------------
# initStat_DyNAMi_rate_same <- function()

update_DyNAMi_rate_same <- function(
  attribute,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  sub_type = "proportion",
  joining = -1,
  node = 0
) {
  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
        next
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

      if (sub_type == "proportion") {
        rep <- sum(attribute[smembers] == attribute[i]) / snmembers
      }
      if (sub_type == "count") {
        rep <- sum(attribute[smembers] == attribute[i])
      }
      if (sub_type == "presence") {
        rep <- min(attribute[smembers] == attribute[i])
      }

      if (statistics[i] != rep) {
        reptotal <- rbind(
          reptotal,
          cbind(node1 = i, replace = rep)
        )
      }
    }
  }

  return(reptotal)
}

# diff --------------------------------------------------------------------
# initStat_DyNAMi_rate_diff <- function()

update_DyNAMi_rate_diff <- function(
  attribute,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  sub_type = "averaged_sum",
  joining = -1,
  node = 0
) {
  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
        next
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

      if (sub_type == "averaged_sum") {
        rep <- sum(abs(attribute[smembers] - attribute[i])) / snmembers
      }
      if (sub_type == "mean") {
        rep <- abs(mean(attribute[smembers]) - attribute[i])
      }
      if (sub_type == "mean_squared") {
        rep <- (mean(attribute[smembers]) - attribute[i])^2
      }
      if (sub_type == "min") {
        rep <- abs(min(attribute[smembers]) - attribute[i])
      }
      if (sub_type == "min_squared") {
        rep <- (min(attribute[smembers]) - attribute[i])^2
      }
      if (sub_type == "max") {
        rep <- abs(max(attribute[smembers]) - attribute[i])
      }
      if (sub_type == "max_squared") {
        rep <- (max(attribute[smembers]) - attribute[i])^2
      }

      if (statistics[i] != rep) {
        reptotal <- rbind(
          reptotal,
          cbind(node1 = i, replace = rep)
        )
      }
    }
  }

  return(reptotal)
}


# sim ---------------------------------------------------------------------
# initStat_DyNAMi_rate_sim <- function()

update_DyNAMi_rate_sim <- function(
  attribute,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  sub_type = "averaged_sum",
  joining = -1,
  node = 0
) {
  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
        next
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

      if (sub_type == "averaged_sum") {
        rep <- (-1) * sum(abs(attribute[smembers] - attribute[i])) / snmembers
      }
      if (sub_type == "mean") {
        rep <- (-1) * abs(mean(attribute[smembers]) - attribute[i])
      }
      if (sub_type == "min") {
        rep <- (-1) * abs(min(attribute[smembers]) - attribute[i])
      }
      if (sub_type == "max") {
        rep <- (-1) * abs(max(attribute[smembers]) - attribute[i])
      }

      if (statistics[i] != 1) {
        reptotal <- rbind(
          reptotal,
          cbind(node1 = i, replace = rep)
        )
      }
    }
  }

  return(reptotal)
}


# Interaction structural and Covariate effects ----------------------------

# sizeXdiff ---------------------------------------------------------------
# initStat_DyNAMi_rate_sizeXdiff <- function()

update_DyNAMi_rate_sizeXdiff <- function(
  attribute,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  sub_type = "averaged_sum",
  joining = -1,
  node = 0
) {
  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
        next
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

      if (sub_type == "averaged_sum") {
        rep <- nmembers *
          sum(abs(attribute[smembers] - attribute[i])) /
          snmembers
      }
      if (sub_type == "mean") {
        rep <- nmembers * abs(mean(attribute[smembers]) - attribute[i])
      }
      if (sub_type == "min") {
        rep <- nmembers * abs(min(attribute[smembers]) - attribute[i])
      }
      if (sub_type == "max") {
        rep <- nmembers * abs(max(attribute[smembers]) - attribute[i])
      }

      if (statistics[i] != rep) {
        reptotal <- rbind(
          reptotal,
          cbind(node1 = i, replace = rep)
        )
      }
    }
  }

  return(reptotal)
}


# dyadXdiff ---------------------------------------------------------------
# initStat_DyNAMi_rate_dyadXdiff <- function()

update_DyNAMi_rate_dyadXdiff <- function(
  attribute,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  sub_type = "averaged_sum",
  joining = -1,
  node = 0
) {
  reptotal <- NULL

  # LEAVING MODEL
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      if (!isingroup) {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
        next
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

      if (nmembers == 2) {
        m <- 1
      } else {
        m <- 0
      }

      if (sub_type == "averaged_sum") {
        rep <- m * sum(abs(attribute[smembers] - attribute[i])) / snmembers
      }
      if (sub_type == "mean") {
        rep <- m * abs(mean(attribute[smembers]) - attribute[i])
      }
      if (sub_type == "min") {
        rep <- m * abs(min(attribute[smembers]) - attribute[i])
      }
      if (sub_type == "max") {
        rep <- m * abs(max(attribute[smembers]) - attribute[i])
      }

      if (statistics[i] != rep) {
        reptotal <- rbind(
          reptotal,
          cbind(node1 = i, replace = rep)
        )
      }
    }
  }

  return(reptotal)
}

# sizeXego -------------------------------------------------------------------
# initStat_DyNAMi_rate_sizeXego <- function()

update_DyNAMi_rate_sizeXego <- function(
  attribute,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  sub_type = "identity",
  joining = -1,
  node = 0
) {
  reptotal <- NULL
  meanatt <- mean(attribute)
  sdatt <- sd(attribute)

  # LEAVING RATE
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

      if (isingroup) {
        if (sub_type == "identity") {
          rep <- nmembers * attribute[i]
        }
        if (sub_type == "squared") {
          rep <- nmembers * attribute[i]^2
        }
        if (sub_type == "centered") {
          rep <- nmembers * (attribute[i] - meanatt)
        }
        if (sub_type == "normalized") {
          if (sdatt > 0) {
            rep <- nmembers * (attribute[i] - meanatt) / sdatt
          } else {
            rep <- 0
          }
        }

        if (statistics[i] != rep) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = rep)
          )
        }
        next
      } else {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
      }
    }
  }

  return(reptotal)
}


# dyadXego -------------------------------------------------------------------
# initStat_DyNAMi_rate_dyadXego <- function()

update_DyNAMi_rate_dyadXego <- function(
  attribute,
  groups_network,
  sender,
  receiver,
  replace,
  n1,
  n2,
  statistics,
  sub_type = "identity",
  joining = -1,
  node = 0
) {
  reptotal <- NULL
  meanatt <- mean(attribute)
  sdatt <- sd(attribute)

  # LEAVING RATE
  if (joining == -1) {
    reptotal <- NULL

    for (i in seq.int(n1)) {
      owngroup <- which(groups_network[i, ] == 1)
      isingroup <- FALSE
      if (length(owngroup) == 1) {
        isingroup <- length(which(groups_network[, owngroup] == 1)) > 1
      }

      members <- which(groups_network[, owngroup] == 1)
      nmembers <- length(members)
      smembers <- members[members != i]
      snmembers <- length(smembers)

      if (nmembers == 2) {
        m <- 1
      } else {
        m <- 0
      }

      if (isingroup) {
        if (sub_type == "identity") {
          rep <- m * attribute[i]
        }
        if (sub_type == "squared") {
          rep <- m * attribute[i]^2
        }
        if (sub_type == "centered") {
          rep <- m * (attribute[i] - meanatt)
        }
        if (sub_type == "normalized") {
          if (sdatt > 0) {
            rep <- m * (attribute[i] - meanatt) / sdatt
          } else {
            rep <- 0
          }
        }

        if (statistics[i] != rep) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = rep)
          )
        }
        next
      } else {
        if (statistics[i] != 0) {
          reptotal <- rbind(
            reptotal,
            cbind(node1 = i, replace = 0)
          )
        }
      }
    }
  }

  return(reptotal)
}
