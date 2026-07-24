####################### DATA PROCESSING ########################################

#' To define the second mode of a DyNAM-i model
#'
#' This function create all objects necessary to the estimation of a DyNAM-i
#' model `model = "DyNAMi"` from dyadic interaction records and an actor set.
#' It first creates a nodeset for the second mode of the interaction network
#' that will be modeled, i.e. the interaction groups set,
#' and an event list that indicates when groups are present or not through time.
#' It then creates a list of interaction events, between actors and groups,
#' in which an actor either joins or leaves a group. It is decomposed in
#' an list of dependent events (that should be modeled) and a list of
#' exogenous events (that should not be modeled).
#' For example when an actor leaves a group and joins her own singleton group,
#' only the leaving event is modeled but not the joining one, and vice versa
#' when an actor belonging to a singleton group joins another group.
#'
#' It is important to notice that sometimes some random decisions have to
#' be made regarding who joined or left a group, for example when two actors
#' start interacting but we do not know who initiated the interaction.
#' Tot est for the robustness of such a procedure, one can use different
#' randomization seeds and run the model several times.
#'
#' @param records an object of class `data.frame` that is a list of rows of type
#' node A, nodeB, Start, End, where nodeA and nodeB indicate the actors
#' involved in a dyadic interaction, and Start and End indicating the starting
#' and ending time of their interaction.
#' @param actors a object of class `nodes.goldfish` that defines the actors
#' interacting (labels in records and actors should be identical).
#' @param seed_randomization an `integer` used whenever there should
#' be some random choice to be made.
#' @param progress logical weather detailed information of intermediate steps
#' should be printed in the console.
#' @export
#' @return a model-ready `goldfish` data object (a stamped multipartite
#'   `stocnet`). Its `nodes` table holds the actors (`mode = "actor"`, keeping
#'   their attributes) and the derived interaction groups (`mode = "group"`); a
#'   focal two-mode `interactions` layer carries the actors x groups join/leave
#'   history (dependent joins stamped `flavor = "join"`, dependent leaves
#'   `flavor = "leave"`, exogenous group-composition rows `flavor = NA`, and the
#'   construction's total event order in the reserved `order` column); and a
#'   one-mode `past` layer carries the actors x actors past-interaction updates.
#'   Pass the object directly to `estimate_dynami()` or `make_specification()`.
#'
#'   This replaces the previous five-component list return
#'   (`interaction.updates`, `groups`, `dependent.events`, `exogenous.events`,
#'   `opportunities`): those pieces are now components of the returned object,
#'   and the `opportunities` list is retired in favor of the choice model's
#'   auto-derived group-availability support constraint.
make_groups_interaction <- function(
  records,
  actors,
  seed_randomization,
  progress = getOption("progress")
) {
  stopifnot(
    inherits(records, "data.frame"),
    inherits(actors, "data.frame"),
    methods::is(seed_randomization, "numeric"),
    is.null(progress) || inherits(progress, "logical")
  )

  if (is.null(progress)) {
    progress <- FALSE
  }

  # PATCH Marion: change actors labels to characters
  actors$label <- as.character(actors$label)

  # inititialization
  ngroups <- 0
  set.seed(seed_randomization)

  nrecords <- nrow(records)
  nactors <- nrow(actors)

  # temporary variables to store the current state of groups
  time <- 0
  is <- 1
  ie <- 1
  i <- 1
  timestarts <- records$Start
  timeends <- records$End
  orderevents <- rep(0, 2 * nrecords) # ids of original
  timesevents <- rep(0, 2 * nrecords) # times events
  typesevents <- rep(0, 2 * nrecords) # 1 for start 0 for end

  # STEP 1: order all events (start and end)
  # if some events occur at the same time, we randomize the order
  # For now, we follow the exact time of the date,
  #  but if events were close enough
  # (<delta t), we could also randomize when we know that we have
  # unperfect data
  while (i <= (2 * nrecords)) {
    if (length(timestarts[timestarts > time]) > 0) {
      nextstart <- min(timestarts[timestarts > time])
    }
    nextend <- min(timeends[timeends > time])

    # check which is the next event
    if (nextend == nextstart) {
      nextind <- c(which(timestarts == nextstart), which(timeends == nextend))
      nexttype <- c(
        rep(1, length(which(timestarts == nextstart))),
        rep(0, length(which(timeends == nextend)))
      )
      l <- length(nextind)
      nexti <- sample(seq.int(l))
      nextind <- nextind[nexti]
      nexttype <- nexttype[nexti]
      for (j in 1:l) {
        orderevents[i] <- nextind[j]
        timesevents[i] <- nextend
        typesevents[i] <- nexttype[j]
        i <- i + 1
      }
      time <- nextend
    } else if (nextend < nextstart) {
      nextendind <- which(timeends == nextend)
      if (length(nextendind) > 1) {
        nextendind <- sample(nextendind)
      }
      for (j in seq.int(length(nextendind))) {
        orderevents[i] <- nextendind[j]
        timesevents[i] <- nextend
        i <- i + 1
      }
      time <- nextend
    } else {
      nextstartind <- which(timestarts == nextstart)
      if (length(nextstartind) > 1) {
        nextstartind <- sample(nextstartind)
      }
      for (j in seq.int(length(nextstartind))) {
        orderevents[i] <- nextstartind[j]
        timesevents[i] <- nextstart
        typesevents[i] <- 1
        i <- i + 1
      }
      time <- nextstart
    }
  }

  # STEP 2: go through all interactions recorded, and assign temporary groups
  #  to actors through all the times of events
  uniqueevents <- unique(timesevents) # aggregate all events at the same time
  nevents <- length(uniqueevents)
  groupassignment <- matrix(0, nactors, nevents) # temporary assignment
  currentnet <- matrix(0, nactors, nactors)

  past_senderevents <- numeric()
  past_receiverevents <- numeric()
  past_timeevents <- numeric()
  past_incrementevents <- numeric()

  for (i in 1:nevents) {
    time <- uniqueevents[i]

    # first build the current network
    tempnet <- matrix(0, nactors, nactors)

    # check all actors
    for (a1 in seq.int(nactors - 1)) {
      # check all others
      for (a2 in seq.int(a1 + 1, nactors)) {
        # interaction records?
        nAa1 <- which(records$NodeA == toString(a1))
        nBa1 <- which(records$NodeB == toString(a1))
        nAa2 <- which(records$NodeA == toString(a2))
        nBa2 <- which(records$NodeB == toString(a2))
        inda1a2 <- c(intersect(nAa1, nBa2), intersect(nAa2, nBa1))

        # is it happening at that time
        areinteracting <- 0
        if (length(inda1a2) > 0) {
          for (j in seq.int(length(inda1a2))) {
            if (
              records$Start[inda1a2[j]] <= time &&
                records$End[inda1a2[j]] > time
            ) {
              areinteracting <- 1
            }
          }
        }
        tempnet[a1, a2] <- areinteracting
        tempnet[a2, a1] <- areinteracting
      }
    }

    # second: assign temporary groups
    g <- 1

    for (a1 in seq.int(nactors)) {
      # is the actor is interacting?
      isinteracting <- sum(tempnet[a1, ]) > 0

      # are the others previously assigned?
      if (isinteracting && min(which(tempnet[a1, ] == 1)) < a1) {
        groupassignment[a1, i] <-
          groupassignment[min(which(tempnet[a1, ] == 1)), i]
      } else {
        # if not, assign a new group
        groupassignment[a1, i] <- g
        g <- g + 1
      }
    }

    # calculate updates in the updates of the previous interaction network
    for (a1 in seq.int(nactors - 1)) {
      for (a2 in seq.int(a1 + 1, nactors)) {
        if (tempnet[a1, a2] == 1 && currentnet[a1, a2] == 0) {
          past_senderevents <- c(past_senderevents, a1)
          past_receiverevents <- c(past_receiverevents, a2)
          past_timeevents <- c(past_timeevents, time)
          past_incrementevents <- c(past_incrementevents, 1)
        }
      }
    }
  }

  # STEP 3: define the joining and leaving events from the start
  #  and end events and group assignments

  # # groups: data.frame(label, present)
  # # composition.changes:
  # # data.frame(time = grouptimeevents,
  # #            node = groupevents,
  # #            replace = groupreplaceevents) +/-1 if creation/deletion
  # grouptimeevents <- numeric()
  # groupnodeevents <- numeric()
  # groupreplaceevents <- numeric()

  # opportunity sets: a list containing which groups are available
  #  at each decision time
  # opportunities$time: same times as the joining events
  #  (the ones used in the choice model)
  # opportunities$groups: vector of available groups
  opportunities <- list()

  # depending events: joining and leaving events to be modeled
  # replace = +/-1 if joining/leaving
  # deporder: vector indicating the order of this event
  #  in the whole scheme of events
  # (to be put as an attribute of the events)
  deptimeevents <- numeric()
  depsenderevents <- numeric()
  depreceiverevents <- numeric()
  depreplaceevents <- numeric()
  deporder <- numeric()

  # exogenous events: joining and leaving events that are "structural"
  # (when an isolate "leaves" its group, or when an actor "joins" an isolate)
  # replace = +/-1 if joining/leaving
  # exoorder: vector indicating the order of this event
  # in the whole scheme of events
  # (to be put as an attribute of the events)
  exotimeevents <- numeric()
  exosenderevents <- numeric()
  exoreceiverevents <- numeric()
  exoreplaceevents <- numeric()
  exoorder <- numeric()

  # interaction updates events: updates of the past interaction network
  # a weight 1 is added between 2 actors when one joins
  #  the other in an interaction
  # pastorder: vector indicating the order of this event
  #  in the whole scheme of events
  # (to be put as an attribute of the events)
  pastsenderevents <- numeric()
  pastreceiverevents <- numeric()
  pasttimeevents <- numeric()
  pastreplaceevents <- numeric()
  pastorder <- numeric()

  currentgroups <- 1:nactors
  allactors <- 1:nactors

  # order of the last event that was added
  cptorder <- 0
  cptopp <- 0

  for (i in 1:nevents) {
    time <- uniqueevents[i]

    # debug
    # cat("Visiting event: ", i, " at time: ", time, "\n")

    # store temporary events
    deptimeevents_temp <- numeric()
    depsenderevents_temp <- numeric()
    depreceiverevents_temp <- numeric()
    depreplaceevents_temp <- numeric()
    deporder_temp <- numeric()

    exotimeevents_temp <- numeric()
    exosenderevents_temp <- numeric()
    exoreceiverevents_temp <- numeric()
    exoreplaceevents_temp <- numeric()
    exoorder_temp <- numeric()

    pastsenderevents_temp <- numeric()
    pastreceiverevents_temp <- numeric()
    pasttimeevents_temp <- numeric()
    pastreplaceevents_temp <- numeric()
    pastorder_temp <- numeric()

    # INTERACTING ACTORS: for each group, update group events
    #  (groups are taken at random)
    numgroups <- 0
    groups <- numeric()
    singletons <- numeric()
    for (g in seq.int(max(groupassignment[, i]))) {
      if (length(which(groupassignment[, i] == g)) > 1) {
        numgroups <- numgroups + 1
        groups <- c(groups, g)
      }
    }
    for (g in 1:max(currentgroups)) {
      if (length(which(currentgroups == g)) == 1) {
        singletons <- c(singletons, g)
      }
    }
    if (numgroups > 1) {
      groups <- sample(groups)
    }

    # in case of merge and splits, we randomly pick the group to keep
    # so we need to keep track of kept groups, and the ones
    #  that will need to be removed
    takengroups <- numeric()
    toberemovedgroups <- unique(currentgroups[currentgroups > 0])

    if (numgroups > 0) {
      for (g in 1:numgroups) {
        groupactors <- which(groupassignment[, i] == groups[g])
        previousassignments <- currentgroups[groupactors]
        previousgroups <- unique(previousassignments)
        numpreviousgroups <- length(previousgroups)
        if (numpreviousgroups > 1) {
          previousgroups <- sample(previousgroups)
        }

        # if some of them were interacting before, in one or several groups
        if (numpreviousgroups >= 1) {
          # we randomly choose the group to keep, potentially
          #  create one more in the split case
          newkeptg <- FALSE
          if (length(intersect(previousgroups, takengroups)) > 0) {
            topickfrom <- previousgroups[
              !previousgroups %in% intersect(previousgroups, takengroups)
            ]
          } else {
            topickfrom <- previousgroups
          }

          if (length(topickfrom) > 0) {
            sizes <- rep(0, length(topickfrom))
            for (g2 in seq.int(length(topickfrom))) {
              sizes[g2] <- length(which(currentgroups == topickfrom[g2]))
            }
            if (max(sizes) > 1) {
              topickfrom <- topickfrom[sizes > 1]
            }
          }

          if (length(topickfrom) > 1) {
            keptg <- sample(topickfrom, 1)
          } else if (length(topickfrom) == 1) {
            keptg <- topickfrom
          } else {
            newkeptg <- TRUE
            keptg <- min(allactors[-unique(currentgroups)])
            # grouptimeevents <- c(grouptimeevents,time-1)
            # groupnodeevents <- c(groupnodeevents,keptg)
            # groupreplaceevents <- c(groupreplaceevents,TRUE)

            # debug
            # cat(paste("group created: ",keptg, "\n"))
          }

          takengroups <- c(takengroups, keptg)
          toberemovedgroups <- toberemovedgroups[toberemovedgroups != keptg]

          if (newkeptg) {
            for (g2 in seq_len(numpreviousgroups)) {
              # we check whether there are some other actors
              #  in the previous group
              previousgroup <- previousgroups[g2]
              previousgroupactors <- groupactors[
                which(currentgroups[groupactors] == previousgroup)
              ]

              if (length(previousgroupactors) > 0) {
                for (a2 in seq_along(previousgroupactors)) {
                  # dependent leaving events for active actors in other groups
                  # + exogenous joining events to intermediary singletons
                  if (!previousgroup %in% singletons) {
                    deptimeevents_temp <- c(deptimeevents_temp, time)
                    depsenderevents_temp <- c(
                      depsenderevents_temp,
                      previousgroupactors[a2]
                    )
                    depreceiverevents_temp <- c(
                      depreceiverevents_temp,
                      previousgroup
                    )
                    depreplaceevents_temp <- c(depreplaceevents_temp, -1)

                    cptorder <- cptorder + 1
                    deporder_temp <- c(deporder_temp, cptorder)

                    # debug
                    # cat(paste("leaving event: ",
                    #    previousgroupactors[a2],"to", previousgroup, "\n"))

                    # We create a fake intermediary singleton!
                    newg <- min(allactors[!allactors %in% currentgroups])
                    exotimeevents_temp <- c(exotimeevents_temp, time)
                    exosenderevents_temp <- c(
                      exosenderevents_temp,
                      previousgroupactors[a2]
                    )
                    exoreceiverevents_temp <- c(exoreceiverevents_temp, newg)
                    exoreplaceevents_temp <- c(exoreplaceevents_temp, 1)

                    cptorder <- cptorder + 1
                    exoorder_temp <- c(exoorder_temp, cptorder)

                    # update current groups
                    currentgroups[previousgroupactors[a2]] <- newg

                    # debug
                    # cat(paste("(exo) joining event: ",
                    #  previousgroupactors[a2],"to", newg, "\n"))
                  }

                  # dependent joining events for everyone
                  deptimeevents_temp <- c(deptimeevents_temp, time)
                  depsenderevents_temp <- c(
                    depsenderevents_temp,
                    previousgroupactors[a2]
                  )
                  depreceiverevents_temp <- c(depreceiverevents_temp, keptg)
                  depreplaceevents_temp <- c(depreplaceevents_temp, 1)

                  # store the information on which groups were available
                  # at the time of the joining
                  cptopp <- cptopp + 1
                  opportunities[[cptopp]] <- unique(currentgroups)
                  # debug
                  # cat(paste("opportunities: ",
                  #  t(unique(currentgroups)), "\n"))

                  cptorder <- cptorder + 1
                  deporder_temp <- c(deporder_temp, cptorder)

                  # debug
                  # cat(paste("joining event: ",
                  #  previousgroupactors[a2]," to ", keptg, "\n"))

                  # update past interaction network
                  othersingroups <- which(currentgroups == keptg)
                  nothers <- length(othersingroups)
                  pasttimeevents_temp <- c(
                    pasttimeevents_temp,
                    rep(time, nothers)
                  )
                  pastsenderevents_temp <- c(
                    pastsenderevents_temp,
                    rep(previousgroupactors[a2], nothers)
                  )
                  pastreceiverevents_temp <- c(
                    pastreceiverevents_temp,
                    othersingroups
                  )
                  pastreplaceevents_temp <- c(
                    pastreplaceevents_temp,
                    rep(1, nothers)
                  )

                  cptorder <- cptorder + 1
                  pastorder_temp <- c(
                    pastorder_temp,
                    cptorder:(cptorder + nothers - 1)
                  )
                  cptorder <- cptorder + nothers - 1

                  # update current groups
                  currentgroups[previousgroupactors[a2]] <- keptg

                  # exogenous leaving events for everyone
                  # if the actor was in a real gorup,
                  #  it leaves the fake intermediary singleton
                  if (!previousgroup %in% singletons) {
                    exotimeevents_temp <- c(exotimeevents_temp, time)
                    exosenderevents_temp <- c(
                      exosenderevents_temp,
                      previousgroupactors[a2]
                    )
                    exoreceiverevents_temp <- c(exoreceiverevents_temp, newg)
                    exoreplaceevents_temp <- c(exoreplaceevents_temp, -1)

                    cptorder <- cptorder + 1
                    exoorder_temp <- c(exoorder_temp, cptorder)

                    # debug
                    # cat(paste("(exo) leaving event: ",
                    #  previousgroupactors[a2],"to", newg, "\n"))
                  } else {
                    # if it was a singleton, it just leaves the singleton
                    exotimeevents_temp <- c(exotimeevents_temp, time)
                    exosenderevents_temp <- c(
                      exosenderevents_temp,
                      previousgroupactors[a2]
                    )
                    exoreceiverevents_temp <- c(
                      exoreceiverevents_temp,
                      previousgroup
                    )
                    exoreplaceevents_temp <- c(exoreplaceevents_temp, -1)

                    cptorder <- cptorder + 1
                    exoorder_temp <- c(exoorder_temp, cptorder)

                    # debug
                    # cat(paste("(exo) leaving event: ",
                    #  previousgroupactors[a2],"to", previousgroup, "\n"))
                  }
                }
              }
            }
          }

          if (numpreviousgroups > 1) {
            numpreviousgroups <- numpreviousgroups - 1
            previousgroups <- previousgroups[which(previousgroups != keptg)]

            for (g2 in seq.int(numpreviousgroups)) {
              # we check whether there are some other actors
              #  in the previous group
              previousgroup <- previousgroups[g2]
              previousgroupactors <- groupactors[
                which(currentgroups[groupactors] == previousgroup)
              ]

              if (length(previousgroupactors) > 0) {
                for (a2 in seq.int(length(previousgroupactors))) {
                  # dependent leaving events for active actors in other groups
                  # + exogenous joining events to intermediary singletons
                  if (!previousgroup %in% singletons) {
                    deptimeevents_temp <- c(deptimeevents_temp, time)
                    depsenderevents_temp <- c(
                      depsenderevents_temp,
                      previousgroupactors[a2]
                    )
                    depreceiverevents_temp <- c(
                      depreceiverevents_temp,
                      previousgroup
                    )
                    depreplaceevents_temp <- c(depreplaceevents_temp, -1)

                    cptorder <- cptorder + 1
                    deporder_temp <- c(deporder_temp, cptorder)

                    # debug
                    # cat(
                    #   paste("leaving event: ",
                    #         previousgroupactors[a2],
                    #         "to", previousgroup, "\n")
                    # )

                    # We create a fake intermediary singleton!
                    newg <- min(allactors[!allactors %in% currentgroups])
                    exotimeevents_temp <- c(exotimeevents_temp, time)
                    exosenderevents_temp <- c(
                      exosenderevents_temp,
                      previousgroupactors[a2]
                    )
                    exoreceiverevents_temp <- c(exoreceiverevents_temp, newg)
                    exoreplaceevents_temp <- c(exoreplaceevents_temp, 1)

                    cptorder <- cptorder + 1
                    exoorder_temp <- c(exoorder_temp, cptorder)

                    # update current groups
                    currentgroups[previousgroupactors[a2]] <- newg

                    # debug
                    # cat(
                    #   paste("(exo) joining event: ",
                    #         previousgroupactors[a2],
                    #         "to", newg, "\n")
                    # )
                  }

                  # dependent joining events for everyone
                  deptimeevents_temp <- c(deptimeevents_temp, time)
                  depsenderevents_temp <- c(
                    depsenderevents_temp,
                    previousgroupactors[a2]
                  )
                  depreceiverevents_temp <- c(depreceiverevents_temp, keptg)
                  depreplaceevents_temp <- c(depreplaceevents_temp, 1)

                  # store the information on which groups were available at
                  # the time of the joining
                  cptopp <- cptopp + 1
                  opportunities[[cptopp]] <- unique(currentgroups)
                  # debug
                  # cat(
                  #   paste("opportunities: ", t(unique(currentgroups)), "\n")
                  # )

                  cptorder <- cptorder + 1
                  deporder_temp <- c(deporder_temp, cptorder)

                  # debug
                  # cat(
                  #   paste("joining event: ", previousgroupactors[a2],
                  #         " to ", keptg, "\n")
                  # )

                  # update past interaction network
                  othersingroups <- which(currentgroups == keptg)
                  nothers <- length(othersingroups)
                  pasttimeevents_temp <- c(
                    pasttimeevents_temp,
                    rep(time, nothers)
                  )
                  pastsenderevents_temp <- c(
                    pastsenderevents_temp,
                    rep(previousgroupactors[a2], nothers)
                  )
                  pastreceiverevents_temp <- c(
                    pastreceiverevents_temp,
                    othersingroups
                  )
                  pastreplaceevents_temp <- c(
                    pastreplaceevents_temp,
                    rep(1, nothers)
                  )

                  cptorder <- cptorder + 1
                  pastorder_temp <- c(
                    pastorder_temp,
                    cptorder:(cptorder + nothers - 1)
                  )
                  cptorder <- cptorder + nothers - 1

                  # update current groups
                  currentgroups[previousgroupactors[a2]] <- keptg

                  # exogenous leaving events for everyone
                  # if the actor was in a real group,
                  # it leaves the fake intermediary singleton
                  if (!previousgroup %in% singletons) {
                    exotimeevents_temp <- c(exotimeevents_temp, time)
                    exosenderevents_temp <- c(
                      exosenderevents_temp,
                      previousgroupactors[a2]
                    )
                    exoreceiverevents_temp <- c(exoreceiverevents_temp, newg)
                    exoreplaceevents_temp <- c(exoreplaceevents_temp, -1)

                    cptorder <- cptorder + 1
                    exoorder_temp <- c(exoorder_temp, cptorder)

                    # debug
                    # cat(
                    #   paste(
                    #     "(exo) leaving event: ",
                    #     previousgroupactors[a2],
                    #     "to", newg, "\n"
                    #   )
                    # )
                  } else {
                    # if it was a singleton, it just leaves the singleton
                    exotimeevents_temp <- c(exotimeevents_temp, time)
                    exosenderevents_temp <- c(
                      exosenderevents_temp,
                      previousgroupactors[a2]
                    )
                    exoreceiverevents_temp <- c(
                      exoreceiverevents_temp,
                      previousgroup
                    )
                    exoreplaceevents_temp <- c(exoreplaceevents_temp, -1)

                    cptorder <- cptorder + 1
                    exoorder_temp <- c(exoorder_temp, cptorder)

                    # debug
                    # cat(
                    #   paste(
                    #     "(exo) leaving event: ",
                    #     previousgroupactors[a2],
                    #     "to", previousgroup, "\n"
                    #   )
                    # )
                  }
                }
              }
            }
          }
        }
      }
    }

    # NOT INTERACTING ACTORS: we check previous time point
    numinactives <- 0
    inactivegroups <- numeric()
    inactiveactors <- numeric()
    for (g in seq.int(max(groupassignment[, i]))) {
      if (length(which(groupassignment[, i] == g)) == 1) {
        numinactives <- numinactives + 1
        inactivegroups <- c(inactivegroups, g)
        inactiveactors <- c(inactiveactors, which(groupassignment[, i] == g))
      }
    }

    allpreviousgroups <- unique(currentgroups[inactiveactors])
    previousgroups <- numeric()
    numgroups <- 0
    if (length(allpreviousgroups) > 0) {
      for (g in seq.int(length(allpreviousgroups))) {
        if (sum(currentgroups == allpreviousgroups[g]) > 1) {
          previousgroups <- c(previousgroups, allpreviousgroups[g])
        }
        if (sum(currentgroups == allpreviousgroups[g]) == 1) {
          toberemovedgroups <- toberemovedgroups[
            toberemovedgroups != allpreviousgroups[g]
          ]
        }
      }
      numgroups <- length(previousgroups)
      if (numgroups > 1) {
        numgroups <- length(previousgroups)
      }
    }

    # if there were previous groups, go through all of them in a random order
    if (numgroups > 0) {
      for (g in seq.int(numgroups)) {
        # what about other actors in the group
        previousgroup <- previousgroups[g]
        groupactors <- which(currentgroups == previousgroup)
        actorsactivity <- rep(0, length(groupactors))
        for (a in seq.int(length(groupactors))) {
          actorsactivity[a] <- length(
            which(groupassignment[, i] == groupassignment[groupactors[a], i])
          ) >
            1
        }

        # GROUP DELETION: all the actors left
        if (max(actorsactivity) == 0) {
          kepta <- sample(groupactors, 1)
          groupactors <- groupactors[groupactors != kepta]
          toberemovedgroups <- toberemovedgroups[
            toberemovedgroups != currentgroups[kepta]
          ]

          # leaving events for the others
          if (length(groupactors) > 0) {
            for (a2 in seq.int(length(groupactors))) {
              deptimeevents_temp <- c(deptimeevents_temp, time)
              depsenderevents_temp <- c(depsenderevents_temp, groupactors[a2])
              depreceiverevents_temp <- c(depreceiverevents_temp, previousgroup)
              depreplaceevents_temp <- c(depreplaceevents_temp, -1)

              cptorder <- cptorder + 1
              deporder_temp <- c(deporder_temp, cptorder)

              # debug
              # cat(
              #   paste("leaving event: ", groupactors[a2],
              #         " to ", previousgroup, "\n")
              # )

              # update current groups
              currentg <- min(allactors[-unique(currentgroups)])
              currentgroups[groupactors[a2]] <- currentg

              # grouptimeevents <- c(grouptimeevents,time-1)
              # groupnodeevents <- c(groupnodeevents,currentg)
              # groupreplaceevents <- c(groupreplaceevents,TRUE)
              # cat(paste("group created: ", currentg, "\n"))

              exotimeevents_temp <- c(exotimeevents_temp, time)
              exosenderevents_temp <- c(exosenderevents_temp, groupactors[a2])
              exoreceiverevents_temp <- c(exoreceiverevents_temp, currentg)
              exoreplaceevents_temp <- c(exoreplaceevents_temp, 1)

              cptorder <- cptorder + 1
              exoorder_temp <- c(exoorder_temp, cptorder)

              # debug
              # cat(
              #   paste("(exo) joining event: ", groupactors[a2],
              #         " to ", currentg, "\n")
              # )
            }
          }
        } else {
          # LEAVING EVENT: some other actors remain active

          # we only take the ones who are no longer active
          groupactors <- groupactors[actorsactivity == 0]

          # leaving events
          for (a2 in seq.int(length(groupactors))) {
            deptimeevents_temp <- c(deptimeevents_temp, time)
            depsenderevents_temp <- c(depsenderevents_temp, groupactors[a2])
            depreceiverevents_temp <- c(depreceiverevents_temp, previousgroup)
            depreplaceevents_temp <- c(depreplaceevents_temp, -1)

            cptorder <- cptorder + 1
            deporder_temp <- c(deporder_temp, cptorder)

            # debug
            # cat(
            #   paste("leaving event: ",
            #         groupactors[a2],
            #         " to ", previousgroup, "\n")
            # )

            # update current groups
            currentg <- min(allactors[-unique(currentgroups)])
            currentgroups[groupactors[a2]] <- currentg

            # grouptimeevents <- c(grouptimeevents,time-1)
            # groupnodeevents <- c(groupnodeevents,currentg)
            # groupreplaceevents <- c(groupreplaceevents,TRUE)
            # cat(paste("group created: ", currentg, "\n"))

            exotimeevents_temp <- c(exotimeevents_temp, time)
            exosenderevents_temp <- c(exosenderevents_temp, groupactors[a2])
            exoreceiverevents_temp <- c(exoreceiverevents_temp, currentg)
            exoreplaceevents_temp <- c(exoreplaceevents_temp, 1)

            cptorder <- cptorder + 1
            exoorder_temp <- c(exoorder_temp, cptorder)

            # debug
            # cat(
            #   paste("(exo) joining event: ",
            #         groupactors[a2],
            #         " to ",
            #         currentg, "\n")
            # )
          }
        }
      }
    }

    # # EMPTY GROUPS to be removed
    # if(length(toberemovedgroups) > 0) {
    #   for(g in 1:length(toberemovedgroups)) {
    #     toberemovedg <- toberemovedgroups[g]
    #
    #     if(!toberemovedg %in% currentgroups) {
    #       grouptimeevents <- c(grouptimeevents,time+1)
    #       groupnodeevents <- c(groupnodeevents,toberemovedg)
    #       groupreplaceevents <- c(groupreplaceevents,FALSE)
    #       cat(paste("group deleted: ", toberemovedg, "\n"))
    #     }
    #   }
    # }

    # store in all events
    deptimeevents <- c(deptimeevents, deptimeevents_temp)
    depsenderevents <- c(depsenderevents, depsenderevents_temp)
    depreceiverevents <- c(depreceiverevents, depreceiverevents_temp)
    depreplaceevents <- c(depreplaceevents, depreplaceevents_temp)
    deporder <- c(deporder, deporder_temp)

    exotimeevents <- c(exotimeevents, exotimeevents_temp)
    exosenderevents <- c(exosenderevents, exosenderevents_temp)
    exoreceiverevents <- c(exoreceiverevents, exoreceiverevents_temp)
    exoreplaceevents <- c(exoreplaceevents, exoreplaceevents_temp)
    exoorder <- c(exoorder, exoorder_temp)

    pasttimeevents <- c(pasttimeevents, pasttimeevents_temp)
    pastsenderevents <- c(pastsenderevents, pastsenderevents_temp)
    pastreceiverevents <- c(pastreceiverevents, pastreceiverevents_temp)
    pastreplaceevents <- c(pastreplaceevents, pastreplaceevents_temp)
    pastorder <- c(pastorder, pastorder_temp)
  }

  # RESULTS
  groups <- data.frame(
    label = paste0("Group", 1:nactors),
    present = TRUE
  )

  # composition.changes <- data.frame(time = grouptimeevents,
  #                                   node = groupnodeevents,
  #                                   replace = groupreplaceevents)

  interaction.updates <- data.frame(
    time = pasttimeevents,
    sender = pastsenderevents,
    receiver = pastreceiverevents,
    increment = pastreplaceevents
  )
  attr(interaction.updates, "order") <- pastorder

  exogenous.events <- data.frame(
    time = exotimeevents,
    sender = exosenderevents,
    receiver = exoreceiverevents,
    increment = exoreplaceevents
  )
  attr(exogenous.events, "order") <- exoorder

  dependent.events <- data.frame(
    time = deptimeevents,
    sender = depsenderevents,
    receiver = depreceiverevents,
    increment = depreplaceevents
  )
  attr(dependent.events, "order") <- deporder

  attr(interaction.updates, "class") <- c(
    attr(interaction.updates, "class"),
    "interaction.network.updates"
  )
  attr(dependent.events, "class") <- c(
    attr(dependent.events, "class"),
    "interaction.groups.updates"
  )
  attr(exogenous.events, "class") <- c(
    attr(exogenous.events, "class"),
    "interaction.groups.updates"
  )

  # PATCH Marion: remove factors in label columns
  groups$label <- as.character(groups$label)

  # PATCH Marion: have the right names in the columns sender and receiver
  interaction.updates$sender <- actors$label[interaction.updates$sender]
  interaction.updates$receiver <- actors$label[interaction.updates$receiver]
  dependent.events$sender <- actors$label[dependent.events$sender]
  dependent.events$receiver <- groups$label[dependent.events$receiver]
  exogenous.events$sender <- actors$label[exogenous.events$sender]
  exogenous.events$receiver <- groups$label[exogenous.events$receiver]

  # Inform about the number of events
  if (progress) {
    cat(
      "Data preparation for DyNAM-i model:\n",
      paste(nrow(dependent.events), "dependent events created"),
      "\n",
      paste(
        nrow(exogenous.events),
        "exogenous events created (group composition updates"
      )
    )
  }

  assemble_interaction_stocnet(
    actors = actors,
    groups = groups,
    dependent_events = dependent.events,
    exogenous_events = exogenous.events,
    interaction_updates = interaction.updates
  )
}

# Assemble the DyNAM-i construction products into a single multipartite stocnet.
# The records->events transformation above is untouched; this only reshapes its
# outputs into the object the estimation surface consumes:
#   - `nodes`: the actors (mode "actor", carrying their attributes) and the
#     `Group1..GroupN` set (mode "group"), one table with a `mode` column.
#   - focal two-mode `interactions` layer: the diagonal initial state (each actor
#     starts in their own singleton group, as `time = NA` rows), the dependent
#     join/leave events (`flavor = "join"`/`"leave"`), and the exogenous rows
#     (`flavor = NA`, state-only). The construction's `order` attributes become
#     the reserved `order` column, so the total within-construction event order
#     survives as the single-object ordering contract.
#   - one-mode `past` covariate layer: the actors x actors past-interaction
#     updates (the paper's X^past).
# The opportunity list is not carried: group availability is a derived support
# constraint on the choice specification, not stored state.
assemble_interaction_stocnet <- function(
  actors,
  groups,
  dependent_events,
  exogenous_events,
  interaction_updates
) {
  n_actors <- nrow(actors)
  n_groups <- nrow(groups)
  labels <- c(actors$label, groups$label)
  local_index <- function(x) match(x, labels)

  # Nodes: actors keep their attributes; groups fill NA. `present` is the
  # default, so it is not carried as an attribute column.
  attr_cols <- setdiff(names(actors), c("label", "present"))
  nodes <- data.frame(
    label = labels,
    mode = c(rep("actor", n_actors), rep("group", n_groups)),
    stringsAsFactors = FALSE
  )
  for (col in attr_cols) {
    nodes[[col]] <- c(actors[[col]], rep(NA, n_groups))
  }

  event_ties <- function(events, layer, flavor) {
    if (is.null(events) || nrow(events) == 0) {
      return(NULL)
    }
    data.frame(
      from = local_index(events$sender),
      to = local_index(events$receiver),
      time = events$time,
      weight = events$increment,
      order = attr(events, "order"),
      flavor = flavor,
      layer = layer,
      stringsAsFactors = FALSE
    )
  }
  # The initial diagonal: actor i affiliated with group i before any event.
  initial_ties <- data.frame(
    from = seq_len(n_actors),
    to = n_actors + seq_len(n_actors),
    time = NA_real_,
    weight = 1,
    order = NA_integer_,
    flavor = NA_character_,
    layer = "interactions",
    stringsAsFactors = FALSE
  )
  dependent_flavor <- ifelse(dependent_events$increment > 0, "join", "leave")
  ties <- rbind(
    initial_ties,
    event_ties(dependent_events, "interactions", dependent_flavor),
    event_ties(exogenous_events, "interactions", NA_character_),
    event_ties(interaction_updates, "past", NA_character_)
  )

  # Declare only the layers that carry ties: a records set with no past
  # interactions produces no `past` rows, and a layer named in sender/receiver
  # with no ties fails validation.
  present <- intersect(c("interactions", "past"), unique(ties$layer))
  info <- list(
    name = "DyNAM-i interaction groups",
    focal = "interactions",
    ties = present,
    update = c(interactions = "increment", past = "increment")[present],
    directed = c(interactions = TRUE, past = FALSE)[present],
    observation = c(interactions = "event", past = "event")[present],
    sender = c(interactions = "actor", past = "actor")[present],
    receiver = c(interactions = "group", past = "actor")[present]
  )

  as_goldfish(manynet::make_stocnet(info = info, nodes = nodes, ties = ties))
}


## For the estimation
# Function that remove extra attributes to windowed events
clean_interaction_events <- function(
  events,
  events_effects_link,
  window_parameters,
  sub_model,
  depName,
  events_objects_link,
  envir
) {
  done.events <- rep(FALSE, dim(events_effects_link)[1])

  # Windowed events: we remove the order of the events
  for (e in seq.int(dim(events_effects_link)[1])) {
    for (eff in seq.int(dim(events_effects_link)[2])) {
      if (
        !done.events[e] &&
          !is.na(events_effects_link[e, eff]) &&
          !is.null(window_parameters[[eff]])
      ) {
        eventsobject <- get(rownames(events_effects_link)[e], envir = envir)

        # correct the order of events
        oldorder <- attr(eventsobject, "order")
        neworder <- rep(0, nrow(eventsobject))
        cpt <- 1
        for (r in seq.int(nrow(eventsobject))) {
          if (eventsobject$increment[r] > 0) {
            neworder[r] <- oldorder[cpt]
            cpt <- cpt + 1
          }
        }
        attr(eventsobject, "order") <- neworder

        # assign the windowed class
        class(eventsobject) <- c(
          class(eventsobject),
          "windowed.interaction.network.updates"
        )

        # reassign object
        assign(rownames(events_effects_link)[e], eventsobject, pos = envir)

        # sanitize events
        objPos <- which(
          events_objects_link$events == rownames(events_effects_link)[e]
        )
        nodesObject <- attr(
          get(events_objects_link[objPos, ]$object, envir = envir),
          "nodes"
        )

        if (length(nodesObject) > 1) {
          nodes <- nodesObject[1]
          nodes2 <- nodesObject[2]
        } else {
          nodes <- nodes2 <- nodesObject
        }
        eventsobject <- sanitizeEvents(eventsobject, nodes, nodes2)
        events[[e]] <- eventsobject

        # we don't go through this event again
        done.events[e] <- TRUE
      }
    }
  }

  return(events)
}
