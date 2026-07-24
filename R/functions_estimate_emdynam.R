##################### ###
#
# Goldfish package
# Functions to perform the EM algorithm for Joint Modeling of Longitudinal
# Network Data with Uneven Temporal Granularity
#
##################### ###

#' estimate_emdynam
#'
#' TODO: change to include the get_options and so on, for initialization,
#'       also for the re-starting with pre-computed "statistics"
#'       The statistics in our case could be {samples, goldfish stats for the samples,...}
#'
#' @return Subset of parameters not fixed
#' @noRd
#'
estimate_emdynam <- function(
  panel_data,
  rem_data,
  actors,
  rem_actors = NULL,
  formulas,
  models = NULL,
  submodels,
  initial_parameters = NULL,
  fixed_parameters = NULL
) {
  #Initialization
  preprocess_emdynam_competition()

  probPerIteration <- list()
  parametersPerIteration <- list()
  mleResultsPerIteration <- list()
  additionalChains <- list()
  nChainsforSim <- nChains
  nSimChains <- 0

  # logProbabilities <- as.data.frame(matrix(NA, nChains, nModels))
  newlogProbabilities <- as.data.frame(matrix(NA, nChains, nModels))

  mleResults <- list()
  estimatedParameters <- list()

  # Initialize loop
  iStep <- 1
  k <- 2
  stopIt <- 0

  while (iStep <= nSteps) {
    cat(paste0(
      "\n\n ****************************************** \n\n Step ",
      iStep,
      "\n\n"
    ))
    sysTime2 <- Sys.time()
    cat(paste0("Time ", sysTime2 - sysTime1, "\n\n"))

    chainSampleP <- get_weights(chainType, logProbabilities, funTransW)
    cat("\nProbabilities: ")
    cat(round(chainSampleP, 3))
    cat("\n")

    if (stratified) {
      sampleIndex <- stratified_resample(cumsum(chainSampleP))
    } else if (residual_samp) {
      sampleIndex <- residual_resample(chainSampleP)
    } else {
      sampleIndex <- sort(sample(
        1:nChains,
        size = nDraws,
        replace = T,
        prob = chainSampleP
      ))
    }

    sampleTable <- table(sampleIndex)
    sampleIndexUnique <- as.numeric(names(sampleTable))

    if (length(sampleIndexUnique) == 1) {
      cat("Resampling: not enough unique samples\n")
      # add more samples to the draw artificially
      if (stratified) {
        auxIndex <- stratified_resample(cumsum(
          chainSampleP[!(1:nChains %in% sampleIndexUnique)] /
            sum(chainSampleP[!(1:nChains %in% sampleIndexUnique)])
        ))
      } else if (residual_samp) {
        auxIndex <- residual_resample(
          chainSampleP[!(1:nChains %in% sampleIndexUnique)] /
            sum(chainSampleP[!(1:nChains %in% sampleIndexUnique)])
        )
      } else {
        auxIndex <- sample(
          (1:nChains)[!(1:nChains %in% sampleIndexUnique)],
          size = nDraws / 5,
          replace = T,
          prob = chainSampleP[!(1:nChains %in% sampleIndexUnique)]
        )
      }

      sampleIndex <- sort(c(sampleIndex[1:(nDraws - nDraws / 5)], auxIndex))
      sampleTable <- table(sampleIndex)
      sampleIndexUnique <- as.numeric(names(sampleTable))
    }

    cat("Sample index: ", sampleIndexUnique, "\n")
    cat("nChains: ", nChains, "\n")
    # Estimate MLE for each chain
    cat("\n")

    cat("\n")
    res_sgd <- sgd_refactor(
      initialParameters,
      fixedParameters,
      nChains,
      formulas,
      formulasType,
      statistics,
      time1,
      time2,
      subModelTypes,
      environmentsChains,
      supportConstrain,
      damping = damping,
      eps = eps_sgd,
      max.it = max.it.sgd,
      impSampling = impSampling,
      chainSamplePRaw = chainType,
      impSamplingUpdate = impSamplingUpdate,
      funTransW = funTransW,
      initialLogLiks = logProbabilities,
      nSizeBatch = nSizeBatch_sgd,
      parallelization = parallelizationSGD,
      nCores = nCores_sgd
    )

    # new initial parameters are calculated as weighted mean
    newParameters <- res_sgd$initialParameters
    newStdErrors <- res_sgd$stdErrors

    # Print current estimates
    cat(paste("Current estimates:\n"))
    for (i in 1:length(newParameters)) {
      cat(paste0(
        "Model ",
        i,
        ": ",
        paste(round(newParameters[[i]], 2), collapse = " "),
        "\n"
      ))
      cat(paste0(
        "Model ",
        i,
        ": ",
        paste(round(newStdErrors[[i]], 2), collapse = " "),
        "\n"
      ))
    }

    cat(paste0("\nCalculating new probability for sample \n"))
    newCalcProbInit <- lapply(1:length(newParameters), function(x) {
      set_algorithm_newton(
        initial_parameters = newParameters[[x]],
        fixed_parameters = fixedParameters[[x]],
        max_iterations = 0L,
        engine = "default"
      )
    })

    clusterExport(cl, c("newCalcProbInit"))
    resP <- clusterApply(cl, seq_along(statistics), function(iChain) {
      Map(
        function(formula, sub_model, preproc_init, op_list, control_est) {
          estimate_dynam(
            x = formula,
            sub_model = sub_model,
            preprocessing_init = preproc_init,
            data = environmentsChains[[iChain]],
            control_estimation = control_est,
            control_preprocessing = set_preprocessing_opt(
              start_time = time1,
              end_time = time2,
              opportunities_list = op_list
            ),
            verbose = FALSE,
            progress = FALSE
          )
        },
        formulas,
        subModelTypes,
        statistics[[iChain]],
        supportConstrain[[iChain]],
        newCalcProbInit
      )
    })

    newlogProbabilities <- t(sapply(resP, function(x) {
      sapply(x, function(model) model$logLikelihood)
    }))
    newlogProbabilities <- as.data.frame(newlogProbabilities)

    # 2. Compute Q and ASE
    # Lambda <- (rowSums(newlogProbabilities[sampleIndexUnique,-3])-rowSums(logProbabilities[sampleIndexUnique,-3]))

    Lambda <- (rowSums(newlogProbabilities[sampleIndexUnique, ]) -
      rowSums(logProbabilities[sampleIndexUnique, ]))
    Q <- sum(as.vector(sampleTable) * Lambda) / nDraws
    sigma2 <- Q^2 *
      (sum(as.vector(sampleTable) * (Lambda)^2) /
        (sum(as.vector(sampleTable) * Lambda))^2 -
        1 / nDraws)
    ASE <- sqrt(sigma2)
    cat(paste0("lambdas = "))
    print(summary(Lambda))
    cat("\n")
    cat(paste0("Q = ", Q, ", ASE = ", ASE, "\n"))

    save(
      newlogProbabilities,
      newParameters,
      newStdErrors,
      nChains,
      logProbabilities,
      sampleTable,
      initialParameters,
      initialStdErrors,
      iStep,
      Q,
      ASE,
      chainSampleP,
      file = filename
    )

    if (is.nan(ASE)) {
      cat("NaN in ASE, end\n")
      break
    }

    cat("Stop: ", abs(Q + qnorm(1 - gamma) * ASE), "\n")

    if (
      abs(Q + qnorm(1 - gamma) * ASE) < epsEnd &
        (Q - qnorm(1 - alpha) * ASE) > 0
    ) {
      cat(paste0("end", "\n"))
      stopIt <- stopIt + 1
      # break
    }

    if (stopIt >= stopIteration) {
      break
    }

    # Secondary stopping rules

    # stop2 <- max(abs(unlist(unfixed_params(newParameters, fixedParameters))-
    #                    unlist(unfixed_params(initialParameters, fixedParameters)))/
    #                (abs(unlist(unfixed_params(initialParameters, fixedParameters)))+delta1))
    # cat(paste0("stop2 ",stop2,"\n"))
    # if(stop2<delta2){
    #   cat(paste0("end, stop2","\n"))
    #   break
    # }
    #
    # stop3 <-  max(abs(unlist(unfixed_params(newParameters, fixedParameters))-
    #                     unlist(unfixed_params(initialParameters, fixedParameters)))/
    #                 (abs(unlist(unfixed_params(newStdErrors,fixedParameters))+delta1)))
    # cat(paste0("stop3 ",stop3,"\n"))
    #
    # if(stop3<delta2){
    #   cat(paste0("end, stop3","\n"))
    #   break
    # }

    if ((Q - qnorm(1 - alpha) * ASE) < 0) {
      # need to increase the number of samples, and repeat step, do not increase iStep
      cat(paste0("need more samples, same iteration", "\n"))
      nSimChains <- ceiling(nChainsforSim / k)
      k <- k + 1

      cat("\nSimulating an additional chain ")

      seed <- sample((100 + 1:(nSimChains + 1000)), nSimChains, replace = F)

      splitIndicesPerCore <- splitIndices(nSimChains, nCores)
      resAux <- clusterApply(
        cl,
        seq_along(splitIndicesPerCore),
        getChainSampleFromModel2MC,
        seed = seed,
        splitIndicesPerCore = splitIndicesPerCore,
        formulasCreation = formulas[formulasType == "creation"],
        formulasDeletion = formulas[formulasType == "deletion"],
        initialParametersCreation = initialParameters[
          formulasType == "creation"
        ],
        initialParametersDeletion = initialParameters[
          formulasType == "deletion"
        ],
        candidateEvents = candidateEvents,
        fb.environment = environment_fb,
        time1 = time1,
        time2 = time2,
        verbose = F,
        actors.u = actors,
        net.u = net1,
        condProbs = condProbs
      )

      resPermute <- unlist(lapply(resAux, "[[", "resChain"), recursive = F)
      proposalProbs <- unlist(
        lapply(resAux, "[[", "resProposalProb"),
        recursive = T
      )

      for (i in 1:nSimChains) {
        iChain <- length(creation.events.sample) + 1

        creation.events.sample[[iChain]] <- subset(
          resPermute[[i]],
          replace == 1
        )
        deletion.events.sample[[iChain]] <- subset(
          resPermute[[i]],
          replace == 0
        )
        supportConstrain[[iChain]] <- computeSupportConstrain(
          resPermute[[i]],
          net1,
          actors$label
        )
        supportConstrain[[iChain]] <- lapply(formulasType, function(mt) {
          if (mt == "creation") {
            supportConstrain[[iChain]]$creation
          } else if (mt == "deletion") {
            supportConstrain[[iChain]]$deletion
          } else {
            NULL
          }
        })
      }
      environmentsChains <- c(
        environmentsChains,
        lapply((nChains + 1):(nChains + nSimChains), loadDataFast)
      )

      for (i in 1:nSimChains) {
        iChain <- nChains + i

        # calculate statistics of friendship models

        res <- Map(
          function(formula, sub_model, op_list, fix) {
            estimate_dynam(
              x = formula,
              sub_model = sub_model,
              data = environmentsChains[[iChain]],
              control_estimation = set_algorithm_newton(
                engine = "default",
                fixed_parameters = fix
              ),
              control_preprocessing = set_preprocessing_opt(
                start_time = time1,
                end_time = time2,
                opportunities_list = op_list
              ),
              verbose = FALSE,
              preprocessing_only = T,
              progress = FALSE
            )
          },
          formulas,
          subModelTypes,
          supportConstrain[[iChain]],
          fixedParameters
        )

        statistics[[length(statistics) + 1]] <- res
      }

      clusterExport(
        cl,
        c(
          "statistics",
          "subModelTypes",
          "calcProbInit",
          "environmentsChains",
          "nChains",
          "supportConstrain"
        )
      )
      resP <- clusterApply(cl, 1:nSimChains, function(iChain) {
        Map(
          function(formula, sub_model, preproc_init, op_list, control_est) {
            estimate_dynam(
              x = formula,
              sub_model = sub_model,
              preprocessing_init = preproc_init,
              data = environmentsChains[[nChains + iChain]],
              control_estimation = control_est,
              control_preprocessing = set_preprocessing_opt(
                start_time = time1,
                end_time = time2,
                opportunities_list = op_list
              ),
              verbose = FALSE,
              progress = FALSE
            )
          },
          formulas,
          subModelTypes,
          statistics[[nChains + iChain]],
          supportConstrain[[nChains + iChain]],
          calcProbInit
        )
      })

      logLikelihoods <- t(sapply(resP, function(x) {
        sapply(x, function(model) model$logLikelihood)
      }))
      logProbabilities <- rbind(logProbabilities, as.data.frame(logLikelihoods))

      nChains <- nChains + nSimChains
      nDraws <- nChains

      if (impSampling) {
        chainType <- c(chainType, proposalProbs)
      } else {
        chainType <- rep(1, nChains)
      }
    } else {
      # next iteration, estimator is accepted.
      cat(paste0("next iteration, checking if more samples needed", "\n"))

      # update probabilities and parameters
      initialParameters <- newParameters
      initialStdErrors <- newStdErrors
      logProbabilities <- newlogProbabilities
      calcProbInit <- newCalcProbInit

      # store some values
      probPerIteration[[iStep]] <- logProbabilities
      parametersPerIteration[[iStep]] <- initialParameters
      mleResultsPerIteration[[iStep]] <- mleResults

      nSimChains <- nChains -
        ceiling(max(
          nChains,
          sigma2 * (qnorm(1 - beta) + qnorm(1 - alpha))^2 / Q^2
        ))

      if (nSimChains > 0) {
        # diffNet <- net2 - net1
        # candidateEvents <- data.frame(which(diffNet != 0, arr.ind = T),
        #                               replace = diffNet[which(diffNet != 0, arr.ind = T)])
        # candidateEvents$replace[candidateEvents$replace == -1] <- 0
        # names(candidateEvents)[1:2] <- c("sender", "receiver")
        # candidateEvents$sender = as.integer(colnames(net1))[candidateEvents$sender]
        # candidateEvents$receiver = as.integer(colnames(net1))[candidateEvents$receiver]
        #

        seed <- sample((100 + 1:(nSimChains + 1000)), nSimChains, replace = F)

        splitIndicesPerCore <- splitIndices(nSimChains, nCores)
        resAux <- clusterApply(
          cl,
          seq_along(splitIndicesPerCore),
          getChainSampleFromModel2MC,
          seed = seed,
          splitIndicesPerCore = splitIndicesPerCore,
          formulasCreation = formulas[formulasType == "creation"],
          formulasDeletion = formulas[formulasType == "deletion"],
          initialParametersCreation = initialParameters[
            formulasType == "creation"
          ],
          initialParametersDeletion = initialParameters[
            formulasType == "deletion"
          ],
          candidateEvents = candidateEvents,
          fb.environment = environment_fb,
          time1 = time1,
          time2 = time2,
          verbose = F,
          actors.u = actors,
          net.u = net1,
          condProbs = condProbs
        )
        resPermute <- unlist(lapply(resAux, "[[", "resChain"), recursive = F)
        proposalProbs <- unlist(
          lapply(resAux, "[[", "resProposalProb"),
          recursive = T
        )

        for (i in 1:nSimChains) {
          iChain <- length(creation.events.sample) + 1

          creation.events.sample[[iChain]] <- subset(
            resPermute[[i]],
            replace == 1
          )
          deletion.events.sample[[iChain]] <- subset(
            resPermute[[i]],
            replace == 0
          )
          supportConstrain[[iChain]] <- computeSupportConstrain(
            resPermute[[i]],
            net1,
            actors$label
          )
          supportConstrain[[iChain]] <- lapply(formulasType, function(mt) {
            if (mt == "creation") {
              supportConstrain[[iChain]]$creation
            } else if (mt == "deletion") {
              supportConstrain[[iChain]]$deletion
            } else {
              NULL
            }
          })
        }
        environmentsChains <- c(
          environmentsChains,
          lapply((nChains + 1):(nChains + nSimChains), loadDataFast)
        )

        for (i in 1:nSimChains) {
          iChain <- nChains + i

          # calculate statistics of friendship models
          res <- Map(
            function(formula, sub_model, op_list, fix) {
              estimate_dynam(
                x = formula,
                sub_model = sub_model,
                data = environmentsChains[[iChain]],
                control_estimation = set_algorithm_newton(
                  engine = "default",
                  fixed_parameters = fix
                ),
                control_preprocessing = set_preprocessing_opt(
                  start_time = time1,
                  end_time = time2,
                  opportunities_list = op_list
                ),
                verbose = FALSE,
                preprocessing_only = T,
                progress = FALSE
              )
            },
            formulas,
            subModelTypes,
            supportConstrain[[iChain]],
            fixedParameters
          )

          statistics[[length(statistics) + 1]] <- res
        }

        clusterExport(
          cl,
          c(
            "statistics",
            "subModelTypes",
            "calcProbInit",
            "environmentsChains",
            "nChains",
            "supportConstrain"
          )
        )
        resP <- clusterApply(cl, 1:nSimChains, function(iChain) {
          Map(
            function(formula, sub_model, preproc_init, op_list, control_est) {
              estimate_dynam(
                x = formula,
                sub_model = sub_model,
                preprocessing_init = preproc_init,
                data = environmentsChains[[nChains + iChain]],
                control_estimation = control_est,
                control_preprocessing = set_preprocessing_opt(
                  start_time = time1,
                  end_time = time2,
                  opportunities_list = op_list
                ),
                verbose = FALSE,
                progress = FALSE
              )
            },
            formulas,
            subModelTypes,
            statistics[[nChains + iChain]],
            supportConstrain[[nChains + iChain]],
            calcProbInit
          )
        })

        logLikelihoods <- t(sapply(resP, function(x) {
          sapply(x, function(model) model$logLikelihood)
        }))
        logProbabilities <- rbind(
          logProbabilities,
          as.data.frame(logLikelihoods)
        )

        nChains <- nChains + nSimChains
        nDraws <- nChains

        if (impSampling) {
          chainType <- c(chainType, proposalProbs)
        } else {
          chainType <- rep(1, nChains)
        }
      }

      iStep <- iStep + 1
      k <- 2
      nChainsforSim <- nChains
      nSimChains <- 0
    }
  }

  stopCluster(cl)

  retunr(list(
    "parameters" = newParameters,
    "std.errors" = newStdErrors,
    "logProbabilities" = logProbabilities
  ))
}
