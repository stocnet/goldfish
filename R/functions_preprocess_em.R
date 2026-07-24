##################### ###
#
# Goldfish package
# Functions to perform the preprocessing of the EM algorithm for Joint Modeling
# of Longitudinal Network Data with Uneven Temporal Granularity
#
##################### ###

#' preprocess_emdynam_competition
#'
#' TODO: change to include the get_options and so on, for initialization,
#'       also for the re-starting with pre-computed "statistics"
#'       The statistics in our case could be {samples, goldfish stats for the samples,...}
#'
#' @return Subset of parameters not fixed
#' @noRd
#'
preprocess_emdynam_competition <- function() {
  creation.events.sample <- list()
  deletion.events.sample <- list()
  supportConstrain <- list()
  seedsGenerated <- sample((1:(nChains + 1000)), nChains, replace = F)
  for (i in 1:nChains) {
    aux <- getChainSample(
      net1 = net1,
      net2 = net2,
      time1 = time1,
      time2 = time2,
      labels = actors$label,
      seed = seedsGenerated[i]
    )
    creation.events.sample[[i]] <- aux[aux$replace == 1, ]
    deletion.events.sample[[i]] <- aux[aux$replace == 0, ]
    eventsTot <- arrange(
      rbind(creation.events.sample[[i]], deletion.events.sample[[i]]),
      time
    )

    supportConstrain[[i]] <- computeSupportConstrain(
      eventsTot,
      net1,
      actors$label
    )
    supportConstrain[[i]] <- lapply(formulasType, function(mt) {
      if (mt == "creation") {
        supportConstrain[[i]]$creation
      } else if (mt == "deletion") {
        supportConstrain[[i]]$deletion
      } else {
        NULL
      }
    })
  }
  proposalProbs <- rep(
    lfactorial(
      nrow(creation.events.sample[[1]]) + nrow(deletion.events.sample[[1]])
    ),
    nChains
  )

  statistics <- list()
  chainType <- c()
  std.errors <- list()

  # iChain =1

  environmentsChains <- lapply(1:nChains, loadDataFast)
  # for(i in 1:nChains) environmentsChains[[i]]$fun_transform <-fun_transform

  for (iChain in 1:nChains) {
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

    statistics[[iChain]] <- res
  }

  initialParameters <- NULL
  probPerIteration <- list()
  parametersPerIteration <- list()
  mleResultsPerIteration <- list()
  additionalChains <- list()
  nChainsforSim <- nChains
  nSimChains <- 0
  estimatedParameters <- list()

  logProbabilities <- as.data.frame(matrix(NA, nChains, nModels))
  newlogProbabilities <- as.data.frame(matrix(NA, nChains, nModels))

  mleResults <- list()
  calcProbInit <- list()

  if (is.null(initialParameters)) {
    calcProbInit <- lapply(1:length(formulasType), function(x) {
      set_algorithm_newton(
        initial_parameters = NULL,
        fixed_parameters = fixedParameters[[x]],
        max_iterations = 0L,
        engine = "default",
        return_interval_loglik = TRUE
      )
    })
  } else {
    calcProbInit <- lapply(1:length(initialParameters), function(x) {
      set_algorithm_newton(
        initial_parameters = initialParameters[[x]],
        fixed_parameters = fixedParameters[[x]],
        max_iterations = 0L,
        engine = "default",
        return_interval_loglik = TRUE
      )
    })
  }

  cl <- makeCluster(getOption("cl.cores", nCores))
  # on.exit(stopCluster(cl))
  clusterExport(
    cl,
    list(
      "getChainSampleFromModel",
      "getDyNAMChoices",
      "getDyNAMRates",
      "labelTorow",
      "computeSupportConstrain",
      "getChoiceProb",
      "getWaitingTime",
      "loadDataFast",
      "getData"
    )
  )
  clusterEvalQ(cl, {
    library(goldfish)
    library(matrixStats)
    library(dplyr)
    NULL
  })

  cat(paste0("\nCalculating probability for sample "))

  clusterExport(
    cl,
    c(
      "time1",
      "time2",
      "initialParameters",
      "formulas",
      "net1",
      "actors",
      "environmentsChains",
      "supportConstrain",
      "fixedParameters",
      "modelTypes",
      "subModelTypes",
      "statistics",
      "calcProbInit"
    )
  )

  chainType <- c()

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
      calcProbInit
    )
  })

  logProbabilities <- t(sapply(resP, function(x) {
    sapply(x, "[[", "logLikelihood")
  }))

  if (impSampling) {
    chainType <- proposalProbs
  } else {
    chainType <- rep(1, nChains)
  }

  if (is.null(initialParameters)) {
    initialParameters <- lapply(resP[[1]], "[[", "parameters")
  }

  chainSampleP <- get_weights(chainType, logProbabilities, funTransW)
  cat(paste0("\nProbabilities: "))
  cat(round(chainSampleP, 3))
  cat("\n")

  # Option 1: resampling -> weigths in the approximation of Q should be set to 1
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

  cat("Sample index: ", sampleIndexUnique)
  # Estimate MLE for each chain
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

  initialParameters <- newParameters
  initialStdErrors <- newStdErrors

  ## Part 1  -------
  # # TODO: Similarly, randomize the direction of sender/receiver in the FB part
  # creation.events.sample <- list()
  # deletion.events.sample <- list()
  # statistics <- list()
  chainType <- c()
  proposalProbs <- c()

  diffNet <- net2 - net1
  candidateEvents <- data.frame(
    which(diffNet != 0, arr.ind = T),
    replace = diffNet[which(diffNet != 0, arr.ind = T)]
  )
  candidateEvents$replace[candidateEvents$replace == -1] <- 0
  names(candidateEvents)[1:2] <- c("sender", "receiver")
  candidateEvents$sender <- as.integer(colnames(net1))[candidateEvents$sender]
  candidateEvents$receiver <- as.integer(colnames(net1))[
    candidateEvents$receiver
  ]

  seed <- sample((100 + 1:(nChains + 1000)), nChains, replace = F)
  stopCluster(cl)
  cl <- makeCluster(getOption("cl.cores", nCores))
  clusterExport(
    cl,
    list(
      "getChainSampleFromModel",
      "getDyNAMChoices",
      "getDyNAMRates",
      "labelTorow",
      "computeSupportConstrain",
      "getChoiceProb",
      "getWaitingTime",
      "loadDataFast",
      "getData",
      "mleMC"
    )
  )
  clusterEvalQ(cl, {
    library(goldfish)
    library(matrixStats)
    library(dplyr)
    NULL
  })
  clusterExport(cl, c("presentUpdate", "fb.events"))

  clusterExport(
    cl,
    c(
      "seed",
      "candidateEvents",
      "time1",
      "time2",
      "initialParameters",
      "formulas",
      "net1",
      "actors",
      "formulasType",
      "environment_fb",
      "subModelTypes"
    )
  )
  clusterExport(
    cl,
    list(
      "getChainSampleFromModel2",
      "getChainSampleFromModel2MC"
    )
  )
  splitIndicesPerCore <- splitIndices(nChains, nCores)
  resAux <- clusterApply(
    cl,
    seq_along(splitIndicesPerCore),
    getChainSampleFromModel2MC,
    seed = seed,
    splitIndicesPerCore = splitIndicesPerCore,
    formulasCreation = formulas[formulasType == "creation"],
    formulasDeletion = formulas[formulasType == "deletion"],
    initialParametersCreation = initialParameters[formulasType == "creation"],
    initialParametersDeletion = initialParameters[formulasType == "deletion"],
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

  for (iChain in 1:nChains) {
    creation.events.sample[[iChain]] <- subset(
      resPermute[[iChain]],
      replace == 1
    )
    deletion.events.sample[[iChain]] <- subset(
      resPermute[[iChain]],
      replace == 0
    )
    supportConstrain[[iChain]] <- computeSupportConstrain(
      resPermute[[iChain]],
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

  environmentsChains <- lapply(1:nChains, loadDataFast)

  calcProbInit <- lapply(1:length(initialParameters), function(x) {
    set_algorithm_newton(
      initial_parameters = initialParameters[[x]],
      fixed_parameters = fixedParameters[[x]],
      max_iterations = 0L,
      engine = "default"
    )
  })

  for (iChain in 1:nChains) {
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

    statistics[[iChain]] <- res
  }

  clusterExport(
    cl,
    c(
      "statistics",
      "subModelTypes",
      "calcProbInit",
      "environmentsChains",
      "supportConstrain"
    )
  )
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
      calcProbInit
    )
  })

  logLikelihoods <- t(sapply(resP, function(x) {
    sapply(x, function(model) model$logLikelihood)
  }))
  logProbabilities <- as.data.frame(logLikelihoods)

  nDraws <- nChains

  if (impSampling) {
    chainType <- c(chainType, proposalProbs)
  } else {
    chainType <- rep(1, nChains)
  }
}
