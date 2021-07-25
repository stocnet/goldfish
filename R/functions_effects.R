#####################################
#
# Goldfish package
# Effects definitions
#
# For developers:
#
# The following parameters are available:
# optional data objects (at least one)
# - network, network2, attribute, attribute2,
# mandatory
# - statistics = NULL,
# either of the following two lines is mandatory
# - sender, receiver, replace
# - node, replace
# reserved for future use in effects with binary network or attributes
# - replaceObject # integer relating to the position of the data object
#   in the signature
#   Specific effect definitions, only use if used; set a default
# - weighted = F
# - parameter, parameter2
#
# allowed return formats:
# - cbind(node1 = x, node2 = y, replace = z)
# - NULL
#
# Things to consider when implementing/testing an effect:
# - What if the initial value is NA?
# - What if the current value is NA?
# - What if the replace value is NA?
# - What if the replace value is zero?
# - What if the replace value is negative?
# - What if the network is weighted?
# - What if the network is two-mode?
#     (e.g. if(sender == receiver) return(NULL) could not work)
# - What if there is composition change?
#
#####################################

# Effects Functions ####

#' Goldfish effects.
#'
#' This page catalogues the list of generic effects that are available in the
#' goldfish package.
#' \code{ignoreRep}, \code{weighted}, \code{window}, and
#' transformed (\code{transformFun}) versions can be included by adding
#' these arguments.
#'
#' Note that the use of some effects (combinations) are ill-advised.
#' For example, using \code{tie(network, ignoreRep = FALSE)},
#' where the network refers to the dependent network,
#' will always result in a change statistic of zero,
#' and thus cannot be used.
#'
#' @param network,attribute Objects upon which the effect should be calculated.
#'   For network effects, this would be either an independent network or the
#'   dependent network by default.
#'   For attribute effects, this should be the attribute name with the
#'   indication of the data frame containing the initial values of
#'   nodes attributes (\code{data.frame$attribute}),
#'   see \code{\link{defineNodes}} for details.
#' @param isTwoMode Identifies whether the effect assumes the network is
#'   originated from a two-mode network.
#'   The default value is \code{FALSE}, it means that updates involving
#'   self-ties are ignore during the statistics update.
#' @param ignoreRep Identifies whether the effect recognizes actors to send
#'   additional ties beyond the first to receivers
#'   (\code{FALSE} means additional ties are taken into account).
#'   The default value is \code{FALSE}.
#' @param weighted Identifies whether the effect relies on the presence or
#'   number of ties (\code{TRUE} means it relies also on the number of ties).
#'   The interpretation depends on the specific effect.
#'   The default value is \code{FALSE}.
#' @param window Identifies a window length within which changes should apply
#'   to events. The default value is \code{Inf} meaning that no windows are
#'   applied to the effect. Window size can be specified as a number in seconds
#'   (i.e. an hour is 3600 seconds), or as a string stating the number of
#'   time units in the format "number unit", for example "30 seconds",
#'   "1 minute", "2 hours", "3 days", "4 weeks", "5 months", or "6 years".
#'   \emph{Note:} The time units are converted to seconds using their most common
#'   lengths in seconds.
#' @param transformFun Use this parameter to obtain transformed statistics.
#'   The argument expected is a function.
#' @param aggregateFun Use this parameter to obtain an aggregated statistics
#'   for indirect effects like \code{tertius} and \code{tertiusDiff}.
#'   The argument expected is a function.
#' @param type Applies only to \code{indegree}, \code{outdegree} and
#'   \code{nodeTrans} in the case of the REM model.
#'   In the default case of \code{type = "alter"}, the effect returns
#'   statistics matrices according to the potential receivers.
#'   In the case where \code{type = "ego"}, the effect returns
#'   statistics matrices according to the sender.
#'   This argument does not apply in the case of the DyNAM models:
#'   the DyNAM-choice model only considers the "alter" value,
#'   while in the DyNAM-rate, the "ego" value is always to be considered
#'   for these three effects.
#' @return List with the changes to a statistics matrix.
#'   It also can have an additional component with the cache object
#'   if the effect computation requires one.
#' @aliases effects_DyNAM effects_REM
#' @usage
#' # Node/actors statistics:
#' ## Structural effects
#' indeg(network, isTwoMode = FALSE, weighted = FALSE, window = Inf,
#'       ignoreRep = FALSE, type = c("alter", "ego"), transformFun = identity)
#' outdeg(network, isTwoMode = FALSE, weighted = FALSE, window = Inf,
#'        ignoreRep = FALSE, type = c("alter", "ego"), transformFun = identity)
#' nodeTrans(network, isTwoMode = FALSE, window = Inf, ignoreRep = FALSE,
#'       type = c("alter", "ego"), transformFun = identity)
#' ## Attribute effects
#' ego(attribute)
#' alter(attribute)
#' ## Structural + Attribute effects
#' tertius(network, attribute, isTwoMode = FALSE, weighted = FALSE, window = Inf,
#'         ignoreRep = FALSE, type = c("alter", "ego"), transformFun = identity,
#'         aggregateFun = function(x) mean(x, na.rm = TRUE))
#'
#' # Dyadic statistics:
#' ## Structural effects
#' tie(network, weighted = FALSE, window = Inf, ignoreRep = FALSE,
#'     transformFun = identity)
#' inertia(network, weighted = FALSE, window = Inf, transformFun = identity)
#' recip(network, weighted = FALSE, window = Inf, ignoreRep = FALSE,
#'       transformFun = identity)
#' ## Attribute effects
#' same(attribute)
#' diff(attribute, transformFun = abs)
#' sim(attribute, transformFun = abs)
#' ## Structural + Attribute effects
#' tertiusDiff(network, attribute, isTwoMode = FALSE, weighted = FALSE,
#'              window = Inf, ignoreRep = FALSE, transformFun = abs,
#'              aggregateFun = function(x) mean(x, na.rm = TRUE))
#'
#' # Closure effects
#' ## Same network
#' trans(network, window = Inf, ignoreRep = FALSE, transformFun = identity)
#' cycle(network, window = Inf, ignoreRep = FALSE, transformFun = identity)
#' clSender(network, window = Inf, ignoreRep = FALSE, transformFun = identity)
#' clReceiver(network, window = Inf, ignoreRep = FALSE, transformFun = identity)
#' four(network, isTwoMode = FALSE, window = Inf, ignoreRep = FALSE,
#'      transformFun = identity)
#' ## Mixed networks
#' mixedTrans(network = list(network1, network2), window = Inf,
#'            ignoreRep = FALSE, transformFun = identity)
#' mixedCycle(network = list(network1, network2), window = Inf,
#'            ignoreRep = FALSE, transformFun = identity)
#' mixedClSender(network = list(network1, network2), window = Inf,
#'               ignoreRep = FALSE, transformFun = identity)
#' mixedClReceiver(network = list(network1, network2), window = Inf,
#'                 ignoreRep = FALSE, transformFun = identity)
#'
#' @section Effects:
#' \subsection{Node/actor statistics:}{
#' \subsection{Structural:}{
#' \describe{
#'   \item{\code{indeg}}{DyNAM-Rate model: tendency of actor i to create an
#'     event when i has a high incoming degree in a covariate network
#'     (ego type).
#'     DyNAM-choice model: tendency to create an event i->j when j has a high
#'     incoming degree in a covariate network ('alter' type).
#'     In REM model: tendency to create an event i->j when either i or j has a
#'     high incoming degree in a covariate network.
#'     An argument type allows to choose whether to use the indegree effect
#'     for sender i (\code{type = "ego"}) or for receiver j
#'     (\code{type = "alter"}).
#'     In DyNAM-choice_coordination: the stat compute the total degree in a
#'     covariate network ('alter' type).
#'     The degree can be transform with \code{transformFun}.}
#'   \item{\code{outdeg}}{DyNAM-Rate model: tendency of actor i to create an
#'     event when i has a high outgoing degree in a covariate network
#'     (ego type).
#'     DyNAM-choice model: tendency to create an event i->j when j has a
#'     high outgoing degree in a covariate network ('alter' type).
#'     REM model: tendency to create an event i->j when either i or j has a
#'     high outgoing degree in a covariate network. An argument type allows to
#'     choose whether to use the outdegree effect for sender i
#'     (\code{type = "ego"}) or for receiver j (\code{type = "alter"}).
#'     The degree can be transform with \code{transformFun}.
#'     It's not available for DyNAM-choice_coordination}
#'   \item{\code{nodeTrans}}{Embeddedness in transitive structures as a
#'    source node.
#'    It is the tendency of actor i to send a tie when there are transitive
#'    triangles where i is the source (i->k->j<-i) that are closed.
#'    Available for DyNAM-rate, DyNAM-choice and REM models.
#'    In REM model, an argument type allows to choose whether to use the
#'    node transitivity effect for sender i (\code{type = "ego"}) or for
#'    receiver j (\code{type = "alter"}).
#'    The statistic can be transform with \code{transformFun},
#'    there is not weighted version for this effect.
#'    It's not available for DyNAM-choice_coordination}
#' }}
#' \subsection{Attribute:}{
#' \describe{
#'   \item{\code{ego}}{DyNAM-rate: tendency of actors to be more active when
#'   they score high on \code{attribute}.
#'   For REM is the tendency of an event i->j when i has a high attribute.
#'   This effects cannot be used for the DyNAM-choice,
#'   DyNAM-choice_coordination models.}
#'   \item{\code{alter}}{Is the tendency to create an event i->j when j has a
#'   high \code{attribute}.
#'   This effects cannot be used for the DyNAM-rate model.}
#' }}
#' \subsection{Structural and attribute:}{
#' \describe{
#'   \item{\code{tertius}}{For DyNAM-choice and REM is the tendency to create
#'   an event i->j when j has a high aggregate (\code{aggregateFun}) value of
#'   its in-neighbors (all k with \code{network[k, j] > 0}).
#'   For DyNAM-rate is the tendency of actors to be more active when they have
#'   a high aggregate value of their in-neighbors.
#'   \emph{Note:} When the node does not have in-neighbors,
#'   the average of the aggregate values of nodes is used for the node.
#'   }
#' }}
#' }
#' \subsection{Dyadic statistics:}{
#' These effects are not available for DyNAM-rate model
#' (\code{model = "DyNAM"} and \code{subModel = "rate"}).
#' \subsection{Structural:}{
#' \describe{
#'   \item{\code{tie}}{Tendency to create an event i->j if the tie i->j exists
#'   in a covariate network.
#'   Parameter \code{weighted} can be set to \code{TRUE} if the weight of i->j
#'   is to be taken as a statistic.
#'   It can be transformed by using \code{transformFun}
#'   (This only makes sense with \code{weighted = TRUE}).}
#'   \item{\code{inertia}}{Usually used as the "intercept" for the
#'     choice function, 'inertia' is the tendency to create an event i->j
#'     if the event i->j happened before.
#'     It can be interpreted as the differential tendency to update existing
#'     ties rather than creating new ones.
#'     Thus, 'inertia' is similar to 'tie', but defined on the network to
#'     which the dependent events relate.
#'     Parameter \code{weighted} can be set to \code{TRUE} if the weight of
#'     i->j is to be taken as a statistic.
#'     It can be transformed by using \code{transformFun}
#'     (This only makes sense with \code{weighted = TRUE}).
#'     \emph{Note:} \code{inertia} can never be used in combination with a
#'     \code{ignoreRep = TRUE} parameter as this would replace all positive
#'     statistics with zeros.}
#'   \item{\code{recip}}{Reciprocity. Tendency to create an event i->j if one
#'     or several j->i happened before.
#'     Parameter \code{weighted} can be set to \code{TRUE} if the weight of j->i
#'     is to be taken as a statistic.
#'     It can be transformed by using \code{transformFun} (This only makes sense
#'     with \code{weighted = TRUE}).
#'     This effect cannot be used for two-mode networks and for
#'     DyNAM-choice_coordination model.}
#' }}
#' \subsection{Attribute:}{
#' \describe{
#'   \item{\code{same}}{Homophily (same value). The tendency of an event i->j
#'   to happen if actors i and j have the same \code{attribute} value.
#'   This effect cannot be used for two-mode networks
#'   and for the DyNAM-rate model.}
#'   \item{\code{sim}}{Homophily (similar value). The tendency of an event i->j
#'   to happen if actors i and j have similar \code{attribute} values
#'   (low absolute differences regarding \code{attribute}
#'   if \code{transformFun = abs}).
#'   This effect cannot be used for two-mode networks.}
#'   \item{\code{diff}}{Heterophily. The tendency of an event i->j to happen
#'   if actors i and j have different \code{attribute} values
#'   (high absolute differences regarding \code{attribute}
#'   if \code{transformFun = abs}).
#'   This effect cannot be used for two-mode networks.}
#' }}
#' \subsection{Structural and attribute:}{
#' \describe{
#'   \item{\code{tertiusDiff}}{The tendency to create an event i->j when i has
#'     a similar value as j aggregate (\code{aggregateFun}) value of its
#'     in-neighbors (all k with \code{network[k, j] > 0}.
#'    \emph{Note:} When the node j does not have in-neighbors, the average of
#'    the similarities computed for the pairs i, k when k has in-neighbors is
#'    used for the node.
#'   }
#' }}
#' }
#' \subsection{Closure statistics:}{
#' These effects are not available for DyNAM-rate model (\code{model = "DyNAM"}
#' and \code{subModel = "rate"}).
#' \subsection{Same covariate network:}{
#' \describe{
#'   \item{\code{trans}}{Transitivity. It is the tendency to create an event
#'    i->j if it closes a two-paths (i->k->j) in the past events \code{network}.
#'    It can be transformed by using \code{transformFun}.
#'    This effect cannot be used for two-mode networks.}
#'   \item{\code{cycle}}{It is the tendency to create an event i->j if it closes
#'     a two-paths (j->k->i) in the past events \code{network}.
#'     It can be transformed by using \code{transformFun}.
#'     This effect cannot be used for two-mode networks and
#'     DyNAM-choice_coordination.}
#'   \item{\code{clSender}}{Closure sender. It is the tendency to create an
#'     event i->j if it closes a two-paths (i<-k->j) in the past events
#'     \code{network}.
#'     It can be transformed by using \code{transformFun}.
#'     This effect cannot be used for two-mode networks and
#'     DyNAM-choice_coordination.}
#'   \item{\code{clReceiver}}{Closure receiver. It is the tendency to create an
#'     event i->j if it closes a two-paths (i->k<-j) in the past events
#'     \code{network}.
#'     It can be transformed by using \code{transformFun}.
#'     This effect cannot be used for two-mode networks and
#'     DyNAM-choice_coordination.}
#'   \item{\code{four}}{It describes the tendency to create an event i->j if
#'     there are many possible closures of three-paths (i->k<-l->j) in the past
#'     events \code{network}.
#'     It can be transformed by using \code{transformFun}.
#'     This effect cannot be used for two-mode networks and
#'     DyNAM-choice_coordination.}
#' }}
#' \subsection{Mixed networks:}{
#' \describe{
#'   \item{\code{mixedTrans}}{Transitivity within 2 networks.
#'     It is the tendency to create an event i->j if it closes
#'     a two-paths with events (i->k) in \code{network1} and
#'     (k->j) in \code{network2} in the past events.
#'     It can be transformed by using \code{transformFun}.
#'     This effect cannot be used for two-mode networks.}
#'   \item{\code{mixedCycle}}{Cycle within 2 networks.
#'     It is the tendency to create an event i->j if it closes
#'     a two-paths with events (k->i) in \code{network1} and
#'     (j->k) in \code{network2} in the past events.
#'     It can be transformed by using \code{transformFun}.
#'     This effect cannot be used for two-mode networks and
#'     DyNAM-choice_coordination.}
#'   \item{\code{mixedClSender}}{Closure sender within 2 networks.
#'     It is the tendency to create an event i->j if it closes
#'     a two-paths with events (k->i) in \code{network1} and
#'     (k->j) in \code{network2} in the past events.
#'     It can be transformed by using \code{transformFun}.
#'     This effect cannot be used for two-mode networks and
#'     DyNAM-choice_coordination.}
#'   \item{\code{mixedClReceiver}}{Closure receiver within 2 networks.
#'     It is the tendency to create an event i->j if it closes
#'     a two-paths with events (i->k) in \code{network1} and
#'     (j->k) in \code{network2} in the past events.
#'     It can be transformed by using \code{transformFun}.
#'     This effect cannot be used for two-mode networks and
#'     DyNAM-choice_coordination.}
#' }}
#' }
#'
#' @examples
#' data("Social_Evolution")
#' actors <- defineNodes(nodes = actors)
#' call.Network <- defineNetwork(nodes = actors, directed = TRUE)
#' call.Network <- linkEvents(x = call.Network, changeEvent = calls,
#'                            nodes = actors)
#' calls <- defineDependentEvents(events = calls, nodes = actors,
#'                              defaultNetwork = call.Network)
#'
#' # Using a DyNAM-Rate model:
#' estimate(calls ~ ego(actors$gradeType) + indeg(call.Network),
#'          model = "DyNAM", subModel = "rate")
#'
#' # Using a DyNAM-Choice model:
#' estimate(calls ~ trans(call.Network, window=259200) + indeg(call.Network),
#'          model = "DyNAM", subModel = "choice")
#'
#' # Using a REM model:
#' estimate(calls ~ outdeg(call.Network, type="ego") +
#'                 indeg(call.Network, type="alter"), model = "REM")
#'
#'
# depEvent ~ inertia + indeg(trade, ignoreRep = FALSE, weighted = TRUE) +
#            four(membership, ignoreRep = TRUE)

goldfishEffects <- function() {
  print("TODO: List of all available effects")
}

# Deprecated effects ####
out <- function(network, sender, receiver, replace,
                weighted = FALSE, parameter = 1, isTwoMode = FALSE) {
  .Deprecated("tie", package = "goldfish", old = "out")
  if (inherits(parameter, "numeric") && parameter != 1) {
    transformFun  <- function(x) x ^ parameter
  } else transformFun  <-  identity
  update_DyNAM_choice_tie(network = network, sender = sender, receiver = receiver, replace = replace,
                          weighted = weighted, transformFun = transformFun)
}

ind <- function(network, cache, sender, receiver, replace,
                weighted = FALSE, parameter = 1, n1, n2, isTwoMode = FALSE) {
  .Deprecated("indeg", package = "goldfish", old = "ind")
  if (inherits(parameter, "numeric") && parameter != 1) {
    transformFun <- function(x) x ^ parameter
  } else transformFun <- identity
  update_DyNAM_choice_indeg(network = network, sender = sender, receiver = receiver, replace = replace,
                            cache = cache, n1 = n1, n2 = n2, isTwoMode = isTwoMode,
                            weighted = weighted, transformFun = transformFun)
}

rec <- function(network, sender, receiver, replace,
                weighted = FALSE, parameter = 1, isTwoMode = FALSE) {
  .Deprecated("recip", package = "goldfish", old = "rec")
  if (inherits(parameter, "numeric") && parameter != 1) {
    transformFun <- function(x) x ^ parameter
  } else transformFun <- identity
  update_DyNAM_choice_recip(network = network, sender = sender, receiver = receiver, replace = replace,
                            isTwoMode = isTwoMode,
                            weighted = weighted, transformFun = transformFun)
}

egoX <- function(attribute, node, replace, n1, n2, isTwoMode = FALSE) {
  .Deprecated("ego", package = "goldfish", old = "egoX")
  update_REM_choice_ego(attribute = attribute, node = node, replace = replace,
                        n1 = n1, n2 = n2, isTwoMode = isTwoMode)
}

altX <- function(attribute, node, replace, n1, n2, isTwoMode = FALSE) {
  .Deprecated("alter", package = "goldfish", old = "altX")
  update_DyNAM_choice_alter(attribute = attribute, node = node, replace = replace,
                            n1 = n1, n2 = n2, isTwoMode = isTwoMode)
}

sameX <- function(attribute, node, replace, isTwoMode = FALSE) {
  .Deprecated("same", package = "goldfish", old = "sameX")
  update_DyNAM_choice_same(attribute = attribute, node = node, replace = replace,
                           isTwoMode = isTwoMode)
}

simX <- function(attribute, node, replace, n1, n2, isTwoMode = FALSE) {
  .Deprecated("sim", package = "goldfish", old = "simX")
  update_DyNAM_choice_sim(attribute = attribute, node = node, replace = replace,
                          n1 = n1, n2 = n2, isTwoMode = isTwoMode)
}

diffX <- function(attribute, node, replace, n1, n2, isTwoMode = FALSE) {
  .Deprecated("diffCov", package = "goldfish", old = "diffX")
  update_DyNAM_choice_diff(attribute = attribute, node = node, replace = replace,
                           n1 = n1, n2 = n2, isTwoMode = isTwoMode)
}

# Defunct effects ####
oneX <- function(attribute, statistics, node, replace, n1, n2, isTwoMode = FALSE) {
  .Defunct("TBD", package = "goldfish")
  # oneCov(attribute=attribute, statistics=statistics, node=node, replace=replace)
}

meanX <- function(attribute, statistics, node, replace, n1, n2, isTwoMode = FALSE) {
  .Defunct("TBD", package = "goldfish")
  # meanCov(attribute=attribute, statistics=statistics, node=node, replace=replace)
}

meanXxdiffX <- function(attribute, statistics, node, replace, n1, n2, isTwoMode = FALSE) {
  .Defunct("TBD", package = "goldfish")
  # meanCovxdiffCov(attribute=attribute, statistics=statistics, node=node, replace=replace)
}

tieIntercept <- function(dep.var = 1, state, cache, update = 1, n1, n2, isTwoMode = FALSE) {
  .Defunct("inertia", package = "goldfish")
}

threePathDiffAttribute <- function(dep.var = 1, attribute.index, state, cache,
                                   update = 1, initialOnly = FALSE, n1, n2, isTwoMode = FALSE) {
  #   covar <- state[[attribute.index]]
  #   diffMat <- outer(covar, covar, "!=")
  #
  #   diffThreePaths <- cache[[dep.var]]$twoPaths %*% cache[[dep.var]]$binarizedNetwork * diffMat
  #
  #   if(initialOnly) {
  #     contributingTies <- getContributingTies( dep.var, state, update )
  #     net <- contributingTies * diffThreePaths
  #   } else
  #     net <- diffThreePaths
  #   diag(net) <- 0
  #   return ( sign(update) * net )
  .Defunct("TBD", package = "goldfish")
}

mixedTransitivity <- function(first.var = 1, second.var, state, cache, update = 1,
                              initialOnly = FALSE, n1, n2, isTwoMode = FALSE) {
  #   mixedTwoPath <- cache[[first.var]]$binarizedNetwork %*% cache[[second.var]]$binarizedNetwork
  #   if(initialOnly) {
  #     contributingTies <- getContributingTies( dep.var, state, update )
  #     net <- contributingTies * mixedTwoPath
  #   } else
  #     net <- mixedTwoPath
  #   diag(net) <- 0
  #   return ( sign(update) * net )
  .Defunct("TBD", package = "goldfish")
}

# # effect modeling the tendency to create (create only!) a very first tie depending on a covariate
activeX <- function(dep.var = 1, state, cache, update = 1, attribute.index,
                    meanValue = NULL, n1, n2, isTwoMode = FALSE) {
  #   outIsolates <- cache[[dep.var]]$outdegree
  #   att <- state[[attribute.index]]
  #   n <- nrow( cache[[dep.var]]$binarizedNetwork )
  #   if (update > 0) {
  #     egoAttMatrix <- matrix(att, n, n, byrow = F) * outIsolates
  #     return(egoAttMatrix)
  #   } else
  #     return(0)
  .Defunct("TBD", package = "goldfish")
}

egoXaltX <- function(dep.var = 1, state, cache, update = 1, attribute.index,
                     meanValue = NULL, initialOnly = FALSE, n1, n2, isTwoMode = FALSE) {
  #   if(initialOnly) {
  #     contributingTies <- getContributingTies( dep.var, state, update )
  #     net <- contributingTies
  #   } else
  #     net <- 1
  #   att <- state[[attribute.index]]
  #   if(is.null(meanValue)) meanValue <- mean(att, na.rm = T)
  #   sign(update) * net * ( (att-meanValue) %*% t(att-meanValue) )
  .Defunct("TBD", package = "goldfish")
}

sumCovariateEffects <- function(dep.var = 1, state, cache, update = 1,
                                attribute.index, meanValue = NULL, initialOnly = FALSE,
                                effectName1, effectName2, n1, n2, isTwoMode = FALSE) {
  #   sum <- eval(call(effectName1, dep.var, state, cache, update, attribute.index, meanValue, initialOnly)) +
  #     eval(call(effectName2, dep.var, state, cache, update, attribute.index, meanValue, initialOnly))
  #   return(sum)
  .Defunct("TBD", package = "goldfish")
}

outInAssortativity <- function(dep.var = 1, state, cache, update = 1, exponent = 0.5,
                               initialOnly = FALSE, n1, n2, isTwoMode = FALSE) {
  #   if(initialOnly) {
  #     contributingTies <- getContributingTies( dep.var, state, update )
  #     net <- contributingTies * outer( (cache[[dep.var]]$outdegree)^exponent,
  #                                     (cache[[dep.var]]$indegree)^exponent, "*" )
  #   } else
  #     net <- outer( (cache[[dep.var]]$outdegree)^exponent,  (cache[[dep.var]]$indegree)^exponent, "*" )
  #   diag(net) <- 0
  #   return ( sign(update) * net )
  .Defunct("TBD", package = "goldfish")
}

outIsolate <- function(dep.var = 1, state, cache, update = 1, n1, n2,
                       isTwoMode = FALSE) {
  # #   contributingTies <- getContributingTies( dep.var, state, update )
  # #   n <- length(cache[[dep.var]]$outIsolates)
  # #   net <- contributingTies * matrix( cache[[dep.var]]$outIsolates, byrow = TRUE, n, n)
  # #   diag(net) <- 0
  # #   return ( sign(update) * net )
  .Defunct("TBD", package = "goldfish")
}

isIsolate <- function(dep.var = 1, state, cache, update = 1, initialOnly = NULL,
                      n1, n2, isTwoMode = FALSE) {
  #   contributingTies <- getContributingTies( dep.var, state, update ) # Get ties that are available to change
  #   n <- length(cache[[dep.var]]$isIsolate) #
  #   if ( update > 0 ) net <- contributingTies * matrix( cache[[dep.var]]$isIsolate, byrow = TRUE, n, n)
  #   if ( update < 0 ) net <- contributingTies * matrix( rowSums(state[[dep.var]]) == 1, byrow = TRUE, n, n)
  #   diag(net) <- 0
  #   return ( sign(update) * net )
  .Defunct("TBD", package = "goldfish")
}

egoIsolate <- function(dep.var = 1, state, cache, update = 1, initialOnly = NULL,
                       n1, n2, isTwoMode = FALSE) {
  #   contributingTies <- getContributingTies( dep.var, state, update ) # Get ties that are available to change
  #   n <- length(cache[[dep.var]]$isIsolate) #
  #   if ( update > 0 ) net <- contributingTies * matrix( cache[[dep.var]]$isIsolate, byrow = FALSE, n, n)
  #   if ( update < 0 ) net <- contributingTies * matrix( rowSums(state[[dep.var]]) == 1, byrow = FALSE, n, n)
  #   return ( sign(update) * net )
  .Defunct("TBD", package = "goldfish")
}

outPop <- function(dep.var = 1, state, cache, update = 1, initialOnly = FALSE,
                   exponent = 1, n1, n2, isTwoMode = FALSE) {
  #   if(initialOnly) {
  #     contributingTies <- getContributingTies( dep.var, state, update ) # Get ties that are available to change
  #     net <- t( t(contributingTies) * cache[[dep.var]]$outdegree )
  #   } else
  #     net <- rep(1, length(cache[[dep.var]]$outdegree)) %*% t(cache[[dep.var]]$outdegree)
  #   net <- net^exponent
  #   diag(net) <- 0
  #   return ( sign(update) * net )
  .Defunct("TBD", package = "goldfish")
}

inPopOutActSum <- function(dep.var = 1, state, cache, update = 1,
                           initialOnly = FALSE, n1, n2, isTwoMode = FALSE) {
  #   sum <- inPop(dep.var, state, cache, update, initialOnly) +
  #     outAct(dep.var, state, cache, update, initialOnly)
  #   return(sum)
  .Defunct("TBD", package = "goldfish")
}

# # experimental sum effect. Only works for structural effects by now
sumStructuralEffects <- function(dep.var = 1, state, cache, update = 1,
                                 initialOnly = FALSE, effectName1, effectName2,
                                 n1, n2, isTwoMode = FALSE) {
  #   sum <- eval(call(effectName1, dep.var, state, cache, update, initialOnly)) +
  #     eval(call(effectName2, dep.var, state, cache, update, initialOnly))
  #   return(sum)
  .Defunct("TBD", package = "goldfish")
}
