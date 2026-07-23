#' Social evolution of a university dormitory cohort
#'
#' An abbreviated version of the MIT Reality Commons Social Evolution dataset,
#' spanning a reduced time period and with fewer variables. Dyadic variables
#' include binary friendships at time of survey, and time-stamped phone call
#' occurrences. Individual variables include the floor of the dormitory on
#' which the student resides, and the grade type of each student including
#' freshmen, sophomore, junior, senior, or graduate tutors.
#'
#' @name Social_Evolution
#' @docType data
#' @usage data(Social_Evolution)
#' @format 3 dataframes: actors (84 rows, 4 columns),
#'  calls (439 rows, 4 columns), friendship (766 rows, 4 columns).
#' See below for variables and formats.
#'
#' \tabular{lll}{
#'    \strong{Object} \tab \strong{Description} \tab\strong{Format} \cr
#'    actors$label \tab  Actor identifier labels \tab character \cr
#'    actors$present \tab  Actor present in dataset \tab boolean \cr
#'    actors$floor \tab  Floor of residence actor lives on
#'      \tab numeric (1-9) \cr
#'    actors$gradeType \tab  Degree level \tab numeric (1-5) \cr
#'    calls$time \tab  Time and date of call \tab numeric from POSIXct \cr
#'    calls$sender \tab  Initiator of phone call \tab character \cr
#'    calls$receiver \tab  Recipient of phone call \tab character \cr
#'    calls$increment \tab  Indicates call number increment (all 1s)
#'      \tab numeric (1) \cr
#'    friendship$time \tab  Time and date of friend nomination
#'      \tab numeric from POSIXct \cr
#'    friendship$sender \tab  Nominator of friendship \tab character \cr
#'    friendship$receiver \tab  Nominee of friendship \tab character \cr
#'    friendship$replace \tab  Indicates friendship value at $time
#'      \tab numeric \cr
#'  }
#'
#' @references
#' A. Madan, M. Cebrian, S. Moturu, K. Farrahi, A. Pentland (2012).
#' Sensing the 'Health State' of a Community.
#' \emph{Pervasive Computing. 11}, 4, pp. 36-45. \doi{10.1109/MPRV.2011.79}.
#'
#' @keywords datasets social evolution network
NULL

#' @rdname Social_Evolution
"actors"
#' @rdname Social_Evolution
"calls"
#' @rdname Social_Evolution
"friendship"

#' Social Evolution as a single stocnet object
#'
#' The [Social_Evolution] data assembled into one `stocnet` object (a plain list
#' of tibbles, as produced by `manynet::make_stocnet()`), the data shape goldfish
#' consumes directly through the `data` argument of [estimate_dynam()],
#' [estimate_rem()], and [make_specification()]. It carries two layers over a
#' single node set: `friendship`, the survey snapshots as a **panel** layer
#' (wave-timed `replace` updates), and `calls`, the phone calls as an **event**
#' layer (timestamped `increment` events). Its optional default focal layer is
#' `calls`; a model names its dependent through the formula LHS or the
#' specification `layer` regardless.
#'
#' @name social_evolution_stocnet
#' @aliases social_evolution
#' @docType data
#' @usage data(social_evolution)
#' @format A `stocnet` list of five components:
#' \describe{
#'   \item{info}{layer metadata: `name`, `layers` (`"friendship"`, `"calls"`),
#'     per-layer `update` (`friendship = "replace"`, `calls = "increment"`),
#'     `directed` (both `TRUE`), `observation` (`friendship = "panel"`,
#'     `calls = "event"`), and the optional default `focal = "calls"`.}
#'   \item{nodes}{84 actors (`label`, `active`, `floor`, `gradeType`).}
#'   \item{ties}{1205 rows (`from`, `to`, `time`, `weight`, `layer`) stacking the
#'     friendship and calls events; `from`/`to` index rows of `nodes`.}
#'   \item{changes, global}{`NULL` (no nodal or global attribute streams).}
#' }
#'
#' @seealso [Social_Evolution] for the raw data frames and the source citation;
#'   [as_goldfish()] for the validate-and-stamp boundary.
#'
#' @references
#' A. Madan, M. Cebrian, S. Moturu, K. Farrahi, A. Pentland (2012).
#' Sensing the 'Health State' of a Community.
#' \emph{Pervasive Computing. 11}, 4, pp. 36-45. \doi{10.1109/MPRV.2011.79}.
#'
#' @examples
#' # Construction workflow (how the shipped object is built from the raw frames):
#' data("Social_Evolution")
#' se <- manynet::from_ties(
#'   manynet::as_stocnet(friendship), # panel survey snapshots
#'   manynet::as_stocnet(calls), # phone-call events
#'   layer_names = c("friendship", "calls")
#' )
#' se <- manynet::join_nodes(se, actors) # bring in the actor attributes
#' # `focal` is optional -- the specification's `layer` names the dependent
#' # below. Set it (`focal = "calls"`) only to default an unnamed model.
#' se <- manynet::add_info(
#'   se,
#'   name = "Social Evolution MIT",
#'   directed = c(friendship = TRUE, calls = TRUE),
#'   observation = c(friendship = "panel", calls = "event")
#' )
#'
#' # Or just load the prebuilt object and estimate on the calls layer:
#' data("social_evolution")
#' spec <- make_specification(
#'   choice = ~ inertia + recip,
#'   model = "DyNAM",
#'   choice_sub_model = "choice",
#'   layer = "calls",
#'   data = social_evolution
#' )
#'
#' @keywords datasets social evolution network
"social_evolution"
