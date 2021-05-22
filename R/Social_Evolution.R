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
#' @format 3 dataframes: actors (84 rows, 4 columns), calls (439 rows, 4 columns), friendship (766 rows, 4 columns).
#' See below for variables and formats.
#'
#' \tabular{lll}{
#'    \strong{Object}      \tab  \strong{Description}                      \tab    \strong{Format}       \cr
#'    actors$label         \tab   Actor identifier labels                  \tab     character            \cr
#'    actors$present       \tab   Actor present in dataset                 \tab     boolean              \cr
#'    actors$floor         \tab   Floor of residence actor lives on        \tab     numeric (1-9)        \cr
#'    actors$gradeType     \tab   Degree level                             \tab     numeric (1-5)        \cr
#'    calls$time           \tab   Time and date of call                    \tab     numeric from POSIXct \cr
#'    calls$sender         \tab   Initiator of phone call                  \tab     character            \cr
#'    calls$receiver       \tab   Recipient of phone call                  \tab     character            \cr
#'    calls$increment      \tab   Indicates call number increment (all 1s) \tab     numeric (1)          \cr
#'    friendship$time      \tab   Time and date of friend nomination       \tab     numeric from POSIXct \cr
#'    friendship$sender    \tab   Nominator of friendship                  \tab     character            \cr
#'    friendship$receiver  \tab   Nominee of friendship                    \tab     character            \cr
#'    friendship$replace   \tab   Indicates friendship value at $time      \tab     numeric              \cr
#'  }
#'
#' @source \url{http://realitycommons.media.mit.edu/socialevolution.html}
#' @references
#' A. Madan, M. Cebrian, S. Moturu, K. Farrahi, A. Pentland (2012). Sensing the 'Health State' of a Community.
#' \emph{Pervasive Computing. 11}, 4, pp. 36-45.
#'
#' @keywords datasets social evolution network
NULL

#' @rdname Social_Evolution
"actors"
#' @rdname Social_Evolution
"calls"
#' @rdname Social_Evolution
"friendship"
