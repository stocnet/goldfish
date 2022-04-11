#' RFID Validity dataset
#'
#' Dataset collected at ETH Zürich by Timon Elmer and colleagues in order to
#' test the accuracy of Radio Frequency Identification (RFID) badges for
#' measuring social interactions.
#' Social interactions of 11 individuals (from the university staff) were
#' recorded with RFID badges in an informal setting. They were then compared to
#' the interactions observed by two confederates who watched the video recording
#' of the event. The RFID data went through the data processing procedure
#' detailed in the original article. See Elmer et al, 2019 for more details,
#' and the OSF platform for all details on the dataset.
#'
#' @name RFID_Validity_Study
#' @docType data
#' @usage data(RFID_Validity_Study)
#' @format 3 dataframes: \cr
#' - participants (11 rows, 7 columns): attributes of the experiment's participants\cr
#' - rfid (1011 rows, 4 columns): dyadic interactions detected by the RFID badges (after data processing)\cr
#' - video (219 rows, 4 columns): dyadic interactions detected by the video rating\cr
#' and one network:\cr
#' - known.before (11 rows, 11 columns): network of previous acquaintances\cr
#' See below for variables and formats.\cr
#'
#'\tabular{lll}{
#'   \strong{Object} \tab \strong{Description} \tab \strong{Format} \cr
#'   participants$actor   \tab  Identifier of the actor                      \tab  integer        \cr
#'   participants$label   \tab  (Anonymized) name                            \tab  Factor         \cr
#'   participants$present \tab  Presence of the actor (all actors are present) \tab  logical      \cr
#'   participants$age     \tab  Actor's age                                  \tab  integer        \cr
#'   participants$gender  \tab  Actor's gender (0: male, 1: female)          \tab  integer        \cr
#'   participants$group   \tab  Actor's group affiliation (groups have distinct ids) \tab  integer \cr
#'   participants$level   \tab  Actor's seniority (1: MSc student, 2: PhD student, 3: PostDoc, 4: Prof)
#'                                                                           \tab  integer        \cr
#'   rfid$NodeA      \tab  Identifier for the first actor                    \tab  chr            \cr
#'   rfid$NodeB      \tab  Identifier for the second actor                   \tab  chr            \cr
#'   rfid$Start      \tab  Time of the beginning of the dyadic interaction   \tab  integer        \cr
#'   rfid$End        \tab  Time of the end of the dyadic interaction         \tab  integer        \cr
#'   video$NodeA     \tab  Identifier for the first actor                    \tab  chr            \cr
#'   video$NodeB     \tab  Identifier for the second actor                   \tab  chr            \cr
#'   video$Start     \tab  Time of the beginning of the dyadic interaction   \tab  integer        \cr
#'   video$End       \tab  Time of the end of the dyadic interaction         \tab  integer        \cr
#'  }
#'
#' @source \url{https://osf.io/rrhxe/}
#' @references
#' Elmer, T., Chaitanya, K., Purwar, P., & Stadtfeld, C. (2019).
#' The validity of RFID badges measuring face-to-face interactions.
#' Behavior research methods, 1-19. \doi{10.3758/s13428-018-1180-y}
#'
#' @keywords datasets RFID validity study social interactions
NULL

#' @rdname RFID_Validity_Study
"rfid"
#' @rdname RFID_Validity_Study
"video"
#' @rdname RFID_Validity_Study
"known.before"
#' @rdname RFID_Validity_Study
"participants"
