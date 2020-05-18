#' @importFrom utils packageVersion packageDescription
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("  <\")))><  The Goldfish package in R\n")
  packageStartupMessage(
    pkgname, ": version ", utils::packageVersion("goldfish"),
    ", \"Bristol Shubunkins\", created on ",
    utils::packageDescription("goldfish", fields = "Date"),
    "\n"
  )
  # packageStartupMessage("Please cite as:\nChristoph Stadtfeld and James Hollway (2018). \"goldfish:
  #   Statistical network models for dynamic network data\". R package version ", packageVersion("goldfish"),
  #   ", www.social-networks.ethz.ch/research/goldfish.html.\n")
}
