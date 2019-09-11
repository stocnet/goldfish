.onLoad <- function(libname, pkgname) {
  packageStartupMessage("  <\")))><  The Goldfish package in R\n")
  packageStartupMessage(
    pkgname, ": version ", packageVersion("goldfish"),
    ", \"American Shubunkins\", created on ",
    packageDescription("goldfish", fields = "Date"),
    "\n"
  )
  # packageStartupMessage("Please cite as:\nChristoph Stadtfeld and James Hollway (2018). \"goldfish: Statistical network models for dynamic network data\". R package version ", packageVersion("goldfish"), ", www.social-networks.ethz.ch/research/goldfish.html.\n")
  # packageStartupMessage("With contributions from: Marion Hoffman, Timon Elmer, Kieran Mepham, Mirko Reul, Xiaolei Zhang, and Per Block.")
}
