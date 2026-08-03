.onLoad <- function(libname, pkgname) {
  register_diagnostic_reconstruct()
}

# dplyr is not a goldfish dependency, so its extension point is registered only
# if and when dplyr itself is loaded -- either already, or later through the
# load hook. The diagnostic tables need the reconstruction method for the dplyr
# verbs to demote on the same rule `[` demotes on.
register_diagnostic_reconstruct <- function() {
  register <- function(...) {
    for (class in c(
      "diagnose_outliers",
      "diagnose_changepoints",
      "margin_table"
    )) {
      registerS3method(
        "dplyr_reconstruct",
        class,
        dplyr_reconstruct_diagnostic,
        envir = asNamespace("dplyr")
      )
    }
  }
  if (isNamespaceLoaded("dplyr")) {
    register()
  }
  setHook(packageEvent("dplyr", "onLoad"), register)
}

#' @importFrom utils packageVersion packageDescription
.onAttach <- function(libname, pkgname) {
  if (!interactive()) {
    return()
  }
  base::packageStartupMessage(
    "  <\")))><  The goldfish package in R\n\n",
    pkgname, ": version ", utils::packageVersion("goldfish"), " ",
    dQuote("Butterfly Tail"), " created on ",
    utils::packageDescription("goldfish", fields = "Date"), "\n"
  )

  # packageStartupMessage(
  # "Please cite as:\nChristoph Stadtfeld and James Hollway (2018). \"goldfish:
  #  Statistical network models for dynamic network data\". R package version ",
  #  packageVersion("goldfish"),
  #   ", www.social-networks.ethz.ch/research/goldfish.html.\n")
}

# Whenever you use C++ code in your package, you need to clean up
# after yourself when your package is unloaded.
# Do this by writing a .onUnload() function that unloads the DLL:
# (http://r-pkgs.had.co.nz/src.html)
.onUnload <- function(libpath) {
  library.dynam.unload("goldfish", libpath)
}
