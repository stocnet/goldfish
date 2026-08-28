# Rebuild every vignette, and README.md with them.
#
# Split out of `precompile.R` so that sourcing a file of definitions cannot
# render anything, and so this rebuild is a named act rather than a side
# effect. It rewrites every `vignettes/*.Rmd`, their `.R` purls, their figure
# directories, and `README.md`.
#
# INSTALL FIRST. The vignettes knit against the installed goldfish, so the
# caller runs `devtools::install()` before this. No check enforces it here --
# the responsibility is the caller's, deliberately, so that rendering against a
# deliberately chosen released version stays possible.
#
#   Rscript -e 'devtools::install()' && Rscript vignettes/rebuild-all.R
#
# Run from the package root.

source("vignettes/precompile.R")

precompile()

print(check_rendered_errors())

rmarkdown::render(
  "README.Rmd",
  output_format = "md_document",
  envir = globalenv()
)
