## Precompute the expensive vignettes
## ==================================
## `vimpute.Rmd`, `vimpute-mi.Rmd`, `vimpute-benchmark.Rmd` and
## `vimpute-coverage.Rmd` are GENERATED from the
## `*.Rmd.orig` sources next to them: the R code is executed here, once, and
## its output (tables, figures under figures/) is embedded in the generated
## .Rmd, so that R CMD build/check -- on CRAN in particular, where the whole
## check must stay under 10 minutes -- only renders text. Edit the .Rmd.orig
## files, never the generated .Rmd.
##
## Re-run (from the package root, with the current VIM installed):
##
##     Rscript vignettes/precompute.R
##
## The .Rmd.orig sources and this script are .Rbuildignore'd; the generated
## .Rmd files and figures/ are what the package ships.

if (basename(getwd()) != "vignettes") setwd("vignettes")
stopifnot(file.exists("precompute.R"))
for (v in c("vimpute-benchmark", "vimpute-coverage", "vimpute", "vimpute-mi")) {
  message("precomputing ", v, ".Rmd ...")
  knitr::knit(paste0(v, ".Rmd.orig"), output = paste0(v, ".Rmd"),
              envir = new.env(), quiet = TRUE)
}
