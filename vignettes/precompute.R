## Precompute the expensive vignettes
## ==================================
## `vimpute.Rmd`, `vimpute-mi.Rmd`, `vimpute-benchmark.Rmd` and
## `vimpute-coverage.Rmd` and `tabiclImpute-benchmark.Rmd` are GENERATED from the
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
available <- c("vimpute-benchmark", "vimpute-coverage", "vimpute", "vimpute-mi",
               "tabiclImpute-benchmark")
targets <- commandArgs(trailingOnly = TRUE)
if (!length(targets)) targets <- available
if (any(!targets %in% available)) stop("Unknown vignette: ", paste(setdiff(targets, available), collapse = ", "))
# An optional name precomputes just that vignette:
# Rscript vignettes/precompute.R tabiclImpute-benchmark
for (v in targets) {
  message("precomputing ", v, ".Rmd ...")
  knitr::knit(paste0(v, ".Rmd.orig"), output = paste0(v, ".Rmd"),
              envir = new.env(), quiet = TRUE)
  # Keep generated Markdown stable and free of trailing whitespace.
  output <- paste0(v, ".Rmd")
  lines <- sub("[ \t]+$", "", readLines(output))
  while (length(lines) && !nzchar(tail(lines, 1L))) lines <- head(lines, -1L)
  writeLines(lines, output)
}
