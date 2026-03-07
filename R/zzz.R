#' @import data.table
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("VIM is ready to use.\n")
  packageStartupMessage("Suggestions and bug-reports can be submitted at: https://github.com/statistikat/VIM/issues")
}

utils::globalVariables(c(
  "alpha", "factors", "ndata", "pivotCoord", "rn", "robGAM", "robust",
  "supportedMethods", "useGLMpoisson", "useLogistic", "x_reg", "r", "p"
))
