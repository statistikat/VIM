#' @import data.table
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("VIM is ready to use.\n")
  packageStartupMessage("Suggestions and bug-reports can be submitted at: https://github.com/statistikat/VIM/issues")
}
