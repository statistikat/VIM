#' Environment for the GUI for Visualization and Imputation of Missing Values
#' 
#' Location were everything from package VIM and VIMGUI is stored.
#' 
#' Internal information regarding the VIM GUI is stored in the environment
#' \code{vmGUIenvir}.
#' 
#' @aliases vmGUIenvir putVm getVm existsVm rmVm
#' @param x object name
#' @param value value to be assigned to x
#' @param mode see 'exists'
#' @param ... see 'rm'
#' @author Andreas Alfons, based on an initial design by Matthias Templ,
#' modifications by Bernd Prantner
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  \emph{Journal of Advances in Data Analysis
#' and Classification}, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords multivariate hplot
#' @rdname vmGUIenvir
#' @export vmGUIenvir
vmGUIenvir <- new.env()
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("VIM is ready to use. \n Since version 4.0.0 the GUI is in its own package VIMGUI.\n
          Please use the package to use the new (and old) GUI.\n")
  packageStartupMessage("Suggestions and bug-reports can be submitted at: https://github.com/alexkowa/VIM/issues")
}

## utility functions for GUI environment

# access vmGUIenv
vmGUIenv <- function() {
  get("vmGUIenvir", envir=as.environment("package:VIM"))
}
# put in vmGUIenv
#' @rdname vmGUIenvir
#' @export putVm
putVm <- function(x, value) {
  assign(x, value, envir=vmGUIenv())
}
# put in vmGUIenv
#' @rdname vmGUIenvir
#' @export getVm
getVm <- function (x, mode="any") { 
  get(x, envir=vmGUIenv(), mode=mode, inherits=FALSE)
}
#' @rdname vmGUIenvir
#' @export existsVm
# does object exist in vmGUIenv?
existsVm <- function (x, mode="any") { 
  exists(x, envir=vmGUIenv(), mode=mode, inherits=FALSE)
}

#' @rdname vmGUIenvir
#' @export rmVm
rmVm <- function(...) {
  localRm <- function(..., envir) rm(..., envir=vmGUIenv())
  localRm(...)
}