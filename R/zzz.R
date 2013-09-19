vmGUIenvir <- new.env()
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("VIM is ready to use. \n Since version 4.0.0 the GUI is in its own package VIMGUI.\n
Please use the package to use the new (and old) GUI.\n") 
}