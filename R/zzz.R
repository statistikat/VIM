#' @import data.table
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("VIM is ready to use.\n")
  packageStartupMessage("Suggestions and bug-reports can be submitted at: https://github.com/statistikat/VIM/issues")
}

.onLoad <- function(libname, pkgname) {
  # VIM exports its own complete() S3 generic, but mice and tidyr export a
  # generic of the same name that masks it once they are attached -- which the
  # documented pooling workflow (?vimmi) tells users to do -- so complete(vimmi)
  # then failed with "no applicable method". Register the vimmi method on those
  # foreign generics whenever they are (or later become) available, so dispatch
  # works regardless of package load order.
  register_complete_vimmi <- function(...) {
    for (pkg in c("mice", "tidyr")) {
      if (isNamespaceLoaded(pkg) || requireNamespace(pkg, quietly = TRUE)) {
        try(registerS3method("complete", "vimmi",
                             get("complete.vimmi", envir = asNamespace(pkgname)),
                             envir = asNamespace(pkg)),
            silent = TRUE)
      }
    }
  }
  register_complete_vimmi()
  setHook(packageEvent("mice", "onLoad"), register_complete_vimmi)
  setHook(packageEvent("tidyr", "onLoad"), register_complete_vimmi)
}

utils::globalVariables(c(
  "alpha", "factors", "ndata", "pivotCoord", "rn", "robust",
  "supportedMethods", "useGLMpoisson", "useLogistic", "x_reg", "r", "p",
  ".cw_row_weights"
))
