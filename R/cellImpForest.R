#' Cellwise-robust imputation of mixed data with tree ensembles
#'
#' Documentation is completed in Task 7.
#' @param data a data.frame with numeric and categorical columns, NA allowed
#' @param ... further arguments (not yet used)
#' @return an object of class \code{cellImpForest}
#' @export
cellImpForest <- function(data, ...) {
  check_data(data)
  if (ncol(data) < 2L) stop("cellImpForest() needs at least two columns")
  stop("cellImpForest() is not implemented yet")
}
