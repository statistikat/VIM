#' Number of OpenMP threads for VIM's compiled code
#'
#' The Gower-distance code behind [gowerD()] and [kNN()] is parallelised with
#' OpenMP. This helper resolves how many threads it may use:
#' `getOption("VIM.ncores")` if set; otherwise at most 2 while the package is
#' being checked (`R CMD check --as-cran` sets `_R_CHECK_LIMIT_CORES_`, and
#' CRAN policy allows at most two cores), and OpenMP's own default otherwise.
#'
#' @return A single integer: the number of threads, or `0L` meaning "let OpenMP
#'   decide" (its default is all cores, or `OMP_NUM_THREADS` when set).
#' @keywords internal
#' @noRd
vim_ncores <- function() {
  n <- getOption("VIM.ncores", NULL)
  if (!is.null(n)) {
    n <- suppressWarnings(as.integer(n[1L]))
    if (is.na(n) || n < 1L)
      stop("option 'VIM.ncores' must be a single positive integer")
    return(n)
  }
  limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(limit) && !tolower(limit) %in% c("false", "0", "no"))
    return(2L)
  0L
}
