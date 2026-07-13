#' Error performance measures
#' 
#' Various error measures evaluating the quality of imputations
#'  
#' @name evaluation
#' @rdname evaluation
#' @aliases evaluation nrmse pfc msecov msecor
#' @param x matrix or data frame
#' @param y matrix or data frame of the same size as x 
#' @param m the indicator matrix for missing cells (kept for backward
#'   compatibility; \code{where} is the documented name)
#' @param where the indicator matrix for missing cells under its documented
#'   name -- the amputed-cell mask as returned in \code{makeMissing()}'s
#'   \code{"where"} attribute. Supply either \code{m} or \code{where}, not
#'   both.
#' @param vartypes a vector of length ncol(x) specifying the variable types
#'   (\code{"numeric"} or \code{"factor"}). The default \code{"guess"} infers the
#'   types from the columns of \code{x} (numeric columns become \code{"numeric"},
#'   everything else \code{"factor"}).
#' @return the error measures value
#' @author Matthias Templ
#' @references M. Templ, A. Kowarik, P. Filzmoser (2011) Iterative stepwise
#' regression imputation using standard and robust methods.  *Computational
#' Statistics & Data Analysis*, Vol. 55, pp. 2793-2806.
#' 
#' @details This function has been mainly written for procudures 
#' that evaluate imputation or replacement of rounded zeros. The ni parameter can thus, e.g. be
#' used for expressing the number of rounded zeros.
#' @keywords manip
#' @export
#' @examples
#' data(iris)
#' iris_orig <- iris_imp <- iris
#' iris_imp$Sepal.Length[sample(1:nrow(iris), 10)] <- NA
#' iris_imp$Sepal.Width[sample(1:nrow(iris), 10)] <- NA
#' iris_imp$Species[sample(1:nrow(iris), 10)] <- NA
#' m <- is.na(iris_imp)
#' iris_imp <- kNN(iris_imp, imp_var = FALSE)
#' evaluation(iris_orig, iris_imp, m = m, vartypes = c(rep("numeric", 4), "factor"))
#' msecov(iris_orig[, 1:4], iris_imp[, 1:4])
# nrmse <- function(x, y, m){
#   return(sqrt( (sum((x[m] - y[m])^2) / sum(m)) / var(x[m])) )
# }
evaluation <- function(x, y, m, vartypes = "guess", where = NULL){
  # 'where' is the documented alias for 'm' (the amputed-cell mask, matching
  # makeMissing()'s attribute name); 'm' stays for backward compatibility.
  if (!is.null(where)) {
    if (!missing(m)) {
      stop("Supply either 'm' or 'where' (they are the same mask), not both.")
    }
    m <- where
  }
  if (length(vartypes) == 1L && identical(as.character(vartypes), "guess")) {
    # infer per-column types: numeric columns -> "numeric", everything else
    # (factor / character / logical) -> "factor".
    if (is.data.frame(x)) {
      vartypes <- vapply(x, function(col)
        if (is.numeric(col)) "numeric" else "factor", character(1))
    } else {
      vartypes <- rep(if (is.numeric(x)) "numeric" else "factor", ncol(x))
    }
  }
  err_num <- err_cat <- err_mixed <- 0
  # guard the divisions: no missing cells of a type -> no error contribution
  # (previously 0/0 = NaN when e.g. only numeric columns were amputed)
  if(any(vartypes == "numeric") && sum(m[, vartypes == "numeric"]) > 0){
    err_num <- sum((x[, vartypes == "numeric"] - y[, vartypes == "numeric"])^2) / sum(m[, vartypes == "numeric"])
  }
  if(any(vartypes == "factor") && sum(m[, vartypes == "factor"]) > 0){
    err_cat <- sum(x[, vartypes == "factor"] != y[, vartypes == "factor"]) / sum(m[, vartypes == "factor"])
  }
  results <- list("err_num" = err_num,
                  "err_cat" = err_cat,
                  "error" = err_num + err_cat + err_mixed)
  return(results)
}

#' @rdname evaluation
#' @export

nrmse <- function(x, y, m){
  return(sqrt( mean((x[m] - y[m])^2)  / var(x[m])) )
}
# nrmse <- function(x, y, m){
#   bias <- x[m] - y[m]
#   variance <- var(x[m] - y[m]) / var(x[m])
#   variance
# }

#' @rdname evaluation
#' @export

pfc <- function(x, y, m){
  return(sum(x != y) / sum(m))
}


# ced <- function(x, y, m){
#   return(robCompositions::aDist(x, y) / sum(m))
# }

#' @rdname evaluation
#' @export

msecov <- function(x, y){
  sum((cov(x) - cov(y))^2) / ncol(x)
}

#' @rdname evaluation
#' @export

msecor <- function(x, y){
  sum((cor(x) - cor(y))^2) / ncol(x)
}

