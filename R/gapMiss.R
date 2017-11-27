#' Missing value gap statistics
#' 
#' Computes the average missing value gap of a vector. 
#' 
#' The length of each sequence of missing values (gap) in a vector is calculated and the 
#' mean gap is reported
#' 
#' @param x a numeric vector
#' @param what default is the arithmetic mean. 
#' One can include an own function that returns a vector of lenght 1 (e.g. median)
#' @author Matthias Templ based on a suggestion and draft from Huang Tian Yuan.
#' @keywords manip
#' @export
#' @return The gap statistics 
#' @examples
#' v <- rnorm(20)
#' v[3] <- NA
#' v[6:9] <- NA
#' v[13:17] <- NA
#' v
#' gapMiss(v)
#' gapMiss(v, what = median)
#' gapMiss(v, what = function(x) mean(x, trim = 0.1))
#' gapMiss(v, what = var)
gapMiss <- function(x, what = mean){
  if(!is.vector(x)) stop("x must be a vector") 
  if(!any(is.na(x))) return(0)
  stat <- rle(ifelse(is.na(x), 1, 0))
  return(what(stat$lengths[stat$values == 1]))
}
