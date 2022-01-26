#' Aggregation function for a ordinal variable
#'
#' The function medianSamp chooses the level as the median or randomly between
#' two levels.
#'
#' @param x ordered factor vector
#' @param weights numeric vector providing weights for the observations in x
#' @family imputation methods
#' @export


medianSamp <- function(x,weights = NULL){
  if(length(x)==1){
    return(x)
  }
  if(is.null(weights)){
    m <- median(as.numeric(x))
  }else{
    m <- weightedMedian(as.numeric(x), weights = weights)
  }
  mi <- floor(m)
  
  return(levels(x)[mi+sample(c(0,1), size = 1, prob = c(1-(m-mi),m-mi))])
}
