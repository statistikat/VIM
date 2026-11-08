#' Random aggregation function for a factor variable
#' 
#' The function sampleCat samples with probabilites corresponding to the
#' occurrence of the level in the NNs. 
#' 
#' @param x factor vector 
#' @param weights numeric vector providing weights for the observations in x
#' @family imputation methods
#' @export 


sampleCat <- function(x,weights = NULL){
  #sample with probabilites corresponding to there number in the NNs
  if(!is.factor(x))
    x <- as.factor(x)
  s <- category_scores(x, weights)
  sample(names(s),1,prob=s)
}
