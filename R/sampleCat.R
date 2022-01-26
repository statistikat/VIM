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
  s <- summary(x)
  s <- s[s!=0]
  if(!is.null(weights)){
    tmpTab <- merge(aggregate(weights,list(x),sum), data.frame("Group.1"=names(s),prob=s))
    s <- tmpTab$prob*tmpTab$x
    names(s) <- tmpTab$Group.1
  }
  
  sample(names(s),1,prob=s)
}