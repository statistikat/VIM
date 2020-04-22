#' Aggregation function for a factor variable
#'
#' The function maxCat chooses the level
#' with the most occurrences and random if the maximum is not unique.
#'
#' @param x factor vector
#' @param weights numeric vector providing weights for the observations in x
#' @export


maxCat <- function(x,weights = NULL){
  #choose cat with max prob, random if max is not unique
  is_logical <- is.logical(x)
  if(!is.factor(x))
    x <- as.factor(x)
  s <- summary(x)
  s <- s[s!=0]
  if(!is.null(weights)){
    tmpTab <- merge(aggregate(weights,list(x),sum), data.frame("Group.1"=names(s),prob=s))
    s <- tmpTab$prob*tmpTab$x
    names(s) <- tmpTab$Group.1
  }
  if(sum(s>0)>1)
    s <- sample(s)
  if (is_logical)
    as.logical(names(s)[which.max(s)])
  else
    names(s)[which.max(s)]
}
