#' Computes the extended Gower distance of two data sets
#' 
#' The function gowerD is used by kNN to compute the distances for numerical,
#' factor ordered and semi-continous variables.
#' 
#' @param data.x data frame
#' @param data.y data frame
#' @param weights numeric vector providing weights for the observations in x
#' @param numerical names of numerical variables
#' @param factors names of factor variables
#' @param orders names of ordered variables
#' @param mixed names of mixed variables
#' @param levOrders vector with number of levels for each orders variable
#' @param mixed.constant 	vector with length equal to the number of semi-continuous variables specifying the point of the semi-continuous distribution with non-zero probability
#' @param returnIndex logical if TRUE return the index of the minimum distance
#' @param nMin integer number of values with smallest distance to be returned
#' @param returnMin logical if the computed distances for the indices should be returned
#' @details returnIndex=FALSE: a numerical matrix n x m with the computed distances
#' returnIndex=TRUE: a named list with "ind" containing the requested indices and "mins" the computed distances
#' @examples
#' data(sleep)
#' # all variables used as numerical
#' gowerD(sleep) 
#' 
#' # split in numerical an 
#' gowerD(sleep, numerical = c("BodyWgt", "BrainWgt", "NonD", "Dream", "Sleep", "Span", "Gest"),
#'   orders = c("Pred","Exp","Danger"), levOrders = c(5,5,5))
#'   
#' # as before but only returning the index of the closest observation
#' gowerD(sleep, numerical = c("BodyWgt", "BrainWgt", "NonD", "Dream", "Sleep", "Span", "Gest"),
#'   orders = c("Pred","Exp","Danger"), levOrders = c(5,5,5), returnIndex = TRUE)
#' @export 

 

gowerD <- function(data.x, data.y = data.x,
                   weights = rep(1, ncol(data.x)),
                   numerical = colnames(data.x),
                   factors = vector(),
                   orders = vector(),
                   mixed = vector(),
                   levOrders  = vector(),
                   mixed.constant = rep(0, length(mixed)),
    returnIndex=FALSE,nMin=1L,returnMin=FALSE) {
  maxplus1 <- function(x){
    if(all(is.na(x)))
      return(1)
    else
      return(max(x,na.rm=TRUE)+1)
  }
  min0 <- function(x,na.rm){
    if(all(is.na(x)))
      return(0)
    else
      return(min(x,na.rm=na.rm))
  }
  max1 <- function(x,na.rm){
    if(all(is.na(x)))
      return(1)
    else
      return(max(x,na.rm=na.rm))
  }
  #weights <- rep(1,ncol(data.x))
  for(i in 1:ncol(data.x)){
    data.x[,i] <- as.numeric(data.x[,i])
    data.y[,i] <- as.numeric(data.y[,i])
  }
  weightind <- order(match(colnames(data.x),c(numerical,factors,orders,mixed)))
  data.x <- data.x[,c(numerical,factors,orders,mixed),drop=FALSE]
  data.y <- data.y[,c(numerical,factors,orders,mixed),drop=FALSE]
  if(length(numerical)>0){
    ##Datensatz durch Range dividieren
    rmin <- apply(rbind(apply(data.x[,numerical,drop=FALSE],2,min,na.rm=TRUE),apply(data.y[,numerical,drop=FALSE],2,min0,na.rm=TRUE)),2,min,na.rm=TRUE)
    rmax <- apply(rbind(apply(data.x[,numerical,drop=FALSE],2,max,na.rm=TRUE),apply(data.y[,numerical,drop=FALSE],2,max1,na.rm=TRUE)),2,max,na.rm=TRUE)
    r <- rmax-rmin
    r[r==0] <- 1
    for(i in seq_along(numerical)){
      data.x[,numerical[i]] <- data.x[,numerical[i]]/r[i]
      data.y[,numerical[i]] <- data.y[,numerical[i]]/r[i]
    }
  }
  justone <- FALSE
  if(nrow(data.y)==1){
    data.y <- rbind(data.y,data.y)
    justone <- TRUE
  }
  # Maximum + 1 for missing values
  # TODO: find better way to handle missing in distance variables
  maxplus1X <- apply(rbind(data.x,data.y),2,maxplus1)
  for(i in 1:ncol(data.x)){
    data.x[is.na(data.x[,i]),i] <- maxplus1X[i]
    data.y[is.na(data.y[,i]),i] <- maxplus1X[i]
  }
  levOrders <- as.numeric(levOrders)
  if(returnIndex){
    out <- gowerDind( as.matrix(data.x), as.matrix(data.y),weights[weightind],
        c(length(numerical),length(factors),length(orders),length(mixed)),
        levOrders,mixed.constant,nMin,as.integer(returnMin))
    out <- list(ind=out$ind,mins=out$min)
    if(justone){
      out$ind <-  out$ind[,1,drop=FALSE]
      out$mins <-  out$mins[,1,drop=FALSE]
    }
      
  }else{
    out <- gowerd(as.matrix(data.x), as.matrix(data.y),weights[weightind],
        c(length(numerical),length(factors),length(orders),length(mixed)),
        levOrders,mixed.constant)
    if(justone)
      out <-  out$delta[,1,drop=FALSE]
    else
      out <- out$delta  
  }
  return(out)
}