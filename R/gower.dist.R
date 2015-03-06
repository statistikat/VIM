## Wrapper function for gowerD
gowerD <- function(data.x, data.y = data.x, weights=NULL,numerical,factors,orders,mixed,levOrders,mixed.constant) {
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
  
  out <- .Call( "gowerD", as.matrix(data.x), as.matrix(data.y),weights[weightind],c(length(numerical),length(factors),length(orders),length(mixed)),levOrders,mixed.constant,PACKAGE="VIM")
  if(justone)
    out <-  out$delta[,1,drop=FALSE]
  else
    out <- out$delta
  out
}
