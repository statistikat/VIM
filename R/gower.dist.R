## Wrapper function for gowerD
gowerD <- function(data.x, data.y = data.x, weights=NULL,numerical,factors,orders,mixed,levOrders,mixed.constant) {
  maxplus1 <- function(x){
    x[is.na(x)] <- max(x,na.rm=TRUE)+1
    x
  }
  #weights <- rep(1,ncol(data.x))
  for(i in 1:ncol(data.x)){
    data.x[,i] <- as.numeric(data.x[,i])
    data.y[,i] <- as.numeric(data.y[,i])
  }
  weightind <- order(match(colnames(data.x),c(numerical,factors,orders,mixed)))
  data.x <- data.x[,c(numerical,factors,orders,mixed),drop=FALSE]
  data.y <- data.y[,c(numerical,factors,orders,mixed),drop=FALSE]
  
  justone <- FALSE
  if(nrow(data.y)==1){
    data.y <- rbind(data.y,data.y)
    justone <- TRUE
  }
  data.x <- apply(data.x,2,maxplus1)
  data.y <- apply(data.y,2,maxplus1)
  levOrders <- as.numeric(levOrders)
  
  out <- .Call( "gowerD", data.x, data.y,weights[weightind],c(length(numerical),length(factors),length(orders),length(mixed)),levOrders,mixed.constant,PACKAGE="VIM")
  if(justone)
    out <-  out$delta[,1,drop=FALSE]
  else
    out <- out$delta
  out
}
