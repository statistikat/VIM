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
  if(length(numerical)>0){
    ##Datensatz durch Range dividieren
    rmin <- apply(rbind(apply(data.x[,numerical,drop=FALSE],2,min,na.rm=TRUE),apply(data.y[,numerical,drop=FALSE],2,min,na.rm=TRUE)),2,min,na.rm=TRUE)
    rmax <- apply(rbind(apply(data.x[,numerical,drop=FALSE],2,max,na.rm=TRUE),apply(data.y[,numerical,drop=FALSE],2,max,na.rm=TRUE)),2,max,na.rm=TRUE)
    r <- rmax-rmin
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
