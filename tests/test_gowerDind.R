# gowerD and gowerDind should give the same result
# 
# Author: alex
###############################################################################
library(VIM)
x <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))
y <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))
d1 <- gowerD(x,y,weights=rep(1,3),numerical=c(1,2),factors = vector(),orders=vector(),
    mixed=vector(),levOrders=vector(),mixed.constant=0)
minind <- apply(d1,2,which.min)

minind2 <- gowerD(x,y,weights=rep(1,3),numerical=c(1,2),factors = vector(),orders=vector(),
    mixed=vector(),levOrders=vector(),mixed.constant=0,returnIndex=TRUE,nMin=1)

indM <- cbind(minind,as.vector(minind2$ind))
if(length(which(indM[,1]!=indM[,2]))>0){
  stop("gowerD and gowerDindex gives different results")
}