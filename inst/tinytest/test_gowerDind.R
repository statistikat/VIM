# gowerD and gowerDind should give the same result
# 
# Author: alex
###############################################################################
x <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))
y <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))

# gowerD and gowerDind should give the same result"
  d1 <- gowerD(x,y,weights=rep(1,3),numerical=c(1,2),factors = vector(),orders=vector(),
               mixed=vector(),levOrders=vector(),mixed.constant=0)
  minind <- apply(d1,2,which.min)
  
  minind2 <- gowerD(x,y,weights=rep(1,3),numerical=c(1,2),factors = vector(),orders=vector(),
                    mixed=vector(),levOrders=vector(),mixed.constant=0,returnIndex=TRUE,nMin=1)
  indM <- cbind(minind,as.vector(minind2$ind))
  
  expect_equal(indM[,1],indM[,2])


# gowerD and gowerDind should give the same result - iqr"
  d1 <- gowerD(x,y,weights=rep(1,3),numerical=c(1,2),factors = vector(),orders=vector(),
               mixed=vector(),levOrders=vector(),mixed.constant=0, methodStand = "iqr")
  minind <- apply(d1,2,which.min)
  
  minind2 <- gowerD(x,y,weights=rep(1,3),numerical=c(1,2),factors = vector(),orders=vector(),
                    mixed=vector(),levOrders=vector(),mixed.constant=0,returnIndex=TRUE,nMin=1,
                    methodStand = "iqr")
  indM <- cbind(minind,as.vector(minind2$ind))
  
  expect_equal(indM[,1],indM[,2])

