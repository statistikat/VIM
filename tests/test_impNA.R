# Simple test for makeNA and impNA
# 
# Author: alex
###############################################################################


library(VIM)
data(sleep)
newsamp <- sleep[1:10,]
newsamp[is.na(newsamp)] <- 999999
# only missing values (999999) should be imputed (e.g. in the first 10 rows of the imputed
# data set

## checking kNN
sI <- kNN(rbind(newsamp,sleep),impNA = FALSE,variable=colnames(sleep),makeNA = rep(999999,ncol(sleep)))
if(any(unlist(sI[-c(1:10),grep("_imp",colnames(sI))]))){
  stop("NAs should not be computed by kNN if impNA is set to FALSE")
}
if(any(is.na(unlist(sI[1:10,])))){
  stop("999999 should be imputed if it is set as makeNA in kNN")
}

### checking hotdeck
sI2 <- hotdeck(rbind(newsamp,sleep),impNA = FALSE,variable=colnames(sleep),makeNA = as.list(rep(999999,ncol(sleep))))
if(any(unlist(sI2[-c(1:10),grep("_imp",colnames(sI2))]))){
  stop("NAs should not be computed by kNN if impNA is set to FALSE")
}
if(any(is.na(unlist(sI2[1:10,])))||any(unlist(sI2[1:10,])==999999)){
  stop("999999 should be imputed if it is set as makeNA in kNN")
}