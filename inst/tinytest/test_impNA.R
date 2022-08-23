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

# checking kNN if NAs are imputed if impNA==FALSE",{
  sI <- kNN(rbind(newsamp,sleep),impNA = FALSE,variable=colnames(sleep),makeNA = rep(999999,ncol(sleep)))
  expect_false(any(unlist(sI[-c(1:10),grep("_imp",colnames(sI))])),info=
                 "NAs should not be computed by kNN if impNA is set to FALSE")
  expect_false(any(is.na(unlist(sI[1:10,]))),info=
                 "999999 should be imputed if it is set as makeNA in kNN")


### checking hotdeck
# checking hotdeck if NAs are imputed if impNA==FALSE",{
  sI2 <- hotdeck(rbind(newsamp,sleep),impNA = FALSE,variable=colnames(sleep),makeNA = as.list(rep(999999,ncol(sleep))))
  expect_false(any(unlist(sI2[-c(1:10),grep("_imp",colnames(sI2))])), info=
                 "NAs should not be computed by hotdeck if impNA is set to FALSE")
  expect_false(any(is.na(unlist(sI2[1:10,])))||any(unlist(sI2[1:10,])==999999),info=
                 "999999 should be imputed if it is set as makeNA in hotdeck")