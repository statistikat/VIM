# irmi should work on data_frame
# 
# Author: Alexander Kowarik
###############################################################################
# sourceD("/Users/alex/git/VIM/R/")
library(dplyr)
library(VIM)

test_data = data_frame(a = c(1, 2, 3, NA, 5), b = c(2, 1, NA, 5, 5), c = c(NA, 2, 1, 4, 3))
identical(as.data.frame(irmi(test_data,noise=FALSE)),irmi(as.data.frame(test_data),noise=FALSE))
identical(as.data.frame(kNN(test_data,addRandom=FALSE)),kNN(as.data.frame(test_data),addRandom=FALSE))
identical(as.data.frame(hotdeck(test_data,ord_var="a")),hotdeck(as.data.frame(test_data),ord_var="a"))

