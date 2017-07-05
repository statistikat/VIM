# irmi should work on data_frame
# 
# Author: Alexander Kowarik
###############################################################################
library(dplyr)
test_data = data_frame(a = c(1, 2, 3, NA, 5), b = c(2, 1, NA, 5, 5), c = c(NA, 2, 1, 4, 3))

test_that("irmi for data_frame should give the same result as data.frame", {
  x1 <- as.data.frame(irmi(test_data,noise=FALSE))
  x2 <- irmi(as.data.frame(test_data),noise=FALSE)
  expect_equal(x1,x2)
})

test_that("hotdeck for data_frame should give the same result as data.frame", {
  x1 <- as.data.frame(hotdeck(test_data,ord_var="a"))
  x2 <- hotdeck(as.data.frame(test_data),ord_var="a")
  expect_equal(x1,x2)
})

test_that("kNN for data_frame should give the same result as data.frame", {
  x1 <- as.data.frame(kNN(test_data,addRandom=FALSE))
  x2 <- kNN(as.data.frame(test_data),addRandom=FALSE)
  expect_equal(x1,x2)
})