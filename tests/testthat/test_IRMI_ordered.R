# irmi should work on ordered factors
# 
# Author: Alexander Kowarik (Issue 11 Deleetdk)
###############################################################################
test_df = data.frame(ord = ordered(sample(c(letters[1:2], NA), size = 1000, replace = T)), v1 = rnorm(1000), v2 = rnorm(1000))
test_that("irmi should work on orderd factors",{
  expect_is(irmi(test_df),"data.frame")
})
