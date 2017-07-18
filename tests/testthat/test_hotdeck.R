# some basic tests for hotdeck
# 
# Author: alex
###############################################################################
set.seed(104)
df <- data.frame(unit_id=101:104, state=rep("NSW",4), wages01=c(NA,NA,NA,229305),r=runif(4))

set.seed(104)
df.out2 <- matchImpute(df, variable="wages01",match_var="state")

test_that("hotdeck should fill all values", {
  df.out <- hotdeck(df, variable="wages01", domain_var="state")
  expect_identical(df.out,na.omit(df.out))
})

test_that("matchImpute should fill all values", {
  expect_identical(df.out2,na.omit(df.out2))
})

test_that("hotdeck should fill all values but give a warning", {
  set.seed(104)
  expect_warning(df.out <- hotdeck(df, variable="wages01", domain_var="state",ord_var = "r"))
  expect_identical(df.out,na.omit(df.out))
})