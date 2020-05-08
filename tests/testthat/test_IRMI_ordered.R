# irmi should work on ordered factors
# 
# Author: Alexander Kowarik (Issue 11 Deleetdk)
###############################################################################
test_df = data.frame(ord = ordered(sample(c(letters[1:2], NA), size = 1000, replace = T)), v1 = rnorm(1000), v2 = rnorm(1000), m=rnorm(1000),
                     b=sample(LETTERS[1:2],1000,replace=TRUE),
                     c=sample(LETTERS[1:5],1000,replace=TRUE),co=rpois(1000,10))
test_df[sample(1000,100),"m"] <- 0
test_that("irmi base",{
  expect_warning(ir <- irmi(test_df, mixed = "m", count="co"))
  expect_is(ir,"data.frame")
})

test_that("irmi step",{
  expect_warning(ir <- irmi(test_df, mixed = "m", count="co",step=TRUE))
  expect_is(ir,"data.frame")
})

test_that("irmi step noise",{
  expect_warning(ir <- irmi(test_df, mixed = "m", count="co",step=TRUE, noise=TRUE))
  expect_is(ir,"data.frame")
})

test_that("irmi robust",{
  expect_warning(ir <- irmi(test_df,robust=TRUE,mixed = "m", count="co"))
  expect_is(ir,"data.frame")
})

test_that("irmi robust step",{
  expect_error(suppressWarnings(irmi(test_df,robust=TRUE,mixed = "m", count="co", step=TRUE)))
})

test_that("irmi robust noise",{
  expect_warning(ir <- irmi(test_df,robust=TRUE,mixed = "m", count="co", noise=TRUE))
  expect_is(ir,"data.frame")
})

test_that("irmi robust noise with no imp_var",{
  expect_warning(ir <- irmi(test_df,robust=TRUE,mixed = "m", count="co", noise=TRUE,imp_var=FALSE))
  expect_is(ir,"data.frame")
})

