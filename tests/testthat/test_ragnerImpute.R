library(VIM)
set.seed(104)
x <- rnorm(100)
df <- data.frame(
  y = x + rnorm(100, sd = .01),
  x = x,
  fac = as.factor(x >= 0)
)

max_dist <- function(x, y) {
  max(abs(x - y))
}

df$y[1:3] <- NA
df$fac[3:5] <- NA

test_that("rangerImpute accuracy", {
  df.out <- rangerImpute(y ~ x, df)
  expect_lt(
    max_dist(df.out$y, df$x),
    0.06
  )
})

test_that("rangerImpute should do nothing for no missings", {
  df.out <- rangerImpute(x ~ y, df)
  expect_identical(df.out$x, df$x)
})

test_that("results form median and mean are similar", {
  df.out <- rangerImpute(y ~ x, df)
  df.out2 <- rangerImpute(y ~ x, df, median = TRUE)
  expect_lt(
    max_dist(df.out$y, df.out2$y),
    0.03
  )
})

test_that("factor response predicted accurately", {
  df.out <- rangerImpute(fac ~ x, df)
  expect_identical(df.out$fac, as.factor(df$x >= 0))
})

test_that("factor regressor used reasonably", {
  df2 <- df
  df2$x[1:10] <- NA
  df.out <- rangerImpute(x ~ fac, df2)
  expect_identical(as.factor(df.out$x >= 0), df$fac)
})

