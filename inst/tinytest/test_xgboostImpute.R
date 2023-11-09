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

# xgboostImpute accuracy", {
  df.out <- xgboostImpute(y ~ x, df)
  expect_true(
    max_dist(df.out$y, df$x)<
    0.06
  )
  
  # xgboostImpute should do nothing for no missings", {
  df.out <- xgboostImpute(x ~ y, df)
  expect_identical(df.out$x, df$x)
# 

# factor response predicted accurately", {
  df.out <- xgboostImpute(fac ~ x, df)
  df.out[df.out$fac_imp,]
  expect_identical(df.out$fac, as.factor(df$x >= 0))
# 

# factor regressor used reasonably", {
  df2 <- df
  df2$x[1:10] <- NA
  df.out <- xgboostImpute(x ~ fac, df2)
  expect_identical(as.factor(df.out$x >= 0), df$fac)
# 

