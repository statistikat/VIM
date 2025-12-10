library(VIM)
set.seed(104)
x <- rnorm(100)
df <- data.frame(
  y = x + rnorm(100, sd = .01),
  x = x,
  fac = as.factor(x >= 0),
  facM = as.factor(abs(round(x)))
)

max_dist <- function(x, y) {
  max(abs(x - y))
}

df$y[1:3] <- NA
df$fac[3:5] <- NA
df$facM[3:5] <- NA
df$binNum <- as.integer(df$fac)+17
df$binInt <- as.integer(df$fac)+17L
# xgboostImpute accuracy", {
  df.out <- xgboostImpute(y ~ x, df)
  expect_true(
    max_dist(df.out$y, df$x)<
    0.06
  )
  
  # xgboostImpute should do nothing for no missings", {
  df.out <- xgboostImpute(x ~ y, df)
  expect_identical(df.out$x, df$x)

  # two-level factor response predicted accurately", {
  set.seed(1)
  df.out <- xgboostImpute(fac ~ x, df)
  expect_identical(df.out$fac, as.factor(df$x >= 0))
  
  # three-level factor response predicted accurately", {
  set.seed(1)
  df.out <- xgboostImpute(facM ~ x, df)
  expect_identical(df.out$facM, as.factor(abs(round(df$x))))
 
  
  # interger binary response predicted accurately", {
  expect_warning(df.out <- xgboostImpute(binInt ~ x, df))
  expect_identical(df.out$binInt==19, df$x >= 0)
  # 
  # numeric binary response predicted accurately", {
  expect_warning(df.out <- xgboostImpute(binNum ~ x, df))
  expect_identical(df.out$binNum==19, df$x >= 0)
  # 
# factor regressor used reasonably", {
  df2 <- df
  df2$x[1:10] <- NA
  df.out <- xgboostImpute(x ~ fac, df2)
  expect_identical(as.factor(df.out$x >= 0), df$fac)
  # with verbose enabled.
  df.out <- xgboostImpute(x ~ fac, df2, verbose = TRUE)
  expect_identical(as.factor(df.out$x >= 0), df$fac)
# 

