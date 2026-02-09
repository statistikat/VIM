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

# rangerImpute accuracy", {
  set.seed(1)
  df.out <- rangerImpute(y ~ x, data= df)
  expect_true(
    max_dist(df.out$y, df$x) <
      0.06
  )
# 

# rangerImpute should do nothing for no missings", {
  set.seed(1)
  df.out <- rangerImpute(x ~ y, df)
  expect_identical(df.out$x, df$x)
# 

# rangerImpute should warn that additional ranger args are ignored", {
  set.seed(1)
  expect_warning(
    rangerImpute(y ~ x, df, median = TRUE, num.trees = 10),
    "Additional ranger arguments are ignored"
  )
# 

# median aggregation can change ranger predictions", {
  set.seed(1)
  df.out <- rangerImpute(y ~ x, df)
  set.seed(1)
  df.out2 <- rangerImpute(y ~ x, df, median = TRUE)
  expect_true(max_dist(df.out$y, df.out2$y) > 1e-6)
# 

# factor response predicted accurately", {
  set.seed(1)
  df.out <- rangerImpute(fac ~ x, df)
  expect_identical(df.out$fac, as.factor(df$x >= 0))
# 

# factor regressor used reasonably", {
  set.seed(1)
  df2 <- df
  df2$x[1:10] <- NA
  idx <- !is.na(df$fac)
  df.out <- rangerImpute(x ~ fac, df2)
  expect_identical(as.factor(df.out$x[idx] >= 0), df$fac[idx])
# 

# rangerImpute supports custom imp_suffix and updates existing indicator columns", {
  set.seed(1)
  df3 <- df
  df3$y_custom <- 1L
  expect_warning(
    out <- rangerImpute(y ~ x, df3, imp_var = TRUE, imp_suffix = "custom"),
    "will be updated"
  )
  expect_true(is.logical(out$y_custom))
  expect_identical(out$y_custom, is.na(df3$y))
# 

# rangerImpute adds indicators for multiple targets", {
  set.seed(1)
  out <- rangerImpute(y + fac ~ x, df, imp_var = TRUE, imp_suffix = "flag")

  expect_true(all(c("y_flag", "fac_flag") %in% names(out)))
  expect_identical(out$y_flag, is.na(df$y))
  expect_identical(out$fac_flag, is.na(df$fac))
# 
