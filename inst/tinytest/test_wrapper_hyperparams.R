library(VIM)

## Regression test for wrapper hyperparameter passthrough (Wave 1, audit P1).
## rangerImpute() / xgboostImpute() documented hyperparameters (num.trees,
## nrounds, objective, ...) but warned they were ignored and passed only
## predict_median to vimpute(). They now forward hyperparameters via
## learner_params, so the parameters actually take effect (an ignored parameter
## would leave the imputations identical to the default).

set.seed(1)
d <- data.frame(a = rnorm(60), b = rnorm(60))
d$c <- 1.2 * d$a - 0.8 * d$b + rnorm(60, 0, 0.5)
mi <- c(5, 12, 20, 33, 48)
d$c[mi] <- NA

## rangerImpute: num.trees must take effect and must not warn "ignored"
w <- character(0)
set.seed(1)
r1 <- withCallingHandlers(
  rangerImpute(c ~ a + b, d, num.trees = 1),
  warning = function(cc) { w[[length(w) + 1L]] <<- conditionMessage(cc); invokeRestart("muffleWarning") }
)
set.seed(1)
r300 <- rangerImpute(c ~ a + b, d, num.trees = 300)
expect_false(any(grepl("ignored", w)), info = "rangerImpute warned that args are ignored")
expect_true(any(r1$c[mi] != r300$c[mi]), info = "num.trees had no effect (ignored)")

## xgboostImpute: nrounds must take effect
set.seed(1)
x1 <- xgboostImpute(c ~ a + b, d, nrounds = 1)
set.seed(1)
x100 <- xgboostImpute(c ~ a + b, d, nrounds = 100)
expect_true(any(x1$c[mi] != x100$c[mi]), info = "nrounds had no effect (ignored)")
