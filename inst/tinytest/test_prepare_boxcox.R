library(VIM)

## Regression guard for the car dependency diet (Wave 1, audit P1).
## car was demoted Imports -> Suggests. prepare()'s Box-Cox now uses a native
## power transform (car is only needed to ESTIMATE lambda). The native transform
## must reproduce the Box-Cox formula, and supplying `powers` must work without
## needing car for estimation.

set.seed(1)
x <- matrix(abs(rnorm(30)) + 1, 15, 2)

pv <- prepare(x, transformation = "boxcox", powers = 0.5)
expect_equal(as.numeric(pv), as.numeric((x^0.5 - 1) / 0.5), tolerance = 1e-8)

## lambda == 0 branch = log
pv0 <- prepare(x, transformation = "boxcox", powers = 0)
expect_equal(as.numeric(pv0), as.numeric(log(x)), tolerance = 1e-8)

## automatic lambda estimation still works when car is available
if (requireNamespace("car", quietly = TRUE)) {
  est <- prepare(x, transformation = "boxcox")
  expect_equal(sum(is.na(est)), 0L)
}
