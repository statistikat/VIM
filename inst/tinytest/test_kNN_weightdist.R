library(VIM)

## Regression test: kNN(weightDist = TRUE) must not produce NaN imputations when
## the k nearest distances exceed 1 (Wave 1 tail, audit P2.69). The aggregation
## weights were 1 - dist under the assumption dist in [0, 1]; with
## methodStand = "iqr" (or NA sentinels / mixed variables) distances exceed 1,
## so every 1 - dist <= 0, min(x[x > 0]) took a min over an empty set = Inf, the
## weights became Inf and the imputed value NaN. The guard falls back to
## rank-preserving finite positive weights when all weights are non-positive.

dd <- data.frame(x = c(1, 2, 3, 4, 100), y = c(10, 20, 30, 40, NA))
wm <- function(x, weights) stats::weighted.mean(x, weights)

## iqr scaling makes the recipient's distances >> 1 -> used to give NaN
imp_iqr <- kNN(dd, variable = "y", dist_var = "x", k = 2, numFun = wm,
               weightDist = TRUE, methodStand = "iqr", imp_var = FALSE)$y[5]
expect_false(is.nan(imp_iqr), info = "weightDist + iqr still yields NaN")
expect_true(is.finite(imp_iqr))

## range mode (distances in [0, 1]) must keep working unchanged
imp_range <- kNN(dd, variable = "y", dist_var = "x", k = 2, numFun = wm,
                 weightDist = TRUE, methodStand = "range", imp_var = FALSE)$y[5]
expect_true(is.finite(imp_range))
