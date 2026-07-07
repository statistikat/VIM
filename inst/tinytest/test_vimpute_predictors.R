library(VIM)

## Wave 2: per-variable predictor control + visit sequence (audit P2.55/P1.28).
## Predictor restriction previously existed only via `formula`, which is
## hard-rejected for ranger/xgboost -- the default learners had NO equivalent of
## mice's predictorMatrix. New `predictors` argument: a named list
## (target -> character vector of predictors) or a mice-style 0/1 matrix
## (rows = targets, columns = predictors), applied uniformly to every learner.
## New `visit_sequence`: "asis" (default), "increasing.na", "decreasing.na", or
## an explicit permutation of the NA-variables.

set.seed(1)
n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)                    # pure noise, unrelated to y
d <- data.frame(x1 = x1, x2 = x2, y = 5 * x1 + rnorm(n, sd = 0.2))
miss <- sample(n, 30)
truth <- d$y[miss]
d$y[miss] <- NA

rmse <- function(x) sqrt(mean((x - truth)^2))

## --- restriction is effective: x1-only beats x2-only by a wide margin --------
imp_x1 <- vimpute(d, method = "ranger", sequential = FALSE, seed = 7,
                  imp_var = FALSE, verbose = FALSE,
                  predictors = list(y = "x1"))
imp_x2 <- vimpute(d, method = "ranger", sequential = FALSE, seed = 7,
                  imp_var = FALSE, verbose = FALSE,
                  predictors = list(y = "x2"))
expect_equal(sum(is.na(imp_x1$y)), 0L)
expect_equal(sum(is.na(imp_x2$y)), 0L)
expect_true(rmse(imp_x1$y[miss]) < 0.5 * rmse(imp_x2$y[miss]),
            info = "predictor restriction has no effect on the model")

## --- mice-style predictor matrix is equivalent to the list form --------------
pm <- matrix(0L, nrow = 1, ncol = 3, dimnames = list("y", c("x1", "x2", "y")))
pm["y", "x1"] <- 1L
imp_pm <- vimpute(d, method = "ranger", sequential = FALSE, seed = 7,
                  imp_var = FALSE, verbose = FALSE, predictors = pm)
expect_identical(imp_x1$y, imp_pm$y)

## --- validation ---------------------------------------------------------------
expect_error(
  vimpute(d, method = "ranger", sequential = FALSE, verbose = FALSE,
          predictors = list(nope = "x1")),
  "Unknown"
)
expect_error(
  vimpute(d, method = "ranger", sequential = FALSE, verbose = FALSE,
          predictors = list(y = "nope")),
  "Unknown"
)
expect_error(
  vimpute(d, method = "ranger", sequential = FALSE, verbose = FALSE,
          predictors = list(y = character(0))),
  "at least one predictor"
)

## --- visit_sequence: imputation order is honoured ----------------------------
set.seed(2)
d2 <- data.frame(a = rnorm(60), b = rnorm(60), cc = rnorm(60))
d2$a[sample(60, 20)] <- NA     # most missing
d2$b[sample(60, 5)] <- NA      # fewest missing
d2$cc[sample(60, 10)] <- NA

order_of <- function(...) {
  msgs <- capture.output(
    invisible(vimpute(d2, method = "ranger", sequential = FALSE,
                      imp_var = FALSE, verbose = TRUE, seed = 1, ...)),
    type = "message"
  )
  hits <- grep("Impute variable:", msgs, value = TRUE)
  unique(sub(".*Impute variable:\\s*", "", hits))
}
expect_equal(order_of(visit_sequence = "increasing.na"), c("b", "cc", "a"))
expect_equal(order_of(visit_sequence = "decreasing.na"), c("a", "cc", "b"))
expect_equal(order_of(visit_sequence = c("cc", "a", "b")), c("cc", "a", "b"))
expect_error(
  vimpute(d2, method = "ranger", sequential = FALSE, verbose = FALSE,
          visit_sequence = c("a", "b")),
  "visit_sequence"
)

## --- m > 1: both arguments are honoured on the MI path -----------------------
dmi <- d
res_mi <- vimpute(dmi, method = "ranger", sequential = FALSE, m = 2,
                  boot = TRUE, uncert = "normalerror", seed = 5,
                  imp_var = FALSE, verbose = FALSE,
                  predictors = list(y = "x1"), visit_sequence = "increasing.na")
expect_inherits(res_mi, "vimmi")
expect_equal(sum(is.na(complete(res_mi, 1)$y)), 0L)
