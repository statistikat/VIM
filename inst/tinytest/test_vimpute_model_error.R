library(VIM)

## Wave 3 (audit P1.27): per-variable imputation-quality feedback by default
## (missForest OOBerror analogue). Every vimpute() run reports how well each
## variable's final model fits, WITHOUT ground truth:
##   numeric targets: NRMSE (RMSE / sd of the observed values)
##   factor targets:  PFC (proportion falsely classified)
## For ranger the metric is the free out-of-bag error (type "oob"); for the
## other learners it is the in-sample error, honestly labelled "insample".
## Returned as attr(result, "model_error") (m = 1) and vimmi$model_error
## (m > 1, from the first imputation), and shown by print.vimmi.

set.seed(41)
n <- 60
dd <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
dd$y <- 2 * dd$x1 + rnorm(n, sd = 0.2)
dd$g <- factor(ifelse(dd$x2 + rnorm(n, sd = 0.5) > 0, "a", "b"))
dd$y[sample(n, 8)] <- NA
dd$g[sample(n, 7)] <- NA

## --- ranger: free OOB quality ----------------------------------------------------
res <- vimpute(dd, method = "ranger", sequential = FALSE, uncert = "none",
               imp_var = FALSE, verbose = FALSE, seed = 3)
me <- attr(res, "model_error")
expect_true(is.list(me), info = "model_error attribute must be present by default")
expect_true(setequal(names(me), c("y", "g")))

expect_equal(me$y$measure, "NRMSE")
expect_equal(me$y$type, "oob")
expect_true(is.finite(me$y$value) && me$y$value > 0)
expect_true(me$y$value < 0.7,
            info = "strong signal (R2 ~ 0.99) must give a small NRMSE")

expect_equal(me$g$measure, "PFC")
expect_equal(me$g$type, "oob")
expect_true(me$g$value >= 0 && me$g$value <= 1)

## --- non-ranger learners: labelled in-sample --------------------------------------
res_rob <- suppressWarnings(
  vimpute(dd, method = "robust", sequential = FALSE, uncert = "none",
          imp_var = FALSE, verbose = FALSE, seed = 3))
me_rob <- attr(res_rob, "model_error")
expect_equal(me_rob$y$measure, "NRMSE")
expect_equal(me_rob$y$type, "insample")
expect_true(is.finite(me_rob$y$value) && me_rob$y$value < 0.7)

## --- m > 1: quality stored in the vimmi and printed -------------------------------
mi <- vimpute(dd, method = "ranger", m = 2, sequential = FALSE,
              imp_var = FALSE, verbose = FALSE, seed = 3)
expect_true(is.list(mi$model_error))
expect_equal(mi$model_error$y$measure, "NRMSE")
out <- capture.output(print(mi))
expect_true(any(grepl("NRMSE", out)),
            info = "print.vimmi must surface the per-variable model quality")
