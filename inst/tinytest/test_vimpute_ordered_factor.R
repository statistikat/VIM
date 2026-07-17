library(VIM)

## Regression test: ordered-factor columns must survive imputation as ordered,
## with their original level order preserved (Wave 1 tail, audit).
##
## precheck() coerces ordered -> factor(as.character(.)) internally, which both
## drops the `ordered` class and alphabetises the levels. Before the fix:
##   * m = 1 returned the column as a plain (unordered) factor (level order was
##     still correct, because factor_levels is captured before precheck);
##   * m > 1 (vimmi) additionally lost the level ORDER -- complete() returned
##     the column with alphabetical levels (e.g. high, low, mid).
## Both paths must now restore the input's ordered class and level order.

set.seed(1)
n <- 60
df <- data.frame(
  x = rnorm(n),
  g = factor(sample(c("low", "mid", "high"), n, replace = TRUE),
             levels = c("low", "mid", "high"), ordered = TRUE)
)
df$g[c(3, 10, 25, 40)] <- NA

## --- m = 1: ordered class and level order preserved -------------------------
set.seed(1)
out1 <- vimpute(df, method = "ranger", sequential = FALSE, verbose = FALSE)
expect_true(is.ordered(out1$g),
            info = "m=1: imputed ordered factor came back unordered")
expect_equal(levels(out1$g), c("low", "mid", "high"))
expect_equal(sum(is.na(out1$g)), 0L)

## --- m > 1: complete() datasets keep ordered class and level order ----------
set.seed(1)
out2 <- vimpute(df, method = "ranger", sequential = FALSE, m = 3,
                boot = TRUE, uncert = "normalerror", verbose = FALSE)
c1 <- complete(out2, 1)
expect_true(is.ordered(c1$g),
            info = "m>1: complete() lost the ordered class")
expect_equal(levels(c1$g), c("low", "mid", "high"))
expect_equal(sum(is.na(c1$g)), 0L)

## a complete (NA-free) ordered column must also stay ordered on output
set.seed(2)
df2 <- data.frame(
  x = rnorm(40),
  h = factor(sample(c("a", "b", "c"), 40, replace = TRUE),
             levels = c("a", "b", "c"), ordered = TRUE)
)
df2$x[1:3] <- NA
out3 <- vimpute(df2, method = "ranger", sequential = FALSE, verbose = FALSE)
expect_true(is.ordered(out3$h),
            info = "untouched ordered column was flattened to plain factor")
expect_equal(levels(out3$h), c("a", "b", "c"))
