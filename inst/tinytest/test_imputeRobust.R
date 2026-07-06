library(VIM)

## Regression tests for imputeRobust() (Wave 1 tail, audit P1.11 / P1.19).
## Before the fix:
##   * method = "gamRob" crashed unconditionally -- it called robGAM(), a ghost
##     function defined nowhere in the package (masked in zzz.R);
##   * uncert = "wresid" crashed -- the pdist call was wrapped in a closure that
##     was never invoked, so the next line read an undefined `d`;
##   * an unknown uncert value crashed cryptically with "object 'ymiss' not
##     found" (no validation);
##   * the PMM donor pool was drawn from na.omit(data[, ynam]) AFTER takeAll
##     kNN-initialised the NAs, so initialised rows leaked into the pool.

data(sleep, package = "VIM")

## --- unknown uncert errors informatively (no mgcv/pdist needed) --------------
err <- tryCatch(
  imputeRobust(Sleep ~ BodyWgt + BrainWgt, sleep, method = "MM", uncert = "pnm"),
  error = function(e) conditionMessage(e)
)
expect_false(grepl("ymiss", err),
             info = "unknown uncert still crashes with the cryptic 'ymiss' error")
expect_true(grepl("uncert|should be one of|arg", err),
            info = "unknown uncert should raise an informative validation error")

## The robust-GAM and weighted-residual paths need mgcv / pdist (Suggests).
if (!requireNamespace("mgcv", quietly = TRUE) ||
    !requireNamespace("pdist", quietly = TRUE)) {
  exit_file("mgcv / pdist not available")
}

## --- method = "gamRob" imputes instead of hitting the ghost robGAM ----------
set.seed(1)
g <- imputeRobust(Sleep ~ BodyWgt + BrainWgt, sleep, method = "gamRob")
expect_true(is.data.frame(g))
expect_equal(sum(is.na(g$Sleep)), 0L)
## the first robGAM call sits outside the boot branch, so boot = FALSE broke too
set.seed(1)
g0 <- imputeRobust(Sleep ~ BodyWgt + BrainWgt, sleep, method = "gamRob", boot = FALSE)
expect_equal(sum(is.na(g0$Sleep)), 0L)
## the "robGAM" / "robGam" aliases resolve to the same method
set.seed(1)
g2 <- imputeRobust(Sleep ~ BodyWgt + BrainWgt, sleep, method = "robGAM", boot = FALSE)
expect_equal(sum(is.na(g2$Sleep)), 0L)

## --- uncert = "wresid" imputes instead of hitting the dead closure ----------
set.seed(1)
w <- imputeRobust(Sleep ~ BodyWgt + BrainWgt, sleep, method = "MM", uncert = "wresid")
expect_true(is.data.frame(w))
expect_equal(sum(is.na(w$Sleep)), 0L)

## --- PMM donor pool contract: imputed values come from observed donors ------
## PMM returns donor values, so every imputed value must be an observed y value.
## This locks in the donor-pool-is-observed-only contract after the leak fix
## (its main effect is distributional: initialised rows no longer duplicate
## donors, since kNN-init copies exact observed values).
set.seed(3)
n <- 120
df <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
df$y <- 2 + 1.5 * df$x1 - df$x2 + rnorm(n, sd = 0.3)
miss <- c(4, 11, 23, 37, 50, 68, 90, 105)
obs_y <- df$y[-miss]
df$y[miss] <- NA
set.seed(3)
p <- suppressWarnings(imputeRobust(y ~ x1 + x2, df, method = "MM", uncert = "pmm"))
expect_true(all(p$y[miss] %in% obs_y),
            info = "PMM imputed a value that is not an observed donor")
expect_equal(sum(is.na(p$y)), 0L)
