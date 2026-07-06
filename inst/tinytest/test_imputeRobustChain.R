library(VIM)

## Regression tests for imputeRobustChain (Wave 1, audit P0.4).
## Before the repair, the default numeric path imputed literal zeros (the donor
## selection in the pmm branch was dead code), method = "lm" crashed on an
## undefined 'alpha', the gam path never assigned its model, and the binary /
## count paths called undefined helpers. After the repair the chain must
## recover the numeric signal (never silently impute zeros) and handle binary
## and count responses without error.

if (!requireNamespace("robustbase", quietly = TRUE)) {
  exit_file("robustbase not available")
}

set.seed(1)
n <- 120
d <- data.frame(a = rnorm(n), b = rnorm(n))
d$c <- 1.5 * d$a - d$b + rnorm(n, 0, 0.3)
truth <- d$c
mi <- c(3, 8, 15, 22, 30, 41, 55, 60, 73, 88, 95, 110)
d$c[mi] <- NA

## --- numeric: must recover the linear signal, never impute all zeros --------
for (meth in c("lts", "lm", "MM")) {
  res <- imputeRobustChain(data = d, method = meth)
  imp <- res$c[mi]
  expect_false(all(abs(imp) < 1e-8),
               info = sprintf("method=%s imputed all zeros", meth))
  expect_true(stats::cor(imp, truth[mi]) > 0.7,
              info = sprintf("method=%s cor=%.2f", meth, stats::cor(imp, truth[mi])))
}

## --- binary response: valid factor imputation, no error ---------------------
set.seed(2)
db <- data.frame(x = rnorm(n), z = rnorm(n))
lp <- 1.2 * db$x - 0.8 * db$z
db$g <- factor(ifelse(plogis(lp) > runif(n), "yes", "no"))
mb <- c(5, 12, 20, 33, 47, 61, 70, 88)
db$g[mb] <- NA
resb <- imputeRobustChain(data = db)
expect_true(all(!is.na(resb$g)), info = "binary NAs remain")
expect_true(all(as.character(resb$g[mb]) %in% c("yes", "no")),
            info = "binary imputed invalid level")

## --- count response: non-negative integers, no error ------------------------
set.seed(3)
dc <- data.frame(u = rnorm(n), v = rnorm(n))
dc$k <- rpois(n, lambda = exp(0.4 + 0.5 * dc$u - 0.3 * dc$v))
mc <- c(4, 11, 19, 28, 40, 52, 66, 80, 99)
dc$k[mc] <- NA
resc <- imputeRobustChain(data = dc)
expect_true(all(!is.na(resc$k)), info = "count NAs remain")
expect_true(all(resc$k[mc] >= 0) && all(resc$k[mc] == round(resc$k[mc])),
            info = "count imputed non-integer or negative")
