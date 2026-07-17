library(VIM)

## Regression test for imputeCellReg cell weights (Wave 1, audit P1).
## The crm engine set cell_flags <- abs(crm cellwiseoutliers) -- continuous
## deviation magnitudes -- but the downstream weight update treats them as a
## binary flag via 1 - cell_flags, so large deviations produced NEGATIVE cell
## weights (observed as low as ~-10). Cell weights must stay in [0, 1].

if (!requireNamespace("crmReg", quietly = TRUE)) {
  exit_file("crmReg not available")
}

set.seed(1)
n <- 100
X <- matrix(rnorm(n * 4), n, 4)
y <- X %*% c(1, -1, 0.5, 0.8) + rnorm(n, 0, 0.3)
d <- data.frame(X1 = X[, 1], X2 = X[, 2], X3 = X[, 3], X4 = X[, 4],
                Y = as.numeric(y))
set.seed(2)
for (cc in 1:4) {
  idx <- sample(n, 8)
  d[idx, cc] <- d[idx, cc] + rnorm(8, 10, 1)   # scattered cellwise outliers
}
d$Y[c(3, 9, 17, 25, 40)] <- NA

res <- imputeCellReg(d, engine = "crm")
W <- res$cellweights

expect_true(all(W >= 0 & W <= 1),
            info = sprintf("cell-weight range [%.2f, %.2f]", min(W), max(W)))
expect_equal(sum(is.na(res$data_imputed)), 0L)
