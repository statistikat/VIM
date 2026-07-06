library(VIM)

## Regression test for the cellIRWLS engine (Wave 1, audit P0.3).
## The cell-weighted IRWLS previously folded cell weights into the design
## VALUES (X_tilde = sqrt(w_row) * w_cell * X) while predicting from the
## UNWEIGHTED design, so imputeCellIRMI recovered no regression signal and
## imputed WORSE than unconditional median imputation even on clean data.
## After the fix (cell weights enter the loss as row reliability; the design
## stays X, consistent with the prediction cbind(1, X) %*% beta) the engine
## must recover the linear signal on clean data.

if (!requireNamespace("cellWise", quietly = TRUE) ||
    !requireNamespace("robustbase", quietly = TRUE)) {
  exit_file("cellWise/robustbase not available")
}

set.seed(42)
n <- 200
z <- matrix(rnorm(n * 4), n, 4)
A <- matrix(c(1.0, 0.0, 0.0, 0.0,
              0.7, 0.7, 0.0, 0.0,
              0.5, 0.3, 0.8, 0.0,
              0.4, 0.4, 0.4, 0.6), 4, 4, byrow = TRUE)
X <- z %*% t(A)
beta <- c(1.5, -1.0, 0.8, 1.2)
y <- as.numeric(X %*% beta + rnorm(n, 0, 0.4))
dat <- data.frame(X1 = X[, 1], X2 = X[, 2], X3 = X[, 3], X4 = X[, 4], Y = y)

truth <- dat$Y
miss <- seq(2L, n, by = 5L)            # deterministic ~40 missing Y cells
dat$Y[miss] <- NA

imp <- imputeCellIRMI(dat, uncert = "none")$data_imputed
rmse_irmi <- sqrt(mean((imp$Y[miss] - truth[miss])^2))

med <- stats::median(truth[-miss])
rmse_med <- sqrt(mean((med - truth[miss])^2))

## On clean, strongly-linear data the engine must recover the signal and beat
## unconditional median imputation by a wide margin (regression residual sd is
## 0.4; sd(y) is ~3, so a working engine lands far below the median baseline).
expect_true(rmse_irmi < 0.6 * rmse_med,
            info = sprintf("cellIRMI RMSE=%.3f vs median RMSE=%.3f", rmse_irmi, rmse_med))

## Under scattered cellwise contamination of the predictors the engine must
## still recover the signal and beat median imputation (the old engine imputed
## ~2x worse than plain irmi here).
set.seed(7)
Xc <- matrix(rnorm(n * 4), n, 4) %*% t(A)
yc <- as.numeric(Xc %*% beta + rnorm(n, 0, 0.4))
datc <- data.frame(X1 = Xc[, 1], X2 = Xc[, 2], X3 = Xc[, 3], X4 = Xc[, 4], Y = yc)
truthc <- datc$Y
set.seed(9)
for (cc in 1:4) {
  idx <- sample(n, round(0.1 * n))
  datc[idx, cc] <- datc[idx, cc] + rnorm(length(idx), 8, 1)   # ~10% wild cells
}
datc$Y[miss] <- NA
impc <- imputeCellIRMI(datc, uncert = "none")$data_imputed
rmse_c <- sqrt(mean((impc$Y[miss] - truthc[miss])^2))
rmse_medc <- sqrt(mean((stats::median(truthc[-miss]) - truthc[miss])^2))
expect_true(rmse_c < 0.6 * rmse_medc,
            info = sprintf("contam cellIRMI RMSE=%.3f vs median RMSE=%.3f", rmse_c, rmse_medc))
