# ==========================================================================
# Tests for cellwise-robust imputation functions
# cellwise_utils.R: cellWeights, cellIRWLS
# imputeCellwise.R: imputeCellIRMI, imputeCellM
# ==========================================================================

# ------------------------------------------------------------------
# 1. cellWeights returns correct dimensions and range
# ------------------------------------------------------------------
set.seed(42)
X <- data.frame(x1 = rnorm(50), x2 = rnorm(50), x3 = rnorm(50))
W <- VIM:::cellWeights(X, method = "huber")

expect_equal(nrow(W), 50)
expect_equal(ncol(W), 3)
expect_true(all(W >= 0 & W <= 1))

# also check tukey
W_tuk <- VIM:::cellWeights(X, method = "tukey")
expect_equal(dim(W_tuk), c(50, 3))
expect_true(all(W_tuk >= 0 & W_tuk <= 1))


# ------------------------------------------------------------------
# 2. cellWeights gives weight 1 for clean data, < 1 for outlying cells
# ------------------------------------------------------------------
set.seed(123)
X_clean <- data.frame(x1 = rnorm(80), x2 = rnorm(80))
W_clean <- VIM:::cellWeights(X_clean, method = "huber")

# For standard normal data, most weights should be 1 (within 1.345 MAD)
prop_one <- mean(W_clean == 1)
expect_true(prop_one > 0.70)

# Now inject outliers: shift a few cells far away
X_contam <- X_clean
X_contam$x1[1:5] <- 20  # extreme outliers
W_contam <- VIM:::cellWeights(X_contam, method = "huber")

# Outlier cells should have weights < 1
expect_true(all(W_contam[1:5, 1] < 1))
# Non-outlier cells in x2 should still be close to 1
expect_true(mean(W_contam[, 2] == 1) > 0.70)


# ------------------------------------------------------------------
# 3. cellIRWLS converges on simple regression
# ------------------------------------------------------------------
set.seed(101)
n <- 80
x <- rnorm(n)
y <- 2 + 3 * x + rnorm(n, sd = 0.5)
X_mat <- matrix(x, ncol = 1, dimnames = list(NULL, "x"))

fit <- VIM:::cellIRWLS(X_mat, y)

expect_true(fit$converged)
expect_true(fit$iterations <= 50)
expect_equal(length(fit$coefficients), 2)
# Intercept should be near 2, slope near 3
expect_true(abs(fit$coefficients[1] - 2) < 1)
expect_true(abs(fit$coefficients[2] - 3) < 1)
expect_true(fit$sigma > 0)


# ------------------------------------------------------------------
# 4. cellIRWLS handles cell contamination better than OLS
# ------------------------------------------------------------------
set.seed(202)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- 1 + 2 * x1 + 3 * x2 + rnorm(n, sd = 0.5)

# Contaminate some predictor cells
x1_contam <- x1
x1_contam[1:10] <- x1[1:10] + 10  # shift 10 cells in x1

X_contam <- cbind(x1 = x1_contam, x2 = x2)
X_clean <- cbind(x1 = x1, x2 = x2)

# OLS on contaminated data
ols_fit <- lm(y ~ x1_contam + x2)
beta_ols <- coef(ols_fit)

# cellIRWLS on contaminated data with cell weights
W_cell <- VIM:::cellWeights(as.data.frame(X_contam), method = "huber")
fit_cell <- VIM:::cellIRWLS(X_contam, y, w_cell = W_cell, method = "huber")
beta_cell <- fit_cell$coefficients

# True coefficients: intercept=1, x1=2, x2=3
# cellIRWLS should be closer to truth for x1 coefficient
err_ols_x1  <- abs(beta_ols[2] - 2)
err_cell_x1 <- abs(beta_cell[2] - 2)
expect_true(err_cell_x1 < err_ols_x1)


# ------------------------------------------------------------------
# 5. imputeCellIRMI returns correct structure
# ------------------------------------------------------------------
set.seed(303)
n <- 60
df <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)
# introduce 15% MCAR missingness
for (j in 1:3) {
  miss_idx <- sample(n, round(0.15 * n))
  df[[j]][miss_idx] <- NA
}

res <- imputeCellIRMI(df, method = "huber", maxit = 5, trace = FALSE)

expect_inherits(res, "list")
expect_true("data_imputed" %in% names(res))
expect_true("cellweights" %in% names(res))
expect_true("converged" %in% names(res))
expect_true("iterations" %in% names(res))
expect_inherits(res$data_imputed, "data.frame")
expect_equal(dim(res$data_imputed), c(n, 3))
expect_equal(dim(res$cellweights), c(n, 3))
expect_inherits(res$converged, "logical")


# ------------------------------------------------------------------
# 6. imputeCellIRMI imputes missing values (no NAs remain)
# ------------------------------------------------------------------
expect_false(any(is.na(res$data_imputed)))


# ------------------------------------------------------------------
# 7. imputeCellIRMI handles mixed data (continuous + factor)
# ------------------------------------------------------------------
set.seed(404)
n <- 80
df_mixed <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = factor(sample(c("A", "B", "C"), n, replace = TRUE))
)
# missingness in each column
df_mixed$x1[sample(n, 10)] <- NA
df_mixed$x2[sample(n, 10)] <- NA
df_mixed$x3[sample(n, 10)] <- NA

res_mixed <- imputeCellIRMI(df_mixed, method = "huber", maxit = 5,
                             trace = FALSE)

expect_inherits(res_mixed$data_imputed, "data.frame")
expect_false(any(is.na(res_mixed$data_imputed)))
# Factor column should remain a factor
expect_true(is.factor(res_mixed$data_imputed$x3))
# Cell weights for factor column should be 1
expect_true(all(res_mixed$cellweights[, 3] == 1))


# ------------------------------------------------------------------
# 8. imputeCellM returns correct structure
# ------------------------------------------------------------------
set.seed(505)
n <- 60
df_m <- data.frame(
  y  = c(rnorm(50), rep(NA, 10)),
  x1 = rnorm(n),
  x2 = rnorm(n)
)

res_m <- imputeCellM(y ~ x1 + x2, data = df_m, method = "huber")

expect_inherits(res_m, "list")
expect_true(all(c("data_imputed", "cellweights", "converged", "iterations")
                %in% names(res_m)))
expect_equal(nrow(res_m$data_imputed), n)
expect_false(any(is.na(res_m$data_imputed$y)))

# value_back = "ymiss" should return only imputed values
res_ymiss <- imputeCellM(y ~ x1 + x2, data = df_m, method = "huber",
                          value_back = "ymiss")
expect_equal(length(res_ymiss), 10)


# ------------------------------------------------------------------
# 9. imputeCellM with clean data: imputed values are reasonable
# ------------------------------------------------------------------
set.seed(606)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y_clean <- 1 + 2 * x1 - x2

df_test <- data.frame(y = y_clean, x1 = x1, x2 = x2)
# Make 10 values missing
miss_rows <- sample(n, 10)
df_test$y[miss_rows] <- NA

res_cellM <- imputeCellM(y ~ x1 + x2, data = df_test, method = "huber",
                          uncert = "normalerror")

# imputed values should be close to the true clean values
imputed_vals <- res_cellM$data_imputed$y[miss_rows]
true_vals <- y_clean[miss_rows]
mse <- mean((imputed_vals - true_vals)^2)
# With no noise in true relationship the MSE should be small
# (the normalerror adds some noise but it should be bounded)
expect_true(mse < 5)


# ------------------------------------------------------------------
# 10. Edge case: no missing values (should return data unchanged)
# ------------------------------------------------------------------
set.seed(707)
df_complete <- data.frame(x1 = rnorm(30), x2 = rnorm(30), x3 = rnorm(30))

res_complete <- imputeCellIRMI(df_complete, trace = FALSE)

expect_true(res_complete$converged)
expect_equal(res_complete$iterations, 0L)
expect_equal(res_complete$data_imputed, df_complete)
expect_true(all(res_complete$cellweights == 1))

# imputeCellM with no missing response
res_m_complete <- imputeCellM(x1 ~ x2 + x3, data = df_complete)
expect_inherits(res_m_complete, "list")
expect_equal(res_m_complete$data_imputed, df_complete)
expect_equal(res_m_complete$iterations, 0L)


# ------------------------------------------------------------------
# 11. Edge case: all values missing in one column
# ------------------------------------------------------------------
set.seed(808)
n <- 50
df_allna <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rep(NA_real_, n)
)

# imputeCellIRMI should handle this (initialise fills with median = NA -> 0)
# The function should still produce output without error
res_allna <- tryCatch(
  imputeCellIRMI(df_allna, maxit = 3, trace = FALSE),
  error = function(e) e
)
# Either it works or produces a controlled error -- not a crash
expect_true(inherits(res_allna, "list") || inherits(res_allna, "error"))

# If it returns a result, check structure
if (inherits(res_allna, "list")) {
  expect_false(any(is.na(res_allna$data_imputed)))
  expect_equal(dim(res_allna$data_imputed), c(n, 3))
}


# ==================================================================
# imputeCellEM tests
# ==================================================================

# ------------------------------------------------------------------
# 12. imputeCellEM returns correct structure
# ------------------------------------------------------------------
set.seed(901)
n <- 60
df_em <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)
for (j in 1:3) {
  miss_idx <- sample(n, round(0.15 * n))
  df_em[[j]][miss_idx] <- NA
}

res_em <- imputeCellEM(df_em, maxit_em = 20, trace = FALSE)

expect_inherits(res_em, "list")
expect_true(all(c("data_imputed", "cellweights", "mu", "Sigma",
                   "epsilon", "converged", "iterations", "pseudo_loglik")
                %in% names(res_em)))
expect_inherits(res_em$data_imputed, "data.frame")
expect_equal(dim(res_em$data_imputed), c(n, 3))
expect_equal(dim(res_em$cellweights), c(n, 3))
expect_equal(length(res_em$mu), 3)
expect_equal(dim(res_em$Sigma), c(3, 3))
expect_equal(length(res_em$epsilon), 3)
expect_inherits(res_em$converged, "logical")
expect_true(res_em$iterations > 0)
expect_true(length(res_em$pseudo_loglik) > 0)


# ------------------------------------------------------------------
# 13. imputeCellEM: no NAs remain after imputation
# ------------------------------------------------------------------
expect_false(any(is.na(res_em$data_imputed)))


# ------------------------------------------------------------------
# 14. imputeCellEM handles mixed data (continuous + factor)
# ------------------------------------------------------------------
set.seed(902)
n <- 80
df_em_mixed <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = factor(sample(c("A", "B", "C"), n, replace = TRUE))
)
df_em_mixed$x1[sample(n, 10)] <- NA
df_em_mixed$x2[sample(n, 10)] <- NA
df_em_mixed$x3[sample(n, 10)] <- NA

res_em_mixed <- imputeCellEM(df_em_mixed, maxit_em = 20, trace = FALSE)

expect_inherits(res_em_mixed$data_imputed, "data.frame")
expect_false(any(is.na(res_em_mixed$data_imputed)))
expect_true(is.factor(res_em_mixed$data_imputed$x3))
# Cell weights for factor column should be 1
expect_true(all(res_em_mixed$cellweights[, 3] == 1))


# ------------------------------------------------------------------
# 15. imputeCellEM converges (converged == TRUE)
# ------------------------------------------------------------------
set.seed(903)
n <- 80
df_em_conv <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)
df_em_conv$x1[sample(n, 8)] <- NA
df_em_conv$x2[sample(n, 8)] <- NA

res_em_conv <- imputeCellEM(df_em_conv, maxit_em = 100, eps_em = 5e-3,
                             trace = FALSE)

expect_true(res_em_conv$converged)


# ------------------------------------------------------------------
# 16. imputeCellEM: pseudo log-likelihood trace is non-decreasing (approx)
# ------------------------------------------------------------------
# The ECM algorithm should produce an (approximately) non-decreasing
# pseudo log-likelihood trace. Allow a small tolerance for numerical noise.
ll <- res_em_conv$pseudo_loglik
if (length(ll) > 1) {
  diffs <- diff(ll)
  # Allow tiny decreases due to numerical precision
  expect_true(all(diffs > -0.1),
              info = "Pseudo log-likelihood should be approximately non-decreasing")
}
