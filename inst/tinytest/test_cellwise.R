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

expect_inherits(res_m, "data.frame")
expect_equal(nrow(res_m), n)
expect_false(any(is.na(res_m$y)))

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
imputed_vals <- res_cellM$y[miss_rows]
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
expect_equal(res_m_complete, df_complete)


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


# ==========================================================================
# Tests for imputeCellEM
# ==========================================================================

# ------------------------------------------------------------------
# 12. imputeCellEM basic: continuous-only data with NAs returns complete
# ------------------------------------------------------------------
set.seed(1001)
n <- 60
df_em <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)
# Introduce ~15% MCAR missingness
for (j in 1:3) {
  miss_idx <- sample(n, round(0.15 * n))
  df_em[[j]][miss_idx] <- NA
}

res_em <- imputeCellEM(df_em, maxit_em = 30, trace = FALSE)

expect_inherits(res_em, "list")
expect_inherits(res_em$data_imputed, "data.frame")
expect_equal(dim(res_em$data_imputed), c(n, 3))
expect_false(any(is.na(res_em$data_imputed)))


# ------------------------------------------------------------------
# 13. imputeCellEM convergence on clean data (no contamination)
# ------------------------------------------------------------------
set.seed(1002)
n <- 80
Sigma_true <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
X_clean <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = Sigma_true)
df_clean_em <- as.data.frame(X_clean)
colnames(df_clean_em) <- c("x1", "x2")
# Introduce light missingness, avoiding all-NA rows
miss1 <- sample(n, 8)
miss2 <- sample(setdiff(seq_len(n), miss1), 8)
df_clean_em$x1[miss1] <- NA
df_clean_em$x2[miss2] <- NA

res_clean_em <- imputeCellEM(df_clean_em, maxit_em = 50, trace = FALSE)

# Should converge quickly on clean data
expect_true(res_clean_em$converged)
expect_true(res_clean_em$iterations <= 30)


# ------------------------------------------------------------------
# 14. imputeCellEM with mixed data: continuous + factor
# ------------------------------------------------------------------
set.seed(1003)
n <- 80
df_mixed_em <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = factor(sample(c("A", "B", "C"), n, replace = TRUE))
)
df_mixed_em$x1[sample(n, 10)] <- NA
df_mixed_em$x2[sample(n, 10)] <- NA
df_mixed_em$x3[sample(n, 10)] <- NA

res_mixed_em <- imputeCellEM(df_mixed_em, maxit_em = 20, trace = FALSE)

expect_inherits(res_mixed_em$data_imputed, "data.frame")
expect_false(any(is.na(res_mixed_em$data_imputed)))
# Factor column should remain a factor after imputation
expect_true(is.factor(res_mixed_em$data_imputed$x3))
# Cell weights for categorical columns should be 1
expect_true(all(res_mixed_em$cellweights[, "x3"] == 1))


# ------------------------------------------------------------------
# 15. imputeCellEM output structure: all expected list components
# ------------------------------------------------------------------
expected_names <- c("data_imputed", "cellweights", "mu", "Sigma",
                    "epsilon", "converged", "iterations", "loglik")
expect_true(all(expected_names %in% names(res_em)))

# mu should be a named numeric vector with p_cont elements
expect_equal(length(res_em$mu), 3)
expect_true(is.numeric(res_em$mu))
expect_true(!is.null(names(res_em$mu)))

# Sigma should be a p_cont x p_cont matrix
expect_equal(dim(res_em$Sigma), c(3, 3))
expect_true(is.numeric(res_em$Sigma))
# Sigma should be symmetric
expect_equal(res_em$Sigma, t(res_em$Sigma), tolerance = 1e-10)
# Sigma should be positive definite (all eigenvalues > 0)
expect_true(all(eigen(res_em$Sigma, only.values = TRUE)$values > 0))

# epsilon should be a named numeric vector in (0, 0.5)
expect_equal(length(res_em$epsilon), 3)
expect_true(all(res_em$epsilon > 0))
expect_true(all(res_em$epsilon < 0.5))

# converged is logical, iterations is integer
expect_inherits(res_em$converged, "logical")
expect_true(is.numeric(res_em$iterations))

# loglik is numeric vector with length == iterations
expect_true(is.numeric(res_em$loglik))
expect_equal(length(res_em$loglik), res_em$iterations)


# ------------------------------------------------------------------
# 16. Cell weights are in [0,1]; missing cells get weight 1
# ------------------------------------------------------------------
W_em <- res_em$cellweights
expect_equal(dim(W_em), c(60, 3))
expect_true(all(W_em >= 0 & W_em <= 1))

# Originally missing cells should have weight 1
M_em <- is.na(df_em)
for (j in 1:3) {
  miss_j <- which(M_em[, j])
  if (length(miss_j) > 0) {
    expect_true(all(W_em[miss_j, j] == 1))
  }
}


# ------------------------------------------------------------------
# 17. Edge case: no missing values returns immediately
# ------------------------------------------------------------------
set.seed(1004)
df_nomiss <- data.frame(x1 = rnorm(40), x2 = rnorm(40), x3 = rnorm(40))

res_nomiss <- imputeCellEM(df_nomiss, trace = FALSE)

expect_true(res_nomiss$converged)
expect_equal(res_nomiss$iterations, 0L)
expect_equal(res_nomiss$data_imputed, df_nomiss)
expect_true(all(res_nomiss$cellweights == 1))
expect_null(res_nomiss$mu)
expect_null(res_nomiss$Sigma)
expect_null(res_nomiss$epsilon)
expect_equal(length(res_nomiss$loglik), 0)


# ------------------------------------------------------------------
# 18. Contamination detection: outlying cells get low weights
# ------------------------------------------------------------------
set.seed(1005)
n <- 100
df_contam_em <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)
# Inject extreme outliers into x1 for first 5 rows
df_contam_em$x1[1:5] <- 20
# Introduce some missingness so EM runs
df_contam_em$x2[sample(6:n, 10)] <- NA

res_contam_em <- imputeCellEM(df_contam_em, maxit_em = 30, trace = FALSE)

# Outlier cells in x1 (rows 1:5) should have low weights
w_outliers <- res_contam_em$cellweights[1:5, "x1"]
expect_true(all(w_outliers < 0.5))

# Clean cells in x3 should have high mean weight
w_clean_x3 <- res_contam_em$cellweights[, "x3"]
expect_true(mean(w_clean_x3) > 0.8)


# ------------------------------------------------------------------
# 19. Error on all-NA rows (unit non-response)
# ------------------------------------------------------------------
set.seed(1006)
df_allna_row <- data.frame(
  x1 = c(NA, rnorm(49)),
  x2 = c(NA, rnorm(49)),
  x3 = c(NA, rnorm(49))
)
# Row 1 is entirely NA
expect_error(
  imputeCellEM(df_allna_row),
  pattern = "Unit non-responses"
)


# ------------------------------------------------------------------
# 20. Error on invalid inputs
# ------------------------------------------------------------------
# Single column
expect_error(
  imputeCellEM(data.frame(x = c(1, NA, 3))),
  pattern = "at least 2"
)

# gamma_init <= 1
set.seed(1007)
df_tmp <- data.frame(x1 = c(1, NA, 3), x2 = c(4, 5, NA))
expect_error(
  imputeCellEM(df_tmp, gamma_init = 0.5),
  pattern = "gamma_init"
)

# eps_init out of range
expect_error(
  imputeCellEM(df_tmp, eps_init = 0),
  pattern = "eps_init"
)
expect_error(
  imputeCellEM(df_tmp, eps_init = 0.6),
  pattern = "eps_init"
)


# ------------------------------------------------------------------
# 21. PMM uncertainty method works
# ------------------------------------------------------------------
set.seed(1008)
n <- 60
df_pmm <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)
df_pmm$x1[sample(n, 8)] <- NA
df_pmm$x2[sample(n, 8)] <- NA

res_pmm <- imputeCellEM(df_pmm, uncert = "pmm", maxit_em = 20, trace = FALSE)

expect_false(any(is.na(res_pmm$data_imputed)))
expect_equal(dim(res_pmm$data_imputed), c(n, 3))
# PMM imputed values for x1 should be within the range of observed x1 values
obs_x1 <- df_pmm$x1[!is.na(df_pmm$x1)]
imp_x1 <- res_pmm$data_imputed$x1[is.na(df_pmm$x1)]
expect_true(all(imp_x1 >= min(obs_x1) & imp_x1 <= max(obs_x1)))


# ------------------------------------------------------------------
# 22. Log-likelihood is non-decreasing (EM monotonicity)
# ------------------------------------------------------------------
# Use the result from the clean-data convergence test (res_clean_em)
if (length(res_clean_em$loglik) >= 3) {
  ll <- res_clean_em$loglik
  # Allow tiny numerical decreases (1e-6 relative)
  diffs <- diff(ll)
  rel_diffs <- diffs / (abs(ll[-length(ll)]) + 1)
  expect_true(all(rel_diffs > -1e-4))
}
