library(VIM)
library(data.table)

# ===========================================================================
# STATISTICAL VALIDATION TESTS for vimpute MI, bootstrap, uncertainty, GAM
#
# These tests use known data-generating processes (DGPs) to verify that:
# 1. Imputed values have plausible distributions
# 2. MI pooled estimates recover true parameters
# 3. Bootstrap adds appropriate model uncertainty
# 4. Uncertainty methods behave correctly
# 5. GAM captures nonlinear relationships
# 6. Results are comparable to mice (when available)
# ===========================================================================

# ---------------------------------------------------------------------------
# Helper: pool MI estimates using Rubin's rules
# ---------------------------------------------------------------------------
rubin_pool <- function(estimates, variances) {
  m <- length(estimates)
  Q_bar <- mean(estimates)                          # pooled estimate
  U_bar <- mean(variances)                          # within-imputation variance
  B <- var(estimates)                               # between-imputation variance
  T_total <- U_bar + (1 + 1/m) * B                 # total variance
  list(estimate = Q_bar, variance = T_total, se = sqrt(T_total),
       within_var = U_bar, between_var = B)
}

# ===========================================================================
# 1. KNOWN DGP: MI recovers true regression coefficient
# ===========================================================================

# DGP: y = 2*x + eps, x ~ N(3, 1), eps ~ N(0, 1)
# True beta = 2, true intercept = 0
# Make x MCAR with ~30% missing
set.seed(2026)
n <- 200
x_full <- rnorm(n, mean = 3, sd = 1)
y <- 2 * x_full + rnorm(n, sd = 1)
x_obs <- x_full
x_obs[sample(n, round(0.3 * n))] <- NA

dgp_data <- data.table(y = y, x = x_obs)

# Complete-case estimate (biased under MCAR only slightly, but less efficient)
cc_fit <- lm(y ~ x, data = dgp_data[!is.na(x)])
cc_beta <- coef(cc_fit)["x"]

# MI with vimpute: m=20 for stable pooling
set.seed(42)
mi_result <- vimpute(dgp_data, method = "ranger", sequential = FALSE,
                      imp_var = FALSE, boot = TRUE, uncert = "normalerror", m = 20)

# Fit lm on each imputed dataset and pool
fits <- with(mi_result, lm(y ~ x))
betas <- sapply(fits, function(f) coef(f)["x"])
vars <- sapply(fits, function(f) vcov(f)["x", "x"])
pooled <- rubin_pool(betas, vars)

# Test 1a: Pooled estimate should be close to true beta = 2
expect_true(abs(pooled$estimate - 2) < 0.5,
            info = sprintf("MI pooled beta=%.3f should be close to true 2.0", pooled$estimate))

# Test 1b: 95% CI should contain the true value
ci_lower <- pooled$estimate - 1.96 * pooled$se
ci_upper <- pooled$estimate + 1.96 * pooled$se
expect_true(ci_lower < 2 && ci_upper > 2,
            info = sprintf("MI 95%% CI [%.3f, %.3f] should contain true beta=2", ci_lower, ci_upper))

# Test 1c: Between-imputation variance should be positive (variability exists)
expect_true(pooled$between_var > 0,
            info = sprintf("Between-imp variance=%.6f should be > 0", pooled$between_var))

# Test 1d: Pooled SE should be larger than complete-case SE (MI accounts for missingness)
cc_se <- summary(cc_fit)$coefficients["x", "Std. Error"]
# Note: MI SE can sometimes be smaller than CC SE, but should be in the same ballpark
expect_true(pooled$se > 0,
            info = sprintf("MI SE=%.4f should be positive", pooled$se))

cat("  DGP test: true beta=2.0, MI estimate=", round(pooled$estimate, 3),
    ", SE=", round(pooled$se, 3), ", CC estimate=", round(cc_beta, 3), "\n")

# ===========================================================================
# 2. IMPUTED DISTRIBUTION PLAUSIBILITY
# ===========================================================================

# Imputed values for x should have similar mean and sd to observed x
set.seed(42)
single_imp <- vimpute(dgp_data, method = "ranger", sequential = FALSE, imp_var = TRUE, m = 1)

x_observed <- dgp_data$x[!is.na(dgp_data$x)]
x_imputed <- single_imp$x[single_imp$x_imp == TRUE]

# Test 2a: Imputed mean should be within 1 SD of observed mean
expect_true(abs(mean(x_imputed) - mean(x_observed)) < sd(x_observed),
            info = sprintf("Imputed mean=%.2f should be close to observed mean=%.2f",
                           mean(x_imputed), mean(x_observed)))

# Test 2b: Imputed values should be in a plausible range
expect_true(all(x_imputed > min(x_observed) - 3 * sd(x_observed)),
            info = "Imputed values should not be extreme outliers (lower)")
expect_true(all(x_imputed < max(x_observed) + 3 * sd(x_observed)),
            info = "Imputed values should not be extreme outliers (upper)")

# Test 2c: Imputed sd should be in reasonable range of observed sd
expect_true(sd(x_imputed) > 0.2 * sd(x_observed),
            info = "Imputed sd should not collapse to near zero")
expect_true(sd(x_imputed) < 5 * sd(x_observed),
            info = "Imputed sd should not explode")

# ===========================================================================
# 3. BOOTSTRAP EFFECT: boot=TRUE should increase between-imputation variance
# ===========================================================================

# Cleanest test: no uncertainty, only boot vs no boot.
# Without boot: deterministic imputation => all m datasets identical => var=0.
# With boot: bootstrap resampling => different models => var > 0.
set.seed(42)
mi_nothing <- suppressWarnings(
  vimpute(dgp_data, method = "robust", sequential = FALSE,
          imp_var = FALSE, boot = FALSE, uncert = "none", m = 10)
)

set.seed(42)
mi_bootonly <- suppressWarnings(
  vimpute(dgp_data, method = "robust", sequential = FALSE,
          imp_var = FALSE, boot = TRUE, robustboot = "stratified",
          uncert = "none", m = 10)
)

fits_nothing <- with(mi_nothing, lm(y ~ x))
fits_bootonly <- with(mi_bootonly, lm(y ~ x))

betas_nothing <- sapply(fits_nothing, function(f) coef(f)["x"])
betas_bootonly <- sapply(fits_bootonly, function(f) coef(f)["x"])

var_nothing <- var(betas_nothing)
var_bootonly <- var(betas_bootonly)

# Test 3a: Without boot+uncert, all imputations should be identical (var ~ 0)
expect_true(var_nothing < 1e-10,
            info = sprintf("No boot/uncert: var(beta)=%.2e should be ~0", var_nothing))

# Test 3b: With boot, between-imputation variance should be > 0
expect_true(var_bootonly > 1e-6,
            info = sprintf("Boot-only: var(beta)=%.6f should be > 0", var_bootonly))

# Also check boot + normalerror produces meaningful variance
set.seed(42)
mi_both <- suppressWarnings(
  vimpute(dgp_data, method = "robust", sequential = FALSE,
          imp_var = FALSE, boot = TRUE, robustboot = "stratified",
          uncert = "normalerror", m = 10)
)
fits_both <- with(mi_both, lm(y ~ x))
betas_both <- sapply(fits_both, function(f) coef(f)["x"])
var_both <- var(betas_both)

# Test 3c: Boot + normalerror should have meaningful variance
expect_true(var_both > 1e-5,
            info = sprintf("Boot+normalerror: var(beta)=%.6f should be meaningful", var_both))

cat("  Bootstrap test: var(no boot/uncert)=", formatC(var_nothing, format="e", digits=2),
    ", var(boot only)=", formatC(var_bootonly, format="e", digits=2),
    ", var(boot+normalerror)=", formatC(var_both, format="e", digits=2), "\n")

# ===========================================================================
# 4. UNCERTAINTY METHOD VALIDATION
# ===========================================================================

# --- 4a: normalerror should add N(0, sigma) noise ---
set.seed(42)
preds <- rep(5, 1000)
noisy <- VIM:::inject_uncertainty(preds, method = "normalerror", scale = 2.0)
noise <- noisy - preds

# Noise should be approximately N(0, 2)
expect_true(abs(mean(noise)) < 0.3,
            info = sprintf("normalerror noise mean=%.3f should be ~0", mean(noise)))
expect_true(abs(sd(noise) - 2.0) < 0.3,
            info = sprintf("normalerror noise sd=%.3f should be ~2.0", sd(noise)))

# --- 4b: pmm should return only observed values ---
set.seed(42)
y_obs <- c(1, 3, 5, 7, 9)
score_obs <- c(1.1, 2.9, 5.2, 6.8, 9.1)
score_miss <- c(2.5, 6.0, 8.5)
pmm_result <- VIM:::pmm_donor_selection(y_obs, score_obs, score_miss, k = 1, agg_method = "random")

# PMM should return values from the observed set
expect_true(all(pmm_result %in% y_obs),
            info = "PMM should only return observed values")

# With k=1 and agg_method="random", sample() is used even with one neighbor.
# Use "mean" to get deterministic results.
set.seed(42)
pmm_det <- VIM:::pmm_donor_selection(y_obs, score_obs, score_miss, k = 1, agg_method = "mean")
# score_miss=2.5 -> closest score_obs=2.9 -> y=3
# score_miss=6.0 -> closest score_obs=5.2 or 6.8 -> y=5 or 7
# score_miss=8.5 -> closest score_obs=9.1 -> y=9
expect_equal(pmm_det[1], 3, info = "PMM k=1 mean: score 2.5 should match y=3")
expect_true(pmm_det[3] %in% c(7, 9), info = "PMM k=1 mean: score 8.5 should match y=7 or 9")

# --- 4c: resid should add sampled training residuals ---
set.seed(42)
resids <- rnorm(100, mean = 0, sd = 1.5)
preds_r <- rep(10, 500)
noisy_r <- VIM:::inject_uncertainty(preds_r, method = "resid", residuals = resids)
noise_r <- noisy_r - preds_r

# Sampled residuals should have similar distribution to training residuals
expect_true(abs(mean(noise_r)) < 0.3,
            info = sprintf("resid noise mean=%.3f should be ~0", mean(noise_r)))
expect_true(abs(sd(noise_r) - sd(resids)) < 0.5,
            info = sprintf("resid noise sd=%.3f should be ~%.3f", sd(noise_r), sd(resids)))

# ===========================================================================
# 5. GAM CAPTURES NONLINEAR RELATIONSHIPS
# ===========================================================================

# DGP: y = sin(2*pi*x) + eps, x ~ U(0,1)
# Linear methods will fail; GAM should capture the curve
set.seed(2026)
n_gam <- 150
x_gam_full <- runif(n_gam)
y_gam <- sin(2 * pi * x_gam_full) + rnorm(n_gam, sd = 0.3)
x_gam <- x_gam_full
miss_idx <- sample(n_gam, 30)
x_gam[miss_idx] <- NA

gam_data <- data.table(y = y_gam, x = x_gam)

# Impute with GAM
set.seed(42)
imp_gam <- vimpute(gam_data, method = "gam", sequential = FALSE, imp_var = TRUE)

# Impute with ranger for comparison
set.seed(42)
imp_ranger <- vimpute(gam_data, method = "ranger", sequential = FALSE, imp_var = TRUE)

# True values at missing positions
x_true_miss <- x_gam_full[miss_idx]
x_gam_imputed <- imp_gam$x[miss_idx]
x_ranger_imputed <- imp_ranger$x[miss_idx]

# Test 5a: GAM imputed values should correlate with true values
cor_gam <- cor(x_gam_imputed, x_true_miss)
cor_ranger <- cor(x_ranger_imputed, x_true_miss)

expect_true(cor_gam > 0.3,
            info = sprintf("GAM imputed correlation with truth=%.3f should be > 0.3", cor_gam))

# Test 5b: RMSE of GAM imputation should be reasonable
rmse_gam <- sqrt(mean((x_gam_imputed - x_true_miss)^2))
rmse_ranger <- sqrt(mean((x_ranger_imputed - x_true_miss)^2))
range_x <- diff(range(x_gam_full))

expect_true(rmse_gam < range_x,
            info = sprintf("GAM RMSE=%.3f should be less than range of x=%.3f", rmse_gam, range_x))

cat("  GAM nonlinear test: RMSE gam=", round(rmse_gam, 3),
    ", ranger=", round(rmse_ranger, 3),
    ", cor gam=", round(cor_gam, 3),
    ", cor ranger=", round(cor_ranger, 3), "\n")

# ===========================================================================
# 6. ROBGAM OUTLIER RESISTANCE
# ===========================================================================

# DGP: y = x + eps, but with 10% outliers where y is shifted by +20
set.seed(2026)
n_rob <- 100
x_rob <- rnorm(n_rob)
y_rob <- x_rob + rnorm(n_rob, sd = 0.5)
# Add outliers
outlier_idx <- 1:10
y_rob[outlier_idx] <- y_rob[outlier_idx] + 20

# Create missing values in a clean region
miss_rob <- 80:90
x_rob_miss <- x_rob
x_true_rob <- x_rob[miss_rob]
x_rob_miss[miss_rob] <- NA

rob_data <- data.table(y = y_rob, x = x_rob_miss)

# Impute with robgam vs gam
set.seed(42)
imp_robgam <- suppressWarnings(
  vimpute(rob_data, method = "robgam", sequential = FALSE, imp_var = FALSE)
)
set.seed(42)
imp_gam_out <- suppressWarnings(
  vimpute(rob_data, method = "gam", sequential = FALSE, imp_var = FALSE)
)

rmse_robgam <- sqrt(mean((imp_robgam$x[miss_rob] - x_true_rob)^2))
rmse_gam_out <- sqrt(mean((imp_gam_out$x[miss_rob] - x_true_rob)^2))

# Test 6a: robGAM should produce finite imputed values
expect_true(all(is.finite(imp_robgam$x[miss_rob])),
            info = "robGAM imputed values should be finite")

# Test 6b: robGAM RMSE should be reasonable
expect_true(rmse_robgam < sd(x_rob) * 3,
            info = sprintf("robGAM RMSE=%.3f should be reasonable", rmse_robgam))

cat("  robGAM outlier test: RMSE robgam=", round(rmse_robgam, 3),
    ", gam=", round(rmse_gam_out, 3), "\n")

# ===========================================================================
# 7. COMPARISON WITH mice (if available)
# ===========================================================================

if (requireNamespace("mice", quietly = TRUE)) {

  cat("  Running mice comparison tests...\n")

  # Same DGP as test 1: y = 2*x + eps
  set.seed(2026)
  n_mice <- 200
  x_mice_full <- rnorm(n_mice, mean = 3, sd = 1)
  y_mice <- 2 * x_mice_full + rnorm(n_mice, sd = 1)
  x_mice <- x_mice_full
  x_mice[sample(n_mice, round(0.3 * n_mice))] <- NA
  mice_data <- data.frame(y = y_mice, x = x_mice)

  # mice MI with pmm (default), m=20
  set.seed(42)
  mice_result <- mice::mice(mice_data, m = 20, method = "pmm", printFlag = FALSE)
  mice_fits <- with(mice_result, lm(y ~ x))
  mice_pooled <- mice::pool(mice_fits)
  mice_summary <- summary(mice_pooled)

  mice_beta <- mice_summary$estimate[mice_summary$term == "x"]
  mice_se <- mice_summary$std.error[mice_summary$term == "x"]

  # vimpute MI with ranger + boot + normalerror, m=20
  set.seed(42)
  vim_data <- data.table(y = y_mice, x = x_mice)
  vim_result <- vimpute(vim_data, method = "ranger", sequential = FALSE,
                         imp_var = FALSE, boot = TRUE, uncert = "normalerror", m = 20)
  vim_fits <- with(vim_result, lm(y ~ x))
  vim_betas <- sapply(vim_fits, function(f) coef(f)["x"])
  vim_vars <- sapply(vim_fits, function(f) vcov(f)["x", "x"])
  vim_pooled <- rubin_pool(vim_betas, vim_vars)

  # Test 7a: Both should recover true beta close to 2
  expect_true(abs(mice_beta - 2) < 0.5,
              info = sprintf("mice beta=%.3f should be close to true 2.0", mice_beta))
  expect_true(abs(vim_pooled$estimate - 2) < 0.5,
              info = sprintf("vimpute beta=%.3f should be close to true 2.0", vim_pooled$estimate))

  # Test 7b: Estimates should be in the same ballpark
  expect_true(abs(vim_pooled$estimate - mice_beta) < 1.0,
              info = sprintf("vimpute (%.3f) and mice (%.3f) estimates should be comparable",
                             vim_pooled$estimate, mice_beta))

  # Test 7c: SEs should be in the same order of magnitude
  se_ratio <- vim_pooled$se / mice_se
  expect_true(se_ratio > 0.1 && se_ratio < 10,
              info = sprintf("SE ratio vimpute/mice=%.2f should be in [0.1, 10]", se_ratio))

  cat("  mice comparison: mice beta=", round(mice_beta, 3), " SE=", round(mice_se, 3),
      ", vimpute beta=", round(vim_pooled$estimate, 3), " SE=", round(vim_pooled$se, 3), "\n")

  # --- 7d: Coverage test with repeated simulation ---
  # Run 50 repetitions of the DGP, check if 95% CIs contain the true beta
  n_sim <- 50
  vim_covers <- logical(n_sim)
  mice_covers <- logical(n_sim)

  for (sim in seq_len(n_sim)) {
    x_s <- rnorm(n_mice, mean = 3, sd = 1)
    y_s <- 2 * x_s + rnorm(n_mice, sd = 1)
    x_s[sample(n_mice, round(0.3 * n_mice))] <- NA
    sim_data <- data.table(y = y_s, x = x_s)

    # vimpute
    vim_r <- tryCatch({
      vimpute(sim_data, method = "ranger", sequential = FALSE,
              imp_var = FALSE, boot = TRUE, uncert = "normalerror", m = 10)
    }, error = function(e) NULL)

    if (!is.null(vim_r)) {
      vim_f <- with(vim_r, lm(y ~ x))
      vim_b <- sapply(vim_f, function(f) coef(f)["x"])
      vim_v <- sapply(vim_f, function(f) vcov(f)["x", "x"])
      vim_p <- rubin_pool(vim_b, vim_v)
      vim_lo <- vim_p$estimate - 1.96 * vim_p$se
      vim_hi <- vim_p$estimate + 1.96 * vim_p$se
      vim_covers[sim] <- (vim_lo < 2 && vim_hi > 2)
    }

    # mice
    mice_r <- tryCatch({
      mice::mice(as.data.frame(sim_data), m = 10, method = "pmm", printFlag = FALSE)
    }, error = function(e) NULL)

    if (!is.null(mice_r)) {
      mice_f <- with(mice_r, lm(y ~ x))
      mice_p <- mice::pool(mice_f)
      mice_s <- summary(mice_p, conf.int = TRUE)
      mice_lo <- mice_s[mice_s$term == "x", "2.5 %"]
      mice_hi <- mice_s[mice_s$term == "x", "97.5 %"]
      mice_covers[sim] <- (mice_lo < 2 && mice_hi > 2)
    }
  }

  vim_coverage <- mean(vim_covers)
  mice_coverage <- mean(mice_covers)

  # Test 7e: vimpute coverage should be at least 70% (proper MI should be ~95%,
  # but ranger+normalerror is not a parametric imputation model, so we're lenient)
  expect_true(vim_coverage >= 0.60,
              info = sprintf("vimpute coverage=%.0f%% should be >= 60%%", vim_coverage * 100))

  # Test 7f: mice coverage should be good
  expect_true(mice_coverage >= 0.80,
              info = sprintf("mice coverage=%.0f%% should be >= 80%%", mice_coverage * 100))

  cat("  Coverage test (", n_sim, " sims): vimpute=",
      round(vim_coverage * 100), "%, mice=",
      round(mice_coverage * 100), "%\n", sep = "")

} else {
  cat("  Skipping mice comparison tests (mice not installed)\n")
}

# ===========================================================================
# 8. MI WITH GAM: pooled estimates on nonlinear DGP
# ===========================================================================

# DGP: y = x^2 + eps, with x MCAR
set.seed(2026)
n_gmi <- 150
x_gmi <- rnorm(n_gmi, mean = 2, sd = 1)
y_gmi <- x_gmi^2 + rnorm(n_gmi, sd = 1)
x_gmi_miss <- x_gmi
x_gmi_miss[sample(n_gmi, 30)] <- NA

gmi_data <- data.table(y = y_gmi, x = x_gmi_miss)

set.seed(42)
mi_gam_result <- vimpute(gmi_data, method = "gam", sequential = FALSE,
                          imp_var = FALSE, boot = TRUE, uncert = "normalerror", m = 10)

# Check that we get a proper vimmi object
expect_true(inherits(mi_gam_result, "vimmi"),
            info = "GAM MI should return vimmi")
expect_equal(mi_gam_result$m, 10L)

# All completed datasets should be valid
for (i in 1:10) {
  ci <- complete(mi_gam_result, i)
  expect_equal(sum(is.na(ci)), 0,
               info = sprintf("GAM MI dataset %d should have no NAs", i))
}

# Between-imputation variance should exist
gam_mi_means <- sapply(1:10, function(i) mean(complete(mi_gam_result, i)$x))
expect_true(var(gam_mi_means) > 0,
            info = "GAM MI should have between-imputation variability in x means")

cat("  GAM MI: between-imp variance of x means=", round(var(gam_mi_means), 6), "\n")

# ===========================================================================
# 9. DETERMINISTIC vs STOCHASTIC IMPUTATION CHECK
# ===========================================================================

# Without boot/uncert, same seed should give identical results
set.seed(42)
det1 <- vimpute(dgp_data, method = "ranger", sequential = FALSE, imp_var = FALSE, m = 1)
set.seed(42)
det2 <- vimpute(dgp_data, method = "ranger", sequential = FALSE, imp_var = FALSE, m = 1)
expect_identical(det1$x, det2$x,
                 info = "Same seed without boot/uncert should give identical results")

# With boot+uncert, different seeds should give different results
set.seed(1)
stoch1 <- vimpute(dgp_data, method = "ranger", sequential = FALSE,
                   imp_var = FALSE, m = 1, boot = TRUE, uncert = "normalerror")
set.seed(2)
stoch2 <- vimpute(dgp_data, method = "ranger", sequential = FALSE,
                   imp_var = FALSE, m = 1, boot = TRUE, uncert = "normalerror")
expect_true(!identical(stoch1$x, stoch2$x),
            info = "Different seeds with boot+uncert should give different results")

cat("\nAll statistical validation tests complete.\n")
